module Bjolang.TailRecursion

open Bjolang.TypedAST

/// Marks self-recursive calls that sit in tail position so that codegen can
/// turn them into a loop instead of a stack frame.
///
/// Only the nodes that actually *propagate* tail position are handled
/// explicitly here; a node's tail-ness cannot be expressed by a plain map, so
/// everything else (where no child is in tail position) is delegated to
/// `TypeVisitor.mapChildren`.
let rec analyzeExpr (inTailPosition: bool) (currentFuncName: string option) (expr: TypedExpr) : TypedExpr =
    /// Analyze a sub-expression that is *not* in tail position.
    let notTail e = analyzeExpr false currentFuncName e
    /// Analyze a sub-expression that inherits the current tail position.
    let inherits e = analyzeExpr inTailPosition currentFuncName e

    match expr.Node with
    | TApply(t, args, kwArgs, _) ->
        let isSelfRecursive =
            match currentFuncName, t.Node with
            | Some cName, TIdent(tName, _) when cName = tName -> true
            | _ -> false

        let newArgs = args |> List.map notTail
        let newKwArgs = kwArgs |> List.map (fun (n, e) -> n, notTail e)

        { expr with
            Node = TApply(notTail t, newArgs, newKwArgs, isSelfRecursive && inTailPosition) }

    | TIf(c, t, f) ->
        { expr with
            Node = TIf(notTail c, inherits t, inherits f) }

    | TLet(n, isFun, args, v, b) ->
        let newValue =
            if isFun then
                analyzeBinding n v
            else
                analyzeExpr false currentFuncName v

        { expr with
            Node = TLet(n, isFun, args, newValue, inherits b) }

    | TLetRec(bindings, b) ->
        let newBindings =
            bindings
            |> List.map (fun (n, isFun, args, v) -> n, isFun, args, analyzeBinding n v)

        { expr with
            Node = TLetRec(newBindings, inherits b) }

    | TLetMutable(n, v, b) ->
        { expr with
            Node = TLetMutable(n, notTail v, inherits b) }

    | TLetTuple(names, v, b) ->
        { expr with
            Node = TLetTuple(names, notTail v, inherits b) }

    | TMatch(e, clauses) ->
        let newClauses =
            clauses
            |> List.map (fun c ->
                { c with
                    Guard = Option.map notTail c.Guard
                    Body = inherits c.Body })

        { expr with
            Node = TMatch(notTail e, newClauses) }

    // A lambda is a *new* function scope. Its body's tail positions belong to the
    // lambda, not to the enclosing function, so a call to the enclosing function
    // from here is an ordinary call: flagging it would make codegen assign the
    // lambda's parameters and jump to the enclosing function's loop.
    | TLambda(args, b) ->
        { expr with
            Node = TLambda(args, analyzeExpr true None b) }

    // No child of any remaining node is in tail position.
    | _ -> TypeVisitor.mapChildren notTail expr

/// Analyzes the value of a function-shaped `let` binding. The binding's own name
/// becomes the current function, and the immediate `TLambda` wrapper inference
/// produces for such a binding is looked *through* — it is the binding's own
/// function scope, not a nested one.
and analyzeBinding (name: string) (value: TypedExpr) : TypedExpr =
    match value.Node with
    | TLambda(args, body) ->
        { value with
            Node = TLambda(args, analyzeExpr true (Some name) body) }
    | _ -> analyzeExpr true (Some name) value

let rec analyzeDecl (decl: TDecl) : TDecl =
    match decl with
    | TModule(n, decls, r) -> TModule(n, List.map analyzeDecl decls, r)
    | TDef(n, e, t, r) -> TDef(n, analyzeExpr false None e, t, r)
    | TDefTuple(names, e, t, r) -> TDefTuple(names, analyzeExpr false None e, t, r)
    | TDefMutable(n, e, t, r) -> TDefMutable(n, analyzeExpr false None e, t, r)
    | TDefun(n, tyArgs, args, kwArgs, restArg, retType, body, r) ->
        let analyzedKwArgs =
            kwArgs |> List.map (fun (kn, kt, ke) -> kn, kt, analyzeExpr false None ke)

        TDefun(n, tyArgs, args, analyzedKwArgs, restArg, retType, analyzeExpr true (Some n) body, r)
    | TImpl(traitName, targetType, assocBindings, methods, r) ->
        TImpl(traitName, targetType, assocBindings, List.map analyzeDecl methods, r)
    | _ -> decl

let analyzeProgram (decls: TDecl list) : TDecl list = List.map analyzeDecl decls
