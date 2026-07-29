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
        let newFuncName = if isFun then Some n else currentFuncName
        let vTail = isFun

        { expr with
            Node = TLet(n, isFun, args, analyzeExpr vTail newFuncName v, inherits b) }

    | TLetRec(bindings, b) ->
        let newBindings =
            bindings
            |> List.map (fun (n, isFun, args, v) -> n, isFun, args, analyzeExpr true (Some n) v)

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

    | TLambda(args, b) ->
        { expr with
            Node = TLambda(args, analyzeExpr true currentFuncName b) }

    // No child of any remaining node is in tail position.
    | _ -> TypeVisitor.mapChildren notTail expr

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
