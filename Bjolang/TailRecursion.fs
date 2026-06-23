module Bjolang.TailRecursion

open Bjolang.TypeChecker

let rec analyzeExpr (inTailPosition: bool) (currentFuncName: string option) (expr: TypedExpr) : TypedExpr =
    let mapNode node =
        match node with
        | TInt _ | TString _ | TKeyword _ | TSymbol _ | TIdent _ -> node

        | TApply (t, args, _) ->
            let isSelfRecursive =
                match currentFuncName, t.Node with
                | Some cName, TIdent(tName, _) when cName = tName -> true
                | _ -> false

            let newArgs = args |> List.map (analyzeExpr false currentFuncName)

            if isSelfRecursive && inTailPosition then
                TApply (analyzeExpr false currentFuncName t, newArgs, true)
            else
                TApply (analyzeExpr false currentFuncName t, newArgs, false)

        | TInterfaceCall (i, m, d, args) ->
            TInterfaceCall (i, m, analyzeExpr false currentFuncName d, args |> List.map (analyzeExpr false currentFuncName))

        | TIf (c, t, f) ->
            TIf (analyzeExpr false currentFuncName c, analyzeExpr inTailPosition currentFuncName t, analyzeExpr inTailPosition currentFuncName f)

        | TLet (n, isFun, args, v, b) ->
            let newFuncName = if isFun then Some n else currentFuncName
            let vTail = if isFun then true else false
            TLet (n, isFun, args, analyzeExpr vTail newFuncName v, analyzeExpr inTailPosition currentFuncName b)

        | TLetRec (bindings, b) ->
            let newBindings =
                bindings |> List.map (fun (n, isFun, args, v) ->
                    n, isFun, args, analyzeExpr true (Some n) v
                )
            TLetRec (newBindings, analyzeExpr inTailPosition currentFuncName b)

        | TLetMutable (n, v, b) ->
            TLetMutable (n, analyzeExpr false currentFuncName v, analyzeExpr inTailPosition currentFuncName b)

        | TLetTuple (names, v, b) ->
            TLetTuple (names, analyzeExpr false currentFuncName v, analyzeExpr inTailPosition currentFuncName b)

        | TMatch (e, clauses) ->
            let newClauses =
                clauses |> List.map (fun c ->
                    { c with Guard = Option.map (analyzeExpr false currentFuncName) c.Guard
                             Body = analyzeExpr inTailPosition currentFuncName c.Body }
                )
            TMatch (analyzeExpr false currentFuncName e, newClauses)

        | TTryFinally (b, c) ->
            TTryFinally (analyzeExpr false currentFuncName b, analyzeExpr false currentFuncName c)

        | TTupleMake exprs -> TTupleMake (List.map (analyzeExpr false currentFuncName) exprs)
        | TListMake exprs -> TListMake (List.map (analyzeExpr false currentFuncName) exprs)
        | TRecordMake fields -> TRecordMake (fields |> List.map (fun (k, v) -> k, analyzeExpr false currentFuncName v))
        | TRecordUpdate (n, fields) -> TRecordUpdate (n, fields |> List.map (fun (k, v) -> k, analyzeExpr false currentFuncName v))

        | TLambda (args, b) ->
            TLambda (args, analyzeExpr true None b)

        | TSet (n, v) -> TSet (n, analyzeExpr false currentFuncName v)
        | TGetField (e, f) -> TGetField (analyzeExpr false currentFuncName e, f)
        | TIsInst (e, t) -> TIsInst (analyzeExpr false currentFuncName e, t)
        | TCast (e, t) -> TCast (analyzeExpr false currentFuncName e, t)
        | TTypeEq (e1, e2) -> TTypeEq (analyzeExpr false currentFuncName e1, analyzeExpr false currentFuncName e2)

    { expr with Node = mapNode expr.Node }

let rec analyzeDecl (decl: TDecl) : TDecl =
    match decl with
    | TModule (n, decls, r) -> TModule (n, List.map analyzeDecl decls, r)
    | TDef (n, e, t, r) -> TDef (n, analyzeExpr false None e, t, r)
    | TDefTuple (names, e, t, r) -> TDefTuple (names, analyzeExpr false None e, t, r)
    | TDefMutable (n, e, t, r) -> TDefMutable (n, analyzeExpr false None e, t, r)
    | TDefun (n, tyArgs, args, retType, body, r) ->
        TDefun (n, tyArgs, args, retType, analyzeExpr true (Some n) body, r)
    | TImpl (traitName, targetType, assocBindings, methods, r) ->
        TImpl (traitName, targetType, assocBindings, List.map analyzeDecl methods, r)
    | _ -> decl

let analyzeProgram (decls: TDecl list) : TDecl list =
    List.map analyzeDecl decls
