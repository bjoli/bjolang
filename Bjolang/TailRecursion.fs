module Bjolang.TailRecursion

open Bjolang.TypeChecker
open Bjolang.Lexer

let rec analyzeExpr (inTailPosition: bool) (currentFuncName: string option) (expr: FExpr) : FExpr =
    let mapNode node =
        match node with
        | FInt _ | FString _ | FKeyword _ | FSymbol _ | FNull | FNewObject _ | FIdent _ -> node
        
        | FApply (t, args, _) ->
            let isSelfRecursive =
                match currentFuncName, t.Node with
                | Some cName, FIdent(tName, _) when cName = tName -> true
                | _ -> false
                
            let newArgs = args |> List.map (analyzeExpr false currentFuncName)
            
            if isSelfRecursive && inTailPosition then
                FTailCall newArgs
            else
                FApply (analyzeExpr false currentFuncName t, newArgs, inTailPosition)
                
        | FTailCall args ->
            // Already a tail call? Shouldn't happen before this pass, but just in case.
            FTailCall (args |> List.map (analyzeExpr false currentFuncName))
            
        | FInterfaceCall (i, m, d, args, _) ->
            FInterfaceCall (i, m, analyzeExpr false currentFuncName d, args |> List.map (analyzeExpr false currentFuncName), inTailPosition)
            
        | FIf (c, t, f) ->
            FIf (analyzeExpr false currentFuncName c, analyzeExpr inTailPosition currentFuncName t, analyzeExpr inTailPosition currentFuncName f)
            
        | FLet (n, isFun, args, v, b) ->
            let newFuncName = if isFun then Some n else currentFuncName
            let vTail = if isFun then true else false
            FLet (n, isFun, args, analyzeExpr vTail newFuncName v, analyzeExpr inTailPosition currentFuncName b)
            
        | FLetRec (bindings, b) ->
            let newBindings = 
                bindings |> List.map (fun (n, isFun, args, v) ->
                    n, isFun, args, analyzeExpr true (Some n) v
                )
            FLetRec (newBindings, analyzeExpr inTailPosition currentFuncName b)
            
        | FLetMutable (n, v, b) ->
            FLetMutable (n, analyzeExpr false currentFuncName v, analyzeExpr inTailPosition currentFuncName b)
            
        | FLetTuple (names, v, b) ->
            FLetTuple (names, analyzeExpr false currentFuncName v, analyzeExpr inTailPosition currentFuncName b)
            
        | FMatch (e, clauses) ->
            let newClauses = 
                clauses |> List.map (fun c ->
                    { c with Guard = Option.map (analyzeExpr false currentFuncName) c.Guard
                             Body = analyzeExpr inTailPosition currentFuncName c.Body }
                )
            FMatch (analyzeExpr false currentFuncName e, newClauses)
            
        | FTryFinally (b, c) ->
            // The CLR forbids .tail inside try blocks
            FTryFinally (analyzeExpr false currentFuncName b, analyzeExpr false currentFuncName c)
            
        | FTupleMake exprs -> FTupleMake (List.map (analyzeExpr false currentFuncName) exprs)
        | FListMake exprs -> FListMake (List.map (analyzeExpr false currentFuncName) exprs)
        | FRecordMake fields -> FRecordMake (fields |> List.map (fun (k, v) -> k, analyzeExpr false currentFuncName v))
        | FRecordUpdate (bRec, fields) -> FRecordUpdate (bRec, fields |> List.map (fun (k, v) -> k, analyzeExpr false currentFuncName v))
        
        | FLambda (args, b) ->
            // Enter new closure, reset currentFuncName to avoid accidental tail-call of outer function
            FLambda (args, analyzeExpr true None b)
            
        | FSet (n, v) -> FSet (n, analyzeExpr false currentFuncName v)
        | FSetField (o, f, v) -> FSetField (analyzeExpr false currentFuncName o, f, analyzeExpr false currentFuncName v)
        | FIsInst (e, t) -> FIsInst (analyzeExpr false currentFuncName e, t)
        | FGetField (e, f) -> FGetField (analyzeExpr false currentFuncName e, f)
        | FTypeEq (e1, e2) -> FTypeEq (analyzeExpr false currentFuncName e1, analyzeExpr false currentFuncName e2)
        | FCreateDelegate (name, tgtOpt) -> FCreateDelegate (name, Option.map (analyzeExpr false currentFuncName) tgtOpt)
        
    { expr with Node = mapNode expr.Node }

let rec analyzeDecl (decl: FDecl) : FDecl =
    match decl with
    | FModule (n, decls, r) -> FModule (n, List.map analyzeDecl decls, r)
    | FDef (n, e, t, r) -> FDef (n, analyzeExpr false None e, t, r)
    | FDefTuple (names, e, t, r) -> FDefTuple (names, analyzeExpr false None e, t, r)
    | FDefMutable (n, e, t, r) -> FDefMutable (n, analyzeExpr false None e, t, r)
    | FDefun (n, tyArgs, args, retType, body, r) ->
        FDefun (n, tyArgs, args, retType, analyzeExpr true (Some n) body, r)
    | FImpl (traitName, targetType, assocBindings, methods, r) ->
        FImpl (traitName, targetType, assocBindings, List.map analyzeDecl methods, r)
    | FDisplayClass (n, fields, methods, r) ->
        FDisplayClass (n, fields, List.map analyzeDecl methods, r)
    | _ -> decl

let analyzeProgram (decls: FDecl list) : FDecl list =
    List.map analyzeDecl decls
