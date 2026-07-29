module Bjolang.Codegen

open System
open System.Text
open Bjolang.TypedAST
open Bjolang.Parser

type UnionCaseInfo = {
    ParentTypeName: string
    IsDataCase: bool
}

type CodegenContext = {
    Builder: StringBuilder
    IndentLevel: int
    UnionCases: Map<string, UnionCaseInfo>
    GlobalBindings: Map<string, string>
    TailCallArgs: string list option
}

let inline append (ctx: CodegenContext) (s: string) =
    ctx.Builder.Append(s) |> ignore

let inline appendLine (ctx: CodegenContext) (s: string) =
    ctx.Builder.AppendLine(s) |> ignore

let inline indent (ctx: CodegenContext) =
    ctx.Builder.Append(String(' ', ctx.IndentLevel * 4)) |> ignore

let withIndent (ctx: CodegenContext) (f: CodegenContext -> unit) =
    f { ctx with IndentLevel = ctx.IndentLevel + 1 }

let mapPrimitiveType (name: string) =
    match name with
    | "System.Int32" -> "int"
    | "System.Byte" -> "byte"
    | "System.Int16" -> "short"
    | "System.UInt16" -> "ushort"
    | "System.UInt32" -> "uint"
    | "System.Int64" -> "long"
    | "System.UInt64" -> "ulong"
    | "System.Double" -> "double"
    | "System.String" -> "string"
    | "System.Boolean" -> "bool"
    | "System.Void" -> "void"
    | "System.Object" -> "object"
    | "Vec" -> "Collections.RrbList"
    | "VecBuilder" -> "Collections.RrbBuilder"
    | _ -> name

let sanitizeIdent (s: string) =
    let s = s.Replace("::", ".").Replace("-", "sub").Replace("?", "_QMARK").Replace("!", "_BANG").Replace("+", "add").Replace("*", "mul").Replace("/", "div").Replace("<", "lt").Replace(">", "gt").Replace("=", "eq").Replace("'", "")
    let s = if s.Length > 0 && Char.IsDigit(s[0]) then "_" + s else s
    match s with
    | "class" | "struct" | "public" | "private" | "protected" | "internal" | "static" | "readonly" | "var" | "ref" | "out" | "in" | "params" | "new" | "return" | "if" | "else" | "while" | "for" | "foreach" | "do" | "switch" | "case" | "default" | "break" | "continue" | "goto" | "try" | "catch" | "finally" | "throw" | "lock" | "typeof" | "sizeof" | "is" | "as" | "true" | "false" | "null" | "void" | "object" | "string" | "int" | "bool" -> "@" + s
    | _ -> s

let rec typeToString (hm: HMType) : string =
    match hm with
    | TCon ("Array", [elemType]) ->
        $"{typeToString elemType}[]"
    | TCon (name, args) ->
        let mapped = mapPrimitiveType name
        let baseName = if mapped = name then sanitizeIdent name else mapped
        if args.IsEmpty then baseName
        else
            let argsStr = args |> List.map typeToString |> String.concat ", "
            $"%s{baseName}<%s{argsStr}>"
    | TVar name -> 
        if name.StartsWith("'") then "T_" + name.Substring(1)
        else "T_" + name
    | TFun (args, ret) ->
        let argsStr = args |> List.map typeToString |> String.concat ", "
        if typeToString ret = "void" then
            if args.IsEmpty then "Action" else $"Action<%s{argsStr}>"
        else
            if args.IsEmpty then $"Func<%s{typeToString ret}>" else $"Func<%s{argsStr}, %s{typeToString ret}>"
    | TTuple types ->
        let typesStr = types |> List.map typeToString |> String.concat ", "
        $"ValueTuple<%s{typesStr}>"
    | TMeta m ->
        match m.Value with
        | Some t -> typeToString t
        | None -> "object /* unresolved meta */"
    | TAssoc (traitName, assocName, implType) ->
        "object /* unresolved assoc */"
type BlockTarget =
    | Return
    | Assign of string
    | DeclareAndAssign of string * string
    | Discard

let rec serializeHMType (t: HMType) : string =
    match t with
    | TCon (name, args) ->
        let baseName = 
            match name with
            | _ when name = TypeConstants.Int32Name -> "int"
            | _ when name = TypeConstants.StringName -> "string"
            | _ when name = TypeConstants.BooleanName -> "bool"
            | _ when name = TypeConstants.VoidName -> "void"
            | _ -> name
        if args.IsEmpty then baseName
        else $"(%s{baseName} " + String.concat " " (List.map serializeHMType args) + ")"
    | TVar name -> name
    | TFun (args, ret) ->
        if args.IsEmpty then $"(-> %s{serializeHMType ret})"
        else $"(-> " + String.concat " " (List.map serializeHMType args) + $" %s{serializeHMType ret})"
    | TTuple types ->
        $"(Tuple " + String.concat " " (List.map serializeHMType types) + ")"
    | TMeta m ->
        match m.Value with
        | Some v -> serializeHMType v
        | None -> "object"
    | TAssoc (traitName, assocName, implType) ->
        $"object"


let getUnionTypeString (hm: HMType) (parentName: string) : string =
    let rec findCon t =
        match t with
        | TCon(n, args) when n = parentName -> Some (n, args)
        | TFun(_, ret) -> findCon ret
        | TMeta m ->
            match m.Value with
            | Some t' -> findCon t'
            | None -> None
        | _ -> None
    match findCon hm with
    | Some (n, args) ->
        let baseName = mapPrimitiveType n
        if args.IsEmpty then baseName
        else
            let argsStr = args |> List.map typeToString |> String.concat ", "
            $"%s{baseName}<%s{argsStr}>"
    | None ->
        sanitizeIdent parentName

let rec hasTailCall (expr: TypedExpr) =
    match expr.Node with
    | TApply (_, _, _, true) -> true
    | TIf (_, t, f) -> hasTailCall t || hasTailCall f
    | TLet (_, _, _, _, b) | TLetMutable (_, _, b) | TLetTuple (_, _, b) -> hasTailCall b
    | TLetRec (_, b) -> hasTailCall b
    | TMatch (_, clauses) -> clauses |> List.exists (fun c -> hasTailCall c.Body)
    | _ -> false

let escapeStringLiteral (s: string) =
    s.Replace("\"", "\\\"").Replace("\n", "\\n")

/// A clause that matches unconditionally; anything after it is dead code.
let private isIrrefutable (c: TMatchClause) =
    c.Guard.IsNone
    && (match c.Pattern.Node with
        | TPWildcard
        | TPIdent _ -> true
        | _ -> false)

/// Bjolang matches are first-match-wins. C# rejects arms it can prove are
/// unreachable (CS8510), so drop everything following the first irrefutable clause.
let private liveClauses (clauses: TMatchClause list) =
    let rec take acc remaining =
        match remaining with
        | [] -> List.rev acc
        | c :: rest -> if isIrrefutable c then List.rev (c :: acc) else take (c :: acc) rest
    take [] clauses

let private matchTempCounter = ref 0

let private freshMatchTemp () =
    matchTempCounter.Value <- matchTempCounter.Value + 1
    $"__match{matchTempCounter.Value}"

/// Translates a typed pattern into C# pattern syntax.
let rec generatePattern (ctx: CodegenContext) (pat: TypedPattern) : unit =
    match pat.Node with
    | TPWildcard -> append ctx "_"
    | TPIdent name -> append ctx $"var {sanitizeIdent name}"
    | TPInt value -> append ctx value
    | TPString value -> append ctx $"\"%s{escapeStringLiteral value}\""
    | TPConstruct (name, args) ->
        let caseTypeStr =
            match Map.tryFind name ctx.UnionCases with
            | Some info -> $"{getUnionTypeString pat.Type info.ParentTypeName}.{sanitizeIdent name}"
            | None -> $"{typeToString pat.Type}.{sanitizeIdent name}"
        append ctx caseTypeStr
        // A positional record with an empty parameter list gets no Deconstruct
        // method, so nullary cases must be emitted as a bare type pattern.
        if not args.IsEmpty then
            append ctx "("
            for i, argPat in List.indexed args do
                if i > 0 then append ctx ", "
                generatePattern ctx argPat
            append ctx ")"
    | TPList (items, tailOpt) ->
        // Lists are the ordinary Cons/Nil union rather than a C# collection type,
        // so desugar into nested constructor patterns.
        let rec desugar elements : TypedPattern =
            match elements with
            | [] ->
                match tailOpt with
                | Some t -> t
                | None -> { Type = pat.Type; Range = pat.Range; Node = TPConstruct("Nil", []) }
            | head :: rest ->
                { Type = pat.Type
                  Range = pat.Range
                  Node = TPConstruct("Cons", [ head; desugar rest ]) }
        generatePattern ctx (desugar items)
    | TPAs _ ->
        failwithf $"'as' patterns have no C# equivalent (line %d{pat.Range.Start.Line})"
    | TPApp _ ->
        failwithf $"Applied patterns are not supported by the C# backend (line %d{pat.Range.Start.Line})"

let rec generateExpr (ctx: CodegenContext) (expr: TypedExpr) : unit =
    match expr.Node with
    | TInt i -> append ctx i
    | TString s -> 
        let escaped = s.Replace("\"", "\\\"").Replace("\n", "\\n")
        append ctx $"\"%s{escaped}\""
    | TKeyword k -> append ctx $"\"%s{k}\""
    | TSymbol s -> append ctx $"\"%s{s}\""
    | TIdent (name, _) ->
        match Map.tryFind name ctx.UnionCases with
        | Some info ->
            let typeStr = getUnionTypeString expr.Type info.ParentTypeName
            if info.IsDataCase then
                match expr.Type with
                | TFun (argTypes, _) ->
                    let argsList = [for i in 0 .. argTypes.Length - 1 -> $"arg{i}"]
                    let argsStr = String.concat ", " argsList
                    append ctx $"({argsStr}) => new {typeStr}.{sanitizeIdent name}({argsStr})"
                | _ ->
                    append ctx $"new {typeStr}.{sanitizeIdent name}()"
            else
                append ctx $"new {typeStr}.{sanitizeIdent name}()"
        | None ->
            let targetName =
                match Map.tryFind name ctx.GlobalBindings with
                | Some modName -> $"%s{sanitizeIdent modName}_Module.%s{sanitizeIdent name}"
                | None -> sanitizeIdent name
            match expr.Type with
            | TFun _ ->
                append ctx $"(({typeToString expr.Type})({targetName}))"
            | _ ->
                append ctx targetName
    | TApply (target, args, kwArgs, _) ->
        match target.Node with
        | TIdent (name, _) when Map.containsKey name ctx.UnionCases ->
            let info = Map.find name ctx.UnionCases
            let typeStr = getUnionTypeString expr.Type info.ParentTypeName
            append ctx $"new {typeStr}.{sanitizeIdent name}("
            for i, arg in List.indexed args do
                if i > 0 then append ctx ", "
                generateExpr ctx arg
            append ctx ")"
        | TIdent (name, _) when List.contains name ["+"; "-"; "*"; "/"; "%"; "<"; ">"; "<="; ">="] && args.Length = 2 && kwArgs.IsEmpty ->
            append ctx "("
            generateExpr ctx args.[0]
            append ctx $" {name} "
            generateExpr ctx args.[1]
            append ctx ")"
        | TIdent ("=", _) when args.Length = 2 && kwArgs.IsEmpty ->
            append ctx "("
            generateExpr ctx args.[0]
            append ctx " == "
            generateExpr ctx args.[1]
            append ctx ")"
        | _ ->
            match target.Node with
            | TIdent (name, tArgs) ->
                let targetName =
                    match Map.tryFind name ctx.GlobalBindings with
                    | Some modName -> $"%s{sanitizeIdent modName}_Module.%s{sanitizeIdent name}"
                    | None -> sanitizeIdent name
                append ctx targetName
                if not tArgs.IsEmpty && args.IsEmpty && kwArgs.IsEmpty then
                    let tyArgsStr = tArgs |> List.map (fun t -> typeToString t) |> String.concat ", "
                    append ctx $"<%s{tyArgsStr}>"
            | _ ->
                generateExpr ctx target
            append ctx "("
            if kwArgs.IsEmpty then
                // Simple case: no keyword args, just emit all positional args
                for i, arg in List.indexed args do
                    if i > 0 then append ctx ", "
                    generateExpr ctx arg
            else
                // We have keyword args. Determine how many mandatory args there are.
                // Function type is TFun([mandatoryTypes..., keywordTypes..., optionalArrayType], retType)
                // positionalArgs = mandatoryArgs ++ restArgs
                // kwArgs = keyword values
                let funArgCount =
                    match target.Type with
                    | TFun(argTypes, _) -> argTypes.Length
                    | _ -> args.Length + kwArgs.Length

                // Check if last param is a rest/params array
                let hasRest =
                    match target.Type with
                    | TFun(argTypes, _) when not argTypes.IsEmpty ->
                        match List.last argTypes with
                        | TCon("Array", _) -> true
                        | _ -> false
                    | _ -> false

                let mandatoryCount = funArgCount - kwArgs.Length - (if hasRest then 1 else 0)
                let mandatoryArgs = args |> List.take (min args.Length mandatoryCount)
                let restArgs = args |> List.skip (min args.Length mandatoryCount)

                let mutable argIdx = 0
                // 1. Emit mandatory positional args
                for arg in mandatoryArgs do
                    if argIdx > 0 then append ctx ", "
                    generateExpr ctx arg
                    argIdx <- argIdx + 1
                // 2. Emit keyword args
                if hasRest then
                    // Emit positionally (can't mix named + params in C#)
                    for (_, kwExpr) in kwArgs do
                        if argIdx > 0 then append ctx ", "
                        generateExpr ctx kwExpr
                        argIdx <- argIdx + 1
                else
                    // Emit as C# named arguments
                    for (kwName, kwExpr) in kwArgs do
                        if argIdx > 0 then append ctx ", "
                        append ctx $"%s{sanitizeIdent kwName}: "
                        generateExpr ctx kwExpr
                        argIdx <- argIdx + 1
                // 3. Emit rest args
                for arg in restArgs do
                    if argIdx > 0 then append ctx ", "
                    generateExpr ctx arg
                    argIdx <- argIdx + 1
            append ctx ")"
    | TInterfaceCall (iType, mName, dict, args) ->
        generateExpr ctx dict
        append ctx "."
        append ctx (sanitizeIdent mName)
        append ctx "("
        for i, arg in List.indexed args do
            if i > 0 then append ctx ", "
            generateExpr ctx arg
        append ctx ")"
    | TLambda (args, body) ->
        append ctx "("
        let argsStr = args |> List.map sanitizeIdent |> String.concat ", "
        append ctx argsStr
        append ctx ") => {\n"
        withIndent ctx (fun c ->
            let isTail = hasTailCall body
            let c2 = if isTail then { c with TailCallArgs = Some (args |> List.map sanitizeIdent) } else c
            if isTail then
                indent c2; appendLine c2 "while (true) {"
                withIndent c2 (fun c3 -> generateBlock c3 Return body)
                indent c2; appendLine c2 "}"
            else
                generateBlock c2 Return body
        )
        indent ctx; append ctx "}"
    | TIf (cond, t, f) ->
        let isVoid = typeToString expr.Type = "void"
        if isVoid then
            append ctx "new Action(() => {\n"
            withIndent ctx (fun c -> generateBlock c Discard expr)
            indent ctx; append ctx "})()"
        else
            append ctx $"new Func<{typeToString expr.Type}>(() => {{\n"
            withIndent ctx (fun c -> generateBlock c Return expr)
            indent ctx; append ctx "})()"
    | TTupleMake args ->
        append ctx "("
        for i, arg in List.indexed args do
            if i > 0 then append ctx ", "
            generateExpr ctx arg
        append ctx ")"
    | TRecordMake fields ->
        let recTypeName = typeToString expr.Type
        append ctx $"new %s{recTypeName}("
        for i, (k, v) in List.indexed fields do
            if i > 0 then append ctx ", "
            generateExpr ctx v
        append ctx ")"
    | TRecordUpdate (name, fields) ->
        // `with` binds loosely, so parenthesize: `(r with { .. }).field` must not
        // parse as `r with { .. field }`.
        let targetName =
            match Map.tryFind name ctx.GlobalBindings with
            | Some modName -> $"%s{sanitizeIdent modName}_Module.%s{sanitizeIdent name}"
            | None -> sanitizeIdent name
        append ctx "("
        append ctx targetName
        append ctx " with { "
        for i, (k, v) in List.indexed fields do
            if i > 0 then append ctx ", "
            append ctx (sanitizeIdent k)
            append ctx " = "
            generateExpr ctx v
        append ctx " })"
    | TIsInst (target, t) ->
        append ctx "("
        generateExpr ctx target
        append ctx " is "
        append ctx (typeToString t)
        append ctx ")"
    | TIsInstCase (target, t, caseName) ->
        append ctx "("
        generateExpr ctx target
        append ctx $" is {typeToString t}.{sanitizeIdent caseName}"
        append ctx ")"
    | TGetField (target, field) ->
        generateExpr ctx target
        append ctx "."
        append ctx (sanitizeIdent field)
    | TCast (target, t) ->
        append ctx "(("
        append ctx (typeToString t)
        append ctx ")("
        generateExpr ctx target
        append ctx "))"
    | TCaseCast (target, t, caseName) ->
        append ctx "(("
        append ctx $"{typeToString t}.{sanitizeIdent caseName}"
        append ctx ")("
        generateExpr ctx target
        append ctx "))"
    | TLet _ | TLetRec _ | TThrow _ as node ->
        let isVoid = typeToString expr.Type = "void"
        if isVoid then
            append ctx "new Action(() => {\n"
            withIndent ctx (fun c -> generateBlock c Discard expr)
            indent ctx; append ctx "})()"
        else
            append ctx $"new Func<{typeToString expr.Type}>(() => {{\n"
            withIndent ctx (fun c -> generateBlock c Return expr)
            indent ctx; append ctx "})()"
    | TListMake items ->
        // Desugar to nested Cons calls: new List<T>.Cons(e1, new List<T>.Cons(e2, new List<T>.Nil()))
        let listTypeStr = typeToString expr.Type
        let rec emitCons remaining =
            match remaining with
            | [] ->
                append ctx $"new {listTypeStr}.Nil()"
            | item :: rest ->
                append ctx $"new {listTypeStr}.Cons("
                generateExpr ctx item
                append ctx ", "
                emitCons rest
                append ctx ")"
        emitCons items
    | TVecMake items ->
        // Emit builder pattern:
        // ((Func<Collections.RrbList<T>>)(() => {
        //     var b = new Collections.RrbBuilder<T>();
        //     b.Add(e1); b.Add(e2); ...
        //     return b.ToImmutable();
        // }))()
        let vecTypeStr = typeToString expr.Type
        let elementTypeStr =
            match expr.Type with
            | TCon(_, [elemT]) -> typeToString elemT
            | _ -> "object"
        let builderTypeStr = $"Collections.RrbBuilder<{elementTypeStr}>"
        append ctx $"((Func<{vecTypeStr}>)(() => {{\n"
        withIndent ctx (fun c ->
            indent c; appendLine c $"var __b = new {builderTypeStr}();"
            for item in items do
                indent c; append c "__b.Add("; generateExpr c item; appendLine c ");"
            indent c; appendLine c "return __b.ToImmutable();"
        )
        indent ctx; append ctx "}))()"
    | TMatch (matchTarget, clauses) ->
        let live = liveClauses clauses
        let isVoid = typeToString expr.Type = "void"
        let hasTail = live |> List.exists (fun c -> hasTailCall c.Body)

        if isVoid || hasTail then
            // A switch *expression* cannot yield void, and it cannot contain the
            // `continue` a tail call compiles to. Fall back to the statement form.
            // TailCallArgs is cleared: a `continue` would bind to the lambda, not
            // to the enclosing function's trampoline.
            let inner = { ctx with TailCallArgs = None }
            if isVoid then
                append ctx "new Action(() => {\n"
                withIndent inner (fun c -> generateBlock c Discard expr)
                indent ctx; append ctx "})()"
            else
                append ctx $"new Func<{typeToString expr.Type}>(() => {{\n"
                withIndent inner (fun c -> generateBlock c Return expr)
                indent ctx; append ctx "})()"
        else
            generateExpr ctx matchTarget
            appendLine ctx " switch {"
            withIndent ctx (fun c ->
                for clause in live do
                    indent c
                    generatePattern c clause.Pattern
                    match clause.Guard with
                    | Some guard ->
                        append c " when "
                        generateExpr c guard
                    | None -> ()
                    append c " => "
                    generateExpr c clause.Body
                    appendLine c ","
                if not (live |> List.exists isIrrefutable) then
                    indent c
                    appendLine c $"_ => throw new Exception(\"Match failure at line %d{expr.Range.Start.Line}\")"
            )
            indent ctx; append ctx "}"
    | _ ->
        append ctx "/* Unimplemented expression node */"

and generateBlock (ctx: CodegenContext) (target: BlockTarget) (expr: TypedExpr) : unit =
    match expr.Node with
    | TApply (_, args, _, true) when ctx.TailCallArgs.IsSome ->
        let tArgs = ctx.TailCallArgs.Value
        for i, arg in List.indexed args do
            indent ctx; append ctx $"var _tailArg{i} = "
            generateExpr ctx arg
            appendLine ctx ";"
        for i in 0 .. args.Length - 1 do
            indent ctx; appendLine ctx $"{tArgs.[i]} = _tailArg{i};"
        indent ctx; appendLine ctx "continue;"
    | TLet (name, isFun, args, value, body) ->
        if isFun then
            match value.Node with
            | TLambda (lambdaArgs, lambdaBody) ->
                match value.Type with
                | TFun (argTypes, retType) ->
                    let rec collectTVars t =
                        match t with
                        | TVar name -> [name]
                        | TFun(args, ret) -> (args |> List.collect collectTVars) @ collectTVars ret
                        | TCon(_, args) -> args |> List.collect collectTVars
                        | TTuple types -> types |> List.collect collectTVars
                        | TMeta m ->
                            match m.Value with
                            | Some t' -> collectTVars t'
                            | None -> []
                        | _ -> []
                    let typeParams = collectTVars value.Type |> List.distinct
                    let tyParamsStr =
                        if typeParams.IsEmpty then ""
                        else "<" + (typeParams |> List.map (fun t -> if t.StartsWith("'") then "T_" + t.Substring(1) else "T_" + t) |> String.concat ", ") + ">"
                    
                    indent ctx
                    append ctx (typeToString retType)
                    append ctx " "
                    append ctx (sanitizeIdent name)
                    append ctx tyParamsStr
                    append ctx "("
                    for i, (argName, argType) in List.indexed (List.zip lambdaArgs argTypes) do
                        if i > 0 then append ctx ", "
                        append ctx (typeToString argType)
                        append ctx " "
                        append ctx (sanitizeIdent argName)
                    appendLine ctx ") {"
                    withIndent ctx (fun c ->
                        generateBlock c Return lambdaBody
                    )
                    indent ctx
                    appendLine ctx "}"
                    generateBlock ctx target body
                | _ ->
                    generateBlock ctx (DeclareAndAssign (typeToString value.Type, sanitizeIdent name)) value
                    generateBlock ctx target body
            | _ ->
                generateBlock ctx (DeclareAndAssign (typeToString value.Type, sanitizeIdent name)) value
                generateBlock ctx target body
        else
            if typeToString value.Type = "void" then
                generateBlock ctx Discard value
            else
                generateBlock ctx (DeclareAndAssign (typeToString value.Type, sanitizeIdent name)) value
            generateBlock ctx target body
    
    | TLetRec (bindings, body) ->
        for (name, _, _, value) in bindings do
            indent ctx
            appendLine ctx $"{typeToString value.Type} {sanitizeIdent name} = default!;"
        for (name, _, _, value) in bindings do
            generateBlock ctx (Assign (sanitizeIdent name)) value
        generateBlock ctx target body
        
    | TIf (cond, t, f) ->
        match target with
        | DeclareAndAssign (varType, varName) ->
            indent ctx; appendLine ctx $"{varType} {varName};"
            indent ctx; append ctx "if ("
            generateExpr ctx cond
            appendLine ctx ") {"
            withIndent ctx (fun c -> generateBlock c (Assign varName) t)
            indent ctx; appendLine ctx "} else {"
            withIndent ctx (fun c -> generateBlock c (Assign varName) f)
            indent ctx; appendLine ctx "}"
        | _ ->
            indent ctx; append ctx "if ("
            generateExpr ctx cond
            appendLine ctx ") {"
            withIndent ctx (fun c -> generateBlock c target t)
            indent ctx; appendLine ctx "} else {"
            withIndent ctx (fun c -> generateBlock c target f)
            indent ctx; appendLine ctx "}"

    | TThrow msgExpr ->
        indent ctx; append ctx "throw new Exception("
        generateExpr ctx msgExpr
        appendLine ctx ");"

    | TMatch (matchTarget, clauses) ->
        // Emitted as a switch *statement* so that arms may contain statements,
        // produce void, or `continue` into the enclosing tail-call trampoline.
        let live = liveClauses clauses

        // C# only treats a switch statement as exhaustive when it has a `default`
        // section, and `case _:` is not legal syntax, so a trailing irrefutable
        // clause is emitted as the default section instead of a case.
        let irrefutableTail, cases =
            match List.rev live with
            | last :: revRest when isIrrefutable last -> Some last, List.rev revRest
            | _ -> None, live

        // `default:` carries no pattern, so an irrefutable `TPIdent` clause needs
        // the scrutinee hoisted into a local that it can alias.
        let needsTemp =
            match irrefutableTail with
            | Some c ->
                match c.Pattern.Node with
                | TPIdent _ -> true
                | _ -> false
            | None -> false

        let scrutinee =
            if needsTemp then
                let tmp = freshMatchTemp ()
                indent ctx; append ctx $"var %s{tmp} = "
                generateExpr ctx matchTarget
                appendLine ctx ";"
                Some tmp
            else
                None

        let emitSwitch (armTarget: BlockTarget) =
            // A `Return` target always terminates the section itself
            // (return / continue / throw), so a break would be unreachable.
            let emitBreak cb =
                match armTarget with
                | Return -> ()
                | _ -> indent cb; appendLine cb "break;"

            indent ctx; append ctx "switch ("
            (match scrutinee with
             | Some tmp -> append ctx tmp
             | None -> generateExpr ctx matchTarget)
            appendLine ctx ") {"
            withIndent ctx (fun c ->
                for clause in cases do
                    indent c
                    append c "case "
                    generatePattern c clause.Pattern
                    match clause.Guard with
                    | Some guard ->
                        append c " when "
                        generateExpr c guard
                    | None -> ()
                    // Each section gets its own block so locals declared by
                    // different arms cannot collide in the shared switch scope.
                    appendLine c ": {"
                    withIndent c (fun cb ->
                        generateBlock cb armTarget clause.Body
                        emitBreak cb)
                    indent c; appendLine c "}"

                indent c
                match irrefutableTail with
                | Some clause ->
                    appendLine c "default: {"
                    withIndent c (fun cb ->
                        match clause.Pattern.Node, scrutinee with
                        | TPIdent name, Some tmp ->
                            indent cb; appendLine cb $"var %s{sanitizeIdent name} = %s{tmp};"
                        | _ -> ()
                        generateBlock cb armTarget clause.Body
                        emitBreak cb)
                    indent c; appendLine c "}"
                | None ->
                    appendLine c $"default: throw new Exception(\"Match failure at line %d{expr.Range.Start.Line}\");"
            )
            indent ctx; appendLine ctx "}"

        match target with
        | DeclareAndAssign (varType, varName) ->
            indent ctx; appendLine ctx $"{varType} {varName};"
            emitSwitch (Assign varName)
        | _ -> emitSwitch target

    | _ ->
        let isVoid = typeToString expr.Type = "void"
        indent ctx
        match target with
        | Return ->
            if isVoid then
                generateExpr ctx expr
                appendLine ctx ";"
                indent ctx; appendLine ctx "return;"
            else
                append ctx "return "
                generateExpr ctx expr
                appendLine ctx ";"
        | Assign name ->
            if isVoid then
                generateExpr ctx expr
                appendLine ctx ";"
            else
                append ctx $"{name} = "
                generateExpr ctx expr
                appendLine ctx ";"
        | DeclareAndAssign (varType, varName) ->
            if isVoid then
                generateExpr ctx expr
                appendLine ctx ";"
            else
                append ctx $"{varType} {varName} = "
                generateExpr ctx expr
                appendLine ctx ";"
        | Discard ->
            generateExpr ctx expr
            appendLine ctx ";"
    | _ ->
        append ctx "/* Unimplemented expression node */"

let rec generateDecl (ctx: CodegenContext) (decl: TDecl) : unit =
    match decl with
    | TDefun (name, tyArgs, args, kwArgs, restArg, retType, body, _) ->
        indent ctx
        append ctx "public static "
        append ctx (typeToString retType)
        append ctx " "
        append ctx (sanitizeIdent name)
        if not tyArgs.IsEmpty then
            let tyArgsStr = tyArgs |> List.map (fun t -> if t.StartsWith("'") then "T_" + t.Substring(1) else "T_" + t) |> String.concat ", "
            append ctx $"<%s{tyArgsStr}>"
        append ctx "("
        let mutable paramIdx = 0
        // Mandatory args
        for (argName, argType) in args do
            if paramIdx > 0 then append ctx ", "
            append ctx (typeToString argType)
            append ctx " "
            append ctx (sanitizeIdent argName)
            paramIdx <- paramIdx + 1
        // Keyword args with defaults
        for (kwName, kwType, kwDefault) in kwArgs do
            if paramIdx > 0 then append ctx ", "
            append ctx (typeToString kwType)
            append ctx " "
            append ctx (sanitizeIdent kwName)
            append ctx " = "
            generateExpr ctx kwDefault
            paramIdx <- paramIdx + 1
        // Rest arg (params)
        match restArg with
        | Some (restName, restElemType) ->
            if paramIdx > 0 then append ctx ", "
            append ctx $"params %s{typeToString restElemType}[] %s{sanitizeIdent restName}"
        | None -> ()
        append ctx ") {\n"
        withIndent ctx (fun c ->
            let isTail = hasTailCall body
            let c2 = 
                if isTail then 
                    let mandatoryNames = args |> List.map fst
                    let kwNames = kwArgs |> List.map (fun (n, _, _) -> n)
                    let restNameOpt = match restArg with Some(n, _) -> [n] | None -> []
                    let allArgs = mandatoryNames @ kwNames @ restNameOpt |> List.map sanitizeIdent
                    { c with TailCallArgs = Some allArgs }
                else c
            if isTail then
                indent c2; appendLine c2 "while (true) {"
                withIndent c2 (fun c3 -> generateBlock c3 Return body)
                indent c2; appendLine c2 "}"
            else
                generateBlock c2 Return body
        )
        indent ctx; appendLine ctx "}"

    | TDef (name, value, t, _) ->
        indent ctx
        append ctx "public static "
        append ctx (typeToString t)
        append ctx " "
        append ctx (sanitizeIdent name)
        append ctx " = "
        generateExpr ctx value
        appendLine ctx ";"

    | TType (defs, _) 
    | TTypeRec (defs, _) ->
        for td in defs do
            let tyArgsStr = 
                if td.TypeArgs.IsEmpty then "" 
                else "<" + (td.TypeArgs |> List.map (fun a -> "T_" + a.TrimStart('\'')) |> String.concat ", ") + ">"
            match td.Kind with
            | Record fields ->
                indent ctx
                append ctx $"public record %s{sanitizeIdent td.Name}%s{tyArgsStr}("
                for i, f in List.indexed fields do
                    if i > 0 then append ctx ", "
                    append ctx (typeToString (Inference.resolveTypeAnnotation Prelude.emptyRegistry f.Type)) // Hack: we need HMType, but TypeDef has FType
                    // Actually, TType doesn't have the fully resolved types.
                    // Wait, we need the resolved types for record fields! 
                    append ctx " "
                    append ctx (sanitizeIdent f.Name)
                appendLine ctx ");"
            | Union cases ->
                indent ctx
                appendLine ctx $"public abstract record %s{sanitizeIdent td.Name}%s{tyArgsStr} {{"
                withIndent ctx (fun ctx ->
                    indent ctx
                    appendLine ctx $"private %s{sanitizeIdent td.Name}() {{}}"
                    for c in cases do
                        indent ctx
                        match c with
                        | SimpleCase (n, _) ->
                            appendLine ctx $"public sealed record %s{sanitizeIdent n}() : %s{sanitizeIdent td.Name}%s{tyArgsStr};"
                        | DataCase (n, ftypes, _) ->
                            append ctx $"public sealed record %s{sanitizeIdent n}("
                            for i, ft in List.indexed ftypes do
                                if i > 0 then append ctx ", "
                                append ctx (typeToString (Inference.resolveTypeAnnotation Prelude.emptyRegistry ft)) // FIXME: registry
                                append ctx $" Item%d{i+1}"
                            appendLine ctx $") : %s{sanitizeIdent td.Name}%s{tyArgsStr};"
                )
                indent ctx
                appendLine ctx "}"
            | Alias _ -> ()

    | TTrait (name, targetVar, assocTypes, signatures, _) ->
        indent ctx
        let tyParams = targetVar :: assocTypes |> List.map (fun t -> "T_" + t.TrimStart('\'')) |> String.concat ", "
        appendLine ctx $"public interface %s{sanitizeIdent name}<%s{tyParams}> {{"
        withIndent ctx (fun ctx ->
            for kvp in signatures do
                let mName = kvp.Key
                let mType = kvp.Value
                match mType with
                | TFun (args, ret) ->
                    indent ctx
                    append ctx (typeToString ret)
                    append ctx " "
                    append ctx (sanitizeIdent mName)
                    append ctx "("
                    for i, arg in List.indexed args do
                        if i > 0 then append ctx ", "
                        append ctx (typeToString arg)
                        append ctx $" arg%d{i}"
                    appendLine ctx ");"
                | _ -> () // Should be function
        )
        indent ctx
        appendLine ctx "}"

    | TImpl (traitName, targetType, assocMap, methods, _) ->
        // This is tricky: we need the target type's concrete name to form the class name.
        let targetTypeName =
            match targetType with
            | TCon(n, _) -> n.Replace(".", "_")
            | _ -> "Unknown"
        let sanitizedTraitName = sanitizeIdent traitName
        let className = $"%s{sanitizedTraitName}_%s{targetTypeName}"
        
        let typeParams =
            match targetType with
            | TCon(_, args) ->
                args |> List.choose (function TVar v -> Some ("T_" + v.TrimStart('\'')) | _ -> None) |> List.distinct
            | _ -> []
        let tyParamsStr = if typeParams.IsEmpty then "" else "<" + String.concat ", " typeParams + ">"
        
        let targetTypeStr = typeToString targetType
        let assocArgsStr =
            assocMap
            |> List.map (fun (_, t) -> typeToString t)
            |> String.concat ", "
        let traitArgsStr = if String.IsNullOrEmpty(assocArgsStr) then targetTypeStr else $"%s{targetTypeStr}, %s{assocArgsStr}"

        indent ctx
        appendLine ctx $"public sealed class %s{className}%s{tyParamsStr} : %s{sanitizedTraitName}<%s{traitArgsStr}> {{"
        withIndent ctx (fun ctx ->
            indent ctx
            appendLine ctx $"public static readonly %s{className}%s{tyParamsStr} Instance = new();"
            for m in methods do
                match m with
                | TDefun (n, _, args, kwArgs, restArg, retType, body, _) ->
                    indent ctx
                    append ctx "public "
                    append ctx (typeToString retType)
                    append ctx " "
                    append ctx (sanitizeIdent n)
                    append ctx "("
                    let mutable paramIdx = 0
                    for (argName, argType) in args do
                        if paramIdx > 0 then append ctx ", "
                        append ctx (typeToString argType)
                        append ctx " "
                        append ctx (sanitizeIdent argName)
                        paramIdx <- paramIdx + 1
                    for (kwName, kwType, kwDefault) in kwArgs do
                        if paramIdx > 0 then append ctx ", "
                        append ctx (typeToString kwType)
                        append ctx " "
                        append ctx (sanitizeIdent kwName)
                        append ctx " = "
                        generateExpr ctx kwDefault
                        paramIdx <- paramIdx + 1
                    match restArg with
                    | Some (restName, restElemType) ->
                        if paramIdx > 0 then append ctx ", "
                        append ctx $"params %s{typeToString restElemType}[] %s{sanitizeIdent restName}"
                    | None -> ()
                    append ctx ") {\n"
                    withIndent ctx (fun ctx ->
                        generateBlock ctx Return body
                    )
                    indent ctx
                    appendLine ctx "}"
                | _ -> ()
        )
        indent ctx
        appendLine ctx "}"

    | TModule (name, decls, _) ->
        let isOuterDecl = function
            | TType _ | TTypeRec _ | TTrait _ | TImpl _ -> true
            | _ -> false

        for d in decls |> List.filter isOuterDecl do
            generateDecl ctx d

        let innerDecls = decls |> List.filter (not << isOuterDecl)
        
        indent ctx
        appendLine ctx $"public static class %s{sanitizeIdent name}_Module {{"
        withIndent ctx (fun ctx ->
            // Emit factory methods for union cases
            for d in decls |> List.filter isOuterDecl do
                match d with
                | TType (defs, _) | TTypeRec (defs, _) ->
                    for td in defs do
                        let tyArgsStr = 
                            if td.TypeArgs.IsEmpty then "" 
                            else "<" + (td.TypeArgs |> List.map (fun a -> "T_" + a.TrimStart('\'')) |> String.concat ", ") + ">"
                        match td.Kind with
                        | Union cases ->
                            for c in cases do
                                match c with
                                | SimpleCase (n, _) ->
                                    indent ctx
                                    appendLine ctx $"public static %s{sanitizeIdent td.Name}%s{tyArgsStr} %s{sanitizeIdent n}%s{tyArgsStr}() => new %s{sanitizeIdent td.Name}%s{tyArgsStr}.%s{sanitizeIdent n}();"
                                | DataCase (n, ftypes, _) ->
                                    indent ctx
                                    append ctx $"public static %s{sanitizeIdent td.Name}%s{tyArgsStr} %s{sanitizeIdent n}%s{tyArgsStr}("
                                    for i, ft in List.indexed ftypes do
                                        if i > 0 then append ctx ", "
                                        append ctx (typeToString (Inference.resolveTypeAnnotation Prelude.emptyRegistry ft)) // Need empty registry because it's just for typeToString
                                        append ctx $" arg{i}"
                                    let argsListStr = String.concat ", " [for i in 0 .. ftypes.Length - 1 -> $"arg{i}"]
                                    appendLine ctx $") => new %s{sanitizeIdent td.Name}%s{tyArgsStr}.%s{sanitizeIdent n}(%s{argsListStr});"
                        | _ -> ()
                | _ -> ()

            for d in innerDecls do
                generateDecl ctx d
        )
        indent ctx
        appendLine ctx "}"

    | _ -> ()


let generateProgram (exportMetadata: string) (dllDeps: string list) (decls: TDecl list) : string =
    let unionCases =
        let rec collect decls =
            decls |> List.collect (function
                | TType (defs, _) | TTypeRec (defs, _) ->
                    defs |> List.collect (fun td ->
                        match td.Kind with
                        | Union cases ->
                            cases |> List.map (fun c ->
                                match c with
                                | SimpleCase (name, _) -> name, { ParentTypeName = td.Name; IsDataCase = false }
                                | DataCase (name, _, _) -> name, { ParentTypeName = td.Name; IsDataCase = true }
                            )
                        | _ -> []
                    )
                | TModule (_, innerDecls, _) -> collect innerDecls
                | _ -> []
            )
        collect decls |> Map.ofList

    let globalBindings =
        let rec collect decls =
            decls |> List.collect (function
                | TModule (modName, innerDecls, _) ->
                    innerDecls |> List.choose (function
                        | TDef (n, _, _, _) -> Some (n, modName)
                        | TDefun (n, _, _, _, _, _, _, _) -> Some (n, modName)
                        | _ -> None
                    )
                | _ -> []
            )
        collect decls |> Map.ofList

    let ctx = { Builder = StringBuilder(); IndentLevel = 0; UnionCases = unionCases; GlobalBindings = globalBindings; TailCallArgs = None }
    appendLine ctx "using System;"
    appendLine ctx "using static BjolangRuntime;"
    
    // Emit 'using static' for all modules to allow unqualified access
    for decl in decls do
        match decl with
        | TModule (name, innerDecls, _) ->
            let isOuterDecl = function
                | TType _ | TTypeRec _ | TTrait _ | TImpl _ -> true
                | _ -> false
            let innerDeclsOnly = innerDecls |> List.filter (not << isOuterDecl)
            appendLine ctx $"using static %s{sanitizeIdent name}_Module;"
            for inner in innerDecls do
                match inner with
                | TImport (specs, _) ->
                    for spec in specs do
                        let moduleName =
                            match spec with
                            | RelativePath p -> System.IO.Path.GetFileNameWithoutExtension(p).Replace(".", "_").Replace("-", "_")
                            | ModulePath parts -> List.last parts |> fun p -> p.Replace(".", "_").Replace("-", "_")
                        appendLine ctx $"using static %s{moduleName}_Module;"
                | _ -> ()
        | _ -> ()
        
    if not (String.IsNullOrWhiteSpace(exportMetadata)) then
        let escapedMeta = exportMetadata.Replace("\"", "\\\"").Replace("\n", "\\n").Replace("\r", "")
        appendLine ctx $"[assembly: System.Reflection.AssemblyMetadata(\"BjolangExports\", \"%s{escapedMeta}\")]"
    
    if not dllDeps.IsEmpty then
        let depsStr = dllDeps |> List.map System.IO.Path.GetFullPath |> String.concat ";"
        appendLine ctx $"[assembly: System.Reflection.AssemblyMetadata(\"BjolangDeps\", \"%s{depsStr}\")]"
        
    appendLine ctx ""
    // Only generate code for the main module (the last one)
    if not decls.IsEmpty then
        let mainModule = List.last decls
        generateDecl ctx mainModule
    
    ctx.Builder.ToString()
