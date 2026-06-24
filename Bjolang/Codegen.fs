module Bjolang.Codegen

open System
open System.Text
open Bjolang.TypeChecker
open Bjolang.Parser

type CodegenContext = {
    Builder: StringBuilder
    IndentLevel: int
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
    | _ -> name

let rec typeToString (hm: HMType) : string =
    match hm with
    | TCon (name, args) ->
        let baseName = mapPrimitiveType name
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

let sanitizeIdent (s: string) =
    let s = s.Replace("-", "sub").Replace("?", "_QMARK").Replace("!", "_BANG").Replace("+", "add").Replace("*", "mul").Replace("/", "div").Replace("<", "lt").Replace(">", "gt").Replace("=", "eq")
    let s = if s.Length > 0 && Char.IsDigit(s[0]) then "_" + s else s
    match s with
    | "class" | "struct" | "public" | "private" | "protected" | "internal" | "static" | "readonly" | "var" | "ref" | "out" | "in" | "params" | "new" | "return" | "if" | "else" | "while" | "for" | "foreach" | "do" | "switch" | "case" | "default" | "break" | "continue" | "goto" | "try" | "catch" | "finally" | "throw" | "lock" | "typeof" | "sizeof" | "is" | "as" | "true" | "false" | "null" | "void" | "object" | "string" | "int" | "bool" -> "@" + s
    | _ -> s

let rec generateExpr (ctx: CodegenContext) (expr: TypedExpr) : unit =
    match expr.Node with
    | TInt i -> append ctx i
    | TString s -> 
        let escaped = s.Replace("\"", "\\\"").Replace("\n", "\\n")
        append ctx $"\"%s{escaped}\""
    | TKeyword k -> append ctx $"\"%s{k}\""
    | TSymbol s -> append ctx $"\"%s{s}\""
    | TIdent (name, _) ->
        match expr.Type with
        | TFun _ ->
            append ctx $"(({typeToString expr.Type})({sanitizeIdent name}))"
        | _ ->
            append ctx (sanitizeIdent name)
    | TApply (target, args, _) ->
        match target.Node with
        | TIdent (name, _) ->
            append ctx (sanitizeIdent name)
        | _ ->
            generateExpr ctx target
        append ctx "("
        for i, arg in List.indexed args do
            if i > 0 then append ctx ", "
            generateExpr ctx arg
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
        withIndent ctx (fun c -> generateBlock c Return body)
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
        append ctx (sanitizeIdent name)
        append ctx " with { "
        for i, (k, v) in List.indexed fields do
            if i > 0 then append ctx ", "
            append ctx (sanitizeIdent k)
            append ctx " = "
            generateExpr ctx v
        append ctx " }"
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
    | _ ->
        append ctx "/* Unimplemented expression node */"

and generateBlock (ctx: CodegenContext) (target: BlockTarget) (expr: TypedExpr) : unit =
    match expr.Node with
    | TLet (name, _, _, value, body) ->
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
    | TDefun (name, tyArgs, args, retType, body, _) ->
        indent ctx
        append ctx "public static "
        append ctx (typeToString retType)
        append ctx " "
        append ctx (sanitizeIdent name)
        if not tyArgs.IsEmpty then
            let tyArgsStr = tyArgs |> List.map (fun t -> if t.StartsWith("'") then "T_" + t.Substring(1) else "T_" + t) |> String.concat ", "
            append ctx $"<%s{tyArgsStr}>"
        append ctx "("
        for i, (argName, argType) in List.indexed args do
            if i > 0 then append ctx ", "
            append ctx (typeToString argType)
            append ctx " "
            append ctx (sanitizeIdent argName)
        append ctx ") {\n"
        withIndent ctx (fun ctx ->
            generateBlock ctx Return body
        )
        indent ctx
        appendLine ctx "}"

    | TDef (name, value, t, _) ->
        indent ctx
        append ctx "public static "
        append ctx (typeToString t)
        append ctx " "
        append ctx (sanitizeIdent name)
        append ctx " = "
        generateExpr ctx value
        appendLine ctx ";"

    | TType (defs, _) ->
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
                    append ctx (typeToString (TypeChecker.resolveTypeAnnotation Prelude.emptyRegistry f.Type)) // Hack: we need HMType, but TypeDef has FType
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
                                append ctx (typeToString (TypeChecker.resolveTypeAnnotation Prelude.emptyRegistry ft)) // FIXME: registry
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
            | TCon(n, _) -> n
            | _ -> "Unknown"
        let className = $"%s{traitName}_%s{targetTypeName}"
        
        let typeParams =
            match targetType with
            | TCon(_, args) ->
                args |> List.choose (function TVar v -> Some ("T_" + v.TrimStart('\'')) | _ -> None) |> List.distinct
            | _ -> []
        let tyParamsStr = if typeParams.IsEmpty then "" else "<" + String.concat ", " typeParams + ">"
        
        // Construct the interface arguments
        let targetTypeStr = typeToString targetType
        // TODO: We need the original TTrait to know the order of assoc types, but we don't have it easily here.
        // As a simplification, let's assume TImpl is compiled appropriately. We will need to map assocMap correctly.
        
        indent ctx
        appendLine ctx $"public sealed class %s{className}%s{tyParamsStr} : %s{traitName}<%s{targetTypeStr}> /* TODO assoc args */ {{"
        withIndent ctx (fun ctx ->
            indent ctx
            appendLine ctx $"public static readonly %s{className}%s{tyParamsStr} Instance = new();"
            for m in methods do
                match m with
                | TDefun (n, _, args, retType, body, _) ->
                    indent ctx
                    append ctx "public "
                    append ctx (typeToString retType)
                    append ctx " "
                    append ctx (sanitizeIdent n)
                    append ctx "("
                    for i, (argName, argType) in List.indexed args do
                        if i > 0 then append ctx ", "
                        append ctx (typeToString argType)
                        append ctx " "
                        append ctx (sanitizeIdent argName)
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
        let hasNonExtern = decls |> List.exists (function TExtern _ -> false | _ -> true)
        if hasNonExtern then
            indent ctx
            appendLine ctx $"public static class %s{sanitizeIdent name} {{"
            withIndent ctx (fun ctx ->
                for d in decls do
                    generateDecl ctx d
            )
            indent ctx
            appendLine ctx "}"

    | _ -> ()


let generateProgram (exportMetadata: string) (decls: TDecl list) : string =
    let ctx = { Builder = StringBuilder(); IndentLevel = 0 }
    appendLine ctx "using System;"
    appendLine ctx "using System.Collections.Generic;"
    appendLine ctx "using static BjolangRuntime;"
    
    // Emit 'using static' for all modules to allow unqualified access
    for decl in decls do
        match decl with
        | TModule (name, _, _) -> appendLine ctx $"using static %s{sanitizeIdent name};"
        | _ -> ()
        
    if not (String.IsNullOrWhiteSpace(exportMetadata)) then
        let escapedMeta = exportMetadata.Replace("\"", "\\\"")
        appendLine ctx $"[assembly: System.Reflection.AssemblyMetadata(\"BjolangExports\", \"%s{escapedMeta}\")]"
        
    appendLine ctx ""
    for decl in decls do
        generateDecl ctx decl
    ctx.Builder.ToString()
