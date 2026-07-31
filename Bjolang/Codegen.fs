module Bjolang.Codegen

open System
open System.Text
open Bjolang.TypedAST
open Bjolang.Parser

type UnionCaseInfo = {
    ParentTypeName: string
    IsDataCase: bool
}

/// The loop a `TRecur` may jump to.
type LoopScope = {
    Members: TLoopMember list
    /// Set when the group was merged into a single switch-dispatched local
    /// function because its members tail-call each other.
    Merged: bool
    /// The state discriminant of a merged group.
    StateVar: string
    /// `switch` statements entered since the group's own dispatch switch. A
    /// `goto case` binds to the *nearest* enclosing switch, so a jump from
    /// inside a nested one has to go through the discriminant instead.
    NestedSwitches: int
}

type CodegenContext = {
    Builder: StringBuilder
    IndentLevel: int
    UnionCases: Map<string, UnionCaseInfo>
    GlobalBindings: Map<string, string>
    /// Where `generateExpr` may hoist statement-shaped operands to. `None` in
    /// the three contexts C# gives no statement position: optional-parameter
    /// defaults, `case ... when` guards, and switch-expression arms.
    Prelude: ResizeArray<string> option
    /// The innermost loop in scope.
    Loop: LoopScope option
    /// Type parameters the enclosing method or class already introduced.
    TypeParams: Set<string>
}

let inline append (ctx: CodegenContext) (s: string) =
    ctx.Builder.Append(s) |> ignore

let inline appendLine (ctx: CodegenContext) (s: string) =
    ctx.Builder.AppendLine(s) |> ignore

let inline indent (ctx: CodegenContext) =
    ctx.Builder.Append(String(' ', ctx.IndentLevel * 4)) |> ignore

let withIndent (ctx: CodegenContext) (f: CodegenContext -> unit) =
    f { ctx with IndentLevel = ctx.IndentLevel + 1 }

/// A user-facing code generation failure. A loud error at compile time beats
/// invalid generated C#, a silent wrong answer, or a stack overflow at run time.
let codegenError (line: int) (message: string) : 'a =
    failwithf $"Codegen Error at line %d{line}: %s{message}"

let private nameCounter = ref 0

let private freshName (prefix: string) =
    nameCounter.Value <- nameCounter.Value + 1
    $"%s{prefix}%d{nameCounter.Value}"

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
    | "List" -> "SchemeList.SchemeList"
    | _ -> name

let sanitizeIdent (s: string) =
    let s = s.Replace("::", ".").Replace("-", "sub").Replace("?", "_QMARK").Replace("!", "_BANG").Replace("+", "add").Replace("*", "mul").Replace("/", "div").Replace("<", "lt").Replace(">", "gt").Replace("=", "eq").Replace("'", "")
    let s = if s.Length > 0 && Char.IsDigit(s[0]) then "_" + s else s
    match s with
    | "class" | "struct" | "public" | "private" | "protected" | "internal" | "static" | "readonly" | "var" | "ref" | "out" | "in" | "params" | "new" | "return" | "if" | "else" | "while" | "for" | "foreach" | "do" | "switch" | "case" | "default" | "break" | "continue" | "goto" | "try" | "catch" | "finally" | "throw" | "lock" | "typeof" | "sizeof" | "is" | "as" | "true" | "false" | "null" | "void" | "object" | "string" | "int" | "bool" -> "@" + s
    | _ -> s

/// The C# class a module's declarations are emitted into.
///
/// A module is named after its source file, so the name can hold characters no
/// C# identifier may hold — or start with a digit, as `06_lib.bjo` does. Every
/// site that spells this class has to agree on the answer: the class definition,
/// the `using static` for it, a qualified reference to one of its bindings, and
/// the generated entry point.
let moduleClassName (moduleName: string) =
    sanitizeIdent (moduleName.Replace(".", "_").Replace("-", "_")) + "_Module"

/// The C# spelling of a Bjolang type parameter.
let typeParamName (name: string) = "T_" + name.TrimStart('\'')

/// The canonical key a type parameter is tracked under, independent of whether
/// the source wrote it quoted.
let typeParamKey (name: string) = name.TrimStart('\'')

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
    | TVar name -> typeParamName name
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

let private isVoidType (t: HMType) = typeToString t = "void"

/// The C# element type of a single-argument container type such as the runtime
/// `SchemeList<T>` or `Vec<T>`.
///
/// Falls back to `object` when the type did not resolve to a one-argument
/// constructor, which keeps the emitted C# well-formed rather than propagating
/// an inference failure into codegen.
let private elementTypeString (t: HMType) =
    match t with
    | TCon (_, [ elemT ]) -> typeToString elemT
    | _ -> "object"

/// Every type variable mentioned by `t`, in source spelling.
let rec collectTypeVars (t: HMType) : string list =
    match t with
    | TVar name -> [ name ]
    | TFun (args, ret) -> (args |> List.collect collectTypeVars) @ collectTypeVars ret
    | TCon (_, args) -> args |> List.collect collectTypeVars
    | TTuple types -> types |> List.collect collectTypeVars
    | TMeta m ->
        match m.Value with
        | Some t' -> collectTypeVars t'
        | None -> []
    | TAssoc (_, _, implType) -> collectTypeVars implType

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

// ---------------------------------------------------------------------------
// Statement shape
// ---------------------------------------------------------------------------

/// True when *this node* has no C# expression form and `generateExpr` therefore
/// has to hoist it into a preceding statement.
///
/// A node whose operands merely *contain* something statement-shaped is not
/// itself statement-shaped: those operands are hoisted individually, which keeps
/// the node an expression.
let rec isStatementShaped (expr: TypedExpr) : bool =
    match expr.Node with
    | TLet _
    | TLetRec _
    | TLetTuple _
    | TLetMutable _
    | TSet _
    | TThrow _
    | TTryFinally _
    | TVecMake _
    | TLoop _
    | TRecur _ -> true

    // A conditional stays `c ? t : f` as long as it yields a value and neither
    // arm needs statements. Hoisting out of an arm would evaluate it
    // unconditionally, so an arm that needs a statement forces the whole node
    // into an `if`. The condition is evaluated unconditionally, so whatever it
    // hoists can safely go ahead of the conditional.
    | TIf (_, t, f) -> isVoidType expr.Type || containsHoist t || containsHoist f

    // A `switch` expression cannot yield void, cannot contain the `continue` or
    // `goto` a jump compiles to, and gives its arms and guards no statement
    // position of their own.
    | TMatch (_, clauses) ->
        isVoidType expr.Type
        || liveClauses clauses
           |> List.exists (fun c ->
               containsHoist c.Body
               || (c.Guard |> Option.map containsHoist |> Option.defaultValue false))

    | _ -> false

/// True when evaluating `expr` will hoist statements into the enclosing
/// statement position — which moves that work earlier than the expression it
/// came from.
///
/// A lambda body is a block of its own, so nothing inside one can need a
/// statement position out here.
and containsHoist (expr: TypedExpr) : bool =
    isStatementShaped expr
    || match expr.Node with
       | TLambda _ -> false
       | _ -> TypeVisitor.children expr |> List.exists containsHoist

/// Translates a typed pattern into C# pattern syntax.
let rec generatePattern (ctx: CodegenContext) (pat: TypedPattern) : unit =
    match pat.Node with
    | TPWildcard -> append ctx "_"
    | TPIdent name -> append ctx $"var {sanitizeIdent name}"
    | TPInt value -> append ctx value
    | TPString value -> append ctx $"\"%s{escapeStringLiteral value}\""
    | TPConstruct (name, args) ->
        // Cons/Nil are now builtins backed by SchemeList.Cons<T>/SchemeList.Nil<T>,
        // not union cases, so they need special-case pattern generation.
        let caseTypeStr =
            match name with
            | "Cons" ->
                let elemTypeStr = elementTypeString pat.Type
                $"SchemeList.Cons<%s{elemTypeStr}>"
            | "Nil" ->
                let elemTypeStr = elementTypeString pat.Type
                $"SchemeList.Nil<%s{elemTypeStr}>"
            | _ ->
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
        // Lists are backed by SchemeList.SchemeList<T>. Desugar into nested
        // type patterns against the runtime Cons<T>/Nil<T> classes.
        let elemTypeStr = elementTypeString pat.Type
        let listTypeStr = typeToString pat.Type
        let rec desugar elements =
            match elements with
            | [] ->
                match tailOpt with
                | Some t -> generatePattern ctx t
                | None -> append ctx $"SchemeList.Nil<%s{elemTypeStr}>"
            | head :: rest ->
                append ctx $"SchemeList.Cons<%s{elemTypeStr}>("
                generatePattern ctx head
                append ctx ", "
                desugar rest
                append ctx ")"
        desugar items
    | TPVec (items, tailOpt) ->
        // Vec is backed by Collections.RrbList<T>, which is countable, indexable
        // and sliceable, so C# list patterns apply directly. A rest pattern
        // becomes a slice pattern, whose value Slice() hands back as an RrbList<T>.
        append ctx "["
        for i, item in List.indexed items do
            if i > 0 then append ctx ", "
            generatePattern ctx item
        match tailOpt with
        | Some t ->
            if not items.IsEmpty then append ctx ", "
            append ctx ".. "
            generatePattern ctx t
        | None -> ()
        append ctx "]"
    | TPAs _ ->
        failwithf $"'as' patterns have no C# equivalent (line %d{pat.Range.Start.Line})"
    | TPApp _ ->
        failwithf $"Applied patterns are not supported by the C# backend (line %d{pat.Range.Start.Line})"

// ---------------------------------------------------------------------------
// Expressions and statements
// ---------------------------------------------------------------------------

let rec generateExpr (ctx: CodegenContext) (expr: TypedExpr) : unit =
    match ctx.Prelude with
    | Some prelude when isStatementShaped expr -> append ctx (hoistToTemp ctx prelude expr)
    | None when containsHoist expr ->
        codegenError
            expr.Range.Start.Line
            "this expression needs statements to evaluate, but it appears where C# has no statement position"
    | _ ->

    match expr.Node with
    | TInt i -> append ctx i
    | TString s -> append ctx $"\"%s{escapeStringLiteral s}\""
    | TKeyword k -> append ctx $"\"%s{k}\""
    | TSymbol s -> append ctx $"\"%s{s}\""
    | TIdent (name, _) ->
        // Cons/Nil are now builtins backed by SchemeList, not union cases.
        match name with
        | "Nil" ->
            let elemTypeStr = elementTypeString expr.Type
            append ctx $"Nil<%s{elemTypeStr}>()"
        | "Cons" ->
            match expr.Type with
            | TFun (argTypes, _) ->
                // First-class function value: emit a lambda.
                let argsList = [for i in 0 .. argTypes.Length - 1 -> $"arg{i}"]
                let argsStr = String.concat ", " argsList
                append ctx $"({argsStr}) => Cons({argsStr})"
            | _ ->
                // Should not happen (Cons always has function type), but safe fallback
                append ctx "Cons"
        | _ ->
        match Map.tryFind name ctx.UnionCases with
        | Some info ->
            let typeStr = getUnionTypeString expr.Type info.ParentTypeName
            if info.IsDataCase then
                match expr.Type with
                | TFun (argTypes, _) ->
                    // A genuine first-class function value. Roslyn caches
                    // no-capture lambdas, so this allocates once per program.
                    let argsList = [for i in 0 .. argTypes.Length - 1 -> $"arg{i}"]
                    let argsStr = String.concat ", " argsList
                    append ctx $"({argsStr}) => new {typeStr}.{sanitizeIdent name}({argsStr})"
                | _ ->
                    append ctx $"new {typeStr}.{sanitizeIdent name}()"
            else
                append ctx $"new {typeStr}.{sanitizeIdent name}()"
        | None ->
            let targetName = qualifiedName ctx name
            match expr.Type with
            | TFun _ ->
                // A delegate-typed cast of a value or method group; Roslyn caches
                // method-group conversions too.
                append ctx $"(({typeToString expr.Type})({targetName}))"
            | _ ->
                append ctx targetName

    | TApply (target, args, kwArgs) ->
        generateApply ctx expr target args kwArgs

    | TInterfaceCall (iType, mName, dict, args) ->
        let emitters = prepareOperands ctx (dict :: args)
        emitters.Head ctx
        append ctx "."
        append ctx (sanitizeIdent mName)
        append ctx "("
        for i, emit in List.indexed emitters.Tail do
            if i > 0 then append ctx ", "
            emit ctx
        append ctx ")"

    | TLambda (args, body) ->
        append ctx "("
        append ctx (args |> List.map sanitizeIdent |> String.concat ", ")
        append ctx ") => {\n"
        // A lambda is its own function scope: it has no access to the enclosing
        // loop's slots, and a `continue` inside it would bind to nothing.
        let inner = { ctx with Prelude = None; Loop = None }
        withIndent inner (fun c -> generateBlock c Return body)
        indent ctx; append ctx "}"

    | TIf (cond, t, f) ->
        // Reached only when nothing inside needs a statement position. Both arms
        // are cast to the conditional's own type: C#'s "best common type" rule
        // rejects arms typed at different subclasses of a union (CS0173).
        let resultType = typeToString expr.Type
        append ctx "("
        generateExpr ctx cond
        append ctx $" ? (%s{resultType})("
        generateExpr ctx t
        append ctx $") : (%s{resultType})("
        generateExpr ctx f
        append ctx "))"

    | TTupleMake args ->
        append ctx "("
        for i, emit in List.indexed (prepareOperands ctx args) do
            if i > 0 then append ctx ", "
            emit ctx
        append ctx ")"

    | TRecordMake fields ->
        append ctx $"new %s{typeToString expr.Type}("
        for i, emit in List.indexed (prepareOperands ctx (fields |> List.map snd)) do
            if i > 0 then append ctx ", "
            emit ctx
        append ctx ")"

    | TRecordUpdate (name, fields) ->
        // `with` binds loosely, so parenthesize: `(r with { .. }).field` must not
        // parse as `r with { .. field }`.
        let emitters = prepareOperands ctx (fields |> List.map snd)
        append ctx "("
        append ctx (qualifiedName ctx name)
        append ctx " with { "
        for i, ((k, _), emit) in List.indexed (List.zip fields emitters) do
            if i > 0 then append ctx ", "
            append ctx (sanitizeIdent k)
            append ctx " = "
            emit ctx
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

    | TListMake items ->
        // Desugar to nested SchemeList.Cons / SchemeList.Empty calls.
        let elemTypeStr = elementTypeString expr.Type
        let emitters = prepareOperands ctx items
        let rec emitCons remaining =
            match remaining with
            | [] -> append ctx $"SchemeList.SchemeList.Empty<%s{elemTypeStr}>()"
            | emit :: rest ->
                append ctx "SchemeList.SchemeList.Cons("
                emit ctx
                append ctx ", "
                emitCons rest
                append ctx ")"
        emitCons emitters

    | TArrayMake items ->
        let elementTypeStr =
            match expr.Type with
            | TCon ("Array", [ elemT ]) -> typeToString elemT
            | _ -> "object"
        append ctx $"new %s{elementTypeStr}[] {{ "
        for i, emit in List.indexed (prepareOperands ctx items) do
            if i > 0 then append ctx ", "
            emit ctx
        append ctx " }"

    | TMatch (matchTarget, clauses) ->
        // Reached only when every live arm and guard is expression-shaped.
        let live = liveClauses clauses
        generateExpr ctx matchTarget
        appendLine ctx " switch {"
        withIndent ctx (fun c ->
            // Arms have no statement position of their own.
            let armCtx = { c with Prelude = None }
            for clause in live do
                indent armCtx
                generatePattern armCtx clause.Pattern
                match clause.Guard with
                | Some guard ->
                    append armCtx " when "
                    generateExpr armCtx guard
                | None -> ()
                append armCtx " => "
                generateExpr armCtx clause.Body
                appendLine armCtx ","
            if not (live |> List.exists isIrrefutable) then
                indent armCtx
                appendLine armCtx $"_ => throw new Exception(\"Match failure at line %d{expr.Range.Start.Line}\")"
        )
        indent ctx; append ctx "}"

    | TTypeEq _ ->
        codegenError expr.Range.Start.Line "type equality tests are not supported by the C# backend"

    | TThrow _
    | TVecMake _
    | TLet _
    | TLetRec _
    | TLetTuple _
    | TLetMutable _
    | TSet _
    | TTryFinally _
    | TLoop _
    | TRecur _ ->
        // Statement-shaped: `needsHoist` has already routed these away.
        codegenError expr.Range.Start.Line "internal error: statement-shaped node reached expression emission"

/// Fully qualifies a module-level binding.
and private qualifiedName (ctx: CodegenContext) (name: string) =
    match Map.tryFind name ctx.GlobalBindings with
    | Some modName -> $"%s{moduleClassName modName}.%s{sanitizeIdent name}"
    | None -> sanitizeIdent name

/// Evaluates a statement-shaped node into a temporary in the enclosing statement
/// position and yields the temporary's name.
and private hoistToTemp (ctx: CodegenContext) (prelude: ResizeArray<string>) (expr: TypedExpr) : string =
    let tmp = freshName "__hoist"
    let scratch = StringBuilder()
    let inner = { ctx with Builder = scratch; Prelude = None }

    if isVoidType expr.Type then
        generateBlock inner Discard expr
    else
        generateBlock inner (DeclareAndAssign(typeToString expr.Type, tmp)) expr

    // Anything the node hoisted in turn is already inside `scratch`, ahead of the
    // node's own statements, so appending as one unit preserves the order.
    prelude.Add(scratch.ToString())
    tmp

/// Emits the operands of a single construct, preserving left-to-right evaluation.
///
/// Hoisting a node out of the middle of an operand list moves its evaluation
/// earlier, so every operand up to and including the last hoisted one is pulled
/// into a temporary too. There is no purity information in the typed AST, so no
/// operand is exempted.
///
/// The list must be given in *source* order; the returned emitters may be used
/// in any order, which is what `TApply`'s keyword branch needs.
and private prepareOperands (ctx: CodegenContext) (operands: TypedExpr list) : (CodegenContext -> unit) list =
    let hoisted =
        match ctx.Prelude with
        | Some prelude when operands |> List.exists containsHoist ->
            let lastHoisted =
                operands
                |> List.mapi (fun i e -> i, containsHoist e)
                |> List.filter snd
                |> List.map fst
                |> List.max

            Some(prelude, lastHoisted)
        | _ -> None

    match hoisted with
    | None -> operands |> List.map (fun operand -> fun (c: CodegenContext) -> generateExpr c operand)
    | Some(prelude, lastHoisted) ->
        operands
        |> List.mapi (fun i operand ->
            if i <= lastHoisted then
                let tmp = hoistToTemp ctx prelude operand
                fun (c: CodegenContext) -> append c tmp
            else
                fun (c: CodegenContext) -> generateExpr c operand)

and private generateApply
    (ctx: CodegenContext)
    (expr: TypedExpr)
    (target: TypedExpr)
    (args: TypedExpr list)
    (kwArgs: (string * TypedExpr) list)
    : unit =

    match target.Node with
    | TIdent (name, _) when Map.containsKey name ctx.UnionCases ->
        let info = Map.find name ctx.UnionCases
        let typeStr = getUnionTypeString expr.Type info.ParentTypeName
        append ctx $"new {typeStr}.{sanitizeIdent name}("
        for i, emit in List.indexed (prepareOperands ctx args) do
            if i > 0 then append ctx ", "
            emit ctx
        append ctx ")"

    | TIdent (name, _) when List.contains name ["+"; "-"; "*"; "/"; "%"; "<"; ">"; "<="; ">="] && args.Length = 2 && kwArgs.IsEmpty ->
        let emitters = prepareOperands ctx args
        append ctx "("
        emitters[0] ctx
        append ctx $" {name} "
        emitters[1] ctx
        append ctx ")"

    | TIdent ("=", _) when args.Length = 2 && kwArgs.IsEmpty ->
        let emitters = prepareOperands ctx args
        append ctx "("
        emitters[0] ctx
        append ctx " == "
        emitters[1] ctx
        append ctx ")"

    | _ ->
        // The callee is evaluated first, so it joins the operand list in source
        // order ahead of the arguments.
        let calleeIsIdent =
            match target.Node with
            | TIdent _ -> true
            | _ -> false

        let sourceOperands =
            (if calleeIsIdent then [] else [ target ]) @ args @ (kwArgs |> List.map snd)

        let emitters = prepareOperands ctx sourceOperands

        let calleeEmitters, argEmitters =
            if calleeIsIdent then [], emitters else [ emitters.Head ], emitters.Tail

        let positionalEmitters = argEmitters |> List.truncate args.Length
        let keywordEmitters = argEmitters |> List.skip args.Length

        match target.Node with
        | TIdent (name, tArgs) ->
            append ctx (qualifiedName ctx name)
            if not tArgs.IsEmpty && args.IsEmpty && kwArgs.IsEmpty then
                let tyArgsStr = tArgs |> List.map typeToString |> String.concat ", "
                append ctx $"<%s{tyArgsStr}>"
        | TLambda _ ->
            // A lambda literal has no type of its own. C# infers one from an
            // argument or assignment context, but a callee position gives it
            // nothing to infer from and `(x) => { … }(a)` is rejected (CS0149),
            // so the delegate type has to be written out.
            append ctx $"(({typeToString target.Type})("
            calleeEmitters.Head ctx
            append ctx "))"
        | _ -> calleeEmitters.Head ctx

        append ctx "("
        if kwArgs.IsEmpty then
            for i, emit in List.indexed positionalEmitters do
                if i > 0 then append ctx ", "
                emit ctx
        else
            // Function type is TFun([mandatory..., keyword..., rest?], ret) and the
            // positional arguments are mandatory ++ rest, so the split point is
            // derived from the callee's arity rather than the call's shape.
            let funArgCount =
                match target.Type with
                | TFun (argTypes, _) -> argTypes.Length
                | _ -> args.Length + kwArgs.Length

            let hasRest =
                match target.Type with
                | TFun (argTypes, _) when not argTypes.IsEmpty ->
                    match List.last argTypes with
                    | TCon ("Array", _) -> true
                    | _ -> false
                | _ -> false

            let mandatoryCount = funArgCount - kwArgs.Length - (if hasRest then 1 else 0)
            let split = min positionalEmitters.Length mandatoryCount
            let mandatoryEmitters = positionalEmitters |> List.truncate split
            let restEmitters = positionalEmitters |> List.skip split

            let mutable argIdx = 0

            for emit in mandatoryEmitters do
                if argIdx > 0 then append ctx ", "
                emit ctx
                argIdx <- argIdx + 1

            if hasRest then
                // C# forbids mixing named arguments with a `params` expansion.
                for emit in keywordEmitters do
                    if argIdx > 0 then append ctx ", "
                    emit ctx
                    argIdx <- argIdx + 1
            else
                for (kwName, _), emit in List.zip kwArgs keywordEmitters do
                    if argIdx > 0 then append ctx ", "
                    append ctx $"%s{sanitizeIdent kwName}: "
                    emit ctx
                    argIdx <- argIdx + 1

            for emit in restEmitters do
                if argIdx > 0 then append ctx ", "
                emit ctx
                argIdx <- argIdx + 1
        append ctx ")"

/// Emits one statement, giving `generateExpr` somewhere to hoist statement-shaped
/// operands to. The statement is built into a scratch buffer first so that the
/// hoisted statements can be written ahead of it — including ahead of its indent.
and private emitStatement (ctx: CodegenContext) (build: CodegenContext -> unit) : unit =
    let prelude = ResizeArray<string>()
    let scratch = StringBuilder()

    build { ctx with Builder = scratch; Prelude = Some prelude }

    for stmt in prelude do
        ctx.Builder.Append(stmt) |> ignore

    ctx.Builder.Append(scratch) |> ignore

and generateBlock (ctx: CodegenContext) (target: BlockTarget) (expr: TypedExpr) : unit =
    match expr.Node with
    | TRecur (index, args) -> generateRecur ctx target expr index args

    | TLoop (members, bodyOpt) ->
        match bodyOpt with
        | Some body ->
            generateLoopGroup ctx members body
            generateBlock ctx target body
        | None ->
            codegenError
                expr.Range.Start.Line
                "internal error: a function-body loop was emitted outside of a function body"

    | TLet (name, isFun, _, value, body) ->
        // `LetRecify` only emits `ELet` for a singleton component with no
        // self-edge, so a function-shaped binding here is always a
        // *non-recursive* local function and needs no loop.
        let asLocalFunction =
            if isFun then
                match value.Node, value.Type with
                | TLambda (lambdaArgs, lambdaBody), TFun (argTypes, retType) ->
                    Some(lambdaArgs, argTypes, retType, lambdaBody)
                | _ -> None
            else
                None

        match asLocalFunction with
        | Some (lambdaArgs, argTypes, retType, lambdaBody) ->
            generateLocalFunction ctx name lambdaArgs argTypes retType lambdaBody value.Type
        | None ->
            if isVoidType value.Type then
                generateBlock ctx Discard value
            else
                generateBlock ctx (DeclareAndAssign(typeToString value.Type, sanitizeIdent name)) value

        generateBlock ctx target body

    | TLetRec (bindings, body) ->
        // A group `LoopLowering` declined to turn into a loop: bindings that are
        // not functions and so have nothing to jump to.
        for (name, _, _, value) in bindings do
            indent ctx
            appendLine ctx $"{typeToString value.Type} {sanitizeIdent name} = default!;"
        for (name, _, _, value) in bindings do
            generateBlock { ctx with Loop = None } (Assign(sanitizeIdent name)) value
        generateBlock ctx target body

    | TLetMutable (name, value, body) ->
        generateBlock ctx (DeclareAndAssign(typeToString value.Type, sanitizeIdent name)) value
        generateBlock ctx target body

    | TSet (name, value) ->
        generateBlock ctx (Assign(sanitizeIdent name)) value
        // `set!` itself yields void, so the enclosing target still has to be
        // discharged.
        match target with
        | Return -> indent ctx; appendLine ctx "return;"
        | Assign _
        | DeclareAndAssign _
        | Discard -> ()

    | TLetTuple (names, value, body) ->
        let tmp = freshName "__tuple"
        generateBlock ctx (DeclareAndAssign(typeToString value.Type, tmp)) value
        for i, name in List.indexed names do
            indent ctx
            appendLine ctx $"var %s{sanitizeIdent name} = %s{tmp}.Item%d{i + 1};"
        generateBlock ctx target body

    | TTryFinally (body, cleanup) ->
        // The declaration has to live outside the `try` or the assignment would
        // not be visible to anything following it.
        let bodyTarget =
            match target with
            | DeclareAndAssign (varType, varName) ->
                indent ctx; appendLine ctx $"%s{varType} %s{varName};"
                Assign varName
            | other -> other

        indent ctx; appendLine ctx "try {"
        withIndent ctx (fun c -> generateBlock c bodyTarget body)
        indent ctx; appendLine ctx "} finally {"
        withIndent ctx (fun c -> generateBlock c Discard cleanup)
        indent ctx; appendLine ctx "}"

    | TVecMake items ->
        let elementTypeStr = elementTypeString expr.Type

        let builder = freshName "__vec"
        indent ctx; appendLine ctx $"var %s{builder} = new Collections.RrbBuilder<%s{elementTypeStr}>();"
        for item in items do
            emitStatement ctx (fun c ->
                indent c
                append c $"%s{builder}.Add("
                generateExpr c item
                appendLine c ");")
        emitTerminal ctx target expr.Type (fun c -> append c $"%s{builder}.ToImmutable()")

    | TIf (cond, t, f) ->
        let armTarget =
            match target with
            | DeclareAndAssign (varType, varName) ->
                indent ctx; appendLine ctx $"{varType} {varName};"
                Assign varName
            | other -> other

        emitStatement ctx (fun c ->
            indent c
            append c "if ("
            generateExpr c cond
            appendLine c ") {")
        withIndent ctx (fun c -> generateBlock c armTarget t)
        indent ctx; appendLine ctx "} else {"
        withIndent ctx (fun c -> generateBlock c armTarget f)
        indent ctx; appendLine ctx "}"

    | TThrow msgExpr ->
        // A `throw` never reaches the declaration's use, but C# still wants the
        // variable to exist for the statements that follow.
        match target with
        | DeclareAndAssign (varType, varName) ->
            indent ctx; appendLine ctx $"%s{varType} %s{varName} = default!;"
        | _ -> ()

        emitStatement ctx (fun c ->
            indent c
            append c "throw new Exception("
            generateExpr c msgExpr
            appendLine c ");")

    | TMatch (matchTarget, clauses) -> generateMatch ctx target expr matchTarget clauses

    // Any node with no statement shape of its own: emit it as a C# expression
    // and let `emitTerminal` discharge the target. The `emitStatement` wrapper
    // supplies the hoisting buffer that `generateExpr` may need.
    | _ -> emitStatement ctx (fun c -> emitTerminal c target expr.Type (fun c2 -> generateExpr c2 expr))

/// Discharges `target` with an already-formed C# expression fragment.
///
/// A void-typed value cannot be assigned or returned in C#, so under every
/// target that would bind it the value is emitted as a bare statement instead.
/// `Return` additionally needs a following `return;`, since the target still has
/// to be discharged.
and private emitTerminal (ctx: CodegenContext) (target: BlockTarget) (valueType: HMType) (emit: CodegenContext -> unit) : unit =
    let isVoid = isVoidType valueType
    indent ctx
    match target with
    | Return ->
        if isVoid then
            emit ctx; appendLine ctx ";"
            indent ctx; appendLine ctx "return;"
        else
            append ctx "return "; emit ctx; appendLine ctx ";"
    | Assign name ->
        if isVoid then
            emit ctx; appendLine ctx ";"
        else
            append ctx $"%s{name} = "; emit ctx; appendLine ctx ";"
    | DeclareAndAssign (varType, varName) ->
        if isVoid then
            emit ctx; appendLine ctx ";"
        else
            append ctx $"%s{varType} %s{varName} = "; emit ctx; appendLine ctx ";"
    | Discard -> emit ctx; appendLine ctx ";"

and private generateMatch
    (ctx: CodegenContext)
    (target: BlockTarget)
    (expr: TypedExpr)
    (matchTarget: TypedExpr)
    (clauses: TMatchClause list)
    : unit =

    // Emitted as a switch *statement* so that arms may contain statements,
    // produce void, or jump into the enclosing loop.
    let live = liveClauses clauses

    // C# only treats a switch statement as exhaustive when it has a `default`
    // section, and `case _:` is not legal syntax, so a trailing irrefutable
    // clause is emitted as the default section instead of a case.
    let irrefutableTail, cases =
        match List.rev live with
        | last :: revRest when isIrrefutable last -> Some last, List.rev revRest
        | _ -> None, live

    // `default:` carries no pattern, so an irrefutable `TPIdent` clause needs the
    // scrutinee hoisted into a local that it can alias.
    let needsTemp =
        match irrefutableTail with
        | Some c ->
            match c.Pattern.Node with
            | TPIdent _ -> true
            | _ -> false
        | None -> false

    let scrutinee =
        if needsTemp then
            let tmp = freshName "__match"
            emitStatement ctx (fun c ->
                indent c
                append c $"var %s{tmp} = "
                generateExpr c matchTarget
                appendLine c ";")
            Some tmp
        else
            None

    // A `goto case` binds to the nearest enclosing switch, so a jump from inside
    // this one has to route through the loop's discriminant instead.
    let inner =
        { ctx with
            Loop = ctx.Loop |> Option.map (fun l -> { l with NestedSwitches = l.NestedSwitches + 1 }) }

    let generateGuard (c: CodegenContext) (guard: TypedExpr) =
        if containsHoist guard then
            codegenError
                guard.Range.Start.Line
                "this `match` guard needs statements to evaluate, but C# gives `case ... when` no statement position; move the test into the arm body"

        append c " when "
        generateExpr { c with Prelude = None } guard

    let emitSwitch (armTarget: BlockTarget) =
        // A `Return` target always terminates the section itself
        // (return / continue / goto / throw), so a break would be unreachable.
        let emitBreak cb =
            match armTarget with
            | Return -> ()
            | _ -> indent cb; appendLine cb "break;"

        emitStatement ctx (fun c ->
            indent c
            append c "switch ("
            (match scrutinee with
             | Some tmp -> append c tmp
             | None -> generateExpr c matchTarget)
            appendLine c ") {")

        withIndent inner (fun c ->
            for clause in cases do
                indent c
                append c "case "
                generatePattern c clause.Pattern
                clause.Guard |> Option.iter (generateGuard c)
                // Each section gets its own block so locals declared by different
                // arms cannot collide in the shared switch scope.
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
                appendLine c $"default: throw new Exception(\"Match failure at line %d{expr.Range.Start.Line}\");")

        indent ctx; appendLine ctx "}"

    match target with
    | DeclareAndAssign (varType, varName) ->
        indent ctx; appendLine ctx $"{varType} {varName};"
        emitSwitch (Assign varName)
    | _ -> emitSwitch target

// ---------------------------------------------------------------------------
// Loops
// ---------------------------------------------------------------------------

and private generateRecur
    (ctx: CodegenContext)
    (target: BlockTarget)
    (expr: TypedExpr)
    (index: int)
    (args: TypedExpr list)
    : unit =

    let loop =
        match ctx.Loop with
        | Some l -> l
        | None ->
            codegenError expr.Range.Start.Line "internal error: a loop jump was emitted with no loop in scope"

    // A jump discards the enclosing function's remaining work, so it is only
    // meaningful where that function's value is produced. Under `Assign` the
    // variable would be left unset and the following `break` unreachable.
    match target with
    | Return -> ()
    | _ ->
        codegenError
            expr.Range.Start.Line
            "this jump is not in the value position of its loop, so the loop's result would be left unset"

    let member_ = loop.Members[index]
    let slots = member_.Slots |> List.map (fst >> sanitizeIdent)

    if slots.Length <> args.Length then
        codegenError
            expr.Range.Start.Line
            $"internal error: jump to '%s{member_.LoopName}' carries %d{args.Length} arguments for %d{slots.Length} slots"

    // The whole vector is evaluated before any slot is written: an argument may
    // read a slot that an earlier assignment would already have overwritten.
    let temps = args |> List.map (fun _ -> freshName "__next")

    for arg, tmp in List.zip args temps do
        emitStatement ctx (fun c ->
            indent c
            append c $"var %s{tmp} = "
            generateExpr c arg
            appendLine c ";")

    for slot, tmp in List.zip slots temps do
        indent ctx; appendLine ctx $"%s{slot} = %s{tmp};"

    if loop.Merged then
        // `goto case` is a direct jump to another switch section rather than a
        // re-dispatch through the discriminant, so prefer it where it is legal.
        if loop.NestedSwitches = 0 then
            indent ctx; appendLine ctx $"goto case %d{index};"
        else
            indent ctx; appendLine ctx $"%s{loop.StateVar} = %d{index};"
            indent ctx; appendLine ctx "continue;"
    else
        indent ctx; appendLine ctx "continue;"

/// Copies each slot into a fresh per-iteration local. Done unconditionally: the
/// JIT elides the copy when nothing captures it, whereas an escape analysis
/// would be a correctness liability to maintain.
and private emitIterationCopies (ctx: CodegenContext) (member_: TLoopMember) : unit =
    for (slot, _), local in List.zip member_.Slots member_.Locals do
        indent ctx
        appendLine ctx $"var %s{sanitizeIdent local} = %s{sanitizeIdent slot};"

/// Emits `TLoop (_, None)`: the loop *is* this function's body, so the
/// `while (true)` lives in the function's own block and its slots are the
/// function's own parameters.
and private generateFunctionBody (ctx: CodegenContext) (body: TypedExpr) : unit =
    match body.Node with
    | TLoop ([ member_ ], None) ->
        indent ctx; appendLine ctx "while (true) {"
        withIndent ctx (fun c ->
            let inner =
                { c with
                    Loop = Some { Members = [ member_ ]; Merged = false; StateVar = ""; NestedSwitches = 0 } }

            emitIterationCopies inner member_
            generateBlock inner Return member_.Body)
        indent ctx; appendLine ctx "}"
    | _ -> generateBlock ctx Return body

/// Emits a `letrec` group as C# local functions.
and private generateLoopGroup (ctx: CodegenContext) (members: TLoopMember list) (body: TypedExpr) : unit =
    let targetsOf (m: TLoopMember) = LoopLowering.recurTargetsIn m.Body

    let jumpedTo =
        members |> List.fold (fun acc m -> Set.union acc (targetsOf m)) Set.empty

    // A jump between members is not a call, so a member the group's body never
    // names is only *entered* by its siblings — it needs no callable form. The
    // fixpoint drops members reachable solely from other unreachable ones.
    let called =
        let allNames = members |> List.map (fun m -> m.LoopName)

        let rec fix (live: Set<string>) =
            let referenced =
                members
                |> List.filter (fun m -> Set.contains m.LoopName live)
                |> List.fold
                    (fun acc m -> Set.union acc (LoopLowering.referencedNames m.Body))
                    (LoopLowering.referencedNames body)

            let next = allNames |> List.filter referenced.Contains |> Set.ofList
            if next = live then live else fix next

        fix (Set.ofList allNames)

    let hasCrossMemberJump =
        members
        |> List.mapi (fun i m -> targetsOf m |> Set.exists (fun j -> j <> i))
        |> List.exists id

    if members.Length > 1 && hasCrossMemberJump then
        generateMergedLoop ctx members called
    else
        for i, member_ in List.indexed members do
            // Nothing enters this member: emitting it would be dead code, and a
            // C# local function that is never used is a warning.
            if Set.contains member_.LoopName called then
                generateSingleLoop ctx members member_ (jumpedTo.Contains i)

and private generateSingleLoop
    (ctx: CodegenContext)
    (members: TLoopMember list)
    (member_: TLoopMember)
    (loops: bool)
    : unit =

    // Nothing jumps here, so the slot/local split has no purpose: the parameters
    // can simply carry the source's names.
    let paramNames = if loops then member_.Slots |> List.map fst else member_.Locals

    // A local loop introduces no type parameters of its own; it inherits the
    // enclosing method's. That also makes polymorphic recursion unrepresentable
    // rather than something to detect and reject.
    indent ctx
    append ctx (typeToString member_.RetType)
    append ctx " "
    append ctx (sanitizeIdent member_.LoopName)
    append ctx "("
    for i, ((_, slotType), paramName) in List.indexed (List.zip member_.Slots paramNames) do
        if i > 0 then append ctx ", "
        append ctx (typeToString slotType)
        append ctx " "
        append ctx (sanitizeIdent paramName)
    appendLine ctx ") {"

    withIndent ctx (fun c ->
        let inner =
            { c with
                Loop = Some { Members = members; Merged = false; StateVar = ""; NestedSwitches = 0 } }

        if loops then
            indent inner; appendLine inner "while (true) {"
            withIndent inner (fun c2 ->
                emitIterationCopies c2 member_
                generateBlock c2 Return member_.Body)
            indent inner; appendLine inner "}"
        else
            generateBlock inner Return member_.Body)

    indent ctx; appendLine ctx "}"

/// Emits a mutually recursive group as one local function whose parameters are
/// the union of the members' slots plus a state discriminant.
///
/// Switch sections are the right jump target: C# forbids jumping *into* a
/// lexical block, so plain labels would force every member's body into one flat
/// scope and require alpha-renaming all their locals to avoid collisions.
and private generateMergedLoop (ctx: CodegenContext) (members: TLoopMember list) (called: Set<string>) : unit =
    let first = List.head members
    let retStr = typeToString first.RetType

    // Only return types can disagree. Members cannot differ in type parameters:
    // a local binding is never generalized, so a loop introduces none of its own
    // (`TestFiles/probe/generic_local_rec.bjo`), and every member of a group
    // inherits the same enclosing method's set.
    for m in members do
        if typeToString m.RetType <> retStr then
            codegenError
                m.Body.Range.Start.Line
                $"'%s{first.LoopName}' and '%s{m.LoopName}' tail-call each other but return %s{retStr} and %s{typeToString m.RetType}; a merged loop has one return type, so split the group so that they do not tail-call each other"

    let groupName = freshName "__group"
    let stateVar = freshName "__state"
    let allSlots = members |> List.collect (fun m -> m.Slots)

    indent ctx
    append ctx retStr
    append ctx $" %s{groupName}(int %s{stateVar}"
    for (slotName, slotType) in allSlots do
        append ctx ", "
        append ctx (typeToString slotType)
        append ctx " "
        append ctx (sanitizeIdent slotName)
    appendLine ctx ") {"

    withIndent ctx (fun c ->
        indent c; appendLine c $"while (true) switch (%s{stateVar}) {{"
        withIndent c (fun cs ->
            for i, member_ in List.indexed members do
                indent cs; appendLine cs $"case %d{i}: {{"
                withIndent cs (fun cb ->
                    let inner =
                        { cb with
                            Loop = Some { Members = members; Merged = true; StateVar = stateVar; NestedSwitches = 0 } }

                    emitIterationCopies inner member_
                    generateBlock inner Return member_.Body)
                indent cs; appendLine cs "}"

            indent cs
            appendLine cs "default: throw new Exception(\"Unreachable loop state\");")
        indent c; appendLine c "}")

    indent ctx; appendLine ctx "}"

    // Entry wrappers keep each member callable — and passable as a value — from
    // outside the group. A member its siblings only ever *jump* to is reached
    // through the discriminant instead, and needs none.
    for i, member_ in List.indexed members do
        if Set.contains member_.LoopName called then
            let owned = member_.Slots |> List.map fst |> Set.ofList

            indent ctx
            append ctx retStr
            append ctx $" %s{sanitizeIdent member_.LoopName}("
            for j, (slotName, slotType) in List.indexed member_.Slots do
                if j > 0 then append ctx ", "
                append ctx (typeToString slotType)
                append ctx " "
                append ctx (sanitizeIdent slotName)
            append ctx $") => %s{groupName}(%d{i}"
            for (slotName, _) in allSlots do
                append ctx ", "
                append ctx (if owned.Contains slotName then sanitizeIdent slotName else "default!")
            appendLine ctx ");"

/// Emits a non-recursive local function.
and private generateLocalFunction
    (ctx: CodegenContext)
    (name: string)
    (lambdaArgs: string list)
    (argTypes: HMType list)
    (retType: HMType)
    (lambdaBody: TypedExpr)
    (funType: HMType)
    : unit =

    // `collectTypeVars` knows nothing about what is already in scope, so a local
    // function over `Vec<'a>` inside a method generic in `'a` would emit
    // `void f<T_a>(...)`, shadowing rather than unifying with the enclosing one.
    let typeParams =
        collectTypeVars funType
        |> List.distinct
        |> List.filter (fun v -> not (Set.contains (typeParamKey v) ctx.TypeParams))

    let tyParamsStr =
        if typeParams.IsEmpty then ""
        else "<" + (typeParams |> List.map typeParamName |> String.concat ", ") + ">"

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
    // A local function is a new function scope: it cannot jump into the
    // enclosing loop.
    withIndent { ctx with Loop = None } (fun c -> generateBlock c Return lambdaBody)
    indent ctx
    appendLine ctx "}"

// ---------------------------------------------------------------------------
// Declarations
// ---------------------------------------------------------------------------

/// Emits a parameter list shared by module functions and trait-`impl` methods.
let private generateParameterList
    (ctx: CodegenContext)
    (ownerName: string)
    (args: (string * HMType) list)
    (kwArgs: (string * HMType * TypedExpr) list)
    (restArg: (string * HMType) option)
    : unit =

    let mutable paramIdx = 0

    for (argName, argType) in args do
        if paramIdx > 0 then append ctx ", "
        append ctx (typeToString argType)
        append ctx " "
        append ctx (sanitizeIdent argName)
        paramIdx <- paramIdx + 1

    for (kwName, kwType, kwDefault) in kwArgs do
        if paramIdx > 0 then append ctx ", "
        append ctx $"BjolangRuntime.Option<{typeToString kwType}> "
        append ctx $"__kw_{sanitizeIdent kwName}"
        append ctx " = default"
        paramIdx <- paramIdx + 1

    match restArg with
    | Some (restName, restElemType) ->
        if paramIdx > 0 then append ctx ", "
        append ctx $"params %s{typeToString restElemType}[] %s{sanitizeIdent restName}"
    | None -> ()

/// Emits a whole method: signature, the keyword-parameter unwrap prologue, and
/// the body. Shared by module-level functions and trait-`impl` methods, which
/// differ only in `modifier` and `genericParams`.
///
/// `ctx` must already carry the type parameters that are in scope: a module
/// function's are its own, an `impl` method's belong to the enclosing class.
let private generateMethod
    (ctx: CodegenContext)
    (modifier: string)
    (genericParams: string)
    (name: string)
    (args: (string * HMType) list)
    (kwArgs: (string * HMType * TypedExpr) list)
    (restArg: (string * HMType) option)
    (retType: HMType)
    (body: TypedExpr)
    : unit =

    indent ctx
    append ctx modifier
    append ctx (typeToString retType)
    append ctx " "
    append ctx (sanitizeIdent name)
    append ctx genericParams
    append ctx "("
    generateParameterList ctx name args kwArgs restArg
    append ctx ") {\n"
    withIndent ctx (fun c ->
        // A keyword parameter arrives as an `Option`, so that an omitted one is
        // distinguishable from one passed explicitly at its default value.
        for (kwName, kwType, kwDefault) in kwArgs do
            let c_type = typeToString kwType
            let s_name = sanitizeIdent kwName
            indent c
            appendLine c $"{c_type} {s_name};"
            indent c
            appendLine c $"if (__kw_{s_name}.IsSome) {{"
            withIndent c (fun c2 ->
                indent c2; appendLine c2 $"{s_name} = __kw_{s_name}.Value;"
            )
            indent c
            appendLine c "} else {"
            withIndent c (fun c2 ->
                generateBlock c2 (Assign(s_name)) kwDefault
            )
            indent c
            appendLine c "}"
        generateFunctionBody c body)
    indent ctx
    appendLine ctx "}"

let rec generateDecl (ctx: CodegenContext) (decl: TDecl) : unit =
    match decl with
    | TDefun (name, tyArgs, args, kwArgs, restArg, retType, body, _) ->
        let ctx = { ctx with TypeParams = tyArgs |> List.map typeParamKey |> Set.ofList }

        let genericParams =
            if tyArgs.IsEmpty then ""
            else
                let tyArgsStr = tyArgs |> List.map typeParamName |> String.concat ", "
                $"<%s{tyArgsStr}>"

        generateMethod ctx "public static " genericParams name args kwArgs restArg retType body

    | TType (defs, _) 
    | TTypeRec (defs, _) ->
        for td in defs do
            let tyArgsStr = 
                if td.TypeArgs.IsEmpty then "" 
                else "<" + (td.TypeArgs |> List.map typeParamName |> String.concat ", ") + ">"
            match td.Kind with
            | Record fields ->
                indent ctx
                append ctx $"public record %s{sanitizeIdent td.Name}%s{tyArgsStr}("
                for i, f in List.indexed fields do
                    if i > 0 then append ctx ", "
                    append ctx (typeToString (Inference.resolveTypeAnnotation Prelude.emptyRegistry f.Type))
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
                                append ctx (typeToString (Inference.resolveTypeAnnotation Prelude.emptyRegistry ft))
                                append ctx $" Item%d{i+1}"
                            appendLine ctx $") : %s{sanitizeIdent td.Name}%s{tyArgsStr};"
                )
                indent ctx
                appendLine ctx "}"
            | Alias _ -> ()

    | TTrait (name, targetVar, assocTypes, signatures, _) ->
        indent ctx
        let tyParams = targetVar :: assocTypes |> List.map typeParamName |> String.concat ", "
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
        let targetTypeName =
            match targetType with
            | TCon(n, _) -> n.Replace(".", "_")
            | _ -> "Unknown"
        let sanitizedTraitName = sanitizeIdent traitName
        let className = $"%s{sanitizedTraitName}_%s{targetTypeName}"

        let typeParamVars =
            match targetType with
            | TCon(_, args) ->
                args |> List.choose (function TVar v -> Some v | _ -> None) |> List.distinct
            | _ -> []
        let tyParamsStr =
            if typeParamVars.IsEmpty then ""
            else "<" + (typeParamVars |> List.map typeParamName |> String.concat ", ") + ">"

        // The class's own type parameters are in scope in every method body.
        let ctx = { ctx with TypeParams = typeParamVars |> List.map typeParamKey |> Set.ofList }

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
                // The method's own `tyArgs` are dropped: a trait signature may
                // only mention the trait's target and associated variables — one
                // naming a variable of its own is rejected during inference
                // (`TestFiles/probe/generic_trait_method.bjo`) — so anything here
                // is a class type parameter, already emitted above and in scope.
                | TDefun (n, _, args, kwArgs, restArg, retType, body, _) ->
                    generateMethod ctx "public " "" n args kwArgs restArg retType body
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

        // A static field initializer cannot contain statements, so module values
        // become `static readonly` fields assigned by a static constructor. That
        // is the last place an IIFE would otherwise still be required.
        let valueDefs =
            innerDecls |> List.choose (function TDef (n, v, t, r) -> Some(n, v, t, r) | _ -> None)

        let className = moduleClassName name

        indent ctx
        appendLine ctx $"public static class %s{className} {{"
        withIndent ctx (fun ctx ->
            // Emit factory methods for union cases
            for d in decls |> List.filter isOuterDecl do
                match d with
                | TType (defs, _) | TTypeRec (defs, _) ->
                    for td in defs do
                        let tyArgsStr = 
                            if td.TypeArgs.IsEmpty then "" 
                            else "<" + (td.TypeArgs |> List.map typeParamName |> String.concat ", ") + ">"
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
                                        append ctx (typeToString (Inference.resolveTypeAnnotation Prelude.emptyRegistry ft))
                                        append ctx $" arg{i}"
                                    let argsListStr = String.concat ", " [for i in 0 .. ftypes.Length - 1 -> $"arg{i}"]
                                    appendLine ctx $") => new %s{sanitizeIdent td.Name}%s{tyArgsStr}.%s{sanitizeIdent n}(%s{argsListStr});"
                        | _ -> ()
                | _ -> ()

            for (defName, _, defType, _) in valueDefs do
                indent ctx
                appendLine ctx $"public static readonly %s{typeToString defType} %s{sanitizeIdent defName};"

            for d in innerDecls do
                match d with
                | TDef _ -> ()
                | _ -> generateDecl ctx d

            if not valueDefs.IsEmpty then
                indent ctx
                appendLine ctx $"static %s{className}() {{"
                withIndent ctx (fun c ->
                    for (defName, defValue, _, _) in valueDefs do
                        generateBlock c (Assign(sanitizeIdent defName)) defValue)
                indent ctx
                appendLine ctx "}"
        )
        indent ctx
        appendLine ctx "}"

    | _ -> ()


/// `metadataDeps` is recorded in the assembly for downstream compilations to
/// link against; it is empty for an executable, which nothing links to.
/// `linkedDlls` is every assembly this compilation references, and each one
/// contributes a `using static` so that names re-exported through one DLL can
/// still be found in the class that actually defines them.
let generateProgram (exportMetadata: string) (metadataDeps: string list) (linkedDlls: string list) (decls: TDecl list) : string =
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

    let ctx =
        { Builder = StringBuilder()
          IndentLevel = 0
          UnionCases = unionCases
          GlobalBindings = globalBindings
          Prelude = None
          Loop = None
          TypeParams = Set.empty }

    appendLine ctx "using System;"
    appendLine ctx "using static BjolangRuntime;"
    
    // Emit 'using static' for all modules to allow unqualified access. A module
    // reached both directly and through another module's import would otherwise
    // be named twice, which C# warns about.
    let moduleUsings =
        [ for decl in decls do
            match decl with
            | TModule (name, innerDecls, _) ->
                yield moduleClassName name
                for inner in innerDecls do
                    match inner with
                    | TImport (specs, _) ->
                        for spec in specs do
                            match spec with
                            | RelativePath p ->
                                yield moduleClassName (System.IO.Path.GetFileNameWithoutExtension p)
                            | ModulePath parts ->
                                yield moduleClassName (List.last parts)
                    | _ -> ()
            | _ -> ()

          // Every linked assembly, including ones reached only transitively.
          // A name re-exported through one DLL is compiled as an unqualified
          // reference, so the class that actually defines it has to be in
          // scope even though its module was never imported.
          for dllPath in linkedDlls do
              yield moduleClassName (System.IO.Path.GetFileNameWithoutExtension dllPath) ]
        |> List.distinct

    for className in moduleUsings do
        appendLine ctx $"using static %s{className};"
        
    if not (String.IsNullOrWhiteSpace(exportMetadata)) then
        let escapedMeta = exportMetadata.Replace("\"", "\\\"").Replace("\n", "\\n").Replace("\r", "")
        appendLine ctx $"[assembly: System.Reflection.AssemblyMetadata(\"BjolangExports\", \"%s{escapedMeta}\")]"
    
    if not metadataDeps.IsEmpty then
        let depsStr = metadataDeps |> List.map System.IO.Path.GetFullPath |> String.concat ";"
        appendLine ctx $"[assembly: System.Reflection.AssemblyMetadata(\"BjolangDeps\", \"%s{depsStr}\")]"
        
    appendLine ctx ""
    // Only generate code for the main module (the last one)
    if not decls.IsEmpty then
        let mainModule = List.last decls
        generateDecl ctx mainModule
    
    ctx.Builder.ToString()
