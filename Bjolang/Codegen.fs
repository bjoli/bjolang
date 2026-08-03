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
    /// When true, this is a flattened inlined loop. Non-recur terminals leave
    /// the `while` loop instead of returning.
    IsInlineLoop: bool
    /// A label emitted just after an inlined loop, for exits that `break`
    /// cannot express because a `switch` stands in the way.
    ExitLabel: string
    /// Set when something actually jumped to `ExitLabel`. A label C# can see no
    /// jump to is a warning, so it is only emitted once it has been used —
    /// which is known only after the body has been generated.
    ExitLabelUsed: bool ref
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
    /// True inside the iterator method a `seq` was emitted as. `yield` is a
    /// property of the *method* it appears in, not of the lexical form, so any
    /// construct that opens a new C# method — a lambda, a local function, a
    /// non-inlined loop member — clears this.
    InSeq: bool
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

let private freshName (prefix: string) = Gensym.fresh prefix

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
    // A `seq` is a C# iterator, so its type is the one C# iterators produce.
    | "Seq" -> "System.Collections.Generic.IEnumerable"
    | "Option" -> "BjolangRuntime.Option"
    | "Map" -> "Map.Map"
    | _ -> name

// Promoted to `Bjolang.Naming`, which the passes that run before code
// generation also need: an inlined body has to be able to name the module a
// free variable came from.
let sanitizeIdent = Naming.sanitizeIdent

/// Does this `::` name qualify a binding to the module class that defines it,
/// rather than name a method of a trait implementation?
///
/// The two shapes are spelled the same because they mean the same thing to
/// `sanitizeIdent` — reach into that class — but they disagree about what the
/// identifier's type arguments are for. A trait landing pad's belong to the
/// *class*, `Foldable_List<int>.Instance.fold`; a qualified binding's belong to
/// the *function*, and C# infers those from the arguments as it would for any
/// other call.
let private isModuleQualified (name: string) =
    match name.LastIndexOf "::" with
    | -1 -> false
    | i -> name.Substring(0, i).EndsWith "_Module"

/// The C# class a module's declarations are emitted into.
///
/// A module is named after its source file, so the name can hold characters no
/// C# identifier may hold — or start with a digit, as `06_lib.bjo` does. Every
/// site that spells this class has to agree on the answer: the class definition,
/// the `using static` for it, a qualified reference to one of its bindings, and
/// the generated entry point.
let moduleClassName = Naming.moduleClassName

/// The C# spelling of a Bjolang type parameter.
let typeParamName = Naming.typeParamName

/// The canonical key a type parameter is tracked under, independent of whether
/// the source wrote it quoted.
let typeParamKey = Naming.typeParamKey



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
    // Projected out of a type variable, an associated type is spelled as the
    // synthesized type parameter that stands for it.
    | TAssoc (_, assocName, TVar implVar) -> typeParamName (assocTypeVar implVar assocName)
    | TAssoc (traitName, assocName, TMeta { Value = Some inner }) ->
        typeToString (TAssoc(traitName, assocName, inner))
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
    // The projection is itself a type parameter, not a mention of the
    // implementor: `Foldable %c`'s element type is `T_c_item`, and a local
    // function that uses it must not redeclare it.
    | TAssoc (_, assocName, TVar implVar) -> [ assocTypeVar implVar assocName ]
    | TAssoc (_, _, implType) -> collectTypeVars implType

/// What is to become of the value a block produces.
///
/// Every case but `Effect` describes a *terminal* position: once the value is
/// discharged, the block is over, and inside an inlined loop that means leaving
/// the loop. `Effect` is the other thing a block can be — one statement among
/// several, after which control simply continues — and the two must not be
/// confused. Spelling both as `Discard` made every intermediate `(println x)`
/// in a named `let` compile to `println(x); break;`.
type BlockTarget =
    | Return
    | Assign of string
    | DeclareAndAssign of string * string
    /// Terminal, but the value is thrown away.
    | Discard
    /// Not terminal: run it for its effect, then fall through to the statements
    /// that follow.
    | Effect

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
    // An unresolved associated type has to survive into the metadata as an
    // associated type. Flattening it to `object` used to make an imported
    // signature unusable: `fold`'s element type is `%item`, and `object` will
    // not unify with the `int` the caller actually has.
    | TAssoc (traitName, assocName, implType) ->
        $"(assoc %s{traitName} %s{assocName} %s{serializeHMType implType})"


/// A trait signature that mentions the implementor applied.
///
/// The hole is written as the implementor variable in applied position —
/// `('m 'a)` — which is the one thing `parseType` accepts only for a quoted
/// head, and therefore the one thing that reads back as a hole rather than as a
/// constructor named `m`.
let rec serializeTplType (implementorVar: string) (t: TplType) : string =
    let go = serializeTplType implementorVar

    match t with
    | TplCon(name, args) ->
        let baseName =
            match name with
            | _ when name = TypeConstants.Int32Name -> "int"
            | _ when name = TypeConstants.StringName -> "string"
            | _ when name = TypeConstants.BooleanName -> "bool"
            | _ when name = TypeConstants.VoidName -> "void"
            | _ -> name

        if args.IsEmpty then baseName
        else $"(%s{baseName} " + String.concat " " (List.map go args) + ")"
    | TplVar name -> name
    | TplFun(args, ret) ->
        if args.IsEmpty then $"(-> %s{go ret})"
        else "(-> " + String.concat " " (List.map go args) + $" %s{go ret})"
    | TplTuple types -> "(Tuple " + String.concat " " (List.map go types) + ")"
    | TplHole args ->
        "('" + implementorVar.TrimStart('\'') + " " + String.concat " " (List.map go args) + ")"

let rec serializeFType (ft: Parser.FType) : string =
    match ft with
    | Parser.TName(n, _) -> n
    | Parser.TApp(n, args, _) -> $"({n} " + String.concat " " (List.map serializeFType args) + ")"
    | Parser.TArrow(mandatory, keywords, restOpt, ret, _) ->
        let mandatoryStrs = mandatory |> List.map serializeFType
        let keywordStrs = keywords |> List.map (fun (n, t) -> $"(#:{n} {serializeFType t})")
        let restStrs = match restOpt with Some t -> [$"#:rest {serializeFType t}"] | None -> []
        let allParts = mandatoryStrs @ keywordStrs @ restStrs @ [serializeFType ret]
        "(-> " + String.concat " " allParts + ")"

// ---------------------------------------------------------------------------
// Untyped expressions, for inline templates
// ---------------------------------------------------------------------------

/// What has to survive a round trip through the reader.
///
/// The metadata string is escaped again on its way into a C# attribute, so
/// backslashes have to be doubled *here* as well as there; escaping only quotes
/// used to turn `\"` into `\\"`, which closes the C# literal.
let private escapeSexpr (s: string) =
    s.Replace("\\", "\\\\").Replace("\"", "\\\"").Replace("\n", "\\n").Replace("\t", "\\t")

let rec serializePattern (p: Parser.Pattern) : string =
    match p with
    | Parser.PWildcard _ -> "_"
    | Parser.PIdent(n, _) -> n
    | Parser.PInt(v, _) -> v
    | Parser.PString(v, _) -> "\"" + escapeSexpr v + "\""
    // Always parenthesized, even with no arguments. A bare name reads back as a
    // constructor only when it happens to start with a capital, and that is not
    // something to rely on.
    | Parser.PConstruct(n, args, _) ->
        "(" + String.concat " " (n :: List.map serializePattern args) + ")"
    | Parser.PList(items, tailOpt, _) -> serializeSeqPattern "List" items tailOpt
    | Parser.PVec(items, tailOpt, _) -> serializeSeqPattern "Vec" items tailOpt

and private serializeSeqPattern (head: string) items tailOpt =
    let itemStrs = items |> List.map serializePattern
    let tailStrs =
        match tailOpt with
        | Some t -> [ serializePattern t; "..." ]
        | None -> []
    "(" + String.concat " " (head :: (itemStrs @ tailStrs)) + ")"

/// Writes an untyped expression as source the reader accepts again.
///
/// The *untyped* expression is what an inline template stores: `HMType` is full
/// of mutable metavariable cells that mean nothing outside the compilation that
/// made them, and re-inferring the body at the call site is exactly what gives
/// the method a type its trait signature could not express.
let rec serializeExpr (e: Parser.Expr) : string =
    let list (parts: string list) = "(" + String.concat " " parts + ")"

    match e with
    | Parser.EInt(v, _) -> v
    | Parser.EString(v, _) -> "\"" + escapeSexpr v + "\""
    | Parser.EQuotedSymbol(s, _) -> "'" + s
    | Parser.EKeyword(k, _) -> "#:" + k
    | Parser.EIdent(n, _) -> n
    | Parser.ETuple(items, _) -> list ("Tuple" :: List.map serializeExpr items)
    | Parser.EApp(target, args, _) -> list (serializeExpr target :: List.map serializeExpr args)
    | Parser.ECast(t, v, _) -> list [ "cast"; serializeFType t; serializeExpr v ]

    | Parser.ELet(n, isFun, args, ann, value, body, _) ->
        let valueStr =
            if isFun then list [ "fun"; list args; serializeExpr value ]
            else serializeExpr value

        let annotated =
            match ann with
            | Some t -> list [ "cast"; serializeFType t; valueStr ]
            | None -> valueStr

        list [ "let"; list [ list [ n; annotated ] ]; serializeExpr body ]

    | Parser.ELetRec(bindings, body, _) ->
        // A body block: consecutive `def`/`defun` forms are collected back into
        // one mutually-recursive group by the reader.
        let defs =
            bindings
            |> List.map (fun (n, isFun, args, _, value) ->
                if isFun then list [ "defun"; list (n :: args); serializeExpr value ]
                else list [ "def"; n; serializeExpr value ])

        list ([ "let"; "()" ] @ defs @ [ serializeExpr body ])

    | Parser.ELetMutable(n, _, value, body, _) ->
        list [ "let"; "()"; list [ "def/mutable"; n; serializeExpr value ]; serializeExpr body ]

    | Parser.ESet(n, v, _) -> list [ "set!"; n; serializeExpr v ]
    | Parser.EIf(c, t, f, _) -> list [ "if"; serializeExpr c; serializeExpr t; serializeExpr f ]
    | Parser.EWhen(c, b, negated, _) ->
        list [ (if negated then "unless" else "when"); serializeExpr c; serializeExpr b ]
    | Parser.EFun(args, body, _) -> list [ "fun"; list args; serializeExpr body ]
    | Parser.ERecord(fields, _) ->
        list ("record" :: (fields |> List.map (fun (k, v) -> list [ k; serializeExpr v ])))
    | Parser.ERecordUpdate(n, fields, _) ->
        list ("record-set" :: n :: (fields |> List.map (fun (k, v) -> list [ k; serializeExpr v ])))
    | Parser.EGetField(target, f, _) -> list [ "record-get"; serializeExpr target; f ]
    | Parser.EVec(items, _) -> "[" + String.concat " " (List.map serializeExpr items) + "]"

    | Parser.EMatch(target, clauses, _) ->
        let clauseStrs =
            clauses
            |> List.map (fun (pat, guard, body) ->
                match guard with
                | Some g -> list [ serializePattern pat; "#:when"; serializeExpr g; serializeExpr body ]
                | None -> list [ serializePattern pat; serializeExpr body ])

        list ("match" :: serializeExpr target :: clauseStrs)

    | Parser.ESeq(body, _) -> list [ "seq"; serializeExpr body ]
    | Parser.EYield(v, _) -> list [ "yield"; serializeExpr v ]
    | Parser.EYieldFrom(s, _) -> list [ "yield-from"; serializeExpr s ]

    // No reader form produces these, so none can appear in a template body.
    | Parser.ELetTuple _ -> failwith "an inline template body may not destructure a tuple binding"
    | Parser.EList _ -> failwith "an inline template body may not contain a bare list literal"
    | Parser.ETryFinally _ -> failwith "an inline template body may not contain try/finally"

/// Can this body be written out and read back at all? A template that cannot be
/// serialized is simply not exported; its landing pad still is.
let isSerializableTemplate (e: Parser.Expr) : bool =
    try
        serializeExpr e |> ignore
        true
    with _ ->
        false

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
    | TRecur _
    // A C# iterator is a *method*: the body has to be emitted as one, and this
    // node's value is a call to it.
    | TSeq _
    | TYield _
    | TYieldFrom _ -> true

    // A conditional stays `c ? t : f` as long as it yields a value and neither
    // arm needs statements. Hoisting out of an arm would evaluate it
    // unconditionally, so an arm that needs a statement forces the whole node
    // into an `if`. The condition is evaluated unconditionally, so whatever it
    // hoists can safely go ahead of the conditional.
    | TIf (_, t, f) -> isVoidType expr.Type || containsHoist t || containsHoist f

    // One-armed and void: there is no C# expression with that shape.
    | TWhen _ -> true

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
       // Both open a block of their own, so nothing inside can need a statement
       // position out here.
       | TLambda _
       | TSeq _ -> false
       | _ -> TypeVisitor.children expr |> List.exists containsHoist

/// Translates a typed pattern into C# pattern syntax.
let rec generatePattern (ctx: CodegenContext) (pat: TypedPattern) : unit =
    match pat.Node with
    | TPWildcard -> append ctx "_"
    | TPIdent name -> append ctx $"var {sanitizeIdent name}"
    | TPInt value -> append ctx value
    | TPString value -> append ctx $"\"%s{escapeStringLiteral value}\""
    // `Option` is the runtime's `Option<T>` struct — a flag and a value rather
    // than a pair of subclasses — so its constructors match as property
    // patterns. The type is left off: the scrutinee already has it, and a type
    // pattern naming a struct's own type reads as a tautology.
    | TPConstruct ("None", _) -> append ctx "{ Tag: 0 }"
    | TPConstruct ("Some", [ inner ]) ->
        append ctx "{ Tag: 1, Value: "
        generatePattern ctx inner
        append ctx " }"

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
    // A dictionary singleton: "Foldable_Vec::Instance" with the impl class's own
    // type arguments. `Lowering` produces these when it passes a dictionary to a
    // constrained function, and the class is generic whenever the implemented
    // type is (`Foldable_Vec<T_a>`), so the arguments cannot be dropped.
    | TIdent (name, tArgs) when name.Contains("::") && not tArgs.IsEmpty && not (isModuleQualified name) ->
        let parts = name.Split("::")
        let tyArgsStr = tArgs |> List.map typeToString |> String.concat ", "
        append ctx (sanitizeIdent parts[0])
        append ctx $"<%s{tyArgsStr}>"
        for part in parts[1..] do
            append ctx "."
            append ctx (sanitizeIdent part)
    | TIdent (name, _) ->
        // Cons/Nil are now builtins backed by SchemeList, not union cases.
        match name with
        | "Nil" ->
            let elemTypeStr = elementTypeString expr.Type
            append ctx $"Nil<%s{elemTypeStr}>()"
        // Like `Nil`, a nullary constructor rather than a bare name: written
        // plain it would be a method group.
        | "None" ->
            let elemTypeStr = elementTypeString expr.Type
            append ctx $"None<%s{elemTypeStr}>()"
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
        // loop's slots, a `continue` inside it would bind to nothing, and it
        // cannot be an iterator, so it cannot yield either.
        let inner = { ctx with Prelude = None; Loop = None; InSeq = false }
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

    // Every trait call has been turned into either a spliced body or a direct
    // call by the time code is generated: `TraitInline` takes the resolved ones
    // and `Lowering` takes the rest. One reaching here means neither did.
    | TTraitCall (tref, _, _) ->
        codegenError
            expr.Range.Start.Line
            $"internal error: call to '{tref.Trait}.{tref.Method}' was never resolved to an implementation"

    | TThrow _
    | TVecMake _
    | TLet _
    | TLetRec _
    | TLetTuple _
    | TLetMutable _
    | TSet _
    | TWhen _
    | TTryFinally _
    | TLoop _
    | TRecur _
    | TSeq _
    | TYield _
    | TYieldFrom _ ->
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
        // Whatever follows in the enclosing expression still has to run, so this
        // is an intermediate statement rather than a block's last word.
        generateBlock inner Effect expr
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
            if name.Contains("::") && not tArgs.IsEmpty && not (isModuleQualified name) then
                // Trait instance method: "TraitName_Type.Instance::methodName"
                // Split at "::" to insert type args on the class portion.
                let parts = name.Split("::")
                let classPart = parts.[0]  // e.g. "Foldable_List.Instance"
                let methodPart = parts.[1] // e.g. "fold"
                let tyArgsStr = tArgs |> List.map typeToString |> String.concat ", "
                // Insert <T> before ".Instance"
                let classPortions = classPart.Split('.')
                if classPortions.Length >= 2 then
                    // className.Instance -> className<T>.Instance
                    append ctx (sanitizeIdent classPortions.[0])
                    append ctx $"<%s{tyArgsStr}>"
                    for i in 1 .. classPortions.Length - 1 do
                        append ctx "."
                        append ctx (sanitizeIdent classPortions.[i])
                else
                    append ctx (sanitizeIdent classPart)
                    append ctx $"<%s{tyArgsStr}>"
                append ctx "."
                append ctx (sanitizeIdent methodPart)
            else
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
                    // The parameter is declared as `__kw_<name>`, so that is
                    // what a named argument has to say. Writing the bare name
                    // produced C# that named a parameter which does not exist —
                    // latent only because nothing in the suite had ever passed a
                    // keyword argument rather than relying on its default.
                    append ctx $"__kw_%s{sanitizeIdent kwName}: "
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
            // Check if this is a single-entry flat loop (like a named-let):
            // A single member whose loop name is immediately called with initial arguments in `body`,
            // and the loop name is not referenced as a first-class value or called non-tail-recursively.
            let flatLoopInfo =
                match members, body.Node with
                | [ member_ ], TApply({ Node = TIdent(calleeName, _) }, initArgs, _)
                    when calleeName = member_.LoopName && initArgs.Length = member_.Slots.Length ->
                    let freeRefs = LoopLowering.referencedNames member_.Body
                    if not (freeRefs.Contains member_.LoopName) then
                        Some (member_, initArgs)
                    else
                        None
                | _ -> None

            match flatLoopInfo with
            | Some (member_, initArgs) ->
                // Flat loop path: emit slot variables, evaluate initial args, and run while(true) inline!
                for (slotName, slotType) in member_.Slots do
                    indent ctx; appendLine ctx $"{typeToString slotType} {sanitizeIdent slotName};"

                let temps = initArgs |> List.map (fun _ -> freshName "__init")
                for arg, tmp in List.zip initArgs temps do
                    emitStatement ctx (fun c ->
                        indent c
                        append c $"var {tmp} = "
                        generateExpr c arg
                        appendLine c ";")

                let slots = member_.Slots |> List.map (fst >> sanitizeIdent)
                for slot, tmp in List.zip slots temps do
                    indent ctx; appendLine ctx $"{slot} = {tmp};"

                let loopTarget =
                    match target with
                    | DeclareAndAssign (varType, varName) ->
                        indent ctx; appendLine ctx $"{varType} {varName};"
                        Assign varName
                    // The loop's own value is dropped, but its body still has to
                    // leave the `while (true)` when it stops jumping — which is
                    // exactly what `Discard` means inside an inlined loop.
                    | Effect -> Discard
                    | _ -> target

                let exitLabel = freshName "__exit"
                let exitLabelUsed = ref false

                let inner =
                    { ctx with
                        Loop =
                            Some
                                { Members = members
                                  Merged = false
                                  StateVar = ""
                                  NestedSwitches = 0
                                  IsInlineLoop = true
                                  ExitLabel = exitLabel
                                  ExitLabelUsed = exitLabelUsed } }

                // Whether the label is needed is only known once the body has
                // been generated, and it has to appear *after* the loop — so
                // the loop is built aside and appended once the answer is in.
                let scratch = StringBuilder()
                let buffered = { inner with Builder = scratch }

                indent buffered; appendLine buffered "while (true) {"
                withIndent buffered (fun c2 ->
                    emitIterationCopies c2 member_
                    generateBlock c2 loopTarget member_.Body)
                indent buffered; appendLine buffered "}"

                ctx.Builder.Append(scratch) |> ignore

                if exitLabelUsed.Value then
                    // A label needs a statement; an empty one will do.
                    indent ctx; appendLine ctx $"%s{exitLabel}: ;"

            | None ->
                // General letrec / mutually-recursive / escaping loop: emit as local functions
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
                // `(begin a b)` is `TLet ("_", …, a, b)`: `a` runs, then `b`. The
                // block is not over, so this is an `Effect`, not a `Discard`.
                generateBlock ctx Effect value
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
        dischargeVoid ctx target

    | TSeq body ->
        // A C# iterator has to be a method, and a lambda cannot be one, so the
        // body becomes a local function and this node's value is a call to it.
        // Nothing is enumerated until that sequence is consumed.
        let iterator = freshName "__seq"

        indent ctx
        appendLine ctx $"%s{typeToString expr.Type} %s{iterator}() {{"
        withIndent { ctx with Prelude = None; Loop = None; InSeq = true } (fun c ->
            generateBlock c Effect body
            // Also what makes this an iterator at all when the body happens to
            // contain no `yield` — without one C# would read it as an ordinary
            // method that never returns a value.
            indent c
            appendLine c "yield break;")
        indent ctx
        appendLine ctx "}"

        emitTerminal ctx target expr.Type (fun c -> append c $"%s{iterator}()")

    | TYield value ->
        requireSeqScope ctx expr "yield"

        emitStatement ctx (fun c ->
            indent c
            append c "yield return "
            generateExpr c value
            appendLine c ";")

        dischargeVoid ctx target

    | TYieldFrom source ->
        requireSeqScope ctx expr "yield-from"

        // `foreach` rather than a bare re-yield: the elements have to be pulled
        // out one at a time and handed on individually, so that the consumer
        // sees one flat sequence and each source is disposed when it is done.
        let element = freshName "__yielded"

        emitStatement ctx (fun c ->
            indent c
            append c $"foreach (var %s{element} in "
            generateExpr c source
            appendLine c ") {")

        withIndent ctx (fun c ->
            indent c
            appendLine c $"yield return %s{element};")

        indent ctx
        appendLine ctx "}"

        dischargeVoid ctx target

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
        // Cleanup runs for its effect and control leaves the `finally` on its
        // own; it must not try to break or return out of one.
        withIndent ctx (fun c -> generateBlock c Effect cleanup)
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

    | TWhen (cond, body, negated) ->
        emitStatement ctx (fun c ->
            indent c
            append c (if negated then "if (!(" else "if (")
            generateExpr c cond
            appendLine c (if negated then ")) {" else ") {"))

        // The body runs for its effect: whatever it evaluates to is discarded,
        // and control then continues after the `if`.
        withIndent ctx (fun c -> generateBlock c Effect body)
        indent ctx; appendLine ctx "}"

        // `when` yields void, like `set!`, so the enclosing target still has to
        // be discharged.
        dischargeVoid ctx target

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

/// Rejects a `yield` that did not end up inside the iterator method its `seq`
/// was emitted as.
///
/// Inference scopes `yield` lexically, but C# scopes it per *method*, and the
/// two disagree wherever a form inside a `seq` needs a method of its own: a
/// lambda, or a loop whose name escapes and so cannot be inlined. Emitting a
/// `yield return` there would produce C# that does not compile, with the error
/// pointing at generated code the author never wrote.
and private requireSeqScope (ctx: CodegenContext) (expr: TypedExpr) (formName: string) : unit =
    if not ctx.InSeq then
        codegenError
            expr.Range.Start.Line
            $"'%s{formName}' is inside a function of its own — a lambda, or a loop that is used as a value — rather than directly in the body of its (seq ...); move it into the sequence's own body"

/// Leaves an inlined loop, if that is what reaching this point means.
///
/// `break` binds to the nearest enclosing breakable statement. A `match` is
/// emitted as a `switch`, so from inside one a `break` leaves the switch and
/// drops back into the loop it was supposed to end — which is not a compile
/// error but an infinite loop. A `goto` to a label after the loop means the
/// same thing from any depth, so that is what a nested exit uses.
and private exitInlineLoop (ctx: CodegenContext) : unit =
    match ctx.Loop with
    | Some ({ IsInlineLoop = true } as loop) ->
        indent ctx

        if loop.NestedSwitches = 0 then
            appendLine ctx "break;"
        else
            loop.ExitLabelUsed.Value <- true
            appendLine ctx $"goto %s{loop.ExitLabel};"
    | _ -> ()

/// Discharges `target` after a form that has already emitted all of its own
/// statements and produced no value.
and private dischargeVoid (ctx: CodegenContext) (target: BlockTarget) : unit =
    match target with
    // Not terminal: the statements that follow still have to run.
    | Effect -> ()
    | Return ->
        indent ctx
        appendLine ctx (if ctx.InSeq then "yield break;" else "return;")
    | Assign _
    | DeclareAndAssign _
    | Discard -> exitInlineLoop ctx

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
    | Effect ->
        // Not terminal, so no `break` and no `return`: whatever follows in the
        // enclosing block still has to run.
        if isVoid then
            emit ctx; appendLine ctx ";"
        else
            append ctx "_ = "; emit ctx; appendLine ctx ";"
    | Return ->
        if isVoid then
            emit ctx; appendLine ctx ";"
            indent ctx
            appendLine ctx (if ctx.InSeq then "yield break;" else "return;")
        else
            append ctx "return "; emit ctx; appendLine ctx ";"
    | Assign name ->
        if isVoid then
            emit ctx; appendLine ctx ";"
        else
            append ctx $"%s{name} = "; emit ctx; appendLine ctx ";"
        exitInlineLoop ctx
    | DeclareAndAssign (varType, varName) ->
        if isVoid then
            emit ctx; appendLine ctx ";"
        else
            append ctx $"%s{varType} %s{varName} = "; emit ctx; appendLine ctx ";"
        exitInlineLoop ctx
    | Discard ->
        // C# has no expression statement for an arbitrary value, so a discarded
        // one is assigned to `_`. A void value is already a statement.
        if isVoid then
            emit ctx; appendLine ctx ";"
        else
            append ctx "_ = "; emit ctx; appendLine ctx ";"
        exitInlineLoop ctx

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

    // A jump discards the enclosing block's remaining work. Under any target
    // (Return, Discard, Assign, DeclareAndAssign), the slot variables are updated
    // and the loop continues to the next iteration.

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
                    Loop = Some { Members = [ member_ ]; Merged = false; StateVar = ""; NestedSwitches = 0; IsInlineLoop = false; ExitLabel = ""; ExitLabelUsed = ref false } }

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
        // A local function is a method of its own: it can neither jump into the
        // enclosing loop nor yield into the enclosing sequence.
        let inner =
            { c with
                InSeq = false
                Loop = Some { Members = members; Merged = false; StateVar = ""; NestedSwitches = 0; IsInlineLoop = false; ExitLabel = ""; ExitLabelUsed = ref false } }

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
                            InSeq = false
                            Loop = Some { Members = members; Merged = true; StateVar = stateVar; NestedSwitches = 0; IsInlineLoop = false; ExitLabel = ""; ExitLabelUsed = ref false } }

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
    // enclosing loop, nor yield into the enclosing sequence.
    withIndent { ctx with Loop = None; InSeq = false } (fun c -> generateBlock c Return lambdaBody)
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

    // An inline trait emits nothing at all. There is no valid C# interface for
    // `Monad<M>`: the parameter would have to be a type constructor.
    | TTrait (_, _, InlineTrait, _, _, _, _) -> ()

    | TTrait (name, targetVar, _, _, assocTypes, signatures, _) ->
        // Helper to collect all TVar names from a type
        let rec collectTVars t =
            match t with
            | TVar v -> [v]
            | TCon(_, args) -> List.collect collectTVars args
            | TFun(args, ret) -> (List.collect collectTVars args) @ collectTVars ret
            | TTuple args -> List.collect collectTVars args
            | TAssoc(_, _, impl) -> collectTVars impl
            | _ -> []

        indent ctx
        // Class-level type params: the implementor var + associated types
        let classTyParamsList = targetVar :: assocTypes
        let tyParams = classTyParamsList |> List.map typeParamName |> String.concat ", "
        appendLine ctx $"public interface %s{sanitizeIdent name}<%s{tyParams}> {{"
        withIndent ctx (fun ctx ->
            // The raw trait signature uses unprimed names (e.g. "col"),
            // but the TVars in the resolved HMType are primed (e.g. "'col").
            let classTyVarNames = classTyParamsList |> List.map (fun v -> "'" + v)
            for kvp in signatures do
                let mName = kvp.Key
                let mType = kvp.Value
                // Method-level generics: TVars in this method that aren't class-level
                let methodVars =
                    collectTVars mType
                    |> List.distinct
                    |> List.filter (fun v -> not (List.contains v classTyVarNames))
                let methodTyParamsStr =
                    if methodVars.IsEmpty then ""
                    else "<" + (methodVars |> List.map typeParamName |> String.concat ", ") + ">"

                match mType with
                | TFun (args, ret) ->
                    indent ctx
                    append ctx (typeToString ret)
                    append ctx " "
                    append ctx (sanitizeIdent mName)
                    append ctx methodTyParamsStr
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

    | TImpl (traitName, kind, holeArity, targetType, assocMap, methods, _) ->
        let targetTypeName =
            match targetType with
            | TCon(n, _) -> n.Replace(".", "_")
            | _ -> "Unknown"
        let sanitizedTraitName = sanitizeIdent traitName
        let className = $"%s{sanitizedTraitName}_%s{targetTypeName}"

        // The class's type parameters are the impl's *fixed prefix*. For an
        // interface trait that is the whole target; for an inline trait the
        // trailing `holeArity` arguments belong to the trait's constructor
        // variable, so they are the method's business rather than the class's.
        let targetArgs =
            match targetType with
            | TCon(_, args) -> args
            | _ -> []

        let prefixArgs = targetArgs |> List.truncate (max 0 (targetArgs.Length - holeArity))

        let typeParamVars =
            prefixArgs |> List.collect collectTypeVars |> List.distinct

        let tyParamsStr =
            if typeParamVars.IsEmpty then ""
            else "<" + (typeParamVars |> List.map typeParamName |> String.concat ", ") + ">"

        // The class's own type parameters are in scope in every method body.
        let ctx = { ctx with TypeParams = typeParamVars |> List.map typeParamKey |> Set.ofList }

        let baseClause =
            match kind with
            | InlineTrait -> ""
            | InterfaceTrait ->
                let targetTypeStr = typeToString targetType
                let assocArgsStr =
                    assocMap
                    |> List.map (fun (_, t) -> typeToString t)
                    |> String.concat ", "
                let traitArgsStr =
                    if String.IsNullOrEmpty(assocArgsStr) then targetTypeStr
                    else $"%s{targetTypeStr}, %s{assocArgsStr}"
                $" : %s{sanitizedTraitName}<%s{traitArgsStr}>"

        indent ctx
        appendLine ctx $"public sealed class %s{className}%s{tyParamsStr}%s{baseClause} {{"
        withIndent ctx (fun ctx ->
            // An inline trait has no interface to satisfy, so there is nothing
            // for a singleton to be an instance *of*: its landing pads are plain
            // static methods.
            match kind with
            | InterfaceTrait ->
                indent ctx
                appendLine ctx $"public static readonly %s{className}%s{tyParamsStr} Instance = new();"
            | InlineTrait -> ()

            let modifier =
                match kind with
                | InterfaceTrait -> "public "
                | InlineTrait -> "public static "

            for m in methods do
                match m with
                | TDefun (n, tyArgs, args, kwArgs, restArg, retType, body, _) ->
                    // Whatever is left over after the class's own parameters is
                    // a method-level generic and must be emitted as one. This is
                    // exactly the restriction inline traits lift: `bind`'s `'b`
                    // belongs to the method, not to the trait's target.
                    let classKeys = typeParamVars |> List.map typeParamKey |> Set.ofList
                    let methodOnlyTyArgs =
                        tyArgs |> List.filter (fun v -> not (Set.contains (typeParamKey v) classKeys))
                    let methodTyArgsStr =
                        if methodOnlyTyArgs.IsEmpty then ""
                        else "<" + (methodOnlyTyArgs |> List.map typeParamName |> String.concat ", ") + ">"
                    // Include method-level type params in scope
                    let methodCtx =
                        { ctx with TypeParams = Set.union ctx.TypeParams (methodOnlyTyArgs |> List.map typeParamKey |> Set.ofList) }
                    generateMethod methodCtx modifier methodTyArgsStr n args kwArgs restArg retType body
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
let generateProgram (exportMetadata: string) (inlineMetadata: string) (metadataDeps: string list) (linkedDlls: string list) (decls: TDecl list) : string =
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
          TypeParams = Set.empty
          InSeq = false }

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
        
    // Backslashes are doubled *first*. Escaping only the quotes turned a `\"`
    // already inside the metadata — which an inline template body carries as
    // soon as it mentions a string literal — into `\\"`, closing the C# literal
    // early and producing source that does not parse.
    let escapeAttribute (s: string) =
        s.Replace("\\", "\\\\").Replace("\"", "\\\"").Replace("\n", "\\n").Replace("\r", "")

    if not (String.IsNullOrWhiteSpace(exportMetadata)) then
        let escapedMeta = escapeAttribute exportMetadata
        appendLine ctx $"[assembly: System.Reflection.AssemblyMetadata(\"BjolangExports\", \"%s{escapedMeta}\")]"

    // Kept in an attribute of its own rather than mixed into the exports: these
    // are expressions, not declarations, and whoever reads them wants them
    // whole rather than folded into the declaration parser.
    if not (String.IsNullOrWhiteSpace(inlineMetadata)) then
        let escapedInline = escapeAttribute inlineMetadata
        appendLine ctx $"[assembly: System.Reflection.AssemblyMetadata(\"BjolangInlineImpls\", \"%s{escapedInline}\")]"
    
    if not metadataDeps.IsEmpty then
        let depsStr = metadataDeps |> List.map System.IO.Path.GetFullPath |> String.concat ";"
        appendLine ctx $"[assembly: System.Reflection.AssemblyMetadata(\"BjolangDeps\", \"%s{depsStr}\")]"
        
    appendLine ctx ""
    // Only generate code for the main module (the last one)
    if not decls.IsEmpty then
        let mainModule = List.last decls
        generateDecl ctx mainModule
    
    ctx.Builder.ToString()
