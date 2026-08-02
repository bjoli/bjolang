module Bjolang.Parser

open Lexer

// --- S-Expression Types ---
type SExpr =
    | SAtom of LexedToken
    | SList of SExpr list * Range

let getRange =
    function
    | SAtom t -> t.Range
    | SList(_, r) -> r

// --- AST Types ---
// Every node carries a Range to enable #line emission.

type FType =
    | TName of string * Range
    | TApp of string * FType list * Range
    // (-> MandatoryTypes... (#:key KeyType)... #:rest RestElemType ReturnType)
    | TArrow of FType list * (string * FType) list * FType option * FType * Range

type UnionCase =
    | SimpleCase of string * Range
    | DataCase of string * FType list * Range

type RecordField =
    { Name: string
      Type: FType
      Range: Range }

type TypeDefKind =
    | Alias of FType
    | Union of UnionCase list
    | Record of RecordField list

type TypeDef =
    { Name: string
      TypeArgs: string list
      Kind: TypeDefKind
      Range: Range }


type Pattern =
    | PWildcard of Range
    | PIdent of string * Range
    | PInt of string * Range
    | PString of string * Range
    | PList of Pattern list * Pattern option * Range // (items, optional tail, range)
    | PVec of Pattern list * Pattern option * Range // (items, optional tail, range)
    | PConstruct of string * Pattern list * Range

and Expr =
    | EInt of string * Range
    | EString of string * Range
    | EQuotedSymbol of string * Range
    | EKeyword of string * Range
    | EIdent of string * Range
    | ETuple of Expr list * Range
    | EApp of Expr * Expr list * Range
    | ECast of FType * Expr * Range
    // ELet (name, isFun, args, typeAnn, value, restOfScope, range)
    | ELet of string * bool * string list * FType option * Expr * Expr * Range
    // ELetRec (bindings, restOfScope, range)
    // binding tuple: (name, isFun, args, typeAnn, value)
    | ELetRec of (string * bool * string list * FType option * Expr) list * Expr * Range
    | ELetTuple of string list * Expr * Expr * Range
    | ELetMutable of string * FType option * Expr * Expr * Range
    | ESet of string * Expr * Range
    | EIf of Expr * Expr * Expr * Range
    /// `(when cond body...)`, and with the flag set, `(unless cond body...)`:
    /// a conditional with only one arm, evaluated for effect.
    | EWhen of Expr * Expr * bool * Range
    | EFun of string list * Expr * Range
    | ERecord of (string * Expr) list * Range
    | ERecordUpdate of string * (string * Expr) list * Range
    | EGetField of Expr * string * Range
    | EList of Expr list * Range
    | EVec of Expr list * Range
    | EMatch of Expr * (Pattern * Expr option * Expr) list * Range
    | ETryFinally of Expr * Expr * Range
    /// `(seq body...)`: a lazy sequence. The body is not a value — it is run,
    /// one `yield` at a time, by whoever enumerates the sequence.
    | ESeq of Expr * Range
    /// `(yield v)`: hand `v` to the enclosing `seq`'s consumer.
    | EYield of Expr * Range
    /// `(yield-from s)`: hand over every element of `s` in turn.
    | EYieldFrom of Expr * Range

and DefunArg =
    | MandatoryArg of string
    | KeywordArg of string * Expr              // (#:keyword defaultValue)
    | RestArg of string                        // #:rest name

type ImportSpec =
    | RelativePath of string
    | ModulePath of string list

type Decl =
    | DSignature of string * FType * (string * string) list * Range
    | DImport of ImportSpec list * Range
    | DExport of string list * Range
    // Re-exports bindings this module imported from elsewhere. Unlike `export`,
    // the names are not required to have a signature in this module — they
    // already have one where they were defined.
    | DReExport of string list * Range
    | DModule of string * Decl list * Range
    | DDef of string * Expr * Range
    | DDefTuple of string list * Expr * Range
    | DDefMutable of string * Expr * Range
    | DDefun of string * DefunArg list * Expr * Range
    | DType of TypeDef list * Range
    | DTypeRec of TypeDef list * Range
    // DTrait (Name, ImplementorVar, AssociatedTypes, Signatures, Range)
    | DTrait of string * string * string list * (string * FType) list * Range
    | DExtern of string * FType * (string * string) list * Range
    
    // DImpl (TraitName, TargetType, AssociatedTypeBindings, Methods, Range)
    | DImpl of string * FType * (string * FType) list * Decl list * Range

    // A declaration-only implementation: it records that the target type
    // implements the trait, and what its associated types are, without carrying
    // any method bodies. This is what a compiled module's metadata exports —
    // the methods themselves already live in that assembly.
    // DImplExtern (TraitName, TargetType, AssociatedTypeBindings, Range)
    | DImplExtern of string * FType * (string * FType) list * Range

// --- Parser ---

let rec parsePattern (s: SExpr) : Pattern =
    let r = getRange s

    match s with
    | SAtom { Token = Symbol "_" } -> PWildcard r
    | SAtom { Token = Symbol sym } -> 
        if System.Char.IsUpper(sym.[0]) then PConstruct(sym, [], r)
        else PIdent(sym, r)
    | SAtom { Token = NumberLit n } -> PInt(n, r)
    | SAtom { Token = StringLit str } -> PString(str, r)

    // Special handling for List/Vec patterns and the spread operator
    | SList(SAtom { Token = Symbol "List" } :: args, _) ->
        let elements, tail = parseSpreadArgs r args
        PList(elements, tail, r)

    // `(Vec a b c ...)` and the bracket literal form `[a b c ...]`, which the
    // reader rewrites to `(vec-literal a b c ...)`.
    | SList(SAtom { Token = Symbol("Vec" | "vec-literal") } :: args, _) ->
        let elements, tail = parseSpreadArgs r args
        PVec(elements, tail, r)

    | SList(SAtom { Token = Symbol name } :: args, _) -> PConstruct(name, List.map parsePattern args, r)

    | SList([], _) -> PList([], None, r) // Empty list pattern

    | _ -> failwithf $"Invalid pattern at line %d{r.Start.Line}"

/// Splits the arguments of a sequence pattern into its fixed leading elements
/// plus an optional trailing rest pattern introduced by `...`.
/// For example `a b c ...` yields ([a; b], Some c), binding `c` to the rest.
and parseSpreadArgs (r: Range) (args: SExpr list) : Pattern list * Pattern option =
    let rec go acc items =
        match items with
        | [] -> (List.rev acc, None)
        // Matches `c ...` at the end of the sequence
        | [ tailItem; SAtom { Token = Spread } ] -> (List.rev acc, Some(parsePattern tailItem))
        // Fails if spread is used incorrectly (e.g., in the middle of the sequence)
        | SAtom { Token = Spread } :: _ -> failwithf $"Invalid use of spread operator at line %d{r.Start.Line}"
        | head :: tail -> go (parsePattern head :: acc) tail

    go [] args

let parseArrowType (items: SExpr list) (r: Range) : FType =
    if items.IsEmpty then failwithf $"Arrow type must have at least a return type at line %d{r.Start.Line}"
    let returnTypeExpr = List.last items
    let argItems = List.take (items.Length - 1) items

    let rec parseArrowTypeInner (s: SExpr) : FType =
        let r = getRange s
        match s with
        | SAtom { Token = QuotedSymbol sym } -> TName("'" + sym, r)
        | SAtom { Token = Symbol sym }
        | SAtom { Token = TypeVar sym } -> TName(sym, r)
        | SList(SAtom { Token = Symbol name } :: typeArgs, _) -> TApp(name, List.map parseArrowTypeInner typeArgs, r)
        | _ -> failwithf $"Invalid type syntax in arrow type at line %d{r.Start.Line}"

    let rec collectArgs mandatory keywords argItems =
        match argItems with
        | [] -> TArrow(List.rev mandatory, List.rev keywords, None, parseArrowTypeInner returnTypeExpr, r)
        | [SAtom { Token = Keyword "rest" }] ->
            failwithf $"Expected rest element type after #:rest at line %d{r.Start.Line}"
        | SAtom { Token = Keyword "rest" } :: restTypeExpr :: [] ->
            TArrow(List.rev mandatory, List.rev keywords, Some (parseArrowTypeInner restTypeExpr), parseArrowTypeInner returnTypeExpr, r)
        | SList(SAtom { Token = Keyword name } :: [ typeExpr ], _) :: rest ->
            collectArgs mandatory ((name, parseArrowTypeInner typeExpr) :: keywords) rest
        | item :: rest when keywords.IsEmpty ->
            collectArgs (parseArrowTypeInner item :: mandatory) keywords rest
        | _ -> failwithf $"Mandatory types must come before keyword/rest types in arrow type at line %d{r.Start.Line}"

    collectArgs [] [] argItems

let rec parseType (s: SExpr) : FType =
    let r = getRange s

    match s with
    | SAtom { Token = QuotedSymbol sym } -> TName("'" + sym, r)  // %a in source → 'a internally
    | SAtom { Token = Symbol sym }
    | SAtom { Token = TypeVar sym } -> TName(sym, r)
    | SList(SAtom { Token = Symbol "->" } :: arrowArgs, _) -> parseArrowType arrowArgs r
    | SList(SAtom { Token = Symbol name } :: typeArgs, _) -> TApp(name, List.map parseType typeArgs, r)
    | _ -> failwithf $"Invalid type syntax at line %d{r.Start.Line}"

let parseUnionCase (s: SExpr) : UnionCase =
    let r = getRange s

    match s with
    | SAtom { Token = Symbol name } -> SimpleCase(name, r)
    | SList(SAtom { Token = Colon } :: SAtom { Token = Symbol name } :: tTypes, _) -> DataCase(name, List.map parseType tTypes, r)
    | _ ->
        printfn $"%A{s}"
        failwithf $"Invalid union case at line %d{r.Start.Line}"

let parseRecordField (s: SExpr) : RecordField =
    let r = getRange s

    match s with
    | SList([ SAtom { Token = Colon }; SAtom { Token = Symbol name }; tType ], _) ->
        { Name = name
          Type = parseType tType
          Range = r }
    | _ -> failwithf $"Invalid record field at line %d{r.Start.Line}"

let parseTypeDefHead (head: SExpr) : string * string list =
    match head with
    | SAtom { Token = Symbol name } -> name, []
    | SList(SAtom { Token = Symbol name } :: args, _) ->
        let parseTypeArg = function
            | SAtom { Token = QuotedSymbol ta } -> ta
            | SAtom { Token = Symbol s } -> s // Just in case they are not quoted
            | _ -> failwithf $"Invalid type argument at line %d{(getRange head).Start.Line}"
        name, List.map parseTypeArg args
    | _ -> failwithf $"Invalid type definition head at line %d{(getRange head).Start.Line}"

let parseTypeDef (s: SExpr) : TypeDef =
    let r = getRange s

    match s with
    // `Struct` is an accepted synonym for `Record`.
    | SList([ SAtom { Token = Colon }
              head
              SList(SAtom { Token = Symbol("Record" | "Struct") } :: fields, _) ],
            _) ->
        let name, typeArgs = parseTypeDefHead head
        { Name = name
          TypeArgs = typeArgs
          Kind = Record(List.map parseRecordField fields)
          Range = r }
    | SList([ SAtom { Token = Colon }; head; aliasType ], _) ->
        let name, typeArgs = parseTypeDefHead head
        { Name = name
          TypeArgs = typeArgs
          Kind = Alias(parseType aliasType)
          Range = r }
    | SList(head :: cases, _) ->
        let name, typeArgs = parseTypeDefHead head
        { Name = name
          TypeArgs = typeArgs
          Kind = Union(List.map parseUnionCase cases)
          Range = r }
    | _ -> failwithf $"Invalid type definition at line %d{r.Start.Line}"

let parseDefunArg (arg: SExpr) : (string * FType option) =
    match arg with
    | SAtom { Token = Symbol n } -> (n, None)
    | SList([ SAtom { Token = Colon }; SAtom { Token = Symbol n }; t ], _) -> (n, Some(parseType t))
    | _ -> failwith "Invalid defun argument"

let parseDefunArgs (args: SExpr list) : (string * FType option) list = args |> List.map parseDefunArg

let parseDefunRest (rest: SExpr list) : (FType option * SExpr list) =
    match rest with
    | SAtom { Token = Colon } :: t :: body -> (Some(parseType t), body)
    | body -> (None, body)


// Desugar a quoted list '(1 2 3) into (Cons 1 (Cons 2 (Cons 3 Nil)))
// Nested lists are recursively quoted: '(1 (2 3)) → (Cons 1 (Cons (Cons 2 (Cons 3 Nil)) Nil))
// Dotted pairs (a . b) within quoted lists become tuples
let desugarQuotedList (items: SExpr list) (r: Range) : Expr =
    let rec quoteItem (s: SExpr) : Expr =
        let ir = getRange s
        match s with
        | SAtom { Token = NumberLit n } -> EInt(n, ir)
        | SAtom { Token = StringLit str } -> EString(str, ir)
        | SAtom { Token = Symbol sym } -> EIdent(sym, ir)
        | SAtom { Token = QuotedSymbol sym } -> EQuotedSymbol(sym, ir)
        // Nested list → recursive Cons chain
        | SList(SAtom { Token = Symbol "Tuple" } :: tupleItems, _) ->
            // Dotted pair in a quoted list: '(a . b) → (Tuple a b)
            ETuple(List.map quoteItem tupleItems, ir)
        | SList(inner, _) ->
            buildConsChain inner ir
        | _ -> failwithf $"Unsupported item in quoted list at line %d{ir.Start.Line}"
    and buildConsChain (items: SExpr list) (r: Range) : Expr =
        match items with
        | [] -> EIdent("Nil", r)
        | item :: rest ->
            let hd = quoteItem item
            let tl = buildConsChain rest r
            EApp(EIdent("Cons", r), [hd; tl], r)
    buildConsChain items r

let rec parseExpr (s: SExpr) : Expr =
    let r = getRange s

    let rec processArgs items =
        match items with
        | [] -> []
        | SAtom { Token = Comma } :: rest -> processArgs rest
        | item :: rest -> parseExpr item :: processArgs rest

    // Treat specific operator tokens as valid identifiers in expressions
    let (|Ident|_|) =
        function
        | SAtom { Token = Symbol "#t" } -> Some "true"
        | SAtom { Token = Symbol "#f" } -> Some "false"
        | SAtom { Token = Symbol sym } -> Some sym
        | _ -> None

    match s with
    | SAtom { Token = NumberLit n } -> EInt(n, r)
    | SAtom { Token = StringLit str } -> EString(str, r)
    | SAtom { Token = QuotedSymbol sym } -> EQuotedSymbol(sym, r)
    | SAtom { Token = Keyword sym } -> EKeyword(sym, r)
    | Ident sym -> EIdent(sym, r)

    | SList(head :: args, listRange) ->
        match head with
        | Ident sym ->
            match sym with
            | "cast" ->
                match args with
                | [ typeSExpr; valSExpr ] ->
                    ECast(parseType typeSExpr, parseExpr valSExpr, r)
                | _ -> failwithf $"Invalid cast syntax at line %d{r.Start.Line}. Expected: (cast <type> <expr>)"
            | "let" ->
                match args with
                | SList(bindings, _) :: bodyExprs ->
                    let body = parseBody bodyExprs listRange

                    List.foldBack
                        (fun bind acc ->
                            match bind with
                            | SList([ Ident k; v ], _) -> ELet(k, false, [], None, parseExpr v, acc, getRange bind)
                            | _ -> failwith "Invalid let binding")
                        bindings
                        body
                | Ident name :: SList(bindings, _) :: bodyExprs ->
                    // Named let
                    let parsedBindings =
                        bindings
                        |> List.map (function
                            | SList([ Ident k; v ], _) -> (k, parseExpr v)
                            | _ -> failwith "Invalid named let binding")
                    
                    let argNames = parsedBindings |> List.map fst
                    let argVals = parsedBindings |> List.map snd
                    let body = parseBody bodyExprs listRange
                    let funcBinding = (name, true, argNames, None, body)
                    ELetRec([funcBinding], EApp(EIdent(name, r), argVals, r), r)
                | _ -> failwith "Invalid let syntax"

            | "letrec" ->
                match args with
                | SList(bindings, _) :: bodyExprs ->
                    let parsedBindings =
                        bindings
                        |> List.map (function
                            // Standard explicit letrec assumes value bindings or manually desugared lambdas
                            | SList([ Ident k; v ], _) -> (k, false, [], None, parseExpr v)
                            | _ -> failwith "Invalid letrec binding")

                    ELetRec(parsedBindings, parseBody bodyExprs listRange, r)
                | _ -> failwith "Invalid letrec syntax"
            | "set!" ->
                match args with
                | [ Ident target; valExpr ] -> ESet(target, parseExpr valExpr, r)
                | _ -> failwithf $"Invalid set! syntax at line %d{r.Start.Line}. Expected: (set! name value)"
            | "if" ->
                match args with
                | [ cond; t; f ] -> EIf(parseExpr cond, parseExpr t, parseExpr f, r)
                | _ -> failwith "Invalid if syntax"

            // `when` and `unless` are one-armed: there is no second branch for
            // the body's type to agree with, so they are statements rather than
            // expressions. Desugaring them into `if` with an empty tuple as the
            // missing arm made every body that was not itself an empty tuple a
            // type error — which is to say every body anyone would write.
            | "when" ->
                match args with
                | cond :: bodyExprs when not bodyExprs.IsEmpty ->
                    EWhen(parseExpr cond, parseBody bodyExprs listRange, false, listRange)
                | _ -> failwithf $"Invalid when syntax at line %d{r.Start.Line}. Expected: (when cond body...)"

            | "unless" ->
                match args with
                | cond :: bodyExprs when not bodyExprs.IsEmpty ->
                    EWhen(parseExpr cond, parseBody bodyExprs listRange, true, listRange)
                | _ -> failwithf $"Invalid unless syntax at line %d{r.Start.Line}. Expected: (unless cond body...)"

            // A `seq` body is a block like any other, but it is *not* run where
            // it is written: the form evaluates to a sequence, and the body runs
            // a `yield` at a time as that sequence is consumed.
            | "seq" ->
                match args with
                | [] -> failwithf $"Invalid seq syntax at line %d{r.Start.Line}. Expected: (seq body...)"
                | bodyExprs -> ESeq(parseBody bodyExprs listRange, listRange)

            | "yield" ->
                match args with
                | [ value ] -> EYield(parseExpr value, listRange)
                | _ -> failwithf $"Invalid yield syntax at line %d{r.Start.Line}. Expected: (yield value)"

            | "yield-from" ->
                match args with
                | [ source ] -> EYieldFrom(parseExpr source, listRange)
                | _ -> failwithf $"Invalid yield-from syntax at line %d{r.Start.Line}. Expected: (yield-from seq)"

            | "and" ->
                let rec buildAnd items =
                    match items with
                    | [] -> EIdent("true", listRange)
                    | [last] -> parseExpr last
                    | current :: rest -> 
                        EIf(parseExpr current, buildAnd rest, EIdent("false", listRange), listRange)
                buildAnd args

            | "or" ->
                let rec buildOr items =
                    match items with
                    | [] -> EIdent("false", listRange)
                    | [last] -> parseExpr last
                    | current :: rest ->
                        EIf(parseExpr current, EIdent("true", listRange), buildOr rest, listRange)
                buildOr args

            | "not" ->
                match args with
                | [arg] -> EIf(parseExpr arg, EIdent("false", listRange), EIdent("true", listRange), listRange)
                | _ -> failwithf $"Invalid not syntax at line %d{r.Start.Line}"

            | "fun" ->
                match args with
                | SList(fargs, _) :: bodyExprs ->
                    let argNames =
                        fargs
                        |> List.choose (function
                            | Ident n -> Some n
                            | SAtom { Token = Comma } -> None
                            | _ -> failwith "Expected arg name")

                    EFun(argNames, parseBody bodyExprs listRange, r)
                | _ -> failwith "Invalid fun syntax"

            | "match" ->
                match args with
                | targetExpr :: clauses ->
                    let target = parseExpr targetExpr

                    let parsedClauses =
                        clauses
                        |> List.map (fun clause ->
                            let rClause = getRange clause

                            match clause with
                            // Clause with a guard: (pattern #:when guard body...)
                            | SList(pattern :: SAtom { Token = Keyword "when" } :: guard :: bodyExprs, _) ->
                                (parsePattern pattern, Some(parseExpr guard), parseBody bodyExprs rClause)
                            // Standard clause: (pattern body...)
                            | SList(pattern :: bodyExprs, _) ->
                                (parsePattern pattern, None, parseBody bodyExprs rClause)
                            | _ -> failwithf $"Invalid match clause at line %d{rClause.Start.Line}")

                    EMatch(target, parsedClauses, r)
                | _ -> failwithf $"Invalid match syntax at line %d{r.Start.Line}"

            // `struct*` forms are accepted synonyms for the `record*` forms.
            | "record" | "struct" ->
                let fields =
                    args
                    |> List.map (function
                        | SList([ Ident k; v ], _) -> (k, parseExpr v)
                        | bad ->
                            failwithf
                                $"Invalid %s{sym} field at line %d{(getRange bad).Start.Line}: expected (field-name value)")

                ERecord(fields, r)

            | "record-set" | "struct-set" ->
                match args with
                | Ident baseRec :: fields ->
                    let parsedFields =
                        fields
                        |> List.map (function
                            | SList([ Ident k; v ], _) -> (k, parseExpr v)
                            | bad ->
                                failwithf
                                    $"Invalid %s{sym} field at line %d{(getRange bad).Start.Line}: expected (field-name value)")

                    ERecordUpdate(baseRec, parsedFields, r)
                | _ -> failwithf $"Invalid %s{sym} syntax at line %d{r.Start.Line}: expected (%s{sym} target (field value) ...)"
            
            | "record-get" | "struct-get" ->
                match args with
                | [ target; Ident field ] ->
                    EGetField(parseExpr target, field, r)
                | _ -> failwithf $"Invalid %s{sym} syntax at line %d{r.Start.Line}: expected (%s{sym} target field-name)"

            | "Tuple" -> ETuple(processArgs args, listRange)

            // Quoted list literal: '(1 2 3) → Cons chain
            | "quoted-list" -> desugarQuotedList args listRange

            // Vec literal: [1 2 3] → EVec
            | "vec-literal" -> EVec(processArgs args, listRange)

            // Standard function application
            | _ -> EApp(EIdent(sym, getRange head), processArgs args, listRange)


        | _ ->
            // Fallback for tuples or unquoted lists
            EApp(parseExpr head, processArgs args, listRange)

    | SList([], listRange) -> ETuple([], listRange)

    // Explicit token catches for better debugging
    | SAtom { Token = Comma } -> failwithf $"Unexpected comma at line %d{r.Start.Line}"
    | SAtom { Token = Quote } -> failwithf $"Unexpected quote at line %d{r.Start.Line}"
    | _ -> failwithf $"Unexpected expression at line %d{r.Start.Line}"

and parseBody (exprs: SExpr list) (fallbackRange: Range) : Expr =
    let rec collectDefs acc remaining =
        match remaining with
        // 1. Standard value definition (def name expr)
        | SList(SAtom { Token = Symbol "def" } :: SAtom { Token = Symbol name } :: [ expr ], _) :: rest ->
            // isFun = false, args = []
            collectDefs ((name, false, [], None, parseExpr expr) :: acc) rest

        // 1b. Annotated value definition (def (: name type) expr)
        | SList(SAtom { Token = Symbol "def" } :: SList([ SAtom { Token = Colon }; SAtom { Token = Symbol name }; tType ], _) :: [ expr ], _) :: rest ->
            collectDefs ((name, false, [], Some(parseType tType), parseExpr expr) :: acc) rest

        // 2. Local function definition (defun (name args...) body)
        | SList(SAtom { Token = Symbol "defun" } :: SList(SAtom { Token = Symbol name } :: args, _) :: rest, r) :: rest' ->
            let argNames = parseDefunArgs args |> List.map fst
            let _, bodyExprs = parseDefunRest rest
            let fBody = parseBody bodyExprs r
            // isFun = true, args = argNames
            collectDefs ((name, true, argNames, None, fBody) :: acc) rest'

        | _ -> (List.rev acc, remaining)

    and parseItems remaining =
        match remaining with
        | [] -> ETuple([], fallbackRange)

        // 1. Intercept local mutable definitions FIRST
        | SList(SAtom { Token = Symbol "def/mutable" } :: SAtom { Token = Symbol name } :: [ expr ], r) :: rest ->
            ELetMutable(name, None, parseExpr expr, parseItems rest, fallbackRange)

        // Annotated form: (def/mutable (: name type) expr) — the type checker will unify it with the initializer.
        | SList(SAtom { Token = Symbol "def/mutable" } :: SList([ SAtom { Token = Colon }; SAtom { Token = Symbol name }; tType ], _) :: [ expr ], r) :: rest ->
            ELetMutable(name, Some(parseType tType), parseExpr expr, parseItems rest, fallbackRange)

        // 2. Starts with def or defun: collect consecutive defs into a letrec block
        | (SList(SAtom { Token = Symbol "def" } :: _, _)) :: _
        | (SList(SAtom { Token = Symbol "defun" } :: _, _)) :: _ ->
            let defs, rest = collectDefs [] remaining
            ELetRec(defs, parseItems rest, fallbackRange)

        // 3. Single expression left — no sequencing wrapper needed
        | [ expr ] -> parseExpr expr

        // 4. Multiple expressions — sequence them with ELet (isFun = false, empty args)
        | expr :: rest -> ELet("_", false, [], None, parseExpr expr, parseItems rest, fallbackRange)

    parseItems exprs

// New defun arg parser for top-level defuns with keyword/rest support
let rec parseNewDefunArgs (args: SExpr list) : DefunArg list =
    match args with
    | [] -> []
    | SAtom { Token = Symbol n } :: rest -> MandatoryArg n :: parseNewDefunArgs rest
    | SAtom { Token = Comma } :: rest -> parseNewDefunArgs rest
    | SList(SAtom { Token = Keyword name } :: [ defaultExpr ], _) :: rest ->
        KeywordArg(name, parseExpr defaultExpr) :: parseNewDefunArgs rest
    | SAtom { Token = Keyword "rest" } :: SAtom { Token = Symbol name } :: rest ->
        if not rest.IsEmpty then
            failwithf $"Rest argument must be the last argument at line %d{(getRange (List.head rest)).Start.Line}"
        [RestArg name]
    | SAtom { Token = Keyword name } :: defaultExpr :: rest ->
        KeywordArg(name, parseExpr defaultExpr) :: parseNewDefunArgs rest
    | bad :: _ -> failwithf $"Invalid defun argument at line %d{(getRange bad).Start.Line}"

let rec parseDecl (s: SExpr) : Decl =
    let r = getRange s

    match s with
    | SList([ SAtom { Token = Colon }; SAtom { Token = Symbol name }; tType ], _) ->
        DSignature(name, parseType tType, [], r)
    // (: name type (where (TraitName %var) ...)) — signature with trait constraints
    | SList(SAtom { Token = Colon } :: SAtom { Token = Symbol name } :: tType :: SList(SAtom { Token = Symbol "where" } :: constraintExprs, _) :: _, _) ->
        let constraints =
            constraintExprs |> List.choose (function
                | SList([ SAtom { Token = Symbol traitName }; SAtom { Token = QuotedSymbol varName } ], _) ->
                    Some (traitName, "'" + varName)
                | SList([ SAtom { Token = Symbol traitName }; SAtom { Token = Symbol varName } ], _) ->
                    Some (traitName, varName)
                | _ -> None)
        DSignature(name, parseType tType, constraints, r)

    | SList(SAtom { Token = Symbol "import" } :: imports, _) ->
        // Parse paths like (io readline) into ["io"; "readline"]
        let parseImportPath =
            function
            | SAtom { Token = StringLit s } -> RelativePath s
            | SList(pathNodes, _) ->
                pathNodes
                |> List.map (function
                    | SAtom { Token = Symbol p } -> p
                    | _ -> failwithf $"Invalid import path element at line %d{r.Start.Line}")
                |> ModulePath
            | _ -> failwithf $"Invalid import syntax at line %d{r.Start.Line}"

        DImport(List.map parseImportPath imports, r)

    | SList(SAtom { Token = Symbol "export" } :: exports, _) ->
        // Parse items like poop-on-you
        let exportNames =
            exports
            |> List.map (function
                | SAtom { Token = Symbol e } -> e
                | _ -> failwithf $"Invalid export item at line %d{r.Start.Line}")

        DExport(exportNames, r)

    | SList(SAtom { Token = Symbol "re-export" } :: reExports, _) ->
        let reExportNames =
            reExports
            |> List.map (function
                | SAtom { Token = Symbol e } -> e
                | _ -> failwithf $"Invalid re-export item at line %d{r.Start.Line}")

        DReExport(reExportNames, r)

    | SList(SAtom { Token = Symbol "module" } :: SAtom { Token = Symbol name } :: body, _) ->
        DModule(name, List.map parseDecl body, r)

    | SList(SAtom { Token = Symbol "def" } :: SAtom { Token = Symbol name } :: [ expr ], _) ->
        DDef(name, parseExpr expr, r)

    | SList(SAtom { Token = Symbol "def" } :: SList([ SAtom { Token = Colon }; SAtom { Token = Symbol name }; tType ], _) :: [ expr ], _) ->
        DDef(name, parseExpr expr, r)

    | SList(SAtom { Token = Symbol "def" } :: SList(names, _) :: [ expr ], _) ->
        let tupleNames =
            names
            |> List.map (function
                | SAtom { Token = Symbol n } -> n
                | SAtom { Token = Comma } -> ""
                | _ -> failwith "Invalid tuple def")
            |> List.filter ((<>) "")

        DDefTuple(tupleNames, parseExpr expr, r)

    | SList(SAtom { Token = Symbol "def/mutable" } :: SAtom { Token = Symbol name } :: [ expr ], _) ->
        DDefMutable(name, parseExpr expr, r)

    | SList(SAtom { Token = Symbol "def/mutable" } :: SList([ SAtom { Token = Colon }; SAtom { Token = Symbol name }; tType ], _) :: [ expr ], _) ->
        DDefMutable(name, parseExpr expr, r)

    | SList(SAtom { Token = Symbol "defun" } :: SList(SAtom { Token = Symbol name } :: args, _) :: rest, _) ->
        let parsedArgs = parseNewDefunArgs args
        // Skip optional inline return type annotation (backward compat, ignored — type comes from signature)
        let bodyExprs =
            match rest with
            | SAtom { Token = Colon } :: _ :: body -> body
            | body -> body
        DDefun(name, parsedArgs, parseBody bodyExprs r, r)
    | SList(SAtom { Token = Symbol "type" } :: typeDefs, _) -> DType(List.map parseTypeDef typeDefs, r)

    | SList(SAtom { Token = Symbol "type-rec" } :: typeDefs, _) -> DTypeRec(List.map parseTypeDef typeDefs, r)

    | SList (SAtom { Token = Symbol "def/trait" } :: 
             SList (SAtom { Token = Symbol traitName } :: [ SAtom { Token = QuotedSymbol implementorVar } ], _) :: 
             body, r) ->
        
        let mutable assocTypes = []
        let mutable signatures = []

        for item in body do
            match item with
            // Match: (type 'item)
            | SList (SAtom { Token = Symbol "type" } :: SAtom { Token = QuotedSymbol assocName } :: [], _) ->
                assocTypes <- assocName :: assocTypes
            
            // Match: (: methodName signatureExpr)
            | SList (SAtom { Token = Colon } :: SAtom { Token = Symbol methodName } :: typeExpr :: [], _) ->
                signatures <- (methodName, parseType typeExpr) :: signatures
            
            | _ -> failwithf $"Syntax error in def/trait '%s{traitName}': Expected (type ...) or (: ...)."

        DTrait (traitName, implementorVar, List.rev assocTypes, List.rev signatures, r)

    // Parse: (def/impl (TraitName (Vec 'a)) (type 'item 'a) (defun (get v i) ...))
    | SList (SAtom { Token = Symbol "def/impl" } :: 
             SList (SAtom { Token = Symbol traitName } :: targetTypeExpr :: [], _) :: 
             body, r) ->
        
        let targetType = parseType targetTypeExpr
        
        let mutable assocBindings = []
        let mutable methods = []

        for item in body do
            match item with
            // Match: (type 'item targetType)
            | SList (SAtom { Token = Symbol "type" } :: SAtom { Token = QuotedSymbol assocName } :: boundTypeExpr :: [], _) ->
                assocBindings <- (assocName, parseType boundTypeExpr) :: assocBindings
            
            // Match: (defun ...)
            | SList (SAtom { Token = Symbol "defun" } :: _, _) as defunExpr ->
                methods <- parseDecl defunExpr :: methods
            
            | _ -> failwithf $"Syntax error in def/impl for '%s{traitName}': Expected (type ...) or (defun ...)."

        DImpl (traitName, targetType, List.rev assocBindings, List.rev methods, r)

    // Parse: (def/impl/extern (Foldable (Vec 'a)) (type 'item 'a))
    //
    // The bodyless counterpart of `def/impl`, emitted into a library's export
    // metadata so that whoever imports it can resolve the trait's associated
    // types and dispatch to the impl class compiled into that assembly.
    | SList (SAtom { Token = Symbol "def/impl/extern" } ::
             SList (SAtom { Token = Symbol traitName } :: targetTypeExpr :: [], _) ::
             body, r) ->

        let assocBindings =
            body
            |> List.map (function
                | SList (SAtom { Token = Symbol "type" } :: SAtom { Token = QuotedSymbol assocName } :: boundTypeExpr :: [], _) ->
                    assocName, parseType boundTypeExpr
                | _ -> failwithf $"Syntax error in def/impl/extern for '%s{traitName}': Expected (type ...).")

        DImplExtern (traitName, parseType targetTypeExpr, assocBindings, r)

    | _ -> failwithf $"Unknown declaration at line %d{r.Start.Line}"

let parseModule (exprs: SExpr list) : Decl list = List.map parseDecl exprs
