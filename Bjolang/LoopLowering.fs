module Bjolang.LoopLowering

open Bjolang.TypedAST

/// Rewrites tail recursion into explicit `TLoop`/`TRecur` nodes.
///
/// Every syntactic form that can recur — a module-level `defun`, a trait-`impl`
/// method, a named `let`, an inner `defun` — is lowered to the *same* shape, so
/// the code generator has exactly one path from a loop to emitted C# and none
/// from a loop to a real call. Whether a call becomes a jump is decided here,
/// once, rather than falling out of whichever emitter branch happens to be
/// reached.
///
/// Two things this pass is responsible for that the emitter cannot do:
///
/// * **Normalizing the argument vector.** A `TRecur` always carries one argument
///   per loop slot, with keyword arguments resolved to their positional slot and
///   omitted optionals filled in from their defaults. The emitter never sees a
///   partial argument list, so it cannot silently leave a slot holding the
///   previous iteration's value.
/// * **Per-iteration parameter copies.** A loop mutates its slots, but C#
///   closures capture by reference, so a lambda created in one iteration would
///   otherwise observe the next iteration's values. Each member's body is
///   alpha-renamed to read fresh per-iteration locals instead of the slots.

// ---------------------------------------------------------------------------
// Fresh names
// ---------------------------------------------------------------------------

let private counter = ref 0

let private fresh (prefix: string) =
    counter.Value <- counter.Value + 1
    $"%s{prefix}__%d{counter.Value}"

// ---------------------------------------------------------------------------
// Alpha renaming
// ---------------------------------------------------------------------------

let rec private patternNames (pat: TypedPattern) : string list =
    match pat.Node with
    | TPWildcard
    | TPInt _
    | TPString _ -> []
    | TPIdent n -> [ n ]
    | TPList(items, tailOpt)
    | TPVec(items, tailOpt) ->
        (items |> List.collect patternNames)
        @ (tailOpt |> Option.map patternNames |> Option.defaultValue [])
    | TPConstruct(_, args) -> args |> List.collect patternNames
    | TPApp(_, inner) -> patternNames inner
    | TPAs(inner, n) -> n :: patternNames inner

let private without (names: string seq) (subst: Map<string, string>) =
    names |> Seq.fold (fun acc n -> Map.remove n acc) subst

/// Renames *free* occurrences of the keys of `subst`, respecting every binder.
let rec renameExpr (subst: Map<string, string>) (expr: TypedExpr) : TypedExpr =
    if Map.isEmpty subst then
        expr
    else

    let sub = renameExpr subst
    let reference n = Map.tryFind n subst |> Option.defaultValue n

    let node =
        match expr.Node with
        | TIdent(n, tArgs) -> TIdent(reference n, tArgs)
        | TSet(n, v) -> TSet(reference n, sub v)
        | TRecordUpdate(n, fields) -> TRecordUpdate(reference n, fields |> List.map (fun (k, v) -> k, sub v))

        | TLet(n, isFun, args, v, b) ->
            // A function-shaped `let` is never self-recursive — `LetRecify` emits
            // `ELet` only for singleton components with no self-edge — so `n` is
            // bound in the body alone.
            let valueSubst = if isFun then without args subst else subst
            TLet(n, isFun, args, renameExpr valueSubst v, renameExpr (Map.remove n subst) b)

        | TLetRec(bindings, b) ->
            let inner = without (bindings |> List.map (fun (n, _, _, _) -> n)) subst

            TLetRec(
                bindings
                |> List.map (fun (n, isFun, args, v) ->
                    n, isFun, args, renameExpr (if isFun then without args inner else inner) v),
                renameExpr inner b
            )

        | TLetTuple(names, v, b) -> TLetTuple(names, sub v, renameExpr (without names subst) b)
        | TLetMutable(n, v, b) -> TLetMutable(n, sub v, renameExpr (Map.remove n subst) b)
        | TLambda(args, b) -> TLambda(args, renameExpr (without args subst) b)

        | TMatch(target, clauses) ->
            TMatch(
                sub target,
                clauses
                |> List.map (fun c ->
                    let inner = without (patternNames c.Pattern) subst

                    { Pattern = c.Pattern
                      Guard = Option.map (renameExpr inner) c.Guard
                      Body = renameExpr inner c.Body })
            )

        | TLoop(members, bodyOpt) ->
            // Member names are in scope throughout the group; a member's slots and
            // per-iteration locals are in scope in its own body only.
            let outer = without (members |> List.map (fun m -> m.LoopName)) subst

            TLoop(
                members
                |> List.map (fun m ->
                    let inner = without ((m.Slots |> List.map fst) @ m.Locals) outer
                    { m with Body = renameExpr inner m.Body }),
                Option.map (renameExpr outer) bodyOpt
            )

        | _ -> (TypeVisitor.mapChildren sub expr).Node

    { expr with Node = node }

// ---------------------------------------------------------------------------
// Loop targets
// ---------------------------------------------------------------------------

/// A loop that a call in tail position may jump to.
type private LoopTarget =
    { Index: int
      /// Every name the loop answers to. A trait-`impl` method has two: its
      /// source name and the devirtualized name `Lowering.fs` rewrote concrete
      /// self-calls to.
      Names: string list
      Mandatory: (string * HMType) list
      Keywords: (string * HMType * TypedExpr) list
      Rest: (string * HMType) option }

    member this.Name = List.head this.Names

/// The slot vector a `TRecur` targeting `t` must fill, in emission order. This
/// has to agree with the parameter order `Codegen` builds for a `TDefun`:
/// mandatory, then keyword, then rest.
let private slotsOf (t: LoopTarget) : (string * HMType) list =
    t.Mandatory
    @ (t.Keywords |> List.map (fun (n, ty, _) -> n, ty))
    @ (match t.Rest with
       | Some(n, elemType) -> [ n, TCon("Array", [ elemType ]) ]
       | None -> [])

/// Builds the complete positional argument vector for a jump to `t`.
let private normalizeRecur
    (t: LoopTarget)
    (args: TypedExpr list)
    (kwArgs: (string * TypedExpr) list)
    (source: TypedExpr)
    : TExprNode =
    let mandatoryCount = t.Mandatory.Length

    if args.Length < mandatoryCount then
        failwithf
            $"Internal error: tail call to '%s{t.Name}' passes %d{args.Length} positional arguments but %d{mandatoryCount} are mandatory (line %d{source.Range.Start.Line})"

    let mandatoryValues = args |> List.truncate mandatoryCount
    let restValues = args |> List.skip mandatoryCount

    // An omitted optional must be re-supplied from its default: the slot still
    // holds the *previous* iteration's value, which is not what a fresh call
    // would have produced.
    let keywordValues =
        t.Keywords
        |> List.map (fun (kwName, _, defaultValue) ->
            match kwArgs |> List.tryFind (fun (n, _) -> n = kwName) with
            | Some(_, value) -> value
            | None -> defaultValue)

    let restValue =
        match t.Rest with
        | Some(_, elemType) ->
            [ ({ Type = TCon("Array", [ elemType ])
                 Range = source.Range
                 Node = TArrayMake restValues }: TypedExpr) ]
        | None ->
            if not restValues.IsEmpty then
                failwithf
                    $"Internal error: tail call to '%s{t.Name}' passes too many positional arguments (line %d{source.Range.Start.Line})"

            []

    TRecur(t.Index, mandatoryValues @ keywordValues @ restValue)

// ---------------------------------------------------------------------------
// Queries
// ---------------------------------------------------------------------------

/// Whether `expr` contains a jump belonging to the loop scope it was lowered in.
/// Lambda bodies and nested loop members carry jumps of their own scopes.
let rec containsRecur (expr: TypedExpr) : bool =
    match expr.Node with
    | TRecur _ -> true
    | TLambda _ -> false
    | TLoop(_, bodyOpt) -> bodyOpt |> Option.map containsRecur |> Option.defaultValue false
    | _ -> TypeVisitor.children expr |> List.exists containsRecur

/// The set of member indices jumped to from within `expr`, in the loop scope
/// `expr` belongs to.
let rec recurTargetsIn (expr: TypedExpr) : Set<int> =
    match expr.Node with
    | TRecur(index, args) ->
        args |> List.fold (fun acc a -> Set.union acc (recurTargetsIn a)) (Set.singleton index)
    | TLambda _ -> Set.empty
    | TLoop(_, bodyOpt) ->
        bodyOpt |> Option.map recurTargetsIn |> Option.defaultValue Set.empty
    | _ ->
        TypeVisitor.children expr
        |> List.fold (fun acc c -> Set.union acc (recurTargetsIn c)) Set.empty

/// Every name `expr` mentions as a reference. Shadowing is ignored, so this
/// over-approximates: the emitter uses it to decide which loop members are still
/// reachable as *calls*, and over-approximating only keeps a member alive that
/// nothing could have called.
let rec referencedNames (expr: TypedExpr) : Set<string> =
    let here =
        match expr.Node with
        | TIdent(n, _) -> Set.singleton n
        | TSet(n, _) -> Set.singleton n
        | TRecordUpdate(n, _) -> Set.singleton n
        | _ -> Set.empty

    TypeVisitor.children expr
    |> List.fold (fun acc c -> Set.union acc (referencedNames c)) here

// ---------------------------------------------------------------------------
// Expressions
// ---------------------------------------------------------------------------

let rec private lowerExpr (targets: LoopTarget list) (inTail: bool) (expr: TypedExpr) : TypedExpr =
    /// A sub-expression that is *not* in tail position.
    let notTail e = lowerExpr targets false e
    /// A sub-expression that inherits the current tail position.
    let inherits e = lowerExpr targets inTail e
    /// A nested function scope. Its tail positions are its own, and it cannot
    /// jump into the enclosing loop.
    let newScope e = lowerExpr [] true e
    /// Shadowing a loop's name rebinds it: calls in the inner scope are not jumps.
    let shadow names =
        targets |> List.filter (fun t -> not (t.Names |> List.exists (fun n -> List.contains n names)))

    match expr.Node with
    | TApply(target, args, kwArgs) ->
        let loopTarget =
            if inTail then
                match target.Node with
                | TIdent(n, _) -> targets |> List.tryFind (fun t -> List.contains n t.Names)
                | _ -> None
            else
                None

        let loweredArgs = args |> List.map notTail
        let loweredKwArgs = kwArgs |> List.map (fun (n, e) -> n, notTail e)

        match loopTarget with
        | Some t ->
            { expr with
                Node = normalizeRecur t loweredArgs loweredKwArgs expr }
        | None ->
            { expr with
                Node = TApply(notTail target, loweredArgs, loweredKwArgs) }

    | TIf(c, t, f) ->
        { expr with
            Node = TIf(notTail c, inherits t, inherits f) }

    // A `when` in tail position leaves its body in tail position too: the value
    // is discarded either way, and a jump never produces one.
    | TWhen(c, body, negated) ->
        { expr with
            Node = TWhen(notTail c, inherits body, negated) }

    | TLetMutable(n, v, b) ->
        { expr with
            Node = TLetMutable(n, notTail v, lowerExpr (shadow [ n ]) inTail b) }

    | TLetTuple(names, v, b) ->
        { expr with
            Node = TLetTuple(names, notTail v, lowerExpr (shadow names) inTail b) }

    | TMatch(target, clauses) ->
        { expr with
            Node =
                TMatch(
                    notTail target,
                    clauses
                    |> List.map (fun c ->
                        let inner = shadow (patternNames c.Pattern)

                        { c with
                            Guard = Option.map (lowerExpr inner false) c.Guard
                            Body = lowerExpr inner inTail c.Body })
                ) }

    | TLambda(args, b) ->
        { expr with
            Node = TLambda(args, newScope b) }

    | TLet(n, isFun, args, v, b) ->
        let loweredValue = if isFun then newScope v else notTail v

        { expr with
            Node = TLet(n, isFun, args, loweredValue, lowerExpr (shadow [ n ]) inTail b) }

    | TLetRec(bindings, b) -> lowerLetRec targets inTail expr bindings b

    // No child of any remaining node is in tail position.
    | _ -> TypeVisitor.mapChildren notTail expr

/// Turns a `letrec` group into a loop group. `LetRecify` has already reduced the
/// binding group to strongly-connected components, so the members handed to us
/// are exactly one component; no graph work is repeated here.
and private lowerLetRec
    (targets: LoopTarget list)
    (inTail: bool)
    (expr: TypedExpr)
    (bindings: (string * bool * string list * TypedExpr) list)
    (body: TypedExpr)
    : TypedExpr =

    let asFunction (_, _, _, (value: TypedExpr)) =
        match value.Node, value.Type with
        | TLambda(lambdaArgs, lambdaBody), TFun(argTypes, retType) when argTypes.Length = lambdaArgs.Length ->
            Some(lambdaArgs, argTypes, retType, lambdaBody)
        | _ -> None

    // Local loop names are made unique: a loop that used to live inside an
    // expression got its own lambda scope, but now becomes a C# local function
    // in the *enclosing* block, where a sibling of the same name could collide.
    let renames =
        bindings |> List.map (fun (n, _, _, _) -> n, fresh n) |> Map.ofList

    let renamedBindings =
        bindings
        |> List.map (fun (n, isFun, args, v) -> renames[n], isFun, args, renameExpr renames v)

    let renamedBody = renameExpr renames body

    match renamedBindings |> List.map asFunction |> List.forall Option.isSome with
    | false ->
        // An explicit `letrec` over values rather than functions. There is nothing
        // to jump to, so keep the mutually-visible-declaration encoding.
        { expr with
            Node =
                TLetRec(
                    renamedBindings
                    |> List.map (fun (n, isFun, args, v) -> n, isFun, args, lowerExpr [] true v),
                    lowerExpr targets inTail renamedBody
                ) }
    | true ->
        let members = renamedBindings |> List.map (asFunction >> Option.get)
        let names = renamedBindings |> List.map (fun (n, _, _, _) -> n)

        // Slots are fresh so that the per-iteration locals can keep the source's
        // parameter names, which is what the member bodies already read.
        let slotNames =
            members
            |> List.map (fun (lambdaArgs, _, _, _) -> lambdaArgs |> List.map (fun a -> fresh ("_" + a)))

        let loopTargets =
            List.mapi2
                (fun i name slots ->
                    let _, argTypes, _, _ = members[i]

                    { Index = i
                      Names = [ name ]
                      Mandatory = List.zip slots argTypes
                      Keywords = []
                      Rest = None })
                names
                slotNames

        let loweredMembers =
            List.mapi2
                (fun i name slots ->
                    let lambdaArgs, argTypes, retType, lambdaBody = members[i]

                    { LoopName = name
                      Slots = List.zip slots argTypes
                      Locals = lambdaArgs
                      RetType = retType
                      Body = lowerExpr loopTargets true lambdaBody })
                names
                slotNames

        // The group's own body is *outside* the loops: entering a loop from here
        // is a call, not a jump, so it keeps the enclosing scope's targets.
        { expr with
            Node = TLoop(loweredMembers, Some(lowerExpr targets inTail renamedBody)) }

// ---------------------------------------------------------------------------
// Declarations
// ---------------------------------------------------------------------------

/// Trait-dictionary parameters are prepended by `Lowering.fs`. They are constant
/// across iterations and a self-call never passes them (a recursive occurrence is
/// bound monomorphically, so it carries no type arguments and picks up no
/// dictionaries), so they are not loop slots.
let private isDictionaryParam (name: string) = name.StartsWith "_dict_"

/// Lowers a function body: `TLoop (_, None)` when it recurs, unchanged otherwise.
let private lowerFunctionBody
    (names: string list)
    (args: (string * HMType) list)
    (kwArgs: (string * HMType * TypedExpr) list)
    (restArg: (string * HMType) option)
    (retType: HMType)
    (body: TypedExpr)
    : TypedExpr =

    let name = List.head names

    let target =
        { Index = 0
          Names = names
          Mandatory = args |> List.filter (fst >> isDictionaryParam >> not)
          Keywords = kwArgs
          Rest = restArg }

    let lowered = lowerExpr [ target ] true body

    if not (containsRecur lowered) then
        lowered
    else
        let slots = slotsOf target
        let locals = slots |> List.map (fun (n, _) -> fresh ("_" + n))

        let toLocals = List.zip (slots |> List.map fst) locals |> Map.ofList

        { body with
            Node =
                TLoop(
                    [ { LoopName = name
                        Slots = slots
                        Locals = locals
                        RetType = retType
                        Body = renameExpr toLocals lowered } ],
                    None
                ) }

/// `aliasFor` supplies the extra name a trait-`impl` method answers to.
let rec private lowerDeclWith (aliasFor: string -> string list) (decl: TDecl) : TDecl =
    match decl with
    | TModule(name, decls, r) -> TModule(name, decls |> List.map (lowerDeclWith aliasFor), r)

    | TImpl(traitName, targetType, assoc, methods, r) ->
        // A concrete self-call inside an `impl` method was devirtualized by
        // `Lowering.fs`, so the method no longer calls itself under its own name.
        let implAlias (methodName: string) =
            match targetType with
            | TCon(targetTypeName, _) -> [ Lowering.implInstanceMethod traitName targetTypeName methodName ]
            | _ -> []

        TImpl(traitName, targetType, assoc, methods |> List.map (lowerDeclWith implAlias), r)

    | TDefun(name, tyArgs, args, kwArgs, restArg, retType, body, r) ->
        let loweredKwArgs = kwArgs |> List.map (fun (n, t, e) -> n, t, lowerExpr [] false e)

        TDefun(
            name,
            tyArgs,
            args,
            loweredKwArgs,
            restArg,
            retType,
            lowerFunctionBody (name :: aliasFor name) args loweredKwArgs restArg retType body,
            r
        )

    | TDef(name, value, t, r) -> TDef(name, lowerExpr [] false value, t, r)
    | TDefTuple(names, value, t, r) -> TDefTuple(names, lowerExpr [] false value, t, r)
    | TDefMutable(name, value, t, r) -> TDefMutable(name, lowerExpr [] false value, t, r)
    | _ -> decl

let lowerDecl (decl: TDecl) : TDecl = lowerDeclWith (fun _ -> []) decl

let lowerProgram (decls: TDecl list) : TDecl list = List.map lowerDecl decls
