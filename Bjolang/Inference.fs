module Bjolang.Inference

open Bjolang.Lexer
open Bjolang.Parser
open Bjolang.TypedAST
open Bjolang.Unification

/// Walk a typed expression body for the trait constraints its enclosing function
/// must carry. Returns a list of TraitConstraints (TraitName, TargetType as TVar).
///
/// There are two ways a body demands one:
///
///   1. It calls a trait method on a type variable — `(fold c ...)` needs
///      `(Foldable %c)`.
///   2. It calls a *constrained function* and passes a type variable where that
///      function's constrained parameter goes — calling `count` at `%c` needs
///      whatever `count` needs of it. The constraint has to be re-advertised
///      here, because the dictionary can only come from our own caller.
let collectTraitConstraints (env: Env) (body: TypedExpr) : TraitConstraint list =
    let registry = env.Registry

    let step (acc: Set<string * string>) (expr: TypedExpr) =
        match expr.Node with
        // A trait call the solver could not pin down. The node says which trait
        // it belongs to, so there is nothing to guess: looking the method name
        // up across every trait picked an arbitrary one whenever two traits
        // shared a method.
        | TTraitCall(tref, _, _) when tref.Resolved.IsNone ->
            tref.Holes
            |> List.fold
                (fun acc hole ->
                    match prune registry hole with
                    | TVar varName -> Set.add (tref.Trait, varName) acc
                    | _ -> acc)
                acc

        | TApply({ Node = TIdent(calleeName, tArgs) }, _, _) ->
            // `tArgs` is positionally aligned with the callee's scheme
            // variables, so it says what each of them was instantiated to
            // at this call site.
            match Map.tryFind calleeName env.Bindings with
            | Some binding ->
                let (Scheme(schemeVars, constraints, _)) = binding.Scheme

                if constraints.IsEmpty || schemeVars.Length <> tArgs.Length then
                    acc
                else
                    let varSubst = List.zip schemeVars tArgs |> Map.ofList

                    constraints
                    |> List.fold
                        (fun acc c ->
                            let instantiated =
                                match c.TargetType with
                                | TVar v -> Map.tryFind v varSubst |> Option.defaultValue c.TargetType
                                | t -> t

                            // A concrete instantiation resolves to a real
                            // impl at this call site and needs nothing from
                            // our caller.
                            match prune registry instantiated with
                            | TVar varName -> Set.add (c.TraitName, varName) acc
                            | _ -> acc)
                        acc
            | None -> acc
        | _ -> acc

    TypeVisitor.foldExpr step Set.empty body
    |> Set.toList
    |> List.map (fun (traitName, varName) ->
        { TraitName = traitName; TargetType = TVar varName })

// --- INFERENCE ENGINE ---
let inferNumericType (value: string) : HMType =
    if value.EndsWith("uy") then TypeConstants.byteType
    elif value.EndsWith("s") then TypeConstants.shortType
    elif value.EndsWith("us") then TypeConstants.ushortType
    elif value.EndsWith("u") then TypeConstants.uintType
    elif value.EndsWith("UL") || value.EndsWith("ul") || value.EndsWith("uL") then TypeConstants.ulongType
    elif value.EndsWith("L") || value.EndsWith("l") then TypeConstants.longType
    elif value.EndsWith("d") || value.EndsWith("D") || value.Contains(".") then TypeConstants.doubleType
    else TypeConstants.intType

let rec applyTypeSubst (subst: Map<string, HMType>) (t: HMType) =
    match t with
    | TVar n -> match Map.tryFind n subst with Some t' -> t' | None -> t
    | TCon(n, args) -> TCon(n, List.map (applyTypeSubst subst) args)
    | TFun(args, ret) -> TFun(List.map (applyTypeSubst subst) args, applyTypeSubst subst ret)
    | TTuple args -> TTuple(List.map (applyTypeSubst subst) args)
    | TAssoc(tn, an, impl) -> TAssoc(tn, an, applyTypeSubst subst impl)
    | _ -> t

/// The environment slot a `seq` records its element type in, so that the
/// `yield`s in its body have something to unify against.
///
/// A `yield` belongs to the nearest enclosing `seq`, which is precisely ordinary
/// lexical scoping — so it is expressed as an ordinary binding rather than as a
/// side channel, and a nested `seq` shadows the outer one for free. The name
/// contains a space, which no token can, so nothing in source can collide with
/// it or read it.
let private seqElementSlot = " seq-element"

let private withSeqElement (elemType: HMType) (env: Env) : Env =
    { env with
        Bindings =
            Map.add
                seqElementSlot
                { Scheme = Scheme([], [], elemType); IsMutable = false }
                env.Bindings }

/// Leaves the enclosing `seq`, if any. A lambda body is compiled as a function
/// of its own and cannot be resumed, so it cannot yield into the sequence it
/// happens to be written inside.
let private withoutSeqElement (env: Env) : Env =
    { env with Bindings = Map.remove seqElementSlot env.Bindings }

let private currentSeqElement (env: Env) (formName: string) (r: Range) : HMType =
    match Map.tryFind seqElementSlot env.Bindings with
    | Some binding ->
        let (Scheme(_, _, t)) = binding.Scheme
        t
    | None ->
        failwithf
            $"Type Error: '%s{formName}' only means something inside a (seq ...) body, and there is none here, at %s{Lexer.formatPos r}"

let rec checkPattern (env: Env) (expectedType: HMType) (pat: Pattern) : TypedPattern * Map<string, HMType> =
    match pat with
    | PWildcard r ->
        { Type = expectedType
          Range = r
          Node = TPWildcard },
        Map.empty
    | PIdent(name, r) ->
        { Type = expectedType
          Range = r
          Node = TPIdent name },
        Map.add name expectedType Map.empty
    | PInt(value, r) ->
        let inferredType = inferNumericType value

        unify env.Registry expectedType inferredType
        { Type = inferredType
          Range = r
          Node = TPInt value },
        Map.empty
    | PString(value, r) ->
        unify env.Registry expectedType TypeConstants.stringType

        { Type = TypeConstants.stringType
          Range = r
          Node = TPString value },
        Map.empty
    | PConstruct(name, args, r) ->
        let binding = 
            match Map.tryFind name env.Bindings with
            | Some b -> b
            | None -> failwithf $"Pattern Error: Unknown constructor '%s{name}' at line %d{r.Start.Line}"

        let consType, _, _ = instantiate env.Registry binding.Scheme

        let argTypes, returnType =
            match prune env.Registry consType with
            | TFun(tArgs, ret) -> tArgs, prune env.Registry ret
            | _ -> [], prune env.Registry consType

        unify env.Registry expectedType returnType

        if args.Length <> argTypes.Length then
            failwithf $"Pattern Error: Constructor {name} expects {argTypes.Length} arguments but got {args.Length} at line {r.Start.Line}"

        let mutable currentEnv = Map.empty
        let typedArgs =
            List.zip argTypes args
            |> List.map (fun (expectedArgType, argPat) ->
                let tp, boundEnv = checkPattern env expectedArgType argPat
                currentEnv <- Map.fold (fun acc k v -> Map.add k v acc) currentEnv boundEnv
                tp)

        { Type = returnType
          Range = r
          Node = TPConstruct(name, typedArgs) },
        currentEnv
    | PList(items, tailOpt, r) ->
        let elemType = freshMeta ()
        let listType = TCon("List", [ elemType ])
        unify env.Registry expectedType listType
        let mutable currentEnv = Map.empty

        let typedItems =
            items
            |> List.map (fun p ->
                let tp, env = checkPattern env elemType p
                currentEnv <- Map.fold (fun acc k v -> Map.add k v acc) currentEnv env
                tp)

        let typedTail =
            tailOpt
            |> Option.map (fun p ->
                let tp, env = checkPattern env listType p
                currentEnv <- Map.fold (fun acc k v -> Map.add k v acc) currentEnv env
                tp)

        { Type = listType
          Range = r
          Node = TPList(typedItems, typedTail) },
        currentEnv
    | PVec(items, tailOpt, r) ->
        let elemType = freshMeta ()
        let vecType = TCon("Vec", [ elemType ])
        unify env.Registry expectedType vecType
        let mutable currentEnv = Map.empty

        let typedItems =
            items
            |> List.map (fun p ->
                let tp, env = checkPattern env elemType p
                currentEnv <- Map.fold (fun acc k v -> Map.add k v acc) currentEnv env
                tp)

        // A rest pattern captures the remaining elements, so it is itself a Vec.
        let typedTail =
            tailOpt
            |> Option.map (fun p ->
                let tp, env = checkPattern env vecType p
                currentEnv <- Map.fold (fun acc k v -> Map.add k v acc) currentEnv env
                tp)

        { Type = vecType
          Range = r
          Node = TPVec(typedItems, typedTail) },
        currentEnv

let private typeNameMap =
    Map.ofList [
        "int", TypeConstants.intType
        "byte", TypeConstants.byteType
        "short", TypeConstants.shortType
        "ushort", TypeConstants.ushortType
        "uint", TypeConstants.uintType
        "long", TypeConstants.longType
        "ulong", TypeConstants.ulongType
        "double", TypeConstants.doubleType
        "string", TypeConstants.stringType
        "bool", TypeConstants.boolType
        "void", TypeConstants.voidType
    ]

let rec resolveTypeAnnotation (registry: TraitRegistry) (ptype: FType) : HMType =
    match ptype with
    | TName(name, _) ->
        if name.StartsWith("'") then
            TVar name
        else
            match Map.tryFind name registry.Aliases with
            | Some (args, t) when args.Length = 0 -> t
            | Some (args, _) -> failwithf $"Type alias {name} expects {args.Length} arguments, but got 0"
            | None ->
                match Map.tryFind name typeNameMap with
                | Some t -> t
                | None -> TCon(name, [])
    | TApp("->", args, _) ->
        let resolvedArgs = args |> List.map (resolveTypeAnnotation registry)
        TFun(List.take (resolvedArgs.Length - 1) resolvedArgs, List.last resolvedArgs)
    | TArrow(mandatory, keywords, restOpt, ret, _) ->
        let mandatoryTypes = mandatory |> List.map (resolveTypeAnnotation registry)
        let keywordTypes = keywords |> List.map (fun (_, t) -> resolveTypeAnnotation registry t)
        let restArrayType =
            match restOpt with
            | Some rt -> [TCon("Array", [resolveTypeAnnotation registry rt])]
            | None -> []
        let retType = resolveTypeAnnotation registry ret
        let allArgTypes = mandatoryTypes @ keywordTypes @ restArrayType
        TFun(allArgTypes, retType)
    // (assoc Trait item 'col) — an associated type projected out of an
    // implementor. Written by the export-metadata serializer rather than by
    // hand: inside a `def/trait` an associated type is named directly.
    // `(Tuple a b)` is the tuple type, not a one-off constructor named "Tuple".
    // It is also what `serializeHMType` writes for a `TTuple`, so without this
    // no exported signature mentioning a tuple could be read back.
    | TApp("Tuple", args, _) -> TTuple(args |> List.map (resolveTypeAnnotation registry))
    | TApp("assoc", [ TName(traitName, _); TName(assocName, _); implType ], _) ->
        TAssoc(traitName, assocName, resolveTypeAnnotation registry implType)
    // `(%f int)` — a type variable applied to arguments. `HMType` has no case
    // for this and deliberately never will: giving the unifier one makes it
    // higher-order. Only an inline trait's own constructor variable may be
    // written applied, and `resolveTemplate` reads those, not this function.
    //
    // Falling through to the general case turned it into a type constructor
    // literally named `'f`, which then failed much later with a confusing
    // complaint about a missing implementation for a type nobody wrote.
    | TApp(name, _, r) when name.StartsWith "'" ->
        failwithf
            $"Kind Error at %s{Lexer.formatPos r}: the type variable %%%s{name.TrimStart('\'')} is applied to arguments here. Bjolang has no higher-kinded type variables: only the constructor variable of an inline trait may be written applied, and only inside that trait's own signatures. A function cannot be generic over a type constructor."

    | TApp(name, args, _) ->
        let resolvedArgs = args |> List.map (resolveTypeAnnotation registry)
        match Map.tryFind name registry.Aliases with
        | Some (typeParams, t) ->
            if typeParams.Length <> resolvedArgs.Length then
                failwithf $"Type alias {name} expects {typeParams.Length} arguments, but got {resolvedArgs.Length}"
            let normalizeParam (p: string) = if p.StartsWith("'") then p else "'" + p
            let subst = List.zip (typeParams |> List.map normalizeParam) resolvedArgs |> Map.ofList
            applyTypeSubst subst t
        | None -> TCon(name, resolvedArgs)


// ---------------------------------------------------------------------------
// Inline-trait signature templates
// ---------------------------------------------------------------------------

let rec private hmToTpl (t: HMType) : TplType =
    match t with
    | TCon(n, args) -> TplCon(n, List.map hmToTpl args)
    | TVar n -> TplVar n
    | TFun(args, ret) -> TplFun(List.map hmToTpl args, hmToTpl ret)
    | TTuple ts -> TplTuple(List.map hmToTpl ts)
    | other -> failwithf $"Type Error: %A{other} may not appear in an inline trait's signature"

/// Reads a trait signature that mentions the implementor *applied*.
///
/// The result is a `TplType`, never an `HMType`: `m` occurs at two different
/// argument lists in `bind`, and giving the unifier a case for that would make
/// it higher-order. Instantiation at an impl (see `instantiateTemplate`)
/// eliminates the hole and hands inference an ordinary first-order type.
let rec resolveTemplate (registry: TraitRegistry) (holeVar: string) (ftype: FType) : TplType =
    let go = resolveTemplate registry holeVar
    let holeName = "'" + holeVar

    match ftype with
    | TName(name, r) when name = holeName ->
        failwithf
            $"Type Error at %s{Lexer.formatPos r}: the constructor variable %%%s{holeVar} must be written applied, as (%%%s{holeVar} ...)."
    | TName _ -> hmToTpl (resolveTypeAnnotation registry ftype)
    | TApp("->", args, _) ->
        let resolved = args |> List.map go
        TplFun(List.take (resolved.Length - 1) resolved, List.last resolved)
    | TArrow(mandatory, keywords, restOpt, ret, r) ->
        if not keywords.IsEmpty || restOpt.IsSome then
            failwithf
                $"Type Error at %s{Lexer.formatPos r}: an inline trait's methods may not take keyword or rest parameters."
        TplFun(mandatory |> List.map go, go ret)
    | TApp("Tuple", args, _) -> TplTuple(args |> List.map go)
    | TApp(name, args, _) when name = holeName -> TplHole(args |> List.map go)
    | TApp(name, args, _) when name.StartsWith "'" ->
        failwithf
            $"Type Error: only the trait's own constructor variable may be applied in a signature; %s{name} is an ordinary type variable."
    | TApp(name, args, _) ->
        match Map.tryFind name registry.Aliases with
        | Some _ ->
            // An alias may expand into anything, including something that hides
            // the hole. Resolving it as an ordinary type is only sound when no
            // argument mentions the hole.
            hmToTpl (resolveTypeAnnotation registry ftype)
        | None -> TplCon(name, args |> List.map go)

/// Instantiates a template for one call site: every ordinary type variable
/// becomes a fresh meta, and every *occurrence* of the hole becomes a meta of
/// its own together with the arguments it was applied to.
///
/// The hole metas are shared with the surrounding expression, which is the whole
/// point: the call node is fully typed immediately, with the constructor still
/// unknown, and whatever later pins one of them — an argument, an enclosing
/// `bind`, a declared return type — pins the constructor.
let instantiateTemplateFresh (tpl: TplType) : HMType * (HMType * HMType list) list =
    let varMap = System.Collections.Generic.Dictionary<string, HMType>()
    let holes = ResizeArray<HMType * HMType list>()

    let rec go t =
        match t with
        | TplVar n ->
            match varMap.TryGetValue n with
            | true, m -> m
            | _ ->
                let m = freshMeta ()
                varMap[n] <- m
                m
        | TplCon(n, args) -> TCon(n, List.map go args)
        | TplFun(args, ret) -> TFun(List.map go args, go ret)
        | TplTuple ts -> TTuple(List.map go ts)
        | TplHole args ->
            let argTypes = List.map go args
            let m = freshMeta ()
            holes.Add(m, argTypes)
            m

    let t = go tpl
    t, List.ofSeq holes

// ---------------------------------------------------------------------------
// Deferred trait resolution
// ---------------------------------------------------------------------------

/// A trait obligation raised by a call and discharged later.
///
/// `bind` resolves from its first argument, but `pure : 'a -> m 'a` mentions the
/// constructor only in its *result*, so any rule that reads argument zero cannot
/// see it at all. Rather than make `infer` bidirectional, the obligation is
/// simply recorded and revisited once the surrounding expression has had its say.
type Wanted =
    { Trait: string
      Method: string
      Kind: TraitKind
      /// One entry per occurrence of the hole: the meta standing for the
      /// constructor application, and the arguments it was applied to.
      HoleArgs: (HMType * HMType list) list
      /// The AST node that reads the answer back.
      Ref: TraitRef
      Range: Range }

let private wantedQueue = ResizeArray<Wanted>()

/// Every metavariable a type still mentions, following bindings by hand.
///
/// Deliberately registry-free: this is consulted from `generalize`, which has no
/// business being handed a queue, let alone an environment.
let rec private metaIdsOf (t: HMType) : int list =
    match t with
    | TMeta m ->
        match m.Value with
        | Some inner -> metaIdsOf inner
        | None -> [ m.Id ]
    | TCon(_, args)
    | TTuple args -> List.collect metaIdsOf args
    | TFun(args, ret) -> (List.collect metaIdsOf args) @ metaIdsOf ret
    | TAssoc(_, _, impl) -> metaIdsOf impl
    | TVar _ -> []

/// The holes an unresolved *inline*-trait obligation is still watching.
///
/// A local helper written without a signature — `(defun (bump fa) (fmap fa inc))`
/// — is let-polymorphic, so its parameter's metavariable used to be quantified
/// the moment the binding was finished, long before any call site said what it
/// was. Resolution then found a rigid type variable and reported that an
/// inline-only trait cannot be used generically, which was true of the type it
/// had just been given and false of the program that was written.
///
/// Holding these back makes such a binding monomorphic, which is the only thing
/// it can honestly be: one use site, one constructor, resolved and inlined.
let private heldByInlineWanteds () : Set<int> =
    wantedQueue
    |> Seq.filter (fun w -> w.Kind = InlineTrait && w.Ref.Resolved.IsNone)
    |> Seq.collect (fun w -> w.HoleArgs |> Seq.collect (fun (m, _) -> metaIdsOf m))
    |> Set.ofSeq

do Unification.heldMetaIds <- heldByInlineWanteds

let private pushWanted (w: Wanted) = wantedQueue.Add w

/// Detaches everything raised so far. Callers solve what they take.
let takeWanteds () : Wanted list =
    let ws = List.ofSeq wantedQueue
    wantedQueue.Clear()
    ws

/// Instantiates an impl's target pattern, giving fresh metas to the impl's own
/// prefix variables.
///
/// Returns the prefix to unify the hole against, and — separately — the metas
/// standing for the class's *type parameters*. The two are not the same list:
/// `impl Show for (List int)` has a one-argument prefix and no type parameters
/// at all, and naming `Show_List<int>` for it is a type error in C#.
let private instantiateImplPrefix (target: ImplTarget) : HMType list * HMType list =
    let vars = target.FixedPrefix |> List.collect typeVarsOf |> List.distinct
    let subst = vars |> List.map (fun v -> v, freshMeta ()) |> Map.ofList
    let prefix = target.FixedPrefix |> List.map (substTypeVars subst)
    prefix, vars |> List.map (fun v -> subst[v])

let private tryResolveWanted (env: Env) (w: Wanted) : bool =
    if w.Ref.Resolved.IsSome then
        true
    else

    let registry = env.Registry

    let ctorOpt =
        w.HoleArgs
        |> List.tryPick (fun (m, _) ->
            match prune registry m with
            | TCon(ctor, _) -> Some ctor
            | _ -> None)

    match ctorOpt with
    | None -> false
    | Some ctor ->
        match Map.tryFind (w.Trait, ctor) registry.ImplTargets with
        | None ->
            failwithf
                $"Type Error at %s{Lexer.formatPos w.Range}: no implementation of trait '%s{w.Trait}' for '%s{ctor}', required by '%s{w.Method}'."
        | Some target ->
            let prefix, classTypeArgs = instantiateImplPrefix target

            for (m, occArgs) in w.HoleArgs do
                unify registry m (TCon(ctor, prefix @ occArgs))

            w.Ref.Resolved <- Some(ctor, classTypeArgs |> List.map (prune registry))
            true

/// Runs the wanted queue to a fixpoint, then reports what is left.
///
/// An unsolved `InterfaceTrait` obligation is not an error: it is exactly the
/// generic-receiver case the dictionary path already handles, and leaving it
/// alone is what keeps the current semantics intact.
let solveWanteds (env: Env) (wanteds: Wanted list) : unit =
    let mutable pending = wanteds |> List.filter (fun w -> w.Ref.Resolved.IsNone)
    let mutable progress = true

    while progress && not pending.IsEmpty do
        progress <- false

        pending <-
            pending
            |> List.filter (fun w ->
                if tryResolveWanted env w then
                    progress <- true
                    false
                else
                    true)

    for w in pending do
        match w.Kind with
        | InterfaceTrait -> ()
        | InlineTrait ->
            let holes = w.HoleArgs |> List.map (fst >> prune env.Registry)

            if holes |> List.exists (function TVar _ -> true | _ -> false) then
                failwithf
                    $"Type Error at %s{Lexer.formatPos w.Range}: '%s{w.Method}' cannot be used at a generic type; '%s{w.Trait}' is an inline-only trait, so there is no dictionary to pass. Give the call a concrete type, or make the caller monomorphic."
            else
                failwithf
                    $"Type Error at %s{Lexer.formatPos w.Range}: cannot determine which '%s{w.Trait}' instance '%s{w.Method}' uses here; add a type annotation. Nothing in this expression says what the constructor is — a `(do ...)` block with no `:bind` never mentions one."

/// Solves everything raised since the last call. Used at every point that is
/// about to generalize, since a scheme must not be built over a constructor
/// that resolution would still have pinned down.
let solvePending (env: Env) : unit = solveWanteds env (takeWanteds ())

/// Reads an impl's target as a pattern.
///
/// The trait's constructor variable abstracts over the *trailing* `HoleArity`
/// arguments; everything before them is fixed by this impl.
let implTargetOf (traitName: string) (info: TraitInfo) (targetType: HMType) (r: Range) : ImplTarget =
    match targetType with
    | TCon(ctor, args) ->
        if args.Length < info.HoleArity then
            failwithf
                $"Kind Error at %s{Lexer.formatPos r}: trait '%s{traitName}' abstracts over the last %d{info.HoleArity} argument(s) of its implementor, but '%s{ctor}' is applied to only %d{args.Length}. A constructor whose abstracted argument is not last — `Either e` in the first position — needs a newtype that flips them."

        { Ctor = ctor
          FixedPrefix = args |> List.take (args.Length - info.HoleArity)
          HoleArity = info.HoleArity }
    | _ -> failwithf $"Trait implementations require concrete target types at %s{Lexer.formatPos r}"

/// Instantiates a trait method at a call site and records the obligation.
let private traitCallType (env: Env) (traitName: string) (methodName: string) (r: Range) : HMType * TraitRef =
    let info = Map.find traitName env.Registry.Traits

    let methodType, holeArgs =
        match info.Kind with
        | InlineTrait ->
            match Map.tryFind methodName info.Templates with
            | Some tpl -> instantiateTemplateFresh tpl
            | None -> failwithf $"Internal error: '%s{methodName}' is not a method of inline trait '%s{traitName}'"
        | InterfaceTrait ->
            // Instantiated from the trait's own signature rather than from
            // whatever `methodName` happens to be bound to. Inside an `impl`
            // the method is also bound monomorphically, for recursion, and that
            // binding quantifies nothing — so reading the implementor out of a
            // scheme's type arguments found no hole at all for a self-call.
            let sigType =
                match Map.tryFind methodName info.Signatures with
                | Some t -> t
                | None -> failwithf $"Internal error: '%s{methodName}' is not a method of trait '%s{traitName}'"

            let implVar = "'" + info.ImplementorVar

            // An associated type is a projection out of the implementor, so it
            // is pinned by the same meta rather than being free on its own.
            let assocSubst =
                info.AssociatedTypes
                |> List.map (fun a -> "'" + a, TAssoc(traitName, a, TVar implVar))
                |> Map.ofList

            let withAssoc = applyTypeSubst assocSubst sigType

            let vars =
                implVar :: (freeTVars env.Registry withAssoc |> List.distinct |> List.filter ((<>) implVar))

            let subst = vars |> List.map (fun v -> v, freshMeta ()) |> Map.ofList

            // An implementor of arity zero is the hole, applied to nothing.
            applyTypeSubst subst withAssoc, [ subst[implVar], [] ]

    let tref =
        { Trait = traitName
          Method = methodName
          Holes = holeArgs |> List.map fst
          Resolved = None }

    pushWanted
        { Trait = traitName
          Method = methodName
          Kind = info.Kind
          HoleArgs = holeArgs
          Ref = tref
          Range = r }

    methodType, tref

/// Instantiates a record type with fresh type variables.
///
/// The record type and its field types have to be instantiated under the *same*
/// substitution, or a field's type variable would be unrelated to the one in the
/// record type it came from. Returns the instantiated record type, the declared
/// fields as written, and the field types under that substitution.
let private instantiateRecord
    (registry: TraitRegistry)
    (recordTypeName: string)
    : HMType * (string * HMType) list * Map<string, HMType> =

    let tArgs, expectedFields = Map.find recordTypeName registry.Records
    let tArgsInst = tArgs |> List.map (fun a -> a.TrimStart('\''))
    let recordScheme = Scheme(tArgsInst, [], TCon(recordTypeName, tArgsInst |> List.map TVar))

    let instantiatedRecordType, freshVars, _ = instantiate registry recordScheme
    let fieldSubst = List.zip tArgsInst freshVars |> Map.ofList

    let expectedFieldsInstantiated =
        expectedFields |> List.map (fun (n, t) -> n, applyTypeSubst fieldSubst t) |> Map.ofList

    instantiatedRecordType, expectedFields, expectedFieldsInstantiated


let rec infer (env: Env) (expr: Expr) : HMType * TypedExpr =
    match expr with
    | EInt(value, r) ->
        let inferredType = inferNumericType value

        inferredType,
        { Type = inferredType
          Range = r
          Node = TInt value }
    | EString(value, r) ->
        TypeConstants.stringType,
        { Type = TypeConstants.stringType
          Range = r
          Node = TString value }

    // An inline trait's methods are never bound as values: there is no single
    // scheme they could be bound under, which is the whole reason the trait is
    // inline-only.
    | EIdent(name, r) when
        Map.containsKey name env.Registry.TraitMethods
        && not (Map.containsKey name env.Bindings)
        ->
        let traitName = env.Registry.TraitMethods[name]

        failwithf
            $"Type Error at %s{Lexer.formatPos r}: '%s{name}' is a method of the inline-only trait '%s{traitName}' and has no value form. Apply it directly, or wrap it in a lambda at a known type."

    | EIdent(name, r) ->
        let binding = lookup env name
        let t, tArgs, constraints = instantiate env.Registry binding.Scheme

        t,
        { Type = t
          Range = r
          Node = TIdent(name, tArgs) }

    | EFun(args, body, r) ->
        let argTypes = args |> List.map (fun _ -> freshMeta ())

        let localEnv =
            List.zip args argTypes
            |> List.fold
                (fun acc (n, t) ->
                    addBinding
                        n
                        { Scheme = Scheme([], [], t)
                          IsMutable = false }
                        acc)
                (withoutSeqElement env)

        let bodyType, typedBody = infer localEnv body
        let funType = TFun(argTypes, bodyType)

        funType,
        { Type = funType
          Range = r
          Node = TLambda(args, typedBody) }

    // A trait method in application position.
    //
    // The call is typed immediately — every position in the template gets a
    // fresh meta and the arguments and result are unified against them — while
    // *which* implementation runs is left blank for the solver. That is what
    // lets `pure`, whose constructor appears only in its result, be resolved at
    // all: the metas are shared with the surrounding expression, so an enclosing
    // `bind` or a declared return type pins them.
    | EApp(EIdent(methodName, _), args, r) when Map.containsKey methodName env.Registry.TraitMethods ->
        let traitName = env.Registry.TraitMethods[methodName]

        let typedArgs =
            args
            |> List.map (function
                | EKeyword(kw, kr) ->
                    failwithf
                        $"Type Error at %s{Lexer.formatPos kr}: trait method '%s{methodName}' takes positional arguments only, but was given '#:%s{kw}'."
                | a -> infer env a)

        let methodType, tref = traitCallType env traitName methodName r
        let retType = freshMeta ()
        unify env.Registry methodType (TFun(typedArgs |> List.map fst, retType))

        retType,
        { Type = retType
          Range = r
          Node = TTraitCall(tref, typedArgs |> List.map snd, []) }

    | EApp(target, args, r) ->
        let targetType, typedTarget = infer env target

        // Separate keyword args from positional args
        // Keyword args appear as EKeyword("name") followed by a value expr
        let rec splitArgs positional keywords remaining =
            match remaining with
            | [] -> List.rev positional, List.rev keywords
            | EKeyword(kwName, _) :: value :: rest ->
                let valType, typedVal = infer env value
                splitArgs positional ((kwName, (valType, typedVal)) :: keywords) rest
            | EKeyword(kwName, kr) :: [] ->
                failwithf $"Keyword argument '#:%s{kwName}' is missing a value at line %d{kr.Start.Line}"
            | arg :: rest ->
                let argType, typedArg = infer env arg
                splitArgs ((argType, typedArg) :: positional) keywords rest

        let positionalArgs, keywordArgs = splitArgs [] [] args
        let retType = freshMeta ()

        // Look up FunMeta if the target is a known identifier
        let funMeta =
            match target with
            | EIdent(name, _) -> Map.tryFind name env.FunMetas
            | _ -> None

        match funMeta with
        | Some meta when not keywordArgs.IsEmpty || meta.RestParam.IsSome || not meta.KeywordParams.IsEmpty ->
            // Structured call: separate mandatory, keyword, and rest args
            let mandatoryArgs = positionalArgs |> List.take (min positionalArgs.Length meta.MandatoryCount)
            let restArgs = positionalArgs |> List.skip (min positionalArgs.Length meta.MandatoryCount)

            // Build the flat arg types for unification (mandatory + keyword in decl order + rest array)
            let kwArgTypes =
                meta.KeywordParams |> List.map (fun (kwName, kwType) ->
                    match keywordArgs |> List.tryFind (fun (n, _) -> n = kwName) with
                    | Some (_, (valType, _)) ->
                        unify env.Registry valType kwType
                        kwType
                    | None -> kwType)  // keyword not provided, will use default

            let restArgTypes =
                match meta.RestParam with
                | Some elemType ->
                    for (rt, _) in restArgs do
                        unify env.Registry rt elemType
                    [TCon("Array", [elemType])]
                | None ->
                    if not restArgs.IsEmpty then
                        failwithf $"Too many arguments at line %d{r.Start.Line}"
                    []

            let allFlatTypes = (mandatoryArgs |> List.map fst) @ kwArgTypes @ restArgTypes
            unify env.Registry targetType (TFun(allFlatTypes, retType))

            let typedKwArgs =
                keywordArgs |> List.map (fun (n, (_, te)) -> (n, te))

            // Positional args in TApply = mandatory + rest (keyword args are separate)
            let positionalTypedArgs =
                (mandatoryArgs |> List.map snd) @ (restArgs |> List.map snd)

            retType,
            { Type = retType
              Range = r
              Node = TApply(typedTarget, positionalTypedArgs, typedKwArgs) }

        | _ ->
            // No FunMeta or no keyword args: simple positional call
            if not keywordArgs.IsEmpty then
                failwithf $"Keyword arguments used on a function without keyword parameter metadata at line %d{r.Start.Line}"

            unify env.Registry targetType (TFun(positionalArgs |> List.map fst, retType))

            retType,
            { Type = retType
              Range = r
              Node = TApply(typedTarget, positionalArgs |> List.map snd, []) }

    | ELet(name, isFun, args, typeAnn, value, body, r) ->
        let valType, typedVal =
            if isFun then
                let argTypes = args |> List.map (fun _ -> freshMeta ())

                let localEnv =
                    List.zip args argTypes
                    |> List.fold
                        (fun acc (n, t) ->
                            addBinding
                                n
                                { Scheme = Scheme([], [], t)
                                  IsMutable = false }
                                acc)
                        env

                let bodyType, typedBody = infer localEnv value
                let lambdaNode =
                    ({ Type = TFun(argTypes, bodyType)
                       Range = r
                       Node = TLambda(args, typedBody) } : TypedExpr)
                TFun(argTypes, bodyType), lambdaNode
            else
                infer env value

        match typeAnn with
        | Some tAnn ->
            let expectedType = resolveTypeAnnotation env.Registry tAnn
            unify env.Registry valType expectedType
        | None -> ()

        let rec isValue (expr: TypedExpr) =
            match expr.Node with
            | TInt _ -> true
            | TString _ -> true
            | TKeyword _ -> true
            | TSymbol _ -> true
            | TLambda(_, _) -> true
            | TIdent(_, _) -> true
            | TTupleMake es -> List.forall isValue es
            | TListMake es -> List.forall isValue es
            | TVecMake es -> List.forall isValue es
            | TRecordMake fields -> fields |> List.forall (snd >> isValue)
            | _ -> false

        let scheme = 
            if isFun || isValue typedVal then generalize env valType
            else Scheme([], [], valType)
        let localEnv = addBinding name { Scheme = scheme; IsMutable = false } env
        let bodyType, typedBody = infer localEnv body

        bodyType,
        { Type = bodyType
          Range = r
          Node = TLet(name, isFun, args, typedVal, typedBody) }

    | ELetRec(bindings, body, r) ->
        let bindingMetas = bindings |> List.map (fun (n, _, _, _, _) -> n, freshMeta ())

        let recEnv =
            bindingMetas
            |> List.fold
                (fun acc (n, t) ->
                    addBinding
                        n
                        { Scheme = Scheme([], [], t)
                          IsMutable = false }
                        acc)
                env

        let typedBindings =
            bindings
            |> List.mapi (fun i (name, isFun, args, typeAnn, expr) ->
                let expectedType = snd bindingMetas[i]

                let valType, typedVal =
                    if isFun then
                        let argTypes = args |> List.map (fun _ -> freshMeta ())

                        let localEnv =
                            List.zip args argTypes
                            |> List.fold
                                (fun acc (n, t) ->
                                    addBinding
                                        n
                                        { Scheme = Scheme([], [], t)
                                          IsMutable = false }
                                        acc)
                                recEnv

                        let bodyType, typedBody = infer localEnv expr
                        let lambdaNode =
                            ({ Type = TFun(argTypes, bodyType)
                               Range = r
                               Node = TLambda(args, typedBody) } : TypedExpr)
                        TFun(argTypes, bodyType), lambdaNode
                    else
                        infer recEnv expr

                unify env.Registry valType expectedType
                
                match typeAnn with
                | Some tAnn ->
                    let annType = resolveTypeAnnotation env.Registry tAnn
                    unify env.Registry valType annType
                | None -> ()
                
                name, isFun, args, typedVal)

        let finalEnv =
            bindingMetas
            |> List.fold
                (fun acc (n, t) ->
                    addBinding
                        n
                        { Scheme = generalize recEnv t
                          IsMutable = false }
                        acc)
                env

        let bodyType, typedBody = infer finalEnv body

        bodyType,
        { Type = bodyType
          Range = r
          Node = TLetRec(typedBindings, typedBody) }

    | ELetMutable(name, typeAnn, value, body, r) ->
        let valType, typedVal = infer env value
        
        match typeAnn with
        | Some tAnn ->
            let expectedType = resolveTypeAnnotation env.Registry tAnn
            unify env.Registry valType expectedType
        | None -> ()

        let localEnv =
            addBinding
                name
                { Scheme = generalize env valType
                  IsMutable = true }
                env

        let bodyType, typedBody = infer localEnv body

        bodyType,
        { Type = bodyType
          Range = r
          Node = TLetMutable(name, typedVal, typedBody) }

    | ESet(name, value, r) ->
        let valType, typedVal = infer env value
        let binding = lookup env name

        if not binding.IsMutable then
            failwithf $"Type Error: Cannot mutate immutable variable '%s{name}' at line %d{r.Start.Line}"

        let targetType, _, _ = instantiate env.Registry binding.Scheme
        unify env.Registry valType targetType

        TypeConstants.voidType,
        { Type = TypeConstants.voidType
          Range = r
          Node = TSet(name, typedVal) }

    | EIf(cond, trueBranch, falseBranch, r) ->
        let condType, tCond = infer env cond
        unify env.Registry condType TypeConstants.boolType
        let trueType, tTrue = infer env trueBranch
        let falseType, tFalse = infer env falseBranch
        unify env.Registry trueType falseType

        trueType,
        { Type = trueType
          Range = r
          Node = TIf(tCond, tTrue, tFalse) }

    | EWhen(cond, body, negated, r) ->
        let condType, tCond = infer env cond
        unify env.Registry condType TypeConstants.boolType

        // The body is evaluated for its effect and its value thrown away, so it
        // constrains nothing: there is no other arm for it to agree with, and
        // the form itself yields nothing.
        let _, tBody = infer env body

        TypeConstants.voidType,
        { Type = TypeConstants.voidType
          Range = r
          Node = TWhen(tCond, tBody, negated) }

    | EQuotedSymbol(sym, r) ->
        let t = TCon("Bjolang.Symbol", [])

        t,
        { Type = t
          Range = r
          Node = TSymbol sym }

    | EKeyword(kw, r) ->
        let t = TCon("Bjolang.Keyword", [])

        t,
        { Type = t
          Range = r
          Node = TKeyword kw }

    | ETuple(exprs, r) ->
        let typedExprs = exprs |> List.map (infer env)
        let tupleType = TTuple(typedExprs |> List.map fst)

        tupleType,
        { Type = tupleType
          Range = r
          Node = TTupleMake(typedExprs |> List.map snd) }

    | ELetTuple(names, value, body, r) ->
        let valType, typedVal = infer env value
        let elementMetas = names |> List.map (fun _ -> freshMeta ())
        unify env.Registry valType (TTuple elementMetas)

        let localEnv =
            List.zip names elementMetas
            |> List.fold
                (fun acc (n, t) ->
                    addBinding
                        n
                        { Scheme = Scheme([], [], t)
                          IsMutable = false }
                        acc)
                env

        let bodyType, typedBody = infer localEnv body

        bodyType,
        { Type = bodyType
          Range = r
          Node = TLetTuple(names, typedVal, typedBody) }

    | EList(exprs, r) ->
        let elementType = freshMeta ()

        let typedExprs =
            exprs
            |> List.map (fun e ->
                let t, te = infer env e
                unify env.Registry t elementType
                te)

        let listType = TCon("List", [ elementType ])

        listType,
        { Type = listType
          Range = r
          Node = TListMake typedExprs }

    | EVec(exprs, r) ->
        let elementType = freshMeta ()

        let typedExprs =
            exprs
            |> List.map (fun e ->
                let t, te = infer env e
                unify env.Registry t elementType
                te)

        let vecType = TCon("Vec", [ elementType ])

        vecType,
        { Type = vecType
          Range = r
          Node = TVecMake typedExprs }

    | ETryFinally(body, cleanup, r) ->
        let bodyType, tBody = infer env body
        let _, tCleanup = infer env cleanup

        bodyType,
        { Type = bodyType
          Range = r
          Node = TTryFinally(tBody, tCleanup) }

    | ESeq(body, r) ->
        let elemType = freshMeta ()

        // The body is run for its yields; whatever its last form evaluates to is
        // discarded, exactly as in `when`. A sequence's *value* is its elements,
        // so there is nothing for the body's own type to agree with.
        let _, tBody = infer (withSeqElement elemType env) body

        let seqType = TCon("Seq", [ elemType ])

        seqType,
        { Type = seqType
          Range = r
          Node = TSeq tBody }

    | EYield(value, r) ->
        let elemType = currentSeqElement env "yield" r
        let valueType, tValue = infer env value
        unify env.Registry valueType elemType

        TypeConstants.voidType,
        { Type = TypeConstants.voidType
          Range = r
          Node = TYield tValue }

    | EYieldFrom(source, r) ->
        let elemType = currentSeqElement env "yield-from" r
        let sourceType, tSource = infer env source
        unify env.Registry sourceType (TCon("Seq", [ elemType ]))

        TypeConstants.voidType,
        { Type = TypeConstants.voidType
          Range = r
          Node = TYieldFrom tSource }


    | EMatch(target, clauses, r) ->
        let targetType, typedTarget = infer env target
        let returnType = freshMeta ()

        let typedClauses =
            clauses
            |> List.map (fun (pat, guard, body) ->
                let typedPat, boundVars = checkPattern env targetType pat

                let boundEnv =
                    Map.fold
                        (fun acc n t ->
                            addBinding
                                n
                                { Scheme = Scheme([], [], t)
                                  IsMutable = false }
                                acc)
                        env
                        boundVars

                let typedGuard =
                    match guard with
                    | Some g ->
                        let gType, tg = infer boundEnv g
                        unify env.Registry gType TypeConstants.boolType
                        Some tg
                    | None -> None

                let bodyType, typedBody = infer boundEnv body

                unify env.Registry bodyType returnType

                { Pattern = typedPat
                  Guard = typedGuard
                  Body = typedBody }
                : TMatchClause)

        returnType,
        { Type = returnType
          Range = r
          Node = TMatch(typedTarget, typedClauses) }

    | ERecord(fields, r) ->
        if fields.IsEmpty then
            failwithf $"Type Error: Empty record creation at line %d{r.Start.Line}"
        
        let firstFieldName = fst fields.Head
        let recordTypeName =
            match Map.tryFind firstFieldName env.Registry.RecordFields with
            | Some tName -> tName
            | None -> failwithf $"Type Error: Unknown record field '%s{firstFieldName}' at line %d{r.Start.Line}"

        let instantiatedRecordType, expectedFields, expectedFieldsInstantiated =
            instantiateRecord env.Registry recordTypeName

        // Check each provided field against the instantiated expected field
        let fieldExprs = 
            fields |> List.map (fun (n, expr) ->
                let exprType, typedExpr = infer env expr
                match Map.tryFind n expectedFieldsInstantiated with
                | Some expectedType -> unify env.Registry exprType expectedType
                | None -> failwithf $"Type Error: Field '%s{n}' does not belong to record '%s{recordTypeName}' at line %d{r.Start.Line}"
                n, typedExpr)

        if fields.Length <> expectedFields.Length then
            failwithf $"Type Error: Missing fields for record '%s{recordTypeName}' at line %d{r.Start.Line}"

        instantiatedRecordType,
        { Type = instantiatedRecordType
          Range = r
          Node = TRecordMake fieldExprs }

    | EGetField(targetExpr, field, r) ->
        let targetType, typedTarget = infer env targetExpr
        let recordTypeName =
            match Map.tryFind field env.Registry.RecordFields with
            | Some tName -> tName
            | None -> failwithf $"Type Error: Unknown record field '%s{field}' at line %d{r.Start.Line}"

        let instantiatedRecordType, _, expectedFieldsInstantiated =
            instantiateRecord env.Registry recordTypeName

        unify env.Registry targetType instantiatedRecordType

        let fieldType =
            match Map.tryFind field expectedFieldsInstantiated with
            | Some t -> t
            | None -> failwithf $"Type Error: Field '%s{field}' does not belong to record '%s{recordTypeName}' at line %d{r.Start.Line}"

        fieldType,
        { Type = fieldType
          Range = r
          Node = TGetField(typedTarget, field) }

    | ERecordUpdate(targetName, fields, r) ->
        let targetBinding = lookup env targetName
        let targetType, _, _ = instantiate env.Registry targetBinding.Scheme
        
        let recordTypeName =
            if fields.IsEmpty then failwithf $"Type Error: Empty record-set at line %d{r.Start.Line}" else
            let firstField = fst fields.Head
            match Map.tryFind firstField env.Registry.RecordFields with
            | Some tName -> tName
            | None -> failwithf $"Type Error: Unknown record field '%s{firstField}' at line %d{r.Start.Line}"
            
        let instantiatedRecordType, _, expectedFieldsInstantiated =
            instantiateRecord env.Registry recordTypeName

        unify env.Registry targetType instantiatedRecordType

        let typedFields =
            fields |> List.map (fun (name, expr) ->
                let exprType, typedExpr = infer env expr
                match Map.tryFind name expectedFieldsInstantiated with
                | Some expectedType -> unify env.Registry exprType expectedType
                | None -> failwithf $"Type Error: Field '%s{name}' does not belong to record '%s{recordTypeName}' at line %d{r.Start.Line}"
                name, typedExpr)

        targetType,
        { Type = targetType
          Range = r
          Node = TRecordUpdate(targetName, typedFields) }

    | ECast(targetTypeAnnotation, expr, r) ->
        let targetType = resolveTypeAnnotation env.Registry targetTypeAnnotation
        let exprType, typedExpr = infer env expr
        targetType,
        { Type = targetType
          Range = r
          Node = TCast(typedExpr, targetType) }

// --- DECLARATION CHECKING ---

let registerTypeDefs (isRec: bool) (typeDefs: TypeDef list) (env: Env) : Env =
    // 1. Pre-register local types for recursion
    let localTypes = typeDefs |> List.fold (fun acc td -> Set.add td.Name acc) env.Registry.LocalTypes
    let preRegistry = { env.Registry with LocalTypes = localTypes }

    // 2. Resolve types and constructors
    let mutable finalRegistry = preRegistry
    let mutable finalBindings = env.Bindings

    for td in typeDefs do
        let tArgs = td.TypeArgs |> List.map (fun a -> if a.StartsWith("'") then a else "'" + a)
        let hmArgs = tArgs |> List.map TVar
        let parentType = TCon(td.Name, hmArgs)

        match td.Kind with
        | Alias ftype ->
            let resolved = resolveTypeAnnotation finalRegistry ftype
            finalRegistry <- { finalRegistry with Aliases = Map.add td.Name (tArgs, resolved) finalRegistry.Aliases }
        | Record fields ->
            let resolvedFields = fields |> List.map (fun f -> f.Name, resolveTypeAnnotation finalRegistry f.Type)
            finalRegistry <- { finalRegistry with Records = Map.add td.Name (tArgs, resolvedFields) finalRegistry.Records }
            for (fName, _) in resolvedFields do
                finalRegistry <- { finalRegistry with RecordFields = Map.add fName td.Name finalRegistry.RecordFields }
        | Union cases ->
            for case in cases do
                let caseName, resolvedArgs =
                    match case with
                    | SimpleCase(n, _) -> n, []
                    | DataCase(n, types, _) -> n, types |> List.map (resolveTypeAnnotation finalRegistry)
                let schemeArgs = tArgs
                let consScheme =
                    if resolvedArgs.IsEmpty then
                        Scheme(schemeArgs, [], parentType)
                    else
                        Scheme(schemeArgs, [], TFun(resolvedArgs, parentType))
                finalBindings <- Map.add caseName { Scheme = consScheme; IsMutable = false } finalBindings

    { env with Registry = finalRegistry; Bindings = finalBindings }

let rec checkDecl (env: Env) (sigs: Map<string, HMType * FType option * (string * string) list>) (decl: Decl) : Env * Map<string, HMType * FType option * (string * string) list> * TDecl list =
    match decl with
    | DSignature(name, ftype, constraints, _) -> env, Map.add name (resolveTypeAnnotation env.Registry ftype, Some ftype, constraints) sigs, []

    | DDef(name, expr, r) ->
        let exprType, typedExpr = infer env expr

        match Map.tryFind name sigs with
        | Some (sigType, _, _) -> unify env.Registry exprType sigType
        | None -> ()

        // Trait obligations are discharged before generalization: a scheme must
        // not be built over a constructor that resolution would still have
        // pinned down.
        solvePending env

        let newEnv =
            addBinding
                name
                { Scheme = generalize env exprType
                  IsMutable = false }
                env

        newEnv, Map.remove name sigs, [ TDef(name, typedExpr, exprType, r) ]

    | DDefun(name, defunArgs, body, r) ->
        // Enforce mandatory signature for all top-level defuns except 'main'
        let sigOpt = Map.tryFind name sigs
        if name <> "main" && sigOpt.IsNone then
            failwithf $"Type Error: Function '%s{name}' requires a type signature (: %s{name} ...) at %s{Lexer.formatPos r}"

        // Extract structured keyword/rest info from the raw FType (if available)
        let mandatoryFTypes, keywordFTypes, restFTypeOpt, retFType =
            match sigOpt with
            | Some (_, Some (TArrow(m, kw, rest, ret, _)), _) -> m, kw, rest, Some ret
            | _ -> [], [], None, None

        let sigHMType = sigOpt |> Option.map (fun (t, _, _) -> t)

        // Extract explicit trait constraints from the signature
        let explicitConstraints =
            match sigOpt with
            | Some (_, _, constraints) ->
                constraints |> List.map (fun (traitName, varName) ->
                    { TraitName = traitName; TargetType = TVar varName })
            | None -> []

        // Match defun args with the signature types
        let mandatoryArgNames =
            defunArgs |> List.choose (function MandatoryArg n -> Some n | _ -> None)
        let keywordArgDefs =
            defunArgs |> List.choose (function KeywordArg(n, defaultExpr) -> Some(n, defaultExpr) | _ -> None)
        let restArgName =
            defunArgs |> List.tryPick (function RestArg n -> Some n | _ -> None)

        // Resolve mandatory arg types from signature
        let mandatoryTypes =
            if mandatoryFTypes.Length > 0 then
                if mandatoryArgNames.Length <> mandatoryFTypes.Length then
                    failwithf $"Type Error: Function '%s{name}' has %d{mandatoryArgNames.Length} mandatory args but signature specifies %d{mandatoryFTypes.Length} at %s{Lexer.formatPos r}"
                List.zip mandatoryArgNames (mandatoryFTypes |> List.map (resolveTypeAnnotation env.Registry))
            else
                // For main or functions without TArrow signature, use fresh metas
                mandatoryArgNames |> List.map (fun n -> n, freshMeta())

        // Resolve keyword arg types from signature and type-check defaults
        let keywordTypes =
            keywordArgDefs |> List.map (fun (kwName, _defaultExpr) ->
                let kwType =
                    match keywordFTypes |> List.tryFind (fun (n, _) -> n = kwName) with
                    | Some (_, ft) -> resolveTypeAnnotation env.Registry ft
                    | None ->
                        if sigOpt.IsSome then
                            failwithf $"Type Error: Keyword argument '#:%s{kwName}' not found in signature for '%s{name}' at %s{Lexer.formatPos r}"
                        else freshMeta()
                kwName, kwType)

        // Resolve rest arg type from signature
        let restArgType =
            match restArgName, restFTypeOpt with
            | Some _, Some ft -> Some (resolveTypeAnnotation env.Registry ft)
            | Some _, None ->
                if sigOpt.IsSome then
                    failwithf $"Type Error: Function '%s{name}' has a rest arg but signature has no #:rest at %s{Lexer.formatPos r}"
                else Some (freshMeta())
            | None, _ -> None

        let expectedRetType =
            match retFType with
            | Some ft -> resolveTypeAnnotation env.Registry ft
            | None -> freshMeta()

        // Build the flat function type for unification
        let allArgTypes =
            (mandatoryTypes |> List.map snd) @
            (keywordTypes |> List.map snd) @
            (match restArgType with Some rt -> [TCon("Array", [rt])] | None -> [])
        let funType = TFun(allArgTypes, expectedRetType)

        match sigHMType with
        | Some st -> unify env.Registry funType st
        | None -> ()

        // Keyword/rest metadata has to exist *before* the body is inferred, or a
        // recursive call that passes a keyword argument, or omits an optional
        // one, has no metadata to resolve against: the keyword-application rule
        // would reject it and the flat `funType` would refuse to unify with the
        // shorter argument list.
        let funMeta = {
            MandatoryCount = mandatoryTypes.Length
            KeywordParams = keywordTypes
            RestParam = restArgType
        }

        let recEnv =
            let bound =
                addBinding
                    name
                    { Scheme = Scheme([], [], funType)
                      IsMutable = false }
                    env

            { bound with FunMetas = Map.add name funMeta bound.FunMetas }

        // Bind mandatory args
        let envWithMandatory =
            mandatoryTypes
            |> List.fold
                (fun acc (n, t) ->
                    addBinding n { Scheme = Scheme([], [], t); IsMutable = false } acc)
                recEnv

        // Bind keyword args
        let bodyEnv =
            keywordTypes
            |> List.fold
                (fun acc (n, t) ->
                    addBinding n { Scheme = Scheme([], [], t); IsMutable = false } acc)
                envWithMandatory

        // Bind rest arg as Array type
        let bodyEnv =
            match restArgName, restArgType with
            | Some rn, Some rt ->
                addBinding rn { Scheme = Scheme([], [], TCon("Array", [rt])); IsMutable = false } bodyEnv
            | _ -> bodyEnv

        let bodyType, typedBody = infer bodyEnv body
        unify env.Registry bodyType expectedRetType

        // Type-check keyword default expressions
        let typedKeywordArgs, _ =
            List.zip keywordArgDefs keywordTypes
            |> List.fold (fun (typedArgs, currentEnv) ((kwName, defaultExpr), (_, kwType)) ->
                let defaultType, typedDefault = infer currentEnv defaultExpr
                unify env.Registry defaultType kwType
                let nextEnv = addBinding kwName { Scheme = Scheme([], [], kwType); IsMutable = false } currentEnv
                (typedArgs @ [kwName, kwType, typedDefault], nextEnv)
            ) ([], envWithMandatory)

        solvePending env

        let scheme = generalize env funType
        let (Scheme(vars, _, schemeType)) = scheme

        // Collect trait constraints from the body and merge with explicit ones
        let inferredConstraints = collectTraitConstraints env typedBody
        let allConstraints =
            let seen = System.Collections.Generic.HashSet<string * string>()
            [ for c in explicitConstraints @ inferredConstraints do
                let key = (c.TraitName, match c.TargetType with TVar v -> v | _ -> "")
                if seen.Add(key) then yield c ]
        let schemeWithConstraints = Scheme(vars, allConstraints, schemeType)

        let finalEnv =
            addBinding
                name
                { Scheme = schemeWithConstraints
                  IsMutable = false }
                env
        let finalEnv = { finalEnv with FunMetas = Map.add name funMeta finalEnv.FunMetas }

        let restArgInfo =
            match restArgName, restArgType with
            | Some rn, Some rt -> Some(rn, rt)
            | _ -> None

        let decl = TDefun(name, vars, mandatoryTypes, typedKeywordArgs, restArgInfo, expectedRetType, typedBody, r)
        finalEnv, Map.remove name sigs, [ decl ]

    | DDefTuple(names, expr, r) ->
        let exprType, typedExpr = infer env expr
        let elementMetas = names |> List.map (fun _ -> freshMeta ())
        unify env.Registry exprType (TTuple elementMetas)
        solvePending env

        let newEnv =
            List.zip names elementMetas
            |> List.fold
                (fun acc (n, t) ->
                    addBinding
                        n
                        { Scheme = generalize env t
                          IsMutable = false }
                        acc)
                env

        newEnv, sigs, [ TDefTuple(names, typedExpr, exprType, r) ]

    | DDefMutable(name, expr, r) ->
        let exprType, typedExpr = infer env expr

        match Map.tryFind name sigs with
        | Some (sigType, _, _) -> unify env.Registry exprType sigType
        | None -> ()

        solvePending env

        let newEnv =
            addBinding
                name
                { Scheme = generalize env exprType
                  IsMutable = true }
                env

        newEnv, Map.remove name sigs, [ TDefMutable(name, typedExpr, exprType, r) ]

    | DModule(moduleName, decls, r) ->
        let finalEnv, finalSigs, typedDecls =
            checkDeclGroup { env with CurrentModule = moduleName } sigs decls

        { finalEnv with CurrentModule = env.CurrentModule }, finalSigs, [ TModule(moduleName, typedDecls, r) ]

    | DImport(paths, r) -> env, sigs, [ TImport(paths, r) ]
    | DExport(names, r) -> env, sigs, [ TExport(names, r) ]

    | DReExport(names, r) ->
        // A re-exported name was defined elsewhere and already carries a
        // signature from there, so the local-signature rule `export` enforces
        // cannot apply. What can be checked is that the name is actually in
        // scope here — otherwise the module would advertise something it does
        // not have.
        for name in names do
            if not (Map.containsKey name env.Bindings) then
                                    failwithf
                                        "Re-export Error: '%s' is not in scope at %s. A re-exported name must be imported by this module."
                                        name
                                        (Lexer.formatPos r)

        env, sigs, [ TReExport(names, r) ]
    | DType(typeDefs, r) -> registerTypeDefs false typeDefs env, sigs, [ TType(typeDefs, r) ]
    | DExtern(name, ftype, constraintPairs, r) ->
        let t = resolveTypeAnnotation env.Registry ftype
        let scheme = generalize env t
        let (Scheme(vars, _, schemeType)) = scheme
        // Add constraints from DLL metadata
        let constraints = 
            constraintPairs |> List.map (fun (traitName, varName) ->
                { TraitName = traitName; TargetType = TVar varName })
        let schemeWithConstraints = Scheme(vars, constraints, schemeType)
        let newEnv = { env with Bindings = Map.add name { Scheme = schemeWithConstraints; IsMutable = false } env.Bindings }

        // Keyword and rest metadata travels with an imported signature too.
        // Without it a call that passes a keyword argument, or omits an optional
        // one, has nothing to resolve against, and the flat function type
        // refuses to unify with the shorter argument list the caller wrote.
        let newEnv =
            match ftype with
            | TArrow(mandatory, keywords, restOpt, _, _) ->
                let funMeta =
                    { MandatoryCount = mandatory.Length
                      KeywordParams =
                        keywords |> List.map (fun (n, ft) -> n, resolveTypeAnnotation env.Registry ft)
                      RestParam = restOpt |> Option.map (resolveTypeAnnotation env.Registry) }

                { newEnv with FunMetas = Map.add name funMeta newEnv.FunMetas }
            | _ -> newEnv

        newEnv, sigs, [ TExtern(name, ftype, r) ]

    | DTrait(traitName, implementorVar, holeArity, assocTypes, signatures, r) ->
        // The kind is derived, not declared: an implementor written applied to
        // arguments cannot be an interface, because there is no C# interface
        // that abstracts over a type constructor.
        let kind = if holeArity > 0 then InlineTrait else InterfaceTrait

        let hmSignatures =
            match kind with
            | InterfaceTrait ->
                signatures
                |> List.map (fun (name, fType) -> name, resolveTypeAnnotation env.Registry fType)
                |> Map.ofList
            | InlineTrait -> Map.empty

        let templates =
            match kind with
            | InterfaceTrait -> Map.empty
            | InlineTrait ->
                signatures
                |> List.map (fun (name, fType) -> name, resolveTemplate env.Registry implementorVar fType)
                |> Map.ofList

        if kind = InlineTrait && not assocTypes.IsEmpty then
            failwithf
                $"Type Error at %s{Lexer.formatPos r}: trait '%s{traitName}' applies its implementor, so it is inline-only and cannot declare associated types. An inline trait's methods may be generic in their own right instead."

        let traitInfo =
            { ImplementorVar = implementorVar
              AssociatedTypes = assocTypes
              Signatures = hmSignatures
              Kind = kind
              HoleArity = holeArity
              Templates = templates }

        let newEnv = addTrait traitName traitInfo env

        // Whatever the kind, the method names are recorded so that `infer` can
        // recognize them in application position without searching every trait.
        let methodNames = signatures |> List.map fst

        // A method name identifies its trait, and that is the *only* thing that
        // can: nothing at a call site says which trait `pure` came from. Two
        // traits claiming one name is therefore not ambiguity to be resolved
        // later but a program with no meaning, and it has to be rejected here
        // rather than silently dispatched to whichever was registered last.
        for m in methodNames do
            match Map.tryFind m newEnv.Registry.TraitMethods with
            | Some owner when owner <> traitName ->
                failwithf
                    $"Type Error at %s{Lexer.formatPos r}: trait '%s{traitName}' declares a method '%s{m}', but '%s{owner}' already does. A call site says nothing about which trait a method name belongs to, so the two are indistinguishable. Rename one of them."
            | _ -> ()

        let newEnv =
            { newEnv with
                Registry =
                    { newEnv.Registry with
                        TraitMethods =
                            methodNames
                            |> List.fold (fun acc m -> Map.add m traitName acc) newEnv.Registry.TraitMethods } }

        let assocSubst = 
            assocTypes 
            |> List.map (fun assocName -> 
                "'" + assocName, TAssoc(traitName, assocName, TVar ("'" + implementorVar)))
            |> Map.ofList

        // An inline trait's methods are deliberately *not* bound into
        // `env.Bindings`. There is no single scheme they could be bound under —
        // `m` appears applied to two different arguments in `bind` — and a
        // weaker stand-in would be worse than nothing.
        let mutable finalEnv = newEnv

        if kind = InterfaceTrait then
            for kvp in hmSignatures do
                let methodTypeWithAssoc = applyTypeSubst assocSubst kvp.Value
                // Collect ALL free type variables from the method signature.
                // The implementor var is always first; any additional vars (like 'acc)
                // are method-level generics that must also be quantified.
                let methodVars = freeTVars env.Registry methodTypeWithAssoc |> List.distinct
                let implVar = "'" + implementorVar
                let allVars = implVar :: (methodVars |> List.filter ((<>) implVar))
                let scheme = Scheme(allVars, [], methodTypeWithAssoc)
                finalEnv <- addBinding kvp.Key { Scheme = scheme; IsMutable = false } finalEnv

        finalEnv, sigs, [ TTrait(traitName, implementorVar, kind, holeArity, assocTypes, hmSignatures, r) ]
    | DTypeRec(typeDefs, r) -> registerTypeDefs true typeDefs env, sigs, [ TTypeRec(typeDefs, r) ]
    | DImpl(traitName, targetTypeExpr, assocBindings, methods, r) ->
        let targetType = resolveTypeAnnotation env.Registry targetTypeExpr

        let typeKey =
            match targetType with
            | TCon(name, _) -> name
            | _ -> failwithf $"Trait implementations require concrete target types at %s{Lexer.formatPos r}"

        let isLocalTrait = env.Registry.IsTraitDefinedLocally(traitName)
        let isLocalType = env.Registry.IsTypeDefinedLocally(typeKey)

        if not (isLocalTrait || isLocalType) then
            failwithf
                $"Orphan Rule Violation at %s{Lexer.formatPos r}: Cannot implement foreign trait '%s{traitName}' for foreign type '%s{typeKey}'."

        let hmAssocBindings =
            assocBindings
            |> List.map (fun (name, fType) -> name, resolveTypeAnnotation env.Registry fType)

        let hmAssocBindingsMap = Map.ofList hmAssocBindings

        let traitInfo =
            match Map.tryFind traitName env.Registry.Traits with
            | Some info -> info
            | None -> failwithf $"Unknown trait '%s{traitName}' at %s{Lexer.formatPos r}"

        let implTarget = implTargetOf traitName traitInfo targetType r
        let regEnv = addImplementation traitName typeKey targetType implTarget hmAssocBindingsMap env

        // FIX 1: Prepend the "'" to the substitution keys so they match TVar "'c"
        let mutable substitutions = Map.add ("'" + traitInfo.ImplementorVar) targetType Map.empty

        for (k, v) in hmAssocBindings do
            substitutions <- Map.add ("'" + k) v substitutions

        let rec applySubst t =
            match prune regEnv.Registry t with
            | TVar name ->
                match Map.tryFind name substitutions with
                | Some concrete -> concrete
                | None -> t
            | TCon(n, args) -> TCon(n, args |> List.map applySubst)
            | TFun(args, ret) -> TFun(args |> List.map applySubst, applySubst ret)
            | TTuple args -> TTuple(args |> List.map applySubst)
            | _ -> t

        // 2. Typecheck methods and enforce signatures
        let typedMethods =
            methods
            |> List.map (fun methodDecl ->
                match methodDecl with
                | DDefun(name, args, body, methodRange) ->
                    // The definition-site check. Checking each body against the
                    // trait's own signature, instantiated at *this* impl, is what
                    // keeps errors out of the instantiation sites: an inline
                    // method that does not match its trait is rejected here,
                    // once, rather than at every place it is later spliced.
                    let expectedSignature =
                        match traitInfo.Kind with
                        | InlineTrait ->
                            match Map.tryFind name traitInfo.Templates with
                            | Some tpl -> instantiateTemplate implTarget tpl
                            | None ->
                                failwithf
                                    $"Method '%s{name}' is not a member of trait '%s{traitName}' at line %d{methodRange.Start.Line}"
                        | InterfaceTrait ->
                            match Map.tryFind name traitInfo.Signatures with
                            | Some sigType -> applySubst sigType
                            | None ->
                                failwithf
                                    $"Method '%s{name}' is not a member of trait '%s{traitName}' at line %d{methodRange.Start.Line}"

                    // After substituting the implementor var and associated types,
                    // the signature may still contain TVars from two sources:
                    //   1. Class-level type params (from targetType, e.g. 'a in List %a)
                    //      → These must stay as rigid TVars so they match the class params.
                    //   2. Method-level generics (like 'acc in fold's signature)
                    //      → These must be instantiated to fresh metas.
                    //
                    // An inline trait's class-level parameters are only the
                    // impl's *fixed prefix*: the arguments the constructor
                    // variable abstracts over belong to the method, and `bind`'s
                    // own `'b` is a method-level generic that has to reach C# as
                    // a generic method parameter.
                    let classLevelVars =
                        match traitInfo.Kind with
                        | InlineTrait -> implTarget.FixedPrefix |> List.collect typeVarsOf |> Set.ofList
                        | InterfaceTrait -> freeTVars regEnv.Registry targetType |> Set.ofList
                    let remainingVars = freeTVars regEnv.Registry expectedSignature |> List.distinct
                    let freshSubst =
                        remainingVars
                        |> List.filter (fun v -> not (Set.contains v classLevelVars))
                        |> List.map (fun v -> v, freshMeta())
                        |> Map.ofList
                    let instantiatedSig = applyTypeSubst freshSubst expectedSignature

                    // Pass instantiatedSig through 'sigs'.
                    // This forces DDefun to unify the expected types into the arguments
                    // BEFORE inference and generalization.
                    let methodSigs = Map.add name (instantiatedSig, None, []) Map.empty
                    
                    let _, _, tDecls = checkDecl regEnv methodSigs methodDecl
                    List.head tDecls // Return the fully verified TDefun node

                | _ -> failwithf $"Only 'defun' declarations are allowed inside 'def/impl' at %s{Lexer.formatPos r}")

        // Ensure all required methods from the trait are implemented
        let requiredMethods =
            match traitInfo.Kind with
            | InlineTrait -> traitInfo.Templates |> Map.toList |> List.map fst
            | InterfaceTrait -> traitInfo.Signatures |> Map.toList |> List.map fst

        for requiredMethod in requiredMethods do
            let isImplemented =
                methods
                |> List.exists (function
                    | DDefun(name, _, _, _) -> name = requiredMethod
                    | _ -> false)

            if not isImplemented then
                failwithf
                    "Implementation of trait '%s' is missing required method '%s' at line %d"
                    traitName requiredMethod r.Start.Line

        // Register every method as an inline template — interface traits
        // included. A statically resolvable call is inlined whatever the kind of
        // trait it belongs to; the difference is only that an interface trait
        // also keeps its dictionary path for the generic case.
        //
        // The body stored is the untyped one. Re-inferring it at the splice is
        // what lets it take a type the trait signature could not express, and a
        // typed AST is not serializable anyway: `HMType` is full of mutable
        // metavariable cells.
        let finalEnv =
            methods
            |> List.fold
                (fun acc methodDecl ->
                    match methodDecl with
                    | DDefun(name, defunArgs, body, _) ->
                        let paramNames =
                            defunArgs |> List.choose (function MandatoryArg n -> Some n | _ -> None)

                        // Keyword and rest parameters would have to survive the
                        // splice as a calling convention, which a spliced body
                        // has no call to carry. Such a method simply is not
                        // inlineable; the landing pad still is.
                        let inlineable =
                            defunArgs |> List.forall (function MandatoryArg _ -> true | _ -> false)

                        if inlineable then
                            addInlineTemplate
                                traitName
                                name
                                implTarget.Ctor
                                { Params = paramNames
                                  Body = body
                                  // Filled in after inference, where a
                                  // name-to-module map exists.
                                  Qualification = Map.empty
                                  OriginModule = acc.CurrentModule }
                                acc
                        else
                            acc
                    | _ -> acc)
                regEnv

        finalEnv,
        sigs,
        [ TImpl(traitName, traitInfo.Kind, traitInfo.HoleArity, targetType, hmAssocBindings, typedMethods, r) ]

    | DInlineImpl(traitName, methodName, ctor, originModule, parameters, body, qualification, r) ->
        // An inline template read back from a compiled module's metadata. Like
        // `DImplExtern` there is nothing to check and nothing to emit: the
        // landing pad is already compiled into the assembly that declared it,
        // and this is only the body to splice instead of calling it.
        let env =
            addInlineTemplate
                traitName
                methodName
                ctor
                { Params = parameters
                  Body = body
                  Qualification = Map.ofList qualification
                  OriginModule = originModule }
                env

        env, sigs, []

    | DImplExtern(traitName, targetTypeExpr, assocBindings, r) ->
        // A bodyless implementation, read back from a compiled module's
        // metadata. Only the registry needs to learn about it: the methods are
        // already compiled into the assembly that declared it, so there is
        // nothing to type-check and nothing to emit.
        let targetType = resolveTypeAnnotation env.Registry targetTypeExpr

        let typeKey =
            match targetType with
            | TCon(name, _) -> name
            | _ -> failwithf $"Trait implementations require concrete target types at %s{Lexer.formatPos r}"

        let traitInfo =
            match Map.tryFind traitName env.Registry.Traits with
            | Some info -> info
            | None ->
                failwithf $"Unknown trait '%s{traitName}' in imported implementation at %s{Lexer.formatPos r}"

        let hmAssocBindings =
            assocBindings
            |> List.map (fun (name, fType) -> name, resolveTypeAnnotation env.Registry fType)
            |> Map.ofList

        let implTarget = implTargetOf traitName traitInfo targetType r
        addImplementation traitName typeKey targetType implTarget hmAssocBindings env, sigs, []

/// Type-checks a group of declarations that share a signature scope: a module
/// body, or a whole program.
///
/// Signatures are collected up front so that declarations may refer to each
/// other out of order, which is why this cannot simply be a fold over
/// `checkDecl`. Signatures inherited from an enclosing group stay visible, with
/// the group's own taking precedence.
and private checkDeclGroup
    (env: Env)
    (sigs: Map<string, HMType * FType option * (string * string) list>)
    (decls: Decl list)
    : Env * Map<string, HMType * FType option * (string * string) list> * TDecl list =

    // 1. Pre-pass: collect all explicit signatures defined in this group
    let explicitSigs =
        decls
        |> List.collect (function
            | DSignature(name, ftype, constraints, _) -> 
                [name, (resolveTypeAnnotation env.Registry ftype, Some ftype, constraints)]
            // An inline trait's signatures are not `HMType`s and never can be:
            // they mention the constructor variable applied. They are read as
            // templates by `DTrait` instead, and there is nothing to inject here.
            | DTrait(_, _, holeArity, _, signatures, _) when holeArity = 0 ->
                signatures
                |> List.map (fun (name, ftype) ->
                    name, (resolveTypeAnnotation env.Registry ftype, Some ftype, []))
            | _ -> [])
        |> Map.ofList

    /// A trait method's signature comes from its `def/trait`, whichever kind of
    /// trait that is, so exporting one is never missing a signature.
    let traitMethodNames =
        decls
        |> List.collect (function
            | DTrait(_, _, _, _, signatures, _) -> signatures |> List.map fst
            | _ -> [])
        |> Set.ofList

    // 2. Validate exports against collected signatures
    decls
    |> List.iter (function
        | DExport(names, exprRange) ->
            for name in names do
                if not (Map.containsKey name explicitSigs || Set.contains name traitMethodNames) then
                    failwithf "Export Error: Exported item '%s' is missing a mandatory type signature at %s" name (Lexer.formatPos exprRange)
        | _ -> ())

    // 3. Inject the group's signatures into the environment for out-of-order inference
    let combinedSigs = Map.fold (fun acc k v -> Map.add k v acc) sigs explicitSigs

    // 3b. Bind every function the group declares *before* checking any of them,
    // so that a call may precede the definition it names — which is what makes
    // two top-level functions able to call each other.
    //
    // Only functions, and only ones with a signature. A `def` is an
    // initialization in order, so letting a later one be referenced early would
    // promise a value that does not exist yet; and a declared signature is what
    // makes the forward binding trustworthy — it is the type the definition is
    // then checked against, not a guess. Anything defined here is re-bound with
    // its inferred scheme as the fold reaches it.
    let declaredFunctions =
        decls
        |> List.choose (function
            | DDefun(name, _, _, _) -> Some name
            | _ -> None)
        |> Set.ofList

    let envWithForwardDecls =
        explicitSigs
        |> Map.fold
            (fun (acc: Env) name (hmType, ftypeOpt, constraintPairs) ->
                if not (Set.contains name declaredFunctions) then
                    acc
                else
                    let (Scheme(vars, _, schemeType)) = generalize acc hmType

                    let constraints =
                        constraintPairs
                        |> List.map (fun (traitName, varName) ->
                            { TraitName = traitName; TargetType = TVar varName })

                    let bound =
                        addBinding
                            name
                            { Scheme = Scheme(vars, constraints, schemeType)
                              IsMutable = false }
                            acc

                    // Keyword and rest parameters need their metadata just as
                    // early: a forward call that passes a keyword argument, or
                    // omits an optional one, has nothing to resolve against
                    // without it.
                    match ftypeOpt with
                    | Some(TArrow(mandatory, keywords, restOpt, _, _)) ->
                        let funMeta =
                            { MandatoryCount = mandatory.Length
                              KeywordParams =
                                keywords |> List.map (fun (n, ft) -> n, resolveTypeAnnotation acc.Registry ft)
                              RestParam = restOpt |> Option.map (resolveTypeAnnotation acc.Registry) }

                        { bound with FunMetas = Map.add name funMeta bound.FunMetas }
                    | _ -> bound)
            env

    // 4. Standard sequential typechecking pass
    let finalEnv, finalSigs, typedDecls =
        decls
        |> List.fold
            (fun (currEnv, currSigs, accDecls) d ->
                let nextEnv, nextSigs, tDecls = checkDecl currEnv currSigs d
                (nextEnv, nextSigs, tDecls @ accDecls))
            (envWithForwardDecls, combinedSigs, [])

    finalEnv, finalSigs, List.rev typedDecls

// --- PIPELINE COORDINATION ---
/// Runs Hindley-Milner inference over a parsed program.
///
/// The result still contains high-level `TMatch` nodes: pattern matching is
/// translated straight to C# patterns by the code generator, and trait dispatch
/// is resolved afterwards by `Bjolang.Lowering`.
///
/// `Pipeline.loadModuleGraph` runs every file through `wrapInModule`, so in
/// practice `program` is a list of `DModule`s and the real work happens one
/// level down. The declarations are still handed to `checkDeclGroup` directly
/// rather than assumed to be wrapped, so a bare list type-checks the same way.
let checkProgram (initialEnv: Env) (program: Decl list) : Env * TDecl list =
    let finalEnv, _, typedDecls = checkDeclGroup initialEnv Map.empty program
    // Anything raised outside a declaration that generalizes still has to be
    // answered for.
    solvePending finalEnv
    finalEnv, typedDecls
