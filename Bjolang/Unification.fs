module Bjolang.Unification

open Bjolang.TypedAST

// --- UNIFICATION ENGINE ---
let mutable nextMetaId = 0
let freshMeta () = 
    let id = nextMetaId
    nextMetaId <- nextMetaId + 1
    TMeta { Id = id; Value = None }

let lookup (env: Env) (name: string) : Binding =
    match Map.tryFind name env.Bindings with
    | Some scheme -> scheme
    | None -> failwithf $"Unbound variable: %s{name}"

let addBinding (name: string) (binding: Binding) (env: Env) : Env =
    { env with
        Bindings = Map.add name binding env.Bindings }

let rec prune (registry: TraitRegistry) (t: HMType) : HMType =
    match t with
    | TMeta m ->
        match m.Value with
        | Some innerT ->
            let pruned = prune registry innerT
            m.Value <- Some pruned
            pruned
        | None -> t
    | TCon(name, args) -> TCon(name, List.map (prune registry) args)
    | TFun(args, ret) -> TFun(List.map (prune registry) args, prune registry ret)
    | TTuple args -> TTuple(List.map (prune registry) args)
    | TAssoc(traitName, assocName, implementor) ->
        let prunedImpl = prune registry implementor

        match prunedImpl with
        // If the implementor is concrete, attempt resolution
        | TCon _
        | TTuple _
        | TFun _ ->
            match registry.ResolveAssociatedType traitName assocName prunedImpl with
            | Some resolved -> prune registry resolved
            | None -> failwithf $"Missing implementation of %s{traitName} for %A{prunedImpl}"
        // If still generic, keep deferred
        | _ -> TAssoc(traitName, assocName, prunedImpl)
    | _ -> t

let instantiate
    (registry: TraitRegistry)
    (Scheme(boundVars, constraints, t))
    : HMType * HMType list * TraitConstraint list =
    let boundVars = List.distinct boundVars

    let boundSubst =
        boundVars |> List.map (fun name -> name, freshMeta ()) |> Map.ofList

    // Positionally aligned with the scheme's own variable list, which is what a
    // caller needs to answer "what was `'c` instantiated to here?" — the
    // dictionary a trait constraint requires is chosen by that answer. Taking
    // them out of the map instead ordered them alphabetically, so `fold`'s
    // ['col; 'acc] came back as ['acc; 'col].
    let boundFreshTypes = boundVars |> List.map (fun name -> Map.find name boundSubst)

    let rec walk node =
        match prune registry node with
        | TVar name ->
            match Map.tryFind name boundSubst with
            | Some fresh -> fresh
            | None -> node
        | TFun(args, ret) -> TFun(List.map walk args, walk ret)
        | TCon(name, args) -> TCon(name, List.map walk args)
        | TTuple args -> TTuple(List.map walk args)
        | TAssoc(tName, aName, impl) -> TAssoc(tName, aName, walk impl)
        | _ -> node

    let instantiatedType = walk t

    let instantiatedConstraints =
        constraints
        |> List.map (fun c ->
            { c with
                TargetType = walk c.TargetType })

    instantiatedType, boundFreshTypes, instantiatedConstraints

let rec occurs (registry: TraitRegistry) (m: MetaVar) (t: HMType) : bool =
    match prune registry t with
    | TMeta m2 -> m.Id = m2.Id
    | TCon(_, args) -> List.exists (occurs registry m) args
    | TFun(args, ret) -> List.exists (occurs registry m) args || occurs registry m ret
    | TTuple args -> List.exists (occurs registry m) args
    | TAssoc(_, _, impl) -> occurs registry m impl
    | TVar _ -> false

let bindMeta (registry: TraitRegistry) (m: MetaVar) (t: HMType) =
    match t with
    | TMeta m2 when m.Id = m2.Id -> ()
    | _ ->
        if occurs registry m t then
            failwith "Type error: Infinite type (occurs check failed)"
        else
            m.Value <- Some t

/// Does this type still hide an associated-type projection whose implementor is
/// unknown? `prune` resolves a projection as soon as the implementor is
/// concrete, so what is left is a projection waiting on a meta variable —
/// something else in the same call has to pin it down first.
let rec private awaitsImplementor (registry: TraitRegistry) (t: HMType) : bool =
    match prune registry t with
    | TAssoc(_, _, impl) ->
        match prune registry impl with
        | TMeta _ -> true
        | _ -> false
    | TCon(_, args)
    | TTuple args -> List.exists (awaitsImplementor registry) args
    | TFun(args, ret) ->
        List.exists (awaitsImplementor registry) args
        || awaitsImplementor registry ret
    | _ -> false

let rec unify (registry: TraitRegistry) (t1: HMType) (t2: HMType) =
    let t1, t2 = prune registry t1, prune registry t2

    match t1, t2 with
    | _ when t1 = t2 -> ()
    | TMeta m, _ -> bindMeta registry m t2
    | _, TMeta m -> bindMeta registry m t1
    | TCon(name1, args1), TCon(name2, args2) when name1 = name2 && args1.Length = args2.Length ->
        List.iter2 (unify registry) args1 args2
    | TFun(args1, ret1), TFun(args2, ret2) when args1.Length = args2.Length ->
        // An argument whose type waits on an implementor is checked last. In
        // `(fold + 0 v)` the folding function's type mentions `%item`, which is
        // `Foldable`'s associated type: it cannot be compared against `+`'s
        // `int` until `v` has said which implementation is in play. Parameter
        // order should not decide whether a program type-checks.
        let ready, waiting =
            List.zip args1 args2
            |> List.partition (fun (a, b) -> not (awaitsImplementor registry a || awaitsImplementor registry b))

        for (a, b) in ready do
            unify registry a b

        for (a, b) in waiting do
            unify registry a b

        unify registry ret1 ret2
    | TTuple args1, TTuple args2 when args1.Length = args2.Length -> List.iter2 (unify registry) args1 args2
    | TAssoc(tn1, an1, impl1), TAssoc(tn2, an2, impl2) when tn1 = tn2 && an1 = an2 -> unify registry impl1 impl2
    | _ -> failwithf $"Type error: Cannot unify %A{t1} with %A{t2}"

let rec freeVars (registry: TraitRegistry) (t: HMType) : MetaVar list =
    match prune registry t with
    | TMeta m -> [ m ]
    | TCon(_, args) -> List.collect (freeVars registry) args
    | TFun(args, ret) -> (List.collect (freeVars registry) args) @ (freeVars registry ret)
    | TTuple args -> List.collect (freeVars registry) args
    | TAssoc(_, _, impl) -> freeVars registry impl
    | TVar _ -> []

let envFreeVars (env: Env) : Set<MetaVar> =
    env.Bindings
    |> Map.toList
    |> List.collect (fun (_, b) ->
        match b.Scheme with
        | Scheme(_, _, t) -> freeVars env.Registry t)
    |> Set.ofList

let rec freeTVars (registry: TraitRegistry) (t: HMType) : string list =
    match prune registry t with
    | TVar name -> [ name ]
    | TMeta _ -> []
    | TCon(_, args) -> List.collect (freeTVars registry) args
    | TFun(args, ret) -> (List.collect (freeTVars registry) args) @ (freeTVars registry ret)
    | TTuple args -> List.collect (freeTVars registry) args
    | TAssoc(_, _, impl) -> freeTVars registry impl

/// Metavariables that a deferred, *un-abstractable* obligation is still waiting
/// on.
///
/// Generalizing one of these would replace the very metavariable resolution is
/// watching with a rigid type variable, and the answer could then never arrive:
/// an inline trait has no dictionary, so there is nothing a quantified
/// constraint over it could mean. Such a binding stays monomorphic instead, and
/// its use site pins the constructor.
///
/// This is deliberately *not* applied to interface traits: quantifying one of
/// those and passing a dictionary is exactly how they are supposed to work.
///
/// A hook rather than a parameter, because `generalize` is called from a dozen
/// places that have no business knowing a constraint queue exists.
let mutable heldMetaIds: unit -> Set<int> = fun () -> Set.empty

let generalize (env: Env) (t: HMType) : Scheme =
    let envFv = envFreeVars env
    let tFv = freeVars env.Registry t |> List.distinct
    let held = heldMetaIds ()

    let generalizable =
        tFv
        |> List.filter (fun m -> not (Set.contains m envFv) && not (Set.contains m.Id held))
    
    // Find all explicitly named TVars that are already in the type
    let explicitTVars = freeTVars env.Registry t |> List.distinct
    
    // Generated names have to be unique across the *whole* program, not just
    // within this type. The code generator maps `'a` to `T_a`, so two
    // independent generalizations that both chose `'a` would produce a nested
    // `T_a` that shadows the enclosing one instead of referring to it — and a
    // value typed at the outer parameter is not assignable to the inner one.
    let generatedNames = generalizable |> List.map (fun _ -> Gensym.fresh "'t")
    
    List.iter2 (fun (m: MetaVar) name -> m.Value <- Some(TVar name)) generalizable generatedNames

    let allVars = (explicitTVars @ generatedNames) |> List.distinct

    // Default to empty constraints for now; gathering happens during inference
    Scheme(allVars, [], t)
