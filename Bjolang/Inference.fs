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

    let traitOf methodName =
        registry.Traits
        |> Map.tryPick (fun traitName info ->
            if Map.containsKey methodName info.Signatures then Some traitName else None)

    let step (acc: Set<string * string>) (expr: TypedExpr) =
        match expr.Node with
        | TApply({ Node = TIdent(calleeName, tArgs); Type = calleeType }, _, _) ->
            match traitOf calleeName with
            | Some traitName ->
                match calleeType with
                | TFun(receiverType :: _, _) ->
                    match prune registry receiverType with
                    | TVar varName -> Set.add (traitName, varName) acc
                    | _ -> acc
                | _ -> acc
            | None ->
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

        let newEnv =
            addBinding
                name
                { Scheme = generalize env exprType
                  IsMutable = true }
                env

        newEnv, Map.remove name sigs, [ TDefMutable(name, typedExpr, exprType, r) ]

    | DModule(moduleName, decls, r) ->
        let finalEnv, finalSigs, typedDecls = checkDeclGroup env sigs decls
        finalEnv, finalSigs, [ TModule(moduleName, typedDecls, r) ]

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
        newEnv, sigs, [ TExtern(name, ftype, r) ]

    | DTrait(traitName, implementorVar, assocTypes, signatures, r) ->
        let hmSignatures =
            signatures
            |> List.map (fun (name, fType) -> name, resolveTypeAnnotation env.Registry fType)
            |> Map.ofList

        let traitInfo =
            { ImplementorVar = implementorVar
              AssociatedTypes = assocTypes
              Signatures = hmSignatures }

        let newEnv = addTrait traitName traitInfo env

        let assocSubst = 
            assocTypes 
            |> List.map (fun assocName -> 
                "'" + assocName, TAssoc(traitName, assocName, TVar ("'" + implementorVar)))
            |> Map.ofList

        let mutable finalEnv = newEnv
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

        // TDecl representation requires a TTrait node definition in your AST
        finalEnv, sigs, [ TTrait(traitName, implementorVar, assocTypes, hmSignatures, r) ]
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
        let regEnv = addImplementation traitName typeKey targetType hmAssocBindingsMap env
        let traitInfo = Map.find traitName regEnv.Registry.Traits

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
                    let expectedSignature =
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
                    let classLevelVars = freeTVars regEnv.Registry targetType |> Set.ofList
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
        for requiredMethod in traitInfo.Signatures.Keys do
            let isImplemented =
                methods
                |> List.exists (function
                    | DDefun(name, _, _, _) -> name = requiredMethod
                    | _ -> false)

            if not isImplemented then
                failwithf
                    "Implementation of trait '%s' is missing required method '%s' at line %d"
                    traitName requiredMethod r.Start.Line

        regEnv, sigs, [ TImpl(traitName, targetType, hmAssocBindings, typedMethods, r) ]

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

        if not (Map.containsKey traitName env.Registry.Traits) then
            failwithf
                $"Unknown trait '%s{traitName}' in imported implementation at %s{Lexer.formatPos r}"

        let hmAssocBindings =
            assocBindings
            |> List.map (fun (name, fType) -> name, resolveTypeAnnotation env.Registry fType)
            |> Map.ofList

        addImplementation traitName typeKey targetType hmAssocBindings env, sigs, []

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
            | DTrait(_, _, _, signatures, _) ->
                signatures
                |> List.map (fun (name, ftype) ->
                    name, (resolveTypeAnnotation env.Registry ftype, Some ftype, []))
            | _ -> [])
        |> Map.ofList

    // 2. Validate exports against collected signatures
    decls
    |> List.iter (function
        | DExport(names, exprRange) ->
            for name in names do
                if not (Map.containsKey name explicitSigs) then
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
    finalEnv, typedDecls
