module Bjolang.TypeChecker

open Bjolang.Lexer
open Bjolang.Parser


// --- MUTABLE HM TYPES (For Inference) ---
[<CustomEquality; CustomComparison>]
type MetaVar = 
    { Id: int; mutable Value: HMType option }
    
    override this.Equals(obj) =
        match obj with
        | :? MetaVar as other -> this.Id = other.Id
        | _ -> false
        
    override this.GetHashCode() = this.Id
    
    interface System.IComparable with
        member this.CompareTo(obj) =
            match obj with
            | :? MetaVar as other -> compare this.Id other.Id
            | _ -> invalidArg "obj" "not a MetaVar"

and HMType =
    | TCon of string * HMType list
    | TFun of HMType list * HMType
    | TTuple of HMType list
    | TVar of string
    | TMeta of MetaVar
    // TraitName * AssociatedTypeName * Implementor
    | TAssoc of string * string * HMType



module TypeConstants =
    [<Literal>]
    let Int32Name = "System.Int32"
    [<Literal>]
    let StringName = "System.String"
    [<Literal>]
    let BooleanName = "System.Boolean"
    [<Literal>]
    let VoidName = "System.Void"
    [<Literal>]
    let ObjectName = "System.Object"

    [<Literal>]
    let ByteName = "System.Byte"
    [<Literal>]
    let Int16Name = "System.Int16"
    [<Literal>]
    let UInt16Name = "System.UInt16"
    [<Literal>]
    let UInt32Name = "System.UInt32"
    [<Literal>]
    let Int64Name = "System.Int64"
    [<Literal>]
    let UInt64Name = "System.UInt64"
    [<Literal>]
    let DoubleName = "System.Double"

    let intType = TCon(Int32Name, [])
    let stringType = TCon(StringName, [])
    let boolType = TCon(BooleanName, [])
    let voidType = TCon(VoidName, [])
    let objType = TCon(ObjectName, [])
    
    let byteType = TCon(ByteName, [])
    let shortType = TCon(Int16Name, [])
    let ushortType = TCon(UInt16Name, [])
    let uintType = TCon(UInt32Name, [])
    let longType = TCon(Int64Name, [])
    let ulongType = TCon(UInt64Name, [])
    let doubleType = TCon(DoubleName, [])

type TypedExpr =
    { Type: HMType
      Range: Range
      Node: TExprNode }

and TypedPattern =
    { Type: HMType
      Range: Range
      Node: TPatternNode }

and TPatternNode =
    | TPWildcard
    | TPInt of string
    | TPString of string
    | TPIdent of string
    | TPList of TypedPattern list * TypedPattern option
    | TPConstruct of string * TypedPattern list
    | TPApp of TypedExpr * TypedPattern
    | TPAs of TypedPattern * string

and TExprNode =
    | TInt of string
    | TString of string
    | TIdent of string * HMType list
    | TKeyword of string
    | TSymbol of string
    | TLet of string * bool * string list * TypedExpr * TypedExpr
    | TLetRec of (string * bool * string list * TypedExpr) list * TypedExpr
    | TLetTuple of string list * TypedExpr * TypedExpr
    | TLambda of string list * TypedExpr
    | TApply of TypedExpr * TypedExpr list * (string * TypedExpr) list * bool
    | TTupleMake of TypedExpr list
    | TListMake of TypedExpr list
    | TVecMake of TypedExpr list
    | TRecordMake of (string * TypedExpr) list
    | TRecordUpdate of string * (string * TypedExpr) list
    | TLetMutable of string * TypedExpr * TypedExpr
    | TSet of string * TypedExpr
    | TIf of TypedExpr * TypedExpr * TypedExpr
    | TTryFinally of TypedExpr * TypedExpr
    | TMatch of TypedExpr * TMatchClause list
    | TInterfaceCall of HMType * string * TypedExpr * TypedExpr list
    | TThrow of TypedExpr
    // Lowered
    | TIsInst of TypedExpr * HMType
    | TIsInstCase of TypedExpr * HMType * string
    | TCast of TypedExpr * HMType
    | TCaseCast of TypedExpr * HMType * string
    | TGetField of TypedExpr * string
    | TTypeEq of TypedExpr * TypedExpr

and TMatchClause =
    { Pattern: TypedPattern
      Guard: TypedExpr option
      Body: TypedExpr }

type TDecl =
    | TImport of ImportSpec list * Range
    | TExport of string list * Range
    | TModule of string * TDecl list * Range
    | TDef of string * TypedExpr * HMType * Range
    | TDefTuple of string list * TypedExpr * HMType * Range
    | TDefMutable of string * TypedExpr * HMType * Range
    | TDefun of string * string list * (string * HMType) list * (string * HMType * TypedExpr) list * (string * HMType) option * HMType * TypedExpr * Range
    //          name     tyArgs          mandatoryArgs           keywordArgs(name,type,default)      restArg(name,elemType)       retType  body       range
    | TType of TypeDef list * Range
    | TTypeRec of TypeDef list * Range
    | TTrait of string * string * string list * Map<string, HMType> * Range
    | TImpl of string * HMType * (string * HMType) list * TDecl list * Range
    | TExtern of string * FType * Range

type FunMeta = {
    MandatoryCount: int
    KeywordParams: (string * HMType) list   // keyword name, type
    RestParam: HMType option                // element type of rest array
}

type TraitConstraint =
    { TraitName: string
      TargetType: HMType }

type Scheme = Scheme of string list * TraitConstraint list * HMType

type Binding = { Scheme: Scheme; IsMutable: bool }

// Metadata resolution callbacks
type TraitInfo =
    { ImplementorVar: string
      AssociatedTypes: string list
      Signatures: Map<string, HMType> }

type TraitRegistry =
    { LocalTraits: Set<string>
      LocalTypes: Set<string>
      Traits: Map<string, TraitInfo>
      // Maps (TraitName * TargetTypeIdentifier) -> Map<AssociatedTypeName, HMType>
      Implementations: Map<string * string, Map<string, HMType>>
      Aliases: Map<string, string list * HMType>
      Records: Map<string, string list * (string * HMType) list>
      RecordFields: Map<string, string> }

    member this.IsTraitDefinedLocally(name) = Set.contains name this.LocalTraits
    member this.IsTypeDefinedLocally(name) = Set.contains name this.LocalTypes

    member this.ResolveAssociatedType (traitName: string) (assocName: string) (implType: HMType) : HMType option =
        // Extract a string key for the concrete type (e.g., "System.Int32" or "Vec")
        let typeKey =
            match implType with
            | TCon(name, _) -> Some name
            | _ -> None

        match typeKey with
        | Some tk ->
            this.Implementations
            |> Map.tryFind (traitName, tk)
            |> Option.bind (Map.tryFind assocName)
        | None -> None

type Env =
    { Bindings: Map<string, Binding>
      Registry: TraitRegistry
      FunMetas: Map<string, FunMeta> }


let addTrait (name: string) (info: TraitInfo) (env: Env) : Env =
    let newRegistry =
        { env.Registry with
            LocalTraits = Set.add name env.Registry.LocalTraits
            Traits = Map.add name info env.Registry.Traits }

    { env with Registry = newRegistry }

let addImplementation (traitName: string) (typeKey: string) (assocBindings: Map<string, HMType>) (env: Env) : Env =
    let newRegistry =
        { env.Registry with
            Implementations = Map.add (traitName, typeKey) assocBindings env.Registry.Implementations }

    { env with Registry = newRegistry }


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
    let boundSubst =
        boundVars |> List.map (fun name -> name, freshMeta ()) |> Map.ofList

    let boundFreshTypes = boundSubst |> Map.toList |> List.map snd
    let mutable unboundSubst = Map.empty
    let mutable unboundFreshTypes = []

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

    instantiatedType, boundFreshTypes @ unboundFreshTypes, instantiatedConstraints

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

let rec unify (registry: TraitRegistry) (t1: HMType) (t2: HMType) =
    let t1, t2 = prune registry t1, prune registry t2

    match t1, t2 with
    | _ when t1 = t2 -> ()
    | TMeta m, _ -> bindMeta registry m t2
    | _, TMeta m -> bindMeta registry m t1
    | TCon(name1, args1), TCon(name2, args2) when name1 = name2 && args1.Length = args2.Length ->
        List.iter2 (unify registry) args1 args2
    | TFun(args1, ret1), TFun(args2, ret2) when args1.Length = args2.Length ->
        List.iter2 (unify registry) args1 args2
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

let generalize (env: Env) (t: HMType) : Scheme =
    let envFv = envFreeVars env
    let tFv = freeVars env.Registry t |> List.distinct
    let generalizable = tFv |> List.filter (fun m -> not (Set.contains m envFv))
    
    // Find all explicitly named TVars that are already in the type
    let explicitTVars = freeTVars env.Registry t |> List.distinct
    
    // Generate new names for the generalizable MetaVars, avoiding existing ones
    // We'll just append them.
    let generatedNames = generalizable |> List.mapi (fun i _ -> "'" + string (char (97 + i)))
    
    List.iter2 (fun (m: MetaVar) name -> m.Value <- Some(TVar name)) generalizable generatedNames

    let allVars = (explicitTVars @ generatedNames) |> List.distinct

    // Default to empty constraints for now; gathering happens during inference
    Scheme(allVars, [], t)

/// Walk a typed expression body to find all trait method calls on type variables.
/// Returns a list of TraitConstraints (TraitName, TargetType as TVar).
let collectTraitConstraints (registry: TraitRegistry) (body: TypedExpr) : TraitConstraint list =
    let constraints = System.Collections.Generic.HashSet<string * string>()
    let rec walk (expr: TypedExpr) =
        match expr.Node with
        | TApply({ Node = TIdent(methodName, _); Type = TFun(argTypes, _) }, args, _, _) ->
            // Check if this is a trait method call
            let traitMethodOpt =
                registry.Traits
                |> Map.tryPick (fun traitName info ->
                    if Map.containsKey methodName info.Signatures then
                        Some(traitName, info)
                    else
                        None)
            match traitMethodOpt with
            | Some(traitName, _) when not argTypes.IsEmpty ->
                let receiverType = prune registry argTypes[0]
                match receiverType with
                | TVar varName ->
                    constraints.Add(traitName, varName) |> ignore
                | _ -> ()
            | _ -> ()
            // Recurse into sub-expressions
            args |> List.iter walk
        | TApply(target, args, _, _) ->
            walk target; args |> List.iter walk
        | TLet(_, _, _, value, body) ->
            walk value; walk body
        | TLetRec(bindings, body) ->
            bindings |> List.iter (fun (_, _, _, e) -> walk e); walk body
        | TLetTuple(_, value, body) ->
            walk value; walk body
        | TLambda(_, body) -> walk body
        | TIf(c, t, f) -> walk c; walk t; walk f
        | TTupleMake items | TListMake items | TVecMake items -> items |> List.iter walk
        | TRecordMake fields -> fields |> List.iter (snd >> walk)
        | TRecordUpdate(_, fields) -> fields |> List.iter (snd >> walk)
        | TLetMutable(_, value, body) -> walk value; walk body
        | TSet(_, value) -> walk value
        | TMatch(target, clauses) ->
            walk target
            clauses |> List.iter (fun c ->
                Option.iter walk c.Guard
                walk c.Body)
        | TInterfaceCall(_, _, dict, args) -> walk dict; args |> List.iter walk
        | TTryFinally(body, cleanup) -> walk body; walk cleanup
        | TThrow e -> walk e
        | TIsInst(tgt, _) | TIsInstCase(tgt, _, _) | TCast(tgt, _) | TCaseCast(tgt, _, _) -> walk tgt
        | TGetField(tgt, _) -> walk tgt
        | TTypeEq(t1, t2) -> walk t1; walk t2
        | _ -> () // TInt, TString, TIdent, TKeyword, TSymbol — no sub-expressions
    walk body
    constraints
    |> Seq.toList
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
                env

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
              Node = TApply(typedTarget, positionalTypedArgs, typedKwArgs, false) }

        | _ ->
            // No FunMeta or no keyword args: simple positional call
            if not keywordArgs.IsEmpty then
                failwithf $"Keyword arguments used on a function without keyword parameter metadata at line %d{r.Start.Line}"

            unify env.Registry targetType (TFun(positionalArgs |> List.map fst, retType))

            retType,
            { Type = retType
              Range = r
              Node = TApply(typedTarget, positionalArgs |> List.map snd, [], false) }

    | ELet(name, isFun, args, value, body, r) ->
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
        let bindingMetas = bindings |> List.map (fun (n, _, _, _) -> n, freshMeta ())

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
            |> List.mapi (fun i (name, isFun, args, expr) ->
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

    | ELetMutable(name, value, body, r) ->
        let valType, typedVal = infer env value

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

                printfn "EMatch Clause: unifying bodyType=%A with returnType=%A" bodyType returnType
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

        let tArgs, expectedFields = Map.find recordTypeName env.Registry.Records
        let tArgsInst = tArgs |> List.map (fun a -> a.TrimStart('\''))
        let recordScheme = Scheme(tArgsInst, [], TCon(recordTypeName, tArgsInst |> List.map TVar))
        
        // Instantiate the record type and its fields
        let instantiatedRecordType, freshVars, _ = instantiate env.Registry recordScheme
        let fieldSubst = List.zip tArgsInst freshVars |> Map.ofList
        let expectedFieldsInstantiated = 
            expectedFields |> List.map (fun (n, t) -> n, applyTypeSubst fieldSubst t) |> Map.ofList

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

        let tArgs, expectedFields = Map.find recordTypeName env.Registry.Records
        let tArgsInst = tArgs |> List.map (fun a -> a.TrimStart('\''))
        let recordScheme = Scheme(tArgsInst, [], TCon(recordTypeName, tArgsInst |> List.map TVar))

        let instantiatedRecordType, freshVars, _ = instantiate env.Registry recordScheme
        let fieldSubst = List.zip tArgsInst freshVars |> Map.ofList
        let expectedFieldsInstantiated = 
            expectedFields |> List.map (fun (n, t) -> n, applyTypeSubst fieldSubst t) |> Map.ofList

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
            
        let tArgs, expectedFields = Map.find recordTypeName env.Registry.Records
        let tArgsInst = tArgs |> List.map (fun a -> a.TrimStart('\''))
        let recordScheme = Scheme(tArgsInst, [], TCon(recordTypeName, tArgsInst |> List.map TVar))
        
        let instantiatedRecordType, freshVars, _ = instantiate env.Registry recordScheme
        let fieldSubst = List.zip tArgsInst freshVars |> Map.ofList
        let expectedFieldsInstantiated = 
            expectedFields |> List.map (fun (n, t) -> n, applyTypeSubst fieldSubst t) |> Map.ofList

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


module MatchCompiler =
    let private freshVarCounter = ref 0

    let private freshLocalName () =
        freshVarCounter.Value <- freshVarCounter.Value + 1
        $"_match_target_%d{freshVarCounter.Value}"

    /// Generates code asserting that a pattern matches, tracking localized variables
    let rec private compilePattern
        (traits: TraitRegistry)
        (target: TypedExpr)
        (pat: TypedPattern)
        (cont: TypedExpr -> TypedExpr)
        (failExpr: TypedExpr)
        : TypedExpr =
        match pat.Node with
        | TPWildcard -> cont failExpr
        | TPIdent name ->
            let body = cont failExpr

            let checkedTarget = target

            // Variable bindings in matches are never functions, so isFun = false, args = []
            ({ Type = body.Type
               Range = pat.Range
               Node = TLet(name, false, [], checkedTarget, body) }
            : TypedExpr)

        | TPInt value ->
            let litExpr =
                ({ Type = TypeConstants.intType
                   Range = pat.Range
                   Node = TInt value }
                : TypedExpr)

            let cond =
                ({ Type = TypeConstants.boolType
                   Range = pat.Range
                   Node = TTypeEq(target, litExpr) }
                : TypedExpr)

            ({ Type = failExpr.Type
               Range = pat.Range
               Node = TIf(cond, cont failExpr, failExpr) }
            : TypedExpr)

        | TPString value ->
            let litExpr =
                ({ Type = TypeConstants.stringType
                   Range = pat.Range
                   Node = TString value }
                : TypedExpr)

            let cond =
                ({ Type = TypeConstants.boolType
                   Range = pat.Range
                   Node = TTypeEq(target, litExpr) }
                : TypedExpr)

            ({ Type = failExpr.Type
               Range = pat.Range
               Node = TIf(cond, cont failExpr, failExpr) }
            : TypedExpr)

        | TPConstruct(name, subPats) ->
            let isConsCond =
                ({ Type = TypeConstants.boolType
                   Range = pat.Range
                   Node = TIsInstCase(target, target.Type, name) }
                : TypedExpr)

            let compileSubPats actualFail =
                let castedTarget =
                    ({ Type = target.Type
                       Range = pat.Range
                       Node = TCaseCast(target, target.Type, name) }
                    : TypedExpr)

                let rec buildSubPats idx remainingPats cont =
                    match remainingPats with
                    | [] -> cont actualFail
                    | p :: ps ->
                        let fieldExpr =
                            ({ Type = p.Type
                               Range = pat.Range
                               Node = TGetField(castedTarget, $"Item%d{idx}") }
                            : TypedExpr)
                        compilePattern traits fieldExpr p (fun f1 -> buildSubPats (idx + 1) ps cont) actualFail

                buildSubPats 1 subPats (fun _ -> cont actualFail)

            ({ Type = failExpr.Type
               Range = pat.Range
               Node = TIf(isConsCond, compileSubPats failExpr, failExpr) }
            : TypedExpr)

        | TPList(items, tailOpt) ->
            let rec desugarListToConstruct elements : TypedPattern =
                match elements with
                | [] ->
                    match tailOpt with
                    | Some t -> t
                    | None ->
                        ({ Type = pat.Type
                           Range = pat.Range
                           Node = TPConstruct("Nil", []) }
                        : TypedPattern)
                | head :: tail ->
                    let tailDesugared = desugarListToConstruct tail

                    ({ Type = pat.Type
                       Range = pat.Range
                       Node = TPConstruct("Cons", [ head; tailDesugared ]) }
                    : TypedPattern)

            compilePattern traits target (desugarListToConstruct items) cont failExpr

        | _ -> failwithf $"Pattern structure not supported yet by lowering pass: %A{pat.Node}"

    let rec private compileClauses
        (traits: TraitRegistry)
        (target: TypedExpr)
        (clauses: TMatchClause list)
        (failExpr: TypedExpr)
        : TypedExpr =
        match clauses with
        | [] -> failExpr
        | clause :: rest ->
            let nextFallback = compileClauses traits target rest failExpr

            compilePattern
                traits
                target
                clause.Pattern
                (fun structuralFail ->
                    match clause.Guard with
                    | Some guard ->
                        ({ Type = clause.Body.Type
                           Range = clause.Body.Range
                           Node = TIf(guard, clause.Body, structuralFail) }
                        : TypedExpr)
                    | None -> clause.Body)
                nextFallback

    /// Lowers structural high-level TMatch expressions into nested conditional branches
    let rec lowerMatchExpressions (env: Env) (expr: TypedExpr) : TypedExpr =
        match expr.Node with
        | TMatch(target, clauses) ->
            let loweredTarget = lowerMatchExpressions env target

            let loweredClauses =
                clauses
                |> List.map (fun c ->
                    { c with
                        Guard = Option.map (lowerMatchExpressions env) c.Guard
                        Body = lowerMatchExpressions env c.Body })

            let panicMsg = $"Match failure occurred at line %d{expr.Range.Start.Line}"

            let failNode =
                ({ Type = expr.Type
                   Range = expr.Range
                   Node =
                       TThrow({ Type = TypeConstants.stringType
                                Range = expr.Range
                                Node = TString panicMsg } : TypedExpr) }
                : TypedExpr)

            let tempVar = freshLocalName ()

            let tempIdent =
                ({ Type = loweredTarget.Type
                   Range = expr.Range
                   Node = TIdent(tempVar, []) }
                : TypedExpr)

            let matchDecisionTree =
                compileClauses env.Registry tempIdent loweredClauses failNode

            ({ Type = expr.Type
               Range = expr.Range
               Node = TLet(tempVar, false, [], loweredTarget, matchDecisionTree) }
            : TypedExpr)

        // Updated Let matches
        | TLet(name, isFun, args, value, body) ->
            { expr with
                Node = TLet(name, isFun, args, lowerMatchExpressions env value, lowerMatchExpressions env body) }
        | TLetRec(bindings, body) ->
            let lowBindings =
                bindings
                |> List.map (fun (n, isF, a, e) -> n, isF, a, lowerMatchExpressions env e)

            { expr with
                Node = TLetRec(lowBindings, lowerMatchExpressions env body) }

        | TLambda(args, body) ->
            { expr with
                Node = TLambda(args, lowerMatchExpressions env body) }
        | TApply(target, args, kwArgs, isTail) ->
            { expr with
                Node = TApply(lowerMatchExpressions env target, args |> List.map (lowerMatchExpressions env), kwArgs |> List.map (fun (n, e) -> n, lowerMatchExpressions env e), isTail) }
        | TIf(c, t, f) ->
            { expr with
                Node = TIf(lowerMatchExpressions env c, lowerMatchExpressions env t, lowerMatchExpressions env f) }
        | TTupleMake items ->
            { expr with
                Node = TTupleMake(items |> List.map (lowerMatchExpressions env)) }
        | TListMake items ->
            { expr with
                Node = TListMake(items |> List.map (lowerMatchExpressions env)) }
        | TVecMake items ->
            { expr with
                Node = TVecMake(items |> List.map (lowerMatchExpressions env)) }
        | TLetMutable(name, value, body) ->
            { expr with
                Node = TLetMutable(name, lowerMatchExpressions env value, lowerMatchExpressions env body) }
        | TSet(name, value) ->
            { expr with
                Node = TSet(name, lowerMatchExpressions env value) }
        | _ -> expr

    let rec lowerDeclMatches (env: Env) (decl: TDecl) : TDecl =
        match decl with
        | TDef(name, value, t, r) -> TDef(name, lowerMatchExpressions env value, t, r)
        | TDefun(name, typeParams, args, kwArgs, restArg, retT, body, r) ->
            TDefun(name, typeParams, args, kwArgs |> List.map (fun (n, t, e) -> n, t, lowerMatchExpressions env e), restArg, retT, lowerMatchExpressions env body, r)
        | TModule(name, decls, r) -> TModule(name, decls |> List.map (lowerDeclMatches env), r)
        | _ -> decl

module DictionaryLowering =

    let rec lowerExpr (env: Env) (activeDicts: Map<string, string>) (expr: TypedExpr) : TypedExpr =
        let recurse e = lowerExpr env activeDicts e

        let node =
            match expr.Node with
            // Base cases (no sub-expressions)
            | TInt _
            | TString _
            | TIdent _
            | TKeyword _
            | TSymbol _ as leaf -> leaf

            // Target trait method invocations
            | TApply({ Node = TIdent(methodName, _)
                       Type = TFun(argTypes, _) } as target,
                     args, kwArgs,
                     isTail) ->
                let traitMethodOpt =
                    env.Registry.Traits
                    |> Map.tryPick (fun traitName info ->
                        if Map.containsKey methodName info.Signatures then
                            Some(traitName, info)
                        else
                            None)

                match traitMethodOpt with
                | Some(traitName, _) ->
                    let targetObj = args.Head
                    let loweredArgs = args |> List.map recurse
                    let receiverType = prune env.Registry argTypes[0]

                    match prune env.Registry targetObj.Type with
                    | TCon(targetTypeName, _) ->
                        // STATIC DISPATCH: Direct devirtualization
                        let targetTypeSanitized = targetTypeName.Replace(".", "_")
                        let implClassName = $"%s{traitName}_%s{targetTypeSanitized}"

                        let staticDirectTarget =
                            { target with
                                Node = TIdent( $"%s{implClassName}::%s{methodName}", []) }

                        TApply(staticDirectTarget, loweredArgs, [], isTail)

                    | TVar varName ->
                        // GENERIC DISPATCH
                        let expectedDictName = $"_dict_%s{traitName}_%s{varName}"

                        if not (Map.containsKey expectedDictName activeDicts) then
                            failwithf
                                $"Missing dictionary '%s{expectedDictName}' for trait dispatch at line %d{expr.Range.Start.Line}"

                        let dictIdent =
                            { Type = TCon(traitName, [ receiverType ])
                              Range = expr.Range
                              Node = TIdent(expectedDictName, []) }
                            : TypedExpr

                        TInterfaceCall(dictIdent.Type, methodName, dictIdent, loweredArgs)

                    | _ -> failwithf $"Unsupported receiver type for trait dispatch at line %d{expr.Range.Start.Line}"

                | None ->
                    // Check if the callee has trait constraints that need dictionary arguments
                    match target.Node with
                    | TIdent(calleeName, tArgs) ->
                        match Map.tryFind calleeName env.Bindings with
                        | Some binding ->
                            let (Scheme(schemeVars, constraints, _)) = binding.Scheme
                            if not constraints.IsEmpty && not tArgs.IsEmpty then
                                // Build a substitution from scheme vars to instantiated types
                                let varSubst =
                                    List.zip schemeVars (tArgs |> List.map (prune env.Registry))
                                    |> Map.ofList
                                // Build dictionary arguments for each constraint
                                let dictArgs =
                                    constraints |> List.map (fun c ->
                                        let resolvedType =
                                            match c.TargetType with
                                            | TVar varName ->
                                                match Map.tryFind varName varSubst with
                                                | Some t -> prune env.Registry t
                                                | None -> c.TargetType
                                            | _ -> prune env.Registry c.TargetType
                                        match resolvedType with
                                        | TCon(typeName, _) ->
                                            // Static dispatch: pass the singleton Instance
                                            let sanitizedTypeName = typeName.Replace(".", "_")
                                            let instanceName = $"%s{c.TraitName}_%s{sanitizedTypeName}::Instance"
                                            { Type = TCon(c.TraitName, [ resolvedType ])
                                              Range = expr.Range
                                              Node = TIdent(instanceName, []) } : TypedExpr
                                        | TVar varName ->
                                            // Forward the dictionary from our own parameters
                                            let expectedDictName = $"_dict_%s{c.TraitName}_%s{varName}"
                                            if not (Map.containsKey expectedDictName activeDicts) then
                                                failwithf
                                                    $"Missing dictionary '%s{expectedDictName}' to forward for call to '%s{calleeName}' at line %d{expr.Range.Start.Line}"
                                            { Type = TCon(c.TraitName, [ resolvedType ])
                                              Range = expr.Range
                                              Node = TIdent(expectedDictName, []) } : TypedExpr
                                        | _ ->
                                            failwithf $"Cannot resolve dictionary for type %A{resolvedType} at line %d{expr.Range.Start.Line}")
                                TApply(recurse target, dictArgs @ (args |> List.map recurse), kwArgs |> List.map (fun (n, e) -> n, recurse e), false)
                            else
                                // No constraints or no type args — standard call
                                TApply(recurse target, args |> List.map recurse, kwArgs |> List.map (fun (n, e) -> n, recurse e), false)
                        | None ->
                            // Unknown callee — standard call
                            TApply(recurse target, args |> List.map recurse, kwArgs |> List.map (fun (n, e) -> n, recurse e), false)
                    | _ ->
                        // Non-identifier target — standard call
                        TApply(recurse target, args |> List.map recurse, kwArgs |> List.map (fun (n, e) -> n, recurse e), false)

            // Explicit TInterfaceCall (if re-running the pass or generated elsewhere)
            | TInterfaceCall(iType, mName, dict, args) ->
                TInterfaceCall(iType, mName, recurse dict, args |> List.map recurse)

            // Standard structural recursion
            | TLet(name, isFun, args, value, body) -> TLet(name, isFun, args, recurse value, recurse body)

            | TLetRec(bindings, body) ->
                let lowBindings = bindings |> List.map (fun (n, isF, a, e) -> n, isF, a, recurse e)
                TLetRec(lowBindings, recurse body)

            | TLetTuple(names, value, body) -> TLetTuple(names, recurse value, recurse body)

            | TLambda(args, body) -> TLambda(args, recurse body)

            | TApply(target, args, kwArgs, isTail) -> // Fallback for non-identifier targets
                TApply(recurse target, args |> List.map recurse, kwArgs |> List.map (fun (n, e) -> n, recurse e), isTail)

            | TTupleMake items -> TTupleMake(items |> List.map recurse)

            | TListMake items -> TListMake(items |> List.map recurse)

            | TVecMake items -> TVecMake(items |> List.map recurse)

            | TRecordMake fields -> TRecordMake(fields |> List.map (fun (k, v) -> k, recurse v))

            | TRecordUpdate(name, fields) -> TRecordUpdate(name, fields |> List.map (fun (k, v) -> k, recurse v))

            | TLetMutable(name, value, body) -> TLetMutable(name, recurse value, recurse body)

            | TSet(name, value) -> TSet(name, recurse value)

            | TIf(cond, t, f) -> TIf(recurse cond, recurse t, recurse f)

            | TTryFinally(body, cleanup) -> TTryFinally(recurse body, recurse cleanup)

            | TMatch(target, clauses) ->
                let lowClauses =
                    clauses
                    |> List.map (fun c ->
                        { c with
                            Guard = Option.map recurse c.Guard
                            Body = recurse c.Body })

                TMatch(recurse target, lowClauses)

            | TIsInst(tgt, t) -> TIsInst(recurse tgt, t)
            | TIsInstCase(tgt, t, caseName) -> TIsInstCase(recurse tgt, t, caseName)
            | TCast(tgt, t) -> TCast(recurse tgt, t)
            | TCaseCast(tgt, t, caseName) -> TCaseCast(recurse tgt, t, caseName)

            | TGetField(tgt, n) -> TGetField(recurse tgt, n)

            | TTypeEq(t1, t2) -> TTypeEq(recurse t1, recurse t2)

            | TThrow e -> TThrow(recurse e)

        { expr with Node = node }

    let rec lowerDecl (env: Env) (decl: TDecl) : TDecl =
        match decl with
        | TDef(name, value, t, r) -> TDef(name, lowerExpr env Map.empty value, t, r)

        | TDefTuple(names, value, t, r) -> TDefTuple(names, lowerExpr env Map.empty value, t, r)

        | TDefMutable(name, value, t, r) -> TDefMutable(name, lowerExpr env Map.empty value, t, r)

        | TDefun(name, tyArgs, args, kwArgs, restArg, retType, body, r) ->
            let binding = lookup env name

            match binding.Scheme with
            | Scheme(_, constraints, _) ->
                // Inject dictionary parameters into generic functions at the declaration level
                let dictParams =
                    constraints
                    |> List.map (fun c ->
                        let typeVarName =
                            match prune env.Registry c.TargetType with
                            | TVar n -> n
                            | _ -> "unknown"

                        let paramName = $"_dict_%s{c.TraitName}_%s{typeVarName}"
                        paramName, TCon(c.TraitName, [ c.TargetType ]))

                let activeDicts =
                    dictParams
                    |> List.fold (fun acc (dName, _) -> Map.add dName dName acc) Map.empty

                let loweredBody = lowerExpr env activeDicts body
                let loweredKwArgs = kwArgs |> List.map (fun (n, t, e) -> n, t, lowerExpr env activeDicts e)
                TDefun(name, tyArgs, dictParams @ args, loweredKwArgs, restArg, retType, loweredBody, r)

        | TImpl(traitName, targetType, assoc, methods, r) ->
            TImpl(traitName, targetType, assoc, methods |> List.map (lowerDecl env), r)

        | TModule(name, decls, r) -> TModule(name, decls |> List.map (lowerDecl env), r)

        | _ -> decl // TTrait, TImport, TExport, TType, TTypeRec

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

let rec checkDecl (env: Env) (sigs: Map<string, HMType * FType option>) (decl: Decl) : Env * Map<string, HMType * FType option> * TDecl list =
    match decl with
    | DSignature(name, ftype, _) -> env, Map.add name (resolveTypeAnnotation env.Registry ftype, Some ftype) sigs, []

    | DDef(name, expr, r) ->
        let exprType, typedExpr = infer env expr

        match Map.tryFind name sigs with
        | Some (sigType, _) -> unify env.Registry exprType sigType
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
            failwithf $"Type Error: Function '%s{name}' requires a type signature (: %s{name} ...) at line %d{r.Start.Line}"

        // Extract structured keyword/rest info from the raw FType (if available)
        let mandatoryFTypes, keywordFTypes, restFTypeOpt, retFType =
            match sigOpt with
            | Some (_, Some (TArrow(m, kw, rest, ret, _))) -> m, kw, rest, Some ret
            | _ -> [], [], None, None

        let sigHMType = sigOpt |> Option.map fst

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
                    failwithf $"Type Error: Function '%s{name}' has %d{mandatoryArgNames.Length} mandatory args but signature specifies %d{mandatoryFTypes.Length} at line %d{r.Start.Line}"
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
                            failwithf $"Type Error: Keyword argument '#:%s{kwName}' not found in signature for '%s{name}' at line %d{r.Start.Line}"
                        else freshMeta()
                kwName, kwType)

        // Resolve rest arg type from signature
        let restArgType =
            match restArgName, restFTypeOpt with
            | Some _, Some ft -> Some (resolveTypeAnnotation env.Registry ft)
            | Some _, None ->
                if sigOpt.IsSome then
                    failwithf $"Type Error: Function '%s{name}' has a rest arg but signature has no #:rest at line %d{r.Start.Line}"
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

        let recEnv =
            addBinding
                name
                { Scheme = Scheme([], [], funType)
                  IsMutable = false }
                env

        // Bind mandatory args
        let bodyEnv =
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
                bodyEnv

        // Bind rest arg as Array type
        let bodyEnv =
            match restArgName, restArgType with
            | Some rn, Some rt ->
                addBinding rn { Scheme = Scheme([], [], TCon("Array", [rt])); IsMutable = false } bodyEnv
            | _ -> bodyEnv

        let bodyType, typedBody = infer bodyEnv body
        unify env.Registry bodyType expectedRetType

        // Type-check keyword default expressions
        let typedKeywordArgs =
            List.zip keywordArgDefs keywordTypes
            |> List.map (fun ((kwName, defaultExpr), (_, kwType)) ->
                let defaultType, typedDefault = infer env defaultExpr
                unify env.Registry defaultType kwType
                kwName, kwType, typedDefault)

        let scheme = generalize env funType
        let (Scheme(vars, _, schemeType)) = scheme

        // Collect trait constraints from the body
        let traitConstraints = collectTraitConstraints env.Registry typedBody
        let schemeWithConstraints = Scheme(vars, traitConstraints, schemeType)

        // Build FunMeta for call-site keyword/rest handling
        let funMeta = {
            MandatoryCount = mandatoryTypes.Length
            KeywordParams = keywordTypes
            RestParam = restArgType
        }

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
        | Some (sigType, _) -> unify env.Registry exprType sigType
        | None -> ()

        let newEnv =
            addBinding
                name
                { Scheme = generalize env exprType
                  IsMutable = true }
                env

        newEnv, Map.remove name sigs, [ TDefMutable(name, typedExpr, exprType, r) ]

    | DModule(moduleName, decls, r) ->
        // 1. Pre-pass: collect all explicit signatures defined in this module
        let explicitSigs =
            decls
            |> List.choose (function
                | DSignature(name, ftype, _) -> Some(name, (resolveTypeAnnotation env.Registry ftype, Some ftype))
                | _ -> None)
            |> Map.ofList

        // 2. Validate exports against collected signatures
        decls
        |> List.iter (function
            | DExport(names, exprRange) ->
                for name in names do
                    if not (Map.containsKey name explicitSigs) then
                        failwithf "Export Error: Exported item '%s' is missing a mandatory type signature at line %d" name exprRange.Start.Line
            | _ -> ())

        // 3. Inject module signatures into the environment for out-of-order inference
        let combinedSigs = Map.fold (fun acc k v -> Map.add k v acc) sigs explicitSigs

        // 4. Standard sequential typechecking pass
        let finalEnv, finalSigs, typedDecls =
            decls
            |> List.fold
                (fun (currEnv, currSigs, accDecls) d ->
                    let nextEnv, nextSigs, tDecls = checkDecl currEnv currSigs d
                    (nextEnv, nextSigs, tDecls @ accDecls))
                (env, combinedSigs, [])

        finalEnv, finalSigs, [ TModule(moduleName, List.rev typedDecls, r) ]

    | DImport(paths, r) -> env, sigs, [ TImport(paths, r) ]
    | DExport(names, r) -> env, sigs, [ TExport(names, r) ]
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
            let scheme = Scheme(["'" + implementorVar], [], methodTypeWithAssoc)
            finalEnv <- addBinding kvp.Key { Scheme = scheme; IsMutable = false } finalEnv

        // TDecl representation requires a TTrait node definition in your AST
        finalEnv, sigs, [ TTrait(traitName, implementorVar, assocTypes, hmSignatures, r) ]
    | DTypeRec(typeDefs, r) -> registerTypeDefs true typeDefs env, sigs, [ TTypeRec(typeDefs, r) ]
    | DImpl(traitName, targetTypeExpr, assocBindings, methods, r) ->
        let targetType = resolveTypeAnnotation env.Registry targetTypeExpr

        let typeKey =
            match targetType with
            | TCon(name, _) -> name
            | _ -> failwithf $"Trait implementations require concrete target types at line %d{r.Start.Line}"

        let isLocalTrait = env.Registry.IsTraitDefinedLocally(traitName)
        let isLocalType = env.Registry.IsTypeDefinedLocally(typeKey)

        if not (isLocalTrait || isLocalType) then
            failwithf
                $"Orphan Rule Violation at line %d{r.Start.Line}: Cannot implement foreign trait '%s{traitName}' for foreign type '%s{typeKey}'."

        let hmAssocBindings =
            assocBindings
            |> List.map (fun (name, fType) -> name, resolveTypeAnnotation env.Registry fType)

        let hmAssocBindingsMap = Map.ofList hmAssocBindings
        let regEnv = addImplementation traitName typeKey hmAssocBindingsMap env
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

                    // FIX 3: Pass expectedSignature through 'sigs'. 
                    // This forces DDefun to unify the expected types into the arguments BEFORE inference and generalization!
                    let methodSigs = Map.add name (expectedSignature, None) Map.empty
                    
                    let _, _, tDecls = checkDecl regEnv methodSigs methodDecl
                    List.head tDecls // Return the fully verified TDefun node

                | _ -> failwithf $"Only 'defun' declarations are allowed inside 'def/impl' at line %d{r.Start.Line}")

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

// --- PIPELINE COORDINATION ---
let checkProgram (initialEnv: Env) (program: Decl list) : Env * TDecl list =
    let explicitSigs =
        program
        |> List.choose (function
            | DSignature(name, typ, _) -> Some(name, (resolveTypeAnnotation initialEnv.Registry typ, Some typ))
            | _ -> None)
        |> Map.ofList

    program
    |> List.iter (function
        | DExport(names, exprRange) ->
            for name in names do
                if not (Map.containsKey name explicitSigs) then
                    failwithf "Export Error: Exported item '%s' is missing a mandatory type signature at line %d" name exprRange.Start.Line
        | _ -> ())

    // 1. Run Hindley-Milner Inference Engine (Outputs TDecl list)
    let finalEnv, _, revDecls =
        program
        |> List.fold
            (fun (currEnv, currSigs, typedDecls: TDecl list) decl ->
                let nextEnv, nextSigs, tDecls = checkDecl currEnv currSigs decl
                (nextEnv, nextSigs, tDecls @ typedDecls))
            (initialEnv, explicitSigs, [])

    let finalMutableAST = List.rev revDecls

    // 2. Lower match expressions into decision trees
    let loweredAST =
        finalMutableAST |> List.map (MatchCompiler.lowerDeclMatches finalEnv)

    // 3. Lower trait dispatch (devirtualize concrete types, dictionary-pass generics)
    let dispatchedAST =
        loweredAST |> List.map (DictionaryLowering.lowerDecl finalEnv)

    finalEnv, dispatchedAST
