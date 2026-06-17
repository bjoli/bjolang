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
    | TApply of TypedExpr * TypedExpr list
    | TTupleMake of TypedExpr list
    | TListMake of TypedExpr list
    | TRecordMake of (string * TypedExpr) list
    | TRecordUpdate of string * (string * TypedExpr) list
    | TLetMutable of string * TypedExpr * TypedExpr
    | TSet of string * TypedExpr
    | TIf of TypedExpr * TypedExpr * TypedExpr
    | TTryFinally of TypedExpr * TypedExpr
    | TMatch of TypedExpr * TMatchClause list
    | TInterfaceCall of HMType * string * TypedExpr * TypedExpr list
    // Lowered
    | TIsInst of TypedExpr * HMType
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
    | TDefun of string * string list * (string * HMType) list * HMType * TypedExpr * Range
    | TType of TypeDef list * Range
    | TTypeRec of TypeDef list * Range
    | TTrait of string * string * string list * Map<string, HMType> * Range
    | TImpl of string * HMType * Map<string, HMType> * TDecl list * Range

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
      Registry: TraitRegistry }


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

// --- IMMUTABLE FINAL AST (For Emission) ---

/// Represents the completely resolved, strictly immutable type of an expression.
/// Used by the IL Emitter to determine exact .NET primitive types, box/unbox operations,
/// and method signature generation.
type FinalType =
    /// A concrete type constructor (e.g., "System.Int32", "List").
    /// Emitted as TypeBuilder/Type references.
    | FCon of string * FinalType list
    /// A function type signature. Emitted as a .NET Delegate type (Func/Action)
    /// or a direct MethodInfo signature depending on call context.
    | FFun of FinalType list * FinalType
    /// A tuple type. Emitted as System.Tuple<...> or a custom struct.
    | FTuple of FinalType list
    /// A rigid generic parameter (e.g., "'a"). The emitter maps this directly to
    /// GenericTypeParameterBuilder instances on the enclosing class or method.
    | FGeneric of string
    | FInterface of string * FinalType list // e.g., FInterface("IGettable", [FGeneric "a"; FGeneric "b"])
    | FClass of string // Represents the synthetic Display Class type

/// The core AST node carrying both syntax and immutable type/location metadata.
type FExpr =
    { Type: FinalType
      Range: Range
      Node: FExprNode }

and FPattern =
    { Type: FinalType
      Range: Range
      Node: FPatternNode }

/// Represents pattern matching constructs. Many of these are lowered
/// into FExprNode equivalents before reaching the Emitter.
and FPatternNode =
    | FPWildcard
    | FPInt of string
    | FPString of string
    | FPIdent of string
    | FPList of FPattern list * FPattern option
    | FPConstruct of string * FPattern list
    | FPApp of FExpr * FPattern
    | FPAs of FPattern * string

/// The executable nodes of the AST. The Closure Conversion pass will consume this
/// and produce a new AST where FLambda is eliminated.
and FExprNode =
    | FInt of string
    | FString of string
    /// A variable usage. The FinalType list contains the generic arguments
    /// instantiated at this specific call site (essential for emitting MethodInfo.MakeGenericMethod).
    | FIdent of string * FinalType list
    | FKeyword of string
    | FSymbol of string
    /// Standard let binding. If isFun is true, `FExpr` is the function body and `string list` are its arguments.
    /// The Closure pass must lift isFun=true bindings into methods or display classes.
    | FLet of string * bool * string list * FExpr * FExpr
    /// Mutually recursive bindings. Contains closures that must reference themselves.
    /// Emitted by allocating display classes first, then assigning method pointers to break the cycle.
    | FLetRec of (string * bool * string list * FExpr) list * FExpr
    | FLetTuple of string list * FExpr * FExpr
    /// An anonymous inline function. Must be explicitly lifted by Closure Conversion
    /// into a synthetic method or class before IL generation.
    | FLambda of string list * FExpr
    /// Function invocation. The Emitter checks if the target is a Delegate (emits Invoke)
    /// or a direct method reference (emits Call/Callvirt).
    | FApply of FExpr * FExpr list * bool
    /// Explicit jump for self-recursive calls
    | FTailCall of FExpr list
    | FTupleMake of FExpr list
    | FListMake of FExpr list
    | FRecordMake of (string * FExpr) list
    | FRecordUpdate of string * (string * FExpr) list
    | FLetMutable of string * FExpr * FExpr
    | FSet of string * FExpr
    | FIf of FExpr * FExpr * FExpr
    | FTryFinally of FExpr * FExpr
    /// High-level match. The Emitter never sees this; the MatchCompiler lowers it first.
    | FMatch of FExpr * FMatchClause list
    // FInterfaceCall(InterfaceType, MethodName, DictionaryInstance, Arguments, IsTail)
    | FInterfaceCall of FinalType * string * FExpr * FExpr list * bool

    // --- LOWERED PRIMITIVES ---
    /// Emits the `isinst` IL instruction for type testing.
    | FIsInst of FExpr * FinalType
    /// Emits `ldfld` or a property getter call.
    | FGetField of FExpr * string
    /// Emits the `ceq` IL instruction or calls Object.Equals for reference types.
    | FTypeEq of FExpr * FExpr

    // --- CLOSURE CONVERSION NODES ---
    /// Represents a dummy value (e.g., null or uninitialized reference) used during letrec desugaring
    | FNull
    /// Allocates a new object of the given type.
    | FNewObject of FinalType
    /// Mutates a field on an object. Maps to `stfld`.
    | FSetField of FExpr * string * FExpr
    /// Instantiates a delegate wrapping the given method name on the given optional instance.
    | FCreateDelegate of string * FExpr option

and FMatchClause =
    { Pattern: FPattern
      Guard: FExpr option
      Body: FExpr }

/// Top-level module declarations. The Emitter maps these to static fields,
/// static methods, and top-level CLR Types.
type FDecl =
    | FImport of ImportSpec list * Range
    | FExport of string list * Range
    | FModule of string * FDecl list * Range
    | FDef of string * FExpr * FinalType * Range
    | FDefTuple of string list * FExpr * FinalType * Range
    | FDefMutable of string * FExpr * FinalType * Range
    /// Top-level function. Contains its own generic type parameters and explicitly typed arguments.
    /// Emitted as a generic static method on the Module's static class.
    | FDefun of string * string list * (string * FinalType) list * FinalType * FExpr * Range
    | FType of TypeDef list * Range
    | FTypeRec of TypeDef list * Range
    | FTrait of string * string * string list * Map<string, FinalType> * Range
    | FImpl of string * FinalType * Map<string, FinalType> * FDecl list * Range
    /// Represents a synthetic display class generated during closure conversion
    | FDisplayClass of string * (string * FinalType) list * FDecl list * Range

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
            | None ->
                match Map.tryFind name unboundSubst with
                | Some fresh -> fresh
                | None ->
                    let fresh = freshMeta ()
                    unboundSubst <- Map.add name fresh unboundSubst
                    unboundFreshTypes <- unboundFreshTypes @ [ fresh ]
                    fresh
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

let generalize (env: Env) (t: HMType) : Scheme =
    let envFv = envFreeVars env
    let tFv = freeVars env.Registry t |> List.distinct
    let generalizable = tFv |> List.filter (fun m -> not (Set.contains m envFv))
    let typeNames = generalizable |> List.mapi (fun i _ -> string (char (97 + i)))

    List.iter2 (fun (m: MetaVar) name -> m.Value <- Some(TVar name)) generalizable typeNames

    // Default to empty constraints for now; gathering happens during inference
    Scheme(typeNames, [], t)

// --- INFERENCE ENGINE ---
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
        let inferredType =
            if value.EndsWith("uy") then TypeConstants.byteType
            elif value.EndsWith("s") then TypeConstants.shortType
            elif value.EndsWith("us") then TypeConstants.ushortType
            elif value.EndsWith("u") then TypeConstants.uintType
            elif value.EndsWith("UL") || value.EndsWith("ul") || value.EndsWith("uL") then TypeConstants.ulongType
            elif value.EndsWith("L") || value.EndsWith("l") then TypeConstants.longType
            elif value.EndsWith("d") || value.EndsWith("D") || value.Contains(".") then TypeConstants.doubleType
            else TypeConstants.intType

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


let rec infer (env: Env) (expr: Expr) : HMType * TypedExpr =
    match expr with
    | EInt(value, r) ->
        let inferredType =
            if value.EndsWith("uy") then TypeConstants.byteType
            elif value.EndsWith("s") then TypeConstants.shortType
            elif value.EndsWith("us") then TypeConstants.ushortType
            elif value.EndsWith("u") then TypeConstants.uintType
            elif value.EndsWith("UL") || value.EndsWith("ul") || value.EndsWith("uL") then TypeConstants.ulongType
            elif value.EndsWith("L") || value.EndsWith("l") then TypeConstants.longType
            elif value.EndsWith("d") || value.EndsWith("D") || value.Contains(".") then TypeConstants.doubleType
            else TypeConstants.intType

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
        let typedArgs = args |> List.map (infer env)
        let retType = freshMeta ()
        unify env.Registry targetType (TFun(typedArgs |> List.map fst, retType))

        retType,
        { Type = retType
          Range = r
          Node = TApply(typedTarget, typedArgs |> List.map snd) }

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
                TFun(argTypes, bodyType), typedBody
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
                        TFun(argTypes, bodyType), typedBody
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

            let checkedTarget =
                if prune traits target.Type <> prune traits body.Type then
                    match prune traits target.Type with
                    | TVar _ ->
                        ({ Type = body.Type
                           Range = pat.Range
                           Node = TIsInst(target, body.Type) }
                        : TypedExpr)
                    | _ -> target
                else
                    target

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
            match name with
            | "Nil" ->
                let isEmptyFn =
                    ({ Type = TFun([ target.Type ], TypeConstants.boolType)
                       Range = pat.Range
                       Node = TIdent("is-empty", []) }
                    : TypedExpr)

                let cond =
                    ({ Type = TypeConstants.boolType
                       Range = pat.Range
                       Node = TApply(isEmptyFn, [ target ]) }
                    : TypedExpr)

                ({ Type = failExpr.Type
                   Range = pat.Range
                   Node = TIf(cond, cont failExpr, failExpr) }
                : TypedExpr)

            | "Cons" ->
                let isEmptyFn =
                    ({ Type = TFun([ target.Type ], TypeConstants.boolType)
                       Range = pat.Range
                       Node = TIdent("is-empty", []) }
                    : TypedExpr)

                let isEmptyCall =
                    ({ Type = TypeConstants.boolType
                       Range = pat.Range
                       Node = TApply(isEmptyFn, [ target ]) }
                    : TypedExpr)

                let falseIdent =
                    ({ Type = TypeConstants.boolType
                       Range = pat.Range
                       Node = TIdent("false", []) }
                    : TypedExpr)

                let trueIdent =
                    ({ Type = TypeConstants.boolType
                       Range = pat.Range
                       Node = TIdent("true", []) }
                    : TypedExpr)

                let isConsCond =
                    ({ Type = TypeConstants.boolType
                       Range = pat.Range
                       Node = TIf(isEmptyCall, falseIdent, trueIdent) }
                    : TypedExpr)

                let compileSubPats actualFail =
                    let elemType =
                        match prune traits target.Type with
                        | TCon(_, [ t ]) -> t
                        | _ -> TypeConstants.objType

                    let headField =
                        ({ Type = elemType
                           Range = pat.Range
                           Node = TGetField(target, "Head") }
                        : TypedExpr)

                    let tailField =
                        ({ Type = target.Type
                           Range = pat.Range
                           Node = TGetField(target, "Tail") }
                        : TypedExpr)

                    compilePattern
                        traits
                        headField
                        subPats[0]
                        (fun f1 -> compilePattern traits tailField subPats[1] (fun f2 -> cont f2) f1)
                        actualFail

                ({ Type = failExpr.Type
                   Range = pat.Range
                   Node = TIf(isConsCond, compileSubPats failExpr, failExpr) }
                : TypedExpr)
            | _ -> failwithf $"Unsupported constructor lowering: %s{name}"

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

            let logFn =
                ({ Type = TFun([ TypeConstants.stringType ], TypeConstants.voidType)
                   Range = expr.Range
                   Node = TIdent("displayln", []) }
                : TypedExpr)

            let msgStr =
                ({ Type = TypeConstants.stringType
                   Range = expr.Range
                   Node = TString panicMsg }
                : TypedExpr)

            let defaultNode =
                match prune env.Registry expr.Type with
                | TCon(name, _) when name = TypeConstants.Int32Name ->
                    ({ Type = expr.Type
                       Range = expr.Range
                       Node = TInt "0" }
                    : TypedExpr)
                | TCon(name, _) when name = TypeConstants.BooleanName ->
                    ({ Type = TypeConstants.boolType
                       Range = expr.Range
                       Node = TIdent("false", []) }
                    : TypedExpr)
                | _ ->
                    ({ Type = expr.Type
                       Range = expr.Range
                       Node = TIdent("Nil", []) }
                    : TypedExpr)

            let failNode =
                ({ Type = expr.Type
                   Range = expr.Range
                   Node =
                     TLet(
                         "_",
                         false,
                         [],
                         ({ Type = TypeConstants.voidType
                            Range = expr.Range
                            Node = TApply(logFn, [ msgStr ]) }
                         : TypedExpr),
                         defaultNode
                     ) }
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
        | TApply(target, args) ->
            { expr with
                Node = TApply(lowerMatchExpressions env target, args |> List.map (lowerMatchExpressions env)) }
        | TIf(c, t, f) ->
            { expr with
                Node = TIf(lowerMatchExpressions env c, lowerMatchExpressions env t, lowerMatchExpressions env f) }
        | TTupleMake items ->
            { expr with
                Node = TTupleMake(items |> List.map (lowerMatchExpressions env)) }
        | TListMake items ->
            { expr with
                Node = TListMake(items |> List.map (lowerMatchExpressions env)) }
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
        | TDefun(name, typeParams, args, retT, body, r) ->
            TDefun(name, typeParams, args, retT, lowerMatchExpressions env body, r)
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
                     args) ->
                let traitMethodOpt =
                    env.Registry.Traits
                    |> Map.tryPick (fun traitName info ->
                        if Map.containsKey methodName info.Signatures then
                            Some(traitName, info)
                        else
                            None)

                match traitMethodOpt with
                | Some(traitName, _) ->
                    let loweredArgs = args |> List.map recurse
                    let receiverType = argTypes[0]

                    match prune env.Registry receiverType with
                    | TCon(typeName, _) ->
                        // DEVIRTUALIZATION
                        let implClassName = $"%s{traitName}_%s{typeName}"

                        let staticDirectTarget =
                            { target with
                                Node = TIdent( $"%s{implClassName}::%s{methodName}", []) }

                        TApply(staticDirectTarget, loweredArgs)

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
                    // Standard function call
                    TApply(recurse target, args |> List.map recurse)

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

            | TApply(target, args) -> // Fallback for non-identifier targets
                TApply(recurse target, args |> List.map recurse)

            | TTupleMake items -> TTupleMake(items |> List.map recurse)

            | TListMake items -> TListMake(items |> List.map recurse)

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

            | TGetField(tgt, n) -> TGetField(recurse tgt, n)

            | TTypeEq(t1, t2) -> TTypeEq(recurse t1, recurse t2)

        { expr with Node = node }

    let rec lowerDecl (env: Env) (decl: TDecl) : TDecl =
        match decl with
        | TDef(name, value, t, r) -> TDef(name, lowerExpr env Map.empty value, t, r)

        | TDefTuple(names, value, t, r) -> TDefTuple(names, lowerExpr env Map.empty value, t, r)

        | TDefMutable(name, value, t, r) -> TDefMutable(name, lowerExpr env Map.empty value, t, r)

        | TDefun(name, tyArgs, args, retType, body, r) ->
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
                TDefun(name, tyArgs, dictParams @ args, retType, loweredBody, r)

        | TImpl(traitName, targetType, assoc, methods, r) ->
            TImpl(traitName, targetType, assoc, methods |> List.map (lowerDecl env), r)

        | TModule(name, decls, r) -> TModule(name, decls |> List.map (lowerDecl env), r)

        | _ -> decl // TTrait, TImport, TExport, TType, TTypeRec

// --- ZONKER (Freezing Pass) ---
type Zonker() =
    let mutable nextGenericId = 0
    let genericMap = System.Collections.Generic.Dictionary<obj, string>()

    let getGenericName (m: obj) =
        match genericMap.TryGetValue(m) with
        | true, name -> name
        | false, _ ->
            let name =
                "'"
                + string (char (97 + (nextGenericId % 26)))
                + (if nextGenericId >= 26 then
                       string (nextGenericId / 26)
                   else
                       "")

            nextGenericId <- nextGenericId + 1
            genericMap[m] <- name
            name

    member this.FreezeType (env: Env) (t: HMType) : FinalType =
        let pruned = prune env.Registry t
        match pruned with
        | TVar(name) -> FGeneric name
        | TMeta m -> 
            printfn "FreezeType TMeta: ID=%d, Value=%A" m.Id m.Value
            FGeneric(getGenericName m)
        | TCon(name, args) ->
            if Map.containsKey name env.Registry.Traits then
                FInterface(name, args |> List.map (this.FreezeType env))
            else
                FCon(name, args |> List.map (this.FreezeType env))
        | TFun(args, ret) -> FFun(args |> List.map (this.FreezeType env), this.FreezeType env ret)
        | TTuple args -> FTuple(args |> List.map (this.FreezeType env))
        | TAssoc(_, assocName, _) -> FGeneric assocName

    member this.FreezePattern (env: Env) (p: TypedPattern) : FPattern =
        let node =
            match p.Node with
            | TPWildcard -> FPWildcard
            | TPInt v -> FPInt v
            | TPString v -> FPString v
            | TPIdent n -> FPIdent n
            | TPList(items, tail) ->
                FPList(List.map (this.FreezePattern env) items, Option.map (this.FreezePattern env) tail)
            | TPConstruct(n, args) -> FPConstruct(n, List.map (this.FreezePattern env) args)
            | TPApp(e, pat) -> FPApp(this.FreezeExpr env e, this.FreezePattern env pat)
            | TPAs(pat, n) -> FPAs(this.FreezePattern env pat, n)

        { Type = this.FreezeType env p.Type
          Range = p.Range
          Node = node }
        : FPattern

    member this.FreezeExpr (env: Env) (e: TypedExpr) : FExpr =
        let node =
            match e.Node with
            | TInt v -> FInt v
            | TString v -> FString v
            | TIdent(n, tArgs) -> FIdent(n, List.map (this.FreezeType env) tArgs)
            | TKeyword kw -> FKeyword kw
            | TSymbol sym -> FSymbol sym
            | TLet(n, isF, args, v, b) -> FLet(n, isF, args, this.FreezeExpr env v, this.FreezeExpr env b)
            | TLetRec(bindings, b) ->
                FLetRec(
                    bindings
                    |> List.map (fun (n, isF, args, expr) -> n, isF, args, this.FreezeExpr env expr),
                    this.FreezeExpr env b
                )
            | TLetTuple(names, v, b) -> FLetTuple(names, this.FreezeExpr env v, this.FreezeExpr env b)
            | TLambda(args, b) -> FLambda(args, this.FreezeExpr env b)
            | TApply(target, args) -> FApply(this.FreezeExpr env target, List.map (this.FreezeExpr env) args, false)
            | TTupleMake exprs -> FTupleMake(List.map (this.FreezeExpr env) exprs)
            | TListMake exprs -> FListMake(List.map (this.FreezeExpr env) exprs)
            | TRecordMake fields -> FRecordMake(fields |> List.map (fun (n, expr) -> n, this.FreezeExpr env expr))
            | TRecordUpdate(name, fields) ->
                FRecordUpdate(name, fields |> List.map (fun (n, expr) -> n, this.FreezeExpr env expr))
            | TLetMutable(n, v, b) -> FLetMutable(n, this.FreezeExpr env v, this.FreezeExpr env b)
            | TSet(n, v) -> FSet(n, this.FreezeExpr env v)
            | TIf(cond, t, f) -> FIf(this.FreezeExpr env cond, this.FreezeExpr env t, this.FreezeExpr env f)
            | TTryFinally(b, c) -> FTryFinally(this.FreezeExpr env b, this.FreezeExpr env c)
            | TMatch(target, clauses) ->
                FMatch(
                    this.FreezeExpr env target,
                    clauses
                    |> List.map (fun c ->
                        { Pattern = this.FreezePattern env c.Pattern
                          Guard = Option.map (this.FreezeExpr env) c.Guard
                          Body = this.FreezeExpr env c.Body })
                )
            | TIsInst(tgt, t) -> FIsInst(this.FreezeExpr env tgt, this.FreezeType env t)
            | TGetField(tgt, n) -> FGetField(this.FreezeExpr env tgt, n)
            | TTypeEq(t1, t2) -> FTypeEq(this.FreezeExpr env t1, this.FreezeExpr env t2)
            | TInterfaceCall(iType, mName, dict, args) -> 
                FInterfaceCall(
                    this.FreezeType env iType, 
                    mName, 
                    this.FreezeExpr env dict, 
                    args |> List.map (this.FreezeExpr env),
                    false
                )

        { Type = this.FreezeType env e.Type
          Range = e.Range
          Node = node }

// --- DECLARATION CHECKING ---
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
                match name with
                | "int" -> TypeConstants.intType
                | "byte" -> TypeConstants.byteType
                | "short" -> TypeConstants.shortType
                | "ushort" -> TypeConstants.ushortType
                | "uint" -> TypeConstants.uintType
                | "long" -> TypeConstants.longType
                | "ulong" -> TypeConstants.ulongType
                | "double" -> TypeConstants.doubleType
                | "string" -> TypeConstants.stringType
                | "bool" -> TypeConstants.boolType
                | _ -> TCon(name, [])
    | TApp("->", args, _) ->
        let resolvedArgs = args |> List.map (resolveTypeAnnotation registry)
        TFun(List.take (resolvedArgs.Length - 1) resolvedArgs, List.last resolvedArgs)
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
                let schemeArgs = tArgs |> List.map (fun s -> s.TrimStart('\''))
                let consScheme =
                    if resolvedArgs.Length = 0 then
                        Scheme(schemeArgs, [], parentType)
                    else
                        Scheme(schemeArgs, [], TFun(resolvedArgs, parentType))
                finalBindings <- Map.add caseName { Scheme = consScheme; IsMutable = false } finalBindings

    { env with Registry = finalRegistry; Bindings = finalBindings }

let rec checkDecl (env: Env) (sigs: Map<string, HMType>) (decl: Decl) : Env * Map<string, HMType> * TDecl list =
    match decl with
    | DSignature(name, ftype, _) -> env, Map.add name (resolveTypeAnnotation env.Registry ftype) sigs, []

    | DDef(name, expr, r) ->
        let exprType, typedExpr = infer env expr

        if Map.containsKey name sigs then
            unify env.Registry exprType sigs[name]

        let newEnv =
            addBinding
                name
                { Scheme = generalize env exprType
                  IsMutable = false }
                env

        newEnv, Map.remove name sigs, [ TDef(name, typedExpr, exprType, r) ]

    | DDefun(name, args, retTypeOpt, body, r) ->
        let argTypes =
            args
            |> List.map (fun (n, tOpt) ->
                n,
                match tOpt with
                | Some t -> resolveTypeAnnotation env.Registry t
                | None -> freshMeta ())

        let expectedRetType =
            match retTypeOpt with
            | Some t -> resolveTypeAnnotation env.Registry t
            | None -> freshMeta ()

        let funType = TFun(argTypes |> List.map snd, expectedRetType)

        if Map.containsKey name sigs then
            unify env.Registry funType sigs[name]

        let recEnv =
            addBinding
                name
                { Scheme = Scheme([], [], funType)
                  IsMutable = false }
                env

        let bodyEnv =
            argTypes
            |> List.fold
                (fun acc (n, t) ->
                    addBinding
                        n
                        { Scheme = Scheme([], [], t)
                          IsMutable = false }
                        acc)
                recEnv

        let bodyType, typedBody = infer bodyEnv body
        unify env.Registry bodyType expectedRetType

        let finalEnv =
            addBinding
                name
                { Scheme = generalize env funType
                  IsMutable = false }
                env

        let decl = TDefun(name, [], argTypes, expectedRetType, typedBody, r)
        if name = "foo" then
            printfn "foo TDefun: %A" decl
        if name = "main" then
            printfn "main TDefun: %A" decl

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

        if Map.containsKey name sigs then
            unify env.Registry exprType sigs[name]

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
                | DSignature(name, ftype, _) -> Some(name, resolveTypeAnnotation env.Registry ftype)
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
        finalEnv, sigs, []
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
            |> Map.ofList

        let regEnv = addImplementation traitName typeKey hmAssocBindings env
        let traitInfo = Map.find traitName regEnv.Registry.Traits

        // FIX 1: Prepend the "'" to the substitution keys so they match TVar "'c"
        let mutable substitutions = Map.add ("'" + traitInfo.ImplementorVar) targetType Map.empty

        for kvp in hmAssocBindings do
            substitutions <- Map.add ("'" + kvp.Key) kvp.Value substitutions

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
                | DDefun(name, args, retTypeOpt, body, methodRange) ->
                    let expectedSignature =
                        match Map.tryFind name traitInfo.Signatures with
                        | Some sigType -> applySubst sigType
                        | None ->
                            failwithf
                                $"Method '%s{name}' is not a member of trait '%s{traitName}' at line %d{methodRange.Start.Line}"

                    // FIX 3: Pass expectedSignature through 'sigs'. 
                    // This forces DDefun to unify the expected types into the arguments BEFORE inference and generalization!
                    let methodSigs = Map.add name expectedSignature Map.empty
                    
                    let _, _, tDecls = checkDecl regEnv methodSigs methodDecl
                    List.head tDecls // Return the fully verified TDefun node

                | _ -> failwithf $"Only 'defun' declarations are allowed inside 'def/impl' at line %d{r.Start.Line}")

        // Ensure all required methods from the trait are implemented
        for requiredMethod in traitInfo.Signatures.Keys do
            let isImplemented =
                methods
                |> List.exists (function
                    | DDefun(name, _, _, _, _) -> name = requiredMethod
                    | _ -> false)

            if not isImplemented then
                failwithf
                    "Implementation of trait '%s' is missing required method '%s' at line %d"
                    traitName requiredMethod r.Start.Line

        regEnv, sigs, [ TImpl(traitName, targetType, hmAssocBindings, typedMethods, r) ]

let rec freezeDecl (env: Env) (decl: TDecl) : FDecl =
    let z = Zonker()

    match decl with
    | TImport(p, r) -> FImport(p, r)
    | TExport(n, r) -> FExport(n, r)
    | TModule(n, decls, r) -> FModule(n, List.map (freezeDecl env) decls, r)
    | TDef(n, e, t, r) -> FDef(n, z.FreezeExpr env e, z.FreezeType env t, r)
    | TDefTuple(names, e, t, r) -> FDefTuple(names, z.FreezeExpr env e, z.FreezeType env t, r)
    | TDefMutable(n, e, t, r) -> FDefMutable(n, z.FreezeExpr env e, z.FreezeType env t, r)
    | TDefun(n, tyArgs, args, retType, body, r) ->
        let fArgs =
            args |> List.map (fun (argName, argType) -> argName, z.FreezeType env argType)

        FDefun(n, tyArgs, fArgs, z.FreezeType env retType, z.FreezeExpr env body, r)
    | TType(td, r) -> FType(td, r)
    | TTypeRec(td, r) -> FTypeRec(td, r)
    | TTrait(traitName, implVar, assocTypes, sigs, r) ->
        let fSigs = sigs |> Map.map (fun _ t -> z.FreezeType env t)
        FTrait(traitName, implVar, assocTypes, fSigs, r)
    | TImpl(traitName, targetType, assocBindings, methods, r) ->
        let fTargetType = z.FreezeType env targetType
        let fAssoc = assocBindings |> Map.map (fun _ t -> z.FreezeType env t)
        let fMethods = methods |> List.map (freezeDecl env)
        FImpl(traitName, fTargetType, fAssoc, fMethods, r)

// --- PIPELINE COORDINATION ---
// --- PIPELINE COORDINATION ---
let checkProgram (initialEnv: Env) (program: Decl list) : Env * FDecl list =
    let explicitSigs =
        program
        |> List.choose (function
            | DSignature(name, typ, _) -> Some(name, resolveTypeAnnotation initialEnv.Registry typ)
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

    // 2. Lower matches using clean, mutable types before zonking eliminates them (TDecl list -> TDecl list)
    let loweredAST =
        finalMutableAST |> List.map (MatchCompiler.lowerDeclMatches finalEnv)

    // 3. Zonk the lowered AST into an entirely deep-cloned, immutable representation (TDecl list -> FDecl list)
    let frozenAST = loweredAST |> List.map (freezeDecl finalEnv)

    finalEnv, frozenAST
