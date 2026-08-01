module Bjolang.TypedAST

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



/// The type variable standing for an associated type projected out of a
/// *generic* implementor, e.g. `%item` of `Foldable %c`.
///
/// C# has nothing to project with, so a function generic in `'c` that dispatches
/// through a `Foldable` dictionary carries the element type as a second type
/// parameter of its own and lets the dictionary argument infer it:
/// `int count<T_c, T_c_item>(Foldable<T_c, T_c_item> dict, T_c c)`.
/// `Lowering` injects the parameter and `Codegen` spells the projection with it,
/// so both have to agree on the name.
let assocTypeVar (implVar: string) (assocName: string) = $"%s{implVar}_%s{assocName}"

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
    | TPVec of TypedPattern list * TypedPattern option
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
    | TApply of TypedExpr * TypedExpr list * (string * TypedExpr) list
    | TTupleMake of TypedExpr list
    | TListMake of TypedExpr list
    | TVecMake of TypedExpr list
    | TRecordMake of (string * TypedExpr) list
    | TRecordUpdate of string * (string * TypedExpr) list
    | TLetMutable of string * TypedExpr * TypedExpr
    | TSet of string * TypedExpr
    | TIf of TypedExpr * TypedExpr * TypedExpr
    /// A one-armed conditional: `(when cond body)`, or `(unless cond body)`
    /// when the flag is set. Always of type void — the body runs for its
    /// effect and its value is discarded.
    | TWhen of TypedExpr * TypedExpr * bool
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
    /// A `params`-style array. Produced by `LoopLowering` when a `TRecur`
    /// argument vector has to re-pack a rest parameter.
    | TArrayMake of TypedExpr list
    /// A group of loops produced by `LoopLowering`. Every member is a single
    /// strongly-connected component's worth of tail recursion.
    ///
    /// `TLoop (members, None)` *is* an enclosing function's body: the loop is
    /// emitted directly into that function and `Slots` name its parameters.
    ///
    /// `TLoop (members, Some body)` binds the members as local functions that
    /// are in scope in `body`.
    | TLoop of TLoopMember list * TypedExpr option
    /// A jump back to the top of member `index` of the innermost enclosing
    /// `TLoop`, carrying a *complete* argument vector aligned with that
    /// member's `Slots`.
    | TRecur of int * TypedExpr list

and TLoopMember =
    { LoopName: string
      /// Mutable parameter slots. A `TRecur` argument vector is positionally
      /// aligned with this list.
      Slots: (string * HMType) list
      /// Per-iteration copies of `Slots`, parallel by index. `Body` reads these
      /// rather than the slots so that a closure escaping one iteration cannot
      /// observe the next iteration's values.
      Locals: string list
      RetType: HMType
      Body: TypedExpr }

and TMatchClause =
    { Pattern: TypedPattern
      Guard: TypedExpr option
      Body: TypedExpr }

type TDecl =
    | TImport of ImportSpec list * Range
    | TExport of string list * Range
    | TReExport of string list * Range
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
      // Maps (TraitName * TargetTypeIdentifier) -> (GenericTargetType * Map<AssociatedTypeName, HMType>)
      // The GenericTargetType preserves TVars (e.g. TCon("List", [TVar "'a"]))
      // so ResolveAssociatedType can substitute them when given a concrete type.
      Implementations: Map<string * string, HMType * Map<string, HMType>>
      Aliases: Map<string, string list * HMType>
      Records: Map<string, string list * (string * HMType) list>
      RecordFields: Map<string, string> }

    member this.IsTraitDefinedLocally(name) = Set.contains name this.LocalTraits
    member this.IsTypeDefinedLocally(name) = Set.contains name this.LocalTypes

    member this.ResolveAssociatedType (traitName: string) (assocName: string) (implType: HMType) : HMType option =
        // Pattern-match a stored generic type against a concrete type to build
        // a substitution for type variables.
        let rec matchTypes pat conc subst =
            match pat, conc with
            | TVar name, _ -> Some (Map.add name conc subst)
            | TCon(n1, args1), TCon(n2, args2) when n1 = n2 && args1.Length = args2.Length ->
                List.fold2 (fun acc p c -> acc |> Option.bind (fun s -> matchTypes p c s)) (Some subst) args1 args2
            | _ when pat = conc -> Some subst
            | _ -> None

        let rec applySubstLocal subst t =
            match t with
            | TVar name -> match Map.tryFind name subst with Some conc -> conc | None -> t
            | TCon(n, args) -> TCon(n, args |> List.map (applySubstLocal subst))
            | TFun(args, ret) -> TFun(args |> List.map (applySubstLocal subst), applySubstLocal subst ret)
            | TTuple args -> TTuple(args |> List.map (applySubstLocal subst))
            | _ -> t

        let typeKey =
            match implType with
            | TCon(name, _) -> Some name
            | _ -> None

        match typeKey with
        | Some tk ->
            match Map.tryFind (traitName, tk) this.Implementations with
            | Some (genericTarget, assocMap) ->
                match matchTypes genericTarget implType Map.empty with
                | Some subst ->
                    Map.tryFind assocName assocMap
                    |> Option.map (applySubstLocal subst)
                | None -> None
            | None -> None
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

let addImplementation (traitName: string) (typeKey: string) (targetType: HMType) (assocBindings: Map<string, HMType>) (env: Env) : Env =
    let newRegistry =
        { env.Registry with
            Implementations = Map.add (traitName, typeKey) (targetType, assocBindings) env.Registry.Implementations }

    { env with Registry = newRegistry }

