module Bjolang.Lowering

open Bjolang.TypedAST
open Bjolang.Unification

/// Resolves trait dispatch after type inference has run.
///
/// Concrete receivers are devirtualized into direct static calls; generic
/// receivers are dispatched through explicit dictionary parameters that are
/// injected into the enclosing function's signature.
///
/// `TMatch` nodes are passed through untouched - pattern matching is emitted
/// directly as C# patterns by the code generator.
/// The name a devirtualized trait method call is emitted under. `Codegen`
/// rewrites `::` to `.`, so this names the singleton's instance method.
let implInstanceMethod (traitName: string) (targetTypeName: string) (methodName: string) =
    let targetTypeSanitized = targetTypeName.Replace(".", "_")
    $"%s{traitName}_%s{targetTypeSanitized}.Instance::%s{methodName}"

/// The type of a dictionary for `traitName` at `implType`.
///
/// A trait is emitted as an interface parameterized by its implementor *and*
/// every associated type — `Foldable<T_col, T_item>` — so a dictionary has to
/// name them all. For a concrete implementor `prune` resolves each projection
/// through the registry; for a type variable it leaves a `TAssoc` standing,
/// which the code generator spells as a synthesized type parameter.
let dictionaryType (env: Env) (traitName: string) (implType: HMType) : HMType =
    let assocArgs =
        match Map.tryFind traitName env.Registry.Traits with
        | Some info ->
            info.AssociatedTypes
            |> List.map (fun assocName -> prune env.Registry (TAssoc(traitName, assocName, implType)))
        | None -> []

    TCon(traitName, implType :: assocArgs)

module DictionaryLowering =

    let rec lowerExpr (env: Env) (activeDicts: Map<string, string>) (expr: TypedExpr) : TypedExpr =
        let recurse e = lowerExpr env activeDicts e

        match expr.Node with
        // Target trait method invocations
        | TApply({ Node = TIdent(methodName, _)
                   Type = TFun(argTypes, _) } as target,
                 args, kwArgs) ->
            let traitMethodOpt =
                env.Registry.Traits
                |> Map.tryPick (fun traitName info ->
                    if Map.containsKey methodName info.Signatures then
                        Some(traitName, info)
                    else
                        None)

            let node =
                match traitMethodOpt with
                | Some(traitName, _) ->
                    let targetObj = args.Head
                    let loweredArgs = args |> List.map recurse
                    let receiverType = prune env.Registry argTypes[0]

                    match prune env.Registry targetObj.Type with
                    | TCon(targetTypeName, tconArgs) ->
                        // STATIC DISPATCH: Direct devirtualization.
                        // The methods live on the impl *class* as instance methods,
                        // so the call has to go through its singleton.
                        // Propagate the TCon's type args so codegen can emit
                        // generic class instantiations (e.g. Foldable_List<int>).
                        let staticDirectTarget =
                            { target with
                                Node = TIdent(implInstanceMethod traitName targetTypeName methodName, tconArgs) }

                        TApply(staticDirectTarget, loweredArgs, [])

                    | TVar varName ->
                        // GENERIC DISPATCH
                        let expectedDictName = $"_dict_%s{traitName}_%s{varName}"

                        if not (Map.containsKey expectedDictName activeDicts) then
                            failwithf
                                $"Missing dictionary '%s{expectedDictName}' for trait dispatch at line %d{expr.Range.Start.Line}"

                        let dictIdent =
                            { Type = dictionaryType env traitName receiverType
                              Range = expr.Range
                              Node = TIdent(expectedDictName, []) }
                            : TypedExpr

                        TInterfaceCall(dictIdent.Type, methodName, dictIdent, loweredArgs)

                    | _ -> failwithf $"Unsupported receiver type for trait dispatch at line %d{expr.Range.Start.Line}"

                | None ->
                    // Not a trait method. The callee may still carry trait constraints
                    // that require us to pass dictionaries explicitly.
                    let standardCall () =
                        TApply(
                            recurse target,
                            args |> List.map recurse,
                            kwArgs |> List.map (fun (n, e) -> n, recurse e)
                        )

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
                                    constraints
                                    |> List.map (fun c ->
                                        let resolvedType =
                                            match c.TargetType with
                                            | TVar varName ->
                                                match Map.tryFind varName varSubst with
                                                | Some t -> prune env.Registry t
                                                | None -> c.TargetType
                                            | _ -> prune env.Registry c.TargetType

                                        match resolvedType with
                                        | TCon(typeName, tconArgs) ->
                                            // Static dispatch: pass the singleton Instance
                                            let sanitizedTypeName = typeName.Replace(".", "_")
                                            let instanceName = $"%s{c.TraitName}_%s{sanitizedTypeName}::Instance"

                                            { Type = dictionaryType env c.TraitName resolvedType
                                              Range = expr.Range
                                              Node = TIdent(instanceName, tconArgs) }
                                            : TypedExpr
                                        | TVar varName ->
                                            // Forward the dictionary from our own parameters
                                            let expectedDictName = $"_dict_%s{c.TraitName}_%s{varName}"

                                            if not (Map.containsKey expectedDictName activeDicts) then
                                                failwithf
                                                    $"Missing dictionary '%s{expectedDictName}' to forward for call to '%s{calleeName}' at line %d{expr.Range.Start.Line}"

                                            { Type = dictionaryType env c.TraitName resolvedType
                                              Range = expr.Range
                                              Node = TIdent(expectedDictName, []) }
                                            : TypedExpr
                                        | _ ->
                                            failwithf
                                                $"Cannot resolve dictionary for type %A{resolvedType} at line %d{expr.Range.Start.Line}")

                                TApply(
                                    recurse target,
                                    dictArgs @ (args |> List.map recurse),
                                    kwArgs |> List.map (fun (n, e) -> n, recurse e)
                                )
                            else
                                standardCall ()
                        | None -> standardCall ()
                    | _ -> standardCall ()

            { expr with Node = node }

        // Everything else recurses structurally.
        | _ -> TypeVisitor.mapChildren recurse expr

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
                        paramName, dictionaryType env c.TraitName c.TargetType)

                // Each associated type of a constrained trait becomes a type
                // parameter of the function itself. The caller never writes it:
                // C# infers it from the dictionary argument, whose impl class
                // fixes the association (`Foldable_Vec<int>` is a
                // `Foldable<Vec<int>, int>`).
                let assocTyArgs =
                    constraints
                    |> List.collect (fun c ->
                        match prune env.Registry c.TargetType, Map.tryFind c.TraitName env.Registry.Traits with
                        | TVar typeVarName, Some info ->
                            info.AssociatedTypes
                            |> List.map (assocTypeVar typeVarName)
                        | _ -> [])

                let activeDicts =
                    dictParams
                    |> List.fold (fun acc (dName, _) -> Map.add dName dName acc) Map.empty

                let loweredBody = lowerExpr env activeDicts body

                let loweredKwArgs =
                    kwArgs |> List.map (fun (n, t, e) -> n, t, lowerExpr env activeDicts e)

                TDefun(
                    name,
                    (tyArgs @ assocTyArgs) |> List.distinct,
                    dictParams @ args,
                    loweredKwArgs,
                    restArg,
                    retType,
                    loweredBody,
                    r
                )

        | TImpl(traitName, targetType, assoc, methods, r) ->
            TImpl(traitName, targetType, assoc, methods |> List.map (lowerDecl env), r)

        | TModule(name, decls, r) -> TModule(name, decls |> List.map (lowerDecl env), r)

        | _ -> decl // TTrait, TImport, TExport, TType, TTypeRec, TExtern

/// Runs every post-inference lowering stage over a checked program.
let lowerProgram (env: Env) (decls: TDecl list) : TDecl list =
    decls |> List.map (DictionaryLowering.lowerDecl env)
