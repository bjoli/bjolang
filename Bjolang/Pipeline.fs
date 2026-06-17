module Bjolang.Pipeline

open System
open System.IO
open Bjolang.ASTPrinter
open Bjolang.Lexer
open Bjolang.Parser
open Bjolang.LetRecify

let unionLexerRanges (r1: Lexer.Range) (r2: Lexer.Range) : Lexer.Range =
    { Start = r1.Start; End = r2.End }

let rec read (tokens: LexedToken list) : SExpr list * LexedToken list =
    let rec loop acc remaining =
        match remaining with
        | [] -> List.rev acc, []
        | { Token = RParen } :: rest -> List.rev acc, rest
        | { Token = LParen; Range = r } as t :: rest ->
            let innerNodes, afterList = read rest
            let endRange = if List.isEmpty afterList then r else (List.head afterList).Range
            let listRange = unionLexerRanges r endRange

            let isDot = function SAtom { Token = Dot } -> true | _ -> false

            let finalNodes =
                if List.exists isDot innerNodes then
                    let tupleToken = { Token = Lexer.Symbol "Tuple"; Range = r }
                    SAtom tupleToken :: List.filter (not << isDot) innerNodes
                else
                    innerNodes

            loop (SList(finalNodes, listRange) :: acc) afterList
        | token :: rest -> loop (SAtom token :: acc) rest

    loop [] tokens

let resolveImportPath (basePath: string) (importSpec: ImportSpec) : string =
    match importSpec with
    | RelativePath p -> Path.GetFullPath(Path.Combine(Path.GetDirectoryName(basePath), p))
    | ModulePath p -> 
        let libPath = Path.GetFullPath(Path.Combine(Environment.CurrentDirectory, "lib"))
        let relPath = Path.Combine(Array.ofList p) + ".bjo"
        Path.GetFullPath(Path.Combine(libPath, relPath))

type LoadedModule = {
    FilePath: string
    ModuleName: string
    Dependencies: string list
    ParsedDecls: Decl list
}

// Simple mangler that prepends moduleName to top-level definitions that are not exported.
let mangleDecls (moduleName: string) (decls: Decl list) : Decl list =
    let exports = 
        decls 
        |> List.choose (function DExport(names, _) -> Some names | _ -> None)
        |> List.concat 
        |> Set.ofList

    let toMangle = 
        decls |> List.choose (function
            | DDef(name, _, _) | DDefun(name, _, _, _, _) | DDefMutable(name, _, _) ->
                if exports.Contains name then None else Some name
            | DSignature(name, _, _) ->
                if exports.Contains name then None else Some name
            | _ -> None
        ) |> Set.ofList

    let mangleName n = if toMangle.Contains n then moduleName + "_" + n else n

    let rec renameExpr (locals: Set<string>) (expr: Expr) : Expr =
        let rename e = renameExpr locals e
        match expr with
        | EIdent(name, r) -> 
            if not (locals.Contains name) && toMangle.Contains name then EIdent(mangleName name, r)
            else expr
        | EApp(t, args, r) -> EApp(rename t, List.map rename args, r)
        | ETuple(es, r) -> ETuple(List.map rename es, r)
        | EList(es, r) -> EList(List.map rename es, r)
        | EIf(c, t, f, r) -> EIf(rename c, rename t, rename f, r)
        | ELet(n, isFun, args, v, b, r) ->
            let locals' = locals.Add(n)
            // If it's a function, args are also local to value. But value is evaluated in `locals` for standard let.
            // Wait, ELet value is evaluated in `locals`, body in `locals'`.
            let v' = renameExpr (if isFun then Seq.fold (fun (acc:Set<string>) a -> acc.Add a) locals args else locals) v
            ELet(n, isFun, args, v', renameExpr locals' b, r)
        | ELetRec(binds, b, r) ->
            let locals' = Seq.fold (fun (acc:Set<string>) (n,_,_,_) -> acc.Add n) locals binds
            let binds' = binds |> List.map (fun (n, isFun, args, v) ->
                let valLocals = Seq.fold (fun (acc:Set<string>) a -> acc.Add a) locals' args
                (n, isFun, args, renameExpr valLocals v))
            ELetRec(binds', renameExpr locals' b, r)
        | ELetTuple(ns, v, b, r) ->
            let locals' = Seq.fold (fun (acc:Set<string>) n -> acc.Add n) locals ns
            ELetTuple(ns, rename v, renameExpr locals' b, r)
        | ELetMutable(n, v, b, r) ->
            let locals' = locals.Add(n)
            ELetMutable(n, rename v, renameExpr locals' b, r)
        | ESet(n, v, r) ->
            if not (locals.Contains n) && toMangle.Contains n then ESet(mangleName n, rename v, r)
            else ESet(n, rename v, r)
        | EFun(args, b, r) ->
            let locals' = Seq.fold (fun (acc:Set<string>) a -> acc.Add a) locals args
            EFun(args, renameExpr locals' b, r)
        | ERecord(fs, r) -> ERecord(fs |> List.map (fun (k,v) -> k, rename v), r)
        | ERecordUpdate(n, fs, r) -> ERecordUpdate(n, fs |> List.map (fun (k,v) -> k, rename v), r)
        | EGetField(e, n, r) -> EGetField(rename e, n, r)
        | EMatch(target, clauses, r) ->
            let clauses' = clauses |> List.map (fun (pat, guard, body) ->
                // Collect locals from pattern
                let rec getPatLocals p acc =
                    match p with
                    | PIdent(n, _) -> Set.add n acc
                    | PList(ps, tailOpt, _) -> 
                        let acc' = List.fold (fun a p' -> getPatLocals p' a) acc ps
                        match tailOpt with Some t -> getPatLocals t acc' | None -> acc'
                    | PConstruct(_, ps, _) -> List.fold (fun a p' -> getPatLocals p' a) acc ps
                    | _ -> acc
                let locals' = getPatLocals pat locals
                let guard' = Option.map (renameExpr locals') guard
                let body' = renameExpr locals' body
                (pat, guard', body')
            )
            EMatch(rename target, clauses', r)
        | ETryFinally(b, c, r) -> ETryFinally(rename b, rename c, r)
        | EInt _ | EString _ | EQuotedSymbol _ | EKeyword _ -> expr

    decls |> List.map (function
        | DDef(n, v, r) -> DDef(mangleName n, renameExpr Set.empty v, r)
        | DDefun(n, args, typ, b, r) -> 
            let locals = Seq.fold (fun (acc:Set<string>) (a,_) -> acc.Add a) Set.empty args
            DDefun(mangleName n, args, typ, renameExpr locals b, r)
        | DDefTuple(ns, v, r) -> DDefTuple(ns, renameExpr Set.empty v, r)
        | DDefMutable(n, v, r) -> DDefMutable(mangleName n, renameExpr Set.empty v, r)
        | DSignature(n, t, r) -> DSignature(mangleName n, t, r)
        | DImpl(traitN, t, asst, methods, r) ->
            let methods' = methods |> List.map (function
                | DDefun(n, args, typ, b, mr) -> 
                    let locals = Seq.fold (fun (acc:Set<string>) (a,_) -> acc.Add a) Set.empty args
                    DDefun(n, args, typ, renameExpr locals b, mr)
                | _ -> failwith "Impl can only contain defun"
            )
            DImpl(traitN, t, asst, methods', r)
        | DModule _ -> failwith "Nested modules not supported for mangling yet"
        | other -> other
    )

let loadModuleGraph (mainFilePath: string) : Decl list =
    let resolvedModules = System.Collections.Generic.Dictionary<string, LoadedModule>()
    let currentPath = System.Collections.Generic.HashSet<string>()

    let rec load (filePath: string) : unit =
        let absPath = Path.GetFullPath(filePath)
        if currentPath.Contains(absPath) then
            failwithf "Cyclic dependency detected: %s" absPath
        if not (resolvedModules.ContainsKey(absPath)) then
            currentPath.Add(absPath) |> ignore
            let sourceCode = File.ReadAllText(absPath)
            let tokens, _ = Lexer.tokenize sourceCode |> read
            let parsedDecls = Parser.parseModule tokens
            
            let deps = 
                parsedDecls 
                |> List.choose (function DImport(specs, _) -> Some specs | _ -> None)
                |> List.concat
                |> List.map (resolveImportPath absPath)

            for dep in deps do
                load dep

            let moduleName = Path.GetFileNameWithoutExtension(absPath).Replace(".", "_").Replace("-", "_")
            resolvedModules.[absPath] <- {
                FilePath = absPath
                ModuleName = moduleName
                Dependencies = deps
                ParsedDecls = parsedDecls
            }
            currentPath.Remove(absPath) |> ignore

    load mainFilePath

    let sorted = System.Collections.Generic.List<LoadedModule>()
    let visited = System.Collections.Generic.HashSet<string>()

    let rec visit path =
        if not (visited.Contains(path)) then
            visited.Add(path) |> ignore
            let m = resolvedModules.[path]
            for dep in m.Dependencies do visit dep
            sorted.Add(m)

    visit (Path.GetFullPath(mainFilePath))
    
    // Concatenate all mangled ASTs
    sorted |> Seq.map (fun m -> mangleDecls m.ModuleName m.ParsedDecls) |> List.concat

let runFullFrontendPipeline (mainFilePath: string) =
    try
        printfn "=== Step 1: Parsing & Module Resolution ==="
        let parsedModuleDecls = loadModuleGraph mainFilePath
        let letrecifiedDecls = letrecifyModule parsedModuleDecls

        printfn "=== Step 2: Type Checking ==="
        let env, typedAst = TypeChecker.checkProgram Prelude.prelude letrecifiedDecls
        
        printfn "=== Step 3: Tail Recursion Analysis ==="
        let tailAnalyzedAst = TailRecursion.analyzeProgram typedAst
    
        
        printfn "tail recursive ast: %A" tailAnalyzedAst
        tailAnalyzedAst
    with ex ->
        printfn $"Compilation Panicked: %s{ex.Message}"
        printfn $"Stack Trace: %s{ex.StackTrace}"
        []

let compile (sourceFilePath: string) (outputFileName: string) (isLibrary: bool) =
    let ast = runFullFrontendPipeline sourceFilePath
    ast
