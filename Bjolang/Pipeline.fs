module Bjolang.Pipeline

open System
open System.IO
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
        | { Token = RBracket } :: rest -> List.rev acc, rest

        // Quoted list: '(items...) → (quoted-list items...)
        | { Token = Quote; Range = qr } :: { Token = LParen; Range = r } :: rest ->
            let innerNodes, afterList = read rest
            let endRange = if List.isEmpty afterList then r else (List.head afterList).Range
            let listRange = unionLexerRanges qr endRange

            let isDot = function SAtom { Token = Dot } -> true | _ -> false

            let finalNodes =
                if List.exists isDot innerNodes then
                    let tupleToken = { Token = Lexer.Symbol "Tuple"; Range = r }
                    SAtom tupleToken :: List.filter (not << isDot) innerNodes
                else
                    let headToken = { Token = Lexer.Symbol "quoted-list"; Range = qr }
                    SAtom headToken :: innerNodes

            loop (SList(finalNodes, listRange) :: acc) afterList

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

        // Vec literal: [items...] → (vec-literal items...)
        | { Token = LBracket; Range = r } :: rest ->
            let innerNodes, afterList = read rest
            let endRange = if List.isEmpty afterList then r else (List.head afterList).Range
            let listRange = unionLexerRanges r endRange
            let headToken = { Token = Lexer.Symbol "vec-literal"; Range = r }
            let finalNodes = SAtom headToken :: innerNodes
            loop (SList(finalNodes, listRange) :: acc) afterList

        | token :: rest -> loop (SAtom token :: acc) rest

    loop [] tokens

let resolveImportPath (basePath: string) (importSpec: ImportSpec) : string option =
    match importSpec with
    | RelativePath p -> 
        let rawPath = Path.GetFullPath(Path.Combine(Path.GetDirectoryName(basePath), p))
        let dllPath = if rawPath.EndsWith(".bjo") then rawPath.Substring(0, rawPath.Length - 4) + ".dll" else rawPath + ".dll"
        if System.IO.File.Exists(dllPath) then Some dllPath
        else Some rawPath
    | ModulePath p -> 
        let libPath = Path.GetFullPath(Path.Combine(Environment.CurrentDirectory, "lib"))
        let relPath = Path.Combine(Array.ofList p)
        let dllPath = Path.GetFullPath(Path.Combine(libPath, relPath + ".dll"))
        let bjoPath = Path.GetFullPath(Path.Combine(libPath, relPath + ".bjo"))
        if System.IO.File.Exists(dllPath) then Some dllPath
        else Some bjoPath

type LoadedModule = {
    FilePath: string
    ModuleName: string
    Dependencies: string list
    ParsedDecls: Decl list
}

let wrapInModule (moduleName: string) (decls: Decl list) : Decl list =
    // Find the first and last range to represent the module range
    let r = 
        match decls with
        | [] -> { Start = { Line = 1; Column = 1 }; End = { Line = 1; Column = 1 } }
        | first :: _ ->
            let last = List.last decls
            let getRange d = 
                match d with
                | DDef(_, _, r) | DDefun(_, _, _, r) | DDefTuple(_, _, r) | DDefMutable(_, _, r)
                | DSignature(_, _, _, r) | DType(_, r) | DTypeRec(_, r) | DTrait(_, _, _, _, r) | DImpl(_, _, _, _, r)
                | DModule(_, _, r) | DImport(_, r) | DExport(_, r) | DExtern(_, _, _, r) -> r
            unionLexerRanges (getRange first) (getRange last)
    
    [ DModule(moduleName, decls, r) ]

let loadModuleGraph (mainFilePath: string) : Decl list * string list =
    let resolvedModules = System.Collections.Generic.Dictionary<string, LoadedModule>()
    let currentPath = System.Collections.Generic.HashSet<string>()
    let dllDeps = System.Collections.Generic.HashSet<string>()

    let rec load (filePath: string) : unit =
        let absPath = Path.GetFullPath(filePath)
        if currentPath.Contains(absPath) then
            failwithf "Cyclic dependency detected: %s" absPath
        if not (resolvedModules.ContainsKey(absPath)) then
            currentPath.Add(absPath) |> ignore
            
            let parsedDecls, deps =
                if absPath.EndsWith(".dll") then
                    dllDeps.Add(absPath) |> ignore
                    let asm = System.Reflection.Assembly.LoadFile(absPath)
                    let attr = asm.GetCustomAttributes(typeof<System.Reflection.AssemblyMetadataAttribute>, false)
                    
                    // Collect transitive DLL dependencies from BjolangDeps metadata
                    let transitiveDeps =
                        attr
                        |> Array.choose (fun a -> 
                            let meta = a :?> System.Reflection.AssemblyMetadataAttribute
                            if meta.Key = "BjolangDeps" then Some meta.Value else None)
                        |> Array.tryHead
                    let transitiveDllDeps = System.Collections.Generic.List<string>()
                    match transitiveDeps with
                    | Some depsStr ->
                        for dep in depsStr.Split(';') do
                            let depPath = dep.Trim()
                            if depPath <> "" && System.IO.File.Exists(depPath) then
                                dllDeps.Add(depPath) |> ignore
                                transitiveDllDeps.Add(depPath)
                                // Also recursively load the dep so its exports get parsed
                                load depPath
                    | None -> ()
                    
                    let exports =
                        attr
                        |> Array.choose (fun a -> 
                            let meta = a :?> System.Reflection.AssemblyMetadataAttribute
                            if meta.Key = "BjolangExports" then Some meta.Value else None)
                        |> Array.tryHead
                    match exports with
                    | Some metaStr ->
                        let tokens, _ = Lexer.tokenize metaStr |> read
                        
                        // Extract constraint info from S-expressions before parsing
                        // Format: (: name type (where (trait var) ...))
                        let extractConstraints (sexpr: SExpr) : (string * string) list =
                            match sexpr with
                            | SList(items, _) ->
                                items |> List.tryPick (function
                                    | SList(SAtom { Token = Lexer.Symbol "where" } :: constraintExprs, _) ->
                                        constraintExprs |> List.choose (function
                                            | SList([ SAtom { Token = Lexer.Symbol traitName }; SAtom { Token = Lexer.QuotedSymbol varName } ], _) ->
                                                Some (traitName, "'" + varName)
                                            | SList([ SAtom { Token = Lexer.Symbol traitName }; SAtom { Token = Lexer.Symbol varName } ], _) ->
                                                Some (traitName, varName)
                                            | _ -> None)
                                        |> Some
                                    | _ -> None)
                                |> Option.defaultValue []
                            | _ -> []
                        
                        // Build a map from name to constraints  
                        let constraintMap =
                            tokens |> List.choose (function
                                | SList(SAtom { Token = Lexer.Colon } :: SAtom { Token = Lexer.Symbol name } :: _, _) as sexpr ->
                                    let constraints = extractConstraints sexpr
                                    if constraints.IsEmpty then None
                                    else Some (name, constraints)
                                | _ -> None)
                            |> Map.ofList
                        
                        let parsedDecls = 
                            Parser.parseModule tokens
                            |> List.map (function
                                | DSignature(name, t, _, r) ->
                                    let constraints = Map.tryFind name constraintMap |> Option.defaultValue []
                                    DExtern(name, t, constraints, r)
                                | d -> d)
                        parsedDecls, transitiveDllDeps |> Seq.toList
                    | None ->
                        [], transitiveDllDeps |> Seq.toList
                else
                    let sourceCode = File.ReadAllText(absPath)
                    let tokens, _ = Lexer.tokenize sourceCode |> read
                    let parsedDecls = Parser.parseModule tokens
                    
                    let deps = 
                        parsedDecls 
                        |> List.choose (function DImport(specs, _) -> Some specs | _ -> None)
                        |> List.concat
                        |> List.choose (resolveImportPath absPath)
                    parsedDecls, deps

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
    
    // Sort topologically
    let sorted = System.Collections.Generic.List<LoadedModule>()
    let visited = System.Collections.Generic.HashSet<string>()
    let rec visit (path: string) =
        if not (visited.Contains(path)) then
            visited.Add(path) |> ignore
            let m = resolvedModules.[path]
            for dep in m.Dependencies do
                visit dep
            sorted.Add(m)

    visit (Path.GetFullPath(mainFilePath))
    
    // Concatenate all module ASTs
    let allDecls = sorted |> Seq.map (fun m -> wrapInModule m.ModuleName m.ParsedDecls) |> List.concat
    allDecls, dllDeps |> Seq.toList

let runFullFrontendPipeline (mainFilePath: string) =
    try
        printfn "=== Step 1: Parsing & Module Resolution ==="
        let parsedModuleDecls, dllDeps = loadModuleGraph mainFilePath
        let letrecifiedDecls = letrecifyModule parsedModuleDecls

        printfn "=== Step 2: Type Checking ==="
        let env, typedAst = Inference.checkProgram Prelude.prelude letrecifiedDecls

        printfn "=== Step 3: Dictionary Lowering ==="
        let loweredAst = Lowering.lowerProgram env typedAst

        printfn "=== Step 4: Tail Recursion Analysis ==="
        let tailAnalyzedAst = TailRecursion.analyzeProgram loweredAst

        printfn "=== Step 5: Loop Lowering ==="
        let loopLoweredAst = LoopLowering.lowerProgram tailAnalyzedAst

        printfn "=== Frontend pipeline complete ==="
        Some (env, loopLoweredAst, dllDeps)
    with ex ->
        printfn $"Compilation Panicked: %s{ex.Message}"
        printfn $"Stack Trace: %s{ex.StackTrace}"
        None

let compile (sourceFilePath: string) (outputFileName: string) (isLibrary: bool) =
    runFullFrontendPipeline sourceFilePath

