type CompilerOptions =
    { InputFile: string option
      IsLibrary: bool }

let defaultOptions = { InputFile = None; IsLibrary = false }

let printUsage () =
    printfn "Fisp Compiler"
    printfn "Usage: fisp [options] <source.bjo>"
    printfn ""
    printfn "Options:"
    printfn "  --lib       Compile the source as a library (.dll) instead of an executable"
    printfn "  --help      Show this help message"

let rec parseArgs (args: string list) (opts: CompilerOptions) =
    match args with
    | [] -> opts
    | "--help" :: _ ->
        printUsage ()
        exit 0
    | "--lib" :: rest -> parseArgs rest { opts with IsLibrary = true }
    | arg :: rest when not (arg.StartsWith("-")) ->
        // If it doesn't start with '-', assume it's the input file
        match opts.InputFile with
        | None -> parseArgs rest { opts with InputFile = Some arg }
        | Some _ ->
            printfn "Error: Multiple input files specified."
            exit 1
    | unknown :: _ ->
        printfn $"Error: Unknown argument '%s{unknown}'"
        printUsage ()
        exit 1



open Bjolang
open System.IO

[<EntryPoint>]
let main argv =
    // 1. Parse CLI arguments
    let options = parseArgs (Array.toList argv) defaultOptions

    // 2. Validate inputs
    let inputFilePath =
        match options.InputFile with
        | Some path -> path
        | None ->
            printfn "Error: No input file specified."
            printUsage ()
            exit 1

    if not (File.Exists(inputFilePath)) then
        printfn $"Error: Source file '%s{inputFilePath}' not found."
        exit 1

    try


        printfn $"Compiling %s{inputFilePath}"

        // 4. (Placeholder) Run your pipeline!
        // let ast = Parser.parseFile inputFilePath
        // let env, _, typedAst = TypedAST.checkProgram resolutionCtx ast
        // let loweredDecls = ClosureConversion.convertProgram typedAst

        // 5. Pass the isLibrary flag to the Emitter
        // Emitter.compileAssembly outputFilePath options.IsLibrary loweredDecls
        // here we should add the complation to a library
        let result = Pipeline.compile inputFilePath "out.exe" options.IsLibrary
        match result with
        | Some (env, typedAst, dllDeps) ->
            // A source file with no `main` is a library whether or not `--lib`
            // was passed: an entry point would call a method that does not
            // exist, and a C# `Exe` without a `Main` does not link at all.
            let isLibrary = options.IsLibrary || not (Map.containsKey "main" env.Bindings)
            let extension = if isLibrary then ".dll" else ".exe"
            let outputFilePath = Path.ChangeExtension(inputFilePath, extension)

            printfn "Compilation succeeded. %d declarations." typedAst.Length
            
            let rec extractExports (decls: TypedAST.TDecl list) =
                decls |> List.choose (function 
                    | TypedAST.TExport(names, _) -> Some names 
                    | TypedAST.TModule(_, innerDecls, _) -> Some (extractExports innerDecls |> List.concat)
                    | _ -> None)
            
            let exports = extractExports typedAst |> List.concat
            
            let rec extractTypes (decls: TypedAST.TDecl list) =
                decls |> List.choose (function
                    | TypedAST.TType(defs, _) -> Some (defs |> List.map (fun d -> d, false))
                    | TypedAST.TTypeRec(defs, _) -> Some (defs |> List.map (fun d -> d, true))
                    | TypedAST.TModule(_, innerDecls, _) -> Some (extractTypes innerDecls |> List.concat)
                    | _ -> None)
            
            let typesToExport = extractTypes typedAst |> List.concat
            
            let exportMetadata =
                if isLibrary && (not exports.IsEmpty || not typesToExport.IsEmpty) then
                    let serializeExport name =
                        match Map.tryFind name env.Bindings with
                        | Some b ->
                            let (TypedAST.Scheme(_, constraints, t)) = b.Scheme
                            let typeStr = $"(: %s{name} %s{Codegen.serializeHMType t})"
                            if constraints.IsEmpty then typeStr
                            else
                                let constraintStrs = 
                                    constraints |> List.map (fun c ->
                                        let targetStr = Codegen.serializeHMType c.TargetType
                                        $"(%s{c.TraitName} %s{targetStr})")
                                let whereClause = "(where " + String.concat " " constraintStrs + ")"
                                $"(: %s{name} %s{Codegen.serializeHMType t} %s{whereClause})"
                        | None -> ""
                        
                    let rec serializeFType (ft: Parser.FType) : string =
                        match ft with
                        | Parser.TName(n, _) -> n
                        | Parser.TApp(n, args, _) -> $"({n} " + String.concat " " (List.map serializeFType args) + ")"
                        | Parser.TArrow(mandatory, keywords, restOpt, ret, _) ->
                            let mandatoryStrs = mandatory |> List.map serializeFType
                            let keywordStrs = keywords |> List.map (fun (n, t) -> $"(#:{n} {serializeFType t})")
                            let restStrs = match restOpt with Some t -> [$"#:rest {serializeFType t}"] | None -> []
                            let allParts = mandatoryStrs @ keywordStrs @ restStrs @ [serializeFType ret]
                            "(-> " + String.concat " " allParts + ")"
                        
                    let serializeTypeDef (td: Parser.TypeDef, isRec: bool) : string =
                        let quotedArgs = td.TypeArgs |> List.map (fun a -> if a.StartsWith("'") then a else "'" + a)
                        let typeArgsStr = if td.TypeArgs.IsEmpty then "" else " " + String.concat " " quotedArgs
                        match td.Kind with
                        | Parser.Alias(ft) -> $"(type ({td.Name}{typeArgsStr}) {serializeFType ft})"
                        | Parser.Union(cases) ->
                            let serializeCase c =
                                match c with
                                | Parser.SimpleCase(n, _) -> n
                                | Parser.DataCase(n, args, _) -> $"(: {n} " + String.concat " " (List.map serializeFType args) + ")"
                            let head = if isRec then "type-rec" else "type"
                            $"({head} (({td.Name}{typeArgsStr})\n  " + String.concat "\n  " (List.map serializeCase cases) + "))"
                        | Parser.Record(fields) -> "" // Ignore for now
                        
                    let sigsStr = exports |> List.map serializeExport |> String.concat "\n"
                    let typesStr = typesToExport |> List.map serializeTypeDef |> String.concat "\n"
                    typesStr + "\n" + sigsStr
                else ""

            let csCode = Codegen.generateProgram exportMetadata (if isLibrary then dllDeps else []) typedAst
            
            // DEBUG: Dump the AST to a file so we can inspect it
            File.WriteAllText("ast_dump.txt", sprintf "%A" typedAst)
            
            let mainArgKind =
                match Map.tryFind "main" env.Bindings with
                | Some b ->
                    let (TypedAST.Scheme(_, _, t)) = b.Scheme
                    match t with
                    | TypedAST.TFun([TypedAST.TCon("List", [TypedAST.TCon("System.String", [])])], _) -> "list_string"
                    | TypedAST.TFun([], _) -> "no_args"
                    | TypedAST.TFun(_, _) -> "other"
                    | _ -> "no_args" // Not a function, treat as no-args
                | None -> "other"

            let mainModuleClass = Path.GetFileNameWithoutExtension(inputFilePath) |> Codegen.moduleClassName
            let entryPointCode = 
                if isLibrary then ""
                elif mainArgKind = "list_string" then
                    $"\npublic static class BjolangEntryPoint {{\n" +
                    $"    public static void Main(string[] args) {{\n" +
                    $"        SchemeList.SchemeList<string> bjoArgs = SchemeList.SchemeList.Empty<string>();\n" +
                    $"        for (int i = args.Length - 1; i >= 0; i--) {{\n" +
                    $"            bjoArgs = SchemeList.SchemeList.Cons(args[i], bjoArgs);\n" +
                    $"        }}\n" +
                    $"        %s{mainModuleClass}.main(bjoArgs);\n" +
                    $"    }}\n" +
                    $"}}\n"
                elif mainArgKind = "no_args" then
                    $"\npublic static class BjolangEntryPoint {{ public static void Main(string[] args) {{ %s{mainModuleClass}.main(); }} }}\n"
                else
                    $"\npublic static class BjolangEntryPoint {{ public static void Main(string[] args) {{ %s{mainModuleClass}.main(0); }} }}\n"
            let fullCode = csCode + entryPointCode

            let tmpDir = Path.Combine(Path.GetTempPath(), "Bjolang_" + System.Guid.NewGuid().ToString("N"))
            Directory.CreateDirectory(tmpDir) |> ignore
            
            let projType = if isLibrary then "Library" else "Exe"
            let runtimeDllPath = Path.GetFullPath("BjolangRuntime/bin/Release/net10.0/BjolangRuntime.dll")
            let collectionsDllPath = Path.GetFullPath("BjolangRuntime/bin/Release/net10.0/Collections.dll")
            let schemeListDllPath = Path.GetFullPath("BjolangRuntime/bin/Release/net10.0/SchemeList.dll")
            

            let dllReferences =
                dllDeps
                |> List.map (fun dllPath ->
                    let name = Path.GetFileNameWithoutExtension(dllPath)
                    $"    <Reference Include=\"{name}\">\n      <HintPath>{dllPath}</HintPath>\n    </Reference>")
                |> String.concat "\n"
                
            let csprojContent = $"""<Project Sdk="Microsoft.NET.Sdk">
  <PropertyGroup>
    <OutputType>{projType}</OutputType>
    <TargetFramework>net10.0</TargetFramework>
    <ImplicitUsings>enable</ImplicitUsings>
    <Nullable>enable</Nullable>
  </PropertyGroup>
  <ItemGroup>
    <Reference Include="BjolangRuntime">
      <HintPath>{runtimeDllPath}</HintPath>
    </Reference>
    <Reference Include="Collections">
      <HintPath>{collectionsDllPath}</HintPath>
    </Reference>
    <Reference Include="SchemeList">
      <HintPath>{schemeListDllPath}</HintPath>
    </Reference>
{dllReferences}
  </ItemGroup>
</Project>"""
            File.WriteAllText(Path.Combine(tmpDir, "Project.csproj"), csprojContent)
            File.WriteAllText(Path.Combine(tmpDir, "Program.cs"), fullCode)
            File.WriteAllText("out.cs", fullCode)
            
            let outDir = Path.GetFullPath(if System.String.IsNullOrWhiteSpace(Path.GetDirectoryName(outputFilePath)) then "." else Path.GetDirectoryName(outputFilePath))
            let assemblyName = Path.GetFileNameWithoutExtension(outputFilePath)
            
            printfn "Invoking C# Compiler..."
            
            // This does a fast compilation using the C# dll instead of the dotnet exe.
            let tryFastCompile () =
                try
                    let loc = typeof<obj>.Assembly.Location
                    let dotnetRoot = Path.GetFullPath(Path.Combine(Path.GetDirectoryName(loc), "..", "..", ".."))
                    let sdkDir = Path.Combine(dotnetRoot, "sdk")
                    if not (Directory.Exists(sdkDir)) then None
                    else
                        let cscFiles = Directory.GetFiles(sdkDir, "csc.dll", SearchOption.AllDirectories)
                        match cscFiles |> Array.tryHead with
                        | None -> None
                        | Some cscDll ->
                            let target = if isLibrary then "library" else "exe"
                            let runtimeDir = Path.GetDirectoryName(loc)
                            let bclRefs =
                                Directory.GetFiles(runtimeDir, "*.dll")
                                |> Array.map (fun p -> $"\"-r:{p}\"")
                                |> String.concat " "
                            
                            let userRefs =
                                ([runtimeDllPath; collectionsDllPath; schemeListDllPath] @ dllDeps)
                                |> List.filter File.Exists
                                |> List.map (fun p -> $"\"-r:{Path.GetFullPath(p)}\"")
                                |> String.concat " "
                                
                            let csFile = Path.Combine(tmpDir, "Program.cs")
                            let targetPath = Path.GetFullPath(outputFilePath)
                            let cscArgs = $"exec \"{cscDll}\" -noconfig -nullable:enable -target:{target} -out:\"{targetPath}\" \"{csFile}\" {userRefs} {bclRefs}"
                            let psi = System.Diagnostics.ProcessStartInfo("dotnet", cscArgs)
                            psi.UseShellExecute <- false
                            psi.RedirectStandardOutput <- true
                            psi.RedirectStandardError <- true
                            let p = System.Diagnostics.Process.Start(psi)
                            let stdout = p.StandardOutput.ReadToEnd()
                            let stderr = p.StandardError.ReadToEnd()
                            p.WaitForExit()
                            if p.ExitCode = 0 then
                                let outputDir = Path.GetDirectoryName(targetPath)
                                let assemblyBaseName = Path.GetFileNameWithoutExtension(targetPath)
                                if not isLibrary then
                                    let runtimeConfigPath = Path.ChangeExtension(targetPath, ".runtimeconfig.json")
                                    let runtimeConfigContent = "{\n  \"runtimeOptions\": {\n    \"tfm\": \"net10.0\",\n    \"framework\": {\n      \"name\": \"Microsoft.NETCore.App\",\n      \"version\": \"10.0.0\"\n    }\n  }\n}"
                                    File.WriteAllText(runtimeConfigPath, runtimeConfigContent)
                                // Build deps.json so .NET can resolve our runtime DLLs
                                let allDeps = [runtimeDllPath; collectionsDllPath; schemeListDllPath] @ dllDeps |> List.filter File.Exists
                                let depEntries =
                                    allDeps |> List.map (fun dllPath ->
                                        let name = Path.GetFileNameWithoutExtension(dllPath)
                                        let ver = "1.0.0.0"
                                        let fileName = Path.GetFileName(dllPath)
                                        $"\"{name}/{ver}\": {{ \"runtime\": {{ \"{fileName}\": {{ \"assemblyVersion\": \"{ver}\", \"fileVersion\": \"{ver}\" }} }} }}")
                                let depNames =
                                    allDeps |> List.map (fun dllPath ->
                                        let name = Path.GetFileNameWithoutExtension(dllPath)
                                        $"\"{name}\": \"1.0.0.0\"")
                                let libEntries =
                                    allDeps |> List.map (fun dllPath ->
                                        let name = Path.GetFileNameWithoutExtension(dllPath)
                                        $"\"{name}/1.0.0.0\": {{ \"type\": \"reference\", \"serviceable\": false, \"sha512\": \"\" }}")
                                let depsJson = "{\n  \"runtimeTarget\": { \"name\": \".NETCoreApp,Version=v10.0\", \"signature\": \"\" },\n  \"compilationOptions\": {},\n  \"targets\": {\n    \".NETCoreApp,Version=v10.0\": {\n      \"" + assemblyBaseName + "/1.0.0\": {\n        \"dependencies\": { " + (String.concat ", " depNames) + " },\n        \"runtime\": { \"" + assemblyBaseName + ".dll\": {} }\n      },\n      " + (String.concat ",\n      " depEntries) + "\n    }\n  },\n  \"libraries\": {\n    \"" + assemblyBaseName + "/1.0.0\": { \"type\": \"project\", \"serviceable\": false, \"sha512\": \"\" },\n    " + (String.concat ",\n    " libEntries) + "\n  }\n}"
                                let depsJsonPath = Path.ChangeExtension(targetPath, ".deps.json")
                                File.WriteAllText(depsJsonPath, depsJson)
                                // Copy runtime DLLs next to the output so the exe can find them
                                for dllPath in allDeps do
                                    let destPath = Path.Combine(outputDir, Path.GetFileName(dllPath))
                                    try File.Copy(dllPath, destPath, true) with | _ -> ()
                                printfn $"Successfully built %s{outputFilePath}"
                                try Directory.Delete(tmpDir, true) with | _ -> ()
                                Some 0
                            else
                                None
                with _ -> None

            match tryFastCompile () with
            | Some code -> code
            | None ->
                let projPath = Path.Combine(tmpDir, "Project.csproj")
                let psi = new System.Diagnostics.ProcessStartInfo(
                    FileName = "dotnet",
                    Arguments = $"build \"%s{projPath}\" -c Release -o \"%s{outDir}\" /p:AssemblyName=%s{assemblyName}",
                    UseShellExecute = false
                )
                let p = System.Diagnostics.Process.Start(psi)
                p.WaitForExit()
                
                if p.ExitCode = 0 then
                    let generatedDll = Path.Combine(outDir, assemblyName + ".dll")
                    if System.IO.File.Exists(generatedDll) && Path.GetFullPath(generatedDll) <> Path.GetFullPath(outputFilePath) then
                        if System.IO.File.Exists(outputFilePath) then System.IO.File.Delete(outputFilePath)
                        System.IO.File.Move(generatedDll, outputFilePath)
                    
                    let genRuntimeConfig = Path.Combine(outDir, assemblyName + ".runtimeconfig.json")
                    let outRuntimeConfig = Path.ChangeExtension(outputFilePath, ".runtimeconfig.json")
                    if System.IO.File.Exists(genRuntimeConfig) && Path.GetFullPath(genRuntimeConfig) <> Path.GetFullPath(outRuntimeConfig) then
                        if System.IO.File.Exists(outRuntimeConfig) then System.IO.File.Delete(outRuntimeConfig)
                        System.IO.File.Move(genRuntimeConfig, outRuntimeConfig)

                    printfn $"Successfully built %s{outputFilePath}"
                    try Directory.Delete(tmpDir, true) with | _ -> ()
                    0
                else
                    printfn "C# Compilation failed."
                    // Leave tmpDir for debugging
                    printfn $"Temp directory: %s{tmpDir}"
                    1
        | None ->
            printfn "Compilation failed."
            1
    with ex ->
        printfn $"Compilation failed: %s{ex.Message}"
        printfn "%s" ex.StackTrace
        1
