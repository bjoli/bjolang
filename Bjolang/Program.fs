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

    // Determine output path (e.g., file.bjo -> file.dll or file.exe)
    let extension = if options.IsLibrary then ".dll" else ".exe"
    let outputFilePath = Path.ChangeExtension(inputFilePath, extension)

    try


        printfn $"Compiling %s{inputFilePath} -> %s{outputFilePath}"

        // 4. (Placeholder) Run your pipeline!
        // let ast = Parser.parseFile inputFilePath
        // let env, _, typedAst = TypeChecker.checkProgram resolutionCtx ast
        // let loweredDecls = ClosureConversion.convertProgram typedAst

        // 5. Pass the isLibrary flag to the Emitter
        // Emitter.compileAssembly outputFilePath options.IsLibrary loweredDecls
        // here we should add the complation to a library
        let result = Pipeline.compile inputFilePath "out.exe" options.IsLibrary
        match result with
        | Some (env, typedAst, dllDeps) ->
            printfn "Compilation succeeded. %d declarations." typedAst.Length
            
            let rec extractExports (decls: TypeChecker.TDecl list) =
                decls |> List.choose (function 
                    | TypeChecker.TExport(names, _) -> Some names 
                    | TypeChecker.TModule(_, innerDecls, _) -> Some (extractExports innerDecls |> List.concat)
                    | _ -> None)
            
            let exports = extractExports typedAst |> List.concat
            
            let exportMetadata =
                if options.IsLibrary && not exports.IsEmpty then
                    let serializeExport name =
                        match Map.tryFind name env.Bindings with
                        | Some b -> 
                            let (TypeChecker.Scheme(_, _, t)) = b.Scheme
                            $"(: %s{name} %s{Codegen.serializeHMType t})"
                        | None -> ""
                    exports |> List.map serializeExport |> String.concat "\n"
                else ""

            let csCode = Codegen.generateProgram exportMetadata typedAst
            
            // DEBUG: Dump the AST to a file so we can inspect it
            File.WriteAllText("ast_dump.txt", sprintf "%A" typedAst)
            
            let mainModuleName = Path.GetFileNameWithoutExtension(inputFilePath) |> Codegen.sanitizeIdent
            let entryPointCode = if not options.IsLibrary then $"\npublic static class BjolangEntryPoint {{ public static void Main(string[] args) {{ %s{mainModuleName}.main(0); }} }}\n" else ""
            let fullCode = csCode + entryPointCode

            let tmpDir = Path.Combine(Path.GetTempPath(), "Bjolang_" + System.Guid.NewGuid().ToString("N"))
            Directory.CreateDirectory(tmpDir) |> ignore
            
            let projType = if options.IsLibrary then "Library" else "Exe"
            let runtimeDllPath = Path.GetFullPath("BjolangRuntime/bin/Release/net10.0/BjolangRuntime.dll")
            
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
{dllReferences}
  </ItemGroup>
</Project>"""
            File.WriteAllText(Path.Combine(tmpDir, "Project.csproj"), csprojContent)
            File.WriteAllText(Path.Combine(tmpDir, "Program.cs"), fullCode)
            File.WriteAllText("out.cs", fullCode)
            
            let outDir = Path.GetFullPath(if System.String.IsNullOrWhiteSpace(Path.GetDirectoryName(outputFilePath)) then "." else Path.GetDirectoryName(outputFilePath))
            let assemblyName = Path.GetFileNameWithoutExtension(outputFilePath)
            
            printfn "Invoking C# Compiler..."
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
