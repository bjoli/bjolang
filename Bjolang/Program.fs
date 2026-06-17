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
        Pipeline.compile inputFilePath "out.exe" options.IsLibrary |> ignore
        0 // Return success code
    with ex ->
        printfn $"Compilation failed: %s{ex.Message}"
        printfn "%s" ex.StackTrace
        1
