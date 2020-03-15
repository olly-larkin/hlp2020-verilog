// Learn more about F# at http://fsharp.org

open System
open System.IO
open Verishot.Util
open Verishot.Parser
open Verishot.CoreTypes
open Verishot.ASTToDecl
open Verishot.Netlist
open Verishot.Visualise

let exitCodes: Map<string, int> = 
    Map [
        ("Success", 0)
        ("InvalidCmd", 1)

        // LINT
        ("LintError", 10)

        // VISUALISE
        ("VisualisationError", 20)
    ]

let getModuleFilePath workspacePath moduleFileName =
    workspacePath + dirSlash + moduleFileName

let getModuleFilePaths vProjFilePath =
    vProjFilePath
    |> readFileToStringList
    |> List.map (getModuleFilePath (getFolderPath vProjFilePath))

let lintHelper filePath =
    filePath
    |> readFileToString
    |> Seq.toList
    |> ParseSource

let intellisense filePath =
    filePath
    |> readFileToStringList
    |> String.concat "\n"
    |> Seq.toList
    |> ParseSource
    |> function
        | Ok _ -> 
            exitCodes.["Success"]
        | Error (errStr, loc) ->
            printf "%d ----- %d ----- %s" loc.line loc.character errStr
            exitCodes.["LintError"]

let lint filePath =
    filePath
    |> lintHelper
    |> function
        | Ok _ ->
            printf "Verishot Lint: No Errors"
            exitCodes.["Success"]
        | Error (errStr, loc) -> 
            printf "Lint Error: %s at Line %d, Char %d" errStr loc.line loc.character
            exitCodes.["LintError"]

let checkModulesExist allModules workspacePath = 
    let getModuleFilePathPair moduleFileName = moduleFileName, File.Exists <| getModuleFilePath workspacePath moduleFileName

    let logNotFound (modName, exists) = 
        match exists with
        | true -> true
        | false -> 
            printf "ERROR: Module '%s' not found in folder\n" modName
            false

    allModules 
    |> List.map (getModuleFilePathPair >> logNotFound)
    |> List.reduce (&&)
                
let checkModulesLint allModules workspacePath =
    let linter filePath =
        filePath
        |> lintHelper
        |> function
            | Ok _ -> true, ""
            | Error (errStr, loc) ->
                false, sprintf "Lint Error: %s at Line %d, Char %d" errStr loc.line loc.character

    let getModuleFilePathPair moduleFileName = moduleFileName, linter <| workspacePath + dirSlash + moduleFileName

    let logLintError (modName, (passed, lintErr)) =
        match passed with 
        | true -> true
        | false -> 
            printf "Error in module `%s`: %s\n" modName lintErr 
            false

    allModules
    |> List.map (getModuleFilePathPair >> logLintError)
    |> List.reduce (&&)

let vProjSanityCheck vProjFilePath =
    let allModules = readFileToStringList vProjFilePath
    let workspacePath = getFolderPath vProjFilePath

    checkModulesExist allModules workspacePath && checkModulesLint allModules workspacePath

(* Only call this function after linting *)
let getAST moduleFilePath =
    moduleFilePath
    |> readFileToString
    |> Seq.toList
    |> ParseSource
    |> function
        | Ok ast -> ast 
        | _ -> failwith "Should not happen"  // linted before, should never happen



let visualise vProjFilePath =
    let workspacePath = getFolderPath vProjFilePath
    let visOutputPath = sprintf "%s%s%s" workspacePath dirSlash "visualisation"

    deleteFolder visOutputPath

    let asts =
        vProjFilePath
        |> getModuleFilePaths
        |> List.map getAST

    let decls = 
        asts 
        |> List.map (ASTToDecl)

    let netlists = 
        asts
        |> List.map (moduleNetlist decls)

    createPathFolder visOutputPath

    let getOutputFilePath id = sprintf "%s%s%s%s" visOutputPath dirSlash id ".svg"

    (netlists, decls)
    ||> visualiseNetlists
    |> List.map (fun (id, svgOutput) -> writeStringToFile (getOutputFilePath id) svgOutput)
    |> ignore

    printf "Visualisation succeeded. View output in `visualisation`."
    exitCodes.["Success"]

    
[<EntryPoint>]
let main argv =
    match Array.length argv with 
    | 0 -> 
        printf "Verishot Verilog Visualiser and Simulator v0.0.1
(C) 2020 Imperial College London; lhl2617, ng2517, mp5617, oll16
-----------------------
`verishot --help` for usage guide" 
        exitCodes.["Success"]
    | _ ->
        match argv.[0] with 
        | "--help" ->
            printf "verishot <flag> <infile> [<workspacefolder]
    flag: --lint, --simulate, --visualise"
            exitCodes.["Success"]
        | "--lint" when argv.Length = 2 ->
            let filePath = argv.[1]
            lint filePath
        | "--intellisense" when argv.Length = 2 -> 
            let filePath = argv.[1]
            intellisense filePath
        | "--simulate" when argv.Length = 2 -> 
            let vprojPath = argv.[1]
            if vProjSanityCheck vprojPath then failwith "TODO1" else failwith "TODO2"
        | "--visualise" when argv.Length = 2 -> 
            let vprojPath = argv.[1]
            if vProjSanityCheck vprojPath
                then visualise vprojPath 
                else exitCodes.["VisualisationError"]
        | _ -> 
            printf "Invalid command! run `verishot --help` for a guide."
            exitCodes.["InvalidCmd"]
