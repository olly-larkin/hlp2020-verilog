(* front end helper functions for CLI interface *)
module Verishot.FrontEnd

open System.IO
open Verishot.Util
open Verishot.FileUtil
open Verishot.Parser
open Verishot.ASTToDecl
open Verishot.Netlist
open Verishot.Visualise

let exitCodes: Map<string, int> = 
    Map [
        ("Success", 0)
        ("InvalidCmd", 1)
        ("NewProjectError", 2)
        ("NewModuleError", 3)
        ("DeleteModuleError", 4)
        ("SanityCheckError", 5)

        // LINT
        ("LintError", 10)

        // VISUALISE
        ("VisualisationError", 20)

        // SIMULATION
        ("SimulationError", 30)

    ]
    

let getModuleFilePaths vProjFilePath =
    vProjFilePath
    |> readVFile
    |> List.map ((+/) (getFolderPath vProjFilePath))

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
    let getModuleFilePathPair moduleFileName = moduleFileName, File.Exists <| workspacePath +/ moduleFileName

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

    let getModuleFilePathPair moduleFileName = moduleFileName, linter <| workspacePath +/ moduleFileName

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
    let allModules = readVFile vProjFilePath
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
    let visOutputPath = workspacePath +/ "visualisation"

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

    let getOutputFilePath id = (visOutputPath +/ id) + ".svg"

    (netlists, decls)
    ||> visualiseNetlists
    |> List.map (fun (id, svgOutput) -> writeStringToFile (getOutputFilePath id) svgOutput)
    |> ignore

    printf "Visualisation succeeded. View output in `visualisation`."
    exitCodes.["Success"]

    