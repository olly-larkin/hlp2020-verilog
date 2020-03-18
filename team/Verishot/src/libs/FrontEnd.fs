(* front end helper functions for CLI interface *)
module Verishot.FrontEnd

open System.IO
open Verishot.Util
open Verishot.FileUtil
open Verishot.Parser
open Verishot.ASTToDecl
open Verishot.Netlist
open Verishot.Visualise
open Verishot.CoreTypes
open Verishot.VisualiserUtil.Functions

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
        ("vInError", 31)

    ]
    

let getExistingModuleNamesFromVProj vProjFilePath =
    vProjFilePath
    |> readVFile
    |> List.map (Path.GetFileNameWithoutExtension)

let getModuleFilePaths vProjFilePath =
    vProjFilePath
    |> readVFile
    |> List.map ((+/) (Directory.GetParent(vProjFilePath).FullName))

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
    let workspacePath = Directory.GetParent(vProjFilePath).FullName

    checkModulesExist allModules workspacePath && checkModulesLint allModules workspacePath

(* Only call this function after linting *)
let getAST moduleFilePath =
    moduleFilePath
    |> readFileToString
    |> Seq.toList
    |> ParseSource
    |> function
        | Ok ast -> ast 
        | _ -> failwith "Lint error"  // linted before, should never happen

let getNetlistsAndDecls vProjFilePath =
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

    netlists, decls

let visualise vProjFilePath =
    let netlists, decls = getNetlistsAndDecls vProjFilePath
    
    let workspacePath = Directory.GetParent(vProjFilePath).FullName
    let visOutputPath = workspacePath +/ "visualisation"
    deleteFolder visOutputPath
    createPathFolder visOutputPath

    let getOutputFilePath id = visOutputPath +/ id + ".svg"

    (netlists, decls)
    ||> visualiseNetlists
    |> List.map (fun (id, svgOutput) -> writeStringToFile (getOutputFilePath id) svgOutput)
    |> ignore

    printf "Visualisation succeeded. View output in `visualisation`."
    exitCodes.["Success"]


// parse the vars and put them into a map
let parseAndMap (vars: string list): Map<Identifier * Range, int64> = 

    failwith "TODO"

// will return false if required inputs in inputPorts are not specified
let matchMapWithInputPorts (varMap: Map<Identifier * Range, int64>) (inputPorts: (Identifier * Range) list): bool = 
    inputPorts
    |> List.map (fun x -> varMap.ContainsKey(x))
    |> List.reduce (&&)

let createVInFile vInFilePath (inputPorts: (Identifier * Range) list) =
    let vInFileHeader = "// ===== Verishot Simulation File =====
// Specify the number of clock cycles to simulate below:

__CYCLES__=100

// Specify each input on a new line\n"
    let inputContent = 
        inputPorts
        |> List.map (fun (id, rng) -> sprintf "%s%s=" id (getRangeStr rng))
        |> String.concat "\n"
    let vInFileContent = vInFileHeader + inputContent
    writeStringToFile vInFilePath vInFileContent

let checkVInFile vProjFilePath (inputPorts: (Identifier * Range) list) = 
    let workspacePath = Directory.GetParent(vProjFilePath).FullName
    let vInFilePath = workspacePath +/ (Path.GetFileNameWithoutExtension vProjFilePath) + ".vin"
    let varMap = vInFilePath |> readVFile |> parseAndMap
    match matchMapWithInputPorts varMap inputPorts with 
    | true -> 
        true
    | false ->
        createVInFile vInFilePath inputPorts
        false


let getTopLevelInputPorts vProjFilePath =
    let workspacePath = Directory.GetParent(vProjFilePath).FullName
    let topLevelModuleFilePath = 
        vProjFilePath
        |> getExistingModuleNamesFromVProj
        |> List.head
        |> fun x -> workspacePath +/ x + ".v"
    let topLevelModuleDecl =
        topLevelModuleFilePath
        |> getAST
        |> ASTToDecl
    topLevelModuleDecl.ports
    |> List.filter (fun (x, _, _)-> x = Input) 
    |> List.map (fun (_, x, y) -> x, y)


let simulate vProjFilePath =
    // first we need to parse the top level module 
    // so that we know what inputs can be specified.
    // then we check a `.vin` file
    // which specifies the number of clock cycles 
    // and the input values
    
    let topLevelInputPorts = getTopLevelInputPorts vProjFilePath
    match checkVInFile vProjFilePath topLevelInputPorts with 
    | true -> 
        // do the necessary processing and then pass it to Simulate API
        let netlists, decls = getNetlistsAndDecls vProjFilePath

        failwith "TODO"
    | false ->
        let projectName = Path.GetFileNameWithoutExtension vProjFilePath
        printf "Please specify your inputs in `%s.vin`." projectName 
        exitCodes.["vInError"]     