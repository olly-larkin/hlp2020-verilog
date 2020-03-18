(*
    Front end code for CLI interface
    ================================
    Author: lhl2617
*)

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
open Verishot.FrontEndParser
open Verishot.ParserUtils

type ExitCode = int
type StdOut = string
type CmdError = ExitCode 
type CmdResult = Result<StdOut, ExitCode * StdOut>

/// exit codes for CLI interface
let exitCodes = 
    {|
        Success                 = 0
        InvalidCmd              = 1
        NewProjectError         = 2
        NewModuleError          = 3
        DeleteModuleError       = 4
        SanityCheckError        = 5

        // LINT
        LintError               =10

        // VISUALISE
        VisualisationError      =20

        // SIMULATION
        SimulationError         =30
        vInError                =31
    |}

  
    

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
            Ok ""
        | Error (errStr, loc) ->
            let exitCode = exitCodes.LintError
            let stdout = sprintf "%d ----- %d ----- %s" loc.line loc.character errStr
            Error (exitCode, stdout)

let lint filePath =
    filePath
    |> lintHelper
    |> function
        | Ok _ ->
            let stdout = "Verishot Lint: No Errors"
            Ok stdout
        | Error (errStr, loc) -> 
            let exitCode = exitCodes.LintError
            let stdout = sprintf "Lint Error: %s at Line %d=Char %d" errStr loc.line loc.character
            Error (exitCode, stdout)

let checkModulesExist allModules workspacePath = 
    let folder (passed, stdout) (modName, exists) = 
        match exists with
        | true -> 
            passed, stdout
        | false -> 
            let stdout2 = sprintf "Module `%s` not found in folder" modName
            let stdout' = stdout +@ stdout2
            false, stdout'

    let modFileExists = 
        allModules
        |> List.map (fun modFileName -> modFileName, File.Exists <| workspacePath +/ modFileName)

    ((true, ""), modFileExists)
    ||> List.fold (folder)
    |> function
        | true, stdout -> Ok stdout
        | _, stdout -> Error (exitCodes.SanityCheckError, stdout)
                
let checkModulesLint allModules workspacePath =
    let folder (passed, stdout) (modFileName, filePath) =
        filePath 
        |> lintHelper
        |> function 
            | Ok _ -> 
                passed, stdout
            | Error (errStr, loc) ->
                let stdout2 = sprintf "Lint Error for module `%s`: %s at Line %d=Char %d\n" modFileName errStr loc.line loc.character
                let stdout' = stdout +@ stdout2
                false, stdout'

    let modFilePaths =
        allModules
        |> List.map (fun modFileName -> modFileName, workspacePath +/ modFileName)

    ((true, ""), modFilePaths)
    ||> List.fold (folder)  
    |> function
        | true, stdout -> Ok stdout
        | false, stdout -> Error (exitCodes.SanityCheckError, stdout)

let vProjSanityCheck vProjFilePath =
    let allModules = readVFile vProjFilePath
    let workspacePath = Directory.GetParent(vProjFilePath).FullName

    match checkModulesExist allModules workspacePath with
    | Ok stdout1 ->
        match checkModulesLint allModules workspacePath with
        | Ok stdout2 ->
            let stdout' = stdout1 +@ stdout2
            Ok stdout'
        | Error x -> Error x
    | Error x -> Error x

/// Only call this function after linting 
let getAST moduleFilePath =
    moduleFilePath
    |> readFileToString
    |> Seq.toList
    |> ParseSource
    |> function
        | Ok ast -> ast 
        | _ -> failwith "Lint error"  // linted before=should never happen

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
    match vProjSanityCheck vProjFilePath with 
    | Ok stdout ->
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

        let stdout' = stdout +@ "Visualisation succeeded. View output in `visualisation`."
        Ok stdout'
    | Error x -> Error x


/// parse the vars and put them into a map
let parseAndMap (vars: string list): Map<Identifier * Range, int64> = 
    let unwrap (x: ParserResult<(string * Range) * int64>) = 
        match x with
        | Ok (((id, rng), value), _, _) -> Some ((id, rng), value)
        | Error _ -> None // treat invalid as not found
    
    vars
    |> List.map (List.ofSeq >> vInAssignmentParser >> unwrap)
    |> List.choose id
    |> Map.ofList

/// will return false if required inputs in inputPorts are not specified
let matchMapWithInputPorts (varMap: Map<Identifier * Range, int64>) (inputPorts: (Identifier * Range) list) = 
    // add in __CYCLES__
    let inputPortsWithCycles = ("__CYCLES__", Single) :: inputPorts
    
    let folder (passed, stdout) (inputPort: Identifier * Range) = 
        match varMap.ContainsKey(inputPort) with 
        | true ->
            passed, stdout
        | _ ->
            let stdout2 = sprintf "Simulator input `%s%s` invalid or not found." (inputPort |> fst) (inputPort |> snd |> getRangeStr)
            let stdout' = stdout +@ stdout2
            false, stdout'

    ((true, ""), inputPortsWithCycles)
    ||> List.fold (folder)
    |> function
        | true, stdout -> Ok stdout
        | false, stdout -> Error (exitCodes.vInError, stdout)

let createVInFile vInFilePath (inputPorts: (Identifier * Range) list) =
    let vInFileHeader = "// ===== Verishot Simulation File =====
// Specify the number of clock cycles to simulate below:

__CYCLES__=100;

// Specify each input on a new line
"
    let inputContent = 
        inputPorts
        |> List.map (fun (id, rng) -> sprintf "%s%s=;" id (getRangeStr rng))
        |> String.concat "\n"
    let vInFileContent = vInFileHeader + inputContent
    writeStringToFile vInFilePath vInFileContent

let checkVInFile vProjFilePath (inputPorts: (Identifier * Range) list) = 
    let workspacePath = Directory.GetParent(vProjFilePath).FullName
    let projectName = Path.GetFileNameWithoutExtension vProjFilePath
    let vInFilePath = workspacePath +/ projectName + ".vin"
    let varMap = vInFilePath |> readVFile |> parseAndMap

    match matchMapWithInputPorts varMap inputPorts with 
    | Ok stdout -> Ok stdout
    | Error (exitCode, stdout) ->
        createVInFile vInFilePath inputPorts
        let stdout2 = sprintf "Please respecify your inputs in `%s.vin`." projectName 
        let stdout' = stdout +@ stdout2
        Error (exitCode, stdout')


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
    | Ok stdout -> 
        // do the necessary processing and then pass it to Simulate API
        let netlists, decls = getNetlistsAndDecls vProjFilePath

        let stdout' = stdout +@ "Simulation succeeded. View output in `simulation`."
        Ok stdout'
    | Error x -> Error x