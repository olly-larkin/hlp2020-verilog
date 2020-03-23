(*
    Front end code for CLI interface
    ================================
    Author: lhl2617
*)

module Verishot.FrontEnd

open System.IO
open Verishot.ASTToDecl
open Verishot.CoreTypes
open Verishot.FileUtil
open Verishot.FrontEndParser
open Verishot.Megafunctions.Registry
open Verishot.Megafunctions.Types
open Verishot.Netlist
open Verishot.Parser
open Verishot.ParserUtils
open Verishot.Simulator.Netlist
open Verishot.Simulator.Simulate
open Verishot.Simulator.Types
open Verishot.Util
open Verishot.Visualise
open Verishot.VisualiserUtil.Functions
open Verishot.Waveform
open WaveTypes

type ExitCode = int

type StdOut = string

type CmdError = ExitCode

type CmdResult = Result<StdOut, ExitCode * StdOut>

type VarMapValStr =
    { wireVal: WireVal
      str: string }

/// Map of key: (InputPortID * range); value: (WireVal * (actual string line)).
/// Actual string line is to aid outputting back again to a new infile
type VarMapType = Map<Identifier * Range, VarMapValStr>

/// exit codes for CLI interface
let exitCodes =
    {| Success = 0
       InvalidCmd = 1
       NewProjectError = 2
       NewModuleError = 3
       DeleteModuleError = 4
       SanityCheckError = 5
       LintError = 10
       VisualisationError = 20
       SimulationError = 30
       vInError = 31 |}


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
    | Ok _ -> Ok ""
    | Error(errStr, loc) ->
        let exitCode = exitCodes.LintError
        let stdout =
            sprintf "%d ----- %d ----- %s" loc.line loc.character errStr
        Error(exitCode, stdout)

let lint filePath =
    filePath
    |> lintHelper
    |> function
    | Ok _ ->
        let stdout = "Verishot Lint: No Errors. "
        Ok stdout
    | Error(errStr, loc) ->
        let exitCode = exitCodes.LintError
        let stdout =
            sprintf "Lint Error: %s at Line %d, Char %d. " errStr loc.line
                loc.character
        Error(exitCode, stdout)

let checkModulesExist allModules workspacePath =
    let folder (passed, stdout) (modName, exists) =
        match exists with
        | true -> passed, stdout
        | false ->
            let stdout2 = sprintf "Module `%s` not found in folder. " modName
            let stdout' = stdout +@ stdout2
            false, stdout'

    let modFileExists =
        allModules
        |> List.map (fun modFileName ->
            modFileName, File.Exists <| workspacePath +/ modFileName)

    ((true, ""), modFileExists)
    ||> List.fold (folder)
    |> function
    | true, stdout -> Ok stdout
    | _, stdout -> Error(exitCodes.SanityCheckError, stdout)

let checkModulesLint allModules workspacePath =
    let folder (passed, stdout) (modFileName, filePath) =
        filePath
        |> lintHelper
        |> function
        | Ok _ -> passed, stdout
        | Error(errStr, loc) ->
            let stdout2 =
                sprintf "Lint Error for module `%s`: %s at Line %d=Char %d. "
                    modFileName errStr loc.line loc.character
            let stdout' = stdout +@ stdout2
            false, stdout'

    let modFilePaths =
        allModules
        |> List.map (fun modFileName ->
            modFileName, workspacePath +/ modFileName)

    ((true, ""), modFilePaths)
    ||> List.fold (folder)
    |> function
    | true, stdout -> Ok stdout
    | false, stdout -> Error(exitCodes.SanityCheckError, stdout)

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
    | _ -> failwith "Lint error" // linted before=should never happen

let getNetlistsAndDecls vProjFilePath =
    let asts =
        vProjFilePath
        |> getModuleFilePaths
        |> List.map getAST

    let decls = asts |> List.map (ASTToDecl)

    let netlists = asts |> List.map (moduleNetlist decls)

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
        |> List.map
            (fun (id, svgOutput) ->
                writeStringToFile (getOutputFilePath id) svgOutput)
        |> ignore

        let stdout' =
            stdout +@ "Visualisation succeeded. View output in `visualisation`."
        Ok stdout'
    | Error x -> Error x


/// parse the vars and put them into a map;
/// Map<Identifier * Range, VarMapValStr>;
/// VarMaoValStr has both the WireVal and the actual string input
/// to help regenerating vIn
let parseAndMap (vars: string list): VarMapType =
    let unwrap (x: ParserResult<(string * Range) * WireVal>) =
        match x with
        | Ok(((id, rng), value), _, _) -> Some((id, rng), value)
        | Error _ -> None // treat invalid as not found

    vars
    |> List.map (fun str ->
        str
        |> List.ofSeq
        |> vInAssignmentParser
        |> unwrap
        |> function
        | Some((id, rng), value) ->
            Some
                ((id, rng),
                 { wireVal = value
                   str = str })
        | _ -> None)
    |> List.choose id
    |> Map.ofList

let matchMapWithInputPorts
    (varMap: VarMapType)
    (inputPorts: (Identifier * Range) list)
    =
    // add in __CYCLES__
    let inputPortsWithCycles = ("__CYCLES__", Single) :: inputPorts

    let folder (passed, stdout) (inputPort: Identifier * Range) =
        match varMap.ContainsKey(inputPort) with
        | true -> passed, stdout
        | _ ->
            let stdout2 =
                sprintf "Simulator input `%s%s` invalid or not found."
                    (inputPort |> fst)
                    (inputPort
                     |> snd
                     |> getRangeStr)
            let stdout' = stdout +@ stdout2
            false, stdout'

    ((true, ""), inputPortsWithCycles)
    ||> List.fold (folder)
    |> function
    | true, stdout -> Ok stdout
    | false, stdout -> Error(exitCodes.vInError, stdout)

let createVInFile vInFilePath (inputPorts: (Identifier * Range) list) (varMap: VarMapType) =
    let getStr x =
        match varMap.ContainsKey(x) with
        | true -> Some varMap.[x].str
        | _ -> None

    let cycleStr =
        match getStr ("__CYCLES__")

    let vInFileHeader = "// ===== Verishot Simulation File =====
// Specify the number of clock cycles to simulate below:

__CYCLES__=;

// Specify each input on a new line (you may use Verilog style numeric constants)
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

    let varMap =
        vInFilePath
        |> readVFile
        |> parseAndMap

    match matchMapWithInputPorts varMap inputPorts with
    | Ok _ -> Ok varMap
    | Error(exitCode, stdout) ->
        // create the .vin file
        createVInFile vInFilePath inputPorts varMap

        let stdout2 =
            sprintf "Please specify your inputs in `%s.vin`. " projectName
        let stdout' = stdout +@ stdout2
        Error(exitCode, stdout')

let getTopLevelPorts vProjFilePath =
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

    let filterPorts dir =
        topLevelModuleDecl.ports
        |> List.filter (fun (x, _, _) -> x = dir)
        |> List.map (fun (_, x, y) -> x, y)

    filterPorts Input, filterPorts Output

let private simulateHelper vProjFilePath (varMap: VarMapType) =

    let netlists, _ = getNetlistsAndDecls vProjFilePath
    let simNetlists = netlists |> List.map (convertNetlist)
    let cycles = varMap.[("__CYCLES__", Single)].wireVal
    let topLevelNetlist = simNetlists.Head

    let otherModules: Map<ModuleIdentifier, StateVar SimulationObject> =
        simNetlists.Tail
        |> List.map
            (fun x -> StringIdentifier x.moduleName, (NetlistInstance x))
        |> Map.ofList

    let otherModulesAndMegafunctions = Map.joinLeft megafunctions otherModules
    let initialState = Map.empty

    let inputs =
        varMap
        |> Map.remove ("__CYCLES__", Single) // need to remove __CYCLES__
        |> Map.mapKeys (fun (id, _) -> id)
        |> Map.mapValues (fun x -> x.wireVal)

    simulateCycles cycles topLevelNetlist otherModulesAndMegafunctions
        initialState inputs |> List.rev
// reversed as the final res is output at the front by simulateCycles

let private wireValToSimPort
    (wireValMaps: WireValMap list)
    (outputPorts: list<Identifier * Range>)
    : SimulatorPort list
    =

    // in a wireValMap list, each elem in a list is a clock cycle
    // we now mutate the structure to be a map containing the identifier of the port
    // and a list of WireVals
    // we make sure inputs are first before output for final readability
    let mapToSimPorts ports =
        ports
        |> List.map (fun (id, rng) ->
            let wireValLst = wireValMaps |> List.map (fun m -> m.[id])
            match rng with
            | Single ->
                // SimWire
                SimWire
                    { portName = id
                      output = wireValLst }
            | Range(a, b) ->
                SimBus
                    { portName = id
                      range = a - b + 1
                      output = wireValLst })

    mapToSimPorts outputPorts

let simulate vProjFilePath =
    // first we need to parse the top level module
    // so that we know what inputs can be specified.
    // then we check a `.vin` file
    // which specifies the number of clock cycles
    // and the input values

    let inputPorts, outputPorts = getTopLevelPorts vProjFilePath

    match checkVInFile vProjFilePath inputPorts with
    | Ok varMap ->
        // do the necessary processing and then pass it to Simulate API

        let wireValMaps = simulateHelper vProjFilePath varMap
        let simPorts = wireValToSimPort wireValMaps outputPorts
        let waveOut = simPorts |> waveformMain

        let workspacePath = Directory.GetParent(vProjFilePath).FullName
        let waveOutputPath = workspacePath +/ "simulation" +/ "output.svg"

        deleteFile waveOutputPath
        createPathFolder (workspacePath +/ "simulation")

        writeStringToFile waveOutputPath waveOut

        let stdout' = "Simulation succeeded. View output in `simulation`. "
        Ok stdout'
    | Error x -> Error x
