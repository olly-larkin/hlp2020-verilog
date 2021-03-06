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

    let megaDecl = 
        megafunctions
        |> Map.toList
        |> List.map (fun (_, func) ->
            match func with
            | NetlistInstance _ -> failwith "Unexpected netlist in megafunction map"
            | Megafunction (Stateful mf) -> mf.declaration
            | Megafunction (Combinational mf) -> mf.declaration)

    let decls = asts |> List.map (ASTToDecl) |> (@) megaDecl

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

/// Params for vIn. (port * defualtValue * top comment)
let vInParams =
    [
        ("__CYCLES__", Single), 10, "// Specify how many cycles to simulate below"
        ("__BREAK_DOWN_BUSSES__", Single), 0, "// Specify (0 or 1) if you would like busses to be broken down"
    ]

let matchMapWithInputPorts
    (varMap: VarMapType)
    (inputPorts: (Identifier * Range) list)
    outputPorts
    =
    // add in required params and outputPorts

    let inputPortsWithCycles = (vInParams |> List.map (fst3)) @ outputPorts @ inputPorts

    let folder (passed, stdout) (port: Identifier * Range) =
        match varMap.ContainsKey port with
        | true -> passed, stdout
        | _ ->
            /// this is not helpful - just prompting the user to check .vin is more succinctly descriptive

            // let stdout2 =
            //     sprintf "Simulator input `%s%s` invalid or not found."
            //         (inputPort |> fst)
            //         (inputPort
            //          |> snd
            //          |> getRangeStr)
            // let stdout' = stdout +@ stdout2
            false, stdout

    ((true, ""), inputPortsWithCycles)
    ||> List.fold (folder)
    |> function
    | true, stdout -> Ok stdout
    | false, stdout -> Error(exitCodes.vInError, stdout)


let getVinContent (inputPorts: (Identifier * Range) list) (outputPorts: (Identifier * Range) list) (varMap: VarMapType) =
    let getStr x =
        match varMap.ContainsKey(x) with
        | true -> Some varMap.[x].str
        | _ -> None

    let header = "// ===== Verishot Simulation File =====\n"

    /// get the params lines
    let paramsContent = 
        vInParams
        |> List.map ((fun (port, defaultVal, comment) -> 
            let outStr = 
                match getStr port with 
                | Some str -> str
                | None -> sprintf "%s=%d;" (fst port) defaultVal
            comment +@ outStr)
        )
        |> String.concat "\n\n"

    // this is to let user specify if they want to see an output or not
    let outputContent = 
        match outputPorts.IsEmpty with
        | true -> ""
        | _ -> 
            "\n// Specify (0 or 1) whether to view these outputs in the waveform" +@
            (outputPorts
            |> List.map (fun (id, rng) -> 
                match getStr (id, rng) with
                | Some str -> str
                | _ -> sprintf "%s%s=1;" id (getRangeStr rng))
            |> String.concat "\n")
          
    let inputContent =
        match inputPorts.IsEmpty with 
        | true -> ""
        | _ -> 
            "\n// Specify each input on a new line (you may use Verilog style numeric constants)" +@ 
            (inputPorts
            |> List.map (fun (id, rng) -> 
                match getStr (id, rng) with
                | Some str -> str
                | _ -> sprintf "%s%s=;" id (getRangeStr rng))
            |> String.concat "\n")

    header +@ paramsContent +@ outputContent +@ inputContent

let checkVInFile vProjFilePath (inputPorts: (Identifier * Range) list) outputPorts =
    let workspacePath = Directory.GetParent(vProjFilePath).FullName
    let projectName = Path.GetFileNameWithoutExtension vProjFilePath
    let vInFilePath = workspacePath +/ projectName + ".vin"

    let varMap =
        vInFilePath
        |> readVFile
        |> parseAndMap

    match matchMapWithInputPorts varMap inputPorts outputPorts with
    | Ok _ -> Ok varMap
    | Error(exitCode, stdout) ->
        // create the .vin file
        getVinContent inputPorts outputPorts varMap
        |> writeStringToFile vInFilePath

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

    // we do not need to remove __CYCLES__ or other params, even the output, because the varMap is used on demand
    let inputs =
        varMap
        |> Map.mapKeys (fun (id, _) -> id)
        |> Map.mapValues (fun x -> x.wireVal)

    simulateCycles cycles topLevelNetlist otherModulesAndMegafunctions
        initialState inputs |> List.rev
// reversed as the final res is output at the front by simulateCycles

let wireValToSimPort
    (wireValMaps: WireValMap list)
    (outputPorts: list<Identifier * Range>)
    (varMap: VarMapType) // varMap is used to see if we need to actually visualise that waveform
    : SimulatorPort list
    =

    // we get the actually needed ports
    let neededOutputs = 
        outputPorts
        |> List.filter (fun x -> varMap.[x].wireVal <> 0UL)

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

    mapToSimPorts neededOutputs

let simulate vProjFilePath =
    // first we need to parse the top level module
    // so that we know what inputs can be specified.
    // then we check a `.vin` file
    // which specifies the number of clock cycles
    // and the input values

    match vProjSanityCheck vProjFilePath with
    | Ok stdout ->
        let inputPorts, outputPorts = getTopLevelPorts vProjFilePath

        match checkVInFile vProjFilePath inputPorts outputPorts with
        | Ok varMap ->
            // do the necessary processing and then pass it to Simulate API

            let wireValMaps = simulateHelper vProjFilePath varMap
            let simPorts = wireValToSimPort wireValMaps outputPorts varMap

            let props = { breakDownBusses=varMap.[("__BREAK_DOWN_BUSSES__", Single)].wireVal <> 0UL }
            let waveOut = waveformMain simPorts props

            let workspacePath = Directory.GetParent(vProjFilePath).FullName
            let waveOutputPath = workspacePath +/ "simulation" +/ "output.svg"

            deleteFile waveOutputPath
            createPathFolder (workspacePath +/ "simulation")

            writeStringToFile waveOutputPath waveOut

            let stdout' = stdout +@ "Simulation succeeded. View output in `simulation`. "
            Ok stdout'
        | Error x -> Error x
    | Error x -> Error x
