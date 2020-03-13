// Learn more about F# at http://fsharp.org

open System
open System.IO
open Verishot.Util
open Verishot.Parser
open Verishot.Netlist
open Verishot.Visualise

let exitCodes: Map<string, int> = 
    Map [
        ("Success", 0)
        ("InvalidCmd", 1)

        // LINT
        ("LintError", 10)
    ]

let lintHelper filePath =
    filePath
    |> readFileToString
    |> Seq.toList
    |> ParseSource
   
let lint filePath intellisense =
    filePath
    |> lintHelper
    |> function
        | Ok _ ->
            match intellisense with 
            | true -> ()
            | false -> printf "Verishot Lint: No Errors"
            exitCodes.["Success"]
        | Error (errStr, loc) -> 
            match intellisense with 
            | true -> printf "%d#####%d#####%s" loc.line loc.character errStr
            | false -> printf "Lint Error: %s at Line %d, Char %d" errStr loc.line loc.character
            exitCodes.["LintError"]

let checkModulesExist allModules workspacePath = 
    let getModuleFilePathPair moduleFileName = moduleFileName, File.Exists <| workspacePath + dirSlash + moduleFileName

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
            printf "ERROR: Lint error in module `%s`: %s\n" modName lintErr 
            false

    allModules
    |> List.map (getModuleFilePathPair >> logLintError)
    |> List.reduce (&&)

let vProjSanityCheck vProjFilePath =
    let allModules = readFileToStringList vProjFilePath
    let workspacePath = getFolderPath vProjFilePath

    checkModulesExist allModules workspacePath && checkModulesLint allModules workspacePath
   

// let getAST filePath =
//     filePath 
//     |> readFileToString
//     |> Seq.toList
//     |> ParseSource
//     |> function 
//         | Ok ast -> ast 
//         | _ -> failwith "Fatal Error" // should not happen as we've linted

// let visualise filePath workspacePath = 
//     let success = exitCodes.["Success"]
//     let lintErr = exitCodes.["LintError"]
//     match lint filePath with
//     | success ->        
//         filePath
//         |> getAST 
//         |> failwith "TODO"
//     | lintErr -> exitCodes.["LintError"]

    
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
        | "--lint" | "--intellisense" when argv.Length = 2 ->
            let filePath = argv.[1]
            lint filePath (argv.[0] = "--intellisense")
        | "--simulate" when argv.Length = 2 -> 
            let vprojPath = argv.[1]
            if vProjSanityCheck vprojPath then failwith "TODO1" else failwith "TODO2"
        | "--visualise" when argv.Length = 2 -> 
            let vprojPath = argv.[1]
            if vProjSanityCheck vprojPath then failwith "TODO1" else failwith "TODO2"
        | _ -> 
            printf "Invalid command! run `verishot --help` for a guide."
            exitCodes.["InvalidCmd"]
