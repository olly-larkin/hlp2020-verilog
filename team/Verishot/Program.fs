// Learn more about F# at http://fsharp.org

open System
open System.IO
open Verishot.FrontEnd
open Verishot.Project

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

        | "--new-project" when argv.Length = 3 ->
            let workspacePath = argv.[1]
            let projectName = argv.[2]
            createNewProject workspacePath projectName

        | "--new-module" when argv.Length = 4 -> 
            let workspacePath = argv.[1]
            let projectName = argv.[2]
            let moduleName = argv.[3]
            createNewModule workspacePath projectName moduleName

        | "--delete-module" when argv.Length = 4->
            let workspacePath = argv.[1]
            let projectName = argv.[2]
            let moduleName = argv.[3]
            deleteModule workspacePath projectName moduleName

        | "--list-modules" when argv.Length = 3 ->
            let workspacePath = argv.[1]
            let projectName = argv.[2]
            listModules workspacePath projectName

        | "--lint" when argv.Length = 2 ->
            let filePath = argv.[1]
            lint filePath

        | "--intellisense" when argv.Length = 2 -> 
            let filePath = argv.[1]
            intellisense filePath

        | "--simulate" when argv.Length = 2 -> 
            let vprojPath = argv.[1]
            if vProjSanityCheck vprojPath 
                then failwith "TODO1" 
                else exitCodes.["SimulationError"]

        | "--visualise" when argv.Length = 2 -> 
            let vprojPath = argv.[1]
            if vProjSanityCheck vprojPath
                then visualise vprojPath 
                else exitCodes.["VisualisationError"]

        | _ -> 
            printf "Invalid command! run `verishot --help` for a guide."
            exitCodes.["InvalidCmd"]

