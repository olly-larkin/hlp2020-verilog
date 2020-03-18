// Learn more about F# at http://fsharp.org

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

        | "--new-module" when argv.Length = 3 -> 
            let vProjFilePath = argv.[1]
            let moduleName = argv.[2]
            createNewModule vProjFilePath moduleName

        | "--delete-module" when argv.Length = 3 ->
            let vProjFilePath = argv.[1]
            let moduleName = argv.[2]
            deleteModule vProjFilePath moduleName

        | "--list-modules" when argv.Length = 2 ->
            let vProjFilePath = argv.[1]
            listModules vProjFilePath

        | "--lint" when argv.Length = 2 ->
            let filePath = argv.[1]
            lint filePath

        | "--intellisense" when argv.Length = 2 -> 
            let filePath = argv.[1]
            intellisense filePath

        | "--simulate" when argv.Length = 2 -> 
            let vProjFilePath = argv.[1]
            if vProjSanityCheck vProjFilePath 
                then simulate vProjFilePath
                else exitCodes.["SanityCheckError"]

        | "--visualise" when argv.Length = 2 -> 
            let vProjFilePath = argv.[1]
            if vProjSanityCheck vProjFilePath
                then visualise vProjFilePath 
                else exitCodes.["SanityCheckError"]

        | _ -> 
            printf "Invalid command! run `verishot --help` for a guide."
            exitCodes.["InvalidCmd"]

