module Verishot.Main

open Verishot.FrontEnd
open Verishot.Project

let intro = "Verishot Verilog Visualiser and Simulator v0.0.1
(C) 2020 Imperial College London; lhl2617, ng2517, mp5617, oll16
-----------------------
`verishot --help` for usage guide"

let usageGuide = "help todo"

let invalidCmd = "Invalid command. Run `verishot --help` for a usage guide. "

let argProcessor argv: CmdResult =
    match Array.length argv with 
    | 0 -> 
        Ok intro
    | _ ->
        match argv.[0] with 
        | "--help" ->
            Ok usageGuide

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
            simulate vProjFilePath

        | "--visualise" when argv.Length = 2 -> 
            let vProjFilePath = argv.[1]
            visualise vProjFilePath 

        | _ -> 
            Error (exitCodes.InvalidCmd, invalidCmd)


[<EntryPoint>]
let main argv =
    match argProcessor argv with 
    | Ok (stdout) -> 
        printf "%s" stdout
        exitCodes.Success
    | Error (exitCode, stdout) ->
        printf "%s" stdout
        exitCode
        