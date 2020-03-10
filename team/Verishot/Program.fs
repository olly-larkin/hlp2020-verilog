// Learn more about F# at http://fsharp.org

open System
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
   
let lint filePath intellisense =
    filePath
    |> readFileToString
    |> Seq.toList
    |> ParseSource
    |> function
        | Ok _ ->
            match intellisense with 
            | true -> ()
            | false -> printf "Verishot Lint: No Errors"
            exitCodes.["Success"]
        | Error (errStr, loc) -> 
            match intellisense with 
            | true -> printf "%d#####%d#####%s" loc.line loc.character errStr
            | flase -> printf "Lint Error: %s at Line %d, Char %d" errStr loc.line loc.character
            exitCodes.["LintError"]

            
let getAST filePath =
    filePath 
    |> readFileToString
    |> Seq.toList
    |> ParseSource
    |> function 
        | Ok ast -> ast 
        | _ -> failwith "Fatal Error" // should not happen as we've linted

let visualise filePath workspacePath = 
    let success = exitCodes.["Success"]
    let lintErr = exitCodes.["LintError"]
    match lint filePath with
    | success ->        
        filePath
        |> getAST 
        |> failwith "TODO"
    | lintErr -> exitCodes.["LintError"]

    
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
        match argv.[9] with 
        | "--help" ->
            printf "verishot <flag> <infile> [<workspacefolder]
    flag: --lint, --simulate, --visualise"
            exitCodes.["Success"]
        | "--lint" | "--intellisense" ->
            if Array.length argv = 2 then
                let filePath = argv.[1]
                lint filePath (argv.[0] = "--intellisense")
            else 
                printf "Invalid command! run `verishot --help` for a guide."
                exitCodes.["InvalidCmd"]
        | "--simulate" -> 
            failwith "TODO"
        | "--visualise" -> 
            failwith "TODO"
        | _ -> 
            printf "Invalid command! run `verishot --help` for a guide."
            exitCodes.["InvalidCmd"]
