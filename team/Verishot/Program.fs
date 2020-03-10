﻿// Learn more about F# at http://fsharp.org

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
   
let lint filePath =
    filePath
    |> readFileToString
    |> Seq.toList
    |> ParseSource
    |> function
        | Ok _ -> 
            printf "Lint: No Errors"
            exitCodes.["Success"]
        | Error (errStr, loc) -> 
            printf "Lint Error: %s at Line %d, Char %d" errStr loc.line loc.character
            exitCodes.["LintError"]

let intellisense code =
    code 
    |> Seq.toList
    |> ParseSource
    |> function
        | Error (errStr, loc) -> 
            printf "%d;%d;%s" loc.line loc.character errStr
            exitCodes.["LintError"]
        | _ ->
            exitCodes.["Success"]
            
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
    | 1 when argv.[0] = "--help" -> 
        printf "verishot <flag> <infile> [<workspacefolder]
flag: --lint, --simulate, --visualise"
        exitCodes.["Success"]
    | 2 when argv.[0] = "--lint" ->
        let filePath = argv.[1]
        lint filePath
    | 2 when argv.[0] = "--intellisense" -> 
        let code = argv.[1]
        intellisense code
    | 3 -> 
        let flag = argv.[0]
        let filePath = argv.[1]
        let workspacePath = argv.[2]
        match flag with 
        | "--simulate" -> 
            failwith "TODO"
        | "--visualise" -> 
            visualise filePath workspacePath
        | _ -> 
            printf "Invalid command! run `verishot --help` for a guide."
            exitCodes.["InvalidCmd"]
    | _ -> 
        printf "Invalid command! run `verishot --help` for a guide."
        exitCodes.["InvalidCmd"]
