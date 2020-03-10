// Learn more about F# at http://fsharp.org

open System

let exitCodes: Map<string, int> = 
    Map [
        ("Success", 0)
        ("InvalidCmd", 1)
    ]

let help =
    printfn "verishot <filePath> <flag> <outputPath>"

let throwInvalidCmd = 
    printfn "Invalid command: run `verishot --help` for a guide."


[<EntryPoint>]
let main argv =

    (*
        verishot <filePath> <flag> <outputPath>
        <flag>: --simulate or --visualise
    *)

    match Array.length argv with
    | 1 when argv.[0] = "--help" -> 
        help
        exitCodes.["Success"]
    | 3 -> 
        let filePath = argv.[0]
        let flag = argv.[1]
        let workspacePath = argv.[2]

        match flag with 
        | "--simulate" -> 
            failwith "TODO"
        | "--visualise" ->
            failwith "TODO"
        | _ -> 
            throwInvalidCmd

        exitCodes.["Success"]
    | _ -> 
        throwInvalidCmd
        exitCodes.["InvalidCmd"]
