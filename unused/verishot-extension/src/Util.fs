module VerishotExtension.Util

open Fable.Import
open Fable.Core

[<Import("*", from="fs")>]
let fs: Node.Fs.IExports = jsNative

type VerishotMode = 
    | Lint
    | Simulate
    | Visualise

let readLinesFromFile (filePath: string): string list option =
    match fs.existsSync (U2.Case1 filePath) with 
    | true -> 
        let inp: string = fs.readFileSync (filePath, "utf-8")
        inp.Split [| '\n' |]
        |> Array.toList
        |> Some
    | false -> None

let deleteFIleIfExists (filePath: string) =
    if fs.existsSync (U2.Case1 filePath) then fs.unlinkSync (U2.Case1 filePath)

let getFileExtension (fileName: string) =
    fileName.[fileName.LastIndexOf "." + 1 ..]

let stripFileExtension (fileName: string) =
    fileName.[.. fileName.LastIndexOf "."]

let getProjectFiles (workspacePath: string) =
    workspacePath
    |> U2.Case1
    |> fs.readdirSync
    |> Seq.toList
    |> List.filter (fun x -> (getFileExtension x = "vproj"))

let getProjectName (workspacePath: string): string option=
    let projectFiles = getProjectFiles workspacePath

    match List.length projectFiles with 
    | 0 -> 
        vscode.window.showErrorMessage "Create a new project first" |> ignore
        None
    | 1 ->
        projectFiles
        |> List.head 
        |> stripFileExtension
        |> Some
    | _ ->
        vscode.window.showErrorMessage "More than one project file exists" |> ignore
        None

let checkFilePath (filePath: string option, suppress: bool): bool =
    match filePath with 
    | Some uFilePath -> 
        match getFileExtension uFilePath = "v" with 
        | true -> true
        | _ -> 
            if not suppress then vscode.window.showErrorMessage "Only `.v` Verilog files are allowed" |> ignore
            false
    | None ->
        vscode.window.showErrorMessage "No active file found. Please open a `.v` Verilog file" |> ignore
        false