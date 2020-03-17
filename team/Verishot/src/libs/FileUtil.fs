module Verishot.FileUtil

open System
open System.IO

let (+/) path1 path2 = Path.Combine(path1, path2)

let readFileToStringList (filename: string) =
    match File.Exists filename with 
    | true ->
        File.ReadAllLines filename
        |> Array.toList
    | false -> 
        failwithf "File `%s` not found." filename

let readFileToString (filename: string) =
    filename
    |> readFileToStringList 
    |> String.concat "\n"

(* clears empty lines and commented (\/\/) lines *)
let readVFile (filename: string): string list = 
    filename
    |> readFileToStringList
    |> List.map (fun x -> x.Trim())
    |> List.filter (String.IsNullOrEmpty >> not)
    |> List.filter (fun x -> x.[.. 1] <> "//")

let createPathFolder (pathString: string) =
    Directory.CreateDirectory pathString |> ignore

let deleteFolder path =
    if Directory.Exists path then Directory.Delete(path, true) |> ignore

let deleteFile (filepath: string) =
    if File.Exists filepath then File.Delete(filepath) |> ignore

let writeStringToFile (filename: string) (content: string) =
    File.WriteAllText (filename, content)

let getFolderPath (filePath: string): string = 
    filePath.[0 .. filePath.LastIndexOf Path.DirectorySeparatorChar - 1]

let getFilenamesInFolderPath (searchPattern) (folderPath: string) = 
    Directory.GetFiles (folderPath, searchPattern) 
    |> Array.map Path.GetFileName 
    |> Array.toList