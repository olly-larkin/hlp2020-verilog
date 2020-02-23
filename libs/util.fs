module Verishot.Util

open System.IO

let print x = printfn "%A" x

module List =
    let catOptions (xs: 'a option list): 'a list =
        xs
        |> List.collect (function
            | Some item -> [ item ]
            | None -> [])

let readFileToString (filename: string): string =
    File.ReadAllLines(filename)
    |> String.concat "\n"

let writeStringToFile (filename: string) (content: string) =
    File.WriteAllText(filename, content)
    