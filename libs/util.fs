module Verishot.Util

open System.IO
open Verishot.CoreTypes

let print x = printfn "%A" x

let readFileToString (filename: string): string =
    File.ReadAllLines(filename)
    |> String.concat "\n"

let writeStringToFile (filename: string) (content: string) =
    File.WriteAllText(filename, content)

/// Get the number of bits a Range represents
let rangeWidth range =
    match range with
    | Range(high, low) -> abs(high - low) + 1
    | Single -> 1

/// Keep the number of bits the same but make range start at 0
let moveRangeToBase range =
    match range with
    | Single -> Single
    | _ -> Range(rangeWidth range - 1, 0)

module List =
    let catOptions (xs: 'a option list): 'a list =
        xs
        |> List.collect (function
            | Some item -> [ item ]
            | None -> [])

module Map =
    /// Join 2 maps, preferring values from the first map on conflict
    let joinLeft (map1: Map<'k, 'v>) (map2: Map<'k, 'v>): Map<'k, 'v> =
        Map.fold (fun acc key value -> Map.add key value acc) map1 map2

    /// Join 2 maps, preferring values from the first map on conflict
    let joinRight (map1: Map<'k, 'v>) (map2: Map<'k, 'v>): Map<'k, 'v> = joinLeft map2 map1

    let mapValues (f: 'a -> 'b) (map: Map<'k, 'a>) = map |> Map.map (fun _ v -> f v)
