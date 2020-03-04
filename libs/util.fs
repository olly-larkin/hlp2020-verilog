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
    | Range(high, low) -> abs (high - low) + 1
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

    /// Map a function onto a list, with an additional 'confirmation' result.
    /// Return the resulting list, as well as a bool for whether any of the
    /// applications of `f` returned true
    /// Useful for functions that only change certain elements on a list
    let confirmedMap (f: 'a -> ('b * bool)) (lst: 'a list): 'b list * bool =
        (false, lst)
        ||> List.mapFold (fun flag el ->
                let (result, thisFlag) = f el
                (result, flag || thisFlag))

    /// Works like List.tryFind, except it also returns a list without the element found
    /// if it is found.
    let tryFindAndRemove (pred: 'a -> bool) (lst: 'a list): 'a option * 'a list =
        let rec go acc lst =
            match (acc, lst) with
            | ((None, prev), (hd :: tl)) when pred hd -> (Some hd, prev @ tl)
            | ((None, prev), (hd :: tl)) -> go (None, prev @ [ hd ]) tl
            | ((Some el, prev), rest) -> (Some el, prev @ rest)
            | (_, []) -> acc

        go (None, []) lst

    let splitBy (pred: 'a -> bool) (lst: 'a list): 'a list * 'a list =
        // There's only 2 elements in this list, one for true, one for false
        let grouped: (bool * 'a list) list = List.groupBy pred lst

        // The find will choose only the element we want and we can then
        // throw away the first part of the tuple which is just the bool
        let trueElems =
            grouped
            |> List.tryFind (fun (p, _) -> p)
            |> Option.defaultValue (true, [])
            |> snd

        let falseElems =
            grouped
            |> List.tryFind (fun (p, _) -> not p)
            |> Option.defaultValue (false, [])
            |> snd

        (trueElems, falseElems)


module Map =
    /// Join 2 maps, preferring values from the first map on conflict
    let joinLeft (map1: Map<'k, 'v>) (map2: Map<'k, 'v>): Map<'k, 'v> =
        Map.fold (fun acc key value -> Map.add key value acc) map1 map2

    /// Join 2 maps, preferring values from the first map on conflict
    let joinRight (map1: Map<'k, 'v>) (map2: Map<'k, 'v>): Map<'k, 'v> =
        joinLeft map2 map1

    let mapValues (f: 'a -> 'b) (map: Map<'k, 'a>) =
        map |> Map.map (fun _ v -> f v)
