module Verishot.Util

open System
open System.IO
open Verishot.CoreTypes

let print x = printfn "%A" x

/// concats two string with a new line
let (+@) strA strB = sprintf "%s\n%s" strA strB 

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

    /// Map a partial function onto a list. Only return a Some if
    /// the function returns Some for at least one element
    let confirmedMap (f: 'a -> 'a option) (lst: 'a list): 'a list option =
        let newLst = List.map f lst

        if List.exists Option.isSome newLst then
            List.zip lst newLst
            |> List.map (function
                | _, Some el -> el
                | el, None -> el)
            |> Some
        else
            None

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

    // Update the value for a key in a Map. If the key does not exist
    // it gets created with the value `f None`
    let update (key: 'k) (f: 'v option -> 'v) (map: Map<'k, 'v>): Map<'k, 'v> =
        let newVal = f (Map.tryFind key map)
        map |> Map.add key newVal

module Tuple =
    let bimap f g (a, b) = (f a, g b)
