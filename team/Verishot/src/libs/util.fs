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

/// The number of bits required to represent a certain *positive* value
let valueWidth value = int(Math.Log(float value, 2.)) + 1

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

    let toMap = Map.ofList

    let rec skipWhile2 predicate =
        function
        | hd1 :: hd2 :: tl when predicate hd1 hd2 -> skipWhile2 predicate (hd2::tl)
        | lst -> lst
       
    let rec safeSkip count =
        function
        | _ :: tl when count > 0 -> tl |> safeSkip (count - 1)
        | lst -> lst


module Map =
    /// Join 2 maps, preferring values from the first map on conflict
    let joinLeft (map1: Map<'k, 'v>) (map2: Map<'k, 'v>): Map<'k, 'v> =
        Map.fold (fun acc key value -> Map.add key value acc) map1 map2

    /// Join 2 maps, preferring values from the first map on conflict
    let joinRight (map1: Map<'k, 'v>) (map2: Map<'k, 'v>): Map<'k, 'v> =
        joinLeft map2 map1

    let mapValues (f: 'a -> 'b) (map: Map<'k, 'a>) =
        map |> Map.map (fun _ v -> f v)

    let mapKeys (f: 'k -> 'l) (map: Map<'k, 'v>): Map<'l, 'v> =
        (Map.empty, map) ||> Map.fold (fun acc k v -> acc |> Map.add (f k) v)

    // Update the value for a key in a Map. If the key does not exist
    // it gets created with the value `f None`
    let update (key: 'k) (f: 'v option -> 'v) (map: Map<'k, 'v>): Map<'k, 'v> =
        let newVal = f (Map.tryFind key map)
        map |> Map.add key newVal

    let collect (f: 'K -> 'V -> 'U list) (map: Map<'K, 'V>): 'U list =
        map
        |> Map.toList
        |> List.collect (fun (k, v) -> f k v)

    let ofListAll (lst: ('K * 'V) list): Map<'K, 'V list> =
        (Map.empty, lst)
        ||> List.fold (fun map (key, item) ->
                map
                |> update key (function
                       | Some items -> item :: items
                       | None -> [ item ]))

module Tuple =
    let bimap f g (a, b) = (f a, g b)
