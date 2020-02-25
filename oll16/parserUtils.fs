module Verishot.ParserUtils

/// Error type
type Err = string * char list

/// Return the furthest Err type
let furthestErr err1 err2 =
    match err1, err2 with
    | (_, lst1), (_, lst2) when List.length lst1 >= List.length lst2 -> err2
    | _ -> err1

let furthestOpErr err1 err2 =
    match err1, err2 with
    | Some _, None -> err1
    | None, Some _ -> err2
    | Some err1', Some err2' -> Some <| furthestErr err1' err2'
    | _ -> None

/// result of parser needs to contain return element, lineNum, char/token Num, remLst (Can use result -- error doesnt need return element but will have message)
/// Positive result must also contain (as option) the error returned from optional paths
/// This is so that the error can be more accurate
/// Error in optional branch could not be reported otherwise
type ParserResult<'a> = Result<'a * char list * Err option, Err>

/// take as input line num, char num, char list
type Parser<'a> = char list -> ParserResult<'a>

/// Either p1 or p2 (p1 takes precidence)
/// If there was an error, take the one that got furthest (p2 takes precidence if the same)
let (<|>) (p1: Parser<'a>) (p2: Parser<'a>) : Parser<'a> =
    fun inp ->
        match p1 inp with
        | Ok res -> Ok res
        | Error err1 ->
            match p2 inp with
            | Ok res -> Ok res
            | Error err2 -> furthestErr err1 err2 |> Error

/// First apply p1 and then p2
/// Outputs returned in a tuple to be unpacked by user
let (>=>) (p1: Parser<'a>) (p2: Parser<'b>) : Parser<'a * 'b> =
    fun inp ->
        match p1 inp with
        | Ok (a, rest, opErr1) ->
            match p2 rest with
            | Ok (b, rest', opErr2) -> Ok ((a, b), rest', furthestOpErr opErr1 opErr2)
            | Error err -> furthestOpErr opErr1 (Some err) |> Option.defaultValue err |> Error
        | Error err -> Error err

/// Reverse map function for Parser type
let (<&>) (p: Parser<'a>) (f: 'a -> 'b) : Parser<'b> =
    p
    >> function
    | Error err -> Error err
    | Ok (out, rest, opErr) -> Ok (f out, rest, opErr)

/// Replacement function for Parser type
let (>|>) (p: Parser<'a>) (newItem: 'b) : Parser<'b> =
    p
    >> function
    | Error err -> Error err
    | Ok (_, rest, opErr) -> Ok (newItem, rest, opErr)

/// Optional combinator (do first, then try second)
/// This operator removed the needd for memoisation
/// Memoisation was very resource intensive bc of the large number of layers
let (?=>) (p1: Parser<'a>) (p2: Parser<'b>) : Parser<'a * 'b option> =
    fun inp ->
        match p1 inp with
        | Error err -> Error err
        | Ok (a, rest, opErr1) ->
            match p2 rest with
            | Error err -> Ok ((a, None), rest, furthestOpErr opErr1 (Some err))
            | Ok (b, rest', opErr2) -> Ok ((a, Some b), rest', furthestOpErr opErr1 opErr2)
        
/// Currently not used but can be added to improve speed if needed
let MemoiseParser fn =
    let mutable cache = Map []
    fun inp -> 
        if Map.containsKey inp cache
        then cache.[inp] 
        else 
            let res = fn inp
            cache <- Map.add inp res cache
            res

/// Make parser from list
let buildParser lst =
    List.reduce (<|>) lst










