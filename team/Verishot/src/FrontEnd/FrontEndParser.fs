(*
    Parsing functions for Front End functions
    =========================================
    Author: lhl2617
*)

module Verishot.FrontEndParser

open Verishot.ParserUtils
open Verishot.Token
open Verishot.ModuleDefinition
open Verishot.CoreTypes

/// true if matches the identifier regex 
let matchIdentifierRegex (id: string) =
    id 
    |> List.ofSeq
    |> Token.Identifier
    |> function
    | Ok _ -> true
    | _ -> false


let vInAssignmentParser (inp: char list) =
    inp 
    |> (Token.Identifier ?=> RangeParser >=> Symbol.AssignEqual >=> Number.numParse >=> Symbol.Semicolon <&> 
        // TODO:- check that bits do fit
        fun ((((identifier, b), _), (_, value)), _) -> 
            let range =
                match b with 
                | Some rng -> rng
                | None -> Single
            (identifier, range), (uint64 value)
        )

