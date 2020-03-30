module Verishot.Parser

open Verishot.ModuleDefinition
open Verishot.Token

let lineCalc lst =
    ((1, 1), lst) ||> List.fold (fun (line, ch) elem ->
        match elem with
        | '\n' -> line + 1, 1
        | _ -> line, ch + 1
    ) |> fun (l, c) -> {| line = l ; character = c |}

let trim n lst =
    List.truncate (List.length lst - n) lst

let ParseSource inp =
    let inp = inp |> List.ofSeq
    inp
    |> List.ofSeq
    |> ParseModuleDefinition
    |> function
    | Ok (res, tl, _) -> 
        if tl |> TokTools.whiteSpaceAndComments |> List.isEmpty
        then Ok (res)
        else Error ("There should be only 1 module declaration per file.", inp |> trim (List.length tl) |> lineCalc)
    | Error (msg, lst) -> Error (msg, inp |> trim (List.length lst) |> lineCalc)