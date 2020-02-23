module Verishot.Main

open System
open Verishot.Parser

let testStrings = [
    "15"
    "5 + 6"
    "hello + 5 + 1"
    "hello"
    "yes + please"
    "(5 / 10) / test"
    "5 + +5"
    "5 +-5"
    "{test}"
    "{5, 10 + 5, true ? 5 : 10}"
]

let printItem item=
    printfn "%A" item
    item

[<EntryPoint>]
let main argv =
    testStrings
    |> List.map (List.ofSeq >> Expression.ExpressionParser >> printfn "%A")
    |> ignore
    0 // return an integer exit code
