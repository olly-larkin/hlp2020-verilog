module Verishot.Main

open System
open Tests.ExpressionTests
open Verishot.Parser
open Verishot.Expression
open Verishot.ModuleDefinition

let testString = "output test3"

[<EntryPoint>]
let main argv =
    runExpressionTests()
    // testString |> List.ofSeq |> PortDeclarationParser |> printfn "%A"
    // Console.ReadKey() |> ignore
    0