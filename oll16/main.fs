module Verishot.Main

open System
open Tests.ExpressionTests
open Verishot.Parser
open Verishot.Expression

let testString = "module test (a); 

input a; 

endmodule"

[<EntryPoint>]
let main argv =
    runExpressionTests()
    // testString |> ParseSource |> printfn "%A"
    // Console.ReadKey() |> ignore
    0