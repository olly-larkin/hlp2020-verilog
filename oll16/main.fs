module Verishot.Main

open System
open Tests.ExpressionTests
open Parser

let testString = "module testModule (a, b, clk);

input a;
input clk;
output b;

wire[3:h] test_wire;
assign test_wire = a + clk;

endmodule"

let testString2 = "assign test_wire = a + clk;"

[<EntryPoint>]
let main argv =
    // runExpressionTests()
    testString |> ParseSource |> printfn "%A"
    // testString2 |> List.ofSeq |> ModuleDefinition.ModuleItemListParser |> printfn "%A"
    // Console.ReadKey() |> ignore
    0 // return an integer exit code