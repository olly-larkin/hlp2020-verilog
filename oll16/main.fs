module Verishot.Main

open System
open Tests.ExpressionTests

[<EntryPoint>]
let main argv =
    runExpressionTests()
    // Console.ReadKey() |> ignore
    0 // return an integer exit code
