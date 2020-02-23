module Verishot.Main

open Tests.ExpressionTests

[<EntryPoint>]
let main argv =
    runExpressionTests()
    0 // return an integer exit code
