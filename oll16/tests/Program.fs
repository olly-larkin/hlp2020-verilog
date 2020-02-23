module Tests.Main

open System
open Tests.ExpressionTests

[<EntryPoint>]
let main argv =
    runExpressionTests() |> ignore
    0 // return an integer exit code
