module Tests.ExpressionTests

open Expecto
open Verishot.Parser

[<Tests>]
let stringParseTest1 =
    testCase "testing stringParse correctly parses string passed in" <| fun () ->
        let pattern = "test"
        let input = "test" |> List.ofSeq
        Expect.equal (Token.Tools.stringParse pattern input) (Ok ("test", [])) "pattern: \"test\", input: \"test\""



let runExpressionTests() =
    runTestsInAssembly defaultConfig [||]