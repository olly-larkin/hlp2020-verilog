module Tests.ExpressionTests

open Expecto
open Verishot.Parser

let equalTestAsync =
    fun (name, inp, exp) ->
        testCaseAsync name <| async { Expect.equal inp exp name }


[<Tests>]
let stringParseTestsList =
    testList "string parse test list" <| ([

        "test -> test passes",
            ("test", List.ofSeq "test") ||> Token.Tools.stringParse,
                Ok ("test", [])

        "test -> not test fails",
            ("test", List.ofSeq "not test") ||> Token.Tools.stringParse,
                Error ("Could not match. Expected \'test\'.", ['n';'o';'t';' ';'t';'e';'s';'t'])

        "whitespace is removed",
            ("test", List.ofSeq "   test") ||> Token.Tools.stringParse,
                Ok ("test", [])

    ] |>List.map equalTestAsync)


[<Tests>]
let regParseTestsList =
    testList "reg parse test list" <| ([

        "matching plain string",
            ("test", List.ofSeq "test") ||> Token.Tools.regParse,
                Ok ("test", [])

        "matching int",
            ("[0-9]+", List.ofSeq "198.5") ||> Token.Tools.regParse,
                Ok ("198", ['.';'5'])

        "checking whitespace is removed",
            ("test", List.ofSeq "   test") ||> Token.Tools.regParse,
                Ok ("test", [])

    ] |> List.map equalTestAsync)

[<Tests>]
let expressionTestsList =
    testList "expression test list" <| ([

        

    ] |> List.map equalTestAsync)

let runExpressionTests() =
    runTests defaultConfig stringParseTestsList |> ignore
    runTests defaultConfig regParseTestsList |> ignore
    runTests defaultConfig expressionTestsList |> ignore