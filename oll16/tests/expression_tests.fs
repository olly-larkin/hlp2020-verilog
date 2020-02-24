module Tests.ExpressionTests

open Expecto
open Verishot.Parser
open Verishot.CoreTypes.VerilogAST

let equalTestAsync =
    fun (name, inp, exp) ->
        testCaseAsync name <| async { Expect.equal inp exp name }

let errorTestAsync =
    fun (name, inp) ->
        testCaseAsync name <| async { Expect.isError inp name }

[<Tests>]
let stringParseTestsList =
    testList "string parse tests" ([
        ([
            "test -> test passes",
                ("test", List.ofSeq "test") ||> Token.Tools.stringParse,
                    Ok ("test", [])

            "whitespace is removed",
                ("test", List.ofSeq "   test") ||> Token.Tools.stringParse,
                    Ok ("test", [])

            "strict string positive test",
                ("test", List.ofSeq "test") ||> Token.Tools.strict Token.Tools.stringParse,
                    Ok ("test", [])

            ] |> List.map equalTestAsync)

        ([

            "test -> not test fails",
                ("test", List.ofSeq "not test") ||> Token.Tools.stringParse

            "strict string failure",
                ("test", List.ofSeq "testa") ||> Token.Tools.strict Token.Tools.stringParse
        
            ] |> List.map errorTestAsync)
    ] |> List.collect id)


[<Tests>]
let regParseTestsList =
    testList "reg parse tests" ([
        ([
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
        ([

            "checking reg parse fails correctly",
                ("[a-z]+", List.ofSeq "198.5") ||> Token.Tools.regParse

            ] |> List.map errorTestAsync)
    ] |> List.collect id)

[<Tests>]
let baseConversionsTestList =
    testList "baseConversionTests tests" ([
        ([
            "hex to dec",
                "1A" |> Token.Tools.hexToDec,
                    26

            "bin to dec",
                "1100" |> Token.Tools.binToDec,
                    12

            "oct to dec",
                "17" |> Token.Tools.octToDec,
                    15

            ] |> List.map equalTestAsync)
        ([

            // failure tests

            ] |> List.map errorTestAsync)
    ] |> List.collect id)

[<Tests>]
let numberParseTestsList =
    testList "number parse tests" ([
        ([
            "correctly matching hex value",
                List.ofSeq "10'h1A" |> Token.Number.Value,
                    Ok (26, [])

            "correctly matching dec value",
                List.ofSeq "10'd20" |> Token.Number.Value,
                    Ok (20, [])

            "correctly matching oct value",
                List.ofSeq "10'o17" |> Token.Number.Value,
                    Ok (15, [])

            "correctly matching bin value",
                List.ofSeq "4'b1100" |> Token.Number.Value,
                    Ok (12, [])

            ] |> List.map equalTestAsync)
        ([

            "correctly matching bin expr",
                List.ofSeq "4'b1100" |> Token.Number.Expr,
                    Ok (ExprNumber (Some 4, 12), [])

            "correctly giving no size",
                List.ofSeq "100" |> Token.Number.Expr,
                    Ok (ExprNumber (None, 100), [])

            ] |> List.map equalTestAsync)
        ([

            // failure tests

            ] |> List.map errorTestAsync)
    ] |> List.collect id)

[<Tests>]
let expressionTestsList =
    testList "expression tests" ([
        ([
            "checking a number gets passed to int from term",
                List.ofSeq "5" |> Expression.TermParser,
                    Ok (ExprNumber (None, 5), [])

            "checking a string gets passed to identifier from term",
                List.ofSeq "test" |> Expression.TermParser,
                    Ok (ExprIdentifier "test", [])

            "unary successful",
                List.ofSeq "+10" |> Expression.UnaryOpParser,
                    Ok (ExprUnary (UOpPlus, ExprNumber (None, 10)), [])

            "add sub successful plus",
                List.ofSeq "5 + 5" |> Expression.AddSubParser,
                    Ok (ExprBinary (ExprNumber (None, 5), BOpPlus, ExprNumber (None, 5)), [])

            "test bracketed expression",
                List.ofSeq "(1+2)*3" |> Expression.ExpressionParser,
                    Ok (ExprBinary (ExprBinary (ExprNumber (None, 1), BOpPlus, ExprNumber (None, 2)), BOpStar, ExprNumber (None, 3)), [])

            ] |> List.map equalTestAsync)
        ([

            "checking invalid identifier for term",
                List.ofSeq "-45" |> Expression.TermParser

            ] |> List.map errorTestAsync)
    ] |> List.collect id)


let runExpressionTests() =
    runTests defaultConfig stringParseTestsList |> ignore
    runTests defaultConfig regParseTestsList |> ignore
    runTests defaultConfig baseConversionsTestList |> ignore
    runTests defaultConfig numberParseTestsList |> ignore
    runTests defaultConfig expressionTestsList |> ignore