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

            ] |> List.map equalTestAsync)

        ([

            "test -> not test fails",
                ("test", List.ofSeq "not test") ||> Token.Tools.stringParse
        
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
let expressionTestsList =
    testList "expression tests" ([
        ([
            "checking a number gets passed to int from term",
                List.ofSeq "5" |> Expression.TermParser,
                    Ok (ExprNumber 5, [])

            "checking a string gets passed to identifier from term",
                List.ofSeq "test" |> Expression.TermParser,
                    Ok (ExprIdentifier "test", [])

            "unary successful",
                List.ofSeq "+10" |> Expression.UnaryOpParser,
                    Ok (ExprUnary (UOpPlus, ExprNumber 10), [])

            "add sub successful plus",
                List.ofSeq "5 + 5" |> Expression.AddSubParser,
                    Ok (ExprBinary (ExprNumber 5, BOpPlus, ExprNumber 5), [])

            ] |> List.map equalTestAsync)
        ([

            "checking invalid identifier for term",
                List.ofSeq "-45" |> Expression.TermParser

            ] |> List.map errorTestAsync)
    ] |> List.collect id)

let runExpressionTests() =
    runTests defaultConfig stringParseTestsList |> ignore
    runTests defaultConfig regParseTestsList |> ignore
    runTests defaultConfig expressionTestsList |> ignore