module Tests.ExpressionTests

open Expecto
open Verishot.CoreTypes.VerilogAST
open Verishot.ParserUtils
open Verishot.Token
open Verishot.Expression
open Verishot.ModuleDefinition
open Verishot.Parser

let equalTestAsync =
    fun (name, inp, exp) ->
        testCaseAsync name <| async { Expect.equal inp exp name }

/// This is needed bc the optional branches generate temporary errors
/// That are not relevant if it succeeds (so it is ignored)
let parserEqualTestAsync =
    fun (name, inp, exp) ->
        testCaseAsync name <| async {
            (match inp, exp with
            | Ok (a, b, _), Ok (a', b') when a = a' && b = b' -> true
            | _ -> false
            |> Expect.isTrue) name 
        }

let errorTestAsync =
    fun (name, inp) ->
        testCaseAsync name <| async { Expect.isError inp name }

[<Tests>]
let stringParseTestsList =
    testList "string parse tests" ([
        ([
            "test -> test passes",
                ("test", List.ofSeq "test") ||> TokTools.stringParse,
                    Ok ("test", [])

            "whitespace is removed",
                ("test", List.ofSeq "   test") ||> TokTools.stringParse,
                    Ok ("test", [])

            "strict string positive test",
                ("test", List.ofSeq "test") ||> TokTools.strict TokTools.stringParse,
                    Ok ("test", [])

            ] |> List.map parserEqualTestAsync)

        ([

            "test -> not test fails",
                ("test", List.ofSeq "not test") ||> TokTools.stringParse

            "strict string failure",
                ("test", List.ofSeq "testa") ||> TokTools.strict TokTools.stringParse
        
            ] |> List.map errorTestAsync)
    ] |> List.collect id)


[<Tests>]
let regParseTestsList =
    testList "reg parse tests" ([
        ([
            "matching plain string",
                ("test", List.ofSeq "test") ||> TokTools.regParse,
                    Ok ("test", [])

            "matching int",
                ("[0-9]+", List.ofSeq "198.5") ||> TokTools.regParse,
                    Ok ("198", ['.';'5'])

            "checking whitespace is removed",
                ("test", List.ofSeq "   test") ||> TokTools.regParse,
                    Ok ("test", [])

            ] |> List.map parserEqualTestAsync)

        ([

            "checking reg parse fails correctly",
                ("[a-z]+", List.ofSeq "198.5") ||> TokTools.regParse

            ] |> List.map errorTestAsync)
    ] |> List.collect id)

[<Tests>]
let baseConversionsTestList =
    testList "baseConversionTests tests" ([
        ([
            "hex to dec",
                "1A" |> TokTools.hexToDec,
                    26

            "bin to dec",
                "1100" |> TokTools.binToDec,
                    12

            "oct to dec",
                "17" |> TokTools.octToDec,
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
                List.ofSeq "10'h1A" |> Number.Value,
                    Ok (26, [])

            "correctly matching dec value",
                List.ofSeq "10'd20" |> Number.Value,
                    Ok (20, [])

            "correctly matching oct value",
                List.ofSeq "10'o17" |> Number.Value,
                    Ok (15, [])

            "correctly matching bin value",
                List.ofSeq "4'b1100" |> Number.Value,
                    Ok (12, [])

            ] |> List.map parserEqualTestAsync)

        ([

            "correctly matching bin expr",
                List.ofSeq "4'b1100" |> Number.Expr,
                    Ok (ExprNumber (Some 4, 12), [])

            "correctly giving no size",
                List.ofSeq "100" |> Number.Expr,
                    Ok (ExprNumber (None, 100), [])

            ] |> List.map parserEqualTestAsync)
        ([

            // failure tests

            ] |> List.map errorTestAsync)
    ] |> List.collect id)

[<Tests>]
let keywordTestsList =
    testList "keyword tests" ([
        ([
            "checking keyword 'assign'",
                List.ofSeq "assign" |> Keyword.Assign,
                    Ok ("assign", [])

            "checking keyword 'endmodule'",
                List.ofSeq "endmodule" |> Keyword.Endmodule,
                    Ok ("endmodule", [])

            "checking keyword 'input'",
                List.ofSeq "input" |> Keyword.Input,
                    Ok ("input", [])

            "checking keyword 'module'",
                List.ofSeq "module" |> Keyword.Module,
                    Ok ("module", [])

            "checking keyword 'output'",
                List.ofSeq "output" |> Keyword.Output,
                    Ok ("output", [])
                   
            "checking keyword 'wire'",
                List.ofSeq "wire" |> Keyword.Wire,
                    Ok ("wire", [])

            ] |> List.map parserEqualTestAsync)

        ([
            "checking keyword 'assign' strict",
                List.ofSeq "assigne" |> Keyword.Assign

            "checking keyword 'endmodule' strict",
                List.ofSeq "endmodulee" |> Keyword.Endmodule

            "checking keyword 'input' strict",
                List.ofSeq "inpute" |> Keyword.Input

            "checking keyword 'module' strict",
                List.ofSeq "modulee" |> Keyword.Module

            "checking keyword 'output' strict",
                List.ofSeq "outpute" |> Keyword.Output
                   
            "checking keyword 'wire' strict",
                List.ofSeq "wiree" |> Keyword.Wire

            ] |> List.map errorTestAsync)
    ] |> List.collect id)

[<Tests>]
let expressionTestsList =
    testList "expression tests" ([
        ([
            "checking a number gets passed to int from term",
                List.ofSeq "5" |> TermParser,
                    Ok (ExprNumber (None, 5), [])

            "checking a string gets passed to identifier from term",
                List.ofSeq "test" |> TermParser,
                    Ok (ExprIdentifier "test", [])

            "unary successful",
                List.ofSeq "+10" |> UnaryOpParser,
                    Ok (ExprUnary (UOpPlus, ExprNumber (None, 10)), [])

            "add sub successful plus",
                List.ofSeq "5 + 6" |> AddSubParser,
                    Ok (ExprBinary (ExprNumber (None, 5), BOpPlus, ExprNumber (None, 6)), [])

            "add sub successful minus",
                List.ofSeq "5 - 6" |> AddSubParser,
                    Ok (ExprBinary (ExprNumber (None, 5), BOpMinus, ExprNumber (None, 6)), [])

            "mul div mod parser successful mul",
                List.ofSeq "5 * 6" |> MulDivModParser,
                    Ok (ExprBinary (ExprNumber (None, 5), BOpStar, ExprNumber (None, 6)), [])

            "mul div mod parser successful div",
                List.ofSeq "5 / 6" |> MulDivModParser,
                    Ok (ExprBinary (ExprNumber (None, 5), BOpDiv, ExprNumber (None, 6)), [])

            "mul div mod parser successful mod",
                List.ofSeq "5 % 6" |> MulDivModParser,
                    Ok (ExprBinary (ExprNumber (None, 5), BOpMod, ExprNumber (None, 6)), [])

            "exponent parser",
                List.ofSeq "5 ** 6" |> ExponentParser,
                    Ok (ExprBinary (ExprNumber (None,5), BOpExponent, ExprNumber (None,6)), [])

            "shift parser logical left",
                List.ofSeq "5 << 6" |> ShiftParser,
                    Ok (ExprBinary (ExprNumber (None,5), BOpLogicLeftShift, ExprNumber (None,6)), [])

            "shift parser logical right",
                List.ofSeq "5 >> 6" |> ShiftParser,
                    Ok (ExprBinary (ExprNumber (None,5), BOpLogicRightShift, ExprNumber (None,6)), [])

            "shift parser arithmatic left",
                List.ofSeq "5 <<< 6" |> ShiftParser,
                    Ok (ExprBinary (ExprNumber (None,5), BOpArithmeticLeftShift, ExprNumber (None,6)), [])

            "shift parser arithmatic right",
                List.ofSeq "5 >>> 6" |> ShiftParser,
                    Ok (ExprBinary (ExprNumber (None,5), BOpArithmeticRightShift, ExprNumber (None,6)), [])

            "test bracketed expression",
                List.ofSeq "(1+2)*3" |> ExpressionParser,
                    Ok (ExprBinary (ExprBinary (ExprNumber (None, 1), BOpPlus, ExprNumber (None, 2)), BOpStar, ExprNumber (None, 3)), [])

            ] |> List.map parserEqualTestAsync)

        ([

            "checking invalid identifier for term",
                List.ofSeq "-45" |> TermParser

            ] |> List.map errorTestAsync)

    ] |> List.collect id)

[<Tests>]
let somePropertyTests =
    let minMax min max var =
        let minFun a b = if a <= b then a else b
        let maxFun a b = if a >= b then a else b
        maxFun min (minFun max var)  
    testList "property tests" [
        testProperty "testing that trim outputs right length" <| fun lst n ->
            (trim n lst |> List.length) = (List.length lst - n |> minMax 0 (List.length lst))
    ]

let runExpressionTests() =
    runTests defaultConfig stringParseTestsList |> ignore
    runTests defaultConfig regParseTestsList |> ignore
    runTests defaultConfig baseConversionsTestList |> ignore
    runTests defaultConfig numberParseTestsList |> ignore
    runTests defaultConfig keywordTestsList |> ignore
    runTests defaultConfig expressionTestsList |> ignore
    runTests defaultConfig somePropertyTests |> ignore