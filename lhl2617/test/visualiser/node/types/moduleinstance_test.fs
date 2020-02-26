module Verishot.Test.ModuleInstance

open Expecto

open Verishot.Test.Util

open Verishot.SVG
open Verishot.CoreTypes
open Verishot.CoreTypes.VerilogAST
open Verishot.CoreTypes.Netlist
open Verishot.VisualiserUtil
open Verishot.VisualiserUtil.ModuleInstance
open Verishot.VisualiserUtil.Functions
open Verishot.VisualiseModuleInstance

let truncPortTextTests =
    [
        "empty unranged",
            ("", 10, false),
                ""
        "empty ranged",
            ("", 10, true),
                "[]"
        "basic unranged",
            ("abc", 10, false),
                "abc"
        "basic ranged",
            ("abc", 10, true),
                "abc[]"
        "trunc unranged",
            ("abcdefg", 6, false),
                "abc..."
        "trunc ranged",
            ("abcdefg", 6, true),
                "a...[]"
    ]

let getWidthTests =
    
    [
        "long instName",
            ("1234578901256789", [], []),
                12.
        "very long portName",
            ("a", ["34567894567891221", 1], []),
                12.
        "short portname and instName",
            ("a", ["1", 1], []),
                4.
        "instName longer than portnName but still short",
            ("abc", ["1", 1], ["1", 1]),
                4.
    ]

let getHeightTests = 
    (*
    assuming
    let defaultGraphicsProps =
    {|
       titleHeight = 2
       |}
       *)
    [
        "empty",
            ([], []),
                3.
        "1",
            ([1], []),
                4.
        "1 both",
            ([1], [1]),
                4.
        "a lot",
            ([1; 2; 3; 4; 5; 6; 7], []),
                10.
    ]

let getSrcPortsRangeFromModInstTests =
    let con0 = { srcRange=Single; targetRange=Single; target=PinTarget "test" }
    let con1 = { srcRange=Range(1, 1); targetRange=Single; target=PinTarget "test" }
    let con2 = { srcRange=Range(3, 0); targetRange=Single; target=PinTarget "test" }

    let modInst0 = { moduleName=UOpIdentifier UOpMinus; instanceName="inst"; connections=Map [("a", [con0])] }
    let modInst1 = { moduleName=UOpIdentifier UOpMinus; instanceName="inst"; connections=Map [("a", [con1])] }
    let modInst2 = { moduleName=UOpIdentifier UOpMinus; instanceName="inst"; connections=Map [("a", [con0; con1])] }

    [
        "simple Single",
            modInst0,
                Single
        "simple 1",
            modInst1,
                Range (1, 1)
        "simple range",
            modInst2,
                Range (3, 0)
    ]

[<Tests>]
let getWidthTestList =
    testList "getWidth" <|
        (getWidthTests
        |> List.map (processIntoAsyncTestList3 getWidth))
    
[<Tests>]
let getHeightTestList =
    testList "getHeight" <|
        (getHeightTests
        |> List.map (processIntoAsyncTestList2 getHeight))

[<Tests>]
let truncPortTextTestList =
    testList "truncPortText" <|
        (truncPortTextTests
        |> List.map (processIntoAsyncTestList3 truncPortText))