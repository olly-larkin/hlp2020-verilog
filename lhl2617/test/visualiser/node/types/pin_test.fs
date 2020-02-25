module Verishot.Test.Pin

open Expecto
open FsCheck

open Verishot.Test.Util
open Verishot.SVG
open Verishot.CoreTypes
open Verishot.VisualisePin
open Verishot.CoreTypes.Netlist
open Verishot.VisualiserUtil
open Verishot.VisualiserUtil.Pin
open Verishot.VisualiserUtil.ModuleInstance
open Verishot.VisualiserUtil.Functions

let truncPinTextTests = 
    [
        "empty",
            ("", 0, ""),
                ""
        "basic",
            ("abc", 10, "[1:12]"),
                "abc[1:12]"
        "basic trunc",
            ("abcdefghi", 8, "[1]"),
                "ab...[1]"
        "basic trunc2",
            ("abcdefghi", 6, "[1]"),
                "...[1]"
        "zero",
            ("abcdef", 0, "[1]"),
                "...[1]"
        "empty zero",
            ("", 0, "[1]"),
                "...[1]"
        "empty range",
            ("abc", 3, ""),
                "abc"
        "empty range 2",
            ("abcdef", 4, ""),
                "a..."
    ]

let getTitleTests = 
    (*
    Assuming 
    defaultGraphicsProps = {| maxTitleLen=16 |}
    *)
    
    let props = { defaultPinProps with height=1.; width=2.; marginLeft=42. }
    let singleRange: Range = Single
    let sameRanged: Range = Range(5, 5)
    let diffRanged: Range = Range (5, 0)
    [
        "input single", 
            ((1., 2.), "test", Input, singleRange, props),
            Text ((1.25, 2.5),"test",[("class", "pin-text-input")],Some "Input Pin: test")
        "input sameRanged",
            ((1., 2.), "test", Input, sameRanged, props),
                Text ((1.25, 2.5),"test[5]",[("class", "pin-text-input")],
                    Some "Input Pin: test\nRange: [5]")
        "input diffRanged",
            ((1., 2.), "test", Input, diffRanged, props),
                Text
                    ((1.25, 2.5),"test[5:0]",[("class", "pin-text-input")],
                     Some "Input Pin: test\nRange: [5:0]")
        "output single",
            ((1., 2.), "test", Output, singleRange, props),
                Text
                    ((44.75, 2.5),"test",[("class", "pin-text-output")],Some "Output Pin: test")
        "output sameRanged",
            ((1., 2.), "test", Output, sameRanged, props),
                Text ((44.75, 2.5),"test[5]",[("class", "pin-text-output")],
                    Some "Output Pin: test\nRange: [5]")
        "output diffRanged",
            ((1., 2.), "test", Output, diffRanged, props),
                Text ((44.75, 2.5),"test[5:0]",[("class", "pin-text-output")],
                    Some "Output Pin: test\nRange: [5:0]")
    ]

let getPortPropTests =
    (*
        Assuming:
        defaultPinProps =
        { width = 12.
          height = 1. }
      defaultPinMargin = 8.
    *)
    [
        "input",
            ((1., 2.), 3., Input, Range(4, 1)),
                { index=0; coord=(4., 2.5); range=Range(4, 1) }
        "output",
            ((1., 2.), 3., Output, Range(4, 1)),
                { index=0; coord=(9., 2.5); range=Range(4, 1) }
    ]

[<Tests>]
let getPortPropTestList =
    testList "getPortProp" <| 
        (getPortPropTests 
         |> List.map (processIntoAsyncTestList4 getPortProp))

[<Tests>]
let getTitleTestList =
    testList "getTitle" <| 
        (getTitleTests 
         |> List.map (processIntoAsyncTestList5 getTitle))

[<Tests>]
let truncPinTextTestList =
    testList "truncPinText" <| 
        (truncPinTextTests 
         |> List.map (processIntoAsyncTestList3 truncPinText))