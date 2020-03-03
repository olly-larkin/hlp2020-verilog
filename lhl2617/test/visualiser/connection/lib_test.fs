module Verishot.Test.VisualiseConnectionLib

open Expecto

open Verishot.Test.Util
open Verishot.CoreTypes
open Verishot.VisualiserUtil.Connection
open Verishot.CoreTypes.Netlist
open Verishot.VisualiseConnectionLib

let groupConnectionsTests = 
    let con0 = { srcRange=Single; targetRange=Single; target=PinTarget "foo" }
    let con1 = { srcRange=Range(3,0); targetRange=Range(5,2); target=PinTarget "foo" }
    let con2 = { srcRange=Range(3,0); targetRange=Range(5,2); target=PinTarget "FOOOOOO" }

    let con3 = { srcRange=Single; targetRange=Single; target=InstanceTarget ("bar", "p1") }
    let con4 = { srcRange=Range(2,0); targetRange=Range(100,98); target=InstanceTarget ("bar", "p1") }
    let con5 = { srcRange=Range(2,0); targetRange=Range(34,32); target=InstanceTarget ("bar", "p1") }
    let con6 = { srcRange=Range(9,7); targetRange=Range(34,32); target=InstanceTarget ("bar", "p1") }
    let con7 = { srcRange=Range(2,0); targetRange=Range(100,98); target=InstanceTarget ("bar", "p2") }
    let con8 = { srcRange=Range(2,0); targetRange=Range(100,98); target=InstanceTarget ("baaaaar", "p1") }
    [
        "single pintarget",
            [con0],
                ["foo", ["foo", { inputRange=Single; outputRange=Single }]]
        "single instanceTarget",
            [con3],
                ["bar", ["p1", { inputRange=Single; outputRange=Single }]]
        "single pintarget and instanceTarget",
            [con0; con3],
                [
                    "foo", ["foo", { inputRange=Single; outputRange=Single }]
                    "bar", ["p1", { inputRange=Single; outputRange=Single }]
                ]
        "all pintargets",
            [con0; con1; con2],
                [
                    "foo", ["foo", { inputRange=Single; outputRange=Single }; "foo", { inputRange=Range(3,0); outputRange=Range(5,2) }]
                    "FOOOOOO", ["FOOOOOO", { inputRange=Range(3,0); outputRange=Range(5,2) }]
                ]
        "all instancetargets",
            [con3; con4; con5; con6; con7; con8],
                [
                    "bar", 
                        [
                            "p1", { inputRange=Single; outputRange=Single }
                            "p1", { inputRange=Range(2,0); outputRange=Range(100,98) }
                            "p1", { inputRange=Range(2,0); outputRange=Range(34,32) }
                            "p1", { inputRange=Range(9,7); outputRange=Range(34,32) }
                            "p2", { inputRange=Range(2,0); outputRange=Range(100,98) }
                        ]
                    "baaaaar", ["p1", { inputRange=Range(2,0); outputRange=Range(100,98) }]
                ]
        "all targets",
            [con0; con1; con2; con3; con4; con5; con6; con7; con8],
                [
                    "foo", ["foo", { inputRange=Single; outputRange=Single }; "foo", { inputRange=Range(3,0); outputRange=Range(5,2) }]
                    "FOOOOOO", ["FOOOOOO", { inputRange=Range(3,0); outputRange=Range(5,2) }]
                    "bar", 
                    [
                        "p1", { inputRange=Single; outputRange=Single }
                        "p1", { inputRange=Range(2,0); outputRange=Range(100,98) }
                        "p1", { inputRange=Range(2,0); outputRange=Range(34,32) }
                        "p1", { inputRange=Range(9,7); outputRange=Range(34,32) }
                        "p2", { inputRange=Range(2,0); outputRange=Range(100,98) }
                    ]
                    "baaaaar", ["p1", { inputRange=Range(2,0); outputRange=Range(100,98) }]
                ]        
    ]

let truncConnectionTextTests =
    let singleRange: Range = Single
    let sameRange: Range = Range(1, 1)
    let range: Range = Range(100, 0)
    [
        "empty",
            ("", 0, singleRange),
                ""
        "single, no effect",
            ("abc", 3, singleRange),
                "abc"
        "normal single trunc text",
            ("abcdef", 5, singleRange),
                "ab..."
        "sameRange no truncated",
            ("abcdef", 9, sameRange),
                "abcdef[1]"
        "sameRange truncated",
            ("abcdefg", 9, sameRange),
                "abc...[1]"
        "range no effect",
            ("abcd", 11, range),
                "abcd[100:0]"
        "range truncated",
            ("abcde", 11, range),
                "a...[100:0]"   
        "too short",
            ("abcde", 0, range),
                "...[100:0]"    
    ]

let getLineClassNameTests =
    let singleRange: Range = Single
    let sameRange: Range = Range(1, 1)
    let range: Range = Range(100, 0)

    [
        "single",
            singleRange,
                "label-line-single"
        "sameRange",
            sameRange,
                "label-line-single"
        "range",
            range,
                "label-line-range"
    ]

[<Tests>]
let groupConnectionsTestList =
    testList "groupConnections" <| 
        (groupConnectionsTests 
         |> List.map (processIntoAsyncTestList1 groupConnections))

[<Tests>]
let truncConnectionTextTestList =
    testList "truncConnectionText" <| 
        (truncConnectionTextTests 
         |> List.map (processIntoAsyncTestList3 truncConnectionText))
         
[<Tests>]
let getLineClassNameTestList =
    testList "getLineClassName" <| 
        (getLineClassNameTests 
         |> List.map (processIntoAsyncTestList1 getLineClassName))
         