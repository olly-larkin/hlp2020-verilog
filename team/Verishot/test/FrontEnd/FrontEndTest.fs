module Verishot.FrontEnd.Test

open Expecto
open Verishot.Util
open Verishot.CoreTypes
open Verishot.Test.Util

let parseAndMapTests = 
    [
        "basic",
            ["bar=2;"; "foo=1;"],
                Map [
                    ("foo", Single), uint64 1
                    ("bar", Single), uint64 2
                ]
        "ranged",
            ["foo[32:3]=7'hFF;";"bar[4]=1'b1;"],
                Map [
                    ("foo", Range (32, 3)), uint64 255
                    ("bar", Range (4, 4)), uint64 1
                ]
        "invalid",
            ["fuu=;"; "foo=1"; "bar"; "car"; "=3"; "life=4'h42"; "some[1:4]=12;"],
                Map []
    ]

    
[<Tests>]
let parseAndMapTestList =
    testList "parseAndMap"
    <| (parseAndMapTests
        |> List.map (processIntoAsyncTestList1 parseAndMap))

let matchMapWithInputPortsTests = 
    [
        "basic passing", 
            (Map [ 
                ("__CYCLES__", Single), uint64 100
            ], []),
                Ok ""
        "basic with one input",
            (Map [
                ("__CYCLES__", Single), uint64 100
                ("foo", Single), uint64 1
            ], [
                ("foo", Single)
            ]),
                Ok ""
        "cycles forgotten",
            (Map [], []),
                Error (exitCodes.vInError, "\nSimulator input `__CYCLES__` invalid or not found.")
        "inp forgotten", 
            (Map [ 
                ("__CYCLES__", Single), uint64 100
            ], [
                ("foo", Range(32, 2))
            ]),
                Error (exitCodes.vInError, "\nSimulator input `foo[32:2]` invalid or not found.")
        "multiple forgotten", 
            (Map [ 
            ], [
                ("foo", Range(32, 2))
                ("bar", Single)
                ("car", Range(4, 4))
            ]),
                Error (exitCodes.vInError, sprintf "\n%s\n%s\n%s\n%s" 
                    "Simulator input `__CYCLES__` invalid or not found."
                    "Simulator input `foo[32:2]` invalid or not found."
                    "Simulator input `bar` invalid or not found."
                    "Simulator input `car[4]` invalid or not found.")

    ]
    
[<Tests>]
let matchMapWithInputPortsTestList =
    testList "matchMapWithInputPorts"
    <| (matchMapWithInputPortsTests
        |> List.map (processIntoAsyncTestList2 matchMapWithInputPorts))
