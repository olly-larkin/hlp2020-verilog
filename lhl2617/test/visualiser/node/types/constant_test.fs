module Verishot.Test.Constant

open Expecto

open Verishot.Test.Util
open Verishot.CoreTypes.Netlist
open Verishot.VisualiseConstant

let verifyConstantConnectionsTests =
    let con0 = { srcPortIndex = 0; target=PinTarget {| pinName="foo"; pinIndex=0 |} }
    let con1 = { srcPortIndex = 1; target=PinTarget {| pinName="foo"; pinIndex=1 |} }
    let con2 = { srcPortIndex = 0; target=InstanceTarget {| targetNode="bar"; portName="p1"; portIndex=1 |} }
    let con3 = { srcPortIndex = 1; target=InstanceTarget {| targetNode="bar"; portName="p1"; portIndex=2 |} }
    [
        "1 pin",
            [con0],
                ("foo", "foo")
        "2 pin",
            [con1; con0],
                ("foo", "foo")
        "1 instance port",
            [con2],
                ("bar", "p1")
        "2 instance port",
            [con2; con3],
                ("bar", "p1")
    ]

let verifyConstantConnectionsTestsE = 
    let con0 = { srcPortIndex = 0; target=PinTarget {| pinName="foo"; pinIndex=0 |} }
    let con1 = { srcPortIndex = 1; target=PinTarget {| pinName="foo"; pinIndex=1 |} }
    let con2 = { srcPortIndex = 3; target=PinTarget {| pinName="FOOOOOO"; pinIndex=1 |} }
    let con3 = { srcPortIndex = 0; target=InstanceTarget {| targetNode="bar"; portName="p1"; portIndex=1 |} }
    let con4 = { srcPortIndex = 1; target=InstanceTarget {| targetNode="bar"; portName="p1"; portIndex=2 |} }
    let con5 = { srcPortIndex = 1; target=InstanceTarget {| targetNode="bar"; portName="p2"; portIndex=2 |} }
    let con6 = { srcPortIndex = 1; target=InstanceTarget {| targetNode="BAAAAAR"; portName="p2"; portIndex=2 |} }
    [
        "different pins",
            [con0; con1; con2],
                "ERROR: Constants can only connect to one Pin or Module Instance."
        "pin + instance",
            [con0; con3],
                "ERROR: Constants can only connect to one Pin or Module Instance."
        "same node, different port",
            [con4; con5],
                "ERROR: Constants can only connect to one Pin or Module Instance."
        "different node, same portname",
            [con5; con6],
                "ERROR: Constants can only connect to one Pin or Module Instance."
    ]

[<Tests>]
let verifyConstantConnectionsTestList =
    testList "verifyConstantConnections" <| 
        (verifyConstantConnectionsTests 
         |> List.map (processIntoAsyncTestList1 verifyConstantConnections))

[<Tests>]
let verifyConstantConnectionsTestListE =
    testList "verifyConstantConnections" <| 
        (verifyConstantConnectionsTestsE 
         |> List.map (processIntoAsyncTestListE1 verifyConstantConnections))
