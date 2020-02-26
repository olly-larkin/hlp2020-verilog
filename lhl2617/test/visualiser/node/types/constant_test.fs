module Verishot.Test.Constant

open Expecto

open Verishot.Test.Util
open Verishot.CoreTypes
open Verishot.CoreTypes.Netlist
open Verishot.VisualiseConstant

let verifyConstantConnectionsTests =

    let con0 = { srcRange=Single; targetRange=Single; target=PinTarget "foo" }
    let con1 = { srcRange=Range(3,0); targetRange=Range(5,2); target=PinTarget "foo" }
    let con2 = { srcRange=Single; targetRange=Single; target=InstanceTarget ("bar", "p1") }
    let con3 = { srcRange=Range(2,0); targetRange=Range(100,98); target=InstanceTarget ("bar", "p1") }
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
    let con0 = { srcRange=Single; targetRange=Single; target=PinTarget "foo" }
    let con1 = { srcRange=Range(3,0); targetRange=Range(5,2); target=PinTarget "foo" }
    let con2 = { srcRange=Range(3,0); targetRange=Range(5,2); target=PinTarget "FOOOOOO" }
    let con3 = { srcRange=Single; targetRange=Single; target=InstanceTarget ("bar", "p1") }
    let con4 = { srcRange=Range(2,0); targetRange=Range(100,98); target=InstanceTarget ("bar", "p1") }
    let con5 = { srcRange=Range(2,0); targetRange=Range(100,98); target=InstanceTarget ("bar", "p2") }
    let con6 = { srcRange=Range(2,0); targetRange=Range(100,98); target=InstanceTarget ("baaaaar", "p1") }
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
