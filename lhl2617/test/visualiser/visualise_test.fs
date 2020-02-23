module Verishot.Test.Visualise

open Expecto
open Verishot.Test.Util
open Verishot.Visualise
open Verishot.CoreTypes.Netlist

let splitNodesTests = 
    let inputPin0 = ("inputPin0", [{ srcPortIndex=0; targetNode="input0"; portName="foo1"; portIndex=0 }])
    let inputPin1 = ("inputPin1", [{ srcPortIndex=0; targetNode="input1"; portName="foo2"; portIndex=0 }])
    let modInst0 = { moduleName="modInst0"; instanceName="inst0"; connections=Map.empty }
    let modInst1 = { moduleName="modInst1"; instanceName="inst1"; connections=Map.empty }
    let outputPin0 = "outputPin0"
    let outputPin1 = "outputPin1"
    let node0 = InputPin inputPin0
    let node1 = InputPin inputPin1
    let node2 = ModuleInstance modInst0
    let node3 = ModuleInstance modInst1
    let node4 = OutputPin outputPin0
    let node5 = OutputPin outputPin1
    [
        "empty",
            [],
                ([], [], [])
        "basic 1",
            [node0; node2; node4],
                ([inputPin0], [modInst0], [outputPin0])
        "basic 2",
            [node0; node1; node2; node3; node4; node5],
                ([inputPin0; inputPin1], [modInst0; modInst1], [outputPin0; outputPin1])
        "basic 2 order changed",
            [node1; node4; node0; node2; node3; node5],
                ([inputPin1; inputPin0], [modInst0; modInst1], [outputPin0; outputPin1])    
    ]

// [<Tests>]
let splitNodesTestList =
    testList "splitNodes" <| 
        (splitNodesTests 
         |> List.map (processIntoAsyncTestList1 splitNodes))