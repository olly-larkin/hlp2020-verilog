module Verishot.Test.Visualise

open Expecto
open Verishot.Test.Util
open Verishot.Visualise
open Verishot.CoreTypes
open Verishot.CoreTypes.Netlist

let splitNodesTests = 
    let inputPin0 = ("inputPin0", [{ srcRange=Single; targetRange=Single; target=PinTarget "test" }])
    let inputPin1 = ("inputPin1", [{ srcRange=Single; targetRange=Single; target=PinTarget "test" }])
    let modInst0 = { moduleName=StringIdentifier "modInst0"; instanceName="inst0"; connections=Map.empty }
    let modInst1 = { moduleName=StringIdentifier "modInst1"; instanceName="inst1"; connections=Map.empty }
 
    let outputPin0 = "outputPin0"
    let outputPin1 = "outputPin1"
    let node0 = InputPin inputPin0
    let node1 = InputPin inputPin1
    let node2 = ModuleInstance modInst0
    let node3 = ModuleInstance modInst1
    let node4 = Constant {| value=1; width=2; connections=[{ srcRange=Single; targetRange=Single; target=PinTarget "test" }] |}
    let node5 = Constant {| value=3; width=4; connections=[{ srcRange=Single; targetRange=Single; target=PinTarget "test" }] |}
    let node6 = OutputPin outputPin0
    let node7 = OutputPin outputPin1

    let extractConst = function | Constant x -> x | _ -> failwith ""
    let const0 = extractConst node4
    let const1 = extractConst node5
    [
        "empty",
            [],
                ([], [], [], [])
        "basic 1",
            [node0; node2; node4; node6],
                ([inputPin0], [modInst0], [outputPin0], [const0])
        "basic 2",
            [node0; node1; node2; node3; node4; node5; node6; node7],
                ([inputPin0; inputPin1], [modInst0; modInst1], [outputPin0; outputPin1], [const0; const1])
        "basic 2 order changed",
            [node4; node1; node6; node0; node2; node3; node7; node5],
                ([inputPin1; inputPin0], [modInst0; modInst1], [outputPin0; outputPin1], [const0; const1])    
    ]

[<Tests>]
let splitNodesTestList =
    testList "splitNodes" <| 
        (splitNodesTests 
         |> List.map (processIntoAsyncTestList1 splitNodes))