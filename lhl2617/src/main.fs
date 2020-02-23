module Verishot.Main

open System
open System.IO
open Verishot.SVG
open Verishot.CoreTypes
open Verishot.CoreTypes.VerilogAST
open Verishot.CoreTypes.Netlist
open Verishot.Visualise
open Verishot.Util
open Verishot.VisualiserUtil
open Verishot.VisualiserUtil.Functions

[<EntryPoint>]
let main argv =
    let styles = loadCSSWithUnit "./src/assets/css/styles.css" unitPx

    // let decls: ModuleDecl list =
    //     [ { name = "mod1"
    //         ports =
    //             [ (Input, "in1", Single)
    //               (Input, "inBus", Range(3, 0))
    //               (Input, "in2", Single)
    //               (Input, "in3", Single)
    //               (Input, "in4", Single)
    //               (Output, "out1", Single)
    //               (Output, "out2", Single)
    //               (Output, "out3", Range(15, 0)) ] }
    //       { name = "mod2"
    //         ports =
    //             [ (Input, "in1", Single)
    //               (Input, "busInput", Range(6, 0))
    //               (Output, "out1", Single)
    //               (Output, "out2", Single) ] }
    //       { name = "Inverter"
    //         ports =
    //             [ (Input, "in1", Single)
    //               (Input, "in2", Single)
    //               (Input, "in3", Range(7, 0))
    //               (Output, "out1", Single) ] } ]

    // let netlists: Netlist list =
    //     [ { moduleName = "Inverter"
    //         nodes =
    //             [ InputPin("in1", [])
    //               OutputPin("out1") ] }
    //       { moduleName = "mod1"
    //         nodes =
    //             [ InputPin
    //                 ("in1",
    //                  [ { portIndex = 0
    //                      portName = "in1"
    //                      srcPortIndex = 0
    //                      targetNode = "ModInst2" } ])
    //               InputPin("in2", [])
    //               InputPin("in3", [])
    //               InputPin("in4", [])
    //               InputPin("inBus", [])
    //               ModuleInstance
    //                   { moduleName = "mod2"
    //                     instanceName = "ModInst2"
    //                     connections = Map.empty }
    //               OutputPin("out1")
    //               OutputPin("out2") ] }
    //       { moduleName = "mod2"
    //         nodes =
    //             [ InputPin
    //                 ("in1",
    //                  [ { portIndex = 0
    //                      portName = "in1"
    //                      srcPortIndex = 0
    //                      targetNode = "ModInst1" }
    //                    { portIndex = 0
    //                      portName = "in2"
    //                      srcPortIndex = 0
    //                      targetNode = "ModInst1" }
    //                    { portIndex = 0
    //                      portName = "in3"
    //                      srcPortIndex = 0
    //                      targetNode = "ModInst1" } ])
    //               InputPin
    //                   ("busInput",
    //                    [ { portIndex = 3
    //                        portName = "inBus"
    //                        srcPortIndex = 6
    //                        targetNode = "ModInst1" }
    //                      { portIndex = 2
    //                        portName = "inBus"
    //                        srcPortIndex = 5
    //                        targetNode = "ModInst1" }
    //                      { portIndex = 1
    //                        portName = "inBus"
    //                        srcPortIndex = 4
    //                        targetNode = "ModInst1" }
    //                      { portIndex = 0
    //                        portName = "inBus"
    //                        srcPortIndex = 3
    //                        targetNode = "ModInst1" }
    //                      { portIndex = 0
    //                        portName = "in4"
    //                        srcPortIndex = 4
    //                        targetNode = "ModInst1" } ])
    //               ModuleInstance
    //                   { moduleName = "mod1"
    //                     instanceName = "ModInst1"
    //                     connections =
    //                         Map
    //                             [ ("out2",
    //                                [ { portIndex = 0
    //                                    portName = "out1"
    //                                    srcPortIndex = 0
    //                                    targetNode = "out1" } ])
    //                               ("out1",
    //                                [ { portIndex = 0
    //                                    portName = "in1"
    //                                    srcPortIndex = 0
    //                                    targetNode = "inverter1" } ]) 
    //                               ("out3",
    //                                [ 
    //                                    { portIndex = 0; portName = "in2"; srcPortIndex = 1; targetNode = "inverter1" } 
    //                                    { portIndex = 7; portName = "in3"; srcPortIndex = 15; targetNode = "inverter1" } 
    //                                    { portIndex = 6; portName = "in3"; srcPortIndex = 14; targetNode = "inverter1" } 
    //                                    { portIndex = 5; portName = "in3"; srcPortIndex = 13; targetNode = "inverter1" } 
    //                                    { portIndex = 4; portName = "in3"; srcPortIndex = 12; targetNode = "inverter1" } 
    //                                    { portIndex = 3; portName = "in3"; srcPortIndex = 11; targetNode = "inverter1" } 
    //                                    { portIndex = 2; portName = "in3"; srcPortIndex = 10; targetNode = "inverter1" } 
    //                                    { portIndex = 1; portName = "in3"; srcPortIndex = 9; targetNode = "inverter1" } 
    //                                    { portIndex = 0; portName = "in3"; srcPortIndex = 8; targetNode = "inverter1" } 
    //                                ]) 
    //                                    ] }
    //               ModuleInstance
    //                   { moduleName = "Inverter"
    //                     instanceName = "inverter1"
    //                     connections =
    //                         Map
    //                             [ ("out1",
    //                                [ { portIndex = 0
    //                                    portName = "out2"
    //                                    srcPortIndex = 0
    //                                    targetNode = "out2" } ]) ] }
    //               OutputPin("out1")
    //               OutputPin("out2") ] } ]

    let decls = 
        [
            { name="A"; ports=[(Input, "in1", Single); (Input, "in2", Single); (Output, "out1", Single); (Output, "out2", Single)] }
            // { name="BBB"; ports=[(Input, "in1", Single); (Input, "in2", Single); (Output, "out1", Single); (Output, "out2", Single)] }
        ]
    let expectedNetlist =
        { moduleName = "A"
          nodes =
              [ InputPin
                  ("in1",
                   [ { srcPortIndex = 0
                       target = InstanceTarget
                                    {| targetNode = "BOpBitwiseAnd-0"
                                       portName = "left"
                                       portIndex = 0 |} }
                     { srcPortIndex = 0
                       target = InstanceTarget
                                    {| targetNode = "BOpBitwiseAnd-1"
                                       portName = "right"
                                       portIndex = 0 |} }
                     { srcPortIndex = 0
                       target = InstanceTarget
                                    {| targetNode = "reeeeeeeeeeeeee"
                                       portName = "in1"
                                       portIndex = 0 |} }                 
                     { srcPortIndex = 0
                       target = InstanceTarget
                                    {| targetNode = "reeeeeeeeeeeeee"
                                       portName = "in2"
                                       portIndex = 0 |} }                 
                                        ]) 
                InputPin
                    ("in2",
                     [ { srcPortIndex = 0
                         target = InstanceTarget
                                      {| targetNode = "BOpBitwiseAnd-0"
                                         portName = "right"
                                         portIndex = 0 |} }
                    //    { srcPortIndex = 0
                    //      target = InstanceTarget
                    //                   {| targetNode = "BOpBitwiseAnd-1"
                    //                      portName = "left"
                    //                      portIndex = 0 |} } 
                                         ])
                OutputPin("out1")
                OutputPin("out2")
                ModuleInstance
                    ({ moduleName = StringIdentifier "A"
                       instanceName = "reeeeeeeeeeeeee"
                       connections = Map [] })
                ModuleInstance
                    ({ moduleName = BOpIdentifier BOpBitwiseAnd
                       instanceName = "BOpBitwiseAnd-0"
                       connections =
                           Map
                               [ "output",
                                 [ { srcPortIndex = 0
                                     target = InstanceTarget
                                             {| targetNode = "BOpBitwiseAnd-1"
                                                portName = "left"
                                                portIndex = 0 |} } ] ] })
                ModuleInstance
                    ({ moduleName = BOpIdentifier BOpBitwiseAnd
                       instanceName = "BOpBitwiseAnd-1"
                       connections =
                           Map
                               [ "output",
                                 [ { srcPortIndex = 1
                                     target = PinTarget
                                                  {| pinName = "out2"
                                                     pinIndex = 1 |} } ] ] }) ] }

    visualiseNetlists [expectedNetlist] decls styles |> ignore

    0 // return an integer exit code
