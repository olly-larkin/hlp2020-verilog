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
let main _ =
    let styles = loadCSSWithUnit "./src/assets/css/styles.css" unitPx

    let decls = 
        [
            { name="Module0"; ports=[(Input, "in0", Range(3, 0)); (Input, "in1", Single); (Output, "out0", Range(3, 0)); (Output, "out1", Single ); (Output, "out2", Single)] }
            { name="SimpleAdd3"; ports=[(Input, "in0", Single); (Input, "in1", Single); (Input, "in2", Single); (Output, "out0", Single)] }
            { name="SimpleMul3"; ports=[(Input, "in0", Single); (Input, "in1", Single); (Input, "in2", Single); (Output, "out0", Single)] }
            { name="AddConst4"; ports=[(Input, "in0", Range(3, 0)); (Output, "out0", Range(3, 0))] }
        ]

    let netlists = 
        [
            { 
                moduleName="Module0"
                nodes=
                [
                    InputPin ("in0", [ 
                        {
                            srcRange=Range(0, 0)
                            targetRange=Single
                            target=InstanceTarget ("3Add", "in0")
                        }
                        {
                            srcRange=Range(1, 1)
                            targetRange=Single
                            target=InstanceTarget ("3Add", "in1")
                        }
                        {
                            srcRange=Range(2, 2)
                            targetRange=Single
                            target=InstanceTarget ("3Add", "in2")
                        }
                        {
                            srcRange=Range(2, 2)
                            targetRange=Single
                            target=InstanceTarget ("3Mul", "in1")
                        }
                        {
                            srcRange=Range(3, 3)
                            targetRange=Single
                            target=InstanceTarget ("3Mul", "in2")
                        }
                        {
                            srcRange=Range(3, 1)
                            targetRange=Range(2, 0)
                            target=InstanceTarget ("Add-Const-4", "in0")
                        }
                    ])
                    InputPin ("in1", [ { srcRange=Single; targetRange=Single; target=PinTarget "out1" }])

                    ModuleInstance {
                        moduleName=StringIdentifier "SimpleAdd3"
                        instanceName="3Add"
                        connections=Map [
                            "out0", [{ srcRange=Single; targetRange=Single; target=InstanceTarget ("3Mul", "in0") }]
                        ]
                    }

                    ModuleInstance {
                        moduleName=StringIdentifier "SimpleMul3"
                        instanceName="3Mul"
                        connections=Map [
                            "out0", [{ srcRange=Single; targetRange=Single; target=PinTarget "out2" }]
                        ]
                    }

                    ModuleInstance {
                        moduleName=StringIdentifier "AddConst4"
                        instanceName="Add-Const-4"
                        connections=Map ["out0", [{ srcRange=Range(3, 0); targetRange=Range(3, 0); target=PinTarget ("out0")}]]
                    }

                    OutputPin ("out0")
                    OutputPin ("out1")
                    OutputPin ("out2")
                ]
            }
            { 
                moduleName="SimpleMul3"
                nodes=[
                    InputPin ("in0", [{ srcRange=Single; targetRange=Single; target=InstanceTarget ("mul0", "left") }])
                    InputPin ("in1", [{ srcRange=Single; targetRange=Single; target=InstanceTarget ("mul0", "right") }])
                    InputPin ("in2", [{ srcRange=Single; targetRange=Single; target=InstanceTarget ("mul1", "right") }])

                    ModuleInstance {
                        moduleName=BOpIdentifier VerilogAST.BOpStar
                        instanceName="mul0"
                        connections=Map ["output", [{ srcRange=Single; targetRange=Single; target=InstanceTarget ("mul1", "left") }]]
                    }

                    ModuleInstance {
                        moduleName=BOpIdentifier VerilogAST.BOpStar
                        instanceName="mul1"
                        connections=Map ["output", [{ srcRange=Single; targetRange=Single; target=PinTarget "out0" }]]
                    }

                    OutputPin ("out0")
                ]
            }
            { 
                moduleName="SimpleAdd3"
                nodes=[
                    InputPin ("in0", [{ srcRange=Single; targetRange=Single; target=InstanceTarget ("add0", "left") }])
                    InputPin ("in1", [{ srcRange=Single; targetRange=Single; target=InstanceTarget ("add0", "right") }])
                    InputPin ("in2", [{ srcRange=Single; targetRange=Single; target=InstanceTarget ("add1", "right") }])

                    ModuleInstance {
                        moduleName=BOpIdentifier VerilogAST.BOpPlus
                        instanceName="add0"
                        connections=Map ["output", [{ srcRange=Single; targetRange=Single; target=InstanceTarget ("add1", "left") }]]
                    }

                    ModuleInstance {
                        moduleName=BOpIdentifier VerilogAST.BOpPlus
                        instanceName="add1"
                        connections=Map ["output", [{ srcRange=Single; targetRange=Single; target=PinTarget "out0" }]]
                    }

                    OutputPin ("out0")
                ]
            }
            {
                moduleName="AddConst4"
                nodes=[
                    InputPin ("in0", [{ srcRange=Range(3, 0); targetRange=Range(3, 0); target=InstanceTarget ("add", "left") }])

                    Constant {| value=4; width=3; connections=[{ srcRange=Range(2, 0); targetRange=Range(2, 0); target=InstanceTarget ("add", "right") }] |}
                    ModuleInstance {
                        moduleName=BOpIdentifier VerilogAST.BOpPlus
                        instanceName="add"
                        connections=Map ["output", [{ srcRange=Range(3, 0); targetRange=Range(3, 0); target=PinTarget "out0" }]]
                    }


                    OutputPin ("out0")
                ]
            }
        ]

    
    visualiseNetlists "SVGOutput" netlists decls (Some styles) None |> ignore

    0 // return an integer exit code
