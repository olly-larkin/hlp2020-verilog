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
            { name="Module0"; ports=[(Input, "in0", Range(3, 0)); (Input, "in1", Single); (Output, "out0", Range(3, 0)); (Output, "out1", Single )] }
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
                    InputPin ("in0", [])
                    InputPin ("in1", [])

                    ModuleInstance {
                        moduleName=StringIdentifier "SimpleAdd3"
                        instanceName="3Add"
                        connections=Map []
                    }

                    ModuleInstance {
                        moduleName=StringIdentifier "SimpleMul3"
                        instanceName="3Mul"
                        connections=Map []
                    }

                    OutputPin ("out0")
                    OutputPin ("out1")
                ]
            }
            { 
                moduleName="SimpleAdd3"
                nodes=[
                    InputPin ("in0", [])
                    InputPin ("in1", [])               
                    InputPin ("in2", [])

                    ModuleInstance {
                        moduleName=BOpIdentifier VerilogAST.BOpPlus
                        instanceName="add"
                        connections=Map []
                    }

                    ModuleInstance {
                        moduleName=BOpIdentifier VerilogAST.BOpPlus
                        instanceName="add"
                        connections=Map []
                    }

                    OutputPin ("out0")
                ]
            }
            { 
                moduleName="SimpleMul3"
                nodes=[
                    InputPin ("in0", [])
                    InputPin ("in1", [])               
                    InputPin ("in2", [])

                    ModuleInstance {
                        moduleName=BOpIdentifier VerilogAST.BOpPlus
                        instanceName="add"
                        connections=Map []
                    }

                    ModuleInstance {
                        moduleName=BOpIdentifier VerilogAST.BOpPlus
                        instanceName="add"
                        connections=Map []
                    }

                    OutputPin ("out0")
                ]
            }
            {
                moduleName="AddConst4"
                nodes=[
                    InputPin ("in0", [])

                    ModuleInstance {
                        moduleName=BOpIdentifier VerilogAST.BOpPlus
                        instanceName="add"
                        connections=Map []
                    }


                    OutputPin ("out0")
                ]
            }
        ]

    
    visualiseNetlists "SVGOutput" netlists decls (Some styles) None |> ignore

    0 // return an integer exit code
