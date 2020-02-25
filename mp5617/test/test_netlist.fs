module Verishot.Test.Netlist

open Expecto

open Verishot.Netlist
open Verishot.CoreTypes
open Verishot.CoreTypes.Netlist
open Verishot.CoreTypes.VerilogAST

let expectNetlist decls ast expectedNetlist =
    match moduleNetlist decls ast with
    | Some actualNetlist ->
        Expect.equal actualNetlist.moduleName expectedNetlist.moduleName "Should have same name"
        Expect.containsAll actualNetlist.nodes expectedNetlist.nodes "Should have all expected nodes"
        Expect.containsAll expectedNetlist.nodes actualNetlist.nodes "Should not have any extra nodes"
    | None -> Expect.equal None (Some expectedNetlist) "Should not fail"

[<Tests>]
let tests =
    testList "Netlist tests"
        [ test "Extracts single module" {
              let decls =
                  [ { name = "B"
                      ports = [] } ]

              let moduleAST =
                  { name = "A"
                    ports = []
                    items = [ ItemInstantiation("B", "theB", []) ] }

              let expectedNetlist =
                  { moduleName = "A"
                    nodes =
                        ([ ModuleInstance
                            ({ moduleName = StringIdentifier "B"
                               instanceName = "theB"
                               connections = Map.empty }) ]) }

              expectNetlist decls moduleAST expectedNetlist
          }

          test "Extracts input pin" {
              let decls = []

              let moduleAST =
                  { name = "A"
                    ports = [ "inpin" ]
                    items = [ ItemPort(Input, Single, "inpin") ] }

              let expectedNetlist =
                  { moduleName = "A"
                    nodes = [ InputPin("inpin", []) ] }

              expectNetlist decls moduleAST expectedNetlist
          }

          test "Extracts output pin" {
              let decls = []

              let moduleAST =
                  { name = "A"
                    ports = [ "outpin" ]
                    items = [ ItemPort(Output, Single, "outpin") ] }

              let expectedNetlist =
                  { moduleName = "A"
                    nodes = [ OutputPin("outpin") ] }

              expectNetlist decls moduleAST expectedNetlist
          }

          test "Connect submodule to output" {
              let decls =
                  [ { name = "B"
                      ports = [ (Output, "bOut", Single) ] } ]

              let moduleAST =
                  { name = "A"
                    ports = [ "aOut" ]
                    items =
                        [ ItemPort(Output, Single, "aOut")
                          ItemInstantiation("B", "theB", [ ExprIdentifier "aOut" ]) ] }

              let expectedNetlist =
                  { moduleName = "A"
                    nodes =
                        [ OutputPin("aOut")
                          ModuleInstance
                              ({ moduleName = StringIdentifier "B"
                                 instanceName = "theB"
                                 connections =
                                     Map
                                         [ ("bOut",
                                            [ { srcRange = Single
                                                targetRange = Single
                                                target = PinTarget "aOut" } ]) ] }) ] }

              expectNetlist decls moduleAST expectedNetlist
          }

          test "Connect input to submodule" {
              let decls =
                  [ { name = "B"
                      ports = [ (Input, "bIn", Single) ] } ]

              let moduleAST =
                  { name = "A"
                    ports = [ "aIn" ]
                    items =
                        [ ItemPort(Input, Single, "aIn")
                          ItemInstantiation("B", "theB", [ ExprIdentifier "aIn" ]) ] }

              let expectedNetlist =
                  { moduleName = "A"
                    nodes =
                        [ InputPin
                            ("aIn",
                             [ { srcRange = Single
                                 targetRange = Single
                                 target = InstanceTarget("theB", "bIn") } ])
                          ModuleInstance
                              ({ moduleName = StringIdentifier "B"
                                 instanceName = "theB"
                                 connections = Map.empty }) ] }

              expectNetlist decls moduleAST expectedNetlist
          }

          test "Connect expression to submodule" {
              let decls =
                  [ { name = "B"
                      ports = [ (Input, "bIn", Single) ] } ]

              let moduleAST =
                  { name = "A"
                    ports = [ "aIn1"; "aIn2" ]
                    items =
                        [ ItemPort(Input, Single, "aIn1")
                          ItemPort(Input, Single, "aIn2")
                          ItemInstantiation
                              ("B", "theB",
                               [ ExprBinary((ExprIdentifier "aIn1"), BOpBitwiseAnd, (ExprIdentifier "aIn2")) ]) ] }

              let expectedNetlist =
                  { moduleName = "A"
                    nodes =
                        [ InputPin
                            ("aIn1",
                             [ { srcRange = Single
                                 targetRange = Single
                                 target = InstanceTarget("BOpBitwiseAnd-0", "left") } ])
                          InputPin
                              ("aIn2",
                               [ { srcRange = Single
                                   targetRange = Single
                                   target = InstanceTarget("BOpBitwiseAnd-0", "right") } ])
                          ModuleInstance
                              ({ moduleName = BOpIdentifier BOpBitwiseAnd
                                 instanceName = "BOpBitwiseAnd-0"
                                 connections =
                                     Map
                                         [ "output",
                                           [ { srcRange = Single
                                               targetRange = Single
                                               target = InstanceTarget("theB", "bIn") } ] ] })
                          ModuleInstance
                              ({ moduleName = StringIdentifier "B"
                                 instanceName = "theB"
                                 connections = Map.empty }) ] }

              expectNetlist decls moduleAST expectedNetlist
          }

          test "Assign binary expression to output pin" {
              let decls = []

              let moduleAST =
                  { name = "A"
                    ports = [ "in1"; "in2"; "out" ]
                    items =
                        [ ItemPort(Input, Single, "in1")
                          ItemPort(Input, Single, "in2")
                          ItemPort(Output, Single, "out")
                          ItemAssign("out", ExprBinary((ExprIdentifier "in1"), BOpBitwiseAnd, (ExprIdentifier "in2"))) ] }

              let expectedNetlist =
                  { moduleName = "A"
                    nodes =
                        [ InputPin
                            ("in1",
                             [ { srcRange = Single
                                 targetRange = Single
                                 target = InstanceTarget("BOpBitwiseAnd-0", "left") } ])
                          InputPin
                              ("in2",
                               [ { srcRange = Single
                                   targetRange = Single
                                   target = InstanceTarget("BOpBitwiseAnd-0", "right") } ])
                          OutputPin("out")
                          ModuleInstance
                              ({ moduleName = BOpIdentifier BOpBitwiseAnd
                                 instanceName = "BOpBitwiseAnd-0"
                                 connections =
                                     Map
                                         [ "output",
                                           [ { srcRange = Single
                                               targetRange = Single
                                               target = PinTarget "out" } ] ] }) ] }

              expectNetlist decls moduleAST expectedNetlist
          }

          test "Assign unary expression to output pin" {
              let decls = []

              let moduleAST =
                  { name = "A"
                    ports = [ "in"; "out" ]
                    items =
                        [ ItemPort(Input, Single, "in")
                          ItemPort(Output, Single, "out")
                          ItemAssign("out", ExprUnary(UOpBitwiseNegation, (ExprIdentifier "in"))) ] }

              let expectedNetlist =
                  { moduleName = "A"
                    nodes =
                        [ InputPin
                            ("in",
                             [ { srcRange = Single
                                 targetRange = Single
                                 target = InstanceTarget("UOpBitwiseNegation-0", "input") } ])
                          OutputPin("out")
                          ModuleInstance
                              ({ moduleName = UOpIdentifier UOpBitwiseNegation
                                 instanceName = "UOpBitwiseNegation-0"
                                 connections =
                                     Map
                                         [ "output",
                                           [ { srcRange = Single
                                               targetRange = Single
                                               target = PinTarget "out" } ] ] }) ] }

              expectNetlist decls moduleAST expectedNetlist
          }

          test "Use same operator twice gives 2 instances" {
              let decls = []

              let moduleAST =
                  { name = "A"
                    ports = [ "in1"; "in2"; "out1"; "out2" ]
                    items =
                        [ ItemPort(Input, Single, "in1")
                          ItemPort(Input, Single, "in2")
                          ItemPort(Output, Single, "out1")
                          ItemPort(Output, Single, "out2")
                          ItemAssign("out1", ExprBinary((ExprIdentifier "in1"), BOpBitwiseAnd, (ExprIdentifier "in2")))
                          ItemAssign("out2", ExprBinary((ExprIdentifier "in2"), BOpBitwiseAnd, (ExprIdentifier "in1"))) ] }

              let expectedNetlist =
                  { moduleName = "A"
                    nodes =
                        [ InputPin
                            ("in1",
                             [ { srcRange = Single
                                 targetRange = Single
                                 target = InstanceTarget("BOpBitwiseAnd-0", "left") }
                               { srcRange = Single
                                 targetRange = Single
                                 target = InstanceTarget("BOpBitwiseAnd-1", "right") } ])
                          InputPin
                              ("in2",
                               [ { srcRange = Single
                                   targetRange = Single
                                   target = InstanceTarget("BOpBitwiseAnd-0", "right") }
                                 { srcRange = Single
                                   targetRange = Single
                                   target = InstanceTarget("BOpBitwiseAnd-1", "left") } ])
                          OutputPin("out1")
                          OutputPin("out2")
                          ModuleInstance
                              ({ moduleName = BOpIdentifier BOpBitwiseAnd
                                 instanceName = "BOpBitwiseAnd-0"
                                 connections =
                                     Map
                                         [ "output",
                                           [ { srcRange = Single
                                               targetRange = Single
                                               target = PinTarget "out1" } ] ] })
                          ModuleInstance
                              ({ moduleName = BOpIdentifier BOpBitwiseAnd
                                 instanceName = "BOpBitwiseAnd-1"
                                 connections =
                                     Map
                                         [ "output",
                                           [ { srcRange = Single
                                               targetRange = Single
                                               target = PinTarget "out2" } ] ] }) ] }

              expectNetlist decls moduleAST expectedNetlist
          }]
