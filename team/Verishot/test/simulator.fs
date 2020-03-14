module rec Verishot.Test.Simulator

open Expecto

open Verishot.CoreTypes
open Verishot.CoreTypes.VerilogAST
open Verishot.Simulator.Simulate
open Verishot.Simulator.Types
open Verishot.Megafunctions.Registry

let expectSameElements actual expected =
    Expect.containsAll actual expected "Should have all expected nodes"
    Expect.containsAll expected actual "Should not have any extra nodes"

[<Tests>]
let simulatorTests =
    ftestList "Simulator"
        [ test "Input to Output single" {
              let netlistIn =
                  { moduleName = "m"
                    nodes =
                        [ InputPin "in"
                          OutputPin
                              ("out",
                               [ { source = PinEndpoint("in")
                                   srcRange = Single
                                   targetRange = Single } ]) ] }

              let actual =
                  getNetlistOutput netlistIn Map.empty None
                      (Map [ ("in", 1UL) ])

              Expect.equal actual.["out"] 1UL "Is equal"
          }

          test "Input to Output range" {
              let netlistIn =
                  { moduleName = "m"
                    nodes =
                        [ InputPin "in"
                          OutputPin
                              ("out",
                               [ { source = PinEndpoint("in")
                                   srcRange = Range(5, 0)
                                   targetRange = Range(5, 0) } ]) ] }

              let actual =
                  getNetlistOutput netlistIn Map.empty None
                      (Map [ ("in", 4UL) ])

              Expect.equal actual.["out"] 4UL "Is Equal"
          }

          test "Input to Output shift left" {
              let netlistIn =
                  { moduleName = "m"
                    nodes =
                        [ InputPin "in"
                          OutputPin
                              ("out",
                               [ { source = PinEndpoint("in")
                                   srcRange = Range(5, 0)
                                   targetRange = Range(6, 1) } ]) ] }

              let actual =
                  getNetlistOutput netlistIn Map.empty None
                      (Map [ ("in", 4UL) ])

              Expect.equal actual.["out"] 8UL "Is Equal"
          }

          test "Input to Output shift right" {
              let netlistIn =
                  { moduleName = "m"
                    nodes =
                        [ InputPin "in"
                          OutputPin
                              ("out",
                               [ { source = PinEndpoint("in")
                                   srcRange = Range(6, 1)
                                   targetRange = Range(5, 0) } ]) ] }

              let actual =
                  getNetlistOutput netlistIn Map.empty None
                      (Map [ ("in", 8UL) ])

              Expect.equal actual.["out"] 4UL "Is Equal"
          }

          test "2 Inputs to Output" {
              let netlistIn =
                  { moduleName = "m"
                    nodes =
                        [ InputPin "in1"
                          InputPin "in2"
                          OutputPin
                              ("out",
                               [ { source = PinEndpoint("in1")
                                   srcRange = Range(3, 0)
                                   targetRange = Range(3, 0) }
                                 { source = PinEndpoint("in2")
                                   srcRange = Range(3, 0)
                                   targetRange = Range(7, 4) } ]) ] }

              let actual =
                  getNetlistOutput netlistIn Map.empty None
                      (Map
                          [ ("in1", 0x5UL)
                            ("in2", 0x3UL) ])

              Expect.equal actual.["out"] 0x35UL "Is Equal"
          }

          test "Add 2 Inputs" {
              let netlistIn =
                  { moduleName = "m"
                    nodes =
                        [ InputPin "in1"
                          InputPin "in2"
                          ModuleInstance
                              ({ moduleName = BOpIdentifier BOpPlus
                                 instanceName = "BOpPlus-0"
                                 connections =
                                     Map
                                         [ "left",
                                           [ { srcRange = Range(63, 0)
                                               targetRange = Range(63, 0)
                                               source = PinEndpoint "in1" } ]
                                           "right",
                                           [ { srcRange = Range(63, 0)
                                               targetRange = Range(63, 0)
                                               source = PinEndpoint "in2" } ]] })
                          OutputPin
                              ("out",
                               [ { source =
                                       InstanceEndpoint("BOpPlus-0", "output")
                                   srcRange = Range(63, 0)
                                   targetRange = Range(63, 0) } ]) ] }

              let actual =
                  getNetlistOutput netlistIn megafunctions None
                      (Map
                          [ ("in1", 5UL)
                            ("in2", 3UL) ])

              Expect.equal actual.["out"] 8UL "Is Equal"
          } ]
