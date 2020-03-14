module rec Verishot.Test.Simulator

open Expecto

open Verishot.CoreTypes
open Verishot.Simulator.Simulate
open Verishot.Simulator.Types

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

              Expect.equal actual.["out"] 1UL
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

              Expect.equal actual.["out"] 4UL
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

              Expect.equal actual.["out"] 8UL
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

              Expect.equal actual.["out"] 4UL
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
                      (Map [ ("in1", 0x5UL); ("in2", 0x3UL) ])

              Expect.equal actual.["out"] 0x35UL
          } ]
