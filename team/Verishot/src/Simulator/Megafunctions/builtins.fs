module Verishot.Megafunctions.Builtins

open Verishot.CoreTypes
open Verishot.CoreTypes.VerilogAST
open Verishot.Megafunctions.Types
open Verishot.Simulator.Types

let no state = failwithf "Received incorrect state: %A" state

let addFunction =
    Combinational
        { declaration =
              { name = BOpIdentifier BOpPlus
                ports =
                    [ Input, "left", Range(63, 0)
                      Input, "right", Range(63, 0)
                      Output, "output", Range(63, 0) ] }
          simulate =
              fun inputs ->
                  Map [ ("output", inputs.["left"] + inputs.["right"]) ] }

let dFlipFlop =
    Stateful
        { declaration =
              { name = StringIdentifier "DFF"
                ports =
                    [ Input, "in", Range(63, 0)
                      Output, "out", Range(63, 0) ] }

          initialState = WireValState 0UL

          getNextState = fun _ inputs -> WireValState(inputs.["in"])
          getOutput =
              function
              | WireValState v -> Map [ ("out", v) ]
              | u -> no u }
