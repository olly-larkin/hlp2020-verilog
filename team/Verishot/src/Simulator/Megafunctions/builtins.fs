module Verishot.Megafunctions.Builtins

open Verishot.CoreTypes
open Verishot.CoreTypes.VerilogAST
open Verishot.Megafunctions.Types
open Verishot.Simulator.Types

let no state = failwithf "Received incorrect state: %A" state

(*--  UNARY BUILT INS  --*)

// let uOpPlusFunction =
//     Combinational
//         { declaration =
//               { name = UOpIdentifier UOpPlus
//                 ports =
//                     [ Input, "inp", Range(63, 0)
//                       Output, "output", Range(63, 0) ] }
//           simulate =
//               fun inputs ->
//                   Map [ ("output", inputs.["inp"]) ] }

(*--  BINARY BUILT INS  --*)

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

(*--  CUSTOM BUILT INS  --*)

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

let counter64 =
    Stateful
        { declaration =
              { name = StringIdentifier "Counter64"
                ports =
                    [ Input, "reset", Single
                      Output, "out", Range(63, 0) ] }

          initialState = WireValState 0UL

          getNextState =
              fun state inputs ->
                  match state with
                  | WireValState v when inputs.["reset"] = 1UL ->
                      WireValState(0UL)
                  | WireValState v -> WireValState(v + 1UL)
                  | u -> no u

          getOutput =
              function
              | WireValState v -> Map [ ("out", v) ]
              | u -> no u }
