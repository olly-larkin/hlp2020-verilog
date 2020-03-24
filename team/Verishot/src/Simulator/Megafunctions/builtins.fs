module Verishot.Megafunctions.Builtins

open Verishot.CoreTypes
open Verishot.CoreTypes.VerilogAST
open Verishot.Megafunctions.Types
open Verishot.Simulator.Types

let no state = failwithf "Received incorrect state: %A" state

(*--  UNARY BUILT INS  --*)

let uOpPlusFunction =
    Combinational
        { declaration =
              { name = UOpIdentifier UOpPlus
                ports =
                    [ Input, "input", Range(63, 0)
                      Output, "output", Range(63, 0) ] }
          simulate =
              fun inputs ->
                  Map [ ("output", inputs.["input"]) ] }

let uOpMinusFunction =
    Combinational
        { declaration =
              { name = UOpIdentifier UOpMinus
                ports =
                    [ Input, "input", Range(63, 0)
                      Output, "output", Range(63, 0) ] }
          simulate =
              fun inputs ->
                  Map [ ("output", ~~~ inputs.["input"] + 1UL) ] }

let uOpBangFunction =
    Combinational
        { declaration =
              { name = UOpIdentifier UOpBang
                ports =
                    [ Input, "input", Range(63, 0)
                      Output, "output", Range(0, 0) ] }
          simulate =
              fun inputs ->
                  Map [ ("output", if inputs.["input"] = 0UL then 1UL else 0UL) ] }    

let uOpBitwiseNegationFunction =
    Combinational
        { declaration =
              { name = UOpIdentifier UOpBitwiseNegation
                ports =
                    [ Input, "input", Range(63, 0)
                      Output, "output", Range(63, 0) ] }
          simulate =
              fun inputs ->
                  Map [ ("output", ~~~inputs.["input"]) ] }     

// TODO: this one doesn't work (would be hard to make it to - would have to know incomming bus width)
// TODO: Other reduction unary instructions (should we not include?)
let uOpAndReduceFunction =
    Combinational
        { declaration =
              { name = UOpIdentifier UOpAndReduce
                ports =
                    [ Input, "input", Range(63, 0)
                      Output, "output", Range(0, 0) ] }
          simulate =
              fun inputs ->
                  Map [ ("output", if inputs.["input"] = 0UL - 1UL then 1UL else 0UL) ] }      

(*--  BINARY BUILT INS  --*)

let bOpPlusFunction =
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

let bOpMinusFunction =
    Combinational
        { declaration =
              { name = BOpIdentifier BOpMinus
                ports =
                    [ Input, "left", Range(63, 0)
                      Input, "right", Range(63, 0)
                      Output, "output", Range(63, 0) ] }
          simulate =
              fun inputs ->
                  Map [ ("output", inputs.["left"] - inputs.["right"]) ] }

let bOpStarFunction =
    Combinational
        { declaration =
              { name = BOpIdentifier BOpStar
                ports =
                    [ Input, "left", Range(63, 0)
                      Input, "right", Range(63, 0)
                      Output, "output", Range(63, 0) ] }
          simulate =
              fun inputs ->
                  Map [ ("output", inputs.["left"] * inputs.["right"]) ] }

let bOpDivFunction =
    Combinational
        { declaration =
              { name = BOpIdentifier BOpDiv
                ports =
                    [ Input, "left", Range(63, 0)
                      Input, "right", Range(63, 0)
                      Output, "output", Range(63, 0) ] }
          simulate =
              fun inputs ->
                  Map [ ("output", inputs.["left"] / inputs.["right"]) ] }

let bOpModFunction =
    Combinational
        { declaration =
              { name = BOpIdentifier BOpMod
                ports =
                    [ Input, "left", Range(63, 0)
                      Input, "right", Range(63, 0)
                      Output, "output", Range(63, 0) ] }
          simulate =
              fun inputs ->
                  Map [ ("output", inputs.["left"] % inputs.["right"]) ] }

let bOpEqualsFunction =
    Combinational
        { declaration =
              { name = BOpIdentifier BOpEquals
                ports =
                    [ Input, "left", Range(63, 0)
                      Input, "right", Range(63, 0)
                      Output, "output", Range(0, 0) ] }
          simulate =
              fun inputs ->
                  Map [ ("output", if inputs.["left"] = inputs.["right"] then 1UL else 0UL) ] }

let bOpBangEqualsFunction =
    Combinational
        { declaration =
              { name = BOpIdentifier BOpBangEquals
                ports =
                    [ Input, "left", Range(63, 0)
                      Input, "right", Range(63, 0)
                      Output, "output", Range(0, 0) ] }
          simulate =
              fun inputs ->
                  Map [ ("output", if inputs.["left"] <> inputs.["right"] then 1UL else 0UL) ] }

(*--  CUSTOM BUILT INS  --*)
// TODO: no point adding these currently bc they won't work until netlist is changed

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
