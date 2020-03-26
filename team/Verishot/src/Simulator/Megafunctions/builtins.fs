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
// TODO: Other reduction unary instructions (should we not include? - have since been removed from parser)
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

let bOpLogicalAndFunction =
    Combinational
        { declaration =
              { name = BOpIdentifier BOpLogicalAnd
                ports =
                    [ Input, "left", Range(63, 0)
                      Input, "right", Range(63, 0)
                      Output, "output", Range(0, 0) ] }
          simulate =
              fun inputs ->
                  Map [ ("output", if inputs.["left"] <> 0UL && inputs.["right"] <> 0UL then 1UL else 0UL) ] }

let bOpLogicalOrFunction =
    Combinational
        { declaration =
              { name = BOpIdentifier BOpLogicalOr
                ports =
                    [ Input, "left", Range(63, 0)
                      Input, "right", Range(63, 0)
                      Output, "output", Range(0, 0) ] }
          simulate =
              fun inputs ->
                  Map [ ("output", if inputs.["left"] <> 0UL || inputs.["right"] <> 0UL then 1UL else 0UL) ] }

let bOpExponentFunction =
    Combinational
        { declaration =
              { name = BOpIdentifier BOpExponent
                ports =
                    [ Input, "left", Range(63, 0)
                      Input, "right", Range(63, 0)
                      Output, "output", Range(63, 0) ] }
          simulate =
              fun inputs ->
                  Map [ ("output", uint64 (double inputs.["left"] ** double inputs.["right"])) ] }

let bOpLessThanFunction =
    Combinational
        { declaration =
              { name = BOpIdentifier BOpLessThan
                ports =
                    [ Input, "left", Range(63, 0)
                      Input, "right", Range(63, 0)
                      Output, "output", Range(0, 0) ] }
          simulate =
              fun inputs ->
                  Map [ ("output", if inputs.["left"] < inputs.["right"] then 1UL else 0UL) ] }

let bOpLessThanEqualFunction =
    Combinational
        { declaration =
              { name = BOpIdentifier BOpLessThanEqual
                ports =
                    [ Input, "left", Range(63, 0)
                      Input, "right", Range(63, 0)
                      Output, "output", Range(0, 0) ] }
          simulate =
              fun inputs ->
                  Map [ ("output", if inputs.["left"] <= inputs.["right"] then 1UL else 0UL) ] }

let bOpGreaterThanFunction =
    Combinational
        { declaration =
              { name = BOpIdentifier BOpGreaterThan
                ports =
                    [ Input, "left", Range(63, 0)
                      Input, "right", Range(63, 0)
                      Output, "output", Range(0, 0) ] }
          simulate =
              fun inputs ->
                  Map [ ("output", if inputs.["left"] > inputs.["right"] then 1UL else 0UL) ] }

let bOpGreaterThanEqualFunction =
    Combinational
        { declaration =
              { name = BOpIdentifier BOpGreaterThanEqual
                ports =
                    [ Input, "left", Range(63, 0)
                      Input, "right", Range(63, 0)
                      Output, "output", Range(0, 0) ] }
          simulate =
              fun inputs ->
                  Map [ ("output", if inputs.["left"] >= inputs.["right"] then 1UL else 0UL) ] }

let bOpBitwiseAndFunction =
    Combinational
        { declaration =
              { name = BOpIdentifier BOpBitwiseAnd
                ports =
                    [ Input, "left", Range(63, 0)
                      Input, "right", Range(63, 0)
                      Output, "output", Range(63, 0) ] }
          simulate =
              fun inputs ->
                  Map [ ("output", inputs.["left"] &&& inputs.["right"]) ] }

let bOpBitwiseOrFunction =
    Combinational
        { declaration =
              { name = BOpIdentifier BOpBitwiseOr
                ports =
                    [ Input, "left", Range(63, 0)
                      Input, "right", Range(63, 0)
                      Output, "output", Range(63, 0) ] }
          simulate =
              fun inputs ->
                  Map [ ("output", inputs.["left"] ||| inputs.["right"]) ] }

let bOpBitwiseNAndFunction =
    Combinational
        { declaration =
              { name = BOpIdentifier BOpBitwiseNAnd
                ports =
                    [ Input, "left", Range(63, 0)
                      Input, "right", Range(63, 0)
                      Output, "output", Range(63, 0) ] }
          simulate =
              fun inputs ->
                  Map [ ("output", ~~~ (inputs.["left"] &&& inputs.["right"])) ] }

let bOpBitwiseNOrFunction =
    Combinational
        { declaration =
              { name = BOpIdentifier BOpBitwiseNOr
                ports =
                    [ Input, "left", Range(63, 0)
                      Input, "right", Range(63, 0)
                      Output, "output", Range(63, 0) ] }
          simulate =
              fun inputs ->
                  Map [ ("output", ~~~ (inputs.["left"] ||| inputs.["right"])) ] }

let bOpXorFunction =
    Combinational
        { declaration =
              { name = BOpIdentifier BOpXor
                ports =
                    [ Input, "left", Range(63, 0)
                      Input, "right", Range(63, 0)
                      Output, "output", Range(63, 0) ] }
          simulate =
              fun inputs ->
                  Map [ ("output", inputs.["left"] ^^^ inputs.["right"]) ] }

let bOpXNorFunction =
    Combinational
        { declaration =
              { name = BOpIdentifier BOpXNor
                ports =
                    [ Input, "left", Range(63, 0)
                      Input, "right", Range(63, 0)
                      Output, "output", Range(63, 0) ] }
          simulate =
              fun inputs ->
                  Map [ ("output", ~~~ (inputs.["left"] ^^^ inputs.["right"])) ] }

let bOpLogicRightShiftFunction =
    Combinational
        { declaration =
              { name = BOpIdentifier BOpLogicRightShift
                ports =
                    [ Input, "left", Range(63, 0)
                      Input, "right", Range(63, 0)
                      Output, "output", Range(63, 0) ] }
          simulate =
              fun inputs ->
                  Map [ ("output", inputs.["left"] >>> int32 inputs.["right"]) ] }

let bOpLogicLeftShiftFunction =
    Combinational
        { declaration =
              { name = BOpIdentifier BOpLogicLeftShift
                ports =
                    [ Input, "left", Range(63, 0)
                      Input, "right", Range(63, 0)
                      Output, "output", Range(63, 0) ] }
          simulate =
              fun inputs ->
                  Map [ ("output", inputs.["left"] <<< int32 inputs.["right"]) ] }

(*--  CUSTOM BUILT INS  --*)

let dFlipFlop =
    Stateful
        { declaration =
              { name = StringIdentifier "DFF"
                ports =
                    [ Input, "in", Range(63, 0)
                      Input, "enable", Single
                      Output, "out", Range(63, 0) ] }

          initialState = WireValState 0UL

          getNextState = fun state inputs -> if inputs.["enable"] <> 0UL then WireValState(inputs.["in"]) else state
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

/// RAM block is 10 bit addressable
let ram =
    Stateful
        { declaration =
              { name = StringIdentifier "RAM"
                ports =
                    [ Input, "address", Range(9, 0)
                      Input, "data", Range(63, 0)
                      Input, "we", Single
                      Output, "out", Range(63, 0) ] }

          initialState = RAMState (Array.zeroCreate 1024, 0)

          getNextState =
              fun state inputs ->
                  match state with
                  | RAMState (arr, _) ->
                      if inputs.["we"] = 0UL
                      then RAMState (arr, int inputs.["address"])
                      else 
                          let address = int inputs.["address"]
                          let arr' = arr |> Array.mapi (fun i elem -> if i = address then inputs.["data"] else elem)
                          RAMState (arr', address)
                  | u -> no u

          getOutput =
              function
              | RAMState (arr, index) -> Map [ "out", arr.[index] ]
              | u -> no u }

// pin names relate to conditional operator
let mux2 =
    Combinational
        { declaration =
              { name = StringIdentifier "Mux2"
                ports =
                    [ Input, "cond", Single
                      Input, "true", Range(63, 0)
                      Input, "false", Range(63, 0)
                      Output, "output", Range(63, 0) ] }

          simulate =
              fun inputs ->
                  let out = if inputs.["cond"] <> 0UL then inputs.["true"] else inputs.["false"]
                  Map [ ("output", out) ] }

let mux4 =
    Combinational
        { declaration =
              { name = StringIdentifier "Mux4"
                ports =
                    [ Input, "select", Range(1,0)
                      Input, "in1", Range(63, 0)
                      Input, "in2", Range(63, 0)
                      Input, "in3", Range(63, 0)
                      Input, "in4", Range(63, 0)
                      Output, "output", Range(63, 0) ] }

          simulate =
              fun inputs ->
                  let out = inputs.[ sprintf "in%d" (int inputs.["select"] + 1) ]
                  Map [ ("output", out) ] }

let mux8 =
    Combinational
        { declaration =
              { name = StringIdentifier "Mux8"
                ports =
                    [ Input, "select", Range(2,0)
                      Input, "in1", Range(63, 0)
                      Input, "in2", Range(63, 0)
                      Input, "in3", Range(63, 0)
                      Input, "in4", Range(63, 0)
                      Input, "in5", Range(63, 0)
                      Input, "in6", Range(63, 0)
                      Input, "in7", Range(63, 0)
                      Input, "in8", Range(63, 0)
                      Output, "output", Range(63, 0) ] }

          simulate =
              fun inputs ->
                  let out = inputs.[ sprintf "in%d" (int inputs.["select"] + 1) ]
                  Map [ ("output", out) ] }


let shiftRegister8 =
    Stateful
        { declaration =
              { name = StringIdentifier "ShiftRegister8"
                ports =
                    [ Input, "in", Single
                      Output, "out", Range(7, 0) ] }

          initialState = WireValState 0UL

          getNextState =
              fun state inputs ->
                  match state with
                  | WireValState v -> WireValState(((v <<< 1) + inputs.["in"]) &&& 0xFFUL)
                  | u -> no u

          getOutput =
              function
              | WireValState v -> Map [ ("out", v) ]
              | u -> no u }