module Verishot.Megafunctions.Builtins

open Verishot.CoreTypes
open Verishot.CoreTypes.VerilogAST
open Verishot.Simulator.Types
open Verishot.Megafunctions.Types

let addFunction =
    Combinational
        { declaration =
              { name = BOpIdentifier BOpPlus
                ports =
                    [ Input, "left", Range(63, 0)
                      Input, "right", Range(63, 0)
                      Output, "output", Range(63, 0) ] }
          simulate =
              (fun inputs ->
                  Map [ ("output", inputs.["left"] + inputs.["right"]) ]) }
