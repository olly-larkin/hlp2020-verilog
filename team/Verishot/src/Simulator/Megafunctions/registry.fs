module Verishot.Megafunctions.Registry

open Verishot.CoreTypes
open Verishot.Util
open Verishot.CoreTypes.VerilogAST
open Verishot.Simulator.Types

open Verishot.Megafunctions.Types
open Verishot.Megafunctions.Builtins

let megafunctions: Map<ModuleIdentifier, StateVar SimulationObject> =
    Map.mapValues Megafunction <| Map [ BOpIdentifier BOpPlus, addFunction; StringIdentifier "DFF", dFlipFlop]
