module Verishot.Megafunctions.Registry

open Verishot.CoreTypes
open Verishot.CoreTypes.VerilogAST
open Verishot.Megafunctions.Builtins

open Verishot.Megafunctions.Types
open Verishot.Simulator.Types
open Verishot.Util

let makeMegafunctions =
    List.map (function
        | Combinational mf ->
            (mf.declaration.name, Megafunction(Combinational mf))
        | Stateful mf -> (mf.declaration.name, Megafunction(Stateful mf)))
    >> List.toMap

let megafunctions: Map<ModuleIdentifier, StateVar SimulationObject> =
    makeMegafunctions [ addFunction; dFlipFlop; counter64 ]
