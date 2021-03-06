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
    makeMegafunctions [

        uOpPlusFunction
        uOpMinusFunction
        uOpBangFunction
        uOpBitwiseNegationFunction
        uOpAndReduceFunction

        bOpPlusFunction
        bOpMinusFunction
        bOpStarFunction
        bOpDivFunction
        bOpModFunction
        bOpEqualsFunction
        bOpBangEqualsFunction
        bOpLogicalAndFunction
        bOpLogicalOrFunction
        bOpExponentFunction
        bOpLessThanFunction
        bOpLessThanEqualFunction
        bOpGreaterThanFunction
        bOpGreaterThanEqualFunction
        bOpBitwiseAndFunction
        bOpBitwiseOrFunction
        bOpBitwiseNAndFunction
        bOpBitwiseNOrFunction
        bOpXorFunction
        bOpXNorFunction
        bOpLogicRightShiftFunction
        bOpLogicLeftShiftFunction

        dFlipFlop
        counter64
        ram
        mux2
        mux4
        mux8
        shiftRegister8
        
    ]

/// Names of built-in modules that are not bOp or uOp.
/// This is to prevent linking (in visualiser) modules that are built-in
let builtinNames = 
    megafunctions
    |> Map.toList
    |> List.map (fst)
    |> List.choose (function
    | StringIdentifier x -> Some x
    | _ -> None)
