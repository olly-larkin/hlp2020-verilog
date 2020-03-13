module rec Verishot.Test.Simulator

open Expecto
open FsCheck

open Verishot.Simulator
open Verishot.CoreTypes.Netlist
open Verishot.CoreTypes.Simulator

[<Tests>]
let simulatorTests = ftestList "Simulator" [
    test "Does not fail" {
        do simulate (NetlistInstance {moduleName=""; nodes=[]}) Map.empty []
    }]
