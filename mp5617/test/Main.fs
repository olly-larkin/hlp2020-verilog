module Verishot.Test.Main

open Verishot.Test.Netlist
open Expecto

[<EntryPoint>]
let main argv =
    Tests.runTestsInAssembly defaultConfig argv
