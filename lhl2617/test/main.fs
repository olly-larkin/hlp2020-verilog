module Verishot.Test.Main

// open Verishot.Test.VisualiserUtil
open Expecto
open FsCheck
open Verishot.Test.SVG
open Verishot.Test.VisualiserUtil
open Verishot.Test.Pin

open Verishot.VisualiserUtil
open Verishot.VisualiserUtil.ModuleInstance
open Verishot.VisualiserUtil.Functions



[<EntryPoint>]
let main argv =
    Tests.runTestsInAssembly defaultConfig argv |> ignore
    Check.QuickAll<VisualiserUtilFsChecks>() |> ignore
    0
