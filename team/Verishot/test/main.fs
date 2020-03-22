module Verishot.Test.Main

open Expecto
open FsCheck
open Verishot.Test.VisualiserUtil

[<EntryPoint>]
let main argv =
    runTestsInAssembly defaultConfig argv |> ignore
    Check.QuickAll<VisualiserUtilFsChecks>() |> ignore
    0
