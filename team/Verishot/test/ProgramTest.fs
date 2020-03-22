module Verishot.Program.Test

open Expecto
open Verishot.Test.Util
open Verishot.Main
open Verishot.FrontEnd
open Verishot.Project

let argProcessorTests = [
    "no arg",
        ([||]),
            Ok intro
    "help", 
        ([|"--help"|]),
            Ok usageGuide
    "invalid flag",
        ([|"--foo"|]),
            Error (exitCodes.InvalidCmd, invalidCmd)     
]

[<Tests>]
let argProcesserTestList =
    testList "argProcessor" <|
        (argProcessorTests
         |> List.map (processIntoAsyncTestList1 argProcessor))