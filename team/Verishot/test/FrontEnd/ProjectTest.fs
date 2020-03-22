module Verishot.Project.Test

open Expecto
open Verishot.FrontEnd
open Verishot.Project
open Verishot.Test.Util

let getModuleTemplateTests = 
    [ 
        "basic", "foo", "module foo (\n);\nendmodule" 
    ]

[<Tests>]
let getModuleTemplateTestList =
    testList "getModuleTemplate"
    <| (getModuleTemplateTests
        |> List.map (processIntoAsyncTestList1 getModuleTemplate))


let createNewModuleTests =
    [
        "no match regex", 
            ("foo", "!"),
                Error (exitCodes.NewModuleError, "Enter a valid module name")
    ]

[<Tests>]
let createNewModuleTestList =
    testList "createNewModule"
    <| (createNewModuleTests
        |> List.map (processIntoAsyncTestList2 createNewModule))