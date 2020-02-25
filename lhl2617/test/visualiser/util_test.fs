module Verishot.Test.VisualiserUtil

open Expecto
open FsCheck

open Verishot.Test.Util
open Verishot.SVG
open Verishot.CoreTypes
open Verishot.CoreTypes.VerilogAST
open Verishot.CoreTypes.Netlist
open Verishot.VisualiserUtil
open Verishot.VisualiserUtil.ModuleInstance
open Verishot.VisualiserUtil.Functions

type IntPair = int * int

type DifferingIntPairs = DifferingIntPairs of IntPair with
    static member op_Explicit(DifferingIntPairs i) = i

type ArbitraryModifiers =
    static member DifferingIntPairs() = 
        Arb.from<IntPair>
        |> Arb.filter (fun (c, d) -> c <> d) 
        |> Arb.convert DifferingIntPairs (fun (DifferingIntPairs x) -> x)

Arb.register<ArbitraryModifiers>() |> ignore

type VisualiserUtilFsChecks =
    static member ``truncText check length`` 
        (text: string) (PositiveInt len) =
            let gottenLen = String.length (truncText text len)
            gottenLen = 3 || gottenLen <= len 
    static member ``truncText check ellipsis``
        (text: string) (PositiveInt len) =
            let gotten = truncText text len 
            let gottenLen = String.length gotten
            match len < String.length text with
            | true -> gotten.[gottenLen - 3 ..] = "..."
            | _ -> gotten = text
     
    static member ``getRangeStr one value range``
        (a: int) =
            getRangeStr (Range (a, a)) = sprintf "[%d]" a
    static member ``getRangeStr two value range``
        (DifferingIntPairs (a, b)) =
            getRangeStr (Range (a, b)) = sprintf "[%d:%d]" a b


let truncTextTests = 
    [
        "basic1", ("a", 1), "a"
        "basic2", ("ab", 2), "ab"
        "trunc1", ("abcd", 3), "..."
        "trunc2", ("abcdef", 4), "a..."
        "notrunc1", ("abcdef", 20), "abcdef"
    ]

let getRangeStrTests = 
    let range0 = Single
    let range1 = Range (0, 0)
    let range2 = Range (42, 42)
    let range3 = Range (3, 0)
    let range4 = Range (50, 42)
    [
        "single",
            range0,
                ""
        "same 0",
            range1,
                "[0]"
        "same 42",
            range2,
                "[42]"
        "range 3-0",
            range3,
                "[3:0]"
        "range 50-42",
            range4,
                "[50:42]"
    ]

let getNumberOfInputsFromNodeTests =
    let inputPin0 = ("inputPin0", [{ srcPortIndex=0; target=PinTarget {| pinName="1"; pinIndex=0 |} }])
    let modInst0 = { moduleName=UOpIdentifier UOpMinus; instanceName="inst0"; connections=Map.empty }
    let modInst1 = { moduleName=BOpIdentifier BOpMinus; instanceName="inst1"; connections=Map.empty }
    let outputPin0 = "outputPin0"
    let node0 = InputPin inputPin0
    let node2 = ModuleInstance modInst0
    let node3 = ModuleInstance modInst1
    let node4 = OutputPin outputPin0
    [
        "inputPin",
            node0,
                0
        "outputPin",
            node4,
                1
        "UOp",
            node2,
                1
        "BOp",
            node3,
                2
    ]

let getNumberOfInputsFromNodeTestsE =
    let modInst0 = { moduleName=StringIdentifier "123"; instanceName="inst0"; connections=Map.empty }
    let node2 = ModuleInstance modInst0
    [
        "modinst invalid",
            node2,
                sprintf "ERROR: Unable to get number of inputs of module '%A'" (StringIdentifier "123")
    ]

let getNumberOfInputsFromVNodeTests =
    let modInst0 = { moduleName=StringIdentifier "modInst0"; instanceName="inst0"; connections=Map.empty }
    let node2 = ModuleInstance modInst0

    let inputPort = (Input, "inp", Single)
    let outputPort = (Output, "output", Single)
    let sampleDecl = { name="fooo"; ports=[] }
    let sampleDecl1Input = { name="fooo"; ports=[inputPort; outputPort; outputPort; outputPort] }
    let sampleDecl3Input = { name="fooo"; ports=[inputPort; inputPort; inputPort; outputPort; outputPort] }
    let defaultVNode = { node=node2 ; decl=None; svg=Group([], [], Some "bar1"); idx=1; coord=(0.,0.); props=defaultModuleInstanceProps }
    
    let declWithNoInput = { defaultVNode with decl=Some sampleDecl } 
    let declWith1Input = { defaultVNode with decl=Some sampleDecl1Input } 
    let declWith3Input = { defaultVNode with decl=Some sampleDecl3Input } 


    [
        "noinput",
            declWithNoInput,
                0
        "1 input",
            declWith1Input,
                1
        "3 input",
            declWith3Input,
                3
    ]


let getBorderBoxTests =
    let p = { defaultModuleInstanceProps with marginLeft=0.; marginRight=0.; width=0.; height=0. }
    [
        "basic", 
            ((0., 0.), p, ""), 
                Rectangle ((0., 0.), (0., 0.), [("class", "")], None)
        "xy", 
            ((1., 2.), p, ""), 
                Rectangle ((1., 2.), (0., 0.), [("class", "")], None)
        "marginLeft", 
            ((1., 2.), { p with marginLeft=42. }, ""), 
                Rectangle ((1., 2.), (42., 0.), [("class", "")], None)
        "marginRight", 
            ((1., 2.), { p with marginRight=42. }, ""), 
                Rectangle ((1., 2.), (42., 0.), [("class", "")], None)
        "width", 
            ((1., 2.), { p with width=42. }, ""), 
                Rectangle ((1., 2.), (42., 0.), [("class", "")], None)
        "height", 
            ((1., 2.), { p with height=42. }, ""), 
                Rectangle ((1., 2.), (0., 42.), [("class", "")], None)
        "comb", 
            ((1., 2.), { p with marginLeft=1.; width=1.; marginRight=1.; height=42. }, ""), 
                Rectangle ((1., 2.), (3., 42.), [("class", "")], None)
        "className", 
            ((0., 0.), p, "abc"), 
                Rectangle ((0., 0.), (0., 0.), [("class", "abc")], None)
    ]

let getActualBoxTests =
    let p = { defaultModuleInstanceProps with marginLeft=0.; marginRight=0.; width=0.; height=0. }
    [
        "basic", 
            ((0., 0.), p, ""), 
                Rectangle ((0., 0.), (0., 0.), [("class", "")], None)
        "xy", 
            ((1., 2.), p, ""), 
                Rectangle ((1., 2.), (0., 0.), [("class", "")], None)
        "marginLeft", 
            ((1., 2.), { p with marginLeft=42. }, ""), 
                Rectangle ((43., 2.), (0., 0.), [("class", "")], None)
        "marginRight", 
            ((1., 2.), { p with marginRight=42. }, ""), 
                Rectangle ((1., 2.), (0., 0.), [("class", "")], None)
        "width", 
            ((1., 2.), { p with width=42. }, ""), 
                Rectangle ((1., 2.), (42., 0.), [("class", "")], None)
        "height", 
            ((1., 2.), { p with height=42. }, ""), 
                Rectangle ((1., 2.), (0., 42.), [("class", "")], None)
        "comb", 
            ((1., 2.), { p with marginLeft=1.; width=1.; marginRight=1.; height=42. }, ""), 
                Rectangle ((2., 2.), (1., 42.), [("class", "")], None)
        "className", 
            ((0., 0.), p, "abc"), 
                Rectangle ((0., 0.), (0., 0.), [("class", "abc")], None)   
    ]

let getSVGFromNodeMapTests =
    let n = Map.empty
    let vNode1 = { node=OutputPin("foo1"); decl=None; svg=Group([], [], Some "bar1"); idx=1; coord=(0.,0.); props=defaultModuleInstanceProps }
    let vNode2 = { node=OutputPin("foo2"); decl=None; svg=Group([], [], Some "bar2"); idx=1; coord=(0.,0.); props=defaultModuleInstanceProps }
    let dn1 = Map [("foo_1", vNode1)]
    let dn2 = Map [("foo_1", vNode1); ("foo_2", vNode2) ]
    [
        "empty",
            (n, []),
                Group ([], [], None)
        "1svg",
            (dn1, []),
                Group([Group([], [], Some "bar1")], [], None)
        "2svg",
            (dn2, []),
                Group([Group([], [], Some "bar1"); Group([], [], Some "bar2")], [], None)
    ]
    
let getNodeFromNodeMapTests =
    let n = Map.empty
    let vNode1 = { node=OutputPin("foo1"); decl=None; svg=Group([], [], Some "bar1"); idx=1; coord=(0.,0.); props=defaultModuleInstanceProps }
    let vNode2 = { node=OutputPin("foo2"); decl=None; svg=Group([], [], Some "bar2"); idx=1; coord=(0.,0.); props=defaultModuleInstanceProps }
    let dn1 = Map [("foo_1", vNode1)]
    let dn2 = Map [("foo_1", vNode1); ("foo_2", vNode2) ]
    [
        "found from 1",
            (dn1, "foo_1"),
                vNode1
        "found from 2",
            (dn2, "foo_2"),
                vNode2
    ]
let getNodeFromNodeMapTestsE =
    let n = Map.empty
    let vNode1 = { node=OutputPin("foo1"); decl=None; svg=Group([], [], Some "bar1"); idx=1; coord=(0.,0.); props=defaultModuleInstanceProps }
    let vNode2 = { node=OutputPin("foo2"); decl=None; svg=Group([], [], Some "bar2"); idx=1; coord=(0.,0.); props=defaultModuleInstanceProps }
    let dn1 = Map [("foo_1", vNode1)]
    let dn2 = Map [("foo_1", vNode1); ("foo_2", vNode2) ]
    [
        "not found",
           (dn2, "foo_3"),
                sprintf "ERROR: Node '%s' not found." "foo_3"
    ]

let getDeclFromDeclMapTests =
    let n = Map.empty
    let md1 = { name="foo1"; ports=[] }
    let md2 = { name="foo2"; ports=[] }
    let mdmap1 = Map [("bar1", md1)]
    let mdmap2 = Map [("bar1", md1); ("bar2", md2)]
    [
        "found from 1",
            (mdmap1, "bar1"),
                md1
        "found from 2",
            (mdmap2, "bar2"),
                md2
    ]
let getDeclFromDeclMapTestsE =
    let n = Map.empty
    let md1 = { name="foo1"; ports=[] }
    let md2 = { name="foo2"; ports=[] }
    let mdmap1 = Map [("bar1", md1)]
    let mdmap2 = Map [("bar1", md1); ("bar2", md2)]
    [
        "not found",
            (mdmap2, "bar3"),
                sprintf "ERROR: Module declaration '%s' does not exist" "bar3"
    ]

let getPortFromModuleDeclTests =
    let decl1 = { name="foo1"; ports=[(Input, "p1", Single)] }
    let decl2 = { name="foo2"; ports=[(Input, "p1", Single); (Output, "p2", Range(1, 5))] }
    [
        "found from 1",
            (decl1, "p1"),
                (Input, "p1", Single)
        "found from 2",
            (decl2, "p2"),
                (Output, "p2", Range(1, 5))
    ]
let getPortFromModuleDeclTestsE =
    let decl2 = { name="foo2"; ports=[(Input, "p1", Single); (Input, "p2", Single)] }
    [
        "not found",
            (decl2, "p3"),
                sprintf "ERROR: Port '%s' not found on module '%s'" "p3" "foo2"
    ]

let getPortPropFromPortPropsTests =
    let pp1 = { coord=(69., 42.); index=420; range=Single }
    let pp2 = { coord=(690., 420.); index=690; range=Range(12,3) }
    let ppm1 = Map [("pp1", pp1)]
    let ppm2 = Map [("pp1", pp1); ("pp2", pp2)]
    [
        "found from 1",
            (ppm1, "pp1"),
                pp1
        "found from 2",
            (ppm2, "pp2"),
                pp2
    ]
let getPortPropFromPortPropsTestsE = 
    let pp1 = { coord=(69., 42.); index=420; range=Single }
    let pp2 = { coord=(690., 420.); index=690; range=Range(12,3) }
    let ppm1 = Map [("pp1", pp1)]
    let ppm2 = Map [("pp1", pp1); ("pp2", pp2)]
    [
        "not found",
            (ppm2, "pp3"),
                sprintf "ERROR: Port '%s' not found." "pp3"
    ]  

let getPortPropFromVNodeTests = 
    let pp1 = { coord=(69., 42.); index=420; range=Single }
    let pp2 = { coord=(690., 420.); index=690; range=Range(12,3) }
    let pp3 = { coord=(123123., 420.); index=690; range=Range(12,3) }
    let pp4 = { coord=(13., 420.); index=690; range=Range(12,3) }
    let defaultVNode = { node=OutputPin("foo1"); decl=None; svg=Group([], [], Some "bar1"); idx=1; coord=(0.,0.); props=defaultModuleInstanceProps }
    let vNode1 = { defaultVNode with props={ defaultModuleInstanceProps with inputPortProps=Map [("pp1", pp1)] } }
    let vNode2 = { defaultVNode with props={ defaultModuleInstanceProps with outputPortProps=Map [("pp2", pp2)] } }
    let vNode3 = { defaultVNode with props={ defaultModuleInstanceProps with inputPortProps=Map [("pp1", pp1); ("pp3", pp3)]; outputPortProps=Map [("pp2", pp2); ("pp4", pp4)] } }
    [
        "found from input",
            (vNode1, Input, "pp1"),
                pp1
        "found from output",
            (vNode2, Output, "pp2"),
                pp2
        "found from input2",
            (vNode3, Input, "pp3"),
                pp3
        "found from output2",
            (vNode3, Output, "pp4"),
                pp4
    ]
let getPortPropFromVNodeTestsE = 
    let pp1 = { coord=(69., 42.); index=420; range=Single }
    let pp2 = { coord=(690., 420.); index=690; range=Range(12,3) }
    let defaultVNode = { node=OutputPin("foo1"); decl=None; svg=Group([], [], Some "bar1"); idx=1; coord=(0.,0.); props=defaultModuleInstanceProps }
    let vNode1 = { defaultVNode with props={ defaultModuleInstanceProps with inputPortProps=Map [("pp1", pp1)] } }
    let vNode2 = { defaultVNode with props={ defaultModuleInstanceProps with outputPortProps=Map [("pp2", pp2)] } }
    let vNode3 = { defaultVNode with props={ defaultModuleInstanceProps with inputPortProps=Map [("pp1", pp1)]; outputPortProps=Map [("pp2", pp2)] } }
    [
        "not found from input",
            (vNode1, Input, "pp2"),
                sprintf "ERROR: Port '%s' not found." "pp2"
        "not found from output",
            (vNode2, Output, "pp1"),
                sprintf "ERROR: Port '%s' not found." "pp1"
        "not found from input2",
            (vNode3, Input, "pp4"),
                sprintf "ERROR: Port '%s' not found." "pp4"
        "not found from output2",
            (vNode3, Output, "pp3"),
                sprintf "ERROR: Port '%s' not found." "pp3"       
    ]

let getBinaryOpHTMLStringTests =
    [
        "BOpPlus",
            BOpPlus,
                "+"
        "BOpBitwiseNAnd",
            BOpBitwiseNAnd,
                "~&amp;"
        "BOpArithmeticLeftShift",
            BOpArithmeticLeftShift,
                "&lt;&lt;&lt;"
        "BOpArithmeticRightShift",
            BOpArithmeticRightShift,
                "&gt;&gt;&gt;"
    ]   

let getUnaryOpHTMLStringTests =
    [
        "UOpPlus",
            UOpPlus,
                "+"
        "UOpNAndReduce",
            UOpNAndReduce,
                "~&amp;"
    ]

[<Tests>]
let truncTextTestList =
    testList "truncText" <| 
        (truncTextTests 
         |> List.map (processIntoAsyncTestList2 truncText))

[<Tests>]
let getRangeStrTestList =
    testList "getRangeStr" <| 
        (getRangeStrTests 
         |> List.map (processIntoAsyncTestList1 getRangeStr))

[<Tests>]
let getNumberOfInputsFromNodeTestList =
    testList "getNumberOfInputsFromNode" <|
        (getNumberOfInputsFromNodeTests
         |> List.map (processIntoAsyncTestList1 getNumberOfInputsFromNode))

[<Tests>]
let getNumberOfInputsFromNodeTestListE =
    testList "getNumberOfInputsFromNodeE" <|
        (getNumberOfInputsFromNodeTestsE
         |> List.map (processIntoAsyncTestListE1 getNumberOfInputsFromNode))

[<Tests>]
let getNumberOfInputsFromVNodeTestList =
    testList "getNumberOfInputsFromVNode" <|
        (getNumberOfInputsFromVNodeTests
         |> List.map (processIntoAsyncTestList1 getNumberOfInputsFromVNode))

[<Tests>]
let getBorderBoxTestList =
    testList "getBorderBox" <|
        (getBorderBoxTests
         |> List.map (processIntoAsyncTestList3 getBorderBox))
   
[<Tests>]
let getActualBoxTestList =
    testList "getActualBox" <|
        (getActualBoxTests
         |> List.map (processIntoAsyncTestList3 getActualBox))
                 
[<Tests>]
let getSVGFromNodeMapTestList =
    testList "getSVGFromNodeMap" <|
        (getSVGFromNodeMapTests
         |> List.map (processIntoAsyncTestList2 getSVGFromNodeMap))
            
[<Tests>]
let getNodeFromNodeMapTestList =
    testList "getNodeFromNodeMap" <|
        (getNodeFromNodeMapTests
         |> List.map (processIntoAsyncTestList2 getNodeFromNodeMap))
            
[<Tests>]
let getNodeFromNodeMapTestListE =
     testList "getNodeFromNodeMapE" <|
         (getNodeFromNodeMapTestsE
         |> List.map (processIntoAsyncTestListE2 getNodeFromNodeMap))
        
[<Tests>]
let getDeclFromDeclMapTestList =
    testList "getDeclFromDeclMap" <|
        (getDeclFromDeclMapTests
         |> List.map (processIntoAsyncTestList2 getDeclFromDeclMap))
            
[<Tests>]
let getDeclFromDeclMapTestListE =
     testList "getDeclFromDeclMapE" <|
         (getDeclFromDeclMapTestsE
         |> List.map (processIntoAsyncTestListE2 getDeclFromDeclMap))
        
[<Tests>]
let getPortFromModuleDeclTestList =
    testList "getPortFromModuleDecl" <|
        (getPortFromModuleDeclTests
         |> List.map (processIntoAsyncTestList2 getPortFromModuleDecl))
            
[<Tests>]
let getPortFromModuleDeclTestListE =
    testList "getPortFromModuleDeclE" <|
        (getPortFromModuleDeclTestsE
        |> List.map (processIntoAsyncTestListE2 getPortFromModuleDecl))
        
[<Tests>]
let getPortPropFromPortPropsTestList =
    testList "getPortPropFromPortProps" <|
        (getPortPropFromPortPropsTests
        |> List.map (processIntoAsyncTestList2 getPortPropFromPortProps))
             
[<Tests>]
let getPortPropFromPortPropsTestListE =
    testList "getPortPropFromPortProps" <|
        (getPortPropFromPortPropsTestsE
        |> List.map (processIntoAsyncTestListE2 getPortPropFromPortProps))
   
[<Tests>]
let getPortPropFromVNodeTestList =
    testList "getPortPropFromVNode" <|
        (getPortPropFromVNodeTests
        |> List.map (processIntoAsyncTestList3 getPortPropFromVNode))
             
[<Tests>]
let getPortPropFromVNodeTestListE =
    testList "getPortPropFromVNode" <|
        (getPortPropFromVNodeTestsE
        |> List.map (processIntoAsyncTestListE3 getPortPropFromVNode))
       
[<Tests>]
let getBinaryOpHTMLStringTestList =
    testList "getBinaryOpHTMLString" <|
        (getBinaryOpHTMLStringTests
        |> List.map (processIntoAsyncTestList1 getBinaryOpHTMLString))

[<Tests>]
let getUnaryOpHTMLStringTestList =
    testList "getUnaryOpHTMLString" <|
        (getUnaryOpHTMLStringTests
        |> List.map (processIntoAsyncTestList1 getUnaryOpHTMLString))