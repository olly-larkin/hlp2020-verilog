module rec Verishot.Test.Netlist

open Expecto
open Expecto.ExpectoFsCheck
open FsCheck

open Verishot.CoreTypes
open Verishot.CoreTypes.Netlist
open Verishot.CoreTypes.VerilogAST
open Verishot.Netlist

module Netlist = Verishot.Netlist

let expectNetlist decls ast expectedNetlist =
    let actualNetlist = moduleNetlist decls ast
    Expect.equal actualNetlist.moduleName expectedNetlist.moduleName
        "Should have same name"
    Expect.containsAll actualNetlist.nodes expectedNetlist.nodes
        "Should have all expected nodes"
    Expect.containsAll expectedNetlist.nodes actualNetlist.nodes
        "Should not have any extra nodes"

[<Tests>]
let fullModuleTests =
    testList "Netlist"
        [ testList "Full module tests"
              [ test "Extracts single module" {
                    let decls =
                        [ { name = StringIdentifier "B"
                            ports = [] } ]

                    let moduleAST =
                        { name = "A"
                          ports = []
                          items = [ ItemInstantiation("B", "theB", []) ] }

                    let expectedNetlist =
                        { moduleName = "A"
                          nodes =
                              ([ ModuleInstance
                                  ({ moduleName = StringIdentifier "B"
                                     instanceName = "theB"
                                     connections = Map.empty }) ]) }

                    expectNetlist decls moduleAST expectedNetlist
                }

                test "Extracts input pin" {
                    let decls = []

                    let moduleAST =
                        { name = "A"
                          ports = [ "inpin" ]
                          items = [ ItemPort(Input, Single, "inpin") ] }

                    let expectedNetlist =
                        { moduleName = "A"
                          nodes = [ InputPin("inpin", []) ] }

                    expectNetlist decls moduleAST expectedNetlist
                }

                test "Extracts output pin" {
                    let decls = []

                    let moduleAST =
                        { name = "A"
                          ports = [ "outpin" ]
                          items = [ ItemPort(Output, Single, "outpin") ] }

                    let expectedNetlist =
                        { moduleName = "A"
                          nodes = [ OutputPin("outpin") ] }

                    expectNetlist decls moduleAST expectedNetlist
                }

                test "Connect submodule to output" {
                    let decls =
                        [ { name = StringIdentifier "B"
                            ports = [ (Output, "bOut", Single) ] } ]

                    let moduleAST =
                        { name = "A"
                          ports = [ "aOut" ]
                          items =
                              [ ItemPort(Output, Single, "aOut")
                                ItemInstantiation
                                    ("B", "theB", [ ExprIdentifier "aOut" ]) ] }

                    let expectedNetlist =
                        { moduleName = "A"
                          nodes =
                              [ OutputPin("aOut")
                                ModuleInstance
                                    ({ moduleName = StringIdentifier "B"
                                       instanceName = "theB"
                                       connections =
                                           Map
                                               [ ("bOut",
                                                  [ { srcRange = Single
                                                      targetRange = Single
                                                      target = PinTarget "aOut" } ]) ] }) ] }

                    expectNetlist decls moduleAST expectedNetlist
                }

                test "Connect input to submodule" {
                    let decls =
                        [ { name = StringIdentifier "B"
                            ports = [ (Input, "bIn", Single) ] } ]

                    let moduleAST =
                        { name = "A"
                          ports = [ "aIn" ]
                          items =
                              [ ItemPort(Input, Single, "aIn")
                                ItemInstantiation
                                    ("B", "theB", [ ExprIdentifier "aIn" ]) ] }

                    let expectedNetlist =
                        { moduleName = "A"
                          nodes =
                              [ InputPin
                                  ("aIn",
                                   [ { srcRange = Single
                                       targetRange = Single
                                       target = InstanceTarget("theB", "bIn") } ])
                                ModuleInstance
                                    ({ moduleName = StringIdentifier "B"
                                       instanceName = "theB"
                                       connections = Map.empty }) ] }

                    expectNetlist decls moduleAST expectedNetlist
                }

                test "Connect expression to submodule" {
                    let decls =
                        [ { name = StringIdentifier "B"
                            ports = [ (Input, "bIn", Single) ] } ]

                    let moduleAST =
                        { name = "A"
                          ports = [ "aIn1"; "aIn2" ]
                          items =
                              [ ItemPort(Input, Single, "aIn1")
                                ItemPort(Input, Single, "aIn2")
                                ItemInstantiation
                                    ("B", "theB",
                                     [ ExprBinary
                                         ((ExprIdentifier "aIn1"), BOpBitwiseAnd,
                                          (ExprIdentifier "aIn2")) ]) ] }

                    let expectedNetlist =
                        { moduleName = "A"
                          nodes =
                              [ InputPin
                                  ("aIn1",
                                   [ { srcRange = Single
                                       targetRange = Single
                                       target =
                                           InstanceTarget
                                               ("BOpBitwiseAnd-0", "left") } ])
                                InputPin
                                    ("aIn2",
                                     [ { srcRange = Single
                                         targetRange = Single
                                         target =
                                             InstanceTarget
                                                 ("BOpBitwiseAnd-0", "right") } ])
                                ModuleInstance
                                    ({ moduleName = BOpIdentifier BOpBitwiseAnd
                                       instanceName = "BOpBitwiseAnd-0"
                                       connections =
                                           Map
                                               [ "output",
                                                 [ { srcRange = Single
                                                     targetRange = Single
                                                     target =
                                                         InstanceTarget
                                                             ("theB", "bIn") } ] ] })
                                ModuleInstance
                                    ({ moduleName = StringIdentifier "B"
                                       instanceName = "theB"
                                       connections = Map.empty }) ] }

                    expectNetlist decls moduleAST expectedNetlist
                }

                test "Assign binary expression to output pin" {
                    let decls = []

                    let moduleAST =
                        { name = "A"
                          ports = [ "in1"; "in2"; "out" ]
                          items =
                              [ ItemPort(Input, Single, "in1")
                                ItemPort(Input, Single, "in2")
                                ItemPort(Output, Single, "out")
                                ItemAssign
                                    ("out",
                                     ExprBinary
                                         ((ExprIdentifier "in1"), BOpBitwiseAnd,
                                          (ExprIdentifier "in2"))) ] }

                    let expectedNetlist =
                        { moduleName = "A"
                          nodes =
                              [ InputPin
                                  ("in1",
                                   [ { srcRange = Single
                                       targetRange = Single
                                       target =
                                           InstanceTarget
                                               ("BOpBitwiseAnd-0", "left") } ])
                                InputPin
                                    ("in2",
                                     [ { srcRange = Single
                                         targetRange = Single
                                         target =
                                             InstanceTarget
                                                 ("BOpBitwiseAnd-0", "right") } ])
                                OutputPin("out")
                                ModuleInstance
                                    ({ moduleName = BOpIdentifier BOpBitwiseAnd
                                       instanceName = "BOpBitwiseAnd-0"
                                       connections =
                                           Map
                                               [ "output",
                                                 [ { srcRange = Single
                                                     targetRange = Single
                                                     target = PinTarget "out" } ] ] }) ] }

                    expectNetlist decls moduleAST expectedNetlist
                }

                test "Assign unary expression to output pin" {
                    let decls = []

                    let moduleAST =
                        { name = "A"
                          ports = [ "in"; "out" ]
                          items =
                              [ ItemPort(Input, Single, "in")
                                ItemPort(Output, Single, "out")
                                ItemAssign
                                    ("out",
                                     ExprUnary
                                         (UOpBitwiseNegation,
                                          (ExprIdentifier "in"))) ] }

                    let expectedNetlist =
                        { moduleName = "A"
                          nodes =
                              [ InputPin
                                  ("in",
                                   [ { srcRange = Single
                                       targetRange = Single
                                       target =
                                           InstanceTarget
                                               ("UOpBitwiseNegation-0", "input") } ])
                                OutputPin("out")
                                ModuleInstance
                                    ({ moduleName =
                                           UOpIdentifier UOpBitwiseNegation
                                       instanceName = "UOpBitwiseNegation-0"
                                       connections =
                                           Map
                                               [ "output",
                                                 [ { srcRange = Single
                                                     targetRange = Single
                                                     target = PinTarget "out" } ] ] }) ] }

                    expectNetlist decls moduleAST expectedNetlist
                }

                test "Use same operator twice gives 2 instances" {
                    let decls = []

                    let moduleAST =
                        { name = "A"
                          ports = [ "in1"; "in2"; "out1"; "out2" ]
                          items =
                              [ ItemPort(Input, Single, "in1")
                                ItemPort(Input, Single, "in2")
                                ItemPort(Output, Single, "out1")
                                ItemPort(Output, Single, "out2")
                                ItemAssign
                                    ("out1",
                                     ExprBinary
                                         ((ExprIdentifier "in1"), BOpBitwiseAnd,
                                          (ExprIdentifier "in2")))
                                ItemAssign
                                    ("out2",
                                     ExprBinary
                                         ((ExprIdentifier "in2"), BOpBitwiseAnd,
                                          (ExprIdentifier "in1"))) ] }

                    let expectedNetlist =
                        { moduleName = "A"
                          nodes =
                              [ InputPin
                                  ("in1",
                                   [ { srcRange = Single
                                       targetRange = Single
                                       target =
                                           InstanceTarget
                                               ("BOpBitwiseAnd-0", "left") }
                                     { srcRange = Single
                                       targetRange = Single
                                       target =
                                           InstanceTarget
                                               ("BOpBitwiseAnd-1", "right") } ])
                                InputPin
                                    ("in2",
                                     [ { srcRange = Single
                                         targetRange = Single
                                         target =
                                             InstanceTarget
                                                 ("BOpBitwiseAnd-0", "right") }
                                       { srcRange = Single
                                         targetRange = Single
                                         target =
                                             InstanceTarget
                                                 ("BOpBitwiseAnd-1", "left") } ])
                                OutputPin("out1")
                                OutputPin("out2")
                                ModuleInstance
                                    ({ moduleName = BOpIdentifier BOpBitwiseAnd
                                       instanceName = "BOpBitwiseAnd-0"
                                       connections =
                                           Map
                                               [ "output",
                                                 [ { srcRange = Single
                                                     targetRange = Single
                                                     target = PinTarget "out1" } ] ] })
                                ModuleInstance
                                    ({ moduleName = BOpIdentifier BOpBitwiseAnd
                                       instanceName = "BOpBitwiseAnd-1"
                                       connections =
                                           Map
                                               [ "output",
                                                 [ { srcRange = Single
                                                     targetRange = Single
                                                     target = PinTarget "out2" } ] ] }) ] }

                    expectNetlist decls moduleAST expectedNetlist
                }

                test "Connect sized constant to module input" {
                    let decls =
                        [ { name = StringIdentifier "B"
                            ports = [ (Input, "bIn", Range(2, 0)) ] } ]

                    let moduleAST =
                        { name = "A"
                          ports = []
                          items =
                              [ ItemInstantiation
                                  ("B", "theB", [ ExprNumber(Some 3, 1) ]) ] }

                    let expectedNetlist =
                        { moduleName = "A"
                          nodes =
                              [ ModuleInstance
                                  ({ moduleName = StringIdentifier "B"
                                     instanceName = "theB"
                                     connections = Map.empty })

                                Constant
                                    {| width = 3
                                       value = 1
                                       connections =
                                           [ { srcRange = Range(2, 0)
                                               targetRange = Range(2, 0)
                                               target =
                                                   InstanceTarget("theB", "bIn") } ] |} ] }

                    expectNetlist decls moduleAST expectedNetlist
                }

                test "Connect 1-bit sized constant to module input" {
                    let decls =
                        [ { name = StringIdentifier "B"
                            ports = [ (Input, "bIn", Single) ] } ]

                    let moduleAST =
                        { name = "A"
                          ports = []
                          items =
                              [ ItemInstantiation
                                  ("B", "theB", [ ExprNumber(Some 1, 1) ]) ] }

                    let expectedNetlist =
                        { moduleName = "A"
                          nodes =
                              [ ModuleInstance
                                  ({ moduleName = StringIdentifier "B"
                                     instanceName = "theB"
                                     connections = Map.empty })

                                Constant
                                    {| width = 1
                                       value = 1
                                       connections =
                                           [ { srcRange = Single
                                               targetRange = Single
                                               target =
                                                   InstanceTarget("theB", "bIn") } ] |} ] }

                    expectNetlist decls moduleAST expectedNetlist
                }

                test "Wire single input to output through wire" {
                    let decls = []

                    let moduleAST =
                        { name = "A"
                          ports = []
                          items =
                              [ ItemPort(Input, Single, "in")
                                ItemPort(Output, Single, "out")
                                ItemWireDecl(Single, "between")
                                ItemAssign("between", ExprIdentifier "in")
                                ItemAssign("out", ExprIdentifier "between") ] }

                    let expectedNetlist =
                        { moduleName = "A"
                          nodes =
                              [ InputPin
                                  ("in",
                                   [ { srcRange = Single
                                       targetRange = Single
                                       target = PinTarget "out" } ])
                                OutputPin("out") ] }

                    expectNetlist decls moduleAST expectedNetlist
                }

                test "Wire bus input to output through wire" {
                    let decls = []

                    let moduleAST =
                        { name = "A"
                          ports = []
                          items =
                              [ ItemPort(Input, Range(5, 0), "in")
                                ItemPort(Output, Range(5, 0), "out")
                                ItemWireDecl(Range(5, 0), "between")
                                ItemAssign("between", ExprIdentifier "in")
                                ItemAssign("out", ExprIdentifier "between") ] }

                    let expectedNetlist =
                        { moduleName = "A"
                          nodes =
                              [ InputPin
                                  ("in",
                                   [ { srcRange = Range(5, 0)
                                       targetRange = Range(5, 0)
                                       target = PinTarget "out" } ])
                                OutputPin("out") ] }

                    expectNetlist decls moduleAST expectedNetlist
                }

                test "Wire output to 2 instances" {
                    let decls =
                        [ { name = StringIdentifier "B"
                            ports = [ (Output, "bOut", Single) ] }
                          { name = StringIdentifier "C"
                            ports = [ (Input, "cIn", Single) ] } ]

                    let moduleAST =
                        { name = "A"
                          ports = []
                          items =
                              [ ItemWireDecl(Single, "between")
                                ItemInstantiation
                                    ("B", "theB", [ ExprIdentifier "between" ])
                                ItemInstantiation
                                    ("C", "C1", [ ExprIdentifier "between" ])
                                ItemInstantiation
                                    ("C", "C2", [ ExprIdentifier "between" ]) ] }

                    let expectedNetlist =
                        { moduleName = "A"
                          nodes =
                              [ ModuleInstance
                                  ({ moduleName = StringIdentifier "B"
                                     instanceName = "theB"
                                     connections =
                                         Map
                                             [ "bOut",
                                               [ { srcRange = Single
                                                   targetRange = Single
                                                   target =
                                                       InstanceTarget
                                                           ("C1", "cIn") }
                                                 { srcRange = Single
                                                   targetRange = Single
                                                   target =
                                                       InstanceTarget
                                                           ("C2", "cIn") }] ] })
                                ModuleInstance
                                    ({ moduleName = StringIdentifier "C"
                                       instanceName = "C1"
                                       connections = Map.empty })
                                ModuleInstance
                                    ({ moduleName = StringIdentifier "C"
                                       instanceName = "C2"
                                       connections = Map.empty }) ] }

                    expectNetlist decls moduleAST expectedNetlist
                }

                test "Wire 2 modules together with bus" {
                    let decls =
                        [ { name = StringIdentifier "B"
                            ports = [ (Output, "bOut", Range(5, 0)) ] }
                          { name = StringIdentifier "C"
                            ports = [ (Input, "cIn", Range(5, 0)) ] } ]

                    let moduleAST =
                        { name = "A"
                          ports = []
                          items =
                              [ ItemWireDecl(Range(5, 0), "between")
                                ItemInstantiation
                                    ("B", "theB", [ ExprIdentifier "between" ])
                                ItemInstantiation
                                    ("C", "theC", [ ExprIdentifier "between" ]) ] }

                    let expectedNetlist =
                        { moduleName = "A"
                          nodes =
                              [ ModuleInstance
                                  ({ moduleName = StringIdentifier "B"
                                     instanceName = "theB"
                                     connections =
                                         Map
                                             [ "bOut",
                                               [ { srcRange = Range(5, 0)
                                                   targetRange = Range(5, 0)
                                                   target =
                                                       InstanceTarget
                                                           ("theC", "cIn") } ] ] })
                                ModuleInstance
                                    ({ moduleName = StringIdentifier "C"
                                       instanceName = "theC"
                                       connections = Map.empty }) ] }

                    expectNetlist decls moduleAST expectedNetlist
                }

                test "Wire 2 modules together with bus (different ranges)" {
                    let decls =
                        [ { name = StringIdentifier "B"
                            ports = [ (Output, "bOut", Range(20, 15)) ] }
                          { name = StringIdentifier "C"
                            ports = [ (Input, "cIn", Range(5, 0)) ] } ]

                    let moduleAST =
                        { name = "A"
                          ports = []
                          items =
                              [ ItemWireDecl(Range(25, 20), "between")
                                ItemInstantiation
                                    ("B", "theB", [ ExprIdentifier "between" ])
                                ItemInstantiation
                                    ("C", "theC", [ ExprIdentifier "between" ]) ] }

                    let expectedNetlist =
                        { moduleName = "A"
                          nodes =
                              [ ModuleInstance
                                  ({ moduleName = StringIdentifier "B"
                                     instanceName = "theB"
                                     connections =
                                         Map
                                             [ "bOut",
                                               [ { srcRange = Range(20, 15)
                                                   targetRange = Range(5, 0)
                                                   target =
                                                       InstanceTarget
                                                           ("theC", "cIn") } ] ] })
                                ModuleInstance
                                    ({ moduleName = StringIdentifier "C"
                                       instanceName = "theC"
                                       connections = Map.empty }) ] }

                    expectNetlist decls moduleAST expectedNetlist
                }

                test "Connect part of input to output" {
                    let decls = []

                    let moduleAST =
                        { name = "A"
                          ports = []
                          items =
                              [ ItemPort(Input, Range(5,0), "in")
                                ItemPort(Output, Range(3,0), "out")
                                ItemAssign("out", ExprIndex(ExprIdentifier "in", IndexRange(5, 2))) ] }

                    let expectedNetlist =
                        { moduleName = "A"
                          nodes =
                              [ InputPin
                                  ("in",
                                   [ { srcRange = Range(5, 2)
                                       targetRange = Range(3, 0)
                                       target = PinTarget "out" } ])
                                OutputPin("out") ] }

                    expectNetlist decls moduleAST expectedNetlist
                }

                test "Connect part of input to submodule" {
                    let decls =
                        [ { name = StringIdentifier "B"
                            ports = [ (Input, "bIn", Range(3, 0)) ] } ]

                    let moduleAST =
                        { name = "A"
                          ports = [ "aIn" ]
                          items =
                              [ ItemPort(Input, Range(5, 0), "aIn")
                                ItemInstantiation
                                    ("B", "theB", [ ExprIndex((ExprIdentifier "aIn"), IndexRange(5, 2))  ]) ] }

                    let expectedNetlist =
                        { moduleName = "A"
                          nodes =
                              [ InputPin
                                  ("aIn",
                                   [ { srcRange = Range(5, 2)
                                       targetRange = Range(3, 0)
                                       target = InstanceTarget("theB", "bIn") } ])
                                ModuleInstance
                                    ({ moduleName = StringIdentifier "B"
                                       instanceName = "theB"
                                       connections = Map.empty }) ] }

                    expectNetlist decls moduleAST expectedNetlist
                }

                test "Connect if-then-else to output" {
                    let decls = []

                    let moduleAST =
                        { name = "A"
                          ports = [ "aIn"; "aOut" ]
                          items =
                              [ ItemPort(Input, Single, "aIn")
                                ItemPort(Output, Range(2, 0), "aOut")
                                ItemAssign("aOut", ExprIfThenElse(ExprIdentifier "aIn", ExprNumber(Some 3, 5), ExprNumber(Some 3, 1))) ] }

                    let expectedNetlist =
                        { moduleName = "A"
                          nodes =
                              [ InputPin
                                  ("aIn",
                                   [ { srcRange = Single
                                       targetRange = Single
                                       target = InstanceTarget("mux2-0", "cond") } ])
                                Constant
                                    {| width=3
                                       value=5
                                       connections=
                                        [ { srcRange = Range(2, 0)
                                            targetRange = Range(2, 0)
                                            target = InstanceTarget("mux2-0", "true") } ] |}
                                Constant
                                    {| width=3
                                       value=1
                                       connections=
                                        [ { srcRange = Range(2, 0)
                                            targetRange = Range(2, 0)
                                            target = InstanceTarget("mux2-0", "false") } ] |}
                                ModuleInstance
                                    ({ moduleName = StringIdentifier "Mux2"
                                       instanceName = "mux2-0"
                                       connections =
                                           Map
                                               [ "output",
                                                 [ { srcRange = Range(2, 0)
                                                     targetRange = Range(2, 0)
                                                     target = PinTarget "aOut" } ] ] })
                                OutputPin("aOut") ] }


                    expectNetlist decls moduleAST expectedNetlist
                }

                test "LH's fail" {
                    let decls =
                        [ { name = StringIdentifier "test"
                            ports = [ (Output, "testout", Single)
                                      (Input, "testin1", Single)
                                      (Input, "testin2", Single) ] } ]

                    let moduleAST =
                        { name = "testproj"
                          ports = ["out1"; "out2"; "a"; "b"]
                          items =
                                  [ItemPort (Input,Single,"a"); ItemPort (Input,Single,"b")
                                   ItemPort (Output,Single,"out1"); ItemPort (Output,Single,"out2")
                                   ItemInstantiation
                                      ("test","re",
                                      [ExprIdentifier "out2"; ExprIdentifier "a"; ExprIdentifier "b"])
                                   ItemAssign
                                      ("out1",ExprBinary (ExprIdentifier "a",BOpPlus,ExprIdentifier "b"))] }

                    let expectedNetlist =
                        { moduleName = "testproj"
                          nodes =
                              [ InputPin
                                  ("a",
                                   [ { srcRange = Single
                                       targetRange = Single
                                       target = InstanceTarget("re", "testin1") }
                                     { srcRange = Single
                                       targetRange = Single
                                       target = InstanceTarget("BOpPlus-0", "left") }])
                                InputPin
                                  ("b",
                                   [ { srcRange = Single
                                       targetRange = Single
                                       target = InstanceTarget("re", "testin2") }
                                     { srcRange = Single
                                       targetRange = Single
                                       target = InstanceTarget("BOpPlus-0", "right") }])
                                OutputPin("out1")
                                OutputPin("out2")
                                ModuleInstance
                                    ({ moduleName = StringIdentifier "test"
                                       instanceName = "re"
                                       connections =
                                           Map
                                               [ "testout",
                                                 [ { srcRange = Single
                                                     targetRange = Single
                                                     target = PinTarget "out2" } ] ] })
                                ModuleInstance
                                    ({ moduleName = BOpIdentifier BOpPlus
                                       instanceName = "BOpPlus-0"
                                       connections =
                                           Map
                                               [ "output",
                                                 [ { srcRange = Single
                                                     targetRange = Single
                                                     target = PinTarget "out1" } ] ] })

                                 ] }


                    expectNetlist decls moduleAST expectedNetlist
                }
                ]

          testList "Unification of connections"
              [ testProperty "Has no effect if there are no named endpoints"
                <| Prop.forAll MyArbitraries.nonNamedEndpointConnectionList
                       (fun conns ->
                           isPermutationOf conns
                               (Internal.unifyConnections conns))

                testProperty "Is idempotent"
                <| Prop.forAll MyArbitraries.connectionList (fun conns ->
                       isPermutationOf
                           (Netlist.Internal.unifyConnections conns)
                           (Netlist.Internal.unifyConnections
                            <| Netlist.Internal.unifyConnections conns))


                testProperty "Is commutative"
                <| Prop.forAll MyArbitraries.connectionList
                    (fun conns ->
                        Prop.forAll (permutationsOf conns)
                            (fun shuffledCons ->
                                isPermutationOf
                                    (Internal.unifyConnections conns)
                                    (Internal.unifyConnections shuffledCons)))
                ] ]

let private permutationsOf lst =
    Arb.fromGen (Gen.map List.ofArray <| Gen.shuffle lst)

let private isPermutationOf lst1 lst2 =
    ((Set.ofList lst1 = Set.ofList lst2) && (lst1.Length = lst2.Length))
    |@ sprintf "\n%A \n ---isPermuationOf--- \n %A\n" lst1 lst2

module private MyArbitraries =
    let connectionList =
        let endpoint =
            Gen.oneof [ portEndpoint; constantEndpoint; nameEndpoint ]
        let targetEndpoint = Gen.oneof [ portEndpoint; nameEndpoint ]

        Arb.fromGenShrink
            (Gen.listOf (connectionOf endpoint targetEndpoint), Arb.shrink)
        |> Arb.filter validConnectionList

    let nonNamedEndpointConnectionList =
        let nonNamedEndpoint = Gen.oneof [ portEndpoint; constantEndpoint ]
        let nonNamedTargetEndpoint = portEndpoint

        Arb.fromGenShrink
            (Gen.listOf (connectionOf nonNamedEndpoint nonNamedTargetEndpoint),
             Arb.shrink) |> Arb.filter validConnectionList

    /// Test with only single ports. Making sure that more complicated
    /// networks are valid is too complicated and would be way too much for
    /// test code
    let private range = Gen.constant Single

    let private connectionOf srcGen targetGen =
        gen {
            let! src = srcGen
            let! target = targetGen |> Gen.filter ((<>) src)
            let! connectionRange = range

            return ({ src = src
                      target = target
                      srcRange = connectionRange
                      targetRange = connectionRange }: Internal.IntermediateConnection)
        }

    let private validConnectionList conns =
        let individualConnsValid =
            conns
            |> Seq.forall (fun conn ->
                conn.srcRange = conn.targetRange && conn.src <> conn.target)

        // If a port has been used as a src then it is a module output
        let outputPorts =
            conns
            |> Seq.choose (fun conn ->
                match conn.src with
                | Internal.PortEndpoint(instance, port) -> Some(instance, port)
                | _ -> None)
        // If a port has been used as a target then it is a module input
        let inputPorts =
            conns
            |> Seq.choose (fun conn ->
                match conn.target with
                | Internal.PortEndpoint(instance, port) -> Some(instance, port)
                | _ -> None)

        let singleDriver: bool =
            conns
            |> List.groupBy (fun c -> c.target)
            |> Seq.forall (fun (_, reps) -> Seq.length reps = 1)

        let unidirectionalPorts =
            Set.isEmpty
                (Set.intersect (Set.ofSeq outputPorts) (Set.ofSeq inputPorts))

        individualConnsValid && unidirectionalPorts && singleDriver

    let private portEndpoint =
        Arb.generate<NonNull<string> * NonNull<string>>
        |> Gen.map (fun (a, b) -> Netlist.Internal.PortEndpoint(a.Get, b.Get))

    let private constantEndpoint =
        Gen.map Netlist.Internal.ConstantEndpoint Arb.generate

    let private nameEndpoint =
        Arb.generate<NonNull<string>>
        |> Gen.map (fun n -> Netlist.Internal.NameEndpoint(n.Get))
