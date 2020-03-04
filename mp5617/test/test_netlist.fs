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
    match moduleNetlist decls ast with
    | Some actualNetlist ->
        Expect.equal actualNetlist.moduleName expectedNetlist.moduleName
            "Should have same name"
        Expect.containsAll actualNetlist.nodes expectedNetlist.nodes
            "Should have all expected nodes"
        Expect.containsAll expectedNetlist.nodes actualNetlist.nodes
            "Should not have any extra nodes"
    | None -> Expect.equal None (Some expectedNetlist) "Should not fail"

[<Tests>]
let fullModuleTests =
    testList "Netlist"
        [ testList "Full module tests"
              [ test "Extracts single module" {
                    let decls =
                        [ { name = "B"
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
                        [ { name = "B"
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
                        [ { name = "B"
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
                        [ { name = "B"
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
                        [ { name = "B"
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
                        [ { name = "B"
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
                        [ { name = "B"
                            ports = [ (Output, "bOut", Single) ] }
                          { name = "C"
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
                        [ { name = "B"
                            ports = [ (Output, "bOut", Range(5, 0)) ] }
                          { name = "C"
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
                        [ { name = "B"
                            ports = [ (Output, "bOut", Range(20, 15)) ] }
                          { name = "C"
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
                } ]

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

                testProperty "Is associative"
                <| Prop.forAll MyArbitraries.connectionList
                       (fun conns1 ->
                            Prop.forAll MyArbitraries.connectionList
                               (fun conns2 ->
                                   isPermutationOf
                                       (Internal.unifyConnections (Internal.unifyConnections conns1)@conns2)
                                       (Internal.unifyConnections conns1@(Internal.unifyConnections conns2)))) ] ]

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

        Arb.fromGen (Gen.listOf (connectionOf endpoint targetEndpoint))
        |> Arb.filter validConnectionList

    let nonNamedEndpointConnectionList =
        let nonNamedEndpoint = Gen.oneof [ portEndpoint; constantEndpoint ]
        let nonNamedTargetEndpoint = portEndpoint

        Arb.fromGen
            (Gen.listOf (connectionOf nonNamedEndpoint nonNamedTargetEndpoint))
        |> Arb.filter validConnectionList

    let private range =
        Gen.oneof
            [ gen { return Single }
              gen {
                  let! start = Arb.generate<NonNegativeInt>
                               |> Gen.map (fun x -> x.Get)
                  let! size = Arb.generate<NonNegativeInt>
                              |> Gen.map (fun x -> x.Get + 1)
                  return Range(start + size, start) } ]

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
        let srcPorts = conns |> Seq.map (fun conn -> conn.src)
        let targetPorts = conns |> Seq.map (fun conn -> conn.target)

        let rangesMatch =
            conns |> Seq.forall (fun conn -> conn.srcRange = conn.targetRange)
        let noLoops =
            Set.isEmpty
                (Set.intersect (Set.ofSeq srcPorts) (Set.ofSeq targetPorts))
        rangesMatch && noLoops

    let private portEndpoint =
        Arb.generate<NonNull<string> * NonNull<string>>
        |> Gen.map (fun (a, b) -> Netlist.Internal.PortEndpoint(a.Get, b.Get))

    let private constantEndpoint =
        Gen.map Netlist.Internal.ConstantEndpoint Arb.generate

    let private nameEndpoint =
        Arb.generate<NonNull<string>>
        |> Gen.map (fun n -> Netlist.Internal.NameEndpoint(n.Get))
