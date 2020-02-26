module rec Verishot.Netlist

open Verishot.CoreTypes
open Verishot.CoreTypes.Netlist
open Verishot.Util

module AST = Verishot.CoreTypes.VerilogAST
module Netlist = Verishot.CoreTypes.Netlist

let moduleNetlist (allModules: ModuleDecl list) (thisModule: AST.Module): Netlist option =
    Some
        { moduleName = thisModule.name
          nodes =
              thisModule
              |> Internal.getIntermediateNodes allModules
              |> function
              | (connections, nodes) ->
                  (Internal.unifyConnections connections, nodes)
                  ||> Internal.applyConnections }

module Internal =
    type Endpoint =
        | NameTarget of Identifier
        | PortTarget of nodeName: Identifier * portName: Identifier
        | ConstantTarget of {| value: int; width: int |}

    /// A connection target along with a range. Useful when instantiating expressions feeding
    /// into a module port
    type RangedEndpoint = Endpoint * Range

    /// Intermediate connections generated while traversing the AST.
    /// The connections here need to be fully specified (source and target)
    /// because either end might refer to a `wire` which should be simplified
    /// in the final Netlist
    type IntermediateConnection =
        { src: Endpoint
          srcRange: Range

          target: Endpoint
          targetRange: Range }

    /// Gather IntermediateNode's from a module AST in a single pass. The output of
    /// this needs to be processed further to get the final list of nodes
    let getIntermediateNodes (allModules: ModuleDecl list)
        (thisModule: AST.Module): IntermediateConnection list * Node list =
        // We use a single, mutable reference
        let operatorIdx = ref 0

        let initialState: IntermediateConnection list * Node list = ([], [])

        (initialState, thisModule.items)
        // We need to use collect because each item can require multiple nodes (e.g. expressions)
        ||> List.fold (fun (connections, nodes) item ->
                match item with
                | AST.ItemPort(Output, _range, name) ->
                    (connections, Netlist.OutputPin(name) :: nodes)
                | AST.ItemPort(Input, _range, name) ->
                    (connections, Netlist.InputPin(name, []) :: nodes)
                | AST.ItemInstantiation(moduleName, instanceName,
                                        connectionExpressions) ->
                    // Find the ports of the instantiated module from its declaration
                    let ports =
                        allModules
                        |> List.tryFind (fun decl -> decl.name = moduleName)
                        |> function
                        // TODO Make this return a result
                        | None ->
                            failwithf "Module %s does not exist" moduleName
                        | Some decl -> decl.ports

                    let instance =
                        Netlist.ModuleInstance
                            ({ moduleName = StringIdentifier moduleName
                               instanceName = instanceName
                               connections = Map.empty })

                    // Figure out what the outputs connect to
                    let outConnections: IntermediateConnection list =
                        List.zip ports connectionExpressions
                        |> List.choose (function
                            | ((Output, portName, portRange),
                               AST.ExprIdentifier targetName) ->
                                Some
                                    { src = PortTarget(instanceName, portName)
                                      srcRange = portRange
                                      target = NameTarget targetName
                                      targetRange = moveRangeToBase portRange }
                            | ((Output, _, _), _) ->
                                failwith
                                    "Not yet implemented (Connecting module output to anything other than an output pin)"
                            | ((Input, _, _), _) -> None)


                    let (inConnections: IntermediateConnection list,
                         inNodes: Node list) =
                        List.zip ports connectionExpressions
                        |> List.choose (fun (port, expr) ->
                            match port with
                            | (Input, portName, range) ->
                                Some
                                    (exprNodesWithOutput operatorIdx expr
                                         (PortTarget(instanceName, portName),
                                          range))
                            | (Output, _, _) -> None)
                        |> List.unzip
                        |> fun (xss, yss) ->
                            (List.concat xss, List.concat yss)


                    (List.concat [ connections; inConnections; outConnections ],
                     instance :: List.append nodes inNodes)

                | AST.ItemAssign(targetNodeName, expression) ->
                    // TODO don't assume all assignments are to single wires
                    exprNodesWithOutput operatorIdx expression
                        (NameTarget targetNodeName, Single)
                | AST.ItemWireDecl _ -> failwith "Not yet implemented (wires)")

    let unifyConnections (connections: IntermediateConnection list): IntermediateConnection list =
        ([], connections)
        // Remove all references
        ||> List.fold (fun directConns conn ->
                match conn.src, conn.target with
                | (NameTarget(_) as src), (NameTarget(_) as target) ->
                    directConns
                    |> List.map (fun existingConn ->
                        if existingConn.target = src then
                            { existingConn with target = target }
                        elif existingConn.src = target then
                            { existingConn with src = src }
                        else
                            existingConn)
                | NameTarget(_) as src, target ->
                    directConns
                    |> List.map (fun existingConn ->
                        if existingConn.target = src then
                            { existingConn with target = target }
                        else
                            existingConn)
                | src, (NameTarget(_) as target) ->
                    directConns
                    |> List.map (fun existingConn ->
                        if existingConn.src = target then
                            { existingConn with src = src }
                        else
                            existingConn)
                | _, _ -> conn :: directConns)
    // TODO Verify that there is no undriven wire, or any wires driving nothing

    let applyConnections (intermediateConnections: IntermediateConnection list)
        (intermediateNodes: Node list): Node list =
        (intermediateNodes, intermediateConnections)
        ||> List.fold (fun intermediateNodes connection ->
                let finalConnection: Netlist.Connection =
                    { srcRange = connection.srcRange
                      target = endpointToTarget connection.target
                      targetRange = connection.targetRange }
                match connection.src with
                | NameTarget srcName ->
                    (false, intermediateNodes)
                    ||> List.mapFold (fun found node ->
                            match node with
                            | InputPin(pinName, conns) when pinName = srcName ->
                                (InputPin(pinName, finalConnection :: conns),
                                 true)
                            | _ -> (node, found))
                    |> (function
                    | (finalNodes, true) -> finalNodes
                    | (_, false) -> failwithf "Pin %s was not found" srcName)

                | ConstantTarget constantSpec ->
                    Constant
                        {| constantSpec with connections = [ finalConnection ] |}
                    :: intermediateNodes
                | PortTarget(instanceName, portName) ->
                    (false, intermediateNodes)
                    ||> List.mapFold (fun found node ->
                            match node with
                            | ModuleInstance instance when instance.instanceName =
                                                               instanceName ->
                                let existingPinConnections =
                                    instance.connections
                                    |> Map.tryFind portName
                                    |> Option.defaultValue []

                                let newConnections =
                                    instance.connections
                                    |> Map.add portName
                                           (finalConnection
                                            :: existingPinConnections)

                                (ModuleInstance
                                    { instance with connections = newConnections },
                                 true)
                            | _ -> (node, found))
                    |> (function
                    | (finalNodes, true) -> finalNodes
                    | (_, false) ->
                        failwithf "Instance %s was not found" instanceName)

                )

    let endpointToTarget (endpoint: Endpoint): Netlist.ConnectionTarget =
        match endpoint with
        | NameTarget name -> PinTarget name
        | PortTarget(nodeName, portName) -> InstanceTarget(nodeName, portName)
        | ConstantTarget _ -> failwith "Tried to connect into constant"

    let nodeName node =
        match node with
        | InputPin(name, _) -> name
        | OutputPin(name) -> name
        | Constant constant -> sprintf "%d'%d" constant.width constant.value
        | ModuleInstance { instanceName = name } -> name

    let exprNodesWithOutput (operatorIdx: int ref) (expr: AST.Expr)
        (target: RangedEndpoint): IntermediateConnection list * Node list =
        let targetEndpoint, targetRange = target

        match expr with
        | AST.ExprIdentifier name ->
            ([ { src = NameTarget(name)
                 srcRange = moveRangeToBase targetRange
                 target = targetEndpoint
                 targetRange = targetRange } ], [])

        | AST.ExprBinary(left, op, right) ->
            let operatorNodeName = sprintf "%A-%d" op !operatorIdx
            operatorIdx := !operatorIdx + 1

            let operatorNode =
                (ModuleInstance
                    { instanceName = operatorNodeName
                      moduleName = BOpIdentifier op
                      connections = Map.empty })
            let outputConnection =
                { src = PortTarget(operatorNodeName, "output")
                  srcRange = moveRangeToBase targetRange
                  target = targetEndpoint
                  targetRange = targetRange }

            let inputRange = moveRangeToBase targetRange

            let (leftConnections, leftNodes) =
                exprNodesWithOutput operatorIdx left
                    (PortTarget(operatorNodeName, "left"), inputRange)
            let (rightConnections, rightNodes) =
                exprNodesWithOutput operatorIdx right
                    (PortTarget(operatorNodeName, "right"), inputRange)

            (outputConnection :: leftConnections @ rightConnections,
             operatorNode :: leftNodes @ rightNodes)
        | AST.ExprUnary(op, expr) ->
            let operatorNodeName = sprintf "%A-%d" op !operatorIdx
            operatorIdx := !operatorIdx + 1

            let operatorNode =
                ModuleInstance
                    { instanceName = operatorNodeName
                      moduleName = UOpIdentifier op
                      connections = Map.empty }
            let outputConnection =
                { src = PortTarget(operatorNodeName, "output")
                  srcRange = moveRangeToBase targetRange
                  target = targetEndpoint
                  targetRange = targetRange }

            let inputRange = moveRangeToBase targetRange
            let (exprConnections, exprNodes) =
                exprNodesWithOutput operatorIdx expr
                    (PortTarget(operatorNodeName, "input"), inputRange)

            (outputConnection :: exprConnections, operatorNode :: exprNodes)
        | AST.ExprNumber(givenWidth, value) ->
            let width =
                givenWidth |> Option.defaultValue (rangeWidth targetRange)

            let connection =
                { src = ConstantTarget
                          {| width = width
                             value = value |}
                  srcRange = moveRangeToBase targetRange
                  target = targetEndpoint
                  targetRange = targetRange }
            ([ connection ], [])

        | AST.ExprIndex _ ->
            failwith "Not yet implemented (indexing expressions)"
        | AST.ExprIfThenElse _ ->
            failwith "Not yet implemented (if-then-else expressions)"
        | AST.ExprConcateneation _ ->
            failwith "Not yet implemented (concatenation expressions)"
