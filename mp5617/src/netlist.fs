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
              let (intermediateConnections, intermediateNodes) =
                  Internal.getIntermediateNodes allModules thisModule
              let unifiedConnections =
                  Internal.unifyConnections intermediateConnections
              let finalNodes =
                  Internal.applyConnections unifiedConnections
                      intermediateNodes
              finalNodes }

module Internal =
    type Endpoint =
        | NameEndpoint of Identifier
        | PortEndpoint of nodeName: Identifier * portName: Identifier
        | ConstantEndpoint of {| value: int; width: int |}

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

        let initialState =
            {| netRanges = Map.empty<Identifier, Range> // Map wires to their declared sizes
               nodes = []
               connections = [] |}

        (initialState, thisModule.items)
        // We need to use collect because each item can require multiple nodes (e.g. expressions)
        ||> List.fold (fun state item ->
                match item with
                | AST.ItemPort(Output, range, name) ->
                    {| state with
                           netRanges = Map.add name range state.netRanges
                           nodes = Netlist.OutputPin(name) :: state.nodes |}
                | AST.ItemPort(Input, range, name) ->
                    {| state with
                           netRanges = Map.add name range state.netRanges
                           nodes = Netlist.InputPin(name, []) :: state.nodes |}
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
                                    { src = PortEndpoint(instanceName, portName)
                                      srcRange = portRange
                                      target = NameEndpoint targetName
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
                                         (PortEndpoint(instanceName, portName),
                                          range))
                            | (Output, _, _) -> None)
                        |> List.unzip
                        |> fun (xss, yss) ->
                            (List.concat xss, List.concat yss)


                    {| state with
                           connections =
                               List.concat
                                   [ state.connections; inConnections; outConnections ]
                           nodes = instance :: (state.nodes @ inNodes) |}


                | AST.ItemAssign(targetNodeName, expression) ->
                    let targetRange =
                        state.netRanges
                        |> Map.tryFind targetNodeName
                        |> Option.defaultWith
                            (fun () ->
                                failwithf "Net %s does not exist"
                                    targetNodeName)

                    let (newConns, newNodes) =
                        exprNodesWithOutput operatorIdx expression
                            (NameEndpoint targetNodeName, targetRange)
                    {| state with
                           connections = state.connections @ newConns
                           nodes = state.nodes @ newNodes |}

                | AST.ItemWireDecl(size, name) ->
                    {| state with netRanges = Map.add name size state.netRanges |})

        |> (fun state -> state.connections, state.nodes)

    let unifyConnections (connections: IntermediateConnection list): IntermediateConnection list =
        ([], connections)
        // Remove all references
        ||> List.fold (fun directConns conn ->
                match conn.src, conn.target with
                | (NameEndpoint(_) as src), (NameEndpoint(_) as target) ->
                    directConns
                    |> List.confirmedMap (fun existingConn ->
                        if existingConn.target = src then
                            { existingConn with target = target }, true
                        elif existingConn.src = target then
                            { existingConn with src = src }, true
                        else
                            existingConn, false)
                    |> (function
                    | (newConns, true) -> newConns
                    | (sameConns, false) -> conn :: sameConns)
                | NameEndpoint(_) as src, target ->
                    directConns
                    |> List.confirmedMap (fun existingConn ->
                        if existingConn.target = src
                        then { existingConn with target = target }, true
                        else existingConn, false)
                    |> (function
                    | (newConns, true) -> newConns
                    | (sameConns, false) -> conn :: sameConns)
                | src, (NameEndpoint(_) as target) ->
                    directConns
                    |> List.confirmedMap (fun existingConn ->
                        if existingConn.src = target then
                            { existingConn with src = src }, true
                        else
                            existingConn, false)
                    |> (function
                    | (newConns, true) -> newConns
                    | (sameConns, false) -> conn :: sameConns)
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
                | NameEndpoint srcName ->
                    intermediateNodes
                    |> List.confirmedMap (function
                        | InputPin(pinName, conns) when pinName = srcName ->
                            (InputPin(pinName, finalConnection :: conns), true)
                        | node -> (node, false))
                    |> (function
                    | (finalNodes, true) -> finalNodes
                    | (_, false) -> failwithf "Pin %s was not found" srcName)

                | ConstantEndpoint constantSpec ->
                    Constant
                        {| constantSpec with connections = [ finalConnection ] |}
                    :: intermediateNodes
                | PortEndpoint(instanceName, portName) ->
                    intermediateNodes
                    |> List.confirmedMap (function
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
                        | node -> (node, false))
                    |> (function
                    | (finalNodes, true) -> finalNodes
                    | (_, false) ->
                        failwithf "Instance %s was not found" instanceName))

    let endpointToTarget (endpoint: Endpoint): Netlist.ConnectionTarget =
        match endpoint with
        | NameEndpoint name -> PinTarget name
        | PortEndpoint(nodeName, portName) -> InstanceTarget(nodeName, portName)
        | ConstantEndpoint _ -> failwith "Tried to connect into constant"

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
            ([ { src = NameEndpoint(name)
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
                { src = PortEndpoint(operatorNodeName, "output")
                  srcRange = moveRangeToBase targetRange
                  target = targetEndpoint
                  targetRange = targetRange }

            let inputRange = moveRangeToBase targetRange

            let (leftConnections, leftNodes) =
                exprNodesWithOutput operatorIdx left
                    (PortEndpoint(operatorNodeName, "left"), inputRange)
            let (rightConnections, rightNodes) =
                exprNodesWithOutput operatorIdx right
                    (PortEndpoint(operatorNodeName, "right"), inputRange)

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
                { src = PortEndpoint(operatorNodeName, "output")
                  srcRange = moveRangeToBase targetRange
                  target = targetEndpoint
                  targetRange = targetRange }

            let inputRange = moveRangeToBase targetRange
            let (exprConnections, exprNodes) =
                exprNodesWithOutput operatorIdx expr
                    (PortEndpoint(operatorNodeName, "input"), inputRange)

            (outputConnection :: exprConnections, operatorNode :: exprNodes)
        | AST.ExprNumber(givenWidth, value) ->
            let width =
                givenWidth |> Option.defaultValue (rangeWidth targetRange)

            let connection =
                { src = ConstantEndpoint
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
