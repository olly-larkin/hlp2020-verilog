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
            {| netRanges =
                   Map.empty<Identifier, Range> // Map wires to their declared sizes
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
                                    (exprNodesWithOutput operatorIdx expr None
                                         (PortEndpoint(instanceName, portName), range))
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
                        exprNodesWithOutput operatorIdx expression None
                            (NameEndpoint targetNodeName, targetRange)
                    {| state with
                           connections = state.connections @ newConns
                           nodes = state.nodes @ newNodes |}

                | AST.ItemWireDecl(size, name) ->
                    {| state with netRanges =
                           Map.add name size state.netRanges |})

        |> (fun state -> state.connections, state.nodes)

    let unifyConnections (connections: IntermediateConnection list): IntermediateConnection list =
        // Detect wire drive by multiple sources
        let drivers = connections |> List.groupBy (fun c -> c.target)

        match drivers |> List.tryFind (fun (_, srcs) -> srcs.Length > 1) with
        | Some(pin, pinDrivers) ->
            failwithf "Wire or pin %A is driven by: \n%A" pin pinDrivers
        | None -> ()

        // We group connections by source because connections are 1-to-many.
        // I.e. A wire can only be driven by one pin but can drive many pins
        let groupedConnections =
            connections |> List.groupBy (fun conn -> conn.src)

        // printf "\n<=============================================>\n\n"
        // printf "Grouped connections:\n%A" groupedConnections

        // Find the connections driven by a wire and eliminate the wires by
        // merging connections together
        ([], groupedConnections)
        ||> List.fold (fun directConns (src, wireConns) ->
                // printf "\n===\n"
                // printf "source: %A\n" src

                // Try to find a connection driving the wire. If it exists,
                // remove it and change the src of the wire to that
                // connection's src (i.e. adjust the connections' target)
                // If it doesn't exist then either the wire is actually a pin or we haven't
                // encountered its driver yet, so just leave the source as it
                // is (just the name of the NameEndpoint for the wire)
                let wireSrcRange, wireSrc, directConns =
                    match src with
                    | NameEndpoint _srcName as wire ->
                        directConns
                        |> List.tryFindAndRemove (fun c -> c.target = wire)
                        |> (function
                        | Some c, ys -> Some(c.srcRange), c.src, ys
                        | None, ys -> None, wire, ys)
                    | _ -> None, src, directConns

                // printf "srcDriver: %A(%A)\n" wireSrc wireSrcRange
                // printf "removed driving connection:\n%A\n" directConns

                // Find the connections that are driving and existing connection.
                let intermediateConns, newConns =
                    wireConns
                    |> List.map (fun c ->
                        { c with
                              src = wireSrc
                              srcRange =
                                  wireSrcRange
                                  |> Option.defaultValue c.srcRange })
                    |> List.splitBy
                        (fun c ->
                            List.exists (fun dc -> c.target = dc.src)
                                directConns)

                // printf "outgoing connections (to existing endpoints):\n%A\n" intermediateConns
                // printf "outgoing connections (to new endpoints):\n%A\n" newConns

                // Apply the intermediate connections
                let directConns =
                    [ for dc in directConns ->
                        if List.exists (fun ic -> ic.target = dc.src)
                               intermediateConns then
                            { dc with
                                  src = wireSrc
                                  srcRange =
                                      wireSrcRange
                                      |> Option.defaultValue dc.srcRange }
                        else
                            dc ]
                // printf "Fixed source on driven connections:\n%A\n" directConns

                // printf "Final connections:\n%A\n" (directConns @ newConns)
                directConns @ newConns)
    // // TODO Verify that there is no undriven wire, or any wires driving nothing

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
        (srcRangeSelect: Range option) (target: RangedEndpoint): IntermediateConnection list * Node list =
        let targetEndpoint, targetRange = target
        let srcRange =
            Option.defaultValue (moveRangeToBase targetRange) srcRangeSelect

        match expr with
        | AST.ExprIdentifier name ->
            ([ { src = NameEndpoint(name)
                 srcRange = srcRange
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
                  srcRange = srcRange
                  target = targetEndpoint
                  targetRange = targetRange }

            let (leftConnections, leftNodes) =
                exprNodesWithOutput operatorIdx left None
                    (PortEndpoint(operatorNodeName, "left"), srcRange)
            let (rightConnections, rightNodes) =
                exprNodesWithOutput operatorIdx right None
                    (PortEndpoint(operatorNodeName, "right"), srcRange)

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
                  srcRange = srcRange
                  target = targetEndpoint
                  targetRange = targetRange }

            let (exprConnections, exprNodes) =
                exprNodesWithOutput operatorIdx expr None
                    (PortEndpoint(operatorNodeName, "input"), srcRange)

            (outputConnection :: exprConnections, operatorNode :: exprNodes)
        | AST.ExprNumber(givenWidth, value) ->
            let width =
                givenWidth |> Option.defaultValue (rangeWidth targetRange)

            let connection =
                { src = ConstantEndpoint
                          {| width = width
                             value = value |}
                  srcRange = srcRange
                  target = targetEndpoint
                  targetRange = targetRange }
            ([ connection ], [])

        | AST.ExprIndex(subExpr, index) ->
            match index with
            | AST.IndexNum n ->
                exprNodesWithOutput operatorIdx subExpr (Some(Range(n, n)))
                    target
            | AST.IndexRange(high, low) ->
                exprNodesWithOutput operatorIdx subExpr
                    (Some(Range(high, low))) target
        | AST.ExprIfThenElse _ ->
            failwith "Not yet implemented (if-then-else expressions)"
        | AST.ExprConcateneation _ ->
            failwith "Not yet implemented (concatenation expressions)"
