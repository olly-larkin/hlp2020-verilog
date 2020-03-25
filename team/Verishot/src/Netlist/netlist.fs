module rec Verishot.Netlist

open System

open Verishot.CoreTypes
open Verishot.CoreTypes.Netlist
open Verishot.Util

module AST = Verishot.CoreTypes.VerilogAST
module Netlist = Verishot.CoreTypes.Netlist

let moduleNetlist (allModules: ModuleDecl list) (thisModule: AST.Module): Netlist =
    { moduleName = thisModule.name
      nodes =
          let (intermediateConnections, intermediateNodes) =
              Internal.getIntermediateNodes allModules thisModule
          let unifiedConnections =
              Internal.unifyConnections intermediateConnections
          let finalNodes =
              Internal.applyConnections unifiedConnections intermediateNodes
          finalNodes }

module Internal =
    /// A terminator to a connection. Somewhat similar to Netlist.ConnectionTarget,
    /// except it also includes constants. Also NameEndpoint is conceptually different
    /// to PinTarget, in that it can represent a wire, not just a pin
    type Endpoint =
        | NameEndpoint of Identifier
        | PortEndpoint of nodeName: Identifier * portName: Identifier
        | ConstantEndpoint of width: int * value: int

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
        // See documentation of exprNodesWithOutput for a rationale behind using a ref
        let operatorIdx = ref 0

        let initialState =
            {| netRanges =
                   Map.empty<Identifier, Range> // Map wires to their declared sizes
               nodes = []
               connections = [] |}

        (initialState, thisModule.items)
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
                    let instance =
                        Netlist.ModuleInstance
                            ({ moduleName = StringIdentifier moduleName
                               instanceName = instanceName
                               connections = Map.empty })

                    // We need the ports of the instantiated module to know
                    // which port each expression connects to
                    let ports =
                        allModules
                        |> List.tryFind
                            (fun decl ->
                                decl.name = StringIdentifier moduleName)
                        |> Option.defaultWith
                            (fun () ->
                                failwithf "Module %s does not exist" moduleName)
                        |> fun decl -> decl.ports

                    let newConnections, newNodes =
                        List.zip ports connectionExpressions
                        |> List.map
                            (function
                            | (Input, portName, range), expr ->
                                exprNodesWithOutput allModules state.netRanges
                                    operatorIdx expr
                                    (PortEndpoint(instanceName, portName), range)
                            | (Output, portName, portRange),
                              AST.ExprIdentifier targetName ->
                                ([ { src = PortEndpoint(instanceName, portName)
                                     srcRange = portRange
                                     target = NameEndpoint targetName
                                     targetRange = moveRangeToBase portRange } ],
                                 [])
                            | ((Output, _, _), _) ->
                                failwith
                                    "Module output can only be connected to identifiers")
                        |> List.unzip
                        |> Tuple.bimap List.concat List.concat

                    {| state with
                           connections = state.connections @ newConnections
                           nodes = instance :: (state.nodes @ newNodes) |}

                | AST.ItemAssign(targetNodeName, expression) ->
                    // We assume that the entirety of the wire is connected
                    // this would change if we allowed indexing on the left hand
                    // of assignments
                    let targetRange =
                        state.netRanges
                        |> Map.tryFind targetNodeName
                        |> Option.defaultWith
                            (fun () ->
                                failwithf "Net %s does not exist"
                                    targetNodeName)

                    let newConnections, newNodes =
                        exprNodesWithOutput allModules state.netRanges
                            operatorIdx expression
                            (NameEndpoint targetNodeName, targetRange)

                    {| state with
                           connections = state.connections @ newConnections
                           nodes = state.nodes @ newNodes |}

                | AST.ItemWireDecl(size, name) ->
                    {| state with netRanges = Map.add name size state.netRanges |})
        |> fun state -> state.connections, state.nodes

    let unifyConnections (connections: IntermediateConnection list): IntermediateConnection list =
        // Detect wire driven by multiple sources
        let drivers = connections |> List.groupBy (fun c -> c.target)

        match drivers |> List.tryFind (fun (_, srcs) -> srcs.Length > 1) with
        | Some(pin, pinDrivers) ->
            failwithf
                "Wire or pin %A is driven by more than one connection: \n%A"
                pin pinDrivers
        | None -> ()

        // We group connections by source because connections are 1-to-many.
        // I.e. A wire can only be driven by one pin but can drive many pins
        let groupedConnections =
            connections |> List.groupBy (fun conn -> conn.src)

        ([], groupedConnections)
        ||> List.fold (fun existingConns (src, outConns) ->
                // Existing srcRange is here so that if a connection is driving
                // this one, then we don't overwrite its range. The src range
                // should be the one from the very first src and the
                // intermediate ranges can vanish
                let existingSrcRange, src, existingConns =
                    match src with
                    | NameEndpoint _srcName ->
                        existingConns
                        |> List.tryFindAndRemove (fun c -> c.target = src)
                        |> (function
                        | Some c, conns -> Some(c.srcRange), c.src, conns
                        | None, conns -> None, src, conns)
                    | _ -> None, src, existingConns

                let srcAdjustedOutConns =
                    outConns
                    |> List.map (fun c ->
                        { c with
                              src = src
                              srcRange =
                                  existingSrcRange
                                  |> Option.defaultValue c.srcRange })

                // We separate the connections that connect to an existing
                // connection (intermediateConns) from the ones that are
                // completely new (newConns)
                let intermediateConns, newConns =
                    srcAdjustedOutConns
                    |> List.splitBy
                        (fun c ->
                            List.exists (fun dc -> c.target = dc.src)
                                existingConns)

                // We apply the intermediateConns to the existing connections
                // (existingConns) which are simply the connections that have
                // already been processed (the accumulator)
                let finalConns =
                    existingConns
                    |> List.map (fun dc ->
                        if List.exists (fun ic -> ic.target = dc.src)
                               intermediateConns then
                            { dc with
                                  src = src
                                  srcRange =
                                      existingSrcRange
                                      |> Option.defaultValue dc.srcRange }
                        else
                            dc)
                finalConns @ newConns)

    let applyConnections (intermediateConnections: IntermediateConnection list)
        (intermediateNodes: Node list): Node list =
        (intermediateNodes, intermediateConnections)
        ||> List.fold (fun intermediateNodes intermediateConnection ->
                let connection: Netlist.Connection =
                    { srcRange = intermediateConnection.srcRange
                      target = endpointToTarget intermediateConnection.target
                      targetRange = intermediateConnection.targetRange }

                match intermediateConnection.src with
                | NameEndpoint srcName ->
                    intermediateNodes
                    |> List.confirmedMap (function
                        | InputPin(pinName, conns) when pinName = srcName ->
                            Some(InputPin(pinName, connection :: conns))
                        | _ -> None)
                    |> Option.defaultWith
                        (fun () -> failwithf "Pin %s was not found" srcName)
                | ConstantEndpoint(width, value) ->
                    Constant
                        {| value = value
                           width = width
                           connections = [ connection ] |} :: intermediateNodes
                | PortEndpoint(instanceName, portName) ->
                    intermediateNodes
                    |> List.confirmedMap (function
                        | ModuleInstance instance when instance.instanceName =
                                                           instanceName ->
                            let newConnections =
                                instance.connections
                                |> Map.update portName
                                       (function
                                       | None -> [ connection ]
                                       | Some existingConnections ->
                                           connection :: existingConnections)

                            Some
                            <| ModuleInstance
                                { instance with connections = newConnections }
                        | _ -> None)
                    |> Option.defaultWith
                        (fun () ->
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


    /// Generate a list of nodes and connections according to an expression
    ///
    /// We use an int ref to give unique names to multiple instances of expression
    /// blocks. It would be possible to do this by threading an int parameter
    /// around the various functions that need it. Simply pass in the current value
    /// and return the updated one (after some indices have been used).
    /// This was deemed to be both cumbersome and error-prone. You could very easily
    /// pass in a stale version of operatorIdx without noticing and the compiler would
    /// not be able to know.

    let exprNodesWithOutput (moduleDecls: ModuleDecl list)
        (netRanges: Map<Identifier, Range>) (operatorIdx: int ref)
        (expr: AST.Expr) (target: RangedEndpoint): IntermediateConnection list * Node list =
        let rec subExprNodes (expr: AST.Expr) =
            match expr with
            | AST.ExprNumber(specifiedWidth, value) ->
                let width =
                    specifiedWidth
                    |> Option.defaultValue
                        (if value = 0 then 1 else (valueWidth value))

                {| nodes = []
                   connections = []
                   outputEndpoint = ConstantEndpoint(width, value)
                   outputRange = Range(width - 1, 0) |}

            | AST.ExprIdentifier(name) ->
                let range =
                    netRanges
                    |> Map.tryFind name
                    |> Option.defaultWith
                        (fun () ->
                            failwithf "Identifier %s is not declared" name)

                {| nodes = []
                   connections = []
                   outputEndpoint = NameEndpoint(name)
                   outputRange = range |}

            | AST.ExprIfThenElse(cond, trueCase, falseCase) ->
                let muxNodeName = sprintf "mux2-%d" !operatorIdx
                operatorIdx := !operatorIdx + 1

                let condResult = subExprNodes cond
                let trueResult = subExprNodes trueCase
                let falseResult = subExprNodes falseCase

                let muxNode =
                    (ModuleInstance
                        { instanceName = muxNodeName
                          moduleName = StringIdentifier "Mux2"
                          connections = Map.empty })

                let outputRange =
                    let size =
                        Math.Max
                            (rangeWidth trueResult.outputRange,
                             rangeWidth trueResult.outputRange)
                    Range(size - 1, 0)

                let inputConnections =
                    [ { src = condResult.outputEndpoint
                        srcRange = condResult.outputRange
                        target = PortEndpoint(muxNodeName, "cond")
                        targetRange = moveRangeToBase condResult.outputRange }
                      { src = trueResult.outputEndpoint
                        srcRange = trueResult.outputRange
                        target = PortEndpoint(muxNodeName, "true")
                        targetRange = moveRangeToBase trueResult.outputRange }
                      { src = falseResult.outputEndpoint
                        srcRange = falseResult.outputRange
                        target = PortEndpoint(muxNodeName, "false")
                        targetRange = moveRangeToBase falseResult.outputRange } ]

                {| nodes =
                       muxNode
                       :: List.concat
                           [ condResult.nodes; trueResult.nodes; falseResult.nodes ]
                   connections =
                       List.concat
                           [ inputConnections
                             condResult.connections
                             trueResult.connections
                             falseResult.connections ]
                   outputEndpoint = PortEndpoint(muxNodeName, "output")
                   outputRange = outputRange |}

            | AST.ExprBinary(left, op, right) ->
                let operatorNodeName = sprintf "%A-%d" op !operatorIdx
                operatorIdx := !operatorIdx + 1

                let operatorOutputRange =
                    findOutputRange (BOpIdentifier op) moduleDecls

                let leftResult = subExprNodes left
                let rightResult = subExprNodes right

                let operatorNode =
                    ModuleInstance
                        { instanceName = operatorNodeName
                          moduleName = BOpIdentifier op
                          connections = Map.empty }

                let inputConnections =
                    [ { src = leftResult.outputEndpoint
                        srcRange = leftResult.outputRange
                        target = PortEndpoint(operatorNodeName, "left")
                        targetRange = leftResult.outputRange }
                      { src = rightResult.outputEndpoint
                        srcRange = rightResult.outputRange
                        target = PortEndpoint(operatorNodeName, "right")
                        targetRange = rightResult.outputRange } ]

                {| nodes =
                       operatorNode
                       :: List.concat [ leftResult.nodes; rightResult.nodes ]
                   connections =
                       List.concat
                           [ inputConnections
                             leftResult.connections
                             rightResult.connections ]
                   outputEndpoint = PortEndpoint(operatorNodeName, "output")
                   outputRange = operatorOutputRange |}
            | AST.ExprUnary(op, subExpr) ->
                let operatorNodeName = sprintf "%A-%d" op !operatorIdx
                operatorIdx := !operatorIdx + 1

                let subExprResult = subExprNodes subExpr

                let operatorNode =
                    ModuleInstance
                        { instanceName = operatorNodeName
                          moduleName = UOpIdentifier op
                          connections = Map.empty }

                let operatorOutputRange =
                    findOutputRange (UOpIdentifier op) moduleDecls

                let inputConnections =
                    [ { src = subExprResult.outputEndpoint
                        srcRange = subExprResult.outputRange
                        target = PortEndpoint(operatorNodeName, "input")
                        targetRange = subExprResult.outputRange } ]

                {| nodes = operatorNode :: subExprResult.nodes
                   connections =
                       List.concat
                           [ inputConnections; subExprResult.connections ]
                   outputEndpoint = PortEndpoint(operatorNodeName, "output")
                   outputRange = operatorOutputRange |}
            | AST.ExprConcateneation _ ->
                failwith "Not yet implemented: Expression concatenation"
            | AST.ExprIndex(subExpr, AST.IndexNum n) ->
                {| subExprNodes subExpr with outputRange = Range(n, n) |}
            | AST.ExprIndex(subExpr, AST.IndexRange(high, low)) ->
                {| subExprNodes subExpr with outputRange = Range(high, low) |}


        let targetEndpoint, requestedRange = target
        let result = subExprNodes expr

        let srcRange, targetRange =
            match result.outputRange, requestedRange with
            | _, Single -> Single, Single
            | Single, _ -> Single, Single
            | Range(srcHigh, srcLow), Range(targetHigh, targetLow) ->
                let targetWidth = rangeWidth (Range(targetHigh, targetLow))
                Range(Math.Min(srcHigh, srcLow + targetWidth - 1), srcLow),
                Range
                    (Math.Min(targetHigh, targetLow + targetWidth - 1),
                     targetLow)

        let outConnection =
            { src = result.outputEndpoint
              srcRange = srcRange
              target = targetEndpoint
              targetRange = targetRange }

        outConnection :: result.connections, result.nodes

    let findOutputRange op moduleDecls =
        moduleDecls
        |> List.tryFind (fun decl -> decl.name = op)
        |> Option.defaultWith
            (fun () -> failwithf "There is no implementation for %A" op)
        |> (fun decl -> decl.ports)
        |> List.tryFind (fun (dir, name, _) -> dir = Output && name = "output")
        |> Option.defaultWith
            (fun () ->
                failwithf
                    "The declaration of %A does not have a valid \"output\" port"
                    op)
        |> (fun (_, _, outRange) -> outRange)
