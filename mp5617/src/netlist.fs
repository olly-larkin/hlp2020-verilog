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
              |> getIntermediateNodes allModules
              |> finalizeNodes }

/// While generating the netlist we will encounter references to nodes that we
/// have not created yet. Allow storing them as references to nodes so we can
/// unify all of them once we have gone through the whole module
type private IntermediateNode =
    | FinalNode of Netlist.Node
    | NodeReference of (Identifier * Map<Identifier, Connection>)

/// Gather IntermediateNode's from a module AST in a single pass. The output of
/// this needs to be processed further to get the final list of nodes
let private getIntermediateNodes (allModules: ModuleDecl list) (thisModule: AST.Module): IntermediateNode list =
    // We use a single, private mutable reference
    let operatorIdx = ref 0

    thisModule.items
    // We need to use collect because each item can require multiple nodes (e.g. expressions)
    |> List.collect
        (function
        | AST.ItemPort(Output, name) -> [ FinalNode(Netlist.OutputPin(name)) ]
        | AST.ItemPort(Input, name) -> [ FinalNode(Netlist.InputPin(name, [])) ]
        | AST.ItemInstantiation(moduleName, instanceName, connectionExpressions) ->
            // Find the ports of the instantiated module from its declaration
            let ports =
                allModules
                |> List.tryFind (fun decl -> decl.name = moduleName)
                |> function
                // TODO Make this return a result
                | None -> failwithf "Module %s does not exist" moduleName
                | Some decl -> decl.ports

            // Figure out what the outputs connect to
            let outConnections: (Identifier * Connection list) list =
                List.zip ports connectionExpressions
                |> List.choose (function
                    | ((Output, name, range), expr) ->
                        let connection = findConnection (name, range) expr
                        Some(name, [ connection ])
                    | ((Input, _, _), _) -> None)


            let inNodes: IntermediateNode list =
                List.zip ports connectionExpressions
                |> List.collect (function
                    | ((Input, portName, range), expr) ->
                        exprNodesWithOutput operatorIdx expr
                            (InstanceTarget
                                {| targetNode = instanceName
                                   portName = portName
                                   portIndex = 1 |})
                    | ((Output, _, _), _) -> [])


            FinalNode
                (Netlist.ModuleInstance
                    ({ moduleName = StringIdentifier moduleName
                       instanceName = instanceName
                       connections = Map outConnections }))
            :: inNodes
        | AST.ItemAssign(targetNodeName, expression) ->
            exprNodesWithOutput operatorIdx expression
                (PinTarget
                    {| pinName = targetNodeName
                       pinIndex = 1 |})
        | AST.ItemWireDecl _ -> failwith "Not yet implemented (wires)")

let private finalizeNodes (intermediateNodes: IntermediateNode list): Node list =
    // Defined locally because of peculiar way of handling connections to
    // InputPins. This only makes sense inside this function.
    // See comments in body.
    let addConnections connections node =
        match node with
        | ModuleInstance instance ->
            ModuleInstance { instance with connections = (Map.joinLeft connections instance.connections) }
        | InputPin(name, existingConnections) ->
            // Only connections on the pin name are valid
            match Map.tryFind name connections with
            | Some newConnections -> InputPin(name, existingConnections @ newConnections)
            | None -> InputPin(name, existingConnections)
        | Constant _  -> failwith "Not yet implemented (Constants)"
        | OutputPin _ -> failwith "Tried to add connections to an output pin"

    // Basically all intermediate nodes, excluding references. Useful for resolving those references.
    let declaredNodes =
        intermediateNodes
        |> List.choose (function
            | NodeReference _ -> None
            | FinalNode node -> Some node)

    intermediateNodes
    // Dereference all NodeReference's by looking them up in `declaredNodes`
    |> List.map (function
        | NodeReference(name, connections) ->
            declaredNodes
            |> List.tryFind (fun node -> nodeName node = name)
            |> Option.defaultWith (fun () -> failwithf "Node %s does not exist" name)
            |> addConnections (Map.mapValues List.singleton connections)
        | FinalNode node -> node)
    // Group together all nodes with the same name (it's just different references to the same node)
    |> List.groupBy nodeName
    // Reduce all those groups together
    |> List.map
        (snd // List.groupBy's output is of type (Identifier * Node list) list. The nodes contain their name already so we ignore it
         >> List.reduce (fun node1 node2 ->
             match (node1, node2) with
             | ModuleInstance mod1, ModuleInstance mod2 when mod1.moduleName = mod2.moduleName ->
                 ModuleInstance { mod1 with connections = Map.joinLeft mod1.connections mod2.connections }
             | InputPin(name, conns1), InputPin(_, conns2) -> InputPin(name, conns1 @ conns2)
             | OutputPin name, OutputPin _ -> OutputPin name
             | _, _ ->
                 failwithf "Both \n %A \n  and \n %A \nhave the same name but are of different type" node1 node2))

let private nodeName node =
    match node with
    | InputPin(name, _) -> name
    | OutputPin(name) -> name
    | Constant constant -> sprintf "%d'%d" constant.width constant.value
    | ModuleInstance { instanceName = name } -> name

let private findConnection (srcPort: Identifier * Range) (expr: AST.Expr): Netlist.Connection =
    match (srcPort, expr) with
    | ((_, Single), AST.ExprIdentifier name) ->
        { srcPortIndex = 1
          target = InstanceTarget
                       {| targetNode = name
                          portName = name
                          portIndex = 1 |} }
    | _ -> failwith "Not implemented"


let private exprNodesWithOutput (operatorIdx: int ref) (expr: AST.Expr) (target: ConnectionTarget): IntermediateNode list =
    let finalConnection =
        { srcPortIndex = 1
          target = target }

    match expr with
    | AST.ExprIdentifier name -> [ NodeReference(name, Map [ (name, finalConnection) ]) ]
    | AST.ExprBinary(left, op, right) ->
        let operatorNodeName = sprintf "%A-%d" op !operatorIdx
        operatorIdx := !operatorIdx + 1

        let operatorNode =
            FinalNode
                (ModuleInstance
                    { instanceName = operatorNodeName
                      moduleName = BOpIdentifier op
                      connections = Map [ ("output", [ finalConnection ]) ] })

        let leftNodes =
            exprNodesWithOutput operatorIdx left
                (InstanceTarget
                    {| targetNode = operatorNodeName
                       portName = "left"
                       portIndex = 1 |})

        let rightNodes =
            exprNodesWithOutput operatorIdx right
                (InstanceTarget
                    {| targetNode = operatorNodeName
                       portName = "right"
                       portIndex = 1 |})

        operatorNode :: (leftNodes @ rightNodes)
    | AST.ExprUnary(op, expr) ->
        let operatorNodeName = sprintf "%A-%d" op !operatorIdx
        operatorIdx := !operatorIdx + 1

        let operatorNode =
            FinalNode
                (ModuleInstance
                    { instanceName = operatorNodeName
                      moduleName = UOpIdentifier op
                      connections = Map [ ("output", [ finalConnection ]) ] })

        let exprNodes =
            exprNodesWithOutput operatorIdx expr
                (InstanceTarget
                    {| targetNode = operatorNodeName
                       portName = "input"
                       portIndex = 1 |})

        operatorNode :: exprNodes
    | AST.ExprNumber _ -> failwith "Not yet implemented (constant expressions)"
    | AST.ExprIndex _ -> failwith "Not yet implemented (indexing expressions)"
    | AST.ExprIfThenElse _ -> failwith "Not yet implemented (if-then-else expressions)"
    | AST.ExprConcateneation _ -> failwith "Not yet implemented (concatenation expressions)"
