module rec Verishot.Simulator.Netlist

open Verishot
open Verishot.CoreTypes
open Verishot.Util

module Netlist = Verishot.CoreTypes.Netlist
module Simulator = Verishot.Simulator.Types

(* OH GOD MY EYES!!! IT BUUUUUUUUUUURNS *)
// TODO Make this look decent

let convertNetlist (netlistIn: Netlist.Netlist): Simulator.Netlist =
    let moduleName = netlistIn.moduleName
    let nodes = Internal.convertNodes netlistIn.nodes

    { moduleName = moduleName
      nodes = nodes }

module Internal =
    let convertNodes nodesIn: Simulator.Node list =
        nodesIn
        |> List.choose (function
            | Netlist.InputPin(name, _) -> Some(Simulator.InputPin(name))
            | Netlist.OutputPin(name) ->
                Simulator.OutputPin
                    (name, (collectPinConnections nodesIn name)) |> Some
            | Netlist.ModuleInstance instance ->
                Simulator.ModuleInstance
                    { moduleName = instance.moduleName
                      instanceName = instance.instanceName
                      connections =
                          collectInstanceConnections nodesIn
                              instance.instanceName }
                |> Some
            | Netlist.Constant _ -> None)


    let collectPinConnections inNodes targetName: Simulator.Connection list =
        let target = Netlist.PinTarget targetName

        inNodes
        |> List.collect (function
            | Netlist.InputPin(srcName, connections) ->
                connections
                |> List.choose (fun connection ->
                    if connection.target = target then
                        Some
                            { srcRange = connection.srcRange
                              targetRange = connection.targetRange
                              source = Simulator.PinEndpoint srcName }
                    else
                        None)
            | Netlist.ModuleInstance(instance) ->
                instance.connections
                |> Map.collect (fun portName connections ->
                    connections
                    |> List.choose (fun connection ->
                        if connection.target = target then
                            Some
                                { srcRange = connection.srcRange
                                  targetRange = connection.targetRange
                                  source =
                                      Simulator.InstanceEndpoint
                                          (instance.instanceName, portName) }
                        else
                            None))
            | Netlist.Constant c ->
                c.connections
                |> List.choose (fun connection ->
                    if connection.target = target then
                        Some
                            { srcRange = connection.srcRange
                              targetRange = connection.targetRange
                              source =
                                  Simulator.ConstantEndpoint(c.width, c.value) }
                    else
                        None)
            | Netlist.OutputPin(_) -> [])

    let collectInstanceConnections inNodes targetName: Map<Identifier, Simulator.Connection list> =
        inNodes
        |> List.collect (function
            | Netlist.InputPin(srcName, connections) ->
                connections
                |> List.choose (fun connection ->
                    match connection.target with
                    | Netlist.InstanceTarget(nodeName, portName) when nodeName =
                                                                          targetName ->
                        Some
                            (portName,
                             { Simulator.Connection.srcRange =
                                   connection.srcRange
                               Simulator.Connection.targetRange =
                                   connection.targetRange
                               Simulator.Connection.source =
                                   Simulator.PinEndpoint(srcName) })
                    | _ -> None)
            | Netlist.ModuleInstance(instance) ->
                instance.connections
                |> Map.collect (fun srcPortName connections ->
                    connections
                    |> List.choose (fun connection ->
                        match connection.target with
                        | Netlist.InstanceTarget(nodeName, targetPortName) when nodeName =
                                                                                    targetName ->
                            Some
                                (targetPortName,
                                 { Simulator.Connection.srcRange =
                                       connection.srcRange
                                   Simulator.Connection.targetRange =
                                       connection.targetRange
                                   Simulator.Connection.source =
                                       Simulator.InstanceEndpoint
                                           (instance.instanceName, srcPortName) })
                        | _ -> None))
            | Netlist.Constant c ->
                c.connections
                |> List.choose (fun connection ->
                    match connection.target with
                    | Netlist.InstanceTarget(nodeName, portName) when nodeName =
                                                                          targetName ->
                        Some
                            (portName,
                             { Simulator.Connection.srcRange =
                                   connection.srcRange
                               Simulator.Connection.targetRange =
                                   connection.targetRange
                               Simulator.Connection.source =
                                   Simulator.ConstantEndpoint(c.width, c.value) })
                    | _ -> None)
            | Netlist.OutputPin(_) -> [])
        |> Map.ofListAll
