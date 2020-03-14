module Verishot.Simulator.Types

open Verishot.CoreTypes
open Verishot.CoreTypes.Simulator

(***
    The way connections are represented in this module is the following:
    Every node (i.e. pin or module instance) holds references for its
    *outgoing* connections.
*)

type Endpoint =
    | PinEndpoint of pinName: Identifier
    | InstanceEndpoint of nodeName: Identifier * portName: Identifier
    | ConstantEndpoint of width: int * value: int

type Connection =
    { source: Endpoint
      srcRange: Range
      targetRange: Range }

/// Instance of a module.
type ModuleInstance =
    { /// Name of the module being declared (first identifier in verilog declaration)
        moduleName: ModuleIdentifier

        /// Name of the instance (second identifier in verilog declaration)
        instanceName: Identifier

        /// *Incoming* connections of the module
        connections: Map<Identifier, Connection list> }

type Node =
    | InputPin of Identifier
    | OutputPin of Identifier * Connection list
    | ModuleInstance of ModuleInstance

type Netlist =
    { nodes: Node list
      moduleName: Identifier }
