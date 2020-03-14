module Verishot.Simulator.Types

open Verishot.CoreTypes

type WireVal = uint64

type WireValMap = Map<Identifier, WireVal>

type 's Megafunction =
    { declaration: ModuleDecl
      initialState: 's
      simulate: 's -> WireValMap -> (WireValMap * 's) }

type 's State = Map<string, 's InstanceState * WireValMap>

and 's InstanceState =
    | VerilogState of 's State
    | MegafunctionState of 's

type 's Instance =
    | NetlistInstance of Netlist.Netlist
    | Megafunction of 's Megafunction

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
