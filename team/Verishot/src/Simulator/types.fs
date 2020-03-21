module Verishot.Simulator.Types

open Verishot.CoreTypes

type WireVal = uint64

(** Simulation Netlist *)

type Endpoint =
    | PinEndpoint of pinName: Identifier
    | InstanceEndpoint of nodeName: Identifier * portName: Identifier
    | ConstantEndpoint of value: WireVal

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

(** Simulation state *)

type WireValMap = Map<Identifier, WireVal>
type EndpointValMap = Map<Endpoint, WireVal>

type 's Megafunction =
    | Combinational of CombinationalMegafunction
    | Stateful of 's StatefulMegafunction

and CombinationalMegafunction =
    { declaration: ModuleDecl
      simulate: WireValMap -> WireValMap }

and 's StatefulMegafunction =
    { declaration: ModuleDecl
      initialState: 's
      getNextState: 's -> WireValMap -> 's
      getOutput: 's -> WireValMap }

type 's State = Map<string, 's InstanceState>

and 's InstanceState =
    | VerilogState of 's State
    | MegafunctionState of 's

type 's SimulationObject =
    | NetlistInstance of Netlist
    | Megafunction of 's Megafunction
