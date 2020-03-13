module Verishot.Simulator

open Verishot.CoreTypes
open Verishot.CoreTypes.Netlist
open Verishot.CoreTypes.Simulator
open Verishot.Megafunctions

type SimulationResult =
    { internals: WireValMap
      outputs: WireValMap
      state: StateVar InstanceState }

type AccType =
    { nextState: StateVar State
      wireVals: WireValMap }

let simulate (instance: StateVar Instance) (lastState: StateVar InstanceState)
    (inputs: WireValMap): SimulationResult =
    match instance with
    | Megafunction mf ->
        let outputs, nextState =
            match lastState with
            | VerilogState _ -> failwith "VerilogState passed to FSharp module"
            | MegafunctionState s -> mf.simulate s inputs

        { internals = inputs
          outputs = outputs
          state = MegafunctionState nextState }

    | NetlistInstance net ->
        let initAcc: AccType =
            { nextState = (Map.empty: StateVar State)
              wireVals = Map.empty<Identifier, WireVal> }

        let rec recFold (acc: AccType) node: AccType =
            match node with
            | InputPin(name, _conns) ->
                let pinValue =
                    inputs
                    |> Map.tryFind name
                    |> Option.defaultWith
                        (fun () -> failwithf "Pin %s is unconnected" name)

                { acc with wireVals = acc.wireVals |> Map.add name pinValue }
            | OutputPin name -> acc
            | ModuleInstance(instance) -> acc
            | Constant _ -> acc

        let result = (initAcc, net.nodes) ||> List.fold recFold

        { internals = result.wireVals
          outputs = Map.empty
          state = VerilogState result.nextState }
