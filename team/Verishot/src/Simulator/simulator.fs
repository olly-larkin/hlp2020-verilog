module Verishot.Simulator.Simulate

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

        let rec evaluate (acc: AccType) node: AccType =
            match node with
            | InputPin(name, _conns) ->
                let pinValue =
                    inputs
                    |> Map.tryFind name
                    |> Option.defaultWith
                        (fun () -> failwithf "Pin %s is unconnected" name)

                { acc with wireVals = acc.wireVals |> Map.add name pinValue }
            | OutputPin name ->
                let dependencies () =
                    net.nodes
                    |> List.filter (function
                        | InputPin(_, connections) ->
                            connections
                            |> List.exists (fun c -> c.target = PinTarget name)
                        | Constant constant ->
                            constant.connections
                            |> List.exists (fun c -> c.target = PinTarget name)
                        | ModuleInstance instance ->
                            instance.connections
                            |> Map.toList
                            |> List.collect snd
                            |> List.exists (fun c -> c.target = PinTarget name)
                        | OutputPin _ -> false)


                if Map.containsKey name acc.wireVals
                then acc
                else failwith "missing"

            | ModuleInstance(instance) -> acc
            | Constant _ -> acc

        let result = (initAcc, net.nodes) ||> List.fold evaluate

        { internals = result.wireVals
          outputs = Map.empty
          state = VerilogState result.nextState }
