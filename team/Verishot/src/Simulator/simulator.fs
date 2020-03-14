module rec Verishot.Simulator.Simulate

open Verishot.CoreTypes
open Verishot.Megafunctions
open Verishot.Simulator.Types
open Verishot.Util

type SimulationResult =
    { outputs: WireValMap
      state: StateVar InstanceState }

type AccType =
    { nextState: StateVar State
      wireVals: WireValMap }

let evaluate (netlist: Netlist)
    (otherModules: Map<ModuleIdentifier, StateVar SimulationObject>)
    (lastState: StateVar State option) (inputs: WireValMap) (node: Node): EndpointValMap =
    match node with
    | InputPin(name) ->
        let inputVal: WireVal =
            inputs
            |> Map.tryFind name
            |> Option.defaultWith
                (fun () -> failwithf "Missing value for input pin %s" name)

        Map [ (PinEndpoint name, inputVal) ]
    | OutputPin(name, connections) ->
        connections
        |> List.map (fun conn ->
            match conn.source with
            | ConstantEndpoint value ->
                shiftAndMask conn.srcRange conn.targetRange value
            | _ ->
                netlist.nodes
                |> List.tryFind (matchesEndpoint conn.source)
                |> Option.defaultWith
                    (fun () ->
                        failwithf "Could not to find endpoint %A" conn.source)
                |> evaluate netlist otherModules lastState inputs
                |> Map.tryFind conn.source
                |> Option.defaultWith
                    (fun () ->
                        failwithf "Could not to find port %A" conn.source)
                |> (fun value ->
                    shiftAndMask conn.srcRange conn.targetRange value))
        |> List.fold (|||) 0UL
        |> (fun v -> Map [ (PinEndpoint name, v) ])

let getNetlistOutput (netlist: Netlist)
    (otherModules: Map<ModuleIdentifier, StateVar SimulationObject>)
    (lastState: StateVar State option) (inputs: WireValMap): WireValMap =

    netlist.nodes
    |> List.choose (function
        | OutputPin(name, _) as node ->
            let pinValue =
                evaluate netlist otherModules lastState inputs node
                |> Map.tryFind (PinEndpoint name)
                |> Option.defaultWith
                    (fun () ->
                        failwithf "Failed getting output on pin %s" name)

            Some(name, pinValue)
        | _ -> None)
    |> List.toMap


let matchesEndpoint (endpoint: Endpoint) (node: Node): bool =
    match (endpoint, node) with
    | PinEndpoint(name'), InputPin(name) -> name = name'
    | PinEndpoint(name'), OutputPin(name, _) -> name = name'
    | InstanceEndpoint(name, _), ModuleInstance(instance) ->
        instance.instanceName = name
    | _ -> false


let shiftAndMask srcRange targetRange value =
    match (srcRange, targetRange) with
    | Range(srcHigh, srcLow), Range(targetHigh, targetLow) ->
        let mask = ((pown 2UL (srcHigh - srcLow + 1)) - 1UL) <<< srcLow
        ((value &&& mask) >>> srcLow) <<< targetLow
    | Single, Single -> value
    | _ -> failwith "Mismatched ranges"
