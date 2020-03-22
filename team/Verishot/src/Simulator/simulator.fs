module rec Verishot.Simulator.Simulate

open Verishot.CoreTypes
open Verishot.Megafunctions.Types
open Verishot.Simulator.Types
open Verishot.Util

type SimulationResult =
    { outputs: WireValMap
      state: StateVar InstanceState }

type AccType =
    { nextState: StateVar State
      wireVals: WireValMap }

// This will return the results in the "wrong order" but this means that the final result is at the head of the list
// Nice for testing
let simulateCycles (cycles: uint64) (netlist: Netlist)
    (otherModules: Map<ModuleIdentifier, StateVar SimulationObject>)
    (initialState: StateVar State) (inputs: WireValMap): WireValMap list =

    (([Map.empty], initialState), [ 1UL .. cycles ])
    ||> List.fold
            (fun (outputs, state) _cycle ->
                let output', state' = simulate netlist otherModules state inputs
                output' :: outputs, state')
    |> fst

let simulate (netlist: Netlist)
    (otherModules: Map<ModuleIdentifier, StateVar SimulationObject>)
    (lastState: StateVar State) (inputs: WireValMap): WireValMap * StateVar State =

    let outputs = getNetlistOutput netlist otherModules lastState inputs
    let nextState =
        getNextNetlistState netlist otherModules lastState inputs

    outputs, nextState

let evaluate (netlist: Netlist)
    (otherModules: Map<ModuleIdentifier, StateVar SimulationObject>)
    (lastState: StateVar State) (inputs: WireValMap) (node: Node): EndpointValMap =

    let getPortValue = getPortValue netlist otherModules lastState inputs

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
        |> getPortValue
        |> (fun v -> Map [ (PinEndpoint name, v) ])
    | ModuleInstance instance ->
        let inputValues(): WireValMap =
            instance.connections |> Map.mapValues getPortValue

        otherModules
        |> Map.tryFind instance.moduleName
        |> Option.defaultWith
            (fun () ->
                failwithf "Module %s does not exist" (instance.moduleName.ToString()))
        |> function
        | Megafunction(Combinational mf) ->
            mf.simulate (inputValues())
            |> Map.mapKeys
                (fun portName ->
                    InstanceEndpoint(instance.instanceName, portName))
        | Megafunction(Stateful mf) ->
            lastState
            |> Map.tryFind instance.instanceName
            |> Option.map (function
                | MegafunctionState s -> s
                | VerilogState _ -> failwith "Unexpected verilog state")
            |> Option.defaultValue mf.initialState
            |> mf.getOutput
            |> Map.mapKeys
                (fun portName ->
                    InstanceEndpoint(instance.instanceName, portName))


let getNetlistOutput (netlist: Netlist)
    (otherModules: Map<ModuleIdentifier, StateVar SimulationObject>)
    (lastState: StateVar State) (inputs: WireValMap): WireValMap =

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

let getNextNetlistState (netlist: Netlist)
    (otherModules: Map<ModuleIdentifier, StateVar SimulationObject>)
    (lastState: StateVar State) (inputs: WireValMap): StateVar State =

    let getPortValue = getPortValue netlist otherModules lastState inputs

    netlist.nodes
    |> List.choose (function
        | ModuleInstance instance ->
            otherModules
            |> Map.tryFind instance.moduleName
            |> Option.defaultWith
                (fun () ->
                    failwithf "Module %s does not exist" instance.instanceName)
            |> function
            | Megafunction(Combinational _) -> None
            | Megafunction(Stateful mf) ->
                let thisLastState =
                    lastState
                    |> Map.tryFind instance.instanceName
                    |> function
                    | Some(MegafunctionState s) -> s
                    | Some(VerilogState _) ->
                        failwith "Unexpected verilog state"
                    | None -> mf.initialState

                let inputValues: WireValMap =
                    instance.connections |> Map.mapValues getPortValue

                Some
                    (instance.instanceName,
                     MegafunctionState
                     <| mf.getNextState thisLastState inputValues)
            | NetlistInstance subNetlist ->
                let submoduleState =
                    lastState
                    |> Map.tryFind instance.instanceName
                    |> function
                    | Some(MegafunctionState _) ->
                        failwith "Unexpected verilog state"
                    | Some(VerilogState s) -> s
                    | None -> Map.empty

                let submoduleInputs: WireValMap =
                    instance.connections |> Map.mapValues getPortValue

                Some
                    (instance.instanceName,
                     VerilogState
                     <| getNextNetlistState subNetlist otherModules
                            submoduleState submoduleInputs)
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
        ((value <<< (63 - srcHigh)) >>> (63 - srcHigh + srcLow)) <<< targetLow
    | Single, Single -> value
    | _ -> failwith "Mismatched ranges"

let getPortValue (netlist: Netlist) otherModules lastState inputs =
    List.map (fun conn ->
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
                (fun () -> failwithf "Could not to find port %A" conn.source)
            |> (fun value ->
                shiftAndMask conn.srcRange conn.targetRange value))
    >> List.fold (|||) 0UL
