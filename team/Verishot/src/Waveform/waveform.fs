module Verishot.Waveform

open Verishot.SVG
open Verishot.Simulator.Types
open WaveTypes
open Verishot.WaveformStyles

   
let addXY (inp:Coord) (x:float) (y:float) = 
    Coord(fst(inp) + x, snd(inp) + y)

let styleprops = [("fill", "none");("stroke","black");("stroke-width","1")] //Standard style for waveforms

let textBox name = 
    Group([Rectangle((0.0,0.0),(9.0, 4.0) ,[("fill", "none"); ("stroke","black")], Some name);
        Text((1.0,2.2),name,[("fill", "black")],Some name)],[],None) //standard style for waveform labels
                         
let wrappedWave waveform = 
    let waveDimension = getDimensionElem waveform
    Group([Rectangle((0.0,0.0),(fst(snd(waveDimension))+0.5,4.0),[("fill", "none"); ("stroke","black")], None);
           translateSVG (0.5,0.5) waveform], [], None)               

let setPortPosition (offset:float) (port:SVGElement) = 
    (translateSVG (0.0, offset) port), offset + snd(snd(getDimensionElem port))

let keyCheck (map: Map<int, WireVal list>) (idx:int) = 
    if map.ContainsKey idx then map.[idx] else []

let addPinValToMap (map: Map<int, WireVal list>) (pinVal:(int*WireVal)) = 
    map.Add ((fst(pinVal)),((keyCheck map (fst(pinVal))) @ [snd(pinVal)]))

let initWireState:WaveformState = {prevVal=0UL; svgVals = Polyline([0.0, 3.0], styleprops, None)}
let initBusState:WaveformState = {prevVal=0UL; svgVals = Group([Polyline([0.0, 3.0], [], None)], styleprops, None)}

let Low2Low state:WaveformState = 
    //print some SVG using state
    //return something with type state but with 0 as the prevVal
    let addLine (w:WaveformState)  = 
        let endPoint = snd(getDimensionElem w.svgVals)
        let newPolyline = match w.svgVals with
                            | Polyline(x, y, z) -> Polyline(x @ [addXY endPoint 5.0 0.0],y ,z)
                            | _ -> failwith "Error constructing wave"
        {w with svgVals = newPolyline}

    let change2Low (w:WaveformState) = {w with prevVal = 0UL}
    state 
    |> change2Low 
    |> addLine

let Low2High state = 
    //print some SVG using state
    //return something with type state but with 0 as the prevVal
    let addLine (w:WaveformState)  = 
        let endPoint = snd(getDimensionElem w.svgVals)
        let newPolyline = match w.svgVals with
                            | Polyline(x, y, z) -> Polyline(x @ [addXY endPoint 0.0 -3.0; addXY endPoint 5.0 -3.0 ],y ,z)
                            | _ -> failwith "Error constructing wave"
        {w with svgVals = newPolyline}

    let change2High (w:WaveformState) = {w with prevVal = 1UL}

    state 
    |> change2High 
    |> addLine

let High2Low state = 
    //print some SVG using state
    //return something with type state but with 0 as the prevVal
    let addLine (w:WaveformState)  = 
        let endPoint = snd(getDimensionElem w.svgVals)
        let newPolyline = match w.svgVals with
                            | Polyline(x, y, z) -> Polyline(x @ [addXY endPoint 0.0 0.0; addXY endPoint 5.0 0.0;],y ,z)
                            | _ -> failwith "Error constructing wave"
        {w with svgVals = newPolyline}

    let change2Low (w:WaveformState) = {w with prevVal = 0UL}

    state 
    |> change2Low 
    |> addLine

let High2High state = 
    let addLine (w:WaveformState)  = 
        let endPoint = snd(getDimensionElem w.svgVals)
        let newPolyline = match w.svgVals with
                            | Polyline(x, y, z) -> Polyline(x @ [addXY endPoint 5.0 -3.0],y ,z)
                            | _ -> failwith "Error constructing wave"
        {w with svgVals = newPolyline}

    let change2High (w:WaveformState) = {w with prevVal = 1UL}

    state 
    |> change2High
    |> addLine

let busSameVal state newVal =
    let addLine (w:WaveformState)  = 
        let endPoint = snd(getDimensionElem w.svgVals)
        let busLine = [Polyline([addXY endPoint 0.0 -3.0; addXY endPoint 5.0 -3.0],[],None);Polyline([endPoint; addXY endPoint 5.0 0.0],[],None)]
        let newBusWave = match w.svgVals with
                            | Group(x, y, z) -> Group(x @ busLine ,y ,z)
                            | _ -> failwith "Error constructing bus waveform"
        {w with svgVals = newBusWave}
    
    let updateState (w:WaveformState) = {w with prevVal = newVal}

    state 
    |> updateState
    |> addLine

let busChangeVal state newVal =
    let addLine (w:WaveformState)  = 
        let endPoint = snd(getDimensionElem w.svgVals)
        let busLine = [Polyline([addXY endPoint 0.0 -3.0; addXY endPoint 0.5 0.0; addXY endPoint 5.0 0.0],[],None); Polyline([endPoint; addXY endPoint 0.5 -3.0; addXY endPoint 5.0 -3.0],[],None)]
        let busVal = "0x" + (sprintf "%X" newVal)
        let busLineAndVal = busLine @ [Text((addXY endPoint 0.6 -1.1),busVal,[("fill", "black")],None)]
        let newBusWave = match w.svgVals with
                            | Group(x, y, z) -> Group(x @ busLineAndVal ,y ,z)
                            | _ -> failwith "Error constructing bus waveform"
        {w with svgVals = newBusWave}
    
    let updateState (w:WaveformState) = {w with prevVal = newVal}

    state 
    |> updateState
    |> addLine

/// Chooses the correct state transition for the current wire waveform being generated
let SingleWireCycle state cycle = 
    match (state.prevVal, cycle) with
    | 0UL,0UL -> Low2Low state
    | 0UL,1UL -> Low2High state
    | 1UL,0UL -> High2Low state
    | 1UL,1UL -> High2High state
    | _ -> failwith "Input value not valid"

/// Checks whether or not the bus value has changed from the previous cycle, and calls the corresponding function to generate the bus waveform
let SingleBusCycle state (newVal:WireVal) = 
    match state.prevVal = newVal with
    | true  -> busSameVal state newVal
    | false -> busChangeVal state newVal


let GenPortWireWaveform (initialState:WaveformState) (portVals:WireVal list) = 
    (initialState, portVals) ||> List.fold SingleWireCycle

let GenPortBusWaveform (initialState:WaveformState) (portVals:WireVal list) = 
    (initialState, portVals) ||> List.fold SingleBusCycle

/// This function handles creating and formatting the waveforms for a bus. Vals is a list of values (can be either 0 or 1) the wire takes at each cycle.
let GenWireWaveform (portName:string) (vals) =
    let waveform = GenPortWireWaveform initWireState vals
    let wireBlock = Group([textBox portName; translateSVG (9.0,0.0) (wrappedWave waveform.svgVals)],[], None)
    wireBlock


/// This function handles creating and formatting the waveforms for a bus. Vals is a list of values the bus takes at each cycle.
let GenBusWaveform (portName:string) (portRange: int) (vals: WireVal list) =
    let pinName pinNo = portName + "[" + string pinNo + "]"
    let singlePortWaveform = GenPortWireWaveform initWireState
    let decToBinary (dec:WireVal) (idx:int) = (idx, dec%2UL), dec/2UL  //mapFolding function which returns a list of index, value tuples
    let addCycleToMap (busWaveforms: Map<int, WireVal list>) (decVal: (int*WireVal) list) = 
        (busWaveforms, decVal) ||> List.fold addPinValToMap 
    let decToPinVals (dec:WireVal) =
        [0..(portRange-1)]
        |> List.mapFold decToBinary dec
        |> fst
    let decToWaveformList = 
        vals
        |> List.map decToPinVals
        |> List.fold addCycleToMap Map.empty
        |> Map.toList
    let createWaveBox (inp:(int * WireVal list)) = 
        let svgList = 
            [(singlePortWaveform (snd(inp))).svgVals
                |> wrappedWave
                |> translateSVG (9.0,0.0)]
            |> (@) [textBox (pinName (fst(inp)))]
        Group(svgList,[], None)
    let individualPortWaveform (offset:float) (inp:(int * WireVal list)) = translateSVG (0.0,4.0 * offset) (createWaveBox inp) , (offset + 1.0)
    let busBox  = 
        let svgList = 
            [(GenPortBusWaveform initBusState vals).svgVals
                |> wrappedWave
                |> translateSVG (9.0,0.0)]
            |> (@) [textBox portName]
        Group(svgList,[], None)
    // let blockList =
    //     List.mapFold individualPortWaveform 1.0 decToWaveformList
    //     |> fst
    //     |> (@) [busBox]
    // Group(blockList,[], None)
    busBox



let clkCycle (state: SVGElement) (iteration: int) = 
    let endPoint = snd(getDimensionElem state)
    let newPolyline = match state with
                        | Polyline(x, y, z) -> Polyline(x @ [addXY endPoint 0.0 -3.0; addXY endPoint 2.5 -3.0; addXY endPoint 2.5 0.0;addXY endPoint 5.0 0.0;],y ,z)
                        | _ -> failwith "Error generating clock"
    newPolyline

///Generate an SVGElement with the clock waveform, should be called after generating simulator waveforms.
let GenClock (waveformList:SVGElement) =
    let viewerDimensions = getDimensionElem(waveformList)
    let noOfCylces = int((fst(snd(viewerDimensions)) - fst(fst(viewerDimensions)) - 9.5) / 5.0) // (MaxX - MinX - offsets)/width per cycle to get the number of cycles
    let clkWaveform = 
        ((Polyline([0.0, 3.0], styleprops, None)), [1..noOfCylces]) ||> List.fold clkCycle 
    translateSVG (0.0,snd(snd(viewerDimensions))) (Group([textBox "clk"; translateSVG (9.0,0.0) (wrappedWave clkWaveform)],[], None))

///Main function of the module. Take in the simulator output and returns an SVG element with all waveforms formatted and ready to be printed.
let SimOutputToWaveform (inp:SimulatorPort list) =
    let portToWaveform (prt:SimulatorPort) =
        match prt with
        | SimWire wire -> GenWireWaveform wire.portName wire.output
        | SimBus bus   -> GenBusWaveform bus.portName bus.range bus.output
    let waveformList = List.map portToWaveform inp
    let groupedWaveforms = Group(fst(List.mapFold setPortPosition 0.0 waveformList),[], None)
    Group([groupedWaveforms ; GenClock groupedWaveforms], [], None)
    
/// TOP Level main func, take simulator output, returns string of outputSVG
let waveformMain (inp: SimulatorPort list) = 
    let svg = inp |> SimOutputToWaveform
    let styles = Some <| loadStyles unitPx
    let script = None
    output svg styles script false