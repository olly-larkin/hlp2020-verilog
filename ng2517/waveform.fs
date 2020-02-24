module Verishot.Waveform

open Verishot.CoreTypes
open Verishot.SVG
open WaveTypes


let styleprops = [("fill", "none");("stroke","black");("stroke-width","1")]
let objTextBox name = Rectangle((0.0,0.0),(6.0, 2.0) ,[("fill", "none"); ("stroke","black")], Some name) 

module Waveform =
       
    let addXY (inp:Coord) (x:float) (y:float) = 
        Coord(fst(inp) + x, snd(inp) + y)

    let Low2Low state:WaveformState = 
        //print some SVG using state
        //return something with type state but with 0 as the prevVal
        let createLine (w:WaveformState)  = 
            let endPoint = snd(getDimensionElem w.svgVals)
            let newPolyline = match w.svgVals with
                                | Polyline(x, y, z) -> Polyline(x @ [addXY endPoint 3.0 0.0],y ,z)
                                | _ -> w.svgVals // DEFO NEEDS TO BE CHANGED ADD ERROS
            {w with svgVals = newPolyline}

        let change2Low (w:WaveformState) = {w with prevVal = 0}
        let addLow2Low (w:WaveformState) = createLine w

        state |> change2Low |> addLow2Low

    let Low2High state = 
        //print some SVG using state
        //return something with type state but with 0 as the prevVal
        let createLine (w:WaveformState)  = 
            let endPoint = snd(getDimensionElem w.svgVals)
            let newPolyline = match w.svgVals with
                                | Polyline(x, y, z) -> Polyline(x @ [addXY endPoint 0.0 -2.4; addXY endPoint 3.0 -2.4 ],y ,z)
                                | _ -> w.svgVals // DEFO NEEDS TO BE CHANGED ADD ERROS
            {w with svgVals = newPolyline}

        let change2High (w:WaveformState) = {w with prevVal = 1}
        let addLow2High(w:WaveformState) = createLine w

        state |> change2High |> addLow2High

    
    let High2Low state = 
        //print some SVG using state
        //return something with type state but with 0 as the prevVal
        let createLine (w:WaveformState)  = 
            let endPoint = snd(getDimensionElem w.svgVals)
            let newPolyline = match w.svgVals with
                                | Polyline(x, y, z) -> Polyline(x @ [addXY endPoint 0.0 0.0; addXY endPoint 3.0 0.0;],y ,z)
                                | _ -> w.svgVals // DEFO NEEDS TO BE CHANGED ADD ERROS
            {w with svgVals = newPolyline}

        let change2Low (w:WaveformState) = {w with prevVal = 0}
        let addHigh2Low (w:WaveformState) = createLine w

        state |> change2Low |> addHigh2Low


    let High2High state = 
        let createLine (w:WaveformState)  = 
            let endPoint = snd(getDimensionElem w.svgVals)
            let newPolyline = match w.svgVals with
                                | Polyline(x, y, z) -> Polyline(x @ [addXY endPoint 3.0 -2.4],y ,z)
                                | _ -> w.svgVals // DEFO NEEDS TO BE CHANGED ADD ERROS
            {w with svgVals = newPolyline}

        let change2High (w:WaveformState) = {w with prevVal = 1}
        let addHigh2High (w:WaveformState) = createLine w

        state |> change2High |> addHigh2High



    let SingleCycle state (cycle:int) = 
        match (state.prevVal, cycle) with
        | 0,0 -> Low2Low state
        | 0,1 -> Low2High state
        | 1,0 -> High2Low state
        | 1,1 -> High2High state
        | _ -> state // DEFO NEEDS TO BE CHANGED ADD ERROS

    let GenPortWaveform (initialState:WaveformState) (portVals:int list) = 
        //returns a list of 
        (initialState, portVals) ||> List.fold SingleCycle

    let GenWireWaveform (portName:string) (vals:int list) : PortWaveform =
        let initState:WaveformState = {prevVal=0; svgVals = Polyline([0.0, 3.0], styleprops, None)}
        let waveform = GenPortWaveform initState vals
        WireWave{portName = objTextBox portName ; wave = waveform.svgVals}

    let GenBusWaveform (portName:string) (vals:(int * int list)list) =
        let initState:WaveformState = {prevVal=0; svgVals = Polyline([0.0, 3.0], styleprops, None)}
        let partialWaveform = GenPortWaveform initState
        let toWaveform (inp:(int * int list)) = (objTextBox(portName + string (fst(inp))), (partialWaveform (snd(inp))).svgVals)
        BusWave{portName = objTextBox portName; waveList = List.map toWaveform vals}

    let PortToWaveform (inp:SimulatorPort) =
        match inp with
        | SimWire wire -> GenWireWaveform wire.portName wire.output
        | SimBus bus   -> GenBusWaveform bus.portName bus.outputList

    let SetWirePos (offset:float) (port:WireWaveform)  =
        WireWave{portName = translateSVG (0.5, float(offset) * 3.0 + 0.8 ) port.portName; wave = translateSVG (7.0, offset *  3.0 ) port.wave }, offset + 1.0 

    let SetBusPos (offset:float) (port:BusWaveform) =
        let setIndexPos (offst:float) (idx:SVGElement*SVGElement) = (translateSVG (0.5, offst * 3.0 + 0.8) (fst(idx)) , translateSVG (7.0, offst * 3.0) (snd(idx))), offst + 1.0 
        let offsetWaves = List.mapFold setIndexPos (offset+1.1) port.waveList
        BusWave{portName = translateSVG (0.0, offset * 3.0 + 0.8) port.portName; waveList = fst(offsetWaves)}, snd(offsetWaves)


    let SetPortPosition (offset:float) (port:PortWaveform) = 
        match port with
        | WireWave wire -> SetWirePos offset wire
        | BusWave bus -> SetBusPos offset bus
    

    let DrawWire (svg:SVGElement) (port:WireWaveform) = 
        groupSVG [] None [svg; port.portName; port.wave]

    let DrawBus (svg:SVGElement) (port:BusWaveform) =
        let indexSVG (_svg:SVGElement ) (subport:SVGElement*SVGElement) =  groupSVG [] None [_svg;fst(subport); snd(subport)]
        let tmpSVG = groupSVG [] None [svg; port.portName]
        List.fold indexSVG tmpSVG port.waveList  
    
    let DrawPort (svg:SVGElement) (port:PortWaveform) = 
        match port with
            | WireWave wire -> DrawWire svg wire
            | BusWave bus -> DrawBus svg bus

    let SimOutputToWaveform (inp:SimulatorPort list) =
        List.map PortToWaveform inp
    
    let SetPosition (portList: PortWaveform list) =
        List.mapFold SetPortPosition 0.0 portList |> fst

    let GroupWaveformElements (outputList: PortWaveform list) = 
        List.fold DrawPort (Group([], [],None )) outputList