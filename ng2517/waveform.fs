module Verishot.Waveform

open Verishot.CoreTypes
open Verishot.SVG
open WaveTypes


module Waveform =
       
    let addXY (inp:Coord) (x:float) (y:float) = 
        Coord(fst(inp) + x, snd(inp) + y)
    
    let styleprops = [("fill", "none");("stroke","black");("stroke-width","1")] //Standard style for waveforms

    let objTextBox name = 
        Group([Rectangle((0.0,0.0),(9.0, 4.0) ,[("fill", "none"); ("stroke","black")], Some name);
        Text((1.0,2.2),name,[("fill", "black")],Some name)],[],None) //standard style for waveform labels
                             
    let wrappedWave waveform = 
        let waveDimension = getDimensionElem waveform
        Group([Rectangle((0.0,0.0),(fst(snd(waveDimension))+0.5,4.0),[("fill", "none"); ("stroke","black")], None);
               translateSVG (0.5,0.5) waveform], [], None)               

    let initState:WaveformState = {prevVal=0; svgVals = Polyline([0.0, 3.0], styleprops, None)}

    let Low2Low state:WaveformState = 
        //print some SVG using state
        //return something with type state but with 0 as the prevVal
        let createLine (w:WaveformState)  = 
            let endPoint = snd(getDimensionElem w.svgVals)
            let newPolyline = match w.svgVals with
                                | Polyline(x, y, z) -> Polyline(x @ [addXY endPoint 5.0 0.0],y ,z)
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
                                | Polyline(x, y, z) -> Polyline(x @ [addXY endPoint 0.0 -3.0; addXY endPoint 5.0 -3.0 ],y ,z)
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
                                | Polyline(x, y, z) -> Polyline(x @ [addXY endPoint 0.0 0.0; addXY endPoint 5.0 0.0;],y ,z)
                                | _ -> w.svgVals // DEFO NEEDS TO BE CHANGED ADD ERROS
            {w with svgVals = newPolyline}

        let change2Low (w:WaveformState) = {w with prevVal = 0}
        let addHigh2Low (w:WaveformState) = createLine w

        state |> change2Low |> addHigh2Low

    let High2High state = 
        let createLine (w:WaveformState)  = 
            let endPoint = snd(getDimensionElem w.svgVals)
            let newPolyline = match w.svgVals with
                                | Polyline(x, y, z) -> Polyline(x @ [addXY endPoint 5.0 -3.0],y ,z)
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
        (initialState, portVals) ||> List.fold SingleCycle

    let GenWireWaveform (portName:string) (vals:int list) : PortWaveform =
        let waveform = GenPortWaveform initState vals
        let wireBlock = Group([objTextBox portName; translateSVG (9.0,0.0) (wrappedWave waveform.svgVals)],[], None)
        {waveBlock = wireBlock}

    let GenBusWaveform (portName:string) (vals:(int * int list)list) =
        let pinName pinNo = portName + "[" + string pinNo + "]"
        let singlePortWaveform = GenPortWaveform initState
        let individualPortWaveform (offset:float) (inp:(int * int list)) = (translateSVG (0.0,4.0 * offset) (Group([objTextBox (pinName (fst(inp))) ; translateSVG (9.0,0.0) (wrappedWave (singlePortWaveform (snd(inp))).svgVals)],[], None))), (offset + 1.0)
        {waveBlock = Group([objTextBox portName] @fst(List.mapFold individualPortWaveform 1.0 vals),[], None)}

    let SetPortPosition (offset:float) (port:PortWaveform) = 
        (translateSVG (0.0, offset) port.waveBlock), offset + snd(snd(getDimensionElem port.waveBlock))
    
    let clkCycle (state: SVGElement) (iteration: int) = 
        let endPoint = snd(getDimensionElem state)
        let newPolyline = match state with
                            | Polyline(x, y, z) -> Polyline(x @ [addXY endPoint 0.0 -3.0; addXY endPoint 2.5 -3.0; addXY endPoint 2.5 0.0;addXY endPoint 5.0 0.0;],y ,z)
                            | _ -> state // DEFO NEEDS TO BE CHANGED ADD ERROS
        newPolyline

    let GenClock (waveformList:SVGElement) =
        let viewerDimensions = getDimensionElem(waveformList)
        let noOfCylces = int((fst(snd(viewerDimensions)) - fst(fst(viewerDimensions)) - 9.5) / 5.0) // (MaxX - MinX - offsets)/width per cycle to get the number of cycles
        let clkWaveform = List.fold clkCycle (Polyline([0.0, 3.0], styleprops, None)) [1..noOfCylces]
        translateSVG (0.0,snd(snd(viewerDimensions))) (Group([objTextBox "clk"; translateSVG (9.0,0.0) (wrappedWave clkWaveform)],[], None))


    let SimOutputToWaveform (inp:SimulatorPort list) =
        let portToWaveform (prt:SimulatorPort) =
            match prt with
            | SimWire wire -> GenWireWaveform wire.portName wire.output
            | SimBus bus   -> GenBusWaveform bus.portName bus.outputList
        let waveformList = List.map portToWaveform inp
        let groupedWaveforms = Group(fst(List.mapFold SetPortPosition 0.0 waveformList),[], None)
        Group([groupedWaveforms ; GenClock groupedWaveforms], [], None)