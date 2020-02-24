module Verishot.Waveform

open Verishot.CoreTypes
open Verishot.SVG
open WaveTypes



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

    let ModuleWaveform (moduleSim:int list, initialState:WaveformState) = 
        //returns a list of 
        (initialState, moduleSim) ||> List.fold SingleCycle