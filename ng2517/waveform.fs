module Verishot.Waveform

open Verishot.CoreTypes



module Waveform =

    let Low2Low state:WaveformState = 
        //print some SVG using state
        //return something with type state but with 0 as the prevVal
        let calcLine (w:WaveformState) : string list  = //should return list of SVG lines
            ["hello"]
        let tmp = calcLine state
        let change2Low (w:WaveformState) = {w with prevVal = 0}
        let addLow2Low  (lines:string list) (w:WaveformState) = {w with svgVals = w.svgVals @ lines}

        state |> change2Low |> addLow2Low tmp

    let Low2High state = 
        let calcLine (w:WaveformState) : string list  = //should return list of SVG lines
            ["hello"]
        let tmp = calcLine state
        let change2High (w:WaveformState) = {w with prevVal = 1}
        let addLow2High  (lines:string list) (w:WaveformState) = {w with svgVals = w.svgVals @ lines}

        state |> change2High |> addLow2High tmp
    
    let High2Low state = 
        let calcLine (w:WaveformState) : string list  = //should return list of SVG lines
            ["hello"]
        let tmp = calcLine state
        let change2Low (w:WaveformState) = {w with prevVal = 0}
        let addHigh2Low  (lines:string list) (w:WaveformState) = {w with svgVals = w.svgVals @ lines}

        state |> change2Low |> addHigh2Low tmp

    let High2High state = 
        let calcLine (w:WaveformState) : string list  = //should return list of SVG lines
            ["hello"]
        let tmp = calcLine state
        let change2High (w:WaveformState) = {w with prevVal = 1}
        let addHigh2High  (lines:string list) (w:WaveformState) = {w with svgVals = w.svgVals @ lines}

        state |> change2High |> addHigh2High tmp


    let SingleCycle state cycle = 
        match (state.prevVal, cycle.val) with
        | 0,0 -> Low2Low state
        | 0,1 -> Low2High state
        | 1,0 -> High2Low state
        | 1,1 -> High2High state

    let ModuleWaveform (moduleSim, initialState:WaveformState) = 
        //returns a list of 
        (initialState, moduleSim) ||> List.fold SingleCycle