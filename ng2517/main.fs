// Learn more about F# at http://fsharp.org
module Verishot.Main



open System
open Verishot.Waveform
open Verishot.CoreTypes


let testInput = [

]

let state:WaveformState =
    {
        prevVal=0;
        coords=(10,01);
        svgVals = []
    }

[<EntryPoint>]
let main argv =    
    List.map Waveform.ModuleWaveform testInput
    0 // return an integer exit code
