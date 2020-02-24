// Learn more about F# at http://fsharp.org
module Verishot.Main



open System
open System.IO
open Verishot.Waveform
open Verishot.CoreTypes
open Verishot.SVG
open Verishot.Util
open WaveTypes


let testInput = [
    0;1;0;1;1;1;0;0;1
]

let style = "fill:none;stroke:black;stroke-width:1"

let state:WaveformState =
    {
        prevVal=0;
        svgVals = Polyline([0.0, 3.0],[("","")], "")
    }

let toFile (modName, svgString) = writeStringToFile (sprintf "D:/Users/naim/Real Document/Project/University/Year_3/HLP/Group_Project/hlp2020-verilog/ng2517/outputsvg/%s.svg" modName) svgString

[<EntryPoint>]
let main argv =    
    let out = Waveform.ModuleWaveform (testInput,state)
    let outString = output out.svgVals style false
    ("Test",outString) |> toFile

    0 // return an integer exit code
