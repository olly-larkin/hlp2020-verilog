﻿// Learn more about F# at http://fsharp.org
module Verishot.Main



open System
open System.IO
open Verishot.Waveform
open Verishot.CoreTypes
open Verishot.SVG
open Verishot.Util
open WaveTypes


let testInput = [
    SimWire{portName = "a"; output = [1;0;0;1;0;1;1;0]};
    SimWire{portName = "b"; output = [0;1;0;1;0;1;0;1]};
    SimWire{portName = "out"; output = [1;1;1;1;0;0;0;0]};
    SimBus{portName = "reg1"; outputList = [(0,[1;0;0;1;0;1;1;0]); (1,[1;1;0;1;0;1;1;0]); (2,[1;0;1;1;0;0;1;0])]};
    SimBus{portName = "reg2"; outputList = [(0,[1;1;0;1;1;0;0;1]); (1,[1;0;1;0;0;0;1;0]); (2,[1;1;1;1;1;1;1;0])]};
]


let styleprops = [("fill", "none");("stroke","black");("stroke-width","1")]

let state:WaveformState =
    {
        prevVal=0;
        svgVals = Polyline([0.0, 3.0], styleprops, None)
    }

let toFile (modName, svgString) = writeStringToFile (sprintf "../../../outputsvg/%s.svg" modName) svgString

[<EntryPoint>]
let main argv =    
    let svgGroup = testInput |> Waveform.SimOutputToWaveform
    let outString =  output svgGroup None None false
    ("Test",outString) |> toFile

    0 // return an integer exit code