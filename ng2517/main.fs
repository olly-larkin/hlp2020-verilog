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
    SimWire{portName = "a"; output = [1;0;0;1;0;1;1;0]};
    SimWire{portName = "b"; output = [0;1;0;1;0;1;0;1]};
    SimWire{portName = "out"; output = [1;1;1;1;0;0;0;0]};
    SimBus{portName = "reg1"; range = 5; output = [0;0;2;2;4;4;13;20] };
    SimBus{portName = "reg2"; range = 5; output = [0;1;2;2;10;15;31;31] };
]


let styleprops = [("fill", "none");("stroke","black");("stroke-width","1")]

let toFile (modName, svgString) = writeStringToFile (sprintf "outputsvg/%s.svg" modName) svgString

[<EntryPoint>]
let main argv =    
    let svgGroup = testInput |> Waveform.SimOutputToWaveform
    let outString =  output svgGroup None None false
    ("Test",outString) |> toFile

    0 // return an integer exit code
