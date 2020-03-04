module Verishot.WaveformTesting


open WaveTypes
open Verishot.SVG
open Verishot.Waveform
open Expecto

let equalTestAsync =
    fun (name, inp, exp) ->
        testCaseAsync name <| async { Expect.equal inp exp name }  

let exceptionTestAsync  =
    fun (name, inp, exp) ->
        testCaseAsync name <| async { Expect.throws inp exp } 

[<Tests>]
let waveStateChangeTests = 
    
    let testWaveformState:WaveformState = {prevVal=0; svgVals = Polyline([0.0, 3.0], styleprops, None)}
    let incorrectWaveformState:WaveformState = {prevVal= 0; svgVals=Group([],[],None)}
    testList "Wire State Change Tests" ([
        ([
            "Low2Low Functional 0 to 0 test",
                testWaveformState |> Low2Low,
                    {prevVal = 0; svgVals = Polyline([0.0,3.0; 5.0,3.0], styleprops,None)}
            
            "Low2High Functional 0 to 1 test",
                testWaveformState |> Low2High,
                    {prevVal = 0; svgVals = Polyline([0.0,3.0; 0.0,0.0;5.0,0.0], styleprops,None)}

            "High2Low Functional 1 to 0 test",
                testWaveformState |> High2Low,
                    {prevVal = 1; svgVals = Polyline([0.0,3.0;0.0,3.0;5.0,3.0], styleprops,None)}

            "High2Low Functional 1 to 1 test",
                testWaveformState |> High2High,
                    {prevVal = 1; svgVals = Polyline([0.0,3.0;5.0,3.0], styleprops,None)}
                 


            ] |> List.map equalTestAsync)
        ([
            "Low2Low Error 0 to 0 test",
                (fun _ -> Low2Low incorrectWaveformState |> ignore),
                    "Error constructing wave"
                  
            ] |> List.map exceptionTestAsync)
    ] |> List.collect id)
