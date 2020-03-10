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
let waveStateChangeFunctionTests = 
    
    let testWaveformState:WaveformState = {prevVal=0; svgVals = Polyline([0.0, 3.0], styleprops, None)}
    let incorrectWaveformState:WaveformState = {prevVal= 0; svgVals=Group([],[],None)}
    testList "Wire State Change Tests for indv. state change functions" ([
        ([
            "Low2Low Functional 0 to 0 test",
                testWaveformState |> Low2Low,
                    {prevVal = 0; svgVals = Polyline([0.0,3.0; 5.0,3.0], styleprops,None)}
            
            "Low2High Functional 0 to 1 test",
                testWaveformState |> Low2High,
                    {prevVal = 1; svgVals = Polyline([0.0,3.0; 0.0,0.0;5.0,0.0], styleprops,None)}

            "High2Low Functional 1 to 0 test",
                testWaveformState |> High2Low,
                    {prevVal = 0; svgVals = Polyline([0.0,3.0;0.0,3.0;5.0,3.0], styleprops,None)}

            "High2High Functional 1 to 1 test",
                testWaveformState |> High2High,
                    {prevVal = 1; svgVals = Polyline([0.0,3.0;5.0,0.0], styleprops,None)}
                 


            ] |> List.map equalTestAsync)
        ([
            "Low2Low Error test",
                (fun _ -> Low2Low incorrectWaveformState |> ignore),
                    "Error constructing wave"
             
            "Low2High Error test",
               (fun _ -> Low2High incorrectWaveformState |> ignore),
                    "Error constructing wave"
            
            "High2Low Error test",
                (fun _ -> High2Low incorrectWaveformState |> ignore),
                    "Error constructing wave"

            "High2High Error test",
                (fun _ -> High2High incorrectWaveformState |> ignore),
                    "Error constructing wave"
            
            ] |> List.map exceptionTestAsync)
    ] |> List.collect id)

[<Tests>]
let waveStateChangeMatchTests = 
    let initLowTestWaveformState:WaveformState = {prevVal=0; svgVals = Polyline([0.0, 3.0], styleprops, None)}
    let initHighTestWaveformState:WaveformState = {prevVal=1; svgVals = Polyline([0.0, 3.0], styleprops, None)}
    let incorrectWaveformState:WaveformState = {prevVal= 2; svgVals=Group([],[],None)}
    testList "Wire State Change Tests for match function" ([
        ([
            "Low2Low Call test",
                (initLowTestWaveformState,0) ||> SingleWireCycle,
                    {prevVal = 0; svgVals = Polyline([0.0,3.0; 5.0,3.0], styleprops,None)}                 

            "Low2High Call test",
                (initLowTestWaveformState,1) ||> SingleWireCycle,
                    {prevVal = 1; svgVals = Polyline([0.0,3.0; 0.0,0.0;5.0,0.0], styleprops,None)}
                 
            "High2Low Call test",
                (initHighTestWaveformState,0) ||> SingleWireCycle,
                    {prevVal = 0; svgVals = Polyline([0.0,3.0;0.0,3.0;5.0,3.0], styleprops,None)} 

            "High2High Call test",
                (initHighTestWaveformState,1) ||> SingleWireCycle,
                    {prevVal = 1; svgVals = Polyline([0.0,3.0;5.0,0.0], styleprops,None)} 

            ] |> List.map equalTestAsync)
        ([
            "state.prevVal:Incorrect, cycle:Correct Error test",
                (fun _ -> SingleWireCycle incorrectWaveformState 0 |> ignore),
                    "Input value not valid"
             
            "state.prevVal:Correct, cycle:Incorrect Error test",
                (fun _ -> SingleWireCycle initLowTestWaveformState 2 |> ignore),
                    "Input value not valid"
            
            "state.prevVal:Incorrect, cycle:Incorrect Error test",
                (fun _ -> SingleWireCycle incorrectWaveformState 2 |> ignore),
                    "Input value not valid"
            
            ] |> List.map exceptionTestAsync)
    ] |> List.collect id)


[<Tests>]
let busStateChangeFunctionTests = 
    
    let testBusState1:WaveformState = {prevVal=0; svgVals = Group([Polyline([0.0, 3.0], [], None)], styleprops, None)}
    let testBusState2:WaveformState = {prevVal=5; svgVals = Group([Polyline([0.0, 3.0], [], None)], styleprops, None)}
    let test1ExpOutput:WaveformState = {prevVal=0; svgVals = Group([Polyline([0.0, 3.0], [], None); Polyline([0.0, 0.0;5.0, 0.0], [], None);Polyline([0.0, 3.0;5.0, 3.0], [], None)], styleprops, None)}
    let test2ExpOutput:WaveformState = {prevVal=5; svgVals = Group([Polyline([0.0, 3.0], [], None); Polyline([0.0, 0.0;5.0, 0.0], [], None);Polyline([0.0, 3.0;5.0, 3.0], [], None)], styleprops, None)}
    let test3ExpOutput:WaveformState = {prevVal=5; svgVals = Group([Polyline([0.0, 3.0], [], None); Polyline([0.0, 0.0;0.5, 3.0; 5.0,3.0], [], None);Polyline([0.0, 3.0;0.5, 0.0;5.0, 0.0], [], None); Text((0.6,1.9),"0x5",[("fill", "black")],None)], styleprops, None)}
    let test4ExpOutput:WaveformState = {prevVal=0; svgVals = Group([Polyline([0.0, 3.0], [], None); Polyline([0.0, 0.0;0.5, 3.0; 5.0,3.0], [], None);Polyline([0.0, 3.0;0.5, 0.0;5.0, 0.0], [], None); Text((0.6,1.9),"0x0",[("fill", "black")],None)], styleprops, None)}
    let incorrectBusWaveformState:WaveformState = {prevVal=0; svgVals = Polyline([0.0, 3.0], styleprops, None)}
    testList "Bus State Change Tests for indv. state change functions" ([
        ([
            "busSameVal Functional 0 to 0 test",
                (testBusState1, 0) ||> busSameVal,
                    test1ExpOutput
            
            "busSameVal Functional 5 to 5 test",
                (testBusState2, 5) ||> busSameVal,
                    test2ExpOutput

            "busChangeVal Functional 0 to 5 test",
                (testBusState1,5) ||> busChangeVal,
                    test3ExpOutput

            "busChangeVal Functional 5 to 0 test",
                (testBusState2,0) ||> busChangeVal,
                    test4ExpOutput
                 
            ] |> List.map equalTestAsync)
        ([
            "busSameVal Error test",
                (fun _ -> busSameVal incorrectBusWaveformState 0 |> ignore),
                    "Error constructing bus waveform"
             
            "busChangeVal Error test",
               (fun _ -> busChangeVal incorrectBusWaveformState 0 |> ignore),
                    "Error constructing wave"
            
            ] |> List.map exceptionTestAsync)
    ] |> List.collect id)

let busStateChangeMatchTests = 
    let testBusState1:WaveformState = {prevVal=0; svgVals = Group([Polyline([0.0, 3.0], [], None)], styleprops, None)}
    let testBusState2:WaveformState = {prevVal=5; svgVals = Group([Polyline([0.0, 3.0], [], None)], styleprops, None)}
    let test1ExpOutput:WaveformState = {prevVal=0; svgVals = Group([Polyline([0.0, 3.0], [], None); Polyline([0.0, 0.0;5.0, 0.0], [], None);Polyline([0.0, 3.0;5.0, 3.0], [], None)], styleprops, None)}
    let test2ExpOutput:WaveformState = {prevVal=5; svgVals = Group([Polyline([0.0, 3.0], [], None); Polyline([0.0, 0.0;5.0, 0.0], [], None);Polyline([0.0, 3.0;5.0, 3.0], [], None)], styleprops, None)}
    let test3ExpOutput:WaveformState = {prevVal=5; svgVals = Group([Polyline([0.0, 3.0], [], None); Polyline([0.0, 0.0;0.5, 3.0; 5.0,3.0], [], None);Polyline([0.0, 3.0;0.5, 0.0;5.0, 0.0], [], None); Text((0.6,1.9),"0x5",[("fill", "black")],None)], styleprops, None)}
    let test4ExpOutput:WaveformState = {prevVal=0; svgVals = Group([Polyline([0.0, 3.0], [], None); Polyline([0.0, 0.0;0.5, 3.0; 5.0,3.0], [], None);Polyline([0.0, 3.0;0.5, 0.0;5.0, 0.0], [], None); Text((0.6,1.9),"0x0",[("fill", "black")],None)], styleprops, None)}
    let incorrectBusWaveformState:WaveformState = {prevVal=0; svgVals = Polyline([0.0, 3.0], styleprops, None)}
    testList "Bus State Change Tests for match function" ([
        ([
            "busSameVal Call test1",
                (testBusState1,0) ||> SingleBusCycle,
                    test1ExpOutput                 

            "busSameVal Call test2",
                (testBusState2,5) ||> SingleBusCycle,
                    test2ExpOutput
                 
            "busChangeVal Call test1",
                (testBusState1,5) ||> SingleBusCycle,
                    test3ExpOutput 

            "busChangeVal Call test2",
                (testBusState2,0) ||> SingleBusCycle,
                    test4ExpOutput

            ] |> List.map equalTestAsync)
        ([
            //NO ERROR TESTS NEEDED SINCE FUNCTION CANNOT THROW EXCEPTIONS
            
            ] |> List.map exceptionTestAsync)
    ] |> List.collect id)




//Helper function tests.

let helperFunctionTests = 
    let testSVGElement = Polyline([0.0, 3.0;5.0,3.0], styleprops, None)
    let errorSVGElement = Group([], styleprops, None)
    let (testMap:Map<int, int list>) = Map[(1,[0;1])]
    let textBoxFuncTest =
        equalTestAsync ("Function: testBox, Input:string, Output:SVGElement" , (textBox "testString"), Group([Rectangle((0.0,0.0),(9.0, 4.0) ,[("fill", "none"); ("stroke","black")], Some "testString");Text((1.0,2.2),"testString",[("fill", "black")],Some "testString")],[],None))

    let wrappedWaveFuncTest =
        equalTestAsync ("Function: wrappedWave, Input:SVGElement, Output:SVGElement", (wrappedWave testSVGElement), Group([Rectangle((0.0,0.0),(5.5,4.0),[("fill", "none"); ("stroke","black")], None );Polyline([0.5, 3.5;5.5,3.5], styleprops, None) ], [], None))
            
    let setPortPositionFuncTest =
        equalTestAsync ("Function: setPortPosition, Input:(float, SVGElement), Output:SVGElement", (setPortPosition 3.0 testSVGElement),(Polyline([0.0, 6.0;5.0,6.0], styleprops, None), 6.0))
    
    let addPinValToMapTest1 =
        equalTestAsync ("Function: addPinValToMap, Input:Map, int*int, Output:Map Test:Add element to pre-existing key", (addPinValToMap testMap (1,1)), Map[(1,[0;1;1])])

    let addPinValToMapTest2 =
        equalTestAsync ("Function: addPinValToMap, Input:(Map, int*int), Output:Map Test:Add element to key not in map", (addPinValToMap testMap (2,1)), Map[(1,[0;1]);(2,[1])])

    let keyCheckTest1 =
        equalTestAsync ("Function: KeyCheck, Input:(Map, int), Output:int list, Test: Check for key already in map", (keyCheck testMap 1), [0;1])
    
    let keyCheckTest2 =
        equalTestAsync ("Function: KeyCheck, Input:(Map, int), Output:int list, Test: Check for key not in map", (keyCheck testMap 2), [])
    
    let clkCycleFuncTest =
        equalTestAsync ("Function: clkCycle, Input:(SVGElement, int), Output:SVGElement, Test: Able to add a single scycle to a line", (clkCycle testSVGElement 1), Polyline([0.0,3.0; 5.0,3.0; 5.0,0.0; 7.5,0.0; 7.5,3.0; 10.0,3.0], styleprops, None))

    let clkCycleErrorTest =
        exceptionTestAsync ("Function: clkCycle, Input:(SVGElement, int), Output:SVGElement, Test: Throw error when input isn't a polyline", (fun _ -> clkCycle errorSVGElement 1 |> ignore), "Error generating clock")

    testList "Functional Tests for all helper functions" [
        textBoxFuncTest
        wrappedWaveFuncTest
        setPortPositionFuncTest
        addPinValToMapTest1
        addPinValToMapTest2
        keyCheckTest1
        keyCheckTest2
        clkCycleFuncTest
        clkCycleErrorTest
    ]       
       

let runAllTests() =
    let collatedTests = testList "All tests collated" [
            waveStateChangeFunctionTests
            waveStateChangeMatchTests
            busStateChangeFunctionTests
            busStateChangeMatchTests
            helperFunctionTests
        ]
    runTests defaultConfig collatedTests |>ignore
//      Add tests for clock generation
//      Add tests for sim output match function