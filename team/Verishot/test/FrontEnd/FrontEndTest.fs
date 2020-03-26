module Verishot.FrontEnd.Test

open Expecto
open Verishot.Util
open Verishot.CoreTypes
open Verishot.Test.Util

let parseAndMapTests = 
    [
        "basic",
            ["bar=2;"; "foo=1;  "],
                Map [
                    ("foo", Single), { wireVal=1UL; str="foo=1;  " }
                    ("bar", Single), { wireVal=2UL; str="bar=2;" }
                ]
        "ranged",
            ["foo[32:3]=7'hFF;";"bar[4]=1'b1;"],
                Map [
                    ("foo", Range (32, 3)), { wireVal=255UL; str="foo[32:3]=7'hFF;" }
                    ("bar", Range (4, 4)), { wireVal=1UL; str="bar[4]=1'b1;" }
                ]
        "invalid",
            ["fuu=;"; "foo=1"; "bar"; "car"; "=3"; "life=4'h42"; "some[1:4]=12;"],
                Map []
    ]

    
[<Tests>]
let parseAndMapTestList =
    testList "parseAndMap"
    <| (parseAndMapTests
        |> List.map (processIntoAsyncTestList1 parseAndMap))

let matchMapWithInputPortsTests = 
    [
        "basic passing", 
            (Map [ 
                ("__CYCLES__", Single), { wireVal=100UL; str="DOES NOT MATTER" }
                ("__BREAK_DOWN_BUSSES__", Single), { wireVal=0UL; str="DOES NOT MATTER"}
            ], [], []),
                Ok ""
        "basic with one input",
            (Map [
                ("__CYCLES__", Single), { wireVal=100UL; str="DOES NOT MATTER" }
                ("__BREAK_DOWN_BUSSES__", Single), { wireVal=0UL; str="DOES NOT MATTER"}
                ("foo", Single), { wireVal=1UL; str="DOES NOT MATTER" }
            ], [
                ("foo", Single)
            ], []),
                Ok ""
        "cycles forgotten",
            (Map [], [], []),
                Error (exitCodes.vInError, "") // killed the invalid or not found
        "inp forgotten", 
            (Map [ 
                ("__CYCLES__", Single), { wireVal=100UL; str="DOES NOT MATTER" }
                ("__BREAK_DOWN_BUSSES__", Single), { wireVal=0UL; str="DOES NOT MATTER"}
            ], [
                ("foo", Range(32, 2))
            ], []),
                Error (exitCodes.vInError, "")
        "multiple forgotten", 
            (Map [ 
            ], [
                ("foo", Range(32, 2))
                ("bar", Single)
                ("car", Range(4, 4))
            ], []),
                Error (exitCodes.vInError, "")
        "basic with one output",
            (Map [
                ("__CYCLES__", Single), { wireVal=100UL; str="DOES NOT MATTER" }
                ("__BREAK_DOWN_BUSSES__", Single), { wireVal=0UL; str="DOES NOT MATTER"}
                ("foo", Single), { wireVal=1UL; str="DOES NOT MATTER" }
            ], [], [("foo", Single)]),
                Ok ""
        "basic with one input and one output",
            (Map [
                ("__CYCLES__", Single), { wireVal=100UL; str="DOES NOT MATTER" }
                ("__BREAK_DOWN_BUSSES__", Single), { wireVal=0UL; str="DOES NOT MATTER"}
                ("foo", Single), { wireVal=1UL; str="DOES NOT MATTER" }
                ("bar", Single), { wireVal=1UL; str="DOES NOT MATTER" }
            ], [("bar", Single)], [("foo", Single)]),
                Ok ""
    ]
    
[<Tests>]
let matchMapWithInputPortsTestList =
    testList "matchMapWithInputPorts"
    <| (matchMapWithInputPortsTests
        |> List.map (processIntoAsyncTestList3 matchMapWithInputPorts))


let getVinContentTests = 
    [
        "basic nothing in map",
            ([("foo", Single); ("bar", Range (3, 0)); ("car", Range(4, 4))], [], Map []),
                "// ===== Verishot Simulation File =====

// Specify how many cycles to simulate below
__CYCLES__=10;

// Specify (0 or 1) if you would like busses to be broken down
__BREAK_DOWN_BUSSES__=0;


// Specify each input on a new line (you may use Verilog style numeric constants)
foo=;
bar[3:0]=;
car[4]=;".Replace("\r", "");


        "all matching in map",
            ([("foo", Single); ("bar", Range (3, 0)); ("car", Range(4, 4))], [], 
                Map [
                    ("foo", Single), { wireVal=1UL; str="foo=1;"}
                    ("bar", Range(3, 0)), { wireVal=2UL; str="bar=2;"}
                    ("car", Range(4, 4)), { wireVal=1UL; str="car=1;" }
                    ]),
                "// ===== Verishot Simulation File =====

// Specify how many cycles to simulate below
__CYCLES__=10;

// Specify (0 or 1) if you would like busses to be broken down
__BREAK_DOWN_BUSSES__=0;


// Specify each input on a new line (you may use Verilog style numeric constants)
foo=1;
bar=2;
car=1;".Replace("\r", "");


        "all matching in map plus cycles",
            ([("foo", Single); ("bar", Range (3, 0)); ("car", Range(4, 4))],
             [], 
                Map [
                    ("foo", Single), { wireVal=1UL; str="foo=1;"}
                    ("bar", Range(3, 0)), { wireVal=2UL; str="bar=2;"}
                    ("car", Range(4, 4)), { wireVal=1UL; str="car=1;" }
                    ("__CYCLES__", Single), { wireVal=120UL; str="__CYCLES__=120;"}
                    ("__BREAK_DOWN_BUSSES__", Single), { wireVal=0UL; str="__BREAK_DOWN_BUSSES__=1;"}
                    ]),
                    "// ===== Verishot Simulation File =====

// Specify how many cycles to simulate below
__CYCLES__=120;

// Specify (0 or 1) if you would like busses to be broken down
__BREAK_DOWN_BUSSES__=1;


// Specify each input on a new line (you may use Verilog style numeric constants)
foo=1;
bar=2;
car=1;".Replace("\r", "");


        "ranges are not matching",
            ([("foo", Single); ("bar", Range (3, 0)); ("car", Range(4, 4))], [],
                Map [
                    ("foo", Range(2, 0)), { wireVal=1UL; str="foo=1;"}
                    ("bar", Range(3, 2)), { wireVal=2UL; str="bar=2;"}
                    ("car", Single), { wireVal=1UL; str="car=1;" }
                    ("__CYCLES__", Range(1, 0)), { wireVal=120UL; str="__CYCLES__=120;"}
                    ("__BREAK_DOWN_BUSSES__", Range(2, 0)), { wireVal=0UL; str="__BREAK_DOWN_BUSSES__=1;"}
                    ]),
                "// ===== Verishot Simulation File =====

// Specify how many cycles to simulate below
__CYCLES__=10;

// Specify (0 or 1) if you would like busses to be broken down
__BREAK_DOWN_BUSSES__=0;


// Specify each input on a new line (you may use Verilog style numeric constants)
foo=;
bar[3:0]=;
car[4]=;".Replace("\r", "");

        "some outputs",
            ([("foo", Single); ("bar", Range (3, 0)); ("car", Range(4, 4))],
             [("foo_out", Single); ("bar_out", Range(4, 0)); (("car_out"), Single)], 
                Map [
                    ("foo", Single), { wireVal=1UL; str="foo=1;"}
                    ("bar", Range(3, 0)), { wireVal=2UL; str="bar=2;"}
                    ("car", Range(4, 4)), { wireVal=1UL; str="car=1;" }
                    ("__CYCLES__", Single), { wireVal=120UL; str="__CYCLES__=120;"}
                    ("__BREAK_DOWN_BUSSES__", Single), { wireVal=0UL; str="__BREAK_DOWN_BUSSES__=1;"}
                    ("foo_out", Single), { wireVal=1UL; str="foo_out=0;"}
                    ("bar_out", Single), { wireVal=1UL; str="bar_out=0;"}
                    ]),
                
                    "// ===== Verishot Simulation File =====

// Specify how many cycles to simulate below
__CYCLES__=120;

// Specify (0 or 1) if you would like busses to be broken down
__BREAK_DOWN_BUSSES__=1;

// Specify (0 or 1) whether to view these outputs in the waveform
foo_out=0;
bar_out[4:0]=1;
car_out=1;

// Specify each input on a new line (you may use Verilog style numeric constants)
foo=1;
bar=2;
car=1;".Replace("\r", "");
        
    ]

// this one needs to be a bit more special 
[<Tests>]
let getVinContentTestList =
    testList "getVinContent"
    <| (getVinContentTests
        |> List.map (processIntoAsyncTestList3 getVinContent))
