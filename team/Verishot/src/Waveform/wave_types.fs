namespace WaveTypes

open Verishot.SVG
open Verishot.Simulator.Types

type WaveformState =
    //Used to keep track of a wave val and svg coordinates when producing waveform
    { prevVal: WireVal
      svgVals: SVGElement }


type SimulatorWire =
    { portName: string
      output: WireVal list } // List of wire values at corresponding clock cycles

type SimulatorBus =
    { portName: string
      range: int
      output: WireVal list } // List of bus values at corresponding clock cycles

type SimulatorPort =
    | SimBus of SimulatorBus
    | SimWire of SimulatorWire //temporay output for simulator

type WaveformProps = 
    { breakDownBusses: bool }