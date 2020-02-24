namespace WaveTypes

open Verishot.SVG

type WaveformState = 
        //Used to keep track of a wave date and svg coordinates when producing waveform
        { prevVal: int 
          svgVals: SVGElement}

type WireWaveform =
    { portName: SVGElement
      wave: SVGElement}

type BusWaveform =
    { portName: SVGElement
      waveList: (SVGElement*SVGElement) list}

type PortWaveform = BusWave of BusWaveform | WireWave of WireWaveform

type SimulatorWire =
    { portName: string
      output: int list}

type SimulatorBus =
    { portName: string
      outputList: (int*(int list)) list}

type SimulatorPort = SimBus of SimulatorBus | SimWire of SimulatorWire