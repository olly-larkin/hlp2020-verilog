namespace WaveTypes

open Verishot.SVG

type WaveformState = 
        //Used to keep track of a wave date and svg coordinates when producing waveform
        { prevVal: int 
          svgVals: SVGElement}


type PortWaveform =
  {
    waveBlock: SVGElement}

type SimulatorWire =
    { portName: string
      output: int list}

type SimulatorBus =
    { portName: string
      outputList: (int*(int list)) list}

type SimulatorPort = SimBus of SimulatorBus | SimWire of SimulatorWire