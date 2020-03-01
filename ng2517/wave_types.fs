namespace WaveTypes

open Verishot.SVG

type WaveformState = 
        //Used to keep track of a wave val and svg coordinates when producing waveform
        { prevVal: int 
          svgVals: SVGElement}


type PortWaveform = //final result of the waveform, contains a block with all the waveforms for that port within in
  { waveBlock: SVGElement}

type SimulatorWire =  
    { portName: string
      output: int list}  // List of wire values at corresponding clock cycles

type SimulatorBus =
    { portName: string
      range: int
      output: int list} // List of bus values at corresponding clock cycles

type SimulatorPort = SimBus of SimulatorBus | SimWire of SimulatorWire //temporay output for simulator
