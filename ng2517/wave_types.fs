namespace WaveTypes

open Verishot.SVG

type WaveformState = 
        //Used to keep track of a wave date and svg coordinates when producing waveform
        { prevVal: int; 
          svgVals: SVGElement}