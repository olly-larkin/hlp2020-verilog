module Verishot.WaveformStyles

let loadStyles (unitPx: float) =
    sprintf ":root {
        --unitPx: %.1fpx
    }
    * {
        font-family: Consolas, 'Courier New', monospace;
        background-color: white;
    }
    text {
        stroke: none;
        fill: black;   
    }" unitPx