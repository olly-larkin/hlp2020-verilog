module Verishot.VisualiseStyles

let loadStyles (unitPx: float) =
    sprintf ":root {
        --unitPx: %.1fpx
    }
    * {
        font-family: Consolas, 'Courier New', monospace;
        background-color: white;
    }
    .node-bord {
        fill: none;
        /* stroke: pink; */
    }
    .node-actual {
        fill: #f7f1e3;
        stroke: black;
    }
    .node-builtin-bord {
        fill: none;
        /* stroke: pink; */
    }
    .node-builtin-actual {
        fill: #e8e9ff;
        rx: 5;
        stroke: black;
    }
    .inputPin-actual {
        fill: #f7f1e3;
        stroke: black; 
    }
    .node-actual:hover {
        fill: #fff3f3;
        stroke: red;
    }
    .node-mod-title {
        font-size: var(--unitPx);
        font-weight: 800;
        dominant-baseline: middle;
        text-anchor: middle;
    }
    .node-mod-leftPortText,
    .node-mod-rightPortText {
        font-size: calc(var(--unitPx) * 0.8);
        dominant-baseline: middle;
    }
    .node-mod-leftPortText {
        text-anchor: start;
    }
    .node-mod-rightPortText {
        text-anchor: end;
    }

    .wire-line {
        stroke-width: calc(var(--unitPx) / 12);
        fill: none;
    }
    .wire-blob {
        stroke: none;
    }
    .label-group {
        font-size: calc(var(--unitPx) * 0.8);
        font-style: italic;
        stroke: black;
        fill: black;
    }
    .label-group:hover {
        stroke: red;
        fill: red;
    }
    .label-output-text,
    .label-input-text {
        dominant-baseline: middle;
        stroke: none;
    }
    .label-output-text {
        text-anchor: start;  
    }
    .label-input-text {
        text-anchor: end;
    }
    .label-x {
        stroke-width: calc(var(--unitPx) / 10);
    }
    .label-line-single {
        stroke-width: calc(var(--unitPx) / 12);
        fill: none;
    }
    .label-line-range {
        stroke-width: calc(var(--unitPx) / 6);
        fill: none;
    }
    .pin-text-input,
    .pin-text-output {
        font-size: calc(var(--unitPx) * 0.8);
        dominant-baseline: middle;
    }
    .pin-text-input {
        text-anchor: start;
    }
    .pin-text-output {
        text-anchor: end;
    }
    .pin-input-arrow,
    .pin-output-arrow {
        fill: wheat;
        stroke: black;
    }

    .output-var-text,
    .input-var-text-source,
    .input-var-text-target {
        stroke: none;
        font-size: calc(var(--unitPx) * 0.5);
        font-style: normal;
    }
    .input-var-text-target {
        text-anchor: end;
    }
    .output-var-text,
    .input-var-text-source {
        text-anchor: start;
    }
    .const {
        font-size: calc(var(--unitPx) * 0.8);
        font-style: italic;
        stroke: black;
        fill: black;
    }
    .const-text {
        text-anchor: end;
        stroke: none;
    }" unitPx