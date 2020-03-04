# `Verishot.Visualise` (lhl2617)

This folder contains code for the `Verishot.Visualise` module, which is used to visualise Verilog code (converted into a netlist) with a block diagram. The output is in SVG format.

## Details and examples
Example SVGs are located in the `SVGOutput` folder (best viewed using `Google Chrome`). These are produced by running `dotnet run` in this folder, and the netlist and declarations used to generate these SVGs are located in `src/main.fs`. 

The example output exercises all required features for supported netlists, which include:
* I/O pins
* User-defined modules
* Built-in modules
* Wires & Busses
* Constants

There are also extra features, such as:
* Bus & wire visualisation support (bit widths are clearly demarcated)
* Tooltips for extra information on labels/text
* Label endpoint highlighting upon hovering to improve readability 
* Linking between module instances to view details of declaration (click on a supported module to go to its individual SVG)

Known limitations:
* Complex modules may take up excessive horizontal space (work required to integrate smart block placement)
* Overlapping of bus-width labels when busses are concatenated into a common bus input (does not occur in code a lot, but hover-highlighting is a workaround to clearly visualise. Quartus has the same problem and does not have a workaround.)

## Individual Statement 
* Code will be inserted into Team part after `Verishot.AST` and `Verishot.Netlist` modules, as this module requires the output from the netlist which follows output of the AST module.
* The main API required for this module is the `visualiseNetlists` function defined in `src/visualiser/visualise.fs`. It requires the project name, all netlists, all user-defined module declarations, an optional CSS styles and an optional JavaScript script.
* I wrote the SVG library (`Verishot.SVG`) located in `../libs/svg.fs`. More information is provided below. 
* This module heavily relies on core types (located in `../libs/core_types.fs`) co-developed by the team. We discussed on the interfacing types before and during work to make sure the features supported are adequate.

## SVG Library
The SVG library is intended to be a easy-to-use and easy-to-extend generic SVG library. The library currently contains SVG elements required by this module and also for another teammate's module (`Verishot.Waveform`), but can be easily extended to contain more SVG elements. It is _not_ specific to this project and can be used in any F# project requiring SVG visualisation.

Current features include:
* Support for SVG Elements: `circle`, `polyline`, `rect`, `text`, `group`, `link`
* Fully-customisable SVG attributes for each element (via setting `props`)
* `CSS` and `JavaScript` support
* Dynamic viewbox sizing based on dimensions of SVG Elements
* User-customisable units to alter pixels per unit (`unitPx`)
* Optional grids and borders

Known limitations:
* Dynamic sizing might fail on `text` amongst elements that are styled via CSS post-generation, as dynamic viewbox sizing does not factor in CSS styles. This may cause some elements from being generated outside the border.

## Tests
Tests are located in `test` and can be run using `dotnet run` in the `test` directory. There are a combination of pre-defined `Expecto` tests as well as `FsCheck` property-based tests.

Tests are written for both individual code and the SVG library module shared with the team.

`test/visualiser/util_test.fs` demonstrates varied testing methodology, including use of arbitrary modifiers to generate custom property-based tests. 

The `Expecto` testbench is designed to be easily-extendable, and the definition of a `Expecto` test list is simply a list of `(name, input, expected_output)` tuples. The test lists are then passed on to test generators to create `async` tests.

## Future work
* Integration of heuristics for smart block placement
* Allow user-draggable elements and block-diagram input as an alternative to Verilog code