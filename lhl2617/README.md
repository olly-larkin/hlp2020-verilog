# Verishot.Visualise (lhl2617)

This folder contains code for the Verishot.Visualise module, which is used to visualise Verilog code (converted into a netlist) with a block diagram. The output is in SVG format.

## Details and examples
Example SVGs are located in the `SVGOutput` folder. These are produced by running `dotnet run` in this folder, and the netlist and declarations used to generate these SVGs are located in `main.fs`. 

The example output exercises all required features for supported netlists. There are also extra features, such as:
* Bus & wire visualisation support
* Tooltip for extra information on labels/text
* Label endpoint highlighting upon hovering to improve readability 
* Linking between module instances to view details of declaration (click on a supported module to go to its individual SVG)

## Individual Statement 
* Code will be inserted into Team part after `Verishot.AST` and `Verishot.Netlist` modules, as this module requires the output from the netlist which follows output  of the AST module.
* I wrote the SVG library (`Verishot.SVG`) located in `../libs/svg.fs`. This is intended to be a easy-to-use and easy-to-extend generic SVG library. The library currently contains SVG elements required by this module and also for another teammate's module (`Verishot.Waveform`), but can be easily extended to contain more SVG elements. There is also support for CSS stylesheets and JavaScript scripts.
* This module heavily relies on core types (located in `../libs/core_types.fs`) co-developed by the team. We discussed on the interfacing types before and during work to make sure the features supported are adequate.

## Tests
Tests are located in `test` and can be run using `dotnet run` in the `test` directory. There are a combination of pre-defined `Expecto` tests as well as generated `FsCheck` property-based tests. 

`test/visualiser/util_test.fs` demonstrates varied testing methodology, including use of arbitrary modifiers to generate custom property-based tests. 

The `Expecto` testbench is designed to be easily-extendable, and the definition of a `Expecto` test list is simply a list of `(name, input, expected output)` tuples. The test lists are then passed on to test generators to create `async` tests.