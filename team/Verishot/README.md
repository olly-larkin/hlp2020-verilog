# `Verishot` Verilog Visualiser and Simulator

(C) 2020 Imperial College London; lhl2617, ng2517, mp5617, oll16

This application provides a CLI for `Verishot`. It is designed to be accompanied by the `Verishot` VSCode extension in VSCode.

## Getting Started

### Pre-built Windows 64-bit binary
* A pre-built `win-x64` executable is located in [`.\bin\release\netcoreapp3.1\win-x64\Verishot.exe`](.\bin\release\netcoreapp3.1\win-x64\Verishot.exe).
* Add the folder containing the built `Verishot.exe` to your `PATH` variables to use the VSCode Extension

### Setup
Requirements:
* [.NET Core SDK](https://dotnet.microsoft.com/download)
#### Windows 
* Option 1: `cmd`: Run, _as administrator_, `setup.bat` in this directory
* Option 2: `Powershell`: Run, _as administrator_, `Setup-Verishot.ps1` in this directory

#### Linux
* Run `setup.sh` in this directory

### Mac OS X
* Not tested

### Debugging
* Run `dotnet run` in this directory.
* See [here](https://intranet.ee.ic.ac.uk/t.clarke/hlp/install-notes.html) on how to set up a F# ecosystem.

### Testing
* Run `dotnet run` in the `./test` directory.
* See [here](https://intranet.ee.ic.ac.uk/t.clarke/hlp/install-notes.html) on how to set up a F# ecosystem.

## Usage guide
Run `verishot --help` for list of available commands.


## VSCode Extension
It is recommended to develop `verishot` projects with VSCode and the supplied VSCode extension located in [`../Verishot-Extension`](../Verishot-Extension). See the corresponding `README.md` for installation and usage instructions.

<!-- TODO: ALL, feats -->