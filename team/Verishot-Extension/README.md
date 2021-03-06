# Verishot VSCode Extension README

This VSCode extension is made to accompany the `Verishot` Verilog Simulator and Visualiser. It is optional but highly recommended.

## Getting started

### Requirements
* `Verishot` CLI
* [VSCode](https://code.visualstudio.com/download)

*IMPORTANT*: Please ensure that a fully working `Verishot` CLI is installed. This extension *WILL NOT WORK* without working `Verishot` CLI.

To check, run `verishot` on any command line.
See `../Verishot/README.md` for a complete installation guide.

### Installing
A pre-packaged extension (`verishot-0.0.1.vsix`) is included for easy installation.

* Option 1: 
    * Windows: run `setup.bat`
    * Linux: run `setup.sh`
* Option 2: Double click the `verishot-0.0.1.vsix` file.
* Option 3: In this directory, run `code --install-extension verishot-0.0.1.vsix`.
* Option 4:
    * Select Extensions (Ctrl + Shift + X)
    * Open “More Actions...” menu (ellipsis on the top) and click “Install from VSIX…”
    * Locate VSIX file and select
    * Reload VSCode
### Debugging
* Make sure `node` is installed.
* Run `npm install` to install required modules.
* Press `F5` to launch an extension development host on VSCode.

### Testing
* Run `npm run test` to test. Tests are defined in `src/test`.
* To fix `"Running extension tests from the command line is currently only supported if no other instance of Code is running."` VSCode error, run `npm run test` in a command line with all VSCode instances closed. See [here](https://code.visualstudio.com/api/working-with-extensions/testing-extension).

## Features

### Verilog code support
Full support for VSCode *intellisense*, *highlighting*, and *linting* for the Verilog subset defined in `Verishot`.

### Project support
Create and manage `Verishot` Verilog projects and modules.

### Streamlined commands
Commands in command palette allow for *linting*, *simulation*, *visualisation* etc.

## Available Commands
You may access these via the VSCode Command Palette

### Verishot: New Project
Creates a new `Verishot` project.

### Verishot: New Module
Creates a new `Verishot` module in the current project.

### Verishot: Delete Module
Deletes an existing `Verishot` module in the current project.

### Verishot: Lint
Lints the current file.

### Verishot: Visualise
Outputs `.svg` visualisation for all modules defined in the project.

### Verishot: Simulate
Simulates the top-level module and outputs `.svg` waveform output. Inputs and cycles can be specified via a `.vin` file created by `Verishot`.

## Known Issues

N/A

## Release Notes

### 0.0.1

Initial release

