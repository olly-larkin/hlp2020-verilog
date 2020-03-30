# lhl2617

## Main Contributions to Project

### Verishot Team Integration & CLI (Front End)
I connected all individual parts together and wrote a CLI, which is the entry point for the program as well as the VSCode extension.

The simulation front end uses `oll16`'s parsing library to parse inputs.

Code is located in [`./team/Verishot/src/FrontEnd`](./team/Verishot/src/FrontEnd). 

### Verishot-Extension
I wrote the VSCode extension designed to abstract away the CLI from the user. It operates with VSCode to allow a fully-integrated `Verishot` development environment. Users can use the VSCode command palette to easily simulate, visualise or manage their projects. Code highlighting, linting and intellisense are also available.

See [`./team/Verishot-Extension/README.md`](./team/Verishot-Extension/README.md) for complete features.

The code base is mainly (functional) TypeScript. 

Code is located in [`./team/Verishot-Extension`](./team/Verishot-Extension). 

### Verishot Visualiser
I wrote, as my individual part pre-group project, the Visualiser which outputs `svg` visualisations of modules. 

More information can be found at [`./lhl2617/README.md`](./lhl2617/README.md).

Code is located in [`./team/Verishot/src/Visualiser`](./team/Verishot/src/Visualiser).

### SVG Library
The SVG library is used both by the `Verishot.Visualise` and `Verishot.Waveform` components (the latter is authored by `ng2517`). This SVG library is a generic easy-to-use and easy-to-extend library to output SVGs.

More information can be found at [`./lhl2617/README.md`](./lhl2617/README.md).

Code is located in [`./team/Verishot/src/libs/svg.fs`](./team/Verishot/src/libs/svg.fs).

## Team Contributions
### Contributions from others
* `oll16`: Parsing library to get inputs for simulation
* Team: Core types and libraries

### Contributions to others
* `oll16`'s and `ng2517`'s testing infrastructure
* Team: Tidying up APIs and connecting everyone's work together.