# lhl2617 - Individual Contributions

## Verishot-Extension
I wrote the VSCode extension designed to abstract away the CLI from the user. It operates with VSCode to allow a fully-integrated `Verishot` development environment. Users can use the VSCode command palette to easily simulate, visualise or manage their projects. Linting and intellisense are also available.

See `./team/Verishot-Extension/README.md` for complete features.

The code base is mainly (functional) TypeScript. 

Code is located in `./team/Verishot-Extension`. 

## Verishot Team Integration & CLI (Front End)
I connected all individual parts together and wrote a CLI, which is the entry point for the program as well as the VSCode extension.

Code is located in `./team/Verishot/src/FrontEnd`. 

## Verishot Visualiser
I wrote, as my individual part pre-group project, the Visualiser which outputs `svg` visualisations of modules. 

More information can be found at `./lhl2617/README.md`.

Code is located in `./team/Verishot/src/Visualiser`.

## SVG Library
The SVG library is used both by the `Verishot.Visualise` and `Verishot.Waveform` components (the latter is authored by `ng2517`). This SVG library is a generic easy-to-use and easy-to-extend library to output SVGs.

More information can be found at `./lhl2617/README.md`.

Code is located in `./team/Verishot/src/libs/svg.fs`.

