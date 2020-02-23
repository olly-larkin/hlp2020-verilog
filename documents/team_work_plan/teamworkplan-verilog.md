Verishot team work plan
=======================

Group deliverable
-----------------

Our project will be to create a simulator for a subset of the Verilog Hardware
Description language along with visualisations for verilog modules and the
signals they generate.

This figure describes the rough architecture for our project, along with the
modules given to each team member. Each member's contributions will be detailed
in the following sections.

![Architecture diagram](diagram.jpg)

The AST and netlist types have already been defined in F# code along with the
function:
```fsharp
let moduleNetlist
    (moduleDecls: Map<Identifier, ModuleDecl>)
    (ast: VerilogAST.Module):
    Netlist
```
which generates a netlist from a verilog module AST. The first parameter is
necessary to connect modules correctly. We need to resolve module names to their
port lists.

During the team phase we plan to integrate our tool into Visual2 to give it a
GUI.

Olly Larkin: `Verishot.Parser`
------------------------------

Olly will be responsible for parsing text into the Verilog AST. Currently the
plan is to define the parser in a monadic style using a set of basic combinators
as well as F#'s computation expressions.

This approach has a few benefits:
1. Very readable parser code

   After defining the combinators and the parser builder, all parts of the
   parser should be able to defined using those blocks

2. Potential for very good error messages

   The basic combinators can create parser failures where they can display error
   messages about the expected and received tokens. This would also completely
   separate error handling from parser definitions.

Michail Pardalos: `Verishot.Simulator`
--------------------------------------

Michail's contribution will be using the netlist generated from the AST to run a
cycle-by-cycle simulation. This is essentially a function of type:
```fsharp
type SimState = Map<Register, RegisterValue>
let simulate (lastState: SimState) (netlist: Netlist): SimState
```
the resulting list should have one element for every cycle simulated. Every one
of those elements should have entries for every register in every module.

Naim Govani: `Verishot.Waveform`
--------------------------------

Naim will be using the output from Michail's simulator (`Map<Register,
RegisterValue> list`) to generate an SVG diagram of the register values at every
clock cycle.

LH Lee: `Verishot.Visualise`
----------------------------

LH will have to generate a block diagram for each defined module. The diagram
should contain instantiated modules, registers, wires as well as special blocks
for any built-in modules we decide to add.


