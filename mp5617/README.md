How will this code be used by team
==================================

The role of the `Verishot.Netlist` module is to generate a netlist from the AST
of a Verilog module. In this context a netlist (represented by
`Verishot.CoreTypes.Netlist.Netlist`) is a list of nodes representing module
instances (blocks) and pins along with connections between them, forming a
circuit.

This netlist will be used in 2 places 

1) LH's `Verishot.Visualize` module, which will turn this into a diagram
2) The planned `Verishot.Simulate` module, which will simulate netlists as
   circuits

Which parts if any are code written for other people?
=====================================================

Part of the `Verishot.Util` module, available for use by all teammates, is
written by me. Namely the `Verishot.Util.Map` and `Verishot.Util.List` modules
as well as the `rangeWidth` and `moveRangeToBase` functions. This module can be
found in `/libs/util.fs`

Which parts if any of code you use is written by others?
========================================================

None, aside for the shared types which were co-written by everyone.

What help have you obtained/given others debugging or doing code review?
========================================================================

LH helped with the `unifyConnections` function. There was a bug in the previous
implementation where if a wire was driving multiple outputs only the first one
would be in the output. LH had the idea of grouping the connections by source
and then merging them group-by-group.

I helped Olly with deciding on the right combinators for his parser. I guided
him towards using the `<&> : Parser<'a> -> ('a -> 'b) -> Parser<'b>` combinator.


How did you work out (who decided what - how do you revise) the types that interface your code with others?
===========================================================================================================

The types were initially drafted with the whole group before any of the
individual coding began. As the individual projects progressed we found ways in
which the types were either missing necessary information, allowed for
inconsistencies, or were inconvenient in certain ways.

For example, the `Netlist` type was initially a pair of lists, one of nodes and
one of lists. This turned out to be inconvenient for LH to generate diagrams
from, since he had to do multiple lookups into the list of connections. We
therefore switched to a type similar to the current one, with small changes
being added later on, e.g. constant nodes.
   
