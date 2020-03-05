The Waveform module written by me should be used as the final stage in the program
data flow to read and display the Verilog simulator output as a waveform.
The order of compilation for our program will likely be:
Lexer/Parser -> Netlist Generator -> Simulator -> Waveform Output
                                   └> Block Diagram Output

No code was written by me for other people

Credit to L.H. Lee for creating the SVG library used by me in this module, and for inspiration in testing structure/

Besides the test format and SVG library from L.H. the module was written without external help.

Due to the fact that the Simulator was pushed to the group stage it was agreed by the group that I could define suitable simulator output types and then interface with those to complete my individual work. It was taken into consideration when programming that these types could change at a future date and therefore I have kept the current ones simple so as to not create too much dependency on them.