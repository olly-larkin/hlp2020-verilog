module alu_decoder (
    in1,
    in2,
    result
);

input[7:0] in1;
input[7:0] in2;
output[7:0] result;

wire[7:0] mult;
wire[7:0] bitwiseOr;
wire[7:0] bitwiseAnd;
wire[7:0] logicalAnd;

wire[1:0] select;
wire[63:0] countVal;

Counter64 count(0, countVal);
Mux4 mux(select, mult, bitwiseOr, bitwiseAnd, logicalAnd, result);

assign select = countVal % 4;

assign mult = in1 * in2;
assign bitwiseOr = in1 | in2;
assign bitwiseAnd = in1 & in2;
assign logicalAnd = in1 && in2;

endmodule