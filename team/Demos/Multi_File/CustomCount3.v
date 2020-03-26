module CustomCount3 (
    out
);

output[2:0] out;

wire[2:0] next;

DFF dff(next, out);

assign next = out + 1;

endmodule