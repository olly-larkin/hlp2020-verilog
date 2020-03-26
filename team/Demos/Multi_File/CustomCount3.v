module CustomCount3 (
    out
);

output[2:0] out;
wire[2:0] hold;

wire[2:0] next;

DFF dff(next, 1, hold);

assign next = hold + 1;
assign out = hold;

endmodule