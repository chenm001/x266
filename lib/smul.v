module SMUL(a, b, c);
parameter WIDTH_A = 1;
parameter WIDTH_B = 1;

input [WIDTH_A-1:0] a;
input [WIDTH_B-1:0] b;
output [WIDTH_A+WIDTH_B-2:0] c;

    assign c = $signed(a) * $signed(b);
endmodule

