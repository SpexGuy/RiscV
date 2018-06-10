
// ADD:  0, ~rev
//   adder_b = b
//   result = a + adder_b + rev
// SUB:  0, rev
//   adder_b = ~b
//   result = a + adder_b + rev
// SLL:  1
//   result = a << b[4:0]
// SLT:  2
//   result = (signed_a < signed_b) ? 1 : 0
// SLTU: 3
//   result = (a < b) ? 1 : 0
// XOR:  4
//   result = a ^ b
// SRL:  5, ~rev
//   result = signed_a >> b[4:0]
// SRA:  5, rev
//   result = signed_a >>> b[4:0]
// OR:   6
//   result = a | b
// AND:  7
//   result = a & b

module alu(
    input [31:0] a, // operand A
    input [31:0] b, // operand B
    input [2:0] op, // opcode, from funct3 field
    input rev,      // turns add into subtract, and logical shift into arithmetic

    output logic [31:0] result
    );

    // the bits of a, interpreted signed
    logic signed [31:0] signed_a;
    // the bits of b, interpreted signed
    logic signed [31:0] signed_b;
    // the second input to the add circuit
    logic signed [31:0] adder_b;
    // the output of the add circuit
    logic signed [31:0] sum;
    // the result of the shift right, modified by rev.
    logic signed [31:0] shift_right;

    assign signed_a = a;
    assign signed_b = b;

    assign adder_b = {32{rev}} ^ signed_b;

    assign sum = signed_a + adder_b + rev;

    assign shift_right = rev ? (signed_a >>> b[4:0]) : (signed_a >> b[4:0]);

    // icarus doesn't support always_comb :(
    always @(*) begin
        result = 32'hXXXXXXXX;
        case (op)
        3'b000: result = sum;
        3'b001: result = a << b[4:0];
        3'b010: result = (signed_a < signed_b) ? 1 : 0;
        3'b011: result = (a < b) ? 1 : 0;
        3'b100: result = a ^ b;
        3'b101: result = shift_right;
        3'b110: result = a | b;
        3'b111: result = a & b;
        endcase
    end

endmodule
