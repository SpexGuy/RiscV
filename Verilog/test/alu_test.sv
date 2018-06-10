`timescale 1ns/1ps
`define NUM_TESTS 320
`define MAX_WORD 1599

module alu_test;

    reg [31:0] a;
    reg [31:0] b;
    reg [3:0] op;
    reg rev;

    wire [31:0] result;

    reg [31:0] tests [1599:0];

    alu dut(
        .a(a),
        .b(b),
        .op(op[2:0]),
        .rev(rev),
        .result(result)
    );

    reg fail;
    initial begin
        fail = 0;
        $readmemh("test_data/alu_tests.h", tests, 0, 1599);
        for (int c = 0; c <= 1599; c += 5) begin
            a = tests[c+0];
            b = tests[c+1];
            op = tests[c+2][2:0];
            rev = tests[c+3][0];

            #100 ;
            if (result != tests[c+4]) begin
                fail = 1;
                $display("ALU Test Failed: a=%0H b=%0H op=%0H rev=%0H expected=%0H actual=%0H", a, b, op, rev, tests[c+4], result);
            end
        end

        if (!fail) begin
            $display("ALU Test: ALL PASS");
        end

        $finish ;
    end

endmodule
