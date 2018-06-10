`timescale 1ns/1ps

module regfile_tb;

    logic clk, rst_n;
    // inputs
    logic [4:0] rs1_id;
    logic [4:0] rs2_id;
    logic [4:0] rd_id;
    logic [31:0] rd_data;
    //outputs
    wire [31:0] rs1_data;
    wire [31:0] rs2_data;

    reg [31:0] test_nums[4:0];

    regfile iDUT(clk, rst_n, rs1_id, rs2_id, rs1_data, rs2_data, rd_id, rd_data);

    always #5 clk = ~clk;

    reg fail;
    reg [31:0] gold_rs1;
    reg [31:0] gold_rs2;
    reg [31:0] errors;
    initial begin
        // Initialize variables
        clk = 0;
        rst_n = 0;
        rs1_id = 0;
        rs2_id = 0;
        rd_id = 0;
        rd_data = 0;
        fail = 0;

        test_nums[0] = 32'h00000000;
        test_nums[1] = 32'hFFFFFFFF;
        test_nums[2] = 32'hAAAAAAAA;
        test_nums[3] = 32'h55555555;
        test_nums[4] = 32'h00000000;

        // end reset signal
        @(posedge clk) rst_n = 1;
        //test
        @(negedge clk) ;

        // we're going to set all of the registers to each of the
        // values from test_nums, in sequence.
        // Outermost loop is which test num to do next.  Since reset
        // puts it all at 0, we'll start with the first index
        for (int d = 1; d < 5; d++) begin

            // Iterate through all of the registers, writing each one
            // with the new value.  This includes register 0, which
            // should not show the change.
            for (int c = 0; c < 32; c++) begin
                rd_id = c;
                rd_data = test_nums[d];

                // verify all registers on both ports before the clock happens
                // (this is to make sure that rd_id doesn't affect the output before the clock)
                errors = 32'h00000000;
                for (int e = 0; e < 32; e++) begin
                    rs1_id = e;
                    for (int f = 0; f < 32; f++) begin
                        rs2_id = f;
                        gold_rs1 = (rs1_id == 0) ? 0 :
                                   (e < c)       ? test_nums[d] :
                                                   test_nums[d-1];
                        gold_rs2 = (rs2_id == 0) ? 0 :
                                   (f < c)       ? test_nums[d] :
                                                   test_nums[d-1];
                        #0;
                        if (!errors[rs1_id] && rs1_data != gold_rs1) begin
                            errors[rs1_id] = 1;
                            fail = 1;
                            $display("REG Test Failed: expected %H from x%0d (port 1, before clk) but got %H", gold_rs1, rs1_id, rs1_data);
                        end
                        if (!errors[rs2_id] && rs2_data != gold_rs2) begin
                            errors[rs2_id] = 1;
                            fail = 1;
                            $display("REG Test Failed: expected %H from x%0d (port 2, before clk) but got %H", gold_rs2, rs2_id, rs2_data);
                        end
                    end
                end

                // process one clock
                @(posedge clk) ;
                @(negedge clk) ;

            end

            // verify all registers on both ports; they should all show the new value except x0
            errors = 32'h00000000;
            for (int e = 0; e < 32; e++) begin
                rs1_id = e;
                for (int f = 0; f < 32; f++) begin
                    rs2_id = f;
                    gold_rs1 = (rs1_id == 0) ? 0 : test_nums[d];
                    gold_rs2 = (rs2_id == 0) ? 0 : test_nums[d];
                    #0;
                    if (!errors[rs1_id] && rs1_data != gold_rs1) begin
                        errors[rs1_id] = 1;
                        fail = 1;
                        $display("REG Test Failed: expected %H from x%0d (port 1, after clk) but got %H", gold_rs1, rs1_id, rs1_data);
                    end
                    if (!errors[rs2_id] && rs2_data != gold_rs2) begin
                        errors[rs2_id] = 1;
                        fail = 1;
                        $display("REG Test Failed: expected %H from x%0d (port 2, after clk) but got %H", gold_rs2, rs2_id, rs2_data);
                    end
                end
            end
        end

        if (!fail) begin
            $display("REG Test: ALL PASS");
        end

        $finish ;
    end

endmodule