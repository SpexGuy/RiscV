// The RISC-V architecture has 32 registers, numbered 0 through 31.
// Register 0 is special.  Reading from register 0 always produces
// the value 0.  As a result, writing to register 0 has no effect
// on the state of the processor.  This is exploited in the CSR
// instructions, where writing to register 0 will sometimes avoid
// a trap that would fire when writing to other registers.

module regfile(
    input clk, // clk
    input rst_n, // active-low async reset

    input [4:0] rs1_id, // register index to read for rs1
    input [4:0] rs2_id, // register index to read for rs2
    output wire [31:0] rs1_data, // register data from rs1
    output wire [31:0] rs2_data, // register data from rs2

    // TODO: Since register 0 is fake, setting rd_id to 0
    // will prevent a register write.  If this causes problems
    // with the critical path, we could add an additional
    // write enable signal.
    input [4:0] rd_id, // register to write on the next clock edge
    input [31:0] rd_data // data to write on the next clock edge
    );

    // the register data
    reg [31:0] data[31:1];

    // no always_ff in icarus :(
    always @(posedge clk or negedge rst_n) begin
        if (!rst_n) begin
            for (int c = 1; c < 32; c++) begin
                data[c] <= 0;
            end
        end else begin
            if (rd_id != 0) begin
                data[rd_id] <= rd_data;
            end
        end
    end

    assign rs1_data = rs1_id == 0 ? 0 : data[rs1_id];
    assign rs2_data = rs2_id == 0 ? 0 : data[rs2_id];

endmodule
