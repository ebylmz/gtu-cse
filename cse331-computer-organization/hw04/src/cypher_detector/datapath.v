module datapath(
	output detected,
	output matched,
	output protect_state,
	output [3:0] wr_cypher_block,
	output [7:0] sum,
	input [15:0] cypher,
	input [3:0] stream,
	input [1:0] current_state,
	input wri_reg_cypher,
	input wri_reg_sum,
	input clr_reg_sum,
	input clock);

	reg [15:0] reg_cypher;
	reg [7:0] reg_sum;

	wire [7:0] wr_adder; 
	wire [15:0] wr_shiftr;
	
	wire [3:0] wr_cypher_block1, wr_cypher_block2;
	
	initial begin 
		reg_cypher = 16'b0;
		reg_sum = 8'b0;
	end
	
	// MUX (4x1)
	assign wr_cypher_block1 = current_state[0] ? reg_cypher[7:4] : reg_cypher[3:0];
	assign wr_cypher_block2 = current_state[0] ? reg_cypher[15:12] : reg_cypher[11:8];
	assign wr_cypher_block = current_state[1] ? wr_cypher_block2: wr_cypher_block1;
	
	assign wr_adder = reg_sum + {4'b0, stream};
	
	assign sum = reg_sum;
	assign matched = stream == wr_cypher_block;
	assign detected = matched & (current_state == 2'b11);
	assign protect_state = (current_state == 2'b01) & (stream == reg_cypher[3:0]);
	
	// registers
	always @ (posedge clock) begin
		if (wri_reg_cypher) begin
			reg_cypher <= cypher;
		end

		if (wri_reg_sum) begin
			reg_sum <= wr_adder;
		end

		if (clr_reg_sum) begin
			reg_sum <= 8'b0;
		end
	end

endmodule 