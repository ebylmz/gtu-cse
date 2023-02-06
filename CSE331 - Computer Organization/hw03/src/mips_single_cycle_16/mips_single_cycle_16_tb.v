`timescale 1 ps / 1 ps

module mips_single_cycle_16_tb();

	reg clock = 1'b0, fast_clock = 1'b0, reset = 1'b0;
	wire [15:0] write_reg_data, alu_data_a, alu_data_b;
	wire [31:0] instr;
	
	mips_single_cycle_16 _mips_single_cycle_16(
		.write_reg_data(write_reg_data),
		.alu_data_a(alu_data_a),
		.alu_data_b(alu_data_b),
		.instr(instr),
		.clock(clock),
		.fast_clock(fast_clock),
		.reset(reset));
	
	always begin
		#6 clock = ~clock; // clock duration increased
	end
	
	always begin
		#1 fast_clock = ~fast_clock;
	end
	
	initial begin
		$monitor("time = %3d, Instr = %32b, alu_data_a = %16b, alu_data_b = %16b, write_reg_data = %16b", $time, instr, alu_data_a, alu_data_b, write_reg_data);
	end
		
endmodule