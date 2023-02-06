`timescale 1 ps / 1 ps

module instr_mem_tb();

	reg [9:0] pc = 10'b0;
	reg clock = 1'b0;
	wire [31:0] instr;
	
	instr_mem _instr_mem(
		.address(pc),
		.clock(clock),
		.q(instr));
	
	always begin
		#1 clock = ~clock;
	end
		
	initial begin
		#2 pc = pc + 1'b1;
		#2 pc = pc + 1'b1;
		#2 pc = pc + 1'b1;
		#2 pc = pc + 1'b1;
		#2 pc = pc + 1'b1;
		#2 pc = pc + 1'b1;
		#2 pc = pc + 1'b1;
		#2 pc = pc + 1'b1;
	end
	
	initial begin
		$monitor("time = %3d, PC: %10b, Instr: %32b", $time, pc, instr);
	end
	
endmodule