module pc_next(
	output reg [9:0] next_addr,
	input [9:0] pc,
	input beq,
	input bne,
	input zero,
	input jump,
	input jumpr,
	input jumpal,
	input [9:0] branch_addr,
	input [9:0] jump_addr_reg, 
	input [9:0] jump_addr_imm);

	// j, jal, jr, beq
	
	always @ (*) begin
		if ((beq & zero) | (bne & ~zero))
			next_addr = pc + 10'b1 + branch_addr; 
		else if (jump)
			next_addr = jump_addr_imm;
		else if (jumpr)
			next_addr = jump_addr_reg;
		else if (jumpal)
			next_addr = jump_addr_reg;
			// set $ra to pc
		else
			next_addr = pc + 10'b1;
	end
	
endmodule