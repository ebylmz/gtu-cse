module mips_single_cycle_16(
	output [15:0] write_reg_data,
	output [15:0] alu_data_a, 
	output [15:0] alu_data_b,
	output [31:0] instr,
	input clock,
	input fast_clock,
	input reset);

	wire reg_dst, beq, bne, jump, jumpr, jumpal, loadi, reg_write, alu_src, mem_read, mem_write, mem_to_reg;
	wire [2:0] alu_ctr;
	wire [5:0] opcode, func;
	
	control_unit _control_unit(
		.reg_dst(reg_dst),
		.beq(beq),
		.bne(bne),
		.jump(jump),
		.jumpr(jumpr),
		.jumpal(jumpal),
		.loadi(loadi),
		.reg_write(reg_write),
		.alu_ctr(alu_ctr),
		.alu_src(alu_src),
		.mem_read(mem_read),
		.mem_write(mem_write),
		.mem_to_reg(mem_to_reg),
		.opcode(opcode),
		.func(func));

	wire [15:0] alu_result;
	//	wire [15:0]  write_reg_data;
	// wire [31:0] instr;
	
	datapath _datapath(
		.write_reg_data(write_reg_data),
		.alu_result(alu_result),
		.alu_data_a(alu_data_a),
		.alu_data_b(alu_data_b),
		.opcode(opcode),
		.func(func),
		.instr(instr),
		.clock(clock),
		.fast_clock(fast_clock),
		.reset(reset),
		.reg_dst(reg_dst),
		.beq(beq),
		.bne(bne),
		.jump(jump),
		.jumpr(jumpr),
		.jumpal(jumpal),
		.loadi(loadi),
		.mem_read(mem_read),
		.mem_to_reg(mem_to_reg),
		.mem_write(mem_write),
		.alu_src(alu_src),
		.reg_write(reg_write),
		.alu_ctr(alu_ctr));

endmodule