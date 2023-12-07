module datapath(
	output [15:0] write_reg_data,
	output [15:0] alu_result,
	output [15:0] alu_data_a, 
	output [15:0] alu_data_b,
	output [5:0] opcode,
	output [5:0] func,
	output reg [31:0] instr,
	input clock,
	input fast_clock,
	input reset,
	// main control signals
	input reg_dst,
	input beq,
	input bne,
	input jump,
	input jumpr,
	input jumpal,
	input loadi,
	input mem_read,
	input mem_to_reg,
	input mem_write,
	input alu_src,
	input reg_write,
	// alu control signals
	input [2:0] alu_ctr);

	/* IF */
	reg [9:0] pc = 10'h0;	
	
	wire [9:0] next_addr;
	wire [15:0] imm16;
	wire [9:0] address;
	wire zero;
	wire [15:0] bus_a, bus_b;
	wire [31:0] read_instr;
	
	pc_next _pc_next(
		.next_addr(next_addr),
		.pc(pc),
		.beq(beq),
		.bne(bne),
		.zero(zero),
		.jump(jump),
		.jumpr(jumpr),
		.jumpal(jumpal),
		.branch_addr(imm16[9:0]),
		.jump_addr_reg(bus_a[9:0]),
		.jump_addr_imm(address));
	

	always @ (posedge clock) begin
		if (~reset) begin
			pc = next_addr;
			instr = read_instr;
		end
		else begin
			pc = 10'h0;
			instr = 16'b0;
		end
	end
	
	
	instr_mem _instr_mem(
		.address(pc),
		.clock(clock), // changed from fast_clock
		.q(read_instr));
	
	
	/* ID */
	wire [3:0] rs, rt, rd, shamt;
	wire [3:0] write_reg;
	 	
	// R: [31 opcode 26] [25 rs 22] [21 rt 18] [17 rd 14] [13 shamt 10] [9 func 4]  [3 0000 0]
	
	// I: [31 opcode 26] [25 rs 22] [21 rt 18] [17 imm16 2]  [1 00 0]
	
	// J: [31 opcode 26] [25 address 16] [15 0000 0000 0000 0000 0]
	
	assign opcode = instr[31:26];
	assign func = instr[9:4];
	assign rs = instr[25:22];
	assign rt = instr[21:18];
	assign rd = instr[17:14];
	assign shamt = instr[13:10];
	assign imm16 = instr[17:2];
	assign address = instr[25:16];
	
	assign write_reg = reg_dst ? rd : rt; 
	
	register_file _register_file(
		.rs(bus_a),
		.rt(bus_b),
		.rs_addr(rs),
		.rt_addr(rt),
		.write_addr(write_reg),
		.write_data(write_reg_data),
		.reg_write(reg_write),
		.clock(clock),
		.fast_clock(fast_clock));
	
	/* EX */		
	assign alu_data_a = bus_a; 
	assign alu_data_b = alu_src ? imm16 : bus_b; 
	
	alu_16 _alu_16(
		.result(alu_result),
		.zero(zero),
		.clock(clock),
		.a(alu_data_a), 
		.b(alu_data_b),
		.alu_ctr(alu_ctr)); 
	
	/* MEM */
	wire [15:0] read_mem_data, mux_alu_mem;
	
	data_mem _data_mem(
		.address(alu_result),
		.clock(fast_clock),
		.data(bus_b), 
		.rden(mem_read),
		.wren(mem_write),
		.q(read_mem_data));
	
	/* WB */
	assign mux_alu_mem = mem_to_reg ? read_mem_data : alu_result; 
	assign write_reg_data = loadi ? imm16 : mux_alu_mem;
	
endmodule 