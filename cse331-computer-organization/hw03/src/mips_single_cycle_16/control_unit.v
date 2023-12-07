module control_unit(
	output reg reg_dst,
	output reg beq,
	output reg bne,
	output reg jump,
	output reg jumpr,
	output reg jumpal,
	output reg loadi,
	output reg reg_write,
	output reg [2:0] alu_ctr,
	output reg alu_src,
	output reg mem_read,
	output reg mem_write,
	output reg mem_to_reg,
	input [5:0] opcode,
	input [5:0] func);
	
	localparam 	ADD = 3'b000, 
					SUB = 3'b001, 
					MULT = 3'b010, 
					AND = 3'b011, 
					OR = 3'b100, 
					SLT = 3'b101, 
					SRL = 3'b110, 
					SLL = 3'b111;
	
	wire wr_r_type, wr_i_type;
	
	// R-type instructions
	wire wr_add, wr_sub, wr_mult, wr_slt, wr_and, wr_or, wr_sll, wr_srl, wr_jr; 
	
	// I-type instructions
	wire wr_addi, wr_andi, wr_ori, wr_slti, wr_sw, wr_lw, wr_li, wr_beq, wr_bne;
	
	// J-type instructions
	wire wr_j, wr_jal;
		
	assign wr_r_type = (opcode == 6'h00);
	
	assign wr_lw = (opcode == 6'h23);
	assign wr_sw = (opcode == 6'h2b);
	
	assign wr_sub = (wr_r_type & func == 6'h22);
	
	assign wr_mult = (wr_r_type  & func == 6'h18);
	
	assign wr_jr = (wr_r_type  & func == 6'h08);
	assign wr_j = (opcode == 6'h02);
	assign wr_jal = (opcode == 6'h03);
	
	assign wr_add = (wr_r_type  & func == 6'h20);
	assign wr_addi = (opcode == 6'h08);
	
	assign wr_and = (wr_r_type  & func == 6'h24);
	assign wr_andi = (opcode == 6'h0c);
	
	assign wr_sll = (wr_r_type  & func == 6'h00);
	assign wr_srl = (wr_r_type  & func == 6'h02);
	
	assign wr_or = (wr_r_type  & func == 6'h25);
	assign wr_ori = (opcode == 6'h0d);
	
	assign wr_slt = (wr_r_type  & func == 6'h2a);
	assign wr_slti = (opcode == 6'h0a);
	
	assign wr_beq = (opcode == 6'h04);
	assign wr_bne = (opcode == 6'h05);
	
	assign wr_li = (opcode == 6'h07);
	
	assign wr_i_type = wr_addi | wr_andi | wr_ori | wr_slti | wr_sw | wr_lw | wr_beq | wr_bne | wr_li;
	
	always @ (*) begin
		reg_dst = wr_r_type; 
		beq = wr_beq;
		bne = wr_bne;
		mem_read = wr_lw;
		mem_to_reg = wr_lw; 
		mem_write = wr_sw;
		alu_src = wr_i_type & ~(wr_beq | wr_bne); // added beq and bne
		reg_write = (wr_r_type & ~wr_jr) | (wr_i_type & ~(wr_beq | wr_bne)); // beq and bne added
		jump = wr_j;
		jumpr = wr_jr;
		jumpal = wr_jal;
		loadi = wr_li;
		
		
		if (wr_add | wr_addi | wr_lw | wr_sw)
			alu_ctr = ADD;
		else if (wr_sub | wr_bne | wr_beq)
			alu_ctr = SUB;
		else if (wr_mult)
			alu_ctr = MULT;
		else if (wr_and | wr_andi)
			alu_ctr = AND;
		else if (wr_or | wr_ori)
			alu_ctr = OR;
		else if (wr_slt | wr_slti)
			alu_ctr = SLT;
		else if (wr_srl)
			alu_ctr = SRL;
		else if (wr_sll)
			alu_ctr = SLL;
		else
			alu_ctr = ADD; // j, jr, li, jal
	end
	

endmodule