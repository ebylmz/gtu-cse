module alu_16_tb();

	reg [15:0] a, b;
	reg clock = 1'b0;
	reg [2:0]alu_ctr;
	wire [15:0] result;

	alu_16 _alu_16(
		.result(result),
		.clock(clock),
		.a(a), 
		.b(b), 
		.alu_ctr(alu_ctr));

	always begin
		#2 clock = ~clock;
	end

	initial begin
		// add
		alu_ctr = 3'b000; a = 16'b0000_0000_0000_1101; b = 16'b0000_0000_0101_1100;
		
		// sub
		#20 alu_ctr = 3'b001; a = 16'b0000_0000_0010_1101; b = 16'b0000_0000_1010_1111;
		
		// mult
		#20 alu_ctr = 3'b010; a = 16'b0000_0000_0000_1111; b = 16'b0000_0000_0000_0101;
		
		// and
		#150 alu_ctr = 3'b011; a = 16'b1111_0000_1111_1101; b = 16'b1111_0000_1111_1100;
	
		// or
		#20 alu_ctr = 3'b100; a = 16'b1111_0000_0000_1101; b = 16'b0011_0000_0000_1100;
		
		// slt
		#20 alu_ctr = 3'b101; a = 16'b0000_0000_0000_1101; b = 16'b0000_0000_0000_1100;
		
		// srl
		#20 alu_ctr = 3'b110; a = 16'b0000_0000_1111_1111; b = 16'b0000_0000_0000_0010;
		
		// sll
		#20 alu_ctr = 3'b111; a = 16'b0000_0000_0000_1111; b = 16'b0000_0000_0000_0101; 

	end
			
	initial begin
		$monitor("time = %5d, alu_ctr = %3b, a = %32b, b = %32b, result = %32b", $time, alu_ctr, a, b, result);
	end
	
	

endmodule