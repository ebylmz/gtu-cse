`define CLOCK 4
`define DELAY 2500

module mult32_tb();

	reg [31:0] A, B;
	reg clk;
	wire [31:0] result, sum;
	
	
	
	mult32 t(result, sum, clk, A, B, 1'b1, 1'b0);
	
	initial begin
		clk = 1'b0;
		
		A = 32'b0000_0000_0000_0000_0000_0000_0000_0011; B = 32'b0000_0000_0000_0000_0000_0000_0000_0010;
		// A = 32'd5; B = 32'd3; 
		#`DELAY;
		
		// A = 32'b0000_0000_0000_0000_0000_1100_0000_0011; B = 32'b0000_0000_0000_0000_0000_0111_0000_0010;
		A = 32'd15; B = 32'd4; 
		#`DELAY;
		
		// A = 32'b0000_0000_0000_0000_0000_0100_0000_1010; B = 32'b0000_0000_0000_0000_0000_0100_0110_0010;
		A = 32'd25; B = 32'd5; 
		#`DELAY;
		$finish;
	end

		
	// define clock cycle
	always begin
		#4 clk = ~clk;
	end
		
	initial begin
		$monitor("time = %4d, A = %4d, B = %4d, result = %4d", $time, A, B, result);
	end

endmodule