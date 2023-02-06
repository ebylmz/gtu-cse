`define DELAY 20

module xor32_tb();

	reg[31:0] A, B; 	// inputs are declared as register
	wire[31:0] Y;  	// outputs are declared as wire

	xor32 t(Y, A, B);
		
	initial begin
		A = 32'b1001_0000_0000_0000_0000_0000_0000_1010; B = 32'b0001_0000_0000_0000_0000_0000_0001_1110;
		#`DELAY;
		A = 32'b1111_0000_0000_0000_0000_1111_0000_1111; B = 32'b0001_0000_1111_0000_0110_0000_0001_1110;
		#`DELAY $finish;
	end
	  
	initial begin
	$monitor("time = %2d, A = %32b, B = %32b, result = %32b", $time, A, B, Y);
	end

endmodule 