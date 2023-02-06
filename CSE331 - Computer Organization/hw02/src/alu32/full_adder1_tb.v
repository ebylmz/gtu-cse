`define DELAY 20

module full_adder1_tb(); 
	
	reg A, B, Ci;
	wire Y, Co;

	
	full_adder1 t(Y, Co, A, B, Ci);
	
	initial begin
		A = 1'b0; B = 1'b0; Ci = 1'b0;
		#`DELAY;
		A = 1'b0; B = 1'b0; Ci = 1'b1;
		#`DELAY;
		A = 1'b0; B = 1'b1; Ci = 1'b0;
		#`DELAY;
		A = 1'b0; B = 1'b1; Ci = 1'b1;
		#`DELAY;
		A = 1'b1; B = 1'b0; Ci = 1'b0;
		#`DELAY;
		A = 1'b1; B = 1'b0; Ci = 1'b1;
		#`DELAY;
		A = 1'b1; B = 1'b1; Ci = 1'b0;
		#`DELAY;
		A = 1'b1; B = 1'b1; Ci = 1'b1;
		#`DELAY $finish;
	end
	 
	 
	initial begin
		$monitor("time = %2d, A = %1b, B = %1b, carry-in = %1b, sum = %1b, carry-out = %1b", $time, A, B, Ci, Y, Co);
	end
 
endmodule