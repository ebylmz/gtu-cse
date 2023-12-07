`define DELAY 1250

module add32_tb();
	reg signed [31:0] A;
	reg signed [31:0] B;
	wire signed [31:0] result;
	reg enSub; // enables substraction and used also as carry in value
	wire Co;
	
	add32 t(result, Co, A, B, enSub, enSub);

	initial begin
		A = 32'b0000_0000_0000_0000_0000_0000_0000_1101; B = 32'b0000_0000_0000_0000_0000_0000_0000_1100; enSub = 1'b0;
		#`DELAY;
		
		A = 32'b0000_0000_0000_0000_0000_0000_0000_1101; B = 32'b0000_0000_0000_0000_0000_0000_0000_1100; enSub = 1'b0;
		#`DELAY;
		
		A = 32'b1111_1111_1111_1111_1111_1111_1111_0001; B = 32'b0000_0000_0000_0000_0000_0000_0000_1100; enSub = 1'b0;
		#`DELAY;
		
		A = 32'b0000_0000_0000_0000_0000_0000_0000_1101; B = 32'b0000_0000_0000_0000_0000_0000_0000_1100; enSub = 1'b1;
		#`DELAY;
		
		A = 32'b1111_1111_1111_1111_1111_1111_1111_1001; B = 32'b0000_0000_0000_0000_0000_0000_0000_1100; enSub = 1'b1;
		#`DELAY;
		
		A = 32'b0000_0000_0000_0000_0000_0000_0000_1101; B = 32'b1111_1111_1111_1111_1111_1111_1110_0001; enSub = 1'b1;
		#`DELAY $finish;
	end

	initial begin
		$monitor("time = %3d, A = %4d, B = %4d, enSub = %1b, carry-out = %1b, result = %4d", $time, A, B, enSub, Co, result);
	end
	
endmodule