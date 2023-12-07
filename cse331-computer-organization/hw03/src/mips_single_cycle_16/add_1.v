module add_1(
	output result, 
	output co, 
	input a, 
	input b, 
	input ci, 
	input sub);

	wire wr_b;
	xor g0(wr_b, b, sub);
	full_adder_1 g1(result, co, a, wr_b, ci);

endmodule 