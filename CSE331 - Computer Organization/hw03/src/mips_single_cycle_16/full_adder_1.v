module full_adder_1(
	output result, 
	output co, 
	input a, 
	input b, 
	input ci);
	
	wire s0, c0, c1;
	
	half_adder_1 g0(s0, c0, a, b);
	half_adder_1 g1(result, c1, s0, ci);
	or g2(co, c0, c1);
	
endmodule 