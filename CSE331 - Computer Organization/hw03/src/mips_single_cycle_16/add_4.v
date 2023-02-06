module add_4(
	output[3:0] result, 
	output co, 
	input[3:0] a, 
	input[3:0] b, 
	input ci, 
	input sub);
	
	wire[3:1] carry;

	add_1 g0(result[0], carry[1], a[0], b[0], ci, sub);
	add_1 g1(result[1], carry[2], a[1], b[1], carry[1], sub);
	add_1 g2(result[2], carry[3], a[2], b[2], carry[2], sub);
	add_1 g3(result[3], co, a[3], b[3], carry[3], sub);
	
endmodule 