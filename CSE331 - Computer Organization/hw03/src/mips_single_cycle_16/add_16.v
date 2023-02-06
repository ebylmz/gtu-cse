module add_16(
	output[15:0] result, 
	output co, 
	input[15:0] a, 
	input[15:0] b, 
	input ci, 
	input sub);
	
	wire[2:0] carry;
	
	add_4 g0(result[3:0], carry[0], a[3:0], b[3:0], ci, sub);
	add_4 g1(result[7:4], carry[1], a[7:4], b[7:4], carry[0], sub);
	add_4 g2(result[11:8], carry[2], a[11:8], b[11:8], carry[1], sub);
	add_4 g3(result[15:12], co, a[15:12], b[15:12], carry[2], sub);
	
endmodule