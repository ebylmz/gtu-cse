module and_16(
	output [15:0] result, 
	input [15:0] a, 
	input [15:0] b);

	and g15 (result[15], a[15], b[15]);
	and g14 (result[14], a[14], b[14]);
	and g13 (result[13], a[13], b[13]);
	and g12 (result[12], a[12], b[12]);
	
	and g11 (result[11], a[11], b[11]);
	and g10 (result[10], a[10], b[10]);
	and g9 (result[9], a[9], b[9]);
	and g8 (result[8], a[8], b[8]);
	
	and g7 (result[7], a[7], b[7]);
	and g6 (result[6], a[6], b[6]);
	and g5 (result[5], a[5], b[5]);
	and g4 (result[4], a[4], b[4]);
	
	and g3 (result[3], a[3], b[3]);
	and g2 (result[2], a[2], b[2]);
	and g1 (result[1], a[1], b[1]);
	and g0 (result[0], a[0], b[0]);

endmodule 