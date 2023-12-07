module or_16(
	output [15:0] result, 
	input [15:0] a, 
	input [15:0] b);

	or g15 (result[15], a[15], b[15]);
	or g14 (result[14], a[14], b[14]);
	or g13 (result[13], a[13], b[13]);
	or g12 (result[12], a[12], b[12]);
	
	or g11 (result[11], a[11], b[11]);
	or g10 (result[10], a[10], b[10]);
	or g9 (result[9], a[9], b[9]);
	or g8 (result[8], a[8], b[8]);
	
	or g7 (result[7], a[7], b[7]);
	or g6 (result[6], a[6], b[6]);
	or g5 (result[5], a[5], b[5]);
	or g4 (result[4], a[4], b[4]);
	
	or g3 (result[3], a[3], b[3]);
	or g2 (result[2], a[2], b[2]);
	or g1 (result[1], a[1], b[1]);
	or g0 (result[0], a[0], b[0]);

endmodule 