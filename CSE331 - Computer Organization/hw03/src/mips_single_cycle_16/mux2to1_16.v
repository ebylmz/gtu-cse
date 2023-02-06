module mux2to1_16(
	output [15:0] result, 
	input sel, 
	input [15:0] in0, 
	input [15:0] in1);
	
	mux2to1 g15 (result[15], sel, in0[15], in1[15]);
	mux2to1 g14 (result[14], sel, in0[14], in1[14]);
	mux2to1 g13 (result[13], sel, in0[13], in1[13]);
	mux2to1 g12 (result[12], sel, in0[12], in1[12]);
	
	mux2to1 g11 (result[11], sel, in0[11], in1[11]);
	mux2to1 g10 (result[10], sel, in0[10], in1[10]);
	mux2to1 g9 (result[9], sel, in0[9], in1[9]);
	mux2to1 g8 (result[8], sel, in0[8], in1[8]);
	
	mux2to1 g7 (result[7], sel, in0[7], in1[7]);
	mux2to1 g6 (result[6], sel, in0[6], in1[6]);
	mux2to1 g5 (result[5], sel, in0[5], in1[5]);
	mux2to1 g4 (result[4], sel, in0[4], in1[4]);
	
	mux2to1 g3 (result[3], sel, in0[3], in1[3]);
	mux2to1 g2 (result[2], sel, in0[2], in1[2]);
	mux2to1 g1 (result[1], sel, in0[1], in1[1]);
	mux2to1 g0 (result[0], sel, in0[0], in1[0]);
	
endmodule