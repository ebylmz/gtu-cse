module mux2to1_32(Y, S, I0, I1);

	output[31:0] Y;
	input S;
	input[31:0] I0, I1;
	
	mux2to1 g31 (Y[31], S, I0[31], I1[31]);
	mux2to1 g30 (Y[30], S, I0[30], I1[30]);
	mux2to1 g29 (Y[29], S, I0[29], I1[29]);
	mux2to1 g28 (Y[28], S, I0[28], I1[28]);
	
	mux2to1 g27 (Y[27], S, I0[27], I1[27]);
	mux2to1 g26 (Y[26], S, I0[26], I1[26]);
	mux2to1 g25 (Y[25], S, I0[25], I1[25]);
	mux2to1 g24 (Y[24], S, I0[24], I1[24]);
	
	mux2to1 g23 (Y[23], S, I0[23], I1[23]);
	mux2to1 g22 (Y[22], S, I0[22], I1[22]);
	mux2to1 g21 (Y[21], S, I0[21], I1[21]);
	mux2to1 g20 (Y[20], S, I0[20], I1[20]);
	
	mux2to1 g19 (Y[19], S, I0[19], I1[19]);
	mux2to1 g18 (Y[18], S, I0[18], I1[18]);
	mux2to1 g17 (Y[17], S, I0[17], I1[17]);
	mux2to1 g16 (Y[16], S, I0[16], I1[16]);
	
	mux2to1 g15 (Y[15], S, I0[15], I1[15]);
	mux2to1 g14 (Y[14], S, I0[14], I1[14]);
	mux2to1 g13 (Y[13], S, I0[13], I1[13]);
	mux2to1 g12 (Y[12], S, I0[12], I1[12]);
	
	mux2to1 g11 (Y[11], S, I0[11], I1[11]);
	mux2to1 g10 (Y[10], S, I0[10], I1[10]);
	mux2to1 g9 (Y[9], S, I0[9], I1[9]);
	mux2to1 g8 (Y[8], S, I0[8], I1[8]);
	
	mux2to1 g7 (Y[7], S, I0[7], I1[7]);
	mux2to1 g6 (Y[6], S, I0[6], I1[6]);
	mux2to1 g5 (Y[5], S, I0[5], I1[5]);
	mux2to1 g4 (Y[4], S, I0[4], I1[4]);
	
	mux2to1 g3 (Y[3], S, I0[3], I1[3]);
	mux2to1 g2 (Y[2], S, I0[2], I1[2]);
	mux2to1 g1 (Y[1], S, I0[1], I1[1]);
	mux2to1 g0 (Y[0], S, I0[0], I1[0]);
	
endmodule