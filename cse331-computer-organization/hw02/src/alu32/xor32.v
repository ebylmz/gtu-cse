module xor32(Y, A, B);

	output[31:0] Y;
	input[31:0] A, B;
	
	xor g31 (Y[31], A[31], B[31]);
	xor g30 (Y[30], A[30], B[30]);
	xor g29 (Y[29], A[29], B[29]);
	xor g28 (Y[28], A[28], B[28]);
	
	xor g27 (Y[27], A[27], B[27]);
	xor g26 (Y[26], A[26], B[26]);
	xor g25 (Y[25], A[25], B[25]);
	xor g24 (Y[24], A[24], B[24]);
	
	xor g23 (Y[23], A[23], B[23]);
	xor g22 (Y[22], A[22], B[22]);
	xor g21 (Y[21], A[21], B[21]);
	xor g20 (Y[20], A[20], B[20]);
	
	xor g19 (Y[19], A[19], B[19]);
	xor g18 (Y[18], A[18], B[18]);
	xor g17 (Y[17], A[17], B[17]);
	xor g16 (Y[16], A[16], B[16]);
	
	xor g15 (Y[15], A[15], B[15]);
	xor g14 (Y[14], A[14], B[14]);
	xor g13 (Y[13], A[13], B[13]);
	xor g12 (Y[12], A[12], B[12]);
	
	xor g11 (Y[11], A[11], B[11]);
	xor g10 (Y[10], A[10], B[10]);
	xor g9 (Y[9], A[9], B[9]);
	xor g8 (Y[8], A[8], B[8]);
	
	xor g7 (Y[7], A[7], B[7]);
	xor g6 (Y[6], A[6], B[6]);
	xor g5 (Y[5], A[5], B[5]);
	xor g4 (Y[4], A[4], B[4]);
	
	xor g3 (Y[3], A[3], B[3]);
	xor g2 (Y[2], A[2], B[2]);
	xor g1 (Y[1], A[1], B[1]);
	xor g0 (Y[0], A[0], B[0]);

endmodule 