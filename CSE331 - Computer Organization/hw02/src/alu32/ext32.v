module ext32(Y, A);
	
	output[31:0] Y;
	input A;
	
	or g31 (Y[31], A, 1'b0);
	or g30 (Y[30], A, 1'b0);
	or g29 (Y[29], A, 1'b0);
	or g28 (Y[28], A, 1'b0);
	
	or g27 (Y[27], A, 1'b0);
	or g26 (Y[26], A, 1'b0);
	or g25 (Y[25], A, 1'b0);
	or g24 (Y[24], A, 1'b0);
	
	or g23 (Y[23], A, 1'b0);
	or g22 (Y[22], A, 1'b0);
	or g21 (Y[21], A, 1'b0);
	or g20 (Y[20], A, 1'b0);
		
	or g19 (Y[19], A, 1'b0);
	or g18 (Y[18], A, 1'b0);
	or g17 (Y[17], A, 1'b0);
	or g16 (Y[16], A, 1'b0);
	
	or g15 (Y[15], A, 1'b0);
	or g14 (Y[14], A, 1'b0);
	or g13 (Y[13], A, 1'b0);
	or g12 (Y[12], A, 1'b0);
	
	or g11 (Y[11], A, 1'b0);
	or g10 (Y[10], A, 1'b0);
	or g9 (Y[9], A, 1'b0);
	or g8 (Y[8], A, 1'b0);
	
	or g7 (Y[7], A, 1'b0);
	or g6 (Y[6], A, 1'b0);
	or g5 (Y[5], A, 1'b0);
	or g4 (Y[4], A, 1'b0);
	
	or g3 (Y[3], A, 1'b0);
	or g2 (Y[2], A, 1'b0);
	or g1 (Y[1], A, 1'b0);
	or g0 (Y[0], A, 1'b0);
	
endmodule	