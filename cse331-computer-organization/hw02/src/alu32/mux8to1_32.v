module mux8to1_32(Y, S, I0, I1, I2, I3, I4, I5, I6, I7);

	output[31:0] Y;
	input [2:0] S;
	input[31:0] I0, I1, I2, I3, I4, I5, I6, I7;
	
	wire[31:0] L0, L1, L2, L3;
	wire[31:0] R0, R1;
	
	// level 1
	mux2to1_32 g0(L0, S[0], I0, I1);
	mux2to1_32 g1(L1, S[0], I2, I3);
	mux2to1_32 g2(L2, S[0], I4, I5);
	mux2to1_32 g3(L3, S[0], I6, I7);
	
	// level 2
	mux2to1_32 g4(R0, S[1], L0, L1);
	mux2to1_32 g5(R1, S[1], L2, L3);
	
	// level 3
	mux2to1_32 g6(Y, S[2], R0, R1);
	
endmodule