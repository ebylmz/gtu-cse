module mux8to1(Y, S, I0, I1, I2, I3, I4, I5, I6, I7);

	output Y;
	input [2:0] S;
	input I0, I1, I2, I3, I4, I5, I6, I7;
	
	wire L0, L1, L2, L3;
	wire R0, R1;
	
	// level 1
	mux2to1 g0(L0, S[0], I0, I1);
	mux2to1 g1(L1, S[0], I2, I3);
	mux2to1 g2(L2, S[0], I4, I5);
	mux2to1 g3(L3, S[0], I6, I7);
	
	// level 2
	mux2to1 g4(R0, S[1], L0, L1);
	mux2to1 g5(R1, S[1], L2, L3);
	
	// level 3
	mux2to1 g6(Y, S[2], R0, R1);
	
endmodule