module full_adder4(Y, Co, A, B, Ci);
	
	output[3:0] Y;
	output Co;
	input[3:0] A, B;
	input Ci;
	
	wire[3:1] C;
	
	full_adder1 g0(Y[0], C[1], A[0], B[0], Ci);
	full_adder1 g1(Y[1], C[2], A[1], B[1], C[1]);
	full_adder1 g2(Y[2], C[3], A[2], B[2], C[2]);
	full_adder1 g3(Y[3], Co, A[3], B[3], C[3]);
	
endmodule 