module add4(Y, Co, A, B, Ci, sub);
	
	output[3:0] Y;
	output Co;
	input[3:0] A, B;
	input Ci, sub;
	
	wire[3:1] C;

	add1 g0(Y[0], C[1], A[0], B[0], Ci, sub);
	add1 g1(Y[1], C[2], A[1], B[1], C[1], sub);
	add1 g2(Y[2], C[3], A[2], B[2], C[2], sub);
	add1 g3(Y[3], Co, A[3], B[3], C[3], sub);
	
endmodule 