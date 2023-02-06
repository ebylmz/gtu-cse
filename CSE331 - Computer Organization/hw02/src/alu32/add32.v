module add32(Y, Co, A, B, Ci, sub);
	
	output[31:0] Y;
	output Co;
	input[31:0] A, B;
	input Ci, sub; 

	wire[7:1] C;
	
	add4 g0(Y[3:0], C[1], A[3:0], B[3:0], Ci, sub);
	add4 g1(Y[7:4], C[2], A[7:4], B[7:4], C[1], sub);
	add4 g2(Y[11:8], C[3], A[11:8], B[11:8], C[2], sub);
	add4 g3(Y[15:12], C[4], A[15:12], B[15:12], C[3], sub);
	add4 g4(Y[19:16], C[5], A[19:16], B[19:16], C[4], sub);
	add4 g5(Y[23:20], C[6], A[23:20], B[23:20], C[5], sub);
	add4 g6(Y[27:24], C[7], A[27:24], B[27:24], C[6], sub);
	add4 g7(Y[31:28], Co, A[31:28], B[31:28], C[7], sub);
	
endmodule