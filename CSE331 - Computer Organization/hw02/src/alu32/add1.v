module add1(Y, Co, A, B, Ci, sub);
	
	output Y, Co;
	input A, B, Ci, sub;

	wire wB;
	xor g0(wB, B, sub);
	full_adder1 g1(Y, Co, A, wB, Ci);

endmodule 