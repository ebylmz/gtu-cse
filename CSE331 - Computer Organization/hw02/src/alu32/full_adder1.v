module full_adder1(Y, Co, A, B, Ci);
	
	output Y, Co;
	input A, B, Ci;
	
	wire s0, c0, c1;
	
	half_adder1 g0(s0, c0, A, B);
	half_adder1 g1(Y, c1, s0, Ci);
	or g2(Co, c0, c1);
	
endmodule 