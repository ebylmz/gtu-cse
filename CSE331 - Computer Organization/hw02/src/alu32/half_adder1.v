module half_adder1(Y, C, A, B);
	
	output Y, C;
	input A, B;
	
	xor g0(Y, A, B);
	and g1(C, A, B);
	
endmodule 