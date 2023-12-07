module half_adder_1(
	output result, 
	output c, 
	input a, 
	input b);
	
	xor g0(result, a, b);
	and g1(c, a, b);
	
endmodule 