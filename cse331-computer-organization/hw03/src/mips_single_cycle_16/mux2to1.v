module mux2to1(
	output result, 
	input sel, 
	input in0, 
	input in1);

	wire r0, r1, notS;
	
	not g0(notS, sel);
	
	and g3(r0, in0, notS);
	and g4(r1, in1, sel);
	or g5(result, r0, r1);
	
endmodule 