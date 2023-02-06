module mux2to1(Y, S, I0, I1);

	output Y;
	input S;
	input I0, I1;
	
	wire r0, r1, notS;
	
	not g0(notS, S);
	
	and g3(r0, I0, notS);
	and g4(r1, I1, S);
	or g5(Y, r0, r1);
	
endmodule 