module mux8to1(
	output result, 
	input [2:0] sel, 
	input in0, 
	input in1, 
	input in2, 
	input in3, 
	input in4, 
	input in5, 
	input in6, 
	input in7);

	wire l0, l1, l2, l3;
	wire r0, r1;
	
	// level 1
	mux2to1 g0(l0, sel[0], in0, in1);
	mux2to1 g1(l1, sel[0], in2, in3);
	mux2to1 g2(l2, sel[0], in4, in5);
	mux2to1 g3(l3, sel[0], in6, in7);
	
	// level 2
	mux2to1 g4(r0, sel[1], l0, l1);
	mux2to1 g5(r1, sel[1], l2, l3);
	
	// level 3
	mux2to1 g6(result, sel[2], r0, r1);
	
endmodule