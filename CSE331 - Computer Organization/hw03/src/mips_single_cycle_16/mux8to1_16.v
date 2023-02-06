module mux8to1_16(
	output[15:0] result, 
	input [2:0] sel, 
	input [15:0] in0, 
	input [15:0] in1, 
	input [15:0] in2, 
	input [15:0] in3, 
	input [15:0] in4, 
	input [15:0] in5, 
	input [15:0] in6, 
	input [15:0] in7);
	
	wire[15:0] l0, l1, l2, l3;
	wire[15:0] r0, r1;
	
	// level 1
	mux2to1_16 g0(l0, sel[0], in0, in1);
	mux2to1_16 g1(l1, sel[0], in2, in3);
	mux2to1_16 g2(l2, sel[0], in4, in5);
	mux2to1_16 g3(l3, sel[0], in6, in7);
	
	// level 2
	mux2to1_16 g4(r0, sel[1], l0, l1);
	mux2to1_16 g5(r1, sel[1], l2, l3);
	
	// level 3
	mux2to1_16 g6(result, sel[2], r0, r1);
	
endmodule