module mult32(multResult, sumResult, clk, A, B, enMult, enSub);

	output[31:0] multResult, sumResult;
	input[31:0] A, B;
	input clk;
	input enSub, enMult;


	wire[31:0] result, product, multiplicant;
	wire done32, clk, init, regWr, shiftR;
	
	wire[31:0] addOpA, addOpB;
	
	mux2to1_32 m0(addOpA, enMult, A, product);
	mux2to1_32 m1(addOpB, enMult, B, multiplicant);
	
	wire Co;
	add32 a0(sumResult, Co, addOpA, addOpB, enSub, enSub);
	
	datapath d0(multResult, product, multiplicant, done32, clk, sumResult, A, B, init, regWr, shiftR);

	control c0(regWr, shiftR, init, clk, /* lsb */ product[0], done32);
	
endmodule 