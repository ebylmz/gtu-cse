module alu32(Y, clk, A, B, ALUop);

	output[31:0] Y;
	input clk;
	input[31:0] A, B;
	input[2:0] ALUop;
	
	wire notALUop2, notALUop0, enSUB, enMULT;
	wire[31:0] wADD, wMULT, wXOR, wAND, wOR, wSLT, wNOR;
	
	not n0(notALUop2, ALUop[2]);
	not n1(notALUop0, ALUop[0]);
	
	wire t0, t1;
	// enSUB = ALUop[2] | (~ALUop[2] & ALUop[0]);
	and e1(t0, notALUop2, ALUop[0]);
	or e2(enSUB, t0, ALUop[2]);
	
	// enMULT = ~ALUop2 & ALUop1 & ~ALUop0
	and a0(t1, notALUop0, notALUop2);
	and a1(enMULT, t1, ALUop[1]);
	
	mult32 g0(wMULT, wADD, clk, A, B, enMULT, enSUB);
	
	// substract result in wADD so R6 = 31'b0 || wADD[31]
	// or g1(wSLT[0], wADD[31], 1'b0); 
	assign wSLT = {31'b0, wADD[31]};
	
	xor32 l0 (wXOR, A, B);
	and32 l1 (wAND, A, B);
	or32 l2 (wOR, A, B);
	not32 l3 (wNOR, wOR); 
	
	// results to multiplexer
	mux8to1_32 m(Y, ALUop, wADD, wADD, wMULT, wXOR, wAND, wOR, wSLT, wNOR);
	
endmodule 