module control(regWr, shiftR, init, clk, lsb, done32);

	output regWr, shiftR, init;
	input clk, lsb, done32;

	reg[1:0] S = 2'b0; 	// current state
	wire[1:0] N;			// next state

	
	wire notS1, notS0;
	not n0 (notS1, S[1]);
	not n1 (notS0, S[0]);
	
	// N1 logic
	// n1 = s1 ^ s0
	xor g0 (N[1], S[0], S[1]);
	
	// N0 logic
	// n0 = ~(s0 & ((~s1 & lsb) | (s1 & done32)))
	
	wire t0, t1, t2, t3; // temp wires
	
	and a0(t0, notS1, lsb);
	and a1(t1, S[1], done32);
	or a2(t2, t0, t1);
	and a3(t3, S[0], t2);
	not g1(N[0], t3);
	
	// init logic
	// init = ~s1 & ~s0
	or g2(init, notS1, notS0);
	
	// regWr logic
	// regWr = s1 & ~s0
	and g3(regWr, S[1], notS0);
	
	// shiftR logic 
	// shiftR = s1 & s0
	and g4(shiftR, S[1], S[0]);
	
	
	always @(posedge clk) begin
		S <= N;
	end
	
endmodule 