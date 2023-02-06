module datapath(multResult, currProduct, multiplicant, done32, clk, sum, A, B, init, regWr, shiftR);	
	
	output reg[31:0] multResult = 32'b0; 
	output reg[31:0] multiplicant = 32'b0;
	output[31:0] currProduct;
	output done32; 
	
	// no need to send multiplicand and multiplier actually
	
	input[31:0] sum, A, B;
	input clk, init, regWr, shiftR;
	
	reg[63:0] product = 64'b0;
	reg[4:0] count = 5'b0;
	
	// done32
	wire t0, t1, t2;
	and a0(t0, count[0], count[1]);
	and a1(t1, count[2], count[3]);
	and a2(t2, t0, t1);
	and a3(done32, t2, count[4]);
	
	// wire[31:0] t4;
	// ext32 e(t4, 1'b0);
	or32 o(currProduct, product[31:0], 32'b0);
	
	
	always @ (posedge clk) begin 
	
		if (init) begin
			count <= 5'b0;
			product[31:0] <= B;
			multiplicant <= B;
		end
		
		if (regWr) begin 
			product[63:32] <= sum;
		end	
	
		if (shiftR) begin
			product <= product >> 1;
			count <= count + 1;
		end
		
		if (done32) begin
			multResult <= product[31:0];
		end

	end

endmodule 