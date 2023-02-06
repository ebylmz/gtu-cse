module mult_16_tb();

	reg [15:0] multiplicand, multiplier;
	reg clock = 1'b0, reset;
	wire [31:0] product;

	mult_16 mult0(
		.multiplier(multiplier), 
		.multiplicand(multiplicand),
		.clock(clock), 
		.reset(reset),
		.product(product));
		
	always begin
		#1
		clock = ~clock;
	end

	initial begin
		reset = 1'b1;
		multiplier = 16'd3;
		multiplicand = 16'd2;
		#10 reset = 1'b0;
		
		#100 reset = 1'b1;
		multiplier = 16'd73;
		multiplicand = 16'd19;
		#10 reset = 1'b0;
		
		#100 reset = 1'b1;
	end 


endmodule