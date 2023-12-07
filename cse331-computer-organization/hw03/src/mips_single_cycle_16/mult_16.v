module mult_16(
	input [15:0] multiplier, multiplicand,
	input clock, reset,
	output [31:0] product);
	
	wire wr_multiplicand, wr_iteration, wr_product;
	wire sl_add_shift, sl_input, sl_iteration;
	wire stop;
	wire [31:0] temp_product;

	assign product = temp_product;
		
	mult_16_datapath d0(
		.wr_multiplicand(wr_multiplicand),
		.wr_product(wr_product),
		.wr_iteration(wr_iteration),
		.sl_add_shift(sl_add_shift),
		.sl_input(sl_input),
		.sl_iteration(sl_iteration),
		.multiplicand (multiplicand), 
		.multiplier(multiplier),
		.clock(clock),
		.stop(stop),
		.product(temp_product)
	);
		
	mult_16_control c0(
		.wr_multiplicand(wr_multiplicand),
		.wr_product(wr_product),
		.wr_iteration(wr_iteration),
		.sl_add_shift(sl_add_shift),
		.sl_input(sl_input),
		.sl_iteration(sl_iteration),
		.product0(temp_product[0]),
		.stop(stop),
		.clock(clock),
		.reset(reset)
	);
	
endmodule