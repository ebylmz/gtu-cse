module mult_16_datapath(
	input wr_multiplicand,
	input wr_product,
	input wr_iteration,
	input sl_add_shift,
	input sl_input,
	input sl_iteration,
	input [15:0] multiplicand, multiplier,
	input clock,
	output stop,
	output [31:0] product);
	
	reg [31:0] reg_product;
	reg [15:0] reg_multiplicand;
	reg [15:0] reg_multiplier;
	reg [5:0] reg_iteration;

	wire [31:0] w_comp;
	wire [31:0] w_adder;
	wire [31:0] w_shift;
	wire [31:0] w_product;
	wire [31:0] w_alpha;
	wire [5:0] w_iteration;
	wire [5:0] w_itsum;

	assign w_adder 	= {reg_product[31:16] + reg_multiplicand, reg_product[15:0]};
	assign w_comp 		= {16'd0, multiplier};
	assign w_shift 	= reg_product >> 1;
	assign w_itsum 	= reg_iteration + 6'b1;
	assign product		= reg_product;
	assign stop 		= reg_iteration >= 16;

	/*MUXes*/
	assign w_alpha 		= sl_add_shift ? w_shift : w_adder;
	assign w_product 		= sl_input ? w_comp : w_alpha;
	assign w_iteration 	= sl_iteration ? w_itsum : 6'b0;

	/*Registers*/
	always @ (posedge clock) begin
		if(wr_multiplicand)
			reg_multiplicand <= multiplicand;
		if(wr_product)
			reg_product <= w_product;
		if(wr_iteration)
			reg_iteration <= w_iteration;
	end

endmodule