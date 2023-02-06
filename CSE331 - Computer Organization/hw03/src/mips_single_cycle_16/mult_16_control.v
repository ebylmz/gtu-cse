module mult_16_control(
	output reg wr_multiplicand,
	output reg wr_product,
	output reg wr_iteration,
	output reg sl_add_shift,
	output reg sl_input,
	output reg sl_iteration,
	input product0,
	input stop,
	input clock,
	input reset);
	
	reg [1:0] current_state, next_state;

	localparam	IDLE 		= 2'b00,
					DECIDE	= 2'b01,
					ADD 		= 2'b10,
					SHIFT		= 2'b11;
		
	//state register
	always @ (posedge clock) begin
		if(reset)
			current_state <= IDLE;
		else 
			current_state <= next_state;
	end
		
	//next state logic
	always @ (*) begin
		case (current_state)
			IDLE: begin
				next_state = DECIDE;
			end
				
			DECIDE: begin
				if(stop)
					next_state = DECIDE;
				else begin
					if(product0)
						next_state = ADD;
					else
						next_state = SHIFT;
				end
			end
			
			ADD: begin
				next_state = SHIFT;
			end
			
			SHIFT: begin
				next_state = DECIDE;
			end
		endcase
	end

	//Outputs
	always @ (*) begin
		wr_multiplicand 	= 1'b0;
		wr_product 			= 1'b0;
		wr_iteration 		= 1'b0;
		sl_add_shift 		= 1'b0;
		sl_iteration		= 1'b0;
		sl_input				= 1'b0;
		
		case (current_state)
			IDLE: begin
				wr_multiplicand 	= 1'b1;
				wr_product 			= 1'b1;
				wr_iteration 		= 1'b1;
				sl_input 			= 1'b1;
			end
				
			DECIDE: begin
				
			end
			
			ADD: begin
				wr_product 		= 1'b1;
				sl_add_shift 	= 1'b0;
				sl_input 		= 1'b0;
			end
			
			SHIFT: begin
				sl_add_shift 	= 1'b1;
				wr_product 		= 1'b1;
				sl_input 		= 1'b0;
				sl_iteration	= 1'b1;
				wr_iteration	= 1'b1;
			end
		endcase
	end
	
endmodule