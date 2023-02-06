module control(
	output reg [2:0] current_state,
	output reg wri_reg_cypher,
	output reg wri_reg_sum,
	output reg clr_reg_sum,
	input matched,
	input protect_state,
	input read,
	input reset,
	input clock);

	reg[2:0] next_state;
	
	localparam 	DET_1 		= 3'b000,
					DET_2 		= 3'b001,
					DET_3 		= 3'b010,
					DET_4 		= 3'b011,
					IDLE 			= 3'b100,
					ACCEPT 		= 3'b101; 
		
	initial begin
		current_state = IDLE;
		next_state = IDLE;
	end
		
	// state register
	always @ (posedge clock) begin
		if (reset)
			current_state <= IDLE;
		else
			current_state <= next_state;
	end

	// next state logic
	always @ (posedge clock) begin 
		#0.5 // some delay to catch input signal (matchced)
		if (~read) begin 
			next_state = current_state;
		end
		else begin
			case (current_state)
				IDLE: begin
					next_state = DET_1;
				end
				
				DET_1: begin
					next_state = matched ? DET_2 : DET_1;
				end
				
				DET_2: begin
					if (matched)
						next_state = DET_3;
					else if (protect_state)
						next_state = current_state;
					else 
						next_state = DET_1;
				end
				
				DET_3: begin
					next_state = matched ? DET_4 : DET_1;
				end
				
				DET_4: begin
					next_state = matched ? IDLE : DET_1;
				end
				
				ACCEPT: begin
					next_state = IDLE;
				end
				
				default: begin
					next_state = current_state;
				end
			endcase
		end
	end
	
	// outputs
	always @ (*) begin	
		wri_reg_sum = read;
		
		if (current_state == IDLE) begin
			clr_reg_sum = 1'b1;
			wri_reg_cypher = 1'b1;
		end
		else begin
			clr_reg_sum = 1'b0;
			wri_reg_cypher = 1'b0;
		end
						
	end

endmodule 