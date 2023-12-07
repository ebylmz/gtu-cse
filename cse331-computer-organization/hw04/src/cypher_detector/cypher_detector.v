module cypher_detector(
	output [3:0] wr_cypher_block,
	output [2:0] current_state,
	output protect_state,
	output detected,
	output matched,
	output [7:0] sum,
	input [15:0] cypher,
	input [3:0] stream,
	input read,
	input reset,
	input clock);

	// wire [2:0] current_state;
	wire wri_reg_cypher, wri_reg_sum, clr_reg_sum;
	
	datapath d0(
		.wr_cypher_block(wr_cypher_block),
		.detected(detected),
		.matched(matched),
		.protect_state(protect_state),
		.sum(sum),
		.cypher(cypher),
		.stream(stream),
		.current_state(current_state[1:0]),
		.wri_reg_cypher(wri_reg_cypher),
		.wri_reg_sum(wri_reg_sum),
		.clr_reg_sum(clr_reg_sum),
		.clock(clock)
	);
	

	control c0(
		.current_state(current_state),
		.wri_reg_cypher(wri_reg_cypher),
		.wri_reg_sum(wri_reg_sum),
		.clr_reg_sum(clr_reg_sum),
		.matched(matched),
		.protect_state(protect_state),
		.read(read),
		.reset(reset),
		.clock(clock)
	);
	
endmodule 