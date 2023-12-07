`define CLOCK 2
`define DELAY 4

module cypher_detector_tb();
	
	reg [15:0] cypher;
	reg [3:0] stream;
	reg read, reset;
	reg clock = 1'b0;
	wire protect_state;
	wire [2:0] current_state;
	wire detected, matched;
	wire [7:0] sum;
	wire [3:0] wr_cypher_block;
	
	cypher_detector c0(
		.current_state(current_state),
		.protect_state(protect_state),
		.wr_cypher_block(wr_cypher_block),
		.detected(detected),
		.matched(matched),
		.sum(sum),
		.cypher(cypher),
		.stream(stream),
		.read(read),
		.reset(reset),
		.clock(clock));
	
	always begin
		#`CLOCK 
		clock = ~clock;
	end
	
	initial begin
		// initialization
		cypher = 16'b0010_0110_0000_0001;
		stream = 4'b0000;
		read = 1'b0;
		
		reset = 1'b1; #`CLOCK; 
		reset = 1'b0;
		read = 1'b1;
		
		// start
		stream = 4'b0000; #`DELAY;
		
		stream = 4'b0001; #`DELAY;
		
		stream = 4'b0011; #`DELAY;
		
		stream = 4'b0000; #`DELAY;
		
		stream = 4'b0011; #`DELAY;
		
		stream = 4'b0100; #`DELAY;

		stream = 4'b0001; #`DELAY;
		
		stream = 4'b0000; #`DELAY;
		
		stream = 4'b0010; #`DELAY;
		
		stream = 4'b0001; #`DELAY;
		
		// start of cypher sequence
		stream = 4'b0001; #`DELAY;
		
		stream = 4'b0000; #`DELAY;
		
		stream = 4'b0110; #`DELAY;
		
		stream = 4'b0010; #`DELAY;
		// end of cypher sequence
		
		stream = 4'b0001; #`DELAY;
		
		stream = 4'b0010; #`DELAY;
		
		stream = 4'b0110; #`DELAY;
		
		stream = 4'b0001; #`DELAY;
		
		stream = 4'b0111; #`DELAY;
		
	end
	
	
endmodule 