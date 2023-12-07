`timescale 1 ps / 1 ps

module registers_tb();

	reg [3:0]  address_a;
	reg [3:0]  address_b;
	reg clock = 1'b0;
	reg [15:0]  data_a;
	reg [15:0]  data_b;
	reg rden_a;
	reg rden_b;
	reg wren_a;
	reg wren_b;
	wire [15:0]  q_a;
	wire [15:0]  q_b;	

	registers _registers(
		.address_a(address_a),
		.address_b(address_b),
		.clock(clock),
		.data_a(data_a),
		.data_b(data_b),
		.rden_a(rden_a),
		.rden_b(rden_b),
		.wren_a(wren_a),
		.wren_b(wren_b),
		.q_a(q_a),
		.q_b(q_b));
	
	always begin
		#1 clock = ~clock;
	end
		
	initial begin

		address_a = 16'h0;
		address_b = 16'hF;
		data_a = 16'b0;
		data_b = 16'b0;
		rden_a = 1'b1;
		rden_b = 1'b1;
		wren_a = 1'b0;
		wren_b = 1'b0;
			
		#2 address_a = 16'h1; address_b = 16'hE;
		#2 address_a = 16'h2; address_b = 16'hD;
		#2 address_a = 16'h3; address_b = 16'hC;
		#2 address_a = 16'h4; address_b = 16'hB;
		#2 address_a = 16'h5; address_b = 16'hA;
		#2 address_a = 16'h6; address_b = 16'h9;
		#2 address_a = 16'h7; address_b = 16'h8;
	end
	
	initial begin
		$monitor("time = %3d, R[%2h]: %16b, R[%2h]: %16b", $time, address_a, q_a, address_b, q_b);
	end
	
endmodule