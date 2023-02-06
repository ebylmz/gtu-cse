module register_file(
	output [15:0] rs,
	output [15:0] rt,
	input	[3:0] rs_addr,
	input [3:0] rt_addr,
	input [3:0] write_addr,
	input	[15:0] write_data,
	input	reg_write,
	input	clock,
	input	fast_clock);

	reg wren = 1'b0, rden = 1'b0;
	reg [3:0] addr_b = 4'b0;
	reg [15:0] data_b = 16'b0;
	
	// read/write in half cycle 
	always @ (*) begin
		
		data_b = write_data;
		
		if (clock) begin
			rden = 1'b1;
			wren = 1'b0;
			addr_b = rt_addr;
		end
		else begin 
			// wren = 1'b0;
			rden = 1'b0;
			wren = reg_write;
			addr_b = write_addr;
		end
	end
		
	registers _registers(
		.address_a(rs_addr),
		.address_b(addr_b),
		.clock(fast_clock),
		.rden_a(rden),
		.rden_b(rden),
		.data_a(16'b0),
		.data_b(data_b),
		.wren_a(1'b0),  
		.wren_b(wren),
		.q_a(rs),
		.q_b(rt));
		
endmodule