`timescale 1 ps / 1 ps

module register_file_tb();

	reg [3:0] rs_addr, rt_addr;
	reg [15:0] write_data; 
	reg [3:0] write_addr;
	reg clock = 1'b0, fast_clock = 1'b0;
	reg reg_write;
	wire [15:0] rs, rt;
	
	register_file _register_file(
		.rs(rs),
		.rt(rt),
		.rs_addr(rs_addr),
		.rt_addr(rt_addr),
		.write_addr(write_addr),
		.write_data(write_data),
		.reg_write(reg_write),
		.clock(clock),
		.fast_clock(fast_clock));
	
	always begin
		#2 clock = ~clock;
	end

	always begin
		#1 fast_clock = ~fast_clock;
	end
		
	
	initial begin
		write_addr = 4'h0; write_data = 16'b0; reg_write = 1'b0;
		
		   rs_addr = 16'h0; rt_addr = 16'hF;
		
		#2 rs_addr = 16'h1; rt_addr = 16'hE;
		
		#2 rs_addr = 16'h2; rt_addr = 16'hD;
		
		#2 rs_addr = 16'h3; rt_addr = 16'hC;
		
		#2 rs_addr = 16'h4; rt_addr = 16'hB;
		
		#2 rs_addr = 16'h5; rt_addr = 16'hA;
		
		#2 rs_addr = 16'h6; rt_addr = 16'h9;
		
		#2 rs_addr = 16'h7; rt_addr = 16'h8;
	end
	
	initial begin
		$monitor("time = %3d, R[%2h]: %16b, R[%2h]: %16b", $time, rs_addr, rs, rt_addr, rt);
	end
	
endmodule