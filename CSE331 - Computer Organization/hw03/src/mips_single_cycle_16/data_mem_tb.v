`timescale 1 ps / 1 ps

module data_mem_tb();

	reg clock = 1'b0, mem_read, mem_write;
	reg [15:0] address;
	reg [15:0] write_data;
	wire [15:0] read_data;
	
	data_mem _data_mem(
		.address(address),
		.clock(clock),
		.data(write_data),
		.rden(mem_read),
		.wren(mem_write),
		.q(read_data));
	
	always begin
		#1 clock = ~clock;
	end
		
	initial begin		
			address = 16'h0; write_data = 16'hFAFA; mem_read = 1'b0; mem_write = 1'b1;
		#4 mem_read = 1'b1; mem_write = 1'b0;
		
		#4 address = 16'h1; write_data = 16'hECEA; mem_read = 1'b0; mem_write = 1'b1;
		#4 mem_read = 1'b1; mem_write = 1'b0;
		
		#4 address = 16'h2; write_data = 16'hABBA; mem_read = 1'b0; mem_write = 1'b1;
		#4 mem_read = 1'b1; mem_write = 1'b0;

		#4 address = 16'h3; write_data = 16'h213A; mem_read = 1'b0; mem_write = 1'b1;
		#4 mem_read = 1'b1; mem_write = 1'b0;
	end
	
/*
	initial begin
		write_data = 16'b0; 
		mem_read = 1'b1; 
		mem_write = 1'b0;
		address = 16'h0;
		
		#4 address = address + 16'h1;
		#4 address = address + 16'h1;
		#4 address = address + 16'h1;
		#4 address = address + 16'h1;
		
		#4 mem_read = 1'b0; mem_write = 1'b1; write_data = 16'b1;
		#4 address = address + 16'h1;
		#4 address = address + 16'h1;
	end
*/
	
	initial begin
		$monitor("time = %3d, address: %4h, value: %16b", $time, address, read_data);
	end
	
endmodule