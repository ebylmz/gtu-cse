`define CLOCK 4

module control_tb();

	reg clk, lsb, done32;
	wire init, regWr, shiftR;
	
	control t(regWr, shiftR, init, clk, lsb, done32);
		
	initial begin
		clk = 1'b0;
		done32 = 1'b0;
		lsb = 1'b0;
		
		// 1st cycle
		#40 
		done32 = 1'b0;
		lsb = 1'b1;
		
		#80 
		lsb = 1'b0;
		
		#120 
		done32 = 1'b1;
		$finish;
	end
	
	// define clock cycle
	always begin
		#2	clk = ~clk;
	end
		
	initial begin
		$monitor("time = %4d, lsb = %1b, done32 = %1b, regWr = %1b, shiftR = %1b, init = %1b", 
			$time, lsb, done32, regWr, shiftR, init);
	end
	
	

endmodule