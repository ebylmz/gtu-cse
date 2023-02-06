transcript on
if {[file exists rtl_work]} {
	vdel -lib rtl_work -all
}
vlib rtl_work
vmap work rtl_work

vlog -vlog01compat -work work +incdir+D:/intelFPGA_lite/18.1/workspace/mips_single_cycle_16 {D:/intelFPGA_lite/18.1/workspace/mips_single_cycle_16/mips_single_cycle_16.v}
vlog -vlog01compat -work work +incdir+D:/intelFPGA_lite/18.1/workspace/mips_single_cycle_16 {D:/intelFPGA_lite/18.1/workspace/mips_single_cycle_16/control_unit.v}
vlog -vlog01compat -work work +incdir+D:/intelFPGA_lite/18.1/workspace/mips_single_cycle_16 {D:/intelFPGA_lite/18.1/workspace/mips_single_cycle_16/datapath.v}
vlog -vlog01compat -work work +incdir+D:/intelFPGA_lite/18.1/workspace/mips_single_cycle_16 {D:/intelFPGA_lite/18.1/workspace/mips_single_cycle_16/register_file.v}
vlog -vlog01compat -work work +incdir+D:/intelFPGA_lite/18.1/workspace/mips_single_cycle_16 {D:/intelFPGA_lite/18.1/workspace/mips_single_cycle_16/pc_next.v}
vlog -vlog01compat -work work +incdir+D:/intelFPGA_lite/18.1/workspace/mips_single_cycle_16 {D:/intelFPGA_lite/18.1/workspace/mips_single_cycle_16/alu_16.v}
vlog -vlog01compat -work work +incdir+D:/intelFPGA_lite/18.1/workspace/mips_single_cycle_16 {D:/intelFPGA_lite/18.1/workspace/mips_single_cycle_16/mult_16.v}
vlog -vlog01compat -work work +incdir+D:/intelFPGA_lite/18.1/workspace/mips_single_cycle_16 {D:/intelFPGA_lite/18.1/workspace/mips_single_cycle_16/mult_16_control.v}
vlog -vlog01compat -work work +incdir+D:/intelFPGA_lite/18.1/workspace/mips_single_cycle_16 {D:/intelFPGA_lite/18.1/workspace/mips_single_cycle_16/mult_16_datapath.v}
vlog -vlog01compat -work work +incdir+D:/intelFPGA_lite/18.1/workspace/mips_single_cycle_16 {D:/intelFPGA_lite/18.1/workspace/mips_single_cycle_16/or_16.v}
vlog -vlog01compat -work work +incdir+D:/intelFPGA_lite/18.1/workspace/mips_single_cycle_16 {D:/intelFPGA_lite/18.1/workspace/mips_single_cycle_16/mux8to1_16.v}
vlog -vlog01compat -work work +incdir+D:/intelFPGA_lite/18.1/workspace/mips_single_cycle_16 {D:/intelFPGA_lite/18.1/workspace/mips_single_cycle_16/mux2to1_16.v}
vlog -vlog01compat -work work +incdir+D:/intelFPGA_lite/18.1/workspace/mips_single_cycle_16 {D:/intelFPGA_lite/18.1/workspace/mips_single_cycle_16/mux2to1.v}
vlog -vlog01compat -work work +incdir+D:/intelFPGA_lite/18.1/workspace/mips_single_cycle_16 {D:/intelFPGA_lite/18.1/workspace/mips_single_cycle_16/add_16.v}
vlog -vlog01compat -work work +incdir+D:/intelFPGA_lite/18.1/workspace/mips_single_cycle_16 {D:/intelFPGA_lite/18.1/workspace/mips_single_cycle_16/add_4.v}
vlog -vlog01compat -work work +incdir+D:/intelFPGA_lite/18.1/workspace/mips_single_cycle_16 {D:/intelFPGA_lite/18.1/workspace/mips_single_cycle_16/add_1.v}
vlog -vlog01compat -work work +incdir+D:/intelFPGA_lite/18.1/workspace/mips_single_cycle_16 {D:/intelFPGA_lite/18.1/workspace/mips_single_cycle_16/full_adder_1.v}
vlog -vlog01compat -work work +incdir+D:/intelFPGA_lite/18.1/workspace/mips_single_cycle_16 {D:/intelFPGA_lite/18.1/workspace/mips_single_cycle_16/half_adder_1.v}
vlog -vlog01compat -work work +incdir+D:/intelFPGA_lite/18.1/workspace/mips_single_cycle_16 {D:/intelFPGA_lite/18.1/workspace/mips_single_cycle_16/and_16.v}
vlog -vlog01compat -work work +incdir+D:/intelFPGA_lite/18.1/workspace/mips_single_cycle_16 {D:/intelFPGA_lite/18.1/workspace/mips_single_cycle_16/instr_mem.v}
vlog -vlog01compat -work work +incdir+D:/intelFPGA_lite/18.1/workspace/mips_single_cycle_16 {D:/intelFPGA_lite/18.1/workspace/mips_single_cycle_16/data_mem.v}
vlog -vlog01compat -work work +incdir+D:/intelFPGA_lite/18.1/workspace/mips_single_cycle_16 {D:/intelFPGA_lite/18.1/workspace/mips_single_cycle_16/registers.v}
vlog -vlog01compat -work work +incdir+D:/intelFPGA_lite/18.1/workspace/mips_single_cycle_16 {D:/intelFPGA_lite/18.1/workspace/mips_single_cycle_16/mips_single_cycle_16_tb.v}

vlog -vlog01compat -work work +incdir+D:/intelFPGA_lite/18.1/workspace/mips_single_cycle_16 {D:/intelFPGA_lite/18.1/workspace/mips_single_cycle_16/mips_single_cycle_16_tb.v}

vsim -t 1ps -L altera_ver -L lpm_ver -L sgate_ver -L altera_mf_ver -L altera_lnsim_ver -L cyclonev_ver -L cyclonev_hssi_ver -L cyclonev_pcie_hip_ver -L rtl_work -L work -voptargs="+acc"  mips_single_cycle_16_tb

add wave *
view structure
view signals
run -all
