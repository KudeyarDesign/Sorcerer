# TCL File Generated by Component Editor 16.1
# Sat Mar 25 14:02:03 MSK 2017
# DO NOT MODIFY


# 
# vga "vga" v1.0
#  2017.03.25.14:02:03
# 
# 

# 
# request TCL package from ACDS 16.1
# 
package require -exact qsys 16.1


# 
# module vga
# 
set_module_property DESCRIPTION ""
set_module_property NAME vga
set_module_property VERSION 1.0
set_module_property INTERNAL false
set_module_property OPAQUE_ADDRESS_MAP true
set_module_property AUTHOR ""
set_module_property DISPLAY_NAME vga
set_module_property INSTANTIATE_IN_SYSTEM_MODULE true
set_module_property EDITABLE true
set_module_property REPORT_TO_TALKBACK false
set_module_property ALLOW_GREYBOX_GENERATION false
set_module_property REPORT_HIERARCHY false


# 
# file sets
# 
add_fileset QUARTUS_SYNTH QUARTUS_SYNTH "" ""
set_fileset_property QUARTUS_SYNTH TOP_LEVEL vga
set_fileset_property QUARTUS_SYNTH ENABLE_RELATIVE_INCLUDE_PATHS false
set_fileset_property QUARTUS_SYNTH ENABLE_FILE_OVERWRITE_MODE false
add_fileset_file display_fifo.vhd VHDL PATH periph/display_fifo.vhd
add_fileset_file utils.vhd VHDL PATH sorcerer/utils.vhd
add_fileset_file vga.vhd VHDL PATH periph/vga.vhd TOP_LEVEL_FILE
add_fileset_file config.vhd VHDL PATH sorcerer/config.vhd

add_fileset SIM_VHDL SIM_VHDL "" ""
set_fileset_property SIM_VHDL TOP_LEVEL vga
set_fileset_property SIM_VHDL ENABLE_RELATIVE_INCLUDE_PATHS false
set_fileset_property SIM_VHDL ENABLE_FILE_OVERWRITE_MODE false
add_fileset_file display_fifo.vhd VHDL PATH periph/display_fifo.vhd
add_fileset_file utils.vhd VHDL PATH sorcerer/utils.vhd
add_fileset_file vga.vhd VHDL PATH periph/vga.vhd
add_fileset_file config.vhd VHDL PATH sorcerer/config.vhd


# 
# parameters
# 
add_parameter DISPLAY_BUF_START NATURAL 0
set_parameter_property DISPLAY_BUF_START DEFAULT_VALUE 0
set_parameter_property DISPLAY_BUF_START DISPLAY_NAME DISPLAY_BUF_START
set_parameter_property DISPLAY_BUF_START TYPE NATURAL
set_parameter_property DISPLAY_BUF_START UNITS None
set_parameter_property DISPLAY_BUF_START ALLOWED_RANGES 0:2147483647
set_parameter_property DISPLAY_BUF_START HDL_PARAMETER true
add_parameter DEVICE_FAMILY STRING "Cyclone V"
set_parameter_property DEVICE_FAMILY DEFAULT_VALUE "Cyclone V"
set_parameter_property DEVICE_FAMILY DISPLAY_NAME DEVICE_FAMILY
set_parameter_property DEVICE_FAMILY TYPE STRING
set_parameter_property DEVICE_FAMILY UNITS None
set_parameter_property DEVICE_FAMILY HDL_PARAMETER true
add_parameter WIDTH INTEGER 32
set_parameter_property WIDTH DEFAULT_VALUE 32
set_parameter_property WIDTH DISPLAY_NAME WIDTH
set_parameter_property WIDTH TYPE INTEGER
set_parameter_property WIDTH UNITS None
set_parameter_property WIDTH ALLOWED_RANGES -2147483648:2147483647
set_parameter_property WIDTH HDL_PARAMETER true
add_parameter FIFO_DEPTH INTEGER 128
set_parameter_property FIFO_DEPTH DEFAULT_VALUE 128
set_parameter_property FIFO_DEPTH DISPLAY_NAME FIFO_DEPTH
set_parameter_property FIFO_DEPTH TYPE INTEGER
set_parameter_property FIFO_DEPTH UNITS None
set_parameter_property FIFO_DEPTH ALLOWED_RANGES -2147483648:2147483647
set_parameter_property FIFO_DEPTH HDL_PARAMETER true
add_parameter BURST_LEN INTEGER 8
set_parameter_property BURST_LEN DEFAULT_VALUE 8
set_parameter_property BURST_LEN DISPLAY_NAME BURST_LEN
set_parameter_property BURST_LEN TYPE INTEGER
set_parameter_property BURST_LEN UNITS None
set_parameter_property BURST_LEN ALLOWED_RANGES -2147483648:2147483647
set_parameter_property BURST_LEN HDL_PARAMETER true
add_parameter CLK_PRESCALE INTEGER 2
set_parameter_property CLK_PRESCALE DEFAULT_VALUE 2
set_parameter_property CLK_PRESCALE DISPLAY_NAME CLK_PRESCALE
set_parameter_property CLK_PRESCALE TYPE INTEGER
set_parameter_property CLK_PRESCALE UNITS None
set_parameter_property CLK_PRESCALE ALLOWED_RANGES -2147483648:2147483647
set_parameter_property CLK_PRESCALE HDL_PARAMETER true


# 
# display items
# 


# 
# connection point mem
# 
add_interface mem avalon start
set_interface_property mem addressUnits SYMBOLS
set_interface_property mem associatedClock bus_clk
set_interface_property mem associatedReset bus_reset
set_interface_property mem bitsPerSymbol 8
set_interface_property mem burstOnBurstBoundariesOnly false
set_interface_property mem burstcountUnits WORDS
set_interface_property mem doStreamReads false
set_interface_property mem doStreamWrites false
set_interface_property mem holdTime 0
set_interface_property mem linewrapBursts false
set_interface_property mem maximumPendingReadTransactions 0
set_interface_property mem maximumPendingWriteTransactions 0
set_interface_property mem readLatency 0
set_interface_property mem readWaitTime 1
set_interface_property mem setupTime 0
set_interface_property mem timingUnits Cycles
set_interface_property mem writeWaitTime 0
set_interface_property mem ENABLED true
set_interface_property mem EXPORT_OF ""
set_interface_property mem PORT_NAME_MAP ""
set_interface_property mem CMSIS_SVD_VARIABLES ""
set_interface_property mem SVD_ADDRESS_GROUP ""

add_interface_port mem avm_mem_address address Output 32
add_interface_port mem avm_mem_burstcount burstcount Output 4
add_interface_port mem avm_mem_read read Output 1
add_interface_port mem avm_mem_readdata readdata Input 32
add_interface_port mem avm_mem_readdatavalid readdatavalid Input 1
add_interface_port mem avm_mem_waitrequest waitrequest Input 1


# 
# connection point ext
# 
add_interface ext conduit end
set_interface_property ext associatedClock ""
set_interface_property ext associatedReset ""
set_interface_property ext ENABLED true
set_interface_property ext EXPORT_OF ""
set_interface_property ext PORT_NAME_MAP ""
set_interface_property ext CMSIS_SVD_VARIABLES ""
set_interface_property ext SVD_ADDRESS_GROUP ""

add_interface_port ext BLUE blue Output 8
add_interface_port ext GREEN green Output 8
add_interface_port ext HSYNC hsync Output 1
add_interface_port ext RAMDAC_CLK ramdac_clk Output 1
add_interface_port ext RED red Output 8
add_interface_port ext VSYNC vsync Output 1
add_interface_port ext nRAMDAC_BLANK nramdac_blank Output 1
add_interface_port ext nRAMDAC_SYNC nramdac_sync Output 1


# 
# connection point bus_reset
# 
add_interface bus_reset reset end
set_interface_property bus_reset associatedClock bus_clk
set_interface_property bus_reset synchronousEdges DEASSERT
set_interface_property bus_reset ENABLED true
set_interface_property bus_reset EXPORT_OF ""
set_interface_property bus_reset PORT_NAME_MAP ""
set_interface_property bus_reset CMSIS_SVD_VARIABLES ""
set_interface_property bus_reset SVD_ADDRESS_GROUP ""

add_interface_port bus_reset bus_reset reset Input 1


# 
# connection point bus_clk
# 
add_interface bus_clk clock end
set_interface_property bus_clk clockRate 0
set_interface_property bus_clk ENABLED true
set_interface_property bus_clk EXPORT_OF ""
set_interface_property bus_clk PORT_NAME_MAP ""
set_interface_property bus_clk CMSIS_SVD_VARIABLES ""
set_interface_property bus_clk SVD_ADDRESS_GROUP ""

add_interface_port bus_clk bus_clk clk Input 1


# 
# connection point pix_clk
# 
add_interface pix_clk clock end
set_interface_property pix_clk clockRate 0
set_interface_property pix_clk ENABLED true
set_interface_property pix_clk EXPORT_OF ""
set_interface_property pix_clk PORT_NAME_MAP ""
set_interface_property pix_clk CMSIS_SVD_VARIABLES ""
set_interface_property pix_clk SVD_ADDRESS_GROUP ""

add_interface_port pix_clk pix_clk clk Input 1


# 
# connection point pix_reset
# 
add_interface pix_reset reset end
set_interface_property pix_reset associatedClock pix_clk
set_interface_property pix_reset synchronousEdges DEASSERT
set_interface_property pix_reset ENABLED true
set_interface_property pix_reset EXPORT_OF ""
set_interface_property pix_reset PORT_NAME_MAP ""
set_interface_property pix_reset CMSIS_SVD_VARIABLES ""
set_interface_property pix_reset SVD_ADDRESS_GROUP ""

add_interface_port pix_reset pix_reset reset Input 1

