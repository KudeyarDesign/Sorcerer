# TCL File Generated by Component Editor 16.1
# Tue Sep 19 19:17:25 GMT+03:00 2017
# DO NOT MODIFY


# 
# button "Button" v1.0
#  2017.09.19.19:17:25
# 
# 

# 
# request TCL package from ACDS 16.1
# 
package require -exact qsys 16.1


# 
# module button
# 
set_module_property DESCRIPTION ""
set_module_property NAME button
set_module_property VERSION 1.0
set_module_property INTERNAL false
set_module_property OPAQUE_ADDRESS_MAP true
set_module_property AUTHOR ""
set_module_property DISPLAY_NAME Button
set_module_property INSTANTIATE_IN_SYSTEM_MODULE true
set_module_property EDITABLE true
set_module_property REPORT_TO_TALKBACK false
set_module_property ALLOW_GREYBOX_GENERATION false
set_module_property REPORT_HIERARCHY false


# 
# file sets
# 
add_fileset QUARTUS_SYNTH QUARTUS_SYNTH "" ""
set_fileset_property QUARTUS_SYNTH TOP_LEVEL debouncer
set_fileset_property QUARTUS_SYNTH ENABLE_RELATIVE_INCLUDE_PATHS false
set_fileset_property QUARTUS_SYNTH ENABLE_FILE_OVERWRITE_MODE false
add_fileset_file debouncer.vhd VHDL PATH periph/debouncer.vhd TOP_LEVEL_FILE
add_fileset_file utils.vhd VHDL PATH sorcerer/utils.vhd
add_fileset_file config.vhd VHDL PATH sorcerer/config.vhd


# 
# parameters
# 
add_parameter FREQ NATURAL 50000000
set_parameter_property FREQ DEFAULT_VALUE 50000000
set_parameter_property FREQ DISPLAY_NAME FREQ
set_parameter_property FREQ TYPE NATURAL
set_parameter_property FREQ UNITS None
set_parameter_property FREQ ALLOWED_RANGES 0:2147483647
set_parameter_property FREQ HDL_PARAMETER true
add_parameter DEBOUNCE_TIME NATURAL 1000
set_parameter_property DEBOUNCE_TIME DEFAULT_VALUE 1000
set_parameter_property DEBOUNCE_TIME DISPLAY_NAME DEBOUNCE_TIME
set_parameter_property DEBOUNCE_TIME TYPE NATURAL
set_parameter_property DEBOUNCE_TIME UNITS None
set_parameter_property DEBOUNCE_TIME ALLOWED_RANGES 0:2147483647
set_parameter_property DEBOUNCE_TIME HDL_PARAMETER true
add_parameter DEFAULT_HIGH INTEGER 1
set_parameter_property DEFAULT_HIGH DEFAULT_VALUE 1
set_parameter_property DEFAULT_HIGH DISPLAY_NAME DEFAULT_HIGH
set_parameter_property DEFAULT_HIGH TYPE INTEGER
set_parameter_property DEFAULT_HIGH UNITS None
set_parameter_property DEFAULT_HIGH ALLOWED_RANGES -2147483648:2147483647
set_parameter_property DEFAULT_HIGH HDL_PARAMETER true
add_parameter INVERT INTEGER 0
set_parameter_property INVERT DEFAULT_VALUE 0
set_parameter_property INVERT DISPLAY_NAME INVERT
set_parameter_property INVERT TYPE INTEGER
set_parameter_property INVERT UNITS None
set_parameter_property INVERT ALLOWED_RANGES -2147483648:2147483647
set_parameter_property INVERT HDL_PARAMETER true


# 
# display items
# 


# 
# connection point reset
# 
add_interface reset reset end
set_interface_property reset associatedClock clock
set_interface_property reset synchronousEdges DEASSERT
set_interface_property reset ENABLED true
set_interface_property reset EXPORT_OF ""
set_interface_property reset PORT_NAME_MAP ""
set_interface_property reset CMSIS_SVD_VARIABLES ""
set_interface_property reset SVD_ADDRESS_GROUP ""

add_interface_port reset reset reset Input 1


# 
# connection point clock
# 
add_interface clock clock end
set_interface_property clock clockRate 0
set_interface_property clock ENABLED true
set_interface_property clock EXPORT_OF ""
set_interface_property clock PORT_NAME_MAP ""
set_interface_property clock CMSIS_SVD_VARIABLES ""
set_interface_property clock SVD_ADDRESS_GROUP ""

add_interface_port clock clk clk Input 1


# 
# connection point inp
# 
add_interface inp conduit end
set_interface_property inp associatedClock ""
set_interface_property inp associatedReset ""
set_interface_property inp ENABLED true
set_interface_property inp EXPORT_OF ""
set_interface_property inp PORT_NAME_MAP ""
set_interface_property inp CMSIS_SVD_VARIABLES ""
set_interface_property inp SVD_ADDRESS_GROUP ""

add_interface_port inp inp export Input 1


# 
# connection point outp
# 
add_interface outp conduit end
set_interface_property outp associatedClock clock
set_interface_property outp associatedReset ""
set_interface_property outp ENABLED true
set_interface_property outp EXPORT_OF ""
set_interface_property outp PORT_NAME_MAP ""
set_interface_property outp CMSIS_SVD_VARIABLES ""
set_interface_property outp SVD_ADDRESS_GROUP ""

add_interface_port outp outp export Output 1
