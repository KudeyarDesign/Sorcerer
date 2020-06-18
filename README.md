# Sorcerer

Configurable RISC-V core with some optimization for FPGA (Altera/Intel)

RV32I architecure with "M", "C" and "A" extensions
RISC-V User-Level ISA Version 2.1
RISC-V Privileged Architecture Version 1.9.1

6-stage In-order pipeline

External Bus - Altera Avalon MM

Seperate Data and Instruction Caches (D-Cache & I-Cache)

Memory Management Unit:
Local separate TLB's for Data and Instruction (D-TLB & I-TLB)
Common pseudo-associative TLB for Data and Instruction (Joint TLB, J-TLB)

Branch Prediction Unit:
Branch History Table (BHT) - gshare direction predictor + 2-bit satiration counters
Branch Target Buffer (BTB) - multiple-associative target address table
Return Address Stack (RS)

Interrupt Controller (PLIC):
RISC-V Privileged Architecture Version 1.9.1

Debug Unit:
RISC-V External Debug Support Version 0.9
Connection to Host through Virtual JTAG

Trace Unit:
RISC-V External Debug Support Version 0.9
Connection to Host through Virtual JTAG


Maximum frequency for FPGA Altera/Intel Cyclone-V - about 140-145 MHz (Quartus 16.0, Balanced Mode synthesis configuration)

Running OS:
FreeRTOS 7 - successfully launched
Linux - not yet

Directory structure:
/rtl/sorcerer    - Processor core and SOC
/rtl/peripheral  - simple peripheral modules for Avalon MM Bus
/rtl/peripheral/hw - Avalon MM Bus HW definitions for peripheral modules

Copyright (C) 2017

Authors:

Alexey Shistko     alexey@kudeyar.com

Andrei Safronov    andrei@kudeyar.com
