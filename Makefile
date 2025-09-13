# File    : Makefile
# Author  : Sandeep Koranne (C) 2025.
# Purpose : Command lines for Verilog generation for BSV
mkFPU.v:	FPU_MODULE.bsv FPU.bsv
	bsc +RTS -Ksize -RTS -verilog -u -g mkFPU FPU_MODULE.bsv	
mkCPU.v:	CPU.bo
	bsc -verilog -g mkCPU CPU.bsv
CPU.bo:	CPU.bsv
	bsc -u CPU.bsv
mkALU.v:	ALU_MODULE.bo
	bsc -verilog -g mkALU ALU_MODULE.bsv
ALU_MODULE.bo:	ALU_MODULE.bsv Instructions.bsv ALU.bsv
	bsc -u ALU_MODULE.bsv
