// File    : Top.bsv
// Author  : Sandeep Koranne (C) 2025.
// Purpose : Testbench for VLIW processor

package Top;

import CPU_IFC :: *;
import FIFOF :: *;
import CPU :: *;
import Instructions :: *;

module mkTop(Empty);
       Reg#(int) i <- mkReg(0);
       CPU_IFC cpu <- mkCPU;

       rule start1 (i==1);
       let instr = {58'b0_0010_0001_0001,pack(Instructions::SLL)};       
       cpu.selftest(instr);       
       endrule

       rule start2 (i==2);
       let instr = {58'b0_0011_0010_0001,pack(Instructions::ADD)};       
       cpu.selftest(instr);       
       endrule

       rule check (i==0);
       cpu.boot;
       endrule

       rule incr;
       i <= i+1;
       endrule
       
       rule stop (i==100);
       $finish(0);
       endrule
endmodule 
endpackage