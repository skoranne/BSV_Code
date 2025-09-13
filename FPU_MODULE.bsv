// File      : FPU_MODULE.bsv
// Author    : Sandeep Koranne (C) 2025 All rights reserved
// Purpose   : ALU MMIX and RISC Processor design
// -*- mode: haskell -*-
//

package FPU_MODULE;
import Arch :: *;
import Instructions :: *;
import FPU :: *;

interface FPUIfc;
  method Word compute( FPUInstruction instr, Word v1, Word v2);
endinterface

module mkFPU( FPUIfc );
  method Word compute( FPUInstruction instr, Word v1, Word v2);
  compute = fnFPU( instr, v1, v2 );
  endmethod
endmodule

endpackage
