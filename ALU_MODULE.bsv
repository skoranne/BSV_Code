// File      : ALU_MODULE.bsv
// Author    : Sandeep Koranne (C) 2025 All rights reserved
// Purpose   : ALU MMIX and RISC Processor design
// -*- mode: haskell -*-
//

package ALU_MODULE;
import Arch :: *;
import ALU :: *;
import Instructions :: *;
interface ALUIfc #(numeric type n);
  method Word compute(ALUInstruction instr, Word v1, Word v2, Word imm);
endinterface

(* synthesize *)
(* doc = "These are the instructions supported by our ALU" *)
module mkALU(ALUIfc#(DATA_WIDTH));
  method Word compute(ALUInstruction instr, Word v1, Word v2, Word imm);
    compute = fnALU( instr, v1, v2, imm );
  endmethod: compute
endmodule: mkALU


module myALU(ALUIfc#(8));
  ALUIfc#(8) ret_val = (interface ALUIfc#(8);
              method Word compute(ALUInstruction instr, Word v1, Word v2, Word imm);
               compute = 0;
              endmethod
             endinterface);
  return ret_val;
endmodule: myALU

endpackage: ALU_MODULE

