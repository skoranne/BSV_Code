// File      : CPU_IFC.bsv
// Author    : Sandeep Koranne (C) 2025 All rights reserved
// Purpose   : ALU MMIX and RISC Processor design
//           : learning from Rishiyur Nikhil's book on Learn RISCv
// -*- mode: haskell -*-
//

package CPU_IFC;
import ALU_MODULE :: *;
import Arch :: *;
import FIFO :: *;
import FIFOF :: *;
import Semi_FIFOF :: *;

typedef enum {MEM_1B, MEM_2B, MEM_4B, MEM_8B} Mem_Req_Size
deriving (Bits, FShow, Eq);
typedef enum {MEM_LOAD, MEM_STORE, MEM_FENCE} Mem_Req_Type
deriving (Bits, FShow, Eq);
//typedef Bit #(5) Mem_Req_Type;

typedef struct {Mem_Req_Type  req_type;
		Mem_Req_Size  size;
		Bit #(32)     addr;
		Bit #(DATA_WIDTH)     data;     // CPU => mem data
} Mem_Req
deriving (Bits, FShow);
typedef enum {MEM_RSP_OK,
	      MEM_RSP_MISALIGNED,
	      MEM_RSP_ERR,
	      MEM_REQ_DEFERRED    // DMem only, for accesses that must be non-speculative
} Mem_Rsp_Type
deriving (Bits, FShow, Eq);

typedef struct {Mem_Rsp_Type  rsp_type;
		Bit #(DATA_WIDTH)     data;      // mem => CPU data
} Mem_Rsp
deriving (Bits, FShow);


// Fetch => Decode

typedef struct {
   Bit #(32)  pc;
} Fetch_to_Decode
deriving (Bits, FShow);
typedef struct {
   Fetch_to_Decode  to_D;
   Mem_Req          mem_req;
} Result_F
deriving (Bits, FShow);

function Result_F fn_Fetch (Bit #(32)  pc);
      Result_F y = ?;
      // Info to next stage
      y.to_D = Fetch_to_Decode {pc:           pc};
      // Request to IMem
      y.mem_req = Mem_Req {req_type: MEM_LOAD,
			   size:     MEM_8B,
			   addr:     zeroExtend (pc),
			   data :    ?};
      return y;
endfunction

typedef enum {OPCLASS_SYSTEM,     // EBREAK, ECALL, CSRRxx
              OPCLASS_CONTROL,    // BRANCH, JAL, JALR
	      OPCLASS_INT,
	      OPCLASS_MEM,        // LOAD, STORE, AMO
	      OPCLASS_FENCE}      // FENCE
OpClass
deriving (Bits, Eq, FShow);

typedef struct {Bit #(32)  pc;
		Bit #(DATA_WIDTH)    instr;
                OpClass      opclass;
		Bool         has_rs1;
		Bool         has_rs2;
		Bool         has_rd;
		Bool         writes_mem;   // All mem ops other than LOAD
		Bit #(DATA_WIDTH)  imm;          // Canonical (bit-swizzled)
} Decode_to_RR
deriving (Bits, FShow);
// Decode => Register Read

function Decode_to_RR fn_Decode( Fetch_to_Decode x_F_to_D,
	 	      		 Mem_Rsp rsp_IMem );
   Bit #(DATA_WIDTH) instr = truncate (rsp_IMem.data);
   let y = Decode_to_RR {   pc: x_F_to_D.pc,
       	   		    instr: instr,
			    opclass:      ?,
			    has_rs1:      False,
			    has_rs2:      False,
			    has_rd:       False,
			    writes_mem:   False,
			    imm:          0};
			  
   return y;
endfunction

interface CPU_IFC;
  method Action boot;
  method ActionValue#(Bit#(DATA_WIDTH)) selftest(Bit#(DATA_WIDTH) instr);
  //interface ALUIfc#(DATA_WIDTH) compute_val;
  //method Action put(Bit#(16) memrd);
  interface FIFOF_O #(Mem_Req) fo_IMem_req;
  interface FIFOF_I #(Mem_Rsp) fi_IMem_rsp;
endinterface

endpackage
