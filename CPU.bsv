// File      : CPU.bsv
// Author    : Sandeep Koranne (C) 2025 All rights reserved
// Purpose   : ALU MMIX and RISC Processor design
// -*- mode: haskell -*-
//

package CPU;
import Arch :: *;
import Instructions :: *;   
import CPU_IFC :: *;
import ALU:: *;
import ALU_MODULE:: *;
import Instructions:: *;
import Decode:: *;
import FIFOF:: *;
import Semi_FIFOF:: *;
import Assert:: *;
import RegFile :: *;
import BRAM :: *;
import StmtFSM :: *;   
   
typedef enum { CPU_STATE_RUNNING,
	       CPU_STATE_HALT_REQUESTED,
	       CPU_STATE_HALTED,
	       CPU_STATE_POWER_THROTTLE
	     } CPUState deriving (Bits,Eq,FShow);
Integer debug_level = 0;
Integer word_length = 64;

// For a multi-stage pipelined CPU,
typedef enum { A_FETCH,
	       A_DECODE,
	       A_EXECUTE,
	       A_RETIRE_CONTROL,
	       A_RETIRE_INT,
	       A_RETIRE_DMEM
	     } CPUAction deriving (Bits,Eq,FShow);

typedef 4 REG_ADDR;   
Integer reg_addr = valueOf( REG_ADDR );
   
   
(* synthesize *)
module mkCPU(CPU_IFC);
   Reg#(CPUState) reg_state <- mkReg(CPU_STATE_HALTED);
   Reg#(CPUAction) action_state <- mkReg( A_FETCH );
   RegFile#(Bit#(REG_ADDR),Word) registers <- mkRegFileFull;
   Reg#(ALUInstruction) alu_instruction <- mkReg(?);
   Reg#(Word) immediate <- mkReg(?);
   Reg#(Bit#(REG_ADDR)) src1 <- mkReg(?);
   Reg#(Bit#(REG_ADDR)) src2 <- mkReg(?);
   Reg#(Bit#(REG_ADDR)) dest1 <- mkReg(?);
   Reg#(Bit#(32)) pc <- mkReg(0);
   Reg#(Fetch_to_Decode) fetch_to_decode <- mkRegU;
   FIFOF #(Mem_Req) f_IMem_req  <- mkFIFOF;
   FIFOF #(Mem_Rsp) f_IMem_rsp  <- mkFIFOF;
   ALUIfc#(DATA_WIDTH) alu1 <- mkALU;

   //function ALUIfc#(DATA_WIDTH) toALUIFC(ALUIfc#(DATA_WIDTH) x);
   //return interface ALUIfc#(DATA_WIDTH);
   //       method Word compute(a,b,c,d) = x.compute(a,b,c,d);
   //endinterface;
   //endfunction
   
   Action decode =
   action
        let mem_rsp <- pop_o (to_FIFOF_O (f_IMem_rsp));
        let y = fn_Decode( fetch_to_decode, mem_rsp );

	alu_instruction <= unpack(y.instr[5:0]);
        src1 <= unpack(y.instr[9:6]);
        src2 <= unpack(y.instr[13:10]);
        dest1 <= unpack(y.instr[17:14]);
        immediate <= extend(unpack(y.instr[63:18]));
   endaction;
     
   Action execute =
   action
   let v1 = ( src1 ==0 ) ? 0 : registers.sub(src1);
   let v2 = ( src2 ==0 ) ? 0 : registers.sub(src2);
   let dval = alu1.compute( alu_instruction,
			    v1,
			    v2,
			    immediate );
   registers.upd( dest1, dval);
   $display("[%d] %d = [%d] %d [%d] %d instr: ",dest1,dval,src1,v1,src2,v2,fshow(alu_instruction));
   endaction;

   rule memory_clearing_house (f_IMem_req.notEmpty );
   f_IMem_req.deq;
   f_IMem_rsp.enq( Mem_Rsp{ rsp_type: MEM_RSP_OK, data: 0} );
   endrule

   rule fetch_rule ((reg_state == CPU_STATE_RUNNING) &&
	       (action_state == A_FETCH ));
   let y = fn_Fetch( pc );
   fetch_to_decode <= y.to_D;
   f_IMem_req.enq (y.mem_req);
   action_state <= A_DECODE;
   pc <= pc + 4;
   $display("Fetch instruction: %d.\n",pc);
   endrule

   rule decode_rule (action_state == A_DECODE );
   $display("Decoding instruction.\n");
   decode;
   action_state <= A_EXECUTE;
   endrule

   rule execute_rule (action_state == A_EXECUTE);
   $display("Executing...\n");
   execute;
   action_state <= A_RETIRE_DMEM;
   endrule

   rule dmem_rule (action_state == A_RETIRE_DMEM);
   action_state <= A_FETCH;
   endrule

   rule print_regs ( (reg_state == CPU_STATE_RUNNING) && (action_state == A_RETIRE_DMEM) );
    $display("\n---------***---------\n"); // atleast this rule prints continuously
    for( Bit#(REG_ADDR) i=1; i < 4; i=i+1) begin
      $display("Reg[%d] = %d", i, registers.sub(i));
    end
   endrule

   method Action boot if(reg_state == CPU_STATE_HALTED);
     $display("Booting processor.......\nDone.\n");
     reg_state <= CPU_STATE_RUNNING;
     registers.upd(1,2);
   endmethod

   method ActionValue#(Bit#(DATA_WIDTH)) selftest( Bit#(DATA_WIDTH) instr);
   $display("Self testing processor.......\nDone.\n");
   f_IMem_rsp.enq( Mem_Rsp{ rsp_type: MEM_RSP_OK, data: instr} );
   return registers.sub(2);
   endmethod

   interface fo_IMem_req = to_FIFOF_O (f_IMem_req);
   interface fi_IMem_rsp = to_FIFOF_I (f_IMem_rsp);
   //interface compute_val = alu1;
endmodule
endpackage
