// File      : Instructions.bsv
// Author    : Sandeep Koranne (C) 2025 All rights reserved
// Purpose   : MMIX and RISC Processor design
// -*- mode: haskell -*-
//

package Instructions;
import Arch :: *;
typedef Bit#(DATA_WIDTH) Word;
typedef enum { NOP, ADD, SUB,
               ADDIMM, SUBIMM,
               AND, OR, NOT, XOR, XNOR, 
               ANDIMM, ORIMM, NOTIMM, XORIMM, XNORIMM,
               SLL, SRL,
               SLLIMM, SRLIMM,
               MUL, DIV, REM,
               MULIMM, DIVIMM, REMIMM,
               ANDN, BCLR, BCLRIMM, BEXT, BEXTIMM,
               BINV, BINVIMM, BSET, BSETIMM, CLZ, CLZIMM,
               CTZ, CTZIMM, MAX, MAXIMM, MIN, MINIMM,
               POPCNT, POPCNTIMM, ROTATEL, ROTATELIMM,
               ROTATER, ROTATERIMM, SIGNEXTEND, SH1ADD, SH2ADD, SH3ADD,
               SPACESHIP, ABS, ABSIMM, ADD3, FMA,
               FACTORIAL, HYPOT, MORTON_MERGE, REVERSE_BITS, FPU, INSTR_63, INSTR_64
               } ALUInstruction deriving(Bits, Eq,FShow, Bounded);
typedef enum { FNOP, FADD, FSUB, FMUL, FDIV, FSQRT, FRSQRT, FRECIPROCAL,
	       FNORM,FSIN, FCOS, FTAN, FEXP, FLOG, FASIN, FACOS,
	       FATAN, FRNG, FSINH, FCOSH, FTANH, FRELU, FABS, FLOGB, FPOW
	       } FPUInstruction deriving(Bits, Eq,FShow, Bounded);
endpackage

