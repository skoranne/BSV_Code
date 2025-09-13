// File      : ALU.bsv
// Author    : Sandeep Koranne (C) 2025 All rights reserved
// Purpose   : ALU MMIX and RISC Processor design
// -*- mode: haskell -*-
//

package ALU;

import Instructions :: *;
import Vector :: *;

//function Integer factorial( Bit#(n) x) provisos( Bits#(Integer,n));
//  if( x <= 1 ) return 1;
//  else return( unpack(x) * fromInteger(factorial(x-1)) );
//endfunction
function Bit#(n) mortonMerge( Bit#(n) x, Bit#(n) y );
  Bit#(n) ret_val = 0;
  for( Integer i=0; i < valueOf(n); i = i+1 )
  begin
     ret_val[i] = ( i % 2 == 0 ) ? x[i] : y[i];
  end
  return ret_val;
endfunction
  
  
function Integer spaceship_operator(Bit#(n) v1, Bit#(n) v2);
  Integer ret_val = 0;
  if(v1 < v2) begin ret_val = -1; end
  if(v1 > v2) begin ret_val = 1; end
  return ret_val;
endfunction
function Integer clz(Bit#(n) x);
  Integer ret_val = 0;
  for( Integer i=valueOf(n)-1; i >= 0; i = i-1 ) begin
     if( x[i] == 1'b0) begin
       ret_val = i;
     end
  end
  return ret_val;
endfunction
function Integer ctz(Bit#(n) x);
  Integer ret_val = 0;
  for( Integer i=0; i < valueOf(n)-1; i = i+1 ) begin
     if( x[i] == 1'b0) begin
       ret_val = i;
     end
  end
  return ret_val;
endfunction
// For some reason, Verilog code generation of cpop is taking too much runtime/memory
// once we changed the return val from Integer to Bit#(n), its fine, this makes sense
// in elaboration space.
function Bit#(n) cpop(Bit#(n) x);
  //Integer ret_val = 0;
  Bit#(n) ret_val = 0; // pack(countOnes(x));
  for( Integer i=0; i < valueOf(n)-1; i = i+1 ) begin
    if( x[i] == 1'b1) begin
      ret_val = ret_val + 1;
    end
  end
  return ret_val;
endfunction

//function Bit#(pop_size) popCount(Bit#(n) x) provisos (Log#(n, log_size),
//                                              Add#(1, log_size, pop_size));
//    if (valueOf(n) == 1) return zeroExtend(x);
//    else begin
//        let half = valueOf(n)/2;
//        let x_lo = x[half-1:0];
//        let x_hi = x[valueOf(n)-1:half];
//        return popCount(x_lo) + popCount(x_hi);
//    end
//endfunction

//function Bit#(TLog#(n+1)) popCountBit#(numeric type n)(Bit#(n) x);
//function Bit#(n) popCountBit(Bit#(n) x);
//    return reduce(+, 0, unpack(x));
//endfunction

function t add3( t a, t b, t c ) provisos(Arith#(t));
  return (a+b+c);
endfunction

function t fma( t a, t b, t c ) provisos(Arith#(t));
  return (a*b+c);
endfunction

//function Bool is_perfect_square(t a) provisos(Arith#(t));
//  let s = sqrt_comb(a);
//  return (s*s == a);
//endfunction

function Bit#(n) rotate_left(Bit#(n) x, Bit#(n) d);
    Bit#(n) shift_amount = unpack(d);
    return (x << shift_amount) | (x >> (fromInteger(valueOf(n)) - shift_amount));
endfunction
function Bit#(n) rotate_right(Bit#(n) x, Bit#(n) d);
    Bit#(n) shift_amount = unpack(d);
    return (x >> shift_amount) | (x << (fromInteger(valueOf(n)) - shift_amount));
endfunction

function Int#(8) findIndex(Vector#(16, Bit#(8)) data, Bit#(8) target);
    Int#(8) result = ~0; // Initialize result to a default "not found" value (-1).
    for (Integer i = 0; i < 16; i = i + 1) begin
        if (data[i] == target) begin
            result = fromInteger(i); // Assign the index if a match is found.
            // In Bluespec, loops are unrolled at compile time.
            // A dynamic 'break' or 'return' from inside the loop isn't possible.
            // Instead, the 'if' condition and result variable achieve the same effect.
        end
    end
    return result;
endfunction
// There is no problem in keeping the implementation
// of this ALU parameterized on the wordlen, as we can
// therefore easily generate 16/32/64/128 bit implementations
function Word fnALU( ALUInstruction instr,
                        Word v1,
                        Word v2,
                        Word imm );
  Word ret_val = ?;

  case( instr )
  NOP: ret_val = 0;
  ADD: ret_val = v1 + v2;
  ADDIMM: ret_val = v1 + imm;
  SUB: ret_val = v1 - v2;
  SUBIMM: ret_val = v1 - imm;
  AND: ret_val = v1 & v2;
  ANDIMM: ret_val = v1 & imm;
  OR: ret_val = v1 | v2;
  ORIMM: ret_val = v1 | imm;
  NOT: ret_val = v1 & (~v2);
  NOTIMM: ret_val = v1 & (~imm);
  XOR: ret_val = v1 ^ v2;
  XORIMM: ret_val = v1 ^ imm;
  XNOR: ret_val = ~(v1 ^ v2);
  XNORIMM: ret_val = ~(v1 ^ imm);
  SLL: ret_val = v1 << v2;
  SLLIMM: ret_val = v1 << imm;
  SRL: ret_val = v1 >> v2;
  SRLIMM: ret_val = v1 >> imm;
  MUL: ret_val = v1 * v2; // overflow
  MULIMM: ret_val = v1 * imm;
  DIV: ret_val = v1 / v2; // overflow
  DIVIMM: ret_val = v1 / imm;
  REM: ret_val = v1 % v2; // overflow
  REMIMM: ret_val = v1 % imm;
  ANDN: ret_val = v1 & (~v2);
  BCLR: ret_val = v1 & ~(1 << v2);
  BCLRIMM: ret_val = v1 & ~(1 << imm);
  BEXT: ret_val = (v1 >> v2) & 1;
  BEXTIMM: ret_val = (v1 >> imm) & 1;
  BINV: ret_val = v1 ^ (1 << v2);
  BINVIMM: ret_val = v1 ^ (1 << imm);
  BSET: ret_val = v1 | (1 << v2);
  BSETIMM: ret_val = v1 | (1 << imm);
  CLZ: ret_val = fromInteger( clz( v1 ) );
  CLZIMM: ret_val = fromInteger( clz( imm ) );
  CTZ: ret_val = fromInteger( clz( v1 ) );
  CTZIMM: ret_val = fromInteger( clz( imm ) );
  MAX: ret_val = max( v1, v2 );
  MAXIMM: ret_val = max( v1, imm );
  MIN: ret_val = min( v1, v2 );
  MINIMM: ret_val = min( v1, imm );
  //POPCNT: ret_val = popCountBit(v1);
  POPCNT: ret_val = cpop( v1 );
  POPCNTIMM: ret_val = cpop( imm );
  ROTATEL: ret_val = rotate_left( v1, v2 );
  ROTATELIMM: ret_val = rotate_left( imm, v2 );
  ROTATER: ret_val = rotate_right( v1, v2 );
  ROTATERIMM: ret_val = rotate_right( imm, v2 );
  SIGNEXTEND: ret_val = signExtend(v1);
  SH1ADD: ret_val = (v1 << 1) + v2;
  SH2ADD: ret_val = (v1 << 2) + v2;
  SH3ADD: ret_val = (v1 << 3) + v2;
  SPACESHIP: ret_val = fromInteger(spaceship_operator(v1, v2));
  ABS: ret_val = ( v1 < 0 ) ? -v1 : v1;
  ABSIMM: ret_val = ( imm < 0 ) ? -imm : imm;
  //FACTORIAL: ret_val = fromInteger(factorial(unpack(v1)));
  ADD3: ret_val = add3( v1, v2, imm );
  FMA: ret_val = fma( imm, v1, v2 );
  HYPOT: ret_val = (begin
          let v1_sq = v1*v1;
          let v2_sq = v2*v2;
          (v1_sq+v2_sq);
          end);
  MORTON_MERGE: ret_val = mortonMerge( v1, v2 );
  REVERSE_BITS: ret_val = reverseBits(v1);
  FPU: ret_val = 0; // but we should never come here
  endcase
  return ret_val;
endfunction
  

endpackage
