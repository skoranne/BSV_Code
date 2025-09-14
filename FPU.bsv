// File      : FPU.bsv
// Author    : Sandeep Koranne (C) 2025 All rights reserved
// Purpose   : ALU MMIX and RISC Processor design
// -*- mode: haskell -*-
//

package FPU;

import Instructions :: *;
import FloatingPoint:: *;
typedef Bit#(64) FP64;
typedef Bit#(32) FP32;
typedef Bit#(16) FP16;
typedef Bit#(8)  FP8;
typedef Bit#(4)  FP4;
function Double convertB2D( FP64 x );
  return Double{ sign: (x[63] == 1),
                 exp : x[62:52], // 11
                 sfd : x[51:0] };
endfunction
function FP64 convertD2B( Double x );
  return { (x.sign) ? 1:0,x.exp,x.sfd };
endfunction

function Double pow(Double x , int y);
  Double ret_val = one(False);
  for(int i = 0; i<y; i = i + 1) begin
    ret_val = ret_val * x;
  end	
  return ret_val;
endfunction

function Double taylorExp( Double x );
  let t2 = pow( x, 2)/2.0;
  let t3 = pow( x, 3)/6.0;  
  let t4 = 0;//pow( x, 4)/24.0;
  let t5 = 0;//pow( x, 5)/120.0;  
  return (one(False) + x + t2 + t3 + t4 + t5);
endfunction
function Double taylorExpN( Double x );
  let t2 = pow( x, 2)/2.0;
  let t3 = pow( x, 3)/6.0;  
  let t4 = pow( x, 4)/24.0;
  let t5 = 0;//pow( x, 5)/120.0;  
  return (one(False) - x + t2 - t3 + t4 - t5);
endfunction
  
function FP64 fnFPU( FPUInstruction instr, FP64 v1, FP64 v2);

  Double ret_val = ?;
  let fv1 = convertB2D( v1 );
  let fv2 = convertB2D( v2 );
  case( instr )
  FADD: ret_val = (begin
                  let v = addFP( fv1, fv2, Rnd_Zero );
                  tpl_1(v); // ignore the exception
                  end);
  FSUB: ret_val = fv1 - fv2;
  FMUL: ret_val = fv1 * fv2;
  FABS: ret_val = abs(fv1);
  FNOP: ret_val = 0;
  FDIV: ret_val = (begin
                  let v = divFP( fv1, fv2, Rnd_Zero );
                  tpl_1(v); // ignore the exception
                  end);
  //FEXP: ret_val = ( fv1.sign == False ) ? taylorExp( fv1 ) : taylorExpN( fv1 );
  //FEXP: ret_val = taylorExp( fv1 );
  FRECIPROCAL: ret_val = one(False)/fv1;
  /*
  FSQRT:ret_val = (begin
                   let v = sqrtFP( fv1, Rnd_Zero );
                   tpl_1(v);
                   end);
  
  FRSQRT: ret_val = (begin
                    let v = sqrtFP( fv1, Rnd_Zero );
                    one(False)/tpl_1(v);
                    end);

  FNORM: ret_val = (begin
                    let u = fv1*fv1 + fv2*fv2;
                    let v = sqrtFP(u, Rnd_Zero);
                    tpl_1(v);
                    end);
  */
  FRNG: ret_val = zero(False);
  //FPOW: ret_val = pow( fv1, 3 );
  
  default: ret_val = zero(False);
  endcase
  return convertD2B(ret_val);
endfunction

endpackage
