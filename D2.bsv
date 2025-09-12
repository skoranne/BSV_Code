package D2;
import BUtils :: *;

interface CheckIFC;

endinterface

//module mkCheckImpl#(parameter Int#(8) cKERNEL_SIZE)(CheckIFC);
//module mkCheckImpl#(parameter Integer cKERNEL_SIZE)(CheckIFC) provisos(TLog#(cKERNEL_SIZE,LogKernel));
module mkCheckImpl#(numeric size_t, numeric addr_t)(CheckIFC) provisos ( TLog#(size_t, addr_t));
endmodule

module mkCheck(CheckIFC);
  CheckIFC ifc <- mkCheckImpl(8,3);
  return ifc;
endmodule

endpackage