// Copyright (c) 2016 Min Chen
// All rights reserved.
//
// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following conditions are met:
//
// 1. Redistributions of source code must retain the above copyright notice, this
// list of conditions and the following disclaimer.
//
// 2. Redistributions in binary form must reproduce the above copyright notice,
// this list of conditions and the following disclaimer in the documentation
// and/or other materials provided with the distribution.
//
// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
// ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
// WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
// DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
// FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
// DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
// SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
// CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
// OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
// OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
//

import Vector::*;
import GetPut::*;
import ConfigReg::*;

interface ITranspose#(numeric type n, type a);
   interface Put#(Vector#(n, a)) inp;
   interface Get#(Vector#(n, a)) oup;
endinterface

module mkTranspose(ITranspose#(n,a))
   provisos(
      Bits#(Vector#(n,a), __a),
      Add#(1, TLog#(n), cntBits)
   );

   RWire#(Vector#(n, a)) rw_inp           <- mkRWire;
   Reg#(Vector#(n, a)) obuf               <- mkRegU;

   Vector#(n, Reg#(Vector#(n, a))) matrix <- replicateM(mkRegU);
   Reg#(Bit#(cntBits)) count              <- mkReg(0);
   Reg#(Bool) startOutput                 <- mkConfigReg(False);

   rule shift_input(isValid(rw_inp.wget));
      let x = fromMaybe(?, rw_inp.wget);
      let dir = msb(count);

      if (dir == 0) begin
         // Row shift
         for(Integer i = 0; i < valueOf(n) - 1; i = i + 1) begin
            matrix[i] <= matrix[i + 1];
         end
         matrix[valueOf(n) - 1] <= x;

         if (startOutput) begin
            obuf <= matrix[0];
         end
      end
      else begin
         // Column shift
         for(Integer i = 0; i < valueOf(n); i = i + 1) begin
            matrix[i] <= shiftInAtN(matrix[i], x[i]);
         end

         if (startOutput) begin
            Vector#(n,a) tmp;
            for(Integer i = 0; i < valueOf(n); i = i + 1) begin
               tmp[i] = matrix[i][0];
            end
            obuf <= tmp;
         end
      end
      count <= count + 1;
   endrule

   rule do_start;
      if (count == '1) begin
         startOutput <= True;
      end
   endrule


   interface Put inp = toPut(rw_inp);
   interface Get oup = interface Get;
                          method ActionValue#(Vector#(n, a)) get() if (startOutput && isValid(rw_inp.wget));
                             return obuf;
                          endmethod
                       endinterface;
endmodule

(* synthesize *)
module mkTranspose32x32(ITranspose#(32, Bit#(8)));
   (* hide *) let _x <- mkTranspose;
   return _x;
endmodule
