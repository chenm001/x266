// Copyright (c) 2016 Min Chen
//
// Permission is hereby granted, free of charge, to any person obtaining a
// copy of this software and associated documentation files (the "Software"),
// to deal in the Software without restriction, including without limitation
// the rights to use, copy, modify, merge, publish, distribute, sublicense,
// and/or sell copies of the Software, and to permit persons to whom the
// Software is furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included
// in all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
// OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
// THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
// FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
// DEALINGS IN THE SOFTWARE.
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
