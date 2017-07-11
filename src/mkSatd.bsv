// Copyright (c) 2016-2017 Min Chen
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

import ConfigReg::*;
import Vector::*;
import GetPut::*;
import FIFOF::*;
import SpecialFIFOs::*;
import BUtils::*;

interface ISatd8;
   interface Put#(Vector#(8, Bit#(9))) inp;
   interface Get#(Vector#(1, Bit#(16))) oup;
endinterface

function Vector#(8, Bit#(na)) satd_1d(Vector#(8, Bit#(n)) x)
   provisos(
      Add#(n, 3, na)
   );
   Vector#(8, Bit#(na)) m0;
   Vector#(8, Bit#(na)) m1;
   Vector#(8, Bit#(na)) m2;

   m0[0] = sExtend(x[0]) + sExtend(x[4]);
   m0[1] = sExtend(x[1]) + sExtend(x[5]);
   m0[2] = sExtend(x[2]) + sExtend(x[6]);
   m0[3] = sExtend(x[3]) + sExtend(x[7]);
   m0[4] = sExtend(x[0]) - sExtend(x[4]);
   m0[5] = sExtend(x[1]) - sExtend(x[5]);
   m0[6] = sExtend(x[2]) - sExtend(x[6]);
   m0[7] = sExtend(x[3]) - sExtend(x[7]);

   m1[0] = m0[0] + m0[2];
   m1[1] = m0[1] + m0[3];
   m1[2] = m0[0] - m0[2];
   m1[3] = m0[1] - m0[3];
   m1[4] = m0[4] + m0[6];
   m1[5] = m0[5] + m0[7];
   m1[6] = m0[4] - m0[6];
   m1[7] = m0[5] - m0[7];

   m2[0] = m0[0] + m0[1];
   m2[1] = m0[0] - m0[1];
   m2[2] = m0[2] + m0[3];
   m2[3] = m0[2] - m0[3];
   m2[4] = m0[4] + m0[5];
   m2[5] = m0[4] - m0[5];
   m2[6] = m0[6] + m0[7];
   m2[7] = m0[6] - m0[7];

   return m2;
endfunction

(* synthesize *)
module mkSatd8(ISatd8);
   FIFOF#(Vector#(8, Bit#(9)))               fifo_inp     <- mkPipelineFIFOF;
   FIFOF#(Vector#(1, Bit#(16)))              fifo_out    <- mkPipelineFIFOF;

   RWire#(Vector#(8, Bit#(12)))              rw_trans    <- mkRWire;
   RWire#(Bool)                              rw_oflag    <- mkRWire;

   Reg#(Bit#(16))                            flags       <- mkConfigReg(0);
   Reg#(Bool)                                dir         <- mkConfigReg(True);
   Vector#(8, Reg#(Vector#(8, Bit#(12))))    matrix      <- replicateM(mkConfigRegU);

   rule shift_matrix(isValid(rw_trans.wget) || flags[15:8] != 0);
      let x = fromMaybe(?, rw_trans.wget);
      let y = isValid(rw_trans.wget) ? 1 : 0;
      
      if (dir) begin
         // Row shift
         for(Integer i = 0; i < 8 - 1; i = i + 1) begin
            matrix[i] <= matrix[i + 1];
         end
         matrix[8 - 1] <= x;
      end
      else begin
         // Column shift
         for(Integer i = 0; i < 8; i = i + 1) begin
            matrix[i] <= shiftInAtN(matrix[i], x[i]);
         end
      end

      let next_flags = flags * 2 + y;
      flags <= next_flags;
      if (next_flags == '1)
         dir <= !dir;
   endrule

   rule do_2d(flags[15:8] != 0);
      Vector#(8, Bit#(12)) tmp = ?;
      
      if (dir) begin
         tmp = matrix[0];
      end
      else begin
         for(Integer i = 0; i < 8; i = i + 1) begin
            tmp[i] = matrix[i][0];
         end
      end
      let x = satd_1d(tmp);
      Vector#(1, Bit#(16)) sum = unpack(0);

      for(Integer i = 0; i < 8; i = i + 1) begin
         Int#(15) v = unpack(x[i]);
         sum[0] = sum[0] + zExtend(pack(abs(v)));
      end

      fifo_out.enq(sum);
   endrule

   rule do_1d(fifo_inp.notEmpty());
      let x = fifo_inp.first;
      fifo_inp.deq;

      let y = satd_1d(x);

      rw_trans.wset(y);
   endrule

   interface Put inp = toPut(fifo_inp);
   interface Get oup = toGet(fifo_out);
endmodule
