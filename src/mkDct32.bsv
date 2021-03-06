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
import Connectable::*;
import ConfigReg::*;
import FIFOF::*;
import SpecialFIFOs::*;
import BUtils::*;
import BRAMCore::*;
import SMul::*;
import ARAM::*;
import Utils::*;

// Internal ROM Table
Integer g_t32[32][16] =
{
    { 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64 },
    { 90, 90, 88, 85, 82, 78, 73, 67, 61, 54, 46, 38, 31, 22, 13,  4 },
    { 90, 87, 80, 70, 57, 43, 25,  9, -9,-25,-43,-57,-70,-80,-87,-90 },
    { 90, 82, 67, 46, 22, -4,-31,-54,-73,-85,-90,-88,-78,-61,-38,-13 },
    { 89, 75, 50, 18,-18,-50,-75,-89,-89,-75,-50,-18, 18, 50, 75, 89 },
    { 88, 67, 31,-13,-54,-82,-90,-78,-46, -4, 38, 73, 90, 85, 61, 22 },
    { 87, 57,  9,-43,-80,-90,-70,-25, 25, 70, 90, 80, 43, -9,-57,-87 },
    { 85, 46,-13,-67,-90,-73,-22, 38, 82, 88, 54, -4,-61,-90,-78,-31 },
    { 83, 36,-36,-83,-83,-36, 36, 83, 83, 36,-36,-83,-83,-36, 36, 83 },
    { 82, 22,-54,-90,-61, 13, 78, 85, 31,-46,-90,-67,  4, 73, 88, 38 },
    { 80,  9,-70,-87,-25, 57, 90, 43,-43,-90,-57, 25, 87, 70, -9,-80 },
    { 78, -4,-82,-73, 13, 85, 67,-22,-88,-61, 31, 90, 54,-38,-90,-46 },
    { 75,-18,-89,-50, 50, 89, 18,-75,-75, 18, 89, 50,-50,-89,-18, 75 },
    { 73,-31,-90,-22, 78, 67,-38,-90,-13, 82, 61,-46,-88, -4, 85, 54 },
    { 70,-43,-87,  9, 90, 25,-80,-57, 57, 80,-25,-90, -9, 87, 43,-70 },
    { 67,-54,-78, 38, 85,-22,-90,  4, 90, 13,-88,-31, 82, 46,-73,-61 },
    { 64,-64,-64, 64, 64,-64,-64, 64, 64,-64,-64, 64, 64,-64,-64, 64 },
    { 61,-73,-46, 82, 31,-88,-13, 90, -4,-90, 22, 85,-38,-78, 54, 67 },
    { 57,-80,-25, 90, -9,-87, 43, 70,-70,-43, 87,  9,-90, 25, 80,-57 },
    { 54,-85, -4, 88,-46,-61, 82, 13,-90, 38, 67,-78,-22, 90,-31,-73 },
    { 50,-89, 18, 75,-75,-18, 89,-50,-50, 89,-18,-75, 75, 18,-89, 50 },
    { 46,-90, 38, 54,-90, 31, 61,-88, 22, 67,-85, 13, 73,-82,  4, 78 },
    { 43,-90, 57, 25,-87, 70,  9,-80, 80, -9,-70, 87,-25,-57, 90,-43 },
    { 38,-88, 73, -4,-67, 90,-46,-31, 85,-78, 13, 61,-90, 54, 22,-82 },
    { 36,-83, 83,-36,-36, 83,-83, 36, 36,-83, 83,-36,-36, 83,-83, 36 },
    { 31,-78, 90,-61,  4, 54,-88, 82,-38,-22, 73,-90, 67,-13,-46, 85 },
    { 25,-70, 90,-80, 43,  9,-57, 87,-87, 57, -9,-43, 80,-90, 70,-25 },
    { 22,-61, 85,-90, 73,-38, -4, 46,-78, 90,-82, 54,-13,-31, 67,-88 },
    { 18,-50, 75,-89, 89,-75, 50,-18,-18, 50,-75, 89,-89, 75,-50, 18 },
    { 13,-38, 61,-78, 88,-90, 85,-73, 54,-31,  4, 22,-46, 67,-82, 90 },
    {  9,-25, 43,-57, 70,-80, 87,-90, 90,-87, 80,-70, 57,-43, 25, -9 },
    {  4,-13, 22,-31, 38,-46, 54,-61, 67,-73, 78,-82, 85,-88, 90,-90 }
};

// External interface
interface IDct32;
   method Action setSize(Bit#(2) dsize);
   interface Put#(Vector#(2, Vector#(32, Bit#(9)))) io_in;
   interface Get#(Vector#(32, Bit#(16))) io_out;
endinterface

interface IDct32Core#(numeric type a);
   method Vector#(15, Bit#(16)) dct(Vector#(16, Bit#(a)) x, Bit#(5) idx);
endinterface


module mkDct32Core(IDct32Core#(a))
   provisos(
      Add#(0, 8, b),
      Add#(1, c, TAdd#(a, b)),
      Add#(c, 1, c1),
      Add#(c, 2, c2),
      Add#(a, 10/*(5-1+6)*/, bs),
      Add#(shift32, 16, bs),
      Add#(1, shift16, shift32),
      Add#(2, shift8, shift32),
      Add#(3, shift4, shift32),
      Add#(0, TExp#(TSub#(shift32, 1)), round),
      Add#(_x1, 9, a),
      Add#(_x2, 16, c),
      Add#(_x3, 16, c1),
      Add#(_x4, 16, c2)
   );

   Vector#(16, SMUL#(a,b)) smul <- replicateM(mkSMUL);

   method Vector#(15, Bit#(16)) dct(Vector#(16, Bit#(a)) x, Bit#(5) idx);
      Bit#(c) x0[16];
      for(Integer i = 0; i < 16; i = i + 1)
         x0[i] = smul[i].c(x[i], fromInteger(g_t32[idx][i]));

      // Export 4 of DCT4 result from here
      Bit#(c1) x1[8];
      for(Integer i = 0; i < 8; i = i + 1)
         x1[i] = sExtend(x0[2 * i + 0]) + sExtend(x0[2 * i + 1]);

      // Export DCT8 result from here
      Bit#(c2) x2[4];
      for(Integer i = 0; i < 4; i = i + 1)
         x2[i] = sExtend(x1[2 * i + 0]) + sExtend(x1[2 * i + 1]);

      // Export DCT16 result from here
      Bit#(bs) x3[2];
      for(Integer i = 0; i < 2; i = i + 1)
         x3[i] = sExtend(x2[2 * i + 0]) + sExtend(x2[2 * i + 1]);

      // Export DCT32 result from here
      Bit#(bs) x4 = sExtend(x3[0]) + sExtend(x3[1]);


      // Generate DCT{4,8,16,32} coeff
      Vector#(15, Bit#(16)) coeff;

      for(Integer i = 0; i < 8; i = i + 1)
         coeff[0 + i] = roundN(x0[i], valueOf(shift4));

      for(Integer i = 0; i < 4; i = i + 1)
         coeff[8 + i] = roundN(x1[i], valueOf(shift8));

      for(Integer i = 0; i < 2; i = i + 1)
         coeff[12 + i] = roundN(x2[i], valueOf(shift16));

      coeff[14] = roundN(x4, valueOf(shift32));

      return coeff;
   endmethod
endmodule

(* synthesize *)
module mkDct32(IDct32);
   FIFOF#(Vector#(2, Vector#(32, Bit#(10)))) fifo_in     <- mkPipelineFIFOF;         // [E[0..15] O[0..15]]
   FIFOF#(Vector#(32, Bit#(16)))             fifo_out    <- mkPipelineFIFOF;
   Reg#(Bit#(2))                             dct_size    <- mkReg(3);


   Reg#(Bit#(16)) cycles <- mkReg(0);
   rule do_cycles;
      cycles <= cycles + 1;
   endrule

   // Internal
   Vector#(16, BRAM_SDP_PORT#(5,64,6,32))  mem <- replicateM(mkRAM_Nx1);

   Reg#(Bit#(9)) rowIdx <- mkConfigReg(0);
   Reg#(Bit#(6)) colCnt <- mkConfigReg(0);
   Reg#(Bit#(3)) colIdx <- mkConfigReg(0);

   Vector#(4, IDct32Core#(10)) dct_1D <- replicateM(mkDct32Core);
   Vector#(4, IDct32Core#(17)) dct_2D <- replicateM(mkDct32Core);
   Wire#(Bit#(1)) w_syncCol <- mkDWire(0);
   Reg#(Bit#(2)) syncCol <- mkReg(0);
   FIFOF#(Vector#(32, Bit#(17))) fifo_col <- mkPipelineFIFOF;        // [E[0..15] O[0..15]]
   RWire#(Tuple3#(Bit#(2), Bit#(9), Vector#(32, Bit#(16)))) wr_1D <- mkRWire;


   rule write_bram_1D(wr_1D.wget matches tagged Valid .x);
      let sz  = tpl_1(x);
      let idx = tpl_2(x);
      let dat = tpl_3(x);

      case(sz)
         0: begin
           mem[{idx[0], 3'd0}].write(0, {dat[ 5], dat[ 4], dat[ 1], dat[ 0]});
           mem[{idx[0], 3'd1}].write(0, {dat[ 7], dat[ 6], dat[ 3], dat[ 2]});
           mem[{idx[0], 3'd2}].write(0, {dat[13], dat[12], dat[ 9], dat[ 8]});
           mem[{idx[0], 3'd3}].write(0, {dat[15], dat[14], dat[10], dat[11]});
           mem[{idx[0], 3'd4}].write(0, {dat[21], dat[20], dat[17], dat[16]});
           mem[{idx[0], 3'd5}].write(0, {dat[23], dat[22], dat[19], dat[18]});
           mem[{idx[0], 3'd6}].write(0, {dat[29], dat[28], dat[25], dat[24]});
           mem[{idx[0], 3'd7}].write(0, {dat[31], dat[30], dat[27], dat[26]});
         end

         1: begin
           mem[{idx[1:0], 2'd0}].write(0, {dat[ 3], dat[ 2], dat[ 1], dat[ 0]});
           mem[{idx[1:0], 2'd1}].write(0, {dat[ 7], dat[ 6], dat[ 5], dat[ 4]});
           mem[{idx[1:0], 2'd2}].write(0, {dat[11], dat[10], dat[ 9], dat[ 8]});
           mem[{idx[1:0], 2'd3}].write(0, {dat[15], dat[14], dat[13], dat[12]});
         end

         2: begin
           mem[{idx[2:0], 1'd0}].write({2'd0, idx[5:3]}, {dat[ 3], dat[ 2], dat[ 1], dat[ 0]});
           mem[{idx[2:0], 1'd1}].write({2'd0, idx[5:3]}, {dat[ 7], dat[ 6], dat[ 5], dat[ 4]});
         end

         default: begin
            mem[idx[7:4]].write({idx[8],idx[3:0]}, pack(take(dat)));
            $display("[%6d] (%2d,%2d) %04X, %04X, %04X, %04X", cycles, idx[7:4], idx[3:0], dat[0], dat[1], dat[2], dat[3]);
         end
      endcase
   endrule


   rule do_dct_1D(fifo_in.notEmpty);
      let x = fifo_in.first;

      /*
      if (rowIdx == 0) begin
         $write("E: ");
         for(Integer i = 0; i < 32 / 2; i = i + 1) begin
            Bit#(16) tmp = sExtend(x[0][i]);
            $write("%04X, ", tmp);
         end
         $write("\n");
      end
      */

      let y0 = dct_1D[0].dct(take    (x[0]), {rowIdx[3:0], 1'd0});
      let y1 = dct_1D[1].dct(take    (x[1]), {rowIdx[3:0], 1'd0});
      let y2 = dct_1D[2].dct(takeTail(x[0]), {rowIdx[3:0], 1'd1});
      let y3 = dct_1D[3].dct(takeTail(x[1]), {rowIdx[3:0], 1'd1});

      let idxTmpA = rowIdx[3:0];
      let idxTmpB = rowIdx[7:4];
      case(dct_size)
         0: begin
            Vector#(8, Bit#(16)) m0 = takeAt(0, y0);
            Vector#(8, Bit#(16)) m1 = takeAt(0, y1);
            Vector#(8, Bit#(16)) m2 = takeAt(0, y2);
            Vector#(8, Bit#(16)) m3 = takeAt(0, y3);
            wr_1D.wset(tuple3(dct_size, rowIdx, unpack({?, pack(m3), pack(m2), pack(m1), pack(m0)})));

            // Update Row & Col sync
            idxTmpA = idxTmpA | 4'b1110;
            idxTmpB = idxTmpB | 4'b1111;
         end

         1: begin
            Vector#(4, Bit#(16)) m0 = takeAt(8, y0);
            Vector#(4, Bit#(16)) m1 = takeAt(8, y1);
            Vector#(4, Bit#(16)) m2 = takeAt(8, y2);
            Vector#(4, Bit#(16)) m3 = takeAt(8, y3);
            wr_1D.wset(tuple3(dct_size, rowIdx, unpack({?, pack(m3), pack(m2), pack(m1), pack(m0)})));

            // Update Row & Col sync
            idxTmpA = idxTmpA | 4'b1100;
            idxTmpB = idxTmpB | 4'b1100;
         end

         2: begin
            Vector#(2, Bit#(16)) m0 = takeAt(12, y0);
            Vector#(2, Bit#(16)) m1 = takeAt(12, y1);
            Vector#(2, Bit#(16)) m2 = takeAt(12, y2);
            Vector#(2, Bit#(16)) m3 = takeAt(12, y3);
            wr_1D.wset(tuple3(dct_size, rowIdx, unpack({?, pack(m3), pack(m2), pack(m1), pack(m0)})));

            // Update Row & Col sync
            idxTmpA = idxTmpA | 4'b1000;
            idxTmpB = {2'b11, rowIdx[4:3]};
         end

         default: begin
            wr_1D.wset(tuple3(dct_size, rowIdx, unpack({?, y3[14], y2[14], y1[14], y0[14]})));
         end
      endcase

      // Update Row & Col sync
      if (idxTmpA == '1) begin
         fifo_in.deq;
      end
      if (idxTmpB == '1) begin
         w_syncCol <= 1;
      end
      rowIdx <= rowIdx + 1;
   endrule


   rule read_mem;
      for(Integer i = 0; i < 16; i = i + 1) begin
         mem[i].read(colCnt);
      end
      syncCol <= {syncCol[0], w_syncCol};
   endrule


   rule pre_2D({syncCol[1],colCnt[4:0]} != 0);
      Vector#(32, Bit#(16)) x;
      Vector#(32, Bit#(17)) y;

      for(Integer i = 0; i < 32 / 2; i = i + 1) begin
         Vector#(2,Bit#(16)) t = unpack(mem[i].read_resp());
         x[i*2+0] = t[0];
         x[i*2+1] = t[1];
      end

      if (False) begin
         $write("[%6d] INP-2D: ", cycles);
         for(Integer i = 0; i < 32; i = i + 1) begin
            $write("%04X, ", x[i]);
         end
         $write("\n");
         //$write("             -O: ");
         //for(Integer i = 0; i < 16; i = i + 1) begin
         //   $write("%04X, ", x[16+i]);
         //end
         $write("\n");
      end

      for(Integer i = 0; i < 32 / 2; i = i + 1) begin
         y[ 0 + i] = sExtend(x[i]) + sExtend(x[31 - i]);
         y[16 + i] = sExtend(x[i]) - sExtend(x[31 - i]);
      end

      fifo_col.enq(y);
      colCnt <= colCnt + 1;
   endrule


   rule do_dct_2D;
      let x = fifo_col.first;

      if (False) begin
         $write("[%6d] EO-2D: ", cycles);
         for(Integer i = 0; i < 32; i = i + 1) begin
            Bit#(16) t = truncate(x[i]);
            $write("%04X, ", t);
         end
         $write("\n");
      end

      let y0 = dct_2D[0].dct(take    (x), {colIdx, 2'd0});
      let y1 = dct_2D[1].dct(takeTail(x), {colIdx, 2'd1});
      let y2 = dct_2D[2].dct(take    (x), {colIdx, 2'd2});
      let y3 = dct_2D[3].dct(takeTail(x), {colIdx, 2'd3});

      case(dct_size)
         0: begin
            Vector#(8, Bit#(16)) m0 = takeAt(0, y0);
            Vector#(8, Bit#(16)) m1 = takeAt(0, y1);
            Vector#(8, Bit#(16)) m2 = takeAt(0, y2);
            Vector#(8, Bit#(16)) m3 = takeAt(0, y3);
            fifo_out.enq(unpack({?, pack(m3), pack(m2), pack(m1), pack(m0)}));
         end

         1: begin
            Vector#(4, Bit#(16)) m0 = takeAt(8, y0);
            Vector#(4, Bit#(16)) m1 = takeAt(8, y1);
            Vector#(4, Bit#(16)) m2 = takeAt(8, y2);
            Vector#(4, Bit#(16)) m3 = takeAt(8, y3);
            fifo_out.enq(unpack({?, pack(m3), pack(m2), pack(m1), pack(m0)}));
         end

         2: begin
            Vector#(2, Bit#(16)) m0 = takeAt(12, y0);
            Vector#(2, Bit#(16)) m1 = takeAt(12, y1);
            Vector#(2, Bit#(16)) m2 = takeAt(12, y2);
            Vector#(2, Bit#(16)) m3 = takeAt(12, y3);
            fifo_out.enq(unpack({?, pack(m3), pack(m2), pack(m1), pack(m0)}));
         end

         default: begin
            fifo_out.enq(unpack({?, y3[14], y2[14], y1[14], y0[14]}));
         end
      endcase
      //$display("[%6d] %3d: %04X, %04X, %04X, %04X", cycles, colIdx, y0, y1, y2, y3);

      colIdx <= colIdx + 1;
      if (colIdx == '1) begin
         fifo_col.deq;
      end
   endrule



   // External Interface
   method Action setSize(Bit#(2) dsize);
      dct_size <= dsize;
   endmethod

   interface Put io_in = interface Put; 
                            method Action put(Vector#(2, Vector#(32, Bit#(9))) x);
                               Vector#(2, Vector#(32, Bit#(10))) y = ?;

                               for(Integer i = 0; i < 32 / 2; i = i + 1) begin
                                  y[0][ 0 + i] = sExtend(x[0][i]) + sExtend(x[0][31 - i]);
                                  y[0][16 + i] = sExtend(x[0][i]) - sExtend(x[0][31 - i]);
                                  y[1][ 0 + i] = sExtend(x[1][i]) + sExtend(x[1][31 - i]);
                                  y[1][16 + i] = sExtend(x[1][i]) - sExtend(x[1][31 - i]);
                               end
                               fifo_in.enq(y);
                            endmethod
                         endinterface;

   interface Get io_out = toGet(fifo_out);
   
endmodule : mkDct32


`ifdef TEST_BENCH_mkDct32
import "BDPI" function Action dct32_genNew();
import "BDPI" function ActionValue#(Vector#(2, Vector#(32, Bit#(16)))) dct32_getDiff();
import "BDPI" function ActionValue#(Vector#(4, Bit#(16))) dct32_getDct();

(* synthesize *)
module mkTb(Empty);
   FIFOF#(Vector#(2, Vector#(32, Bit#(9)))) fifo_in <- mkPipelineFIFOF;
   FIFOF#(Vector#(32, Bit#(16))) fifo_out <- mkPipelineFIFOF;
   Reg#(Bit#(16)) cycles <- mkReg(0);
   Reg#(Bit#(9))  states <- mkReg(0);
   Reg#(Bit#(8))  passed <- mkReg(0);

   IDct32 dct32 <- mkDct32;

   mkConnection(toGet(fifo_in), dct32.io_in);
   mkConnection(dct32.io_out, toPut(fifo_out));

   rule do_cycles;
      cycles <= cycles + 1;
   endrule

   rule do_init(states == 0);
        dct32_genNew();
        $display("[%6d] Init DCT test data", cycles);
        states <= 1;
   endrule

   rule do_input(states >= 1 && states <= 16);
         let x <- dct32_getDiff;
         Vector#(2, Vector#(32, Bit#(9))) y;
         y[0] = map(truncate, x[0]);
         y[1] = map(truncate, x[1]);

         fifo_in.enq( y );

         $write("[%6d] ", cycles);
         for(Integer i = 0; i < 32; i = i + 1) begin
            $write("%04X, ", x[0][i]);
         end
         $write("\n         ");
         for(Integer i = 0; i < 32; i = i + 1) begin
            $write("%04X, ", x[1][i]);
         end
         $write("\n");
         states <= states + 1;
   endrule

   rule do_check_32(states >= 17 && states < 17+32*8);
      let x <- dct32_getDct;
      Vector#(4, Bit#(16)) y = take(fifo_out.first);

      if (x != y) begin
         $display("Verify failed: %X -> %X", x, y);
         $finish;
      end
      else begin
         $display("[%6d] (%3d) %X pass", cycles, states-17, x);
      end

      fifo_out.deq;
      states <= states + 1;
   endrule

   rule do_end(states >= 17+32*8);
      $display("[%d] All test passed!", passed);
      states <= 0;
      passed <= passed + 1;
      if (passed >= 10)
         $finish;
   endrule

      //states <= (states == 17+32*8) ? 0 : states + 1;
endmodule
`endif
