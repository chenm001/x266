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
import Connectable::*;
import ConfigReg::*;
import FIFOF::*;
import SpecialFIFOs::*;
import BUtils::*;
import BRAMCore::*;
import SMul::*;

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

interface IDct32;
   interface Put#(Vector#(2, Vector#(32, Bit#(9)))) io_in;
   interface Get#(Vector#(4, Bit#(16))) io_out;
endinterface

interface IDct32Core#(numeric type a);
   method Bit#(16) dct(Vector#(16, Bit#(a)) x, Bit#(5) idx);
endinterface

module mkDct32Core(IDct32Core#(a))
   provisos(
      Add#(_x, 9, a),
      Add#(0, 8, b),
      Add#(1, c, TAdd#(a, b)),
      Add#(c, 1, c1),
      Add#(c, 2, c2),
      Add#(a, 10/*(5-1+6)*/, bs),
      Add#(shift, 16, bs),
      Add#(0, TExp#(TSub#(shift, 1)), round)
   );

   Vector#(16, SMUL#(a,b)) smul <- replicateM(mkSMUL);

   method Bit#(16) dct(Vector#(16, Bit#(a)) x, Bit#(5) idx);
      Bit#(c) x0[16];
      for(Integer i = 0; i < 16; i = i + 1)
         x0[i] = smul[i].c(x[i], fromInteger(g_t32[idx][i]));

      Bit#(c1) x1[8];
      for(Integer i = 0; i < 8; i = i + 1)
         x1[i] = sExtend(x0[2 * i + 0]) + sExtend(x0[2 * i + 1]);

      Bit#(c2) x2[4];
      for(Integer i = 0; i < 4; i = i + 1)
         x2[i] = sExtend(x1[2 * i + 0]) + sExtend(x1[2 * i + 1]);

      Bit#(bs) x3[2];
      for(Integer i = 0; i < 2; i = i + 1)
         x3[i] = sExtend(x2[2 * i + 0]) + sExtend(x2[2 * i + 1]);

      Bit#(bs) sum = sExtend(x3[0]) + sExtend(x3[1]) + fromInteger(valueOf(round));

      return truncate(sum >> valueOf(shift));
   endmethod
endmodule

(* synthesize *)
module mkDct32(IDct32);
   FIFOF#(Vector#(2, Vector#(32, Bit#(10)))) fifo_in <- mkPipelineFIFOF;         // [E[0..15] O[0..15]]
   FIFOF#(Vector#(4, Bit#(16))) fifo_out <- mkPipelineFIFOF;


   Reg#(Bit#(16)) cycles <- mkReg(0);
   rule do_cycles;
      cycles <= cycles + 1;
   endrule

   // Internal
   Vector#(16, FIFOF#(Bit#(64)))  mem;
   for(Integer i = 0; i < 15; i = i + 1)
      mem[i] <- mkUGSizedFIFOF(16);
   mem[15] <- mkSizedFIFOF(16);

   Reg#(Bit#(8)) rowIdx <- mkConfigReg(0);
   Reg#(Bit#(4)) colCnt <- mkConfigReg(0);
   Reg#(Bit#(3)) colIdx <- mkConfigReg(0);

   Vector#(4, IDct32Core#(10)) dct_1D <- replicateM(mkDct32Core);
   Vector#(4, IDct32Core#(17)) dct_2D <- replicateM(mkDct32Core);
   FIFOF#(Vector#(32, Bit#(17))) fifo_col <- mkPipelineFIFOF;        // [E[0..15] O[0..15]]

   rule do_1D(fifo_in.notEmpty);
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

      mem[rowIdx[7:4]].enq({y3,y2,y1,y0});
      $display("[%6d] (%2d,%2d) %04X, %04X, %04X, %04X", cycles, rowIdx[7:4], rowIdx[3:0], y0, y1, y2, y3);

      rowIdx <= rowIdx + 1;
      if (rowIdx[3:0] == '1) begin
         fifo_in.deq;
      end
   endrule

   //(* no_implicit_conditions *)
   rule pre_col(mem[15].notEmpty);
      Vector#(32, Bit#(16)) x;
      Vector#(32, Bit#(17)) y;

      for(Integer i = 0; i < 32 / 2; i = i + 1) begin
         Vector#(4,Bit#(16)) t = unpack(mem[i].first());
         x[i*2+0] = t[{colCnt[0], 1'd0}];
         x[i*2+1] = t[{colCnt[0], 1'd1}];
      end

      if (True) begin
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
      if (colCnt[0] == 1) begin
         for(Integer i = 0; i < 16; i = i + 1) begin
            mem[i].deq;
         end
      end
   endrule

   rule do_2D;
      let x = fifo_col.first;

      if (True) begin
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

      fifo_out.enq(unpack({y3,y2,y1,y0}));
      $display("[%6d] %3d: %04X, %04X, %04X, %04X", cycles, colIdx, y0, y1, y2, y3);

      colIdx <= colIdx + 1;
      if (colIdx == '1) begin
         fifo_col.deq;
      end
   endrule

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
   FIFOF#(Vector#(4, Bit#(16))) fifo_out <- mkPipelineFIFOF;
   Reg#(Bit#(16)) cycles <- mkReg(0);
   Reg#(Bit#(9))  states <- mkReg(0);

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

   rule do_check(states >= 17 && states < 17+32*8);
      let x <- dct32_getDct;
      let y = fifo_out.first;

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
      $display("All test passed!");
      $finish;
   endrule

      //states <= (states == 17+32*8) ? 0 : states + 1;
endmodule
`endif
