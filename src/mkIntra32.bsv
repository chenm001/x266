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

import BuildVector::*;
import Connectable::*;
import Vector::*;
import GetPut::*;
import ConfigReg::*;
import FIFOF::*;
import SpecialFIFOs::*;
import Utils::*;

typedef struct {
   Vector#(64, Bit#(8)) left;
   Vector#(65, Bit#(8)) top;
} IntraRef_t deriving(Bits);

typedef union tagged {
   void Decide;
   void Recon;
} IntraChannel_t deriving(Bits);

typedef struct {
   Bit#(6)           mode;
   IntraChannel_t    id;
   IntraRef_t        refs;
} IntraCmd_t deriving(Bits);

interface IIntra#(numeric type size);
   interface Put#(IntraCmd_t) io_in;
   interface Get#(Vector#(size, Bit#(8))) io_out;
endinterface

interface IIntra32;
   interface Put#(IntraRef_t) io_in;
   interface Get#(Vector#( 32, Bit#(8))) io_out;
endinterface


module mkIntra(IIntra#(size));
   FIFOF#(Vector#(size, Bit#(8)))                  fifo_o   <- mkPipelineFIFOF;

   Reg#(Maybe#(IntraChannel_t))                    id       <- mkReg(tagged Invalid);
   Reg#(Vector#(64, Bit#(8)))                      buff     <- mkRegU;
   Reg#(Bit#(6))                                   mode     <- mkRegU;
   Reg#(Bit#(8))                                   dcVal    <- mkRegU;
   Reg#(Bit#(31))                                  sflag    <- mkRegU;
   FIFOF#(Tuple2#(Bit#(6), Vector#(64, Bit#(8))))  fifo_s1  <- mkPipelineFIFOF;
   Reg#(Bit#(6))                                   rows     <- mkReg(0);

   Integer mapTbl[17][32] = {
      { 1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32},   // Mode  2, 26-34
      { 0,  1,  2,  3,  4,  4,  5,  6,  7,  8,  8,  9, 10, 11, 12, 13, 13, 14, 15, 16, 17, 17, 18, 19, 20, 21, 21, 22, 23, 24, 25, 26},   // Mode  3
      { 0,  1,  1,  2,  3,  3,  4,  5,  5,  6,  7,  7,  8,  9,  9, 10, 11, 11, 12, 13, 13, 14, 15, 15, 16, 17, 17, 18, 19, 19, 20, 21},   // Mode  4
      { 0,  1,  1,  2,  2,  3,  3,  4,  4,  5,  5,  6,  6,  7,  7,  8,  9,  9, 10, 10, 11, 11, 12, 12, 13, 13, 14, 14, 15, 15, 16, 17},   // Mode  5
      { 0,  0,  1,  1,  2,  2,  2,  3,  3,  4,  4,  4,  5,  5,  6,  6,  6,  7,  7,  8,  8,  8,  9,  9, 10, 10, 10, 11, 11, 12, 12, 13},   // Mode  6
      { 0,  0,  0,  1,  1,  1,  1,  2,  2,  2,  3,  3,  3,  3,  4,  4,  4,  5,  5,  5,  5,  6,  6,  6,  7,  7,  7,  7,  8,  8,  8,  9},   // Mode  7
      { 0,  0,  0,  0,  0,  0,  1,  1,  1,  1,  1,  1,  2,  2,  2,  2,  2,  2,  2,  3,  3,  3,  3,  3,  3,  4,  4,  4,  4,  4,  4,  5},   // Mode  8
      { 0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  2},   // Mode  9
      { 0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0},   //*Mode 10
      { 1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0},   // Mode 11
      { 4,  4,  4,  4,  4,  4,  3,  3,  3,  3,  3,  3,  2,  2,  2,  2,  2,  2,  2,  1,  1,  1,  1,  1,  1,  0,  0,  0,  0,  0,  0,  0},   // Mode 12
      { 8,  8,  8,  7,  7,  7,  7,  6,  6,  6,  5,  5,  5,  5,  4,  4,  4,  3,  3,  3,  3,  2,  2,  2,  1,  1,  1,  1,  0,  0,  0,  0},   // Mode 13
      {12, 12, 11, 11, 10, 10, 10,  9,  9,  8,  8,  8,  7,  7,  6,  6,  6,  5,  5,  4,  4,  4,  3,  3,  2,  2,  2,  1,  1,  0,  0,  0},   // Mode 14
      {16, 15, 15, 14, 14, 13, 13, 12, 12, 11, 11, 10, 10,  9,  9,  8,  7,  7,  6,  6,  5,  5,  4,  4,  3,  3,  2,  2,  1,  1,  0,  0},   // Mode 15
      {20, 19, 19, 18, 17, 17, 16, 15, 15, 14, 13, 13, 12, 11, 11, 10,  9,  9,  8,  7,  7,  6,  4,  5,  5,  3,  3,  2,  1,  1,  0,  0},   // Mode 16
      {25, 24, 23, 22, 21, 21, 20, 19, 18, 17, 17, 16, 15, 14, 13, 13, 12, 11, 10,  9,  8,  8,  7,  6,  5,  4,  4,  3,  2,  1,  0,  0},   // Mode 17
      {32, 31, 30, 29, 28, 27, 26, 25, 24, 23, 22, 21, 20, 19, 18, 17, 16, 15, 14, 13, 12, 11, 10,  9,  8,  7,  6,  5,  4,  3,  2,  1}    // Mode 18-25
   };

   Integer facTbl[16][32] = {
      { 0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0},   // Mode  2, 34, 18
      {26, 20, 14,  8,  2, 28, 22, 16, 10,  4, 30, 24, 18, 12,  6,  0, 26, 20, 14,  8,  2, 28, 22, 16, 10,  4, 30, 24, 18, 12,  6,  0},   // Mode  3, 33
      {21, 10, 31, 20,  9, 30, 19,  8, 29, 18,  7, 28, 17,  6, 27, 16,  5, 26, 15,  4, 25, 14,  3, 24, 13,  2, 23, 12,  1, 22, 11,  0},   // Mode  4, 32
      {17,  2, 19,  4, 21,  6, 23,  8, 25, 10, 27, 12, 29, 14, 31, 16,  1, 18,  3, 20,  5, 22,  7, 24,  9, 26, 11, 28, 13, 30, 15,  0},   // Mode  5, 31
      {13, 26,  7, 20,  1, 14, 27,  8, 21,  2, 15, 28,  9, 22,  3, 16, 29, 10, 23,  4, 17, 30, 11, 24,  5, 18, 31, 12, 25,  6, 19,  0},   // Mode  6, 30
      { 9, 18, 27,  4, 13, 22, 31,  8, 17, 26,  3, 12, 21, 30,  7, 16, 25,  2, 11, 20, 29,  6, 15, 24,  1, 10, 19, 28,  5, 14, 23,  0},   // Mode  7, 29
      { 5, 10, 15, 20, 25, 30,  3,  8, 13, 18, 23, 28,  1,  6, 11, 16, 21, 26, 31,  4,  9, 14, 19, 24, 29,  2,  7, 12, 17, 22, 27,  0},   // Mode  8, 28
      { 2,  4,  6,  8, 10, 12, 14, 16, 18, 20, 22, 24, 26, 28, 30,  0,  2,  4,  6,  8, 10, 12, 14, 16, 18, 20, 22, 24, 26, 28, 30,  0},   // Mode  9, 27
      { 0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0},   //*Mode 10, 26,  1
      {30, 28, 26, 24, 22, 20, 18, 16, 14, 12, 10,  8,  6,  4,  2,  0, 30, 28, 26, 24, 22, 20, 18, 16, 14, 12, 10,  8,  6,  4,  2,  0},   // Mode 11, 25
      {27, 22, 17, 12,  7,  2, 29, 24, 19, 14,  9,  4, 31, 26, 21, 16, 11,  6,  1, 28, 23, 18, 13,  8,  3, 30, 25, 20, 15, 10,  5,  0},   // Mode 12, 24
      {23, 14,  5, 28, 19, 10,  1, 24, 15,  6, 29, 20, 11,  2, 25, 16,  7, 30, 21, 12,  3, 26, 17,  8, 31, 22, 13,  4, 27, 18,  9,  0},   // Mode 13, 23
      {19,  6, 25, 12, 31, 18,  5, 24, 11, 30, 17,  4, 23, 10, 29, 16,  3, 22,  9, 28, 15,  2, 21,  8, 27, 14,  1, 20,  7, 26, 13,  0},   // Mode 14, 22
      {15, 30, 13, 28, 11, 26,  9, 24,  7, 22,  5, 20,  3, 18,  1, 16, 31, 14, 29, 12, 27, 10, 25,  8, 23,  6, 21,  4, 19,  2, 17,  0},   // Mode 15, 21
      {11, 22,  1, 12, 23,  2, 13, 24,  3, 14, 25,  4, 15, 26,  5, 16, 27,  6, 17, 28,  7, 18, 29,  8, 19, 30,  9, 20, 31, 10, 21,  0},   // Mode 16, 20
      { 6, 12, 18, 24, 30,  4, 10, 16, 22, 28,  2,  8, 14, 20, 26,  0,  6, 12, 18, 24, 30,  4, 10, 16, 22, 28,  2,  8, 14, 20, 26,  0}    // Mode 17, 19
   };

   Bit#(1) mapShift[16][31] = {   // 0-Keep, 1-Shift
   //  0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,
      {1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1},     // Mode 34
      {1, 1, 1, 1, 0, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 0, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1},     // Mode 33
      {1, 0, 1, 1, 0, 1, 1, 0, 1, 1, 0, 1, 1, 0, 1, 1, 0, 1, 1, 0, 1, 1, 0, 1, 1, 0, 1, 1, 0, 1, 1},     // Mode 32
      {1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 1},     // Mode 31
      {0, 1, 0, 1, 0, 0, 1, 0, 1, 0, 0, 1, 0, 1, 0, 0, 1, 0, 1, 0, 0, 1, 0, 1, 0, 0, 1, 0, 1, 0, 1},     // Mode 30
      {0, 0, 1, 0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 1, 0, 0, 1},     // Mode 29
      {0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1},     // Mode 28
      {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1},     // Mode 27
      {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},     // Mode 26,  1
      {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},     // Mode 25
      {0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0},     // Mode 24
      {0, 0, 1, 0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0},     // Mode 23
      {0, 1, 0, 1, 0, 0, 1, 0, 1, 0, 0, 1, 0, 1, 0, 0, 1, 0, 1, 0, 0, 1, 0, 1, 0, 0, 1, 0, 1, 0, 0},     // Mode 22
      {1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0},     // Mode 21
      {1, 0, 1, 1, 0, 1, 1, 0, 1, 1, 0, 1, 1, 0, 1, 1, 0, 1, 1, 0, 1, 1, 0, 1, 1, 0, 1, 1, 0, 1, 0},     // Mode 20
      {1, 1, 1, 1, 0, 1, 1, 1, 1, 0, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 0, 1, 1, 1, 1, 0}      // Mode 19
   };


   function Vector#(64, Bit#(8)) getRefPixels(Bit#(6) xMode, Vector#(65, Bit#(8)) xL, Vector#(65, Bit#(8)) xT);
      let y = ?;
      case(xMode)
         // --------------------------- Horizon ---------------------------
          0, 1, 2, 3, 4, 5, 6, 7, 8: begin
            y = unpack({?,         xL[64], xL[63], xL[62], xL[61], xL[60], xL[59], xL[58], 
                           xL[57], xL[56], xL[55], xL[54], xL[53], xL[52], xL[51], xL[50],
                           xL[49], xL[48], xL[47], xL[46], xL[45], xL[44], xL[43], xL[42],
                           xL[41], xL[40], xL[39], xL[38], xL[37], xL[36], xL[35], xL[34],
                                                                                   xL[33],
                           xL[32], xL[31], xL[30], xL[29], xL[28], xL[27], xL[26], xL[25],
                           xL[24], xL[23], xL[22], xL[21], xL[20], xL[19], xL[18], xL[17],
                           xL[16], xL[15], xL[14], xL[13], xL[12], xL[11], xL[10], xL[ 9],
                           xL[ 8], xL[ 7], xL[ 6], xL[ 5], xL[ 4], xL[ 3], xL[ 2], xL[ 1]
                        });
         end
          9: begin
            y = unpack({?,                                                         xT[32],
                           xL[31], xL[30], xL[29], xL[28], xL[27], xL[26], xL[25], xL[24],
                           xL[23], xL[22], xL[21], xL[20], xL[19], xL[18], xL[17], xL[16],
                           xL[15], xL[14], xL[13], xL[12], xL[11], xL[10], xL[ 9], xL[ 8],
                           xL[ 7], xL[ 6], xL[ 5], xL[ 4], xL[ 3], xL[ 2], xL[ 1], xL[ 0],
                           xT[16]
                        });
         end
        10: begin
            y = unpack({?,                                                         xT[32],
                           xL[31], xL[30], xL[29], xL[28], xL[27], xL[26], xL[25], xL[24],
                           xL[23], xL[22], xL[21], xL[20], xL[19], xL[18], xL[17], xL[16],
                           xL[15], xL[14], xL[13], xL[12], xL[11], xL[10], xL[ 9], xL[ 8],
                           xL[ 7], xL[ 6], xL[ 5], xL[ 4], xL[ 3], xL[ 2], xL[ 1], xL[ 0],
                           xT[ 6], xT[13], xT[19], xT[26]
                        });
         end
        11: begin
            y = unpack({?,                                                         xT[32],
                           xL[31], xL[30], xL[29], xL[28], xL[27], xL[26], xL[25], xL[24],
                           xL[23], xL[22], xL[21], xL[20], xL[19], xL[18], xL[17], xL[16],
                           xL[15], xL[14], xL[13], xL[12], xL[11], xL[10], xL[ 9], xL[ 8],
                           xL[ 7], xL[ 6], xL[ 5], xL[ 4], xL[ 3], xL[ 2], xL[ 1], xL[ 0],
                           xT[ 4], xT[ 7], xT[11], xT[14], xT[18], xT[21], xT[25], xT[28]
                        });
         end
        12: begin
            y = unpack({?,                                                         xT[32],
                           xL[31], xL[30], xL[29], xL[28], xL[27], xL[26], xL[25], xL[24],
                           xL[23], xL[22], xL[21], xL[20], xL[19], xL[18], xL[17], xL[16],
                           xL[15], xL[14], xL[13], xL[12], xL[11], xL[10], xL[ 9], xL[ 8],
                           xL[ 7], xL[ 6], xL[ 5], xL[ 4], xL[ 3], xL[ 2], xL[ 1], xL[ 0],
                           xT[ 2], xT[ 5], xT[ 7], xT[10], xT[12], xT[15], xT[17], xT[20],
                           xT[22], xT[25], xT[27], xT[30]
                        });
         end
        13: begin
            y = unpack({?,                                                         xT[32],
                           xL[31], xL[30], xL[29], xL[28], xL[27], xL[26], xL[25], xL[24],
                           xL[23], xL[22], xL[21], xL[20], xL[19], xL[18], xL[17], xL[16],
                           xL[15], xL[14], xL[13], xL[12], xL[11], xL[10], xL[ 9], xL[ 8],
                           xL[ 7], xL[ 6], xL[ 5], xL[ 4], xL[ 3], xL[ 2], xL[ 1], xL[ 0],
                           xT[ 2], xT[ 4], xT[ 6], xT[ 8], xT[ 9], xT[11], xT[13], xT[15],
                           xT[17], xT[19], xT[21], xT[23], xT[24], xT[26], xT[28], xT[30]
                        });
         end
        14: begin
            y = unpack({?,                                                         xT[32],
                           xL[31], xL[30], xL[29], xL[28], xL[27], xL[26], xL[25], xL[24],
                           xL[23], xL[22], xL[21], xL[20], xL[19], xL[18], xL[17], xL[16],
                           xL[15], xL[14], xL[13], xL[12], xL[11], xL[10], xL[ 9], xL[ 8],
                           xL[ 7], xL[ 6], xL[ 5], xL[ 4], xL[ 3], xL[ 2], xL[ 1], xL[ 0],
                           xT[ 2], xT[ 3], xT[ 5], xT[ 6], xT[ 8], xT[ 9], xT[11], xT[12],
                           xT[14], xT[15], xT[17], xT[18], xT[20], xT[21], xT[23], xT[24],
                           xT[26], xT[27], xT[29], xT[30]
                        });
         end
        15: begin
            y = unpack({?,                                                         xT[32],
                           xL[31], xL[30], xL[29], xL[28], xL[27], xL[26], xL[25], xL[24],
                           xL[23], xL[22], xL[21], xL[20], xL[19], xL[18], xL[17], xL[16],
                           xL[15], xL[14], xL[13], xL[12], xL[11], xL[10], xL[ 9], xL[ 8],
                           xL[ 7], xL[ 6], xL[ 5], xL[ 4], xL[ 3], xL[ 2], xL[ 1], xL[ 0],
                           xT[ 1], xT[ 2], xT[ 4], xT[ 5], xT[ 6], xT[ 7], xT[ 9], xT[10],
                           xT[11], xT[12], xT[14], xT[15], xT[16], xT[17], xT[18], xT[20],
                           xT[21], xT[22], xT[23], xT[25], xT[26], xT[27], xT[28], xT[30],
                           xT[31]
                        });
         end

         // --------------------------- Vertical ---------------------------
        16: begin
            y = unpack({?,         xL[31], xL[30], xL[29], xL[28], xL[27], xL[26], xL[25],
                           xL[24], xL[23], xL[22], xL[21], xL[20], xL[19], xL[18], xL[17],
                           xL[16], xL[15], xL[14], xL[13], xL[12], xL[11], xL[10], xL[ 9],
                           xL[ 8], xL[ 7], xL[ 6], xL[ 5], xL[ 4], xL[ 3], xL[ 2], xL[ 1],
                                                                                   xT[ 0],
                           xT[ 1], xT[ 2], xT[ 3], xT[ 4], xT[ 5], xT[ 6], xT[ 7], xT[ 8],
                           xT[ 9], xT[10], xT[11], xT[12], xT[13], xT[14], xT[15], xT[16],
                           xT[17], xT[18], xT[19], xT[20], xT[21], xT[22], xT[23], xT[24],
                           xT[25], xT[26], xT[27], xT[28], xT[29], xT[30], xT[31], xT[32]
                        });
         end
        17: begin
            y = unpack({?,                                                         xL[31],
                           xL[30], xL[28], xL[27], xL[26], xL[25], xL[23], xL[22], xL[21],
                           xL[20], xL[18], xL[17], xL[16], xL[15], xL[14], xL[12], xL[11],
                           xL[10], xL[ 9], xL[ 7], xL[ 6], xL[ 5], xL[ 4], xL[ 2], xL[ 1],
                                                                                   xT[ 0],
                           xT[ 1], xT[ 2], xT[ 3], xT[ 4], xT[ 5], xT[ 6], xT[ 7], xT[ 8],
                           xT[ 9], xT[10], xT[11], xT[12], xT[13], xT[14], xT[15], xT[16],
                           xT[17], xT[18], xT[19], xT[20], xT[21], xT[22], xT[23], xT[24],
                           xT[25], xT[26], xT[27], xT[28], xT[29], xT[30], xT[31], xT[32]
                        });
         end
        18: begin
            y = unpack({?,                                 xL[30], xL[29], xL[27], xL[26],
                           xL[24], xL[23], xL[21], xL[20], xL[18], xL[17], xL[15], xL[14],
                           xL[12], xL[11], xL[ 9], xL[ 8], xL[ 6], xL[ 5], xL[ 3], xL[ 2],
                                                                                   xT[ 0],
                           xT[ 1], xT[ 2], xT[ 3], xT[ 4], xT[ 5], xT[ 6], xT[ 7], xT[ 8],
                           xT[ 9], xT[10], xT[11], xT[12], xT[13], xT[14], xT[15], xT[16],
                           xT[17], xT[18], xT[19], xT[20], xT[21], xT[22], xT[23], xT[24],
                           xT[25], xT[26], xT[27], xT[28], xT[29], xT[30], xT[31], xT[32]
                        });
         end
        19: begin
            y = unpack({?, xL[30], xL[28], xL[26], xL[24], xL[23], xL[21], xL[19], xL[17],
                           xL[15], xL[13], xL[11], xL[ 9], xL[ 8], xL[ 6], xL[ 4], xL[ 2],
                                                                                   xT[ 0],
                           xT[ 1], xT[ 2], xT[ 3], xT[ 4], xT[ 5], xT[ 6], xT[ 7], xT[ 8],
                           xT[ 9], xT[10], xT[11], xT[12], xT[13], xT[14], xT[15], xT[16],
                           xT[17], xT[18], xT[19], xT[20], xT[21], xT[22], xT[23], xT[24],
                           xT[25], xT[26], xT[27], xT[28], xT[29], xT[30], xT[31], xT[32]
                        });
         end
        20: begin
            y = unpack({?,                                 xL[30], xL[27], xL[25], xL[22],
                           xL[20], xL[17], xL[15], xL[12], xL[10], xL[ 7], xL[ 5], xL[ 2],
                                                                                   xT[ 0],
                           xT[ 1], xT[ 2], xT[ 3], xT[ 4], xT[ 5], xT[ 6], xT[ 7], xT[ 8],
                           xT[ 9], xT[10], xT[11], xT[12], xT[13], xT[14], xT[15], xT[16],
                           xT[17], xT[18], xT[19], xT[20], xT[21], xT[22], xT[23], xT[24],
                           xT[25], xT[26], xT[27], xT[28], xT[29], xT[30], xT[31], xT[32]
                        });
         end
        21: begin
            y = unpack({?, xL[28], xL[25], xL[21], xL[18], xL[14], xL[11], xL[ 7], xL[ 4],
                                                                                   xT[ 0],
                           xT[ 1], xT[ 2], xT[ 3], xT[ 4], xT[ 5], xT[ 6], xT[ 7], xT[ 8],
                           xT[ 9], xT[10], xT[11], xT[12], xT[13], xT[14], xT[15], xT[16],
                           xT[17], xT[18], xT[19], xT[20], xT[21], xT[22], xT[23], xT[24],
                           xT[25], xT[26], xT[27], xT[28], xT[29], xT[30], xT[31], xT[32]
                        });
         end
        22: begin
            y = unpack({?,                                 xL[26], xL[19], xL[13], xL[ 6],
                                                                                   xT[ 0],
                           xT[ 1], xT[ 2], xT[ 3], xT[ 4], xT[ 5], xT[ 6], xT[ 7], xT[ 8],
                           xT[ 9], xT[10], xT[11], xT[12], xT[13], xT[14], xT[15], xT[16],
                           xT[17], xT[18], xT[19], xT[20], xT[21], xT[22], xT[23], xT[24],
                           xT[25], xT[26], xT[27], xT[28], xT[29], xT[30], xT[31], xT[32]
                        });
         end
        23: begin
            y = unpack({?,                                                         xL[16],
                                                                                   xT[ 0],
                           xT[ 1], xT[ 2], xT[ 3], xT[ 4], xT[ 5], xT[ 6], xT[ 7], xT[ 8],
                           xT[ 9], xT[10], xT[11], xT[12], xT[13], xT[14], xT[15], xT[16],
                           xT[17], xT[18], xT[19], xT[20], xT[21], xT[22], xT[23], xT[24],
                           xT[25], xT[26], xT[27], xT[28], xT[29], xT[30], xT[31], xT[32]
                        });
         end
        24, 25, 26, 27, 28, 29, 30, 31, 32: begin
            y = unpack({?,         xT[64], xT[63], xT[62], xT[61], xT[60], xT[59], xT[58],
                           xT[57], xT[56], xT[55], xT[54], xT[53], xT[52], xT[51], xT[50],
                           xT[49], xT[48], xT[47], xT[46], xT[45], xT[44], xT[43], xT[42],
                           xT[41], xT[40], xT[39], xT[38], xT[37], xT[36], xT[35], xT[34],
                                                                                   xT[33],
                           xT[32], xT[31], xT[30], xT[29], xT[28], xT[27], xT[26], xT[25],
                           xT[24], xT[23], xT[22], xT[21], xT[20], xT[19], xT[18], xT[17],
                           xT[16], xT[15], xT[14], xT[13], xT[12], xT[11], xT[10], xT[ 9],
                           xT[ 8], xT[ 7], xT[ 6], xT[ 5], xT[ 4], xT[ 3], xT[ 2], xT[ 1]
                        });
         end
        33: begin
            Bit#(8) dc = truncate(dcVal >> 6);
            y = unpack({?, dc, dc, dc, dc, dc, dc, dc, dc,
                           dc, dc, dc, dc, dc, dc, dc, dc,
                           dc, dc, dc, dc, dc, dc, dc, dc,
                           dc, dc, dc, dc, dc, dc, dc, dc
                        });
         end
      endcase

      return y;
   endfunction

   rule stage1(isValid(id));
      Vector#(33, Bit#(8)) x0 = take(buff);
      Vector#(33, Bit#(8)) x1 = (sflag[0] == 1) ? takeAt(1, buff) : x0;
      Vector#(32, Bit#(8)) y = ?;
      Vector#(32, Bit#(8)) z = ?;

      // Mapping to correct order
      for(Integer i = 0; i < 32; i = i + 1) begin
         let idx = mapTbl[mode][i];
         y[i] = x0[idx];
         z[i] = x1[idx];
      end

      buff <= shiftInAtN(buff, ?);
      sflag <= (sflag >> 1);
      rows <= rows + 1;
      if (rows == ~0)
         id <= tagged Invalid;

      fifo_s1.enq(tuple2(rows, append(y, z)));
   endrule

   rule stage2;
      let x = fifo_s1.first;
      let r = tpl_1(x);
      let v = tpl_2(x);
      fifo_s1.deq;

      Vector#(32, Bit#(8)) z = ?;
      Vector#(2, Vector#(32, Bit#(8))) y = unpack(pack(x));
      let fac = facTbl[mode][r];

      for(Integer i = 0; i < 32; i = i + 1) begin
         Bit#(13) tmp = zExtend(v[0][i]) * zExtend(32 - fac) + zExtend(v[1][i]) * zExtend(fac) + 16;
         z[i] = truncate(tmp >> 6);
      end

      fifo_o.enq(z);
   endrule


   interface io_in  =  interface Put; 
                           method Action put(IntraCmd_t x) if (!isValid(id));
                              id <= tagged Valid x.id;

                              let xL = cons(?, x.refs.left);
                              let xT = x.refs.top;
                              let xMode = x.mode;

                              buff <= getRefPixels(x.mode, xL, xT);
                              mode <= xMode;

                              Bit#(31) flags = ?;
                              for(Integer i = 0; i < 32; i = i + 1) begin
                                 flags[i] = mapShift[xMode][i];
                              end
                              sflag <= flags;

                              Bit#(14) sum = 0;
                              for(Integer i = 0; i < 32; i = i + 1) begin
                                 sum = sum + zeroExtend(xL[1+i]) + zeroExtend(xT[1+i]);
                              end
                              dcVal <= truncate(sum >> 6);
                           endmethod
                      endinterface;

   interface io_out = toGet(fifo_o);
endmodule


(* synthesize *)
module mkIntra32(IIntra32);
   FIFOF#(IntraRef_t)                              fifo_i   <- mkPipelineFIFOF;
   FIFOF#(Vector#(32, Bit#(8)))                    fifo_o   <- mkPipelineFIFOF;
   Reg#(Bit#(6))                                   mode     <- mkReg(0);            // 0-Mode2, 1-Mode3..., 15-Mode17, 16-Mode34, 17-Mode33..., 32-Mode18

   IIntra#(32) dut <- mkIntra;

`ifdef XXX
   Reg#(Maybe#(Vector#(64, Bit#(8))))              stage0   <- mkReg(tagged Invalid);
   FIFOF#(Tuple2#(Bit#(6), Vector#(64, Bit#(8))))  fifo_s1  <- mkPipelineFIFOF;
   FIFOF#(Tuple3#(Bit#(4), Vector#(32, Bit#( 8)), Vector#(32, Bit#( 8)))) stage1 <- mkPipelineFIFOF;
   FIFOF#(Vector#(2, (Vector#(32, Bit#(13)))))     stage2   <- mkPipelineFIFOF;
   Reg#(Vector#(32, Bit#(8)))                      pixd1    <- mkRegU;
   Reg#(Bit#(6))                                   cnt1     <- mkReg(0);
`endif

   Reg#(Bit#(8))                                   dcVal    <- mkRegU;

   Integer chen_debug = 0;

`ifdef XXX
   rule do_stage0;
      let x = fifo_i.first;
      fifo_i.deq;
      let xL = cons(?, x.left);
      let xT = x.top;
      Vector#(64, Bit#(8)) y = ?;


      mode   <= mode + 1;

      fifo_s1.enq(tuple2(mode, y));

      if (chen_debug == 1) begin
         if (mode == 2)
            $finish;
      end
   endrule

   rule do_stage1(fifo_s1.notEmpty());
      let x = fifo_s1.first;
      fifo_s1.deq;

      Vector#(33, Bit#(8)) y0 = take(x);
      Vector#(31, Bit#(8)) y1 = takeAt(33, x);
      let modeX = moded1[3:0];
      let fac = facTbl[modeX][cnt1];

      Vector#(32, Bit#(8)) m = take    (y0);
      Vector#(32, Bit#(8)) n = takeTail(y0);
      let shiftIdx = modeX;

      if (moded1 < 16) begin
         m = pixd1;
         for(Integer i = 0; i < 32; i = i + 1) begin
            n[i] = x[mapTbl[modeX][i]];
         end
         shiftIdx = 0;
      end

      pixd1 <= n;

      // Prediction
      if (cnt1[5] != 0) begin
         stage1.enq(tuple3(modeX, m, n));
      end

      // next row
      cnt1 <= cnt1 + 1;
      if (cnt1 == 31) begin
         stage0 <= tagged Invalid;
      end
      else if (mapShift[shiftIdx][cnt1] == 1) begin
         y0 = ((moded1 >= 24 && moded1 <= 31) ? shiftInAt0(y0, y1[0]) : shiftInAtN(y0, y1[0]));
         y1 = shiftOutFrom0(?, y1, 1);
         stage0 <= tagged Valid append(y0, y1);
      end
   endrule


   rule do_stage2(stage1.notEmpty);
      let x = stage1.first;
      stage1.deq;

      let modeX = tpl_1(x);
      let y0 = tpl_2(x);
      let y1 = tpl_3(x);

      Vector#(32, Bit#(13)) xm;
      Vector#(32, Bit#(13)) xn;
      for(Integer i = 0; i < 32; i = i + 1) begin
         let fac = facTbl[modeX][i];
         xm[i] = (fromInteger(32 - fac) * zeroExtend(y0[i]));
         xn[i] = (fromInteger(     fac) * zeroExtend(y1[i]));
      end
      stage2.enq(vec(xm, xn));
   endrule

   rule do_stage3;
      let x = stage2.first;
      Vector#(32, Bit#(8)) y;
      for(Integer i = 0; i < 32; i = i + 1) begin
         y[i] = roundN((x[0][i] + x[1][i]), 5);
      end
      fifo_o.enq(y);
   endrule
`endif


   interface io_in  =  interface Put; 
                           method Action put(IntraRef_t x);
                              fifo_i.enq(x);
                              Bit#(14) sum = 0;
                              for(Integer i = 0; i < 32; i = i + 1) begin
                                 sum = sum + zeroExtend(x.left[i]) + zeroExtend(x.top[1+i]);
                              end
                              dcVal <= truncate(sum >> 6);
                           endmethod
                      endinterface;
   interface io_out = toGet(fifo_o);
endmodule : mkIntra32



`ifdef TEST_BENCH_mkIntra32
(* synthesize *)
module mkTb(Empty);
   FIFOF#(IntraRef_t)   fifo_in  <- mkPipelineFIFOF;
   Reg#(Bit#(16))       cycles   <- mkReg(0);
   IIntra32             dut      <- mkIntra32;

   mkConnection(toGet(fifo_in), dut.io_in);

   rule do_cycles;
      cycles <= cycles + 1;
   endrule

   rule do_input;
      Vector#(64, Bit#(8)) xL;
      Vector#(65, Bit#(8)) xT;
      for(Integer i=0; i<64; i=i+1)
         xL[i] = fromInteger(-(i+1));

      for(Integer i=0; i<65; i=i+1)
         xT[i] = fromInteger(i);

      fifo_in.enq(IntraRef_t {left:xL, top:xT});
   endrule


endmodule

`endif
