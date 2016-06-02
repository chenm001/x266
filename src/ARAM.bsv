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

import BRAMCore::*;
import RegFile::*;
import Vector::*;

interface BRAM_SDP_PORT#(numeric type aa, numeric type da, numeric type ab, numeric type db);
   (* always_ready *) method Action write(Bit#(aa) addr, Bit#(da) din);
   (* always_ready *) method Action read(Bit#(ab) addr);
   (* always_ready *) method Bit#(db) read_resp();
endinterface

// asymmetric_ram
import "BVI" RAM_Nx1 =
   module vmkRAM_Nx1(BRAM_SDP_PORT#(aa,da,ab,db))
      provisos(
         Add#(0, TExp#(aa), sa),
         Add#(0, TExp#(ab), sb)
      );
      parameter ADDRWIDTHA  = valueOf(aa);
      parameter WIDTHA      = valueOf(da);
      parameter SIZEA       = valueOf(sa);
      parameter ADDRWIDTHB  = valueOf(ab);
      parameter WIDTHB      = valueOf(db);
      parameter SIZEB       = valueOf(sb);

      default_clock clkA(clkA);
      input_clock clkB(clkB) <- exposeCurrentClock;
      default_reset no_reset;

      method write(addrA, diA) enable(weA);
      method read(addrB) clocked_by(clkB) enable(reB);
      method doB read_resp() clocked_by(no_clock);


      schedule(write) CF (read,read_resp);
      schedule(read,read_resp) CF (write);
      schedule(read_resp) CF (read_resp);
      schedule(read_resp) CF (read);
      schedule(write) C (write);
      schedule(read) C (read);
   endmodule

module cmkRAM_Nx1(BRAM_SDP_PORT#(aa,da,ab,db))
   provisos(
      Div#(da, db, ratio),
      Add#(lsb, aa, ab),
      Bits#(Vector#(ratio,Bit#(db)), da)
   );

   RegFile#(Bit#(aa), Bit#(db)) _mem[valueOf(ratio)];
   Reg#(Bit#(ab)) raddr <- mkRegU;

   for(Integer i = 0; i < valueOf(ratio); i = i + 1) begin
      _mem[i] <- mkRegFileFull;
   end

   method Action write(Bit#(aa) addr, Bit#(da) din);
      Vector#(ratio, Bit#(db)) x = unpack(din);

      for(Integer i = 0; i < valueOf(ratio); i = i + 1) begin
         _mem[i].upd(addr, x[i]);
      end
   endmethod

   method Action read(Bit#(ab) addr);
      raddr <= addr;
   endmethod

   method Bit#(db) read_resp();
      Bit#(lsb) idx = raddr[valueOf(lsb)-1:0];
      Bit#(db) x = _mem[idx].sub(truncate(raddr>>valueOf(lsb)));
      return x;
   endmethod
endmodule : cmkRAM_Nx1


module mkRAM_Nx1(BRAM_SDP_PORT#(aa,da,ab,db))
   provisos(
      Div#(da, db, ratio),
      Add#(lsb, aa, ab),
      Bits#(Vector#(ratio,Bit#(db)), da)
   );

   BRAM_SDP_PORT#(aa,da,ab,db) _r;

   if (genC) begin
      _r <- liftModule(cmkRAM_Nx1);
   end
   else begin
      _r <- liftModule(vmkRAM_Nx1);
   end

   return _r;
endmodule : mkRAM_Nx1

/*
(* synthesize *)
module mkARAM64x16(BRAM_SDP_PORT#(8, 64, 10, 16));
   (* hide *) let _mem <- mkRAM_Nx1;

   return _mem;
endmodule
*/
