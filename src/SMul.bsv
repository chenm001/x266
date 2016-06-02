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

(* always_ready, always_enabled *)
interface SMUL#(numeric type width_a, numeric type width_b);
    method Bit#(TSub#(TAdd#(width_a,width_b),1)) c(Bit#(width_a) a, Bit#(width_b) b);
endinterface

import "BVI" SMUL =
   module vmkSMUL(SMUL#(width_a, width_b))
      provisos(Add#(1, width_c, TAdd#(width_a, width_b)));
      let width_a = valueOf(width_a);
      let width_b = valueOf(width_b);
      parameter WIDTH_A = width_a;
      parameter WIDTH_B = width_b;

      default_clock clk();
      default_reset rst();

      //method a(a) enable((*inhigh*) EN_a);
      //method b(b) enable((*inhigh*) EN_b);
      method c c(a, b);

      path(a, c);
      path(b, c);
      schedule(c) C (c);
   endmodule

//(* no_default_clock, no_default_reset *)
module cmkSMUL(SMUL#(width_a, width_b))
   provisos(Add#(1, width_c, TAdd#(width_a, width_b)));

   method Bit#(TSub#(TAdd#(width_a,width_b),1)) c(Bit#(width_a) a, Bit#(width_b) b);
      return truncate(pack(signedMul(unpack(a), unpack(b))));
   endmethod

endmodule

module mkSMUL(SMUL#(width_a, width_b))
   provisos(Add#(1, width_c, TAdd#(width_a, width_b)));

   SMUL#(width_a, width_b) _r;

   if (genC) begin
      _r <- liftModule(cmkSMUL);
   end
   else begin
      _r <- liftModule(vmkSMUL);
   end

   return _r;
endmodule : mkSMUL

