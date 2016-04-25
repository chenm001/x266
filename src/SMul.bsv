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

