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

