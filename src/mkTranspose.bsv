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
      Log#(n, cntBits)
   );

   RWire#(Vector#(n, a)) rw_inp           <- mkRWire;
   Reg#(Vector#(n, a)) obuf               <- mkRegU;

   Vector#(n, Reg#(Vector#(n, a))) matrix <- replicateM(mkRegU);
   Reg#(Bit#(cntBits)) count              <- mkReg(0);
   Reg#(Bool) dir                         <- mkConfigReg(False);
   Reg#(Bool) startOutput                 <- mkConfigReg(False);

   rule shift_input(isValid(rw_inp.wget));
      let x = fromMaybe(?, rw_inp.wget);

      if (!dir) begin
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
         dir <= !dir;
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
