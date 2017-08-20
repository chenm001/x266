
import GetPut::*;
import BRAM::*;

import Types::*;
import MemTypes::*;
import RegFile::*;

module mkMemInitRegFile(RegFile#(Bit#(16), Data) mem, MemInitIfc ifc);
    Reg#(Bool) initialized <- mkReg(False);

    interface Put request;
        method Action put(MemInit x) if (!initialized);
          case (x) matches
            tagged InitLoad .l: begin
                mem.upd(truncate(l.addr), l.data);
            end

            tagged InitDone: begin
                initialized <= True;
            end
          endcase
        endmethod
    endinterface

    method Bool done() = initialized;

endmodule

module mkMemInitBRAM(BRAM1Port#(Bit#(16), Data) mem, MemInitIfc ifc);
    Reg#(Bool) initialized <- mkReg(False);

    interface Put request;
        method Action put(MemInit x) if (!initialized);
          case (x) matches
            tagged InitLoad .l: begin
                mem.portA.request.put(BRAMRequest {
                    write: True,
                    responseOnWrite: False,
                    address: truncate(l.addr),
                    datain: l.data});
            end

            tagged InitDone: begin
                initialized <= True;
            end
          endcase
        endmethod
    endinterface

    method Bool done() = initialized;

endmodule

module mkDummyMemInit(MemInitIfc);
    Reg#(Bool) initialized <- mkReg(False);

    interface Put request;
        method Action put(MemInit x) if (!initialized);
          case (x) matches
            tagged InitDone: begin
                initialized <= True;
            end
          endcase
        endmethod
    endinterface
    
    method Bool done() = initialized;

endmodule
