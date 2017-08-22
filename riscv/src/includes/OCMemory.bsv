import Types::*;
import MemTypes::*;
import RegFile::*;

interface OCMemory;
    method MemResp ireq(Addr a);
    method ActionValue#(MemResp) dreq(MemReq x);
endinterface

(* synthesize *)
module mkMemory(OCMemory);
    // This is Video Processing MCU
`ifdef SIM
    RegFile#(Bit#(15), Data) mem <- mkRegFileWCFLoad("mem.vmh", 0, ~0);
`else
    RegFile#(Bit#(15), Data) mem <- mkRegFileWCF(0, ~0);
`endif

    method MemResp ireq(Addr a);
        return mem.sub(truncate(a>>2));
    endmethod

    method ActionValue#(MemResp) dreq(MemReq r);
        Bit#(15) index = truncate(r.addr>>2);
        let data = mem.sub(index);
        if(r.op==St) begin
            mem.upd(index, r.data);
        end
        return data;
    endmethod
endmodule
