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
    RegFile#(Bit#(14), Data) imem <- mkRegFileFullLoad("mem.vmh");
    RegFile#(Bit#(15), Data) dmem <- mkRegFileWCFLoad("mem.vmh.D", 0, ~0);
`else
    RegFile#(Bit#(14), Data) imem <- mkRegFileFull();
    RegFile#(Bit#(15), Data) dmem <- mkRegFileWCF(0, ~0);
`endif

    method MemResp ireq(Addr a);
        return imem.sub(truncate((a - iMemSt) >> 2));
    endmethod

    method ActionValue#(MemResp) dreq(MemReq r);
        Bit#(15) index = truncate((r.addr - dMemSt) >> 2);
        let data = dmem.sub(index);
        if(r.op==St) begin
            dmem.upd(index, r.data);
        end
        return data;
    endmethod
endmodule
