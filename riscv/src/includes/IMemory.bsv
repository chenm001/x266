import Types::*;
import MemTypes::*;
import RegFile::*;

interface IMemory;
    method MemResp req(Addr a);
endinterface

(* synthesize *)
module mkIMemory(IMemory);
    // This is Video Processing MCU, all of programs in the ROM
    RegFile#(Bit#(15), Data) mem <- mkRegFileFullLoad("mem.vmh");

    method MemResp req(Addr a);
        return mem.sub(truncate(a>>2));
    endmethod
endmodule

