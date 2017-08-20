import Types::*;
import MemTypes::*;
import MemInit::*;
import BRAM::*;

interface FPGAMemory;
    method Action req(MemReq r);
    method ActionValue#(MemResp) resp;
    interface MemInitIfc init;
endinterface

(* synthesize *)
module mkFPGAMemory(FPGAMemory);
    BRAM_Configure cfg = defaultValue;

`ifdef SIM
	// directly load vmh in simulation
	cfg.loadFormat = Hex ("mem.vmh");
`endif

    BRAM1Port#(Bit#(16), Data) bram <- mkBRAM1Server(cfg);

`ifdef SIM
	MemInitIfc memInit <- mkDummyMemInit;
`else
    MemInitIfc memInit <- mkMemInitBRAM(bram);
`endif

    method Action req(MemReq r) if (memInit.done());
        bram.portA.request.put( BRAMRequest{
			write: (r.op == St), 
			responseOnWrite: False, 
			address: truncate(r.addr >> 2), 
			datain: r.data 
		} );
    endmethod

    method ActionValue#(MemResp) resp if(memInit.done());
        let d <- bram.portA.response.get;
        return d;
    endmethod

    interface init = memInit;
endmodule

