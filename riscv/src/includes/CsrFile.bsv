/*

Copyright (C) 2012 Muralidaran Vijayaraghavan <vmurali@csail.mit.edu>

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

*/


import Types::*;
import ProcTypes::*;
import Ehr::*;
import ConfigReg::*;
import Fifo::*;

interface CsrFile;
    method Action start(Data id);
    method Bool started;
    method Data rd(CsrIndx idx);
    method Action wr(Maybe#(CsrIndx) idx, Data val);
    method ActionValue#(CpuToHostData) cpuToHost;
endinterface

(* synthesize *)
module mkCsrFile(CsrFile);
    Reg#(Bool) startReg <- mkConfigReg(False);

	// CSR 
    Reg#(Data) numInsts <- mkConfigReg(0); // csrInstret -- read only
    Reg#(Data) cycles <- mkReg(0); // csrCycle -- read only
	Reg#(Data) coreId <- mkConfigReg(0); // csrMhartid -- read only
    Fifo#(2, CpuToHostData) toHostFifo <- mkCFFifo; // csrMtohost -- write only

    rule count (startReg);
        cycles <= cycles + 1;
        $display("\nCycle %d ----------------------------------------------------", cycles);
    endrule

    method Action start(Data id) if(!startReg);
        startReg <= True;
        cycles <= 0;
		coreId <= id;
    endmethod

    method Bool started;
        return startReg;
    endmethod

    method Data rd(CsrIndx idx);
        return (case(idx)
                    csrCycle: cycles;
                    csrInstret: numInsts;
                    csrMhartid: coreId;
					default: ?;
                endcase);
    endmethod

    method Action wr(Maybe#(CsrIndx) csrIdx, Data val);
        if(csrIdx matches tagged Valid .idx) begin
            case (idx)
				csrMtohost: begin
					// high 16 bits encodes type, low 16 bits are data
					Bit#(16) hi = truncateLSB(val);
					Bit#(16) lo = truncate(val);
					toHostFifo.enq(CpuToHostData {
						c2hType: unpack(truncate(hi)),
						data: lo
					});
				end
            endcase
        end
        numInsts <= numInsts + 1;
    endmethod

    method ActionValue#(CpuToHostData) cpuToHost;
        toHostFifo.deq;
        return toHostFifo.first;
    endmethod
endmodule
