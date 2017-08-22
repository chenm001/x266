/*

Copyright (C) 2012 Muralidaran Vijayaraghavan <vmurali@csail.mit.edu>

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

*/


import Types::*;
import MemTypes::*;
import RegFile::*;

interface DMemory;
    method ActionValue#(MemResp) req(MemReq r);
endinterface

(* synthesize *)
module mkDMemory(DMemory);
    // This is Video Processing MCU, all of data is uninitialize when start.
    // workaround for testbench, it have data section for verify
`ifdef SIM
    RegFile#(Bit#(15), Data) mem <- mkRegFileFullLoad("mem.vmh");
`else
    RegFile#(Bit#(15), Data) mem <- mkRegFileFull();
`endif

    method ActionValue#(MemResp) req(MemReq r);
        Bit#(15) index = truncate(r.addr>>2);
        let data = mem.sub(index);
        if(r.op==St) begin
            mem.upd(index, r.data);
        end
        return data;
    endmethod
endmodule
