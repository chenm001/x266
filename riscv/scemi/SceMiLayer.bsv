
import ClientServer::*;
import FIFO::*;
import GetPut::*;
import DefaultValue::*;
import SceMi::*;
import Clocks::*;
import ResetXactor::*;

import Types::*;
import MemTypes::*;
import ProcTypes::*;

// Where to find mkProc
// PROCFILE is defined differently for each scemi build target
import `PROC_FILE::*;

typedef Proc DutInterface;
typedef CpuToHostData ToHost;
typedef Addr FromHost;

(* synthesize *)
module [Module] mkDutWrapper (DutInterface);
    let m <- mkProc();
    return m;
endmodule

module [SceMiModule] mkSceMiLayer();

    SceMiClockConfiguration conf = defaultValue;

    SceMiClockPortIfc clk_port <- mkSceMiClockPort(conf);
    DutInterface dut <- buildDutWithSoftReset(mkDutWrapper, clk_port);

    Empty tohost <- mkCpuToHostXactor(dut, clk_port);
    Empty fromhost <- mkHostToCpuXactor(dut, clk_port);

    Empty shutdown <- mkShutdownXactor();
endmodule

module [SceMiModule] mkCpuToHostXactor#(Proc proc, SceMiClockPortIfc clk_port ) (Empty);

    Get#(ToHost) resp = interface Get;
        method ActionValue#(ToHost) get = proc.cpuToHost();
    endinterface;

    Empty get <- mkGetXactor(resp, clk_port);
endmodule

module [SceMiModule] mkHostToCpuXactor#(Proc proc, SceMiClockPortIfc clk_port ) (Empty);

    Put#(FromHost) req = interface Put;
        method Action put(FromHost x) = proc.hostToCpu(x);
    endinterface;

    Empty put <- mkPutXactor(req, clk_port);
endmodule

