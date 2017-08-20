
import ClientServer::*;
import FIFO::*;
import GetPut::*;
import SceMi::*;
import Clocks::*;

typedef Server#(Bit#(1), Bit#(1)) SoftReset;

interface DutWithSoftReset#(type dutifc);
    interface dutifc dut;
    interface SoftReset rst;
endinterface

module [m] mkDutWithSoftReset(m#(dutifc) dutmodule, DutWithSoftReset#(dutifc) ifc)
        provisos(IsModule#(m, a__));

    Clock clk <- exposeCurrentClock;
    MakeResetIfc myrst <- mkReset(6, False, clk);

    Reg#(Bool) resetting <- mkReg(False);
    FIFO#(Bit#(1)) didreset <- mkFIFO();

    dutifc m_dut <- dutmodule(reset_by myrst.new_rst);

    rule donerest (resetting && !myrst.isAsserted());
        resetting <= False;
        didreset.enq(?);
    endrule

    interface dut = m_dut;

    interface Server rst;
        interface Put request;
            method Action put(Bit#(1) x);
                resetting <= True;
                myrst.assertReset();
            endmethod
        endinterface

        interface Get response = toGet(didreset);
    endinterface

endmodule

module [SceMiModule] buildDutWithSoftReset(m#(ifc) d, SceMiClockPortIfc clk_port, ifc i)
        provisos(IsModule#(m, a__), WithOrWithoutProbes#(m));

    DutWithSoftReset#(ifc) dut <- buildDut(mkDutWithSoftReset(d), clk_port);
    Empty softrst <- mkServerXactor(dut.rst, clk_port);
    return dut.dut;
endmodule

