// Two stage

import Types::*;
import ProcTypes::*;
import MemTypes::*;
import RFile::*;
import OCMemory::*;
import Decode::*;
import Exec::*;
import CsrFile::*;
import myFifo::*;
import Ehr::*;
import Btb::*;
import Scoreboard::*;

// Data structure for Fetch to Execute stage
typedef struct {
    Addr pc;
    Addr predPc;
    DecodedInst dInst;
    Data rVal1;
    Data rVal2;
    Data csrVal;
    Bool epoch;
} Fetch2Execute deriving (Bits, Eq);

// redirect msg from Execute stage
typedef struct {
    Addr pc;
    Addr nextPc;
} ExeRedirect deriving (Bits, Eq);

(* synthesize *)
module mkProc(Proc);
    Ehr#(2, Addr) pcReg <- mkEhr(?);
    RFile            rf <- mkRFile;
    Scoreboard#(2)   sb <- mkCFScoreboard;
    OCMemory        mem <- mkMemory;
    CsrFile        csrf <- mkCsrFile;
    Btb#(6)         btb <- mkBtb; // 64-entry BTB

    // global epoch for redirection from Execute stage
    Reg#(Bool) exeEpoch <- mkReg(False);

    // EHR for redirection
    Ehr#(2, Maybe#(ExeRedirect)) exeRedirect <- mkEhr(Invalid);

    // FIFO between two stages
    Fifo#(2, Fetch2Execute) f2eFifo <- mkCFFifo;

    // fetch, decode, reg read stage
    rule doFetch(csrf.started);
        // fetch
        Data inst = mem.ireq(pcReg[0]);
        Addr predPc = btb.predPc(pcReg[0]);
    //    Addr predPc = pcReg[0]; // discussion 1: if we always predict PC to be next PC
        // decode
        DecodedInst dInst = decode(inst);
        // reg read
        Data rVal1 = rf.rd1(fromMaybe(?, dInst.src1));
        Data rVal2 = rf.rd2(fromMaybe(?, dInst.src2));
        Data csrVal = csrf.rd(fromMaybe(?, dInst.csr));
        // data to enq to FIFO
        Fetch2Execute f2e = Fetch2Execute {
            pc: pcReg[0],
            predPc: predPc,
            dInst: dInst,
            rVal1: rVal1,
            rVal2: rVal2,
            csrVal: csrVal,
            epoch: exeEpoch
        };
        // search scoreboard to determine stall
        if(!sb.search1(dInst.src1) && !sb.search2(dInst.src2)) begin
            // enq & update PC, sb
            f2eFifo.enq(f2e);
            pcReg[0] <= predPc;
            sb.insert(dInst.dst);
            $display("Fetch: PC = %x, inst = %x, expanded = ", pcReg[0], inst, showInst(inst));
        end
        else begin
            $display("Fetch Stalled: PC = %x", pcReg[0]);
        end
    endrule

    (* fire_when_enabled *)
    (* no_implicit_conditions *)
    rule cononicalizeRedirect(csrf.started);
        if(exeRedirect[1] matches tagged Valid .r) begin
            // fix mispred
            pcReg[1] <= r.nextPc;
            exeEpoch <= !exeEpoch; // flip epoch
            btb.update(r.pc, r.nextPc); // train BTB
            $display("Fetch: Mispredict, redirected by Execute");
        end
        // reset EHR
        exeRedirect[1] <= Invalid;
    endrule

    // ex, mem, wb stage
    rule doExecute(csrf.started);
        f2eFifo.deq;
        let f2e = f2eFifo.first;

        if(f2e.epoch != exeEpoch) begin
            // kill wrong-path inst, just deq sb
            sb.remove;
            $display("Execute: Kill instruction");
        end
        else begin
            // execute
            ExecInst eInst = exec(f2e.dInst, f2e.rVal1, f2e.rVal2, f2e.pc, f2e.predPc, f2e.csrVal);  
            // memory
            if(eInst.iType == Ld) begin
                eInst.data <- mem.dreq(MemReq{op: Ld, addr: eInst.addr, data: ?});
            end else if(eInst.iType == St) begin
                let d <- mem.dreq(MemReq{op: St, addr: eInst.addr, data: eInst.data});
            end
            // check unsupported instruction at commit time. Exiting
            if(eInst.iType == Unsupported) begin
                $fwrite(stderr, "ERROR: Executing unsupported instruction at pc: %x. Exiting\n", f2e.pc);
                $finish;
            end
            // write back to reg file
            if(isValid(eInst.dst)) begin
                rf.wr(fromMaybe(?, eInst.dst), eInst.data);
            end
            csrf.wr(eInst.iType == Csrw ? eInst.csr : Invalid, eInst.data);
            // remove from scoreboard
            sb.remove;
            // check mispred: with proper BTB, it is only possible for branch/jump inst
            if(eInst.mispredict) begin
                $display("Execute finds misprediction: PC = %x", f2e.pc);
                exeRedirect[0] <= Valid (ExeRedirect {
                    pc: f2e.pc,
                    nextPc: eInst.addr // Hint for discussion 1: check this line
                });
            end
            else begin
                $display("Execute: PC = %x", f2e.pc);
            end
        end
    endrule

    method ActionValue#(CpuToHostData) cpuToHost() if (csrf.started);
        let ret <- csrf.cpuToHost;
        return ret;
    endmethod

    method Action hostToCpu(Bit#(32) startpc) if (!csrf.started);
        csrf.start(0); // only 1 core, id = 0
        pcReg[0] <= startpc;
    endmethod
endmodule

