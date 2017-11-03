// Copyright (c) 2017 Min Chen.  All Rights Reserved.
// Author: Min Chen

// ================================================================
// BSV library imports

import RegFile       :: *;    // For RISC-V GPRs
import ConfigReg     :: *;
import FIFOF         :: *;
import SpecialFIFOs  :: *;
import Vector        :: *;

// ================================================================
// BSV project imports

import ISA_Decls :: *;    // Instruction encodings

// ================================================================
// Memory interface for CPU
// ================================================================

import BRAMCore :: *;
import DReg     :: *;

// ----------------
// IMem responses: either and exception or an instruction

typedef Word IMem_Resp;

// ----------------
// DMem request ops and sizes

typedef enum {
   MEM_OP_LOAD,
   MEM_OP_STORE
} Mem_Op deriving(Eq, Bits, FShow);

// ----------------
// DMem requests

typedef struct {
   Mem_Op               mem_op;
   Addr                 addr;
   Word                 data;          // Only relevant if mem_op == MEM_OP_STORE
   Bit#(Bytes_per_Word) written;
} DMem_Req deriving(Bits);

// ----------------
// DMem responses: either an exception or data
// (data value is only relevant for LOADs, irrelevant for STOREs)

typedef Maybe#(Word) DMem_Resp;


// ----------------
// Memory interface reference design

module mkIMemory#(Reg#(Bit#(64)) cycles, Addr base)(IMemory_IFC#(size))
   provisos(Add#(a__, size, ALEN));
   BRAM_PORT#(Bit#(size), Word)     mem   <- mkBRAMCore1Load(2 ** valueOf(size), False, (genC ? "mem.vmh" : "R:/mem.vmh"), False);

   method Action mem_req(Addr addr);
      let phyAddr = (addr - base);
      mem.put(False, truncate(phyAddr >> 2), ?);
      //$display("[%7d] [IMEM] ReqAddr = 0x%08h", cycles, addr);
   endmethod

   method ActionValue#(IMem_Resp) mem_resp();
      let x = mem.read();
      return x;
   endmethod
endmodule

module mkDMemory#(Reg#(Bit#(64)) cycles, Integer bankID)(DMemory_IFC#(size))
   provisos(Add#(a__, size, ALEN));
   BRAM_DUAL_PORT_BE#(Bit#(size), Word, 4)   mem      <- mkBRAMCore2BELoad(2 ** valueOf(size), False, (genC ? "mem.vmh.D" : "R:/mem.vmh.D") + integerToString(bankID), False);
   Reg#(Bool)                                mem_rd   <- mkDReg(False);

   method Action mem_req(DMem_Req req);
      mem.a.put(req.written, truncate(req.addr), req.data);
      mem_rd <= !(req.mem_op == MEM_OP_STORE);
      //$display("[DMEM] Addr = 0x%08h", req.addr);
   endmethod

   method ActionValue#(DMem_Resp) mem_resp;
      Word v = mem.a.read();

      //$display("[DMEM] Data = 0x%08h", v);
      return mem_rd ? tagged Valid v : tagged Invalid;
   endmethod
endmodule

// ----------------------------------------------------------------
// This interface is an argument to the '_mkRISCV' module,
// and is used insided the module to access memory.

interface IMemory_IFC#(numeric type size);
   method Action                  mem_req(Addr addr);
   method ActionValue#(IMem_Resp) mem_resp;
endinterface

interface DMemory_IFC#(numeric type size);
   method Action                  mem_req(DMem_Req req);
   method ActionValue#(DMem_Resp) mem_resp;
endinterface

// ================================================================
// This interfacce is offered by the '_mkRISCV' module to the environment.
// It is not part of the spec, per se, and just has scaffolding that allows
// the environment to control and CPU and probe its state.

interface RISCV_IFC;
   method Action start(Addr initial_pc);

   method ActionValue#(Word) cpuToHost;
endinterface

// ================================================================
// The RISC-V CPU Specification module, 'mkRISCV'


// ----------------
// Default fall-through PC

function Addr fv_fall_through_pc(Addr pc);
   return pc + 4;
endfunction: fv_fall_through_pc

// ----------------

module _mkRISCV#(Bit#(3) cfg_verbose)(RISCV_IFC)
   provisos(
      Log#(MemBanks, bitsMemBanks)
   );

   // CPU state
   Reg#(Bool)  cpu_enabled <- mkReg(False);

   // Program counter
   Reg#(Addr)     rg_FetchPC  <- mkRegU;
   RWire#(Addr)   rw_nxtPC    <- mkRWire;
   RWire#(Addr)   rw_jmpPC    <- mkUnsafeRWire;
   Reg#(Addr)     pcEpoch     <- mkConfigRegU;

   // General Purpose Registers
   RegFile#(RegName, Word) rf_GPRs  <- mkRegFileWCFLoad((genC ? "zeros_32.hex" : "R:/zeros_32.hex"), 0, ~0);

   // CSRs
   Reg#(Bit#(64))    csr_cycle   <- mkConfigReg(0);
   Reg#(Bit#(64))    csr_instret <- mkConfigReg(0);

   // internal components
   IMemory_IFC#(14)     imemory     <- mkIMemory(csr_cycle, imemSt);
   DMemory_IFC#(12)     dmemory[memBanks];
   for(Integer i = 0; i < memBanks; i = i + 1)
      dmemory[i]  <- mkDMemory(csr_cycle, i);

   // ----------------
   // These CSRs are technically not present in the user-mode ISA.

   Reg#(Word)  csr_mepc       <- mkRegU;
   Reg#(Word)  csr_mcause     <- mkRegU;
   Reg#(Word)  csr_mbadaddr   <- mkRegU;

   Reg#(Maybe#(Word)) csr_dscratch <- mkDReg(tagged Invalid);


   // ----------------------------------------------------------------
   // Non-architectural state, for this model

   Reg#(Maybe#(Addr))      rg_f2d      <- mkDReg(tagged Invalid);
   FIFOF#(Decoded_Instr)   fifo_d2e    <- mkPipelineFIFOF;
   FIFOF#(Exec2Wb_t)       fifo_e2w    <- mkPipelineFIFOF;

   // ----------------------------------------------------------------
   // Scoreboard map
   Reg#(Bool)        rg_scoreGPRs[numRegs];
   for(Integer i = 0; i < numRegs; i = i + 1)
      rg_scoreGPRs[i] <- mkReg(False);
   RWire#(RegName)   rw_scoreGPRsSet   <- mkRWire;
   RWire#(RegName)   rw_scoreGPRsReset <- mkRWire;

   // ----------------------------------------------------------------
   // Internal 256-bits memory bus
   RWire#(Vector#(8, Word)) rw_memBus  <- mkUnsafeRWire;

   // ----------------------------------------------------------------
   // Read a CSR
   // If the addr is valid, return tagged Valid value
   // else return tagged Invalid

   function Maybe#(Word) fv_read_csr(CSR_Addr csr_addr);
      if      (csr_addr == csr_CYCLE)     return tagged Valid truncate   (csr_cycle);
      else if (csr_addr == csr_INSTRET)   return tagged Valid truncate   (csr_instret);

      else if (csr_addr == csr_CYCLEH  )  return tagged Valid truncateLSB(csr_cycle);
      else if (csr_addr == csr_INSTRETH)  return tagged Valid truncateLSB(csr_instret);

      else if (csr_addr == csr_DSCRATCH)  return tagged Valid 0 ;

      else return tagged Invalid;
   endfunction: fv_read_csr

   // ----------------------------------------------------------------
   // Write a CSR
   // We assume a valid csr_addr, since this is always preceded by a read_csr which performs the check

   function Action fa_write_csr(CSR_Addr csr_addr, Word csr_value);
      action
         if (csr_addr == csr_DSCRATCH) begin
            csr_dscratch <= tagged Valid csr_value;
         end

         else begin
            $display("ERROR: fa_write_csr: (csr_addr 0x%0h, csr_value 0x%0h): illegal csr_addr", csr_addr, csr_value);
            $finish;
         end
      endaction
   endfunction: fa_write_csr

   // ================================================================
   // Instruction execution

   // ----------------------------------------------------------------
   // The following functions are common idioms for finishing an instruction

   // ----------------
   // Finish exception: record exception cause info, go to ENV_CALL state

   function Action fa_finish_with_exception(Bit#(4) exc_code, Addr badaddr);
      action
         if (cfg_verbose > 0) begin
            $display("[%7d] fa_do_exception: epc = 0x%0h, exc_code = 0x%0h, badaddr = 0x%0h", csr_cycle, pcEpoch, exc_code, badaddr);
         end

         csr_mepc     <= extend(pcEpoch);
         csr_mcause   <= { 1'b0, 0, exc_code };
         csr_mbadaddr <= extend(badaddr);

         $finish;
      endaction
   endfunction

   // ----------------
   // Finish instr with no output (no Rd-write): set PC, go to FETCH state

   function Action fa_finish_with_no_output();
      action
      endaction
   endfunction

   // ----------------
   // Finish instr with Rd-write: set Rd, set PC, go to WRITE_BACK state

   function Action fa_finish_with_Rd(RegName rd, Word rd_value);
      action
         fifo_e2w.enq( Exec2Wb_t {rd: rd, rd_value: tagged Value rd_value} );
         fa_finish_with_no_output;
      endaction
   endfunction

   // ----------------
   // Finish instr with Rd-write: set Rd, set PC, go to WRITE_BACK state

   function Action fa_finish_with_Ld(RegName rd, LdFunc op, Bit#(5) lsb5);
      action
         fifo_e2w.enq( Exec2Wb_t {rd: rd, rd_value: (tagged MemOp {ld_op: op, lsb5: lsb5})} );
         fa_finish_with_no_output;
      endaction
   endfunction

   // ----------------
   // Finish jump instrs; write Rd, set PC, go to FETCH state

   function Action fa_finish_jump(RegName rd, Word rd_value, Addr next_pc);
      action
         fifo_e2w.enq( Exec2Wb_t {rd: rd, rd_value: tagged Value rd_value} );
         rw_jmpPC.wset(next_pc);
      endaction
   endfunction

   // ----------------
   // Finish conditional branch instr: set PC, go to FETCH state

   function Action fa_finish_cond_branch(Bool condition_taken, Addr next_pc);
      action
         if (condition_taken) begin
            rw_jmpPC.wset(next_pc);
         end
      endaction
   endfunction

   // ----------------------------------------------------------------
   // Instruction execution
   // This function encapsulates ALL the opcodes.
   // It has internal functions that group related sub-opcodes.

   function ActionValue#(Fmt) fa_exec(Decoded_Instr decoded, Decoded_Fields fields);
      actionvalue

         // Values of Rs1 and Rs2 fields of the instr, unsigned
         //Word v1 = decoded.v1;
         //Word v2 = decoded.v2;
         Word  v1  = rf_GPRs.sub(fields.rs1);
         Word  v2  = rf_GPRs.sub(fields.rs2);

         // Values of Rs1 and Rs2 fields of the instr, signed versions
         Word_S  s_v1 = unpack(v1);
         Word_S  s_v2 = unpack(v2);

         // Value of CSR field of instr (if a valid CSR address)
         Maybe #(Word) m_v_csr = fv_read_csr(fields.csr);

         // ----------------------------------------------------------------
         // Instructions for Upper Immediate
         function ActionValue#(Fmt) fa_exec_LUI();
            actionvalue
               Bit#(32)    v32   = { fields.imm20_U, 12'h0 };
               Word_S      iv    = extend(unpack(v32));
               let         value = pack(iv);

               fa_finish_with_Rd(fields.rd, value);
               let msg = $format("lui %s, 0x%h", regNameABI[fields.rd], value[31:12]);
               return msg;
            endactionvalue
         endfunction: fa_exec_LUI

         function ActionValue#(Fmt) fa_exec_AUIPC();
            actionvalue
               Word_S  iv    = extend(unpack({ fields.imm20_U, 12'b0}));
               Word_S  pc_s  = extend(unpack(pcEpoch));
               Word    value = pack(pc_s + iv);

               fa_finish_with_Rd(fields.rd, value);
               let msg = $format("auipc %s, 0x%h", regNameABI[fields.rd], value[31:12]);
               return msg;
            endactionvalue
         endfunction: fa_exec_AUIPC

         // ----------------------------------------------------------------
         // Instructions for control-transfer

         function ActionValue#(Fmt) fa_exec_JAL();
            actionvalue
               Word_S offset  = extend(unpack(fields.imm21_UJ));
               Addr   next_pc = truncate(pack(extend(unpack(pcEpoch)) + offset));

               fa_finish_jump(fields.rd, extend(fv_fall_through_pc(pcEpoch)), next_pc);
               let msg = $format("jal %s, 0x%h", regNameABI[fields.rd], next_pc);
               return msg;
            endactionvalue
         endfunction: fa_exec_JAL

         function ActionValue#(Fmt) fa_exec_JALR();
            actionvalue
               Word_S offset  = extend(unpack(fields.imm12_I));
               Addr   next_pc = {truncate((pack(s_v1 + offset)) >> 1), 1'b0};

               fa_finish_jump(fields.rd, extend(fv_fall_through_pc(pcEpoch)), next_pc);
               let msg = $format("jalr %s, %s, %1d -> %h", regNameABI[fields.rd], regNameABI[fields.rs1], offset, next_pc);
               return msg;
            endactionvalue
         endfunction: fa_exec_JALR

         function ActionValue#(Fmt) fa_exec_BRANCH(BrFunc op);
            actionvalue
               Word_S offset  = extend(unpack(fields.imm13_SB));
               Addr   next_pc = truncate(pack(extend(unpack(pcEpoch)) + offset));

               case(op)
                  Eq    :  fa_finish_cond_branch(v1  == v2,    next_pc);
                  Neq   :  fa_finish_cond_branch(v1  != v2,    next_pc);
                  Lt    :  fa_finish_cond_branch(s_v1 <  s_v2, next_pc);
                  Ge    :  fa_finish_cond_branch(s_v1 >= s_v2, next_pc);
                  Ltu   :  fa_finish_cond_branch(v1  <  v2,    next_pc);
                  /* Geu */
               default  :  fa_finish_cond_branch(v1  >= v2,    next_pc);
               endcase

               let msg =  $format("B")
                        + fshow(op)
                        + $format(" %s, %s, 0x%h", regNameABI[fields.rd], regNameABI[fields.rs1], next_pc);
               return msg;
            endactionvalue
         endfunction: fa_exec_BRANCH

         // ----------------------------------------------------------------
         // LD and ST instructions.
         // Issue request here; will be completed in STATE_EXEC_LD/ST_RESPONSE

         function Vector#(8, Addr) f_getBankAddr(Addr mem_addr, Addr stride);
            Bit#(3) bank = truncate(mem_addr >> 2);
            Vector#(8, Addr) ret = ?;

            // =================
            // Generate bank offset table
            // Example: bank=2
            // 0 1 2 3 4 5 6 7
            //     * * * * * *
            // * *
            function Bit#(3) mapBankToOffset(Integer x);
               return (fromInteger(x) - bank);
            endfunction

            Vector#(8, Bit#(3)) offset = map(mapBankToOffset, genVector);
            for(Integer i = 0; i < 8; i = i + 1) begin
               ret[i] = (mem_addr + zeroExtend(offset[i]) * stride) >> (2+3);
            end
            return ret;
         endfunction

         function ActionValue#(Fmt) fa_exec_LD_Req(LdFunc op);
            actionvalue
               Word_S  imm_s    = extend(unpack(fields.imm12_I));
               Addr    mem_addr = truncate(pack(s_v1 + imm_s)) - dmemSt;
               Bit#(5) lsb5     = truncate(mem_addr);
               Vector#(8, Addr) bankAddr = f_getBankAddr(mem_addr, 4);

               function Action fa_LD_Req(Mem_Data_Size sz);
                  action
                     for(Integer i = 0; i < memBanks; i = i + 1) begin
                        let req = DMem_Req {mem_op:      MEM_OP_LOAD,
                                            addr:        bankAddr[i],
                                            written:     0,
                                            data:        ?};
                        dmemory[i].mem_req(req);
                     end
                     fa_finish_with_Ld(fields.rd, op, lsb5);
                  endaction
               endfunction

               case(op)
                  Lb    :  fa_LD_Req(BITS8);
                  Lbu   :  fa_LD_Req(BITS8);
                  Lh    :  fa_LD_Req(BITS16);
                  Lhu   :  fa_LD_Req(BITS16);
                  /*Lw*/
               default  :  fa_LD_Req(BITS32);
               endcase

               let msg =  fshow(op)
                        + $format(" %s, %1d(%s)", regNameABI[fields.rd], imm_s, regNameABI[fields.rs1]);
               return msg;
            endactionvalue
         endfunction: fa_exec_LD_Req

         function ActionValue#(Fmt) fa_exec_ST_Req(StFunc op);
            actionvalue
               Word_S  imm_s    = extend(unpack(fields.imm12_S));
               Addr    mem_addr = truncate(pack(s_v1 + imm_s)) - dmemSt;
               Vector#(8, Addr) bankAddr = f_getBankAddr(mem_addr, 4);

               function Action fa_ST_req(Mem_Data_Size sz);
                  action
                     Bit#(Bits_per_Word_Byte_Index) align = truncate(mem_addr);
                     Bit#(3) bank = truncate(mem_addr >> 2);
                     Word aligned_data = v2 << {align, 3'b0};
                     Bit#(Bytes_per_Word) write_en = (case(sz)
                                                         BITS8:  ('b0001 << align);
                                                         BITS16: ('b0011 << align);
                                                         default/*BITS32*/: ('b1111);
                                                      endcase);

                     for(Integer i = 0; i < memBanks; i = i + 1) begin
                        let req = DMem_Req {mem_op:      MEM_OP_STORE,
                                            addr:        bankAddr[i],
                                            data:        aligned_data,
                                            written:     (fromInteger(i) == bank ? write_en : 0)};
                        dmemory[i].mem_req(req);
                     end
                     fa_finish_with_no_output;
                  endaction
               endfunction

               case(op)
                  Sb    :  fa_ST_req(BITS8);
                  Sh    :  fa_ST_req(BITS16);
                  /*Sw*/
               default  :  fa_ST_req(BITS32);
               endcase

               let msg =  fshow(op)
                        + $format(" %s, %1d(%s)", regNameABI[fields.rd], imm_s, regNameABI[fields.rs1]);
               return msg;
            endactionvalue
         endfunction: fa_exec_ST_Req

         // ----------------------------------------------------------------
         // Instructios for Register-Immediate alu ops

         function ActionValue#(Fmt) fa_exec_OP_IMM(AluFunc op);
            actionvalue
               Word                v2    = zeroExtend(fields.imm12_I);
               Word_S              s_v2  = signExtend(unpack(fields.imm12_I));
               Bit#(TLog#(XLEN))   shamt = truncate(fields.imm12_I);

               case(op)
                  Add   :  fa_finish_with_Rd(fields.rd, pack(s_v1 + s_v2));
                  Slt   :  fa_finish_with_Rd(fields.rd, ((s_v1 < s_v2) ? 1 : 0));
                  Sltu  :  fa_finish_with_Rd(fields.rd, ((v1  < pack(s_v2))  ? 1 : 0));
                  Xor   :  fa_finish_with_Rd(fields.rd, pack(s_v1 ^ s_v2));
                  Or    :  fa_finish_with_Rd(fields.rd, pack(s_v1 | s_v2));
                  And   :  fa_finish_with_Rd(fields.rd, pack(s_v1 & s_v2));
                  Sll   :  fa_finish_with_Rd(fields.rd, (v1 << shamt));
                  Srl   :  fa_finish_with_Rd(fields.rd, (v1 >> shamt));
                  /*Sra*/
               default  :  fa_finish_with_Rd(fields.rd, pack(s_v1 >> shamt));
               endcase

               let msg =  fshow(op)
                        + $format("i %s, %s, 0x%h", regNameABI[fields.rd], regNameABI[fields.rs1], fields.imm12_I);
               return msg;
            endactionvalue
         endfunction: fa_exec_OP_IMM

         // ----------------------------------------------------------------
         // Instructios for Register-Register alu ops

         function ActionValue#(Fmt) fa_exec_OP(AluFunc op);
            actionvalue
               Bit#(TLog#(XLEN)) shamt = truncate(v2);    // NOTE: upper bits are unspecified in spec

               case(op)
                  Add   :  fa_finish_with_Rd(fields.rd, pack(s_v1 + s_v2));
                  Sub   :  fa_finish_with_Rd(fields.rd, pack(s_v1 - s_v2));
                  Sll   :  fa_finish_with_Rd(fields.rd, (v1 << shamt));
                  Slt   :  fa_finish_with_Rd(fields.rd, ((s_v1 < s_v2) ? 1 : 0));
                  Sltu  :  fa_finish_with_Rd(fields.rd, ((v1  < v2)  ? 1 : 0));
                  Xor   :  fa_finish_with_Rd(fields.rd, pack(s_v1 ^ s_v2));
                  Srl   :  fa_finish_with_Rd(fields.rd, (v1 >> shamt));
                  Sra   :  fa_finish_with_Rd(fields.rd, pack(s_v1 >> shamt));
                  Or    :  fa_finish_with_Rd(fields.rd, pack(s_v1 | s_v2));
                  /*And*/
               default  :  fa_finish_with_Rd(fields.rd, pack(s_v1 & s_v2));
               endcase

               let msg =  fshow(op)
                        + $format(" %s, %s, %s", regNameABI[fields.rd], regNameABI[fields.rs1], regNameABI[fields.rs2]);
               return msg;
            endactionvalue
         endfunction: fa_exec_OP


         // ----------------------------------------------------------------
         // Instrucions for System-level ops

         function ActionValue#(Fmt) fa_exec_SYSTEM(SysFunc op);
            actionvalue
               let csr_old_val = fromMaybe(?, m_v_csr);

               case(op)
                  CSRRW :  begin
                                 fa_write_csr(fields.csr, v1);
                                 fa_finish_with_Rd(fields.rd, csr_old_val);
                              end

                  CSRRS :  begin
                                 if (fields.rs1 != 0) begin
                                    Word csr_new_val = (csr_old_val | v1);
                                    fa_write_csr(fields.csr, csr_new_val);
                                 end
                                 fa_finish_with_Rd(fields.rd, csr_old_val);
                              end

                  /*CSRRC*/
               default  :  begin
                                 if (fields.rs1 != 0) begin
                                    Word csr_new_val = (csr_old_val & (~ v1));
                                    fa_write_csr(fields.csr, csr_new_val);
                                 end
                                 fa_finish_with_Rd(fields.rd, csr_old_val);
                              end
               endcase

               let msg = ?;
               if ( (op == CSRRS) && (fields.csr == csr_CYCLE) )
                  msg = $format("rdcycle %s", regNameABI[fields.rd]);
               else if ( (op == CSRRS) && (fields.csr == csr_INSTRET) )
                  msg = $format("rdinstret %s", regNameABI[fields.rd]);
               else if ( (op == CSRRW) && (fields.csr == csr_DSCRATCH) )
                  msg = $format("csrw dscratch, %s", regNameABI[fields.rs1]);
               else begin
                  msg =  fshow(op)
                       + $format(" %s, 0x%h, %s", regNameABI[fields.rd], fields.csr, regNameABI[fields.rs1]);
               end
               return msg;
            endactionvalue
         endfunction: fa_exec_SYSTEM

         // ----------------------------------------------------------------
         // Main body of fa_exec(), dispatching to the sub functions
         // based on major OPCODE

         let x = ?;
         case(decoded.op.opcode) matches
            tagged Lui        :  x <- fa_exec_LUI();
            tagged Auipc      :  x <- fa_exec_AUIPC();
            tagged Jal        :  x <- fa_exec_JAL();
            tagged Jalr       :  x <- fa_exec_JALR();
            tagged Br   .op   :  x <- fa_exec_BRANCH(op);
            tagged Ld   .op   :  x <- fa_exec_LD_Req(op);
            tagged St   .op   :  x <- fa_exec_ST_Req(op);
            tagged Alui .op   :  x <- fa_exec_OP_IMM(op);
            tagged Alu  .op   :  x <- fa_exec_OP(op);
            tagged Sys  .op   :  x <- fa_exec_SYSTEM(op);
            default           :  fa_finish_with_exception(exc_code_ILLEGAL_INSTRUCTION, ?);
         endcase
         return x;
      endactionvalue
   endfunction: fa_exec

   function Bit#(256) f_shuffle(Bit#(512) v, Vector#(32, Bit#(6)) s);
      Vector#(64, Bit#(8)) x = unpack(v);
      Vector#(32, Bit#(8)) r = ?;

      for(Integer i = 0; i < 32; i = i + 1) begin
         r[i] = x[s[i]];
      end
      return pack(r);
   endfunction: f_shuffle

   // ================================================================
   // The CPU's top-level logic

   // Update CPU internal status in every cycle
   (* fire_when_enabled, no_implicit_conditions *)
   rule update_score;
      let x = fromMaybe(0, rw_scoreGPRsSet.wget);
      let y = fromMaybe(0, rw_scoreGPRsReset.wget);

      rg_scoreGPRs[x] <= True;
      if (x != y) begin
         rg_scoreGPRs[y] <= False;
      end
   endrule

   // ---------------- FETCH
   // Issue instruction request and decode

   // ----------------------------------------------------------------
   // Instruction fetch
   (* fire_when_enabled *)
   rule rl_fetch(cpu_enabled);
      let next_pc = fromMaybe(fromMaybe(rg_FetchPC, rw_nxtPC.wget), rw_jmpPC.wget);
      if (cfg_verbose > 1) $display("[%7d] (  |F   ) : %25s Read instruction pc = 0x%08h", csr_cycle, "", next_pc);
      imemory.mem_req(next_pc);
      rg_f2d      <= tagged Valid next_pc;
      rg_FetchPC  <= next_pc;
   endrule

   // ----------------------------------------------------------------
   // Instruction decode
   (* conflict_free="rl_decode, rl_exec" *)
   rule rl_decode(rg_f2d matches tagged Valid .xPC);
      let instr <- imemory.mem_resp;

      if (cfg_verbose > 1) $display("[%7d] (  | D  ) : %25s pc = 0x%08h, instr = %h", csr_cycle, "", xPC, instr);

      Decoded_Instr  decoded = fv_decode(xPC, instr);
      Decoded_Fields fields  = fv_decode_fields(decoded.instr);

      // Calculate dependency register
      let score1 = False;
      let score2 = False;
      let rd_upd = fromMaybe(0, rw_scoreGPRsSet.wget);
   
      if (decoded.op.rs1 != 0)
         score1 = rg_scoreGPRs[decoded.op.rs1] || (rd_upd == decoded.op.rs1);
   
      if (decoded.op.rs2 != 0)
         score2 = rg_scoreGPRs[decoded.op.rs2] || (rd_upd == decoded.op.rs2);

      let score_conflict = score1 || score2;

      if (cfg_verbose > 2) begin
         $write("[%7d] (  |    ) : %25s Scoreboard = [", csr_cycle, "");
         for(Integer i = 0; i < numRegs; i = i + 1) begin
            $write("%1d, ", rg_scoreGPRs[i] ? 1 : 0);
         end
         $write("], dst = %d, rs1 = %d, rs2 = %d\n", fields.rd, fields.rs1, fields.rs2);
      end

      if (score_conflict) begin
         if (cfg_verbose > 1) $display("[%7d] ( R| D  ) : %25s STALL Conflict pc = 0x%08h, instr = 0x%h, epoch = 0x%08h", csr_cycle, "", decoded.pc, decoded.instr, pcEpoch);
      end
      else begin
         let next_pc = fv_fall_through_pc(xPC);
         rw_nxtPC.wset(next_pc);

         decoded.bypass = isValid(rw_jmpPC.wget);
         fifo_d2e.enq( decoded );
      end
   endrule

   // ---------------- EXECUTE
   // Receive instruction from IMem; handle exception if any, else execute it;

   //(* no_implicit_conditions *)
   rule rl_exec(fifo_d2e.notEmpty && fifo_e2w.notFull);
      let decoded = fifo_d2e.first;

      // ----------------------------------------------------------------
      // Instruction fields decode
      Decoded_Fields fields   = fv_decode_fields(decoded.instr);

      if (decoded.bypass) begin
         if (cfg_verbose > 1) $display("[%7d] (A |  E ) : %25s STALL Ignore pc = 0x%08h, instr = 0x%08h, epoch = 0x%08h", csr_cycle, "", decoded.pc, decoded.instr, pcEpoch);
         fifo_d2e.deq;
      end
      else begin
         // Update dependency flag for $rd
         rw_scoreGPRsSet.wset(decoded.op.rd);

         let msg <- fa_exec(decoded, fields);
         if (cfg_verbose > 1) begin
            $write("[%7d] (  |  E ) : [ ", csr_cycle, msg);
            $display(" ]    pc = 0x%08h, instr = 0x%08h, epoch = 0x%08h", decoded.pc, decoded.instr, pcEpoch);
         end
         fifo_d2e.deq;

         // Update pcEpoch
         pcEpoch <= fromMaybe(fv_fall_through_pc(pcEpoch), rw_jmpPC.wget);

         // ---------------- FINISH: increment csr_instret or record explicit CSRRx update of csr_instret
         csr_instret <= csr_instret + 1;
      end
   endrule

   // ---------------- Read and forward memory data
   rule rl_mem_bus;
      // Shuffle memory bus response
      Vector#(8, Word) resp = ?;
      for(Integer i = 0; i < 8; i = i + 1) begin
         let data_resp <- dmemory[i].mem_resp;
         resp[i] = fromMaybe(?, data_resp);
    
         if (cfg_verbose > 0 && !isValid(data_resp)) begin
            $display("[%7d] (  |   W) : Memory read failed on Bank(%d)", csr_cycle, i);
            $finish;
         end
      end
      rw_memBus.wset(resp);
   endrule

   // ---------------- RegFile & DMem Write Back
   rule rl_write_back_gpr(fifo_e2w.notEmpty);
      let x = fifo_e2w.first;
      fifo_e2w.deq;
      let rd = x.rd;
      let rd_value = ?;

      case(x.rd_value) matches
         tagged MemOp .op: begin
            let ld_op = op.ld_op;
            let lsb5 = op.lsb5;
            Bit#(3) bank = truncateLSB(lsb5);
            Bit#(2) shift = truncate(lsb5);
            let resp = fromMaybe(?, rw_memBus.wget);

            Word data = (resp[bank] >> {shift, 3'd0});
            let extendFunc = (ld_op == Lbu || ld_op == Lhu) ? zeroExtend : signExtend;
            rd_value = (case(ld_op)
                    Lb, Lbu: extendFunc(data[7:0]);
                    Lh, Lhu: extendFunc(data[15:0]);
                    default: extendFunc(data[31:0]);
                  endcase);
         end
         tagged Value .value: begin
            rd_value = value;
         end
      endcase

      if (cfg_verbose > 1) $display("[%7d] (  |   W) : %25s %s = %h, clear scoreGPRs[%1d] (= %1d)", csr_cycle, "", regNameABI[rd], rd_value, rd, rg_scoreGPRs[rd] ? 1 : 0);

      // NOTE: DOES NOT check register x0 because set value to Zero when read
      if (rd != 0) begin
         rf_GPRs.upd(rd, rd_value);
      end
      rw_scoreGPRsReset.wset(rd);
   endrule


   // ---------------- Increment csr_cycle according to external oracles

   rule rl_incr_cycle;
      csr_cycle <= csr_cycle + 1;
   endrule

   // ----------------------------------------------------------------
   // INTERFACE

   method Action start(Addr initial_pc) if (!cpu_enabled);
      rg_FetchPC  <= initial_pc;
      pcEpoch     <= initial_pc;
      cpu_enabled <= True;
   endmethod

   method ActionValue#(Word) cpuToHost() if (csr_dscratch matches tagged Valid .ret);
      return ret;
   endmethod
endmodule

// ================================================================

(* synthesize *)
module mkRISCV(RISCV_IFC);
   (* hide *) let _m <- _mkRISCV(0);
   return _m;
endmodule

// ================================================================
`ifdef TEST_BENCH_RISCV
module mkTb();
   Reg#(Bit#(32))    cycles <- mkConfigReg(0);

   let               dut         <- mkRISCV;
   Reg#(Bit#(16))    csr_int_low <- mkRegU;

   rule do_cycle;
      cycles <= cycles + 1;
      //if (cycles > 1200)
      //   $finish;
   endrule

   rule do_cpuToHost;
      let csr_value <- dut.cpuToHost;
      Bit#(16) csrCmd = truncateLSB(csr_value);
      Bit#(16) csrDat = truncate(csr_value);

      case(csrCmd)
         0: begin // Exit
            if (csrDat == 0) begin
               $fdisplay(stderr, "PASSED\n");
            end
            else begin
               $fdisplay(stderr, "FAILED: exit code = %d\n", csrDat);
            end
            $finish;
         end
         1: begin // PrintChar
            $fwrite(stderr, "%c", csrDat[7:0]);
         end
         2: begin // PrintIntLow
            csr_int_low <= csrDat;
         end
         3: begin // PrintIntHigh
            $fwrite(stderr, "%d", {csrDat, csr_int_low});
         end
         default: begin
            $fdisplay(stderr, "Unknown type %d", csrCmd);
            $finish;
         end
      endcase
   endrule

   rule do_start(cycles == 0);
      dut.start('h200);
   endrule
endmodule
`endif // TESTBENCH

