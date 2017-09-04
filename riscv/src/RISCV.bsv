// Copyright (c) 2017 Min Chen.  All Rights Reserved.
// Author: Min Chen

// ================================================================
// BSV library imports

import RegFile   :: *;    // For RISC-V GPRs
import ConfigReg :: *;

// ================================================================
// BSV project imports

import ISA_Decls :: *;    // Instruction encodings

// ================================================================
// Memory interface for CPU
// ================================================================

import BRAMCore :: *;

// ----------------
// IMem responses: either and exception or an instruction

typedef Maybe#(Word) IMem_Resp;

// ----------------
// DMem request ops and sizes

typedef enum {
   MEM_OP_LOAD,
   MEM_OP_STORE
} Mem_Op deriving(Eq, Bits, FShow);

// ----------------
// DMem requests

typedef struct {
   Mem_Op         mem_op;
   Mem_Data_Size  mem_data_size;
   Addr           addr;
   Word           data;          // Only relevant if mem_op == MEM_OP_STORE
} DMem_Req deriving(Bits, FShow);

// ----------------
// DMem responses: either an exception or data
// (data value is only relevant for LOADs, irrelevant for STOREs)

typedef union tagged {
   Exc_Code  DMem_Resp_Exception;
   Word      DMem_Resp_Ok;
} DMem_Resp deriving(Bits, FShow);


// ----------------
// Memory interface reference design

module mkMemory(Memory_IFC);
   BRAM_PORT#(Bit#(14), Word)             imem <- mkBRAMCore1Load(valueOf(TExp#(14)), False, "mem.vmh", False);
   BRAM_DUAL_PORT_BE#(Bit#(15), Word, 4)  dmem <- mkBRAMCore2BELoad(valueOf(TExp#(15)), False, "mem.vmh.D", False);
   Reg#(Bit#(2))                          reg_shift <- mkRegU;
   Reg#(Mem_Data_Size)                    reg_size  <- mkRegU;

   method Action imem_req(Addr addr);
      let phyAddr = addr - imemSt;
      imem.put(False, truncate(phyAddr >> 2), ?);
      //$display("[IMEM] Addr = 0x%08h", addr);
   endmethod

   method ActionValue#(IMem_Resp) imem_resp;
      return tagged Valid imem.read();
   endmethod

   method Action dmem_req(DMem_Req req);
      let phyAddr = (req.addr - dmemSt);
      Word val = req.data;
      Bit#(Bits_per_Word_Byte_Index) shift = truncate(req.addr);
      Bit#(Bytes_per_Word) mask = 4'b1111;
      Word pad = ?;

      reg_shift <= shift;
      reg_size <= req.mem_data_size;

      case(req.mem_data_size)
         BITS8: begin
            case(shift)
               0: begin
                  mask = 4'b0001;
               end
               2'd1: begin
                  mask = 4'b0010;
                  val = {pad[15:0], val[7:0], ?};
               end
               2: begin
                  mask = 4'b0100;
                  val = {pad[7:0], val[7:0], ?};
               end
               3: begin
                  mask = 4'b1000;
                  val = {val[7:0], ?};
               end
            endcase
         end
         BITS16: begin
            if (shift[0] != 0) begin
               $display("Unaligned memory access on 16-bits bound");
               $finish;
            end
            if (shift[1] == 0) begin
               mask = 4'b0011;
            end
            else begin
               mask = 4'b1100;
               val = {val[15:0], ?};
            end
         end
      endcase

      dmem.a.put((req.mem_op == MEM_OP_STORE) ? mask : 0, truncate(phyAddr >> 2), val);
      //$display("[DMEM] Addr = 0x%08h", req.addr);
   endmethod

   method ActionValue#(DMem_Resp) dmem_resp;
      Word v = dmem.a.read();

      case(reg_shift)
         0: v = v;
         1: v = {?, v[31: 8]};
         2: v = {?, v[31:16]};
         3: v = {?, v[31:24]};
      endcase

      return tagged DMem_Resp_Ok v;
   endmethod
endmodule


// ----------------------------------------------------------------
// This interface is an argument to the 'mkRISCV_Spec' module,
// and is used insided the module to access memory.
// MMUs, caches etc. are outside this boundary.

interface Memory_IFC;
   method Action                  imem_req(Addr addr);
   method ActionValue#(IMem_Resp) imem_resp;

   method Action                  dmem_req(DMem_Req req);
   method ActionValue#(DMem_Resp) dmem_resp;
endinterface

// ================================================================
// This interfacce is offered by the 'mkRISCV_Spec' module to the environment.
// It is not part of the spec, per se, and just has scaffolding that allows
// the environment to control and CPU and probe its state.

interface RISCV_IFC;
   method Action start(Addr initial_pc);
   method Bool   halted;

   method ActionValue#(Word) cpuToHost;
endinterface

// ================================================================
// The RISC-V CPU Specification module, 'mkRISCV'

// ----------------
// The CPU is initially in the IDLE state.
// It starts running when it is put into the FETCH state.
// When running, it cycles through the following sequences of states:
//
//     FETCH -> EXEC ->                     FINISH    for most instructions
//     FETCH -> EXEC -> EXEC_LD_RESPONSE -> FINISH    for LD instructions
//     FETCH -> EXEC -> EXEC_ST_RESPONSE -> FINISH    for ST instructions
//

typedef enum {STATE_IDLE,
              STATE_FETCH,
              STATE_EXEC,
              STATE_EXEC_LD_RESPONSE,
              STATE_EXEC_ST_RESPONSE,
              STATE_FINISH,
              STATE_HALT
   } CPU_STATE
deriving(Eq, Bits, FShow);

// ----------------
// Default fall-through PC

function Addr fv_fall_through_pc(Addr pc);
   return pc + 4;
endfunction: fv_fall_through_pc

// ----------------

module _mkRISCV#(Bit#(3) cfg_verbose)(RISCV_IFC);

   // internal components
   Memory_IFC           memory   <- mkMemory;

   // Program counter
   Reg#(Word) pc <- mkRegU;

   // General Purpose Registers
   RegFile#(RegName, Word) regfile  <- mkRegFileFull;

   // CSRs
   Reg#(Bit #(64))   csr_cycle   <- mkConfigReg(0);
   Reg#(Bit #(64))   csr_instret <- mkConfigReg(0);

   // ----------------
   // These CSRs are technically not present in the user-mode ISA.

   Reg#(Word)  csr_mepc       <- mkRegU;
   Reg#(Word)  csr_mcause     <- mkRegU;
   Reg#(Word)  csr_mbadaddr   <- mkRegU;

   Reg#(Maybe#(Word)) csr_dcsr <- mkReg(tagged Invalid);


   // ----------------------------------------------------------------
   // Non-architectural state, for this model

   Reg#(CPU_STATE)   cpu_state   <- mkReg(STATE_IDLE);
   Reg#(Addr)        rg_mem_addr <- mkRegU;    // Effective addr in LD/ST
   Reg#(Word)        rg_instr    <- mkRegU;    // Current instruction

   // ----------------------------------------------------------------
   // Read a CSR
   // If the addr is valid, return tagged Valid value
   // else return tagged Invalid

   function Maybe#(Word) fv_read_csr(CSR_Addr csr_addr);
      if      (csr_addr == csr_CYCLE)     return tagged Valid truncate   (csr_cycle);
      else if (csr_addr == csr_INSTRET)   return tagged Valid truncate   (csr_instret);

      else if (csr_addr == csr_CYCLEH  )  return tagged Valid truncateLSB(csr_cycle);
      else if (csr_addr == csr_INSTRETH)  return tagged Valid truncateLSB(csr_instret);

      else if (csr_addr == csr_DCSR)      return tagged Valid 0 ;

      else return tagged Invalid;
   endfunction: fv_read_csr

   // ----------------------------------------------------------------
   // Write a CSR
   // We assume a valid csr_addr, since this is always preceded by a read_csr which performs the check

   function Action fa_write_csr(CSR_Addr csr_addr, Word csr_value);
      action
              if (csr_addr == csr_CYCLE)    csr_cycle   <= zeroExtend(csr_value);

         else if (csr_addr == csr_CYCLEH)
            csr_cycle <= { csr_value[31:0], truncate(csr_cycle) };

         else if (csr_addr == csr_DCSR) begin
            csr_dcsr <= tagged Valid csr_value;
         end

         else begin
            $display("ERROR: RISCV_Spec.fa_write_csr (csr_addr 0x%0h, csr_value 0x%0h): illegal csr_addr", csr_addr, csr_value);
         end
      endaction
   endfunction: fa_write_csr

   // ================================================================
   // Instruction execution

   // ----------------------------------------------------------------
   // The following functions are common idioms for finishing an instruction

   // ----------------
   // Finish exception: record exception cause info, go to ENV_CALL state

   function Action fa_finish_with_exception(Word epc, Exc_Code exc_code, Addr badaddr);
      action
         if ((cfg_verbose != 0) || (exc_code == exc_code_ILLEGAL_INSTRUCTION)) begin
            $display("[%7d] fa_do_exception: epc = 0x%0h, exc_code = 0x%0h, badaddr = 0x%0h", csr_cycle, epc, exc_code, badaddr);
         end

         csr_mepc     <= epc;
         csr_mcause   <= { 1'b0, 0, exc_code };
         csr_mbadaddr <= badaddr;

         cpu_state    <= STATE_HALT;
      endaction
   endfunction

   // ----------------
   // Finish instr with no output (no Rd-write): set PC, go to FETCH state

   function Action fa_finish_with_no_output();
      action
         pc        <= fv_fall_through_pc (pc);
         cpu_state <= STATE_FINISH;
      endaction
   endfunction

   // ----------------
   // Finish instr with Rd-write: write Rd, set PC, go to FETCH state

   function Action fa_finish_with_Rd(RegName rd, Word rd_value);
      action
         if (rd != 0) regfile.upd(rd, rd_value);
         pc        <= fv_fall_through_pc(pc);
         cpu_state <= STATE_FINISH;
      endaction
   endfunction

   // ----------------
   // Finish jump instrs; write Rd, set PC, go to FETCH state

   function Action fa_finish_jump(RegName rd, Word rd_value, Addr next_pc);
      action
         if (rd != 0) regfile.upd(rd, rd_value);
         pc        <= next_pc;
         cpu_state <= STATE_FINISH;
      endaction
   endfunction

   // ----------------
   // Finish conditional branch instr: set PC, go to FETCH state

   function Action fa_finish_cond_branch(Bool condition_taken, Addr next_pc);
      action
         pc        <= (condition_taken ? next_pc : fv_fall_through_pc(pc));
         cpu_state <= STATE_FINISH;
      endaction
   endfunction

   // ----------------------------------------------------------------
   // Instruction execution
   // This function encapsulates ALL the opcodes.
   // It has internal functions that group related sub-opcodes.

   function Action fa_exec(Decoded_Instr decoded);
      action

         // Values of Rs1 and Rs2 fields of the instr, unsigned
         Word    v1   = ((decoded.rs1 == 0) ? 0: regfile.sub(decoded.rs1));
         Word    v2   = ((decoded.rs2 == 0) ? 0: regfile.sub(decoded.rs2));

         // Values of Rs1 and Rs2 fields of the instr, signed versions
         Word_S  s_v1 = unpack(v1);
         Word_S  s_v2 = unpack(v2);

         // Value of CSR field of instr (if a valid CSR address)
         Maybe #(Word) m_v_csr = fv_read_csr(decoded.csr);

         // ----------------------------------------------------------------
         // Instructions for Upper Immediate

         function Action fa_exec_LUI();
            action
               Bit#(32)    v32   = { decoded.imm20_U, 12'h0 };
               Word_S      iv    = extend(unpack(v32));
               let         value = pack(iv);

               fa_finish_with_Rd(decoded.rd, value);
               if (cfg_verbose > 2) $display("[%7d] Decoded: PC = %h, lui %s, 0x%h", csr_cycle, decoded.pc, regNameABI[decoded.rd], value[31:12]);
            endaction
         endfunction: fa_exec_LUI

         function Action fa_exec_AUIPC();
            action
               Word_S  iv    = extend(unpack({ decoded.imm20_U, 12'b0}));
               Word_S  pc_s  = unpack(pc);
               Word    value = pack(pc_s + iv);

               fa_finish_with_Rd(decoded.rd, value);
               if (cfg_verbose > 2) $display("[%7d] Decoded: PC = %h, auipc %s, 0x%h", csr_cycle, decoded.pc, regNameABI[decoded.rd], value[31:12]);
            endaction
         endfunction: fa_exec_AUIPC

         // ----------------------------------------------------------------
         // Instructions for control-transfer

         function Action fa_exec_JAL();
            action
               Word_S offset  = extend(unpack(decoded.imm21_UJ));
               Addr   next_pc = pack(unpack(pc) + offset);

               fa_finish_jump(decoded.rd, fv_fall_through_pc(pc), next_pc);
               if (cfg_verbose > 2) $display("[%7d] Decoded: PC = %h, jal %s, 0x%h", csr_cycle, decoded.pc, regNameABI[decoded.rd], next_pc);
            endaction
         endfunction: fa_exec_JAL

         function Action fa_exec_JALR();
            action
               Word_S offset  = extend(unpack(decoded.imm12_I));
               Addr   next_pc = {truncateLSB(pack(s_v1 + offset)), 1'b0};

               fa_finish_jump(decoded.rd, fv_fall_through_pc(pc), next_pc);
               if (cfg_verbose > 2) $display("[%7d] Decoded: PC = %h, jalr %s, %s, %1d", csr_cycle, decoded.pc, regNameABI[decoded.rd], regNameABI[decoded.rs1], offset);
            endaction
         endfunction: fa_exec_JALR

         function Action fa_exec_BRANCH();
            action
               Word_S offset  = extend(unpack(decoded.imm13_SB));
               Word   next_pc = pack(unpack(pc) + offset);

               if      (decoded.funct3 == f3_BEQ)  fa_finish_cond_branch(v1  == v2,    next_pc);
               else if (decoded.funct3 == f3_BNE)  fa_finish_cond_branch(v1  != v2,    next_pc);
               else if (decoded.funct3 == f3_BLT)  fa_finish_cond_branch(s_v1 <  s_v2, next_pc);
               else if (decoded.funct3 == f3_BGE)  fa_finish_cond_branch(s_v1 >= s_v2, next_pc);
               else if (decoded.funct3 == f3_BLTU) fa_finish_cond_branch(v1  <  v2,    next_pc);
               else if (decoded.funct3 == f3_BGEU) fa_finish_cond_branch(v1  >= v2,    next_pc);
               else fa_finish_with_exception(pc, exc_code_ILLEGAL_INSTRUCTION, ?);

               if (cfg_verbose > 2) begin
                  $display("[%7d] Decoded: PC = %h, %s %s, %s, 0x%h", csr_cycle, decoded.pc,
                              case(decoded.funct3)
                                 f3_BEQ  : "beq";
                                 f3_BNE  : "bne";
                                 f3_BLT  : "blt";
                                 f3_BGE  : "bge";
                                 f3_BLTU : "bltu";
                                 f3_BGEU : "bgeu";
                              endcase,
                              regNameABI[decoded.rs1],
                              regNameABI[decoded.rs2],
                              next_pc
                  );
               end
            endaction
         endfunction: fa_exec_BRANCH

         // ----------------------------------------------------------------
         // LD and ST instructions.
         // Issue request here; will be completed in STATE_EXEC_LD/ST_RESPONSE

         function Action fa_exec_LD_Req();
            action
               Word_S  imm_s    = extend(unpack(decoded.imm12_I));
               Word    mem_addr = pack(s_v1 + imm_s);

               function Action fa_LD_Req(Mem_Data_Size sz);
                  action
                     let req = DMem_Req {mem_op:        MEM_OP_LOAD,
                                         mem_data_size: sz,
                                         addr:          mem_addr,
                                         data:          ?};
                     memory.dmem_req(req);
                     cpu_state <= STATE_EXEC_LD_RESPONSE;
                  endaction
               endfunction

               rg_mem_addr <= mem_addr;

               if      (decoded.funct3 == f3_LB)      fa_LD_Req(BITS8);
               else if (decoded.funct3 == f3_LBU)     fa_LD_Req(BITS8);
               else if (decoded.funct3 == f3_LH)      fa_LD_Req(BITS16);
               else if (decoded.funct3 == f3_LHU)     fa_LD_Req(BITS16);
               else if (decoded.funct3 == f3_LW)      fa_LD_Req(BITS32);
               else fa_finish_with_exception(pc, exc_code_ILLEGAL_INSTRUCTION, ?);
               if (cfg_verbose > 2) begin
                  $display("[%7d] Decoded: PC = %h, %s %s, %s, %1d", csr_cycle, decoded.pc,
                              case(decoded.funct3)
                                 f3_LB  : "lb";
                                 f3_LBU : "lbu";
                                 f3_LH  : "lh";
                                 f3_LHU : "lhu";
                                 f3_LW  : "lw";
                              endcase,
                              regNameABI[decoded.rd],
                              regNameABI[decoded.rs1],
                              imm_s
                  );
               end
            endaction
         endfunction: fa_exec_LD_Req

         function Action fa_exec_ST_Req();
            action
               Word_S  imm_s    = extend(unpack(decoded.imm12_S));
               Word    mem_addr = pack(s_v1 + imm_s);

               function Action fa_ST_req(Mem_Data_Size sz);
                  action
                     let req = DMem_Req {mem_op:        MEM_OP_STORE,
                                         mem_data_size: sz,
                                         addr:          mem_addr,
                                         data:          v2};
                     memory.dmem_req(req);
                     cpu_state <= STATE_EXEC_ST_RESPONSE;
                  endaction
               endfunction

               rg_mem_addr <= mem_addr;

               if      (decoded.funct3 == f3_SB)      fa_ST_req(BITS8);
               else if (decoded.funct3 == f3_SH)      fa_ST_req(BITS16);
               else if (decoded.funct3 == f3_SW)      fa_ST_req(BITS32);
               else fa_finish_with_exception(pc, exc_code_ILLEGAL_INSTRUCTION, ?);
            endaction
         endfunction: fa_exec_ST_Req

         // ----------------------------------------------------------------
         // Instructios for Register-Immediate alu ops

         function Action fa_exec_OP_IMM ();
            action
               Word                v2    = zeroExtend(decoded.imm12_I);
               Word_S              s_v2  = signExtend(unpack(decoded.imm12_I));
               Bit#(TLog#(XLEN))   shamt = truncate(decoded.imm12_I);

               if (cfg_verbose > 2) begin
                  $display("[%7d] Decoded: PC = %h, %s %s, %s, 0x%h", csr_cycle, decoded.pc,
                        case(decoded.funct3)
                           f3_ADDI: "addi";
                           f3_SLTI: "slti";
                           f3_SLTIU: "sltiu";
                           f3_XORI: "xori";
                           f3_ANDI: "andi";
                           f3_SLLI: "slli";
                           f3_SRxI: ((decoded.imm12_I[10] == 1'b0) ? "srli" : "srai");
                        endcase,
                        regNameABI[decoded.rd],
                        regNameABI[decoded.rs1],
                        decoded.imm12_I
                  );
               end


               if      (decoded.funct3 == f3_ADDI)  fa_finish_with_Rd(decoded.rd, pack(s_v1 + s_v2));
               else if (decoded.funct3 == f3_SLTI)  fa_finish_with_Rd(decoded.rd, ((s_v1 < s_v2) ? 1 : 0));
               else if (decoded.funct3 == f3_SLTIU) fa_finish_with_Rd(decoded.rd, ((v1  < v2)  ? 1 : 0));
               else if (decoded.funct3 == f3_XORI)  fa_finish_with_Rd(decoded.rd, pack(s_v1 ^ s_v2));
               else if (decoded.funct3 == f3_ORI)   fa_finish_with_Rd(decoded.rd, pack(s_v1 | s_v2));
               else if (decoded.funct3 == f3_ANDI)  fa_finish_with_Rd(decoded.rd, pack(s_v1 & s_v2));

               else if ((decoded.funct3 == f3_SLLI) && (decoded.imm12_I[10] == 1'b0))
                  fa_finish_with_Rd(decoded.rd, (v1 << shamt));


               // SRLI
               else if ((decoded.funct3 == f3_SRxI) && (decoded.imm12_I[10] == 1'b0))
                  fa_finish_with_Rd(decoded.rd, (v1 >> shamt));

               // SRAI
               else if ((decoded.funct3 == f3_SRxI) && (decoded.imm12_I[10] == 1'b1))
                  fa_finish_with_Rd(decoded.rd, pack(s_v1 >> shamt));

               else
                  fa_finish_with_exception(pc, exc_code_ILLEGAL_INSTRUCTION, ?);
            endaction
         endfunction: fa_exec_OP_IMM

         // ----------------------------------------------------------------
         // Instructios for Register-Register alu ops

         function Action fa_exec_OP();
            action
               Bit#(TLog#(XLEN)) shamt = truncate(v2);    // NOTE: upper bits are unspecified in spec

               if      (decoded.funct10 == f10_ADD)  fa_finish_with_Rd(decoded.rd, pack(s_v1 + s_v2));
               else if (decoded.funct10 == f10_SUB)  fa_finish_with_Rd(decoded.rd, pack(s_v1 - s_v2));
               else if (decoded.funct10 == f10_SLL)  fa_finish_with_Rd(decoded.rd, (v1 << shamt));
               else if (decoded.funct10 == f10_SLT)  fa_finish_with_Rd(decoded.rd, ((s_v1 < s_v2) ? 1 : 0));
               else if (decoded.funct10 == f10_SLTU) fa_finish_with_Rd(decoded.rd, ((v1  < v2)  ? 1 : 0));
               else if (decoded.funct10 == f10_XOR)  fa_finish_with_Rd(decoded.rd, pack(s_v1 ^ s_v2));
               else if (decoded.funct10 == f10_SRL)  fa_finish_with_Rd(decoded.rd, (v1 >> shamt));
               else if (decoded.funct10 == f10_SRA)  fa_finish_with_Rd(decoded.rd, pack(s_v1 >> shamt));
               else if (decoded.funct10 == f10_OR)   fa_finish_with_Rd(decoded.rd, pack(s_v1 | s_v2));
               else if (decoded.funct10 == f10_AND)  fa_finish_with_Rd(decoded.rd, pack(s_v1 & s_v2));

               else
                  fa_finish_with_exception(pc, exc_code_ILLEGAL_INSTRUCTION, ?);
            endaction
         endfunction: fa_exec_OP

         // ----------------------------------------------------------------
         // Instructions for MISC-MEM
         // Currently implemented as no-ops (todo: fix)

         function Action fa_exec_MISC_MEM();
            action
               // FENCE
               if (   (decoded.funct3 == f3_FENCE)
                   && (decoded.rd == 0)
                   && (decoded.rs1 == 0)
                   && (truncateLSB(decoded.imm12_I) == 4'b0)) begin   // same as instr[31:28] on Spec v 2.2
                                                                 fa_finish_with_no_output;
                                                              end

               // FENCE.I
               else if (   (decoded.funct3 == f3_FENCE_I)
                        && (decoded.rd == 0)
                        && (decoded.rs1 == 0)
                        && (decoded.imm12_I == 12'b0)) begin
                                                          fa_finish_with_no_output;
                                                       end

               else begin
                  fa_finish_with_exception(pc, exc_code_ILLEGAL_INSTRUCTION, ?);
               end
            endaction
         endfunction: fa_exec_MISC_MEM

         // ----------------------------------------------------------------
         // Instrucions for System-level ops

         function Action fa_exec_SYSTEM();
            action
               // SCALL/ECALL
               if      (   (decoded.funct3 == f3_PRIV)
                        && (decoded.imm12_I == f12_ECALL)
                        && (decoded.rs1 == 0)
                        && (decoded.rd == 0))
                  fa_finish_with_exception(pc, exc_code_ECALL_FROM_U, ?);

               // SBREAK/EBREAK
               else if (   (decoded.funct3 == f3_PRIV)
                        && (decoded.imm12_I == f12_EBREAK)
                        && (decoded.rs1 == 0)
                        && (decoded.rd == 0))
                  fa_finish_with_exception(pc, exc_code_BREAKPOINT, ?);

               // CSRRW
               else if (m_v_csr matches tagged Valid .csr_old_val &&& (decoded.funct3 == f3_CSRRW)) begin
                  fa_write_csr(decoded.csr, v1);
                  fa_finish_with_Rd(decoded.rd, csr_old_val);
               end

               // CSRRS
               else if (m_v_csr matches tagged Valid .csr_old_val &&& (decoded.funct3 == f3_CSRRS)) begin
                  if (decoded.rs1 != 0) begin
                     Word csr_new_val = (csr_old_val | v1);
                     fa_write_csr(decoded.csr, csr_new_val);
                  end
                  fa_finish_with_Rd(decoded.rd, csr_old_val);
               end

               // CSRRC
               else if (m_v_csr matches tagged Valid .csr_old_val &&& (decoded.funct3 == f3_CSRRC)) begin
                  if (decoded.rs1 != 0) begin
                     Word csr_new_val = (csr_old_val & (~ v1));
                     fa_write_csr(decoded.csr, csr_new_val);
                  end
                  fa_finish_with_Rd(decoded.rd, csr_old_val);
               end

               // CSRRWI
               else if (m_v_csr matches tagged Valid .csr_old_val &&& (decoded.funct3 == f3_CSRRW)) begin
                  let v1 = zeroExtend (decoded.rs1);
                  fa_write_csr(decoded.csr, v1);
                  fa_finish_with_Rd(decoded.rd, csr_old_val);
               end

               // CSRRSI
               else if (m_v_csr matches tagged Valid .csr_old_val &&& (decoded.funct3 == f3_CSRRS)) begin
                  Word v1 = zeroExtend (decoded.rs1);
                  if (v1 != 0) begin
                     Word csr_new_val = (csr_old_val | v1);
                     fa_write_csr(decoded.csr, csr_new_val);
                  end
                  fa_finish_with_Rd(decoded.rd, csr_old_val);
               end

               // CSRRCI
               else if (m_v_csr matches tagged Valid .csr_old_val &&& (decoded.funct3 == f3_CSRRC)) begin
                  Word v1 = zeroExtend (decoded.rs1);
                  if (v1 != 0) begin
                     Word csr_new_val = (csr_old_val & (~ v1));
                     fa_write_csr(decoded.csr, csr_new_val);
                  end
                  fa_finish_with_Rd(decoded.rd, csr_old_val);
               end

               else begin
                  fa_finish_with_exception(pc, exc_code_ILLEGAL_INSTRUCTION, ?);
               end
            endaction
         endfunction: fa_exec_SYSTEM

         // ----------------------------------------------------------------
         // Main body of fa_exec(), dispatching to the sub functions
         // based on major OPCODE

         if      (decoded.opcode == op_LUI)      fa_exec_LUI();
         else if (decoded.opcode == op_AUIPC)    fa_exec_AUIPC();
         else if (decoded.opcode == op_JAL)      fa_exec_JAL();
         else if (decoded.opcode == op_JALR)     fa_exec_JALR();
         else if (decoded.opcode == op_BRANCH)   fa_exec_BRANCH();
         else if (decoded.opcode == op_LOAD)     fa_exec_LD_Req();
         else if (decoded.opcode == op_STORE)    fa_exec_ST_Req();
         else if (decoded.opcode == op_OP_IMM)   fa_exec_OP_IMM();
         else if (decoded.opcode == op_OP)       fa_exec_OP();
         else if (decoded.opcode == op_MISC_MEM) fa_exec_MISC_MEM();
         else if (decoded.opcode == op_SYSTEM)   fa_exec_SYSTEM();
         else fa_finish_with_exception(pc, exc_code_ILLEGAL_INSTRUCTION, ?);
      endaction
   endfunction: fa_exec



   // ================================================================
   // The CPU's top-level logic

   // ---------------- FETCH
   // Issue instruction request

   rule rl_fetch(cpu_state == STATE_FETCH);
      if (cfg_verbose > 1) $display("[%7d] rl_fetch: PC = 0x%08h", csr_cycle, pc);

      memory.imem_req(pc);
      cpu_state <= STATE_EXEC;
   endrule

   // ---------------- EXECUTE
   // Receive instruction from IMem; handle exception if any, else execute it;
   // for LD/ST, issue request and move to LD_RESPONSE/ST_RESPONSE state)

   rule rl_exec(cpu_state == STATE_EXEC);
      let imem_resp <- memory.imem_resp;

      if (imem_resp matches tagged Valid .instr) begin
         if (cfg_verbose > 1) $display("[%7d] rl_exec: PC = 0x%08h, instr = 0x%08h", csr_cycle, pc, instr);
         rg_instr <= instr;

         if (cfg_verbose != 0) $display("[%7d] fa_exec: instr 0x%08h", csr_cycle, instr);

         // ----------------------------------------------------------------
         // Instruction decode

         Decoded_Instr decoded = fv_decode(pc, instr);
         fa_exec(decoded);
      end
   endrule

   // ---------------- LD-responses from DMem

   rule rl_exec_LD_response(cpu_state == STATE_EXEC_LD_RESPONSE);
      if (cfg_verbose > 1) $display("[%7d] rl_exec_LD_response: ", csr_cycle);

      Decoded_Instr decoded = fv_decode(?, rg_instr);

      let resp <- memory.dmem_resp;

      case (resp) matches
         tagged DMem_Resp_Exception .exc_code: fa_finish_with_exception(pc, exc_code, rg_mem_addr);
         tagged DMem_Resp_Ok .u: begin
            case (decoded.funct3)
               f3_LB:  begin
                          Int#(8)   s8    = unpack(truncate(u));
                          Word_S    s     = signExtend(s8);
                          Word      value = pack(s);
                          fa_finish_with_Rd(decoded.rd, value);
                       end
               f3_LBU: begin
                          Bit#(8)   u8    = truncate(u);
                          Word      value = zeroExtend(u8);
                          fa_finish_with_Rd(decoded.rd, value);
                       end
               f3_LH:  begin
                          Int#(16)  s16   = unpack(truncate(u));
                          Word_S    s     = signExtend(s16);
                          Word      value = pack(s);
                          fa_finish_with_Rd(decoded.rd, value);
                       end
               f3_LHU: begin
                          Bit#(16)  u16   = truncate(u);
                          Word      value = zeroExtend(u16);
                          fa_finish_with_Rd(decoded.rd, value);
                       end
               f3_LW:  begin
                          Int#(32)  s32   = unpack(truncate(u));
                          Word_S    s     = signExtend(s32);
                          Word      value = pack(s);
                          fa_finish_with_Rd(decoded.rd, value);
                       end
               // Note: request has already checked that 'default' case is impossible
            endcase
            if (cfg_verbose > 1) $display("RISCV_Spec.rl_exec_LD_response: %08h", u);
         end
      endcase
   endrule

   // ---------------- ST-responses from DMem

   rule rl_exec_ST_response(cpu_state == STATE_EXEC_ST_RESPONSE);
      if (cfg_verbose > 1) $display("[%7d] rl_exec_ST_response: ", csr_cycle);

      DMem_Resp resp <- memory.dmem_resp;

      case (resp) matches
         tagged DMem_Resp_Exception .exc_code: fa_finish_with_exception(pc, exc_code, rg_mem_addr);
         tagged DMem_Resp_Ok        .u:        fa_finish_with_no_output;
      endcase
   endrule

   // ---------------- FINISH: increment csr_instret or record explicit CSRRx update of csr_instret

   rule rl_finish(cpu_state == STATE_FINISH);
      if (cfg_verbose > 1) $display("[%7d] rl_finish: ", csr_cycle);

      csr_instret <= csr_instret + 1;
      cpu_state   <= STATE_FETCH;
   endrule

   // ---------------- Increment csr_cycle according to external oracles

   rule rl_incr_cycle;
      csr_cycle <= csr_cycle + 1;
   endrule

   // ----------------------------------------------------------------
   // INTERFACE

   method Action start(Addr initial_pc);
      pc        <= initial_pc;
      cpu_state <= STATE_FETCH;
   endmethod

   method Bool halted;
      return(cpu_state == STATE_HALT);
   endmethod

   method ActionValue#(Word) cpuToHost() if (csr_dcsr matches tagged Valid .ret);
      csr_dcsr <= tagged Invalid;
      return ret;
   endmethod
endmodule

// ================================================================

(* synthesize *)
module mkRISCV(RISCV_IFC);
   (* hide *) let _m <- _mkRISCV(3);
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
         end
      endcase
   endrule

   rule do_start(cycles == 0);
      dut.start('h200);
   endrule

   rule do_check(cycles > 0);
      let halted = dut.halted;

      if (halted) begin
         $fdisplay(stderr, "CPU Task Finished");
         $finish;
      end
   endrule
endmodule
`endif // TESTBENCH

