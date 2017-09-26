// Copyright (c) 2017 Min Chen.  All Rights Reserved.
// Author: Min Chen

// ================================================================
// BSV library imports

import RegFile       :: *;    // For RISC-V GPRs
import ConfigReg     :: *;
import FIFOF         :: *;
import SpecialFIFOs  :: *;

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

typedef union tagged {
   struct { Word instr;  Addr pc; } Resp;
   void                             Invalid;
} IMem_Resp deriving(Bits);

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

typedef Maybe#(Word) DMem_Resp;


// ----------------
// Memory interface reference design

module mkMemory#(Reg#(Bit#(64)) cycles)(Memory_IFC);
   BRAM_PORT#(Bit#(14), Word)             imem <- mkBRAMCore1Load(valueOf(TExp#(14)), False, "mem.vmh", False);
   BRAM_DUAL_PORT_BE#(Bit#(15), Word, 4)  dmem <- mkBRAMCore2BELoad(valueOf(TExp#(15)), False, "mem.vmh.D", False);
   Reg#(Maybe#(Addr))                     imem_rd  <- mkDReg(tagged Invalid);
   Reg#(Bool)                             dmem_rd  <- mkDReg(False);
   Reg#(Bit#(2))                          rg_shift <- mkRegU;
   Reg#(Mem_Data_Size)                    rg_size  <- mkRegU;

   method Action imem_req(Addr addr);
      let phyAddr = addr - imemSt;
      imem.put(False, truncate(phyAddr >> 2), ?);
      imem_rd  <= tagged Valid addr;
      //$display("[%7d] [IMEM] ReqAddr = 0x%08h", cycles, addr);
   endmethod

   method ActionValue#(IMem_Resp) imem_resp();
      //$display("[%7d] [IMEM] Addr = 0x%08h", cycles, fromMaybe(?, imem_rd));
      if (isValid(imem_rd)) begin
         let addr = fromMaybe(?, imem_rd);
         let instr = imem.read();
         return tagged Resp {instr:instr, pc:addr};
      end
      else begin
         return tagged Invalid;
      end
   endmethod

   method Action dmem_req(DMem_Req req);
      let phyAddr = (req.addr - dmemSt);
      Word val = req.data;
      Bit#(Bits_per_Word_Byte_Index) shift = truncate(req.addr);
      Bit#(Bytes_per_Word) mask = 4'b1111;
      Word pad = ?;

      rg_shift <= shift;
      rg_size  <= req.mem_data_size;

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
      dmem_rd <= !(req.mem_op == MEM_OP_STORE);
      //$display("[DMEM] Addr = 0x%08h", req.addr);
   endmethod

   method ActionValue#(DMem_Resp) dmem_resp;
      Word v = dmem.a.read();

      case(rg_shift)
         0: v = v;
         1: v = {?, v[31: 8]};
         2: v = {?, v[31:16]};
         3: v = {?, v[31:24]};
      endcase

      return dmem_rd ? tagged Valid v : tagged Invalid;
   endmethod
endmodule


// ----------------------------------------------------------------
// This interface is an argument to the 'mkRISCV_Spec' module,
// and is used insided the module to access memory.
// MMUs, caches etc. are outside this boundary.

interface Memory_IFC;
   method Action                  imem_req(Addr addr);
   method ActionValue#(IMem_Resp)               imem_resp;

   method Action                  dmem_req(DMem_Req req);
   method ActionValue#(DMem_Resp) dmem_resp;
endinterface

// ================================================================
// This interfacce is offered by the 'mkRISCV_Spec' module to the environment.
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

module _mkRISCV#(Bit#(3) cfg_verbose)(RISCV_IFC);

   // CPU state
   Reg#(Bool)  cpu_enabled <- mkReg(False);

   // Program counter
   Reg#(Word)     pc       <- mkRegU;
   RWire#(Word)   rw_pc    <- mkRWire;
   Reg#(Word)     pcEpoch  <- mkConfigRegU;

   // General Purpose Registers
   RegFile#(RegName, Word) rf_GPRs  <- mkRegFileWCF(0, ~0);

   // CSRs
   Reg#(Bit#(64))    csr_cycle   <- mkConfigReg(0);
   Reg#(Bit#(64))    csr_instret <- mkConfigReg(0);

   // internal components
   Memory_IFC           memory   <- mkMemory(csr_cycle);

   // ----------------
   // These CSRs are technically not present in the user-mode ISA.

   Reg#(Word)  csr_mepc       <- mkRegU;
   Reg#(Word)  csr_mcause     <- mkRegU;
   Reg#(Word)  csr_mbadaddr   <- mkRegU;

   Reg#(Maybe#(Word)) csr_dcsr <- mkDReg(tagged Invalid);


   // ----------------------------------------------------------------
   // Non-architectural state, for this model

   FIFOF#(Decoded_Instr)   fifo_f2e    <- mkPipelineFIFOF;
   FIFOF#(Exec2Wb_t)       fifo_e2w    <- mkPipelineFIFOF;

   // ----------------------------------------------------------------
   // Scoreboard map
   Reg#(Bool)  rg_scoreGPRs[numRegs][2];
   for(Integer i = 0; i < numRegs; i = i + 1)
      rg_scoreGPRs[i] <- mkCReg(2, False);

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
         if (csr_addr == csr_DCSR) begin
            csr_dcsr <= tagged Valid csr_value;
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
         if (cfg_verbose != 0) begin
            $display("[%7d] fa_do_exception: epc = 0x%0h, exc_code = 0x%0h, badaddr = 0x%0h", csr_cycle, pcEpoch, exc_code, badaddr);
         end

         csr_mepc     <= pcEpoch;
         csr_mcause   <= { 1'b0, 0, exc_code };
         csr_mbadaddr <= badaddr;

         $finish;
      endaction
   endfunction

   // ----------------
   // Finish instr with no output (no Rd-write): set PC, go to FETCH state

   function Action fa_finish_with_no_output();
      action
         //pc[1]       <= fv_fall_through_pc(pcEpoch);
         pcEpoch     <= fv_fall_through_pc(pcEpoch);
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

   function Action fa_finish_with_Ld(RegName rd, Bit#(3) funct3);
      action
         fifo_e2w.enq( Exec2Wb_t {rd: rd, rd_value: tagged Funct3 funct3} );
         fa_finish_with_no_output;
      endaction
   endfunction

   // ----------------
   // Finish jump instrs; write Rd, set PC, go to FETCH state

   function Action fa_finish_jump(RegName rd, Word rd_value, Addr next_pc);
      action
         fifo_e2w.enq( Exec2Wb_t {rd: rd, rd_value: tagged Value rd_value} );
         rw_pc.wset(next_pc);
         pcEpoch  <= next_pc;
      endaction
   endfunction

   // ----------------
   // Finish conditional branch instr: set PC, go to FETCH state

   function Action fa_finish_cond_branch(Bool condition_taken, Addr next_pc);
      action
         rw_pc.wset(condition_taken ? next_pc : fv_fall_through_pc(pcEpoch));
         pcEpoch  <= (condition_taken ? next_pc : fv_fall_through_pc(pcEpoch));
      endaction
   endfunction

   // ----------------------------------------------------------------
   // Instruction execution
   // This function encapsulates ALL the opcodes.
   // It has internal functions that group related sub-opcodes.

   function Action fa_exec(Decoded_Instr decoded, Decoded_Fields fields);
      action

         // Values of Rs1 and Rs2 fields of the instr, unsigned
         Word v1 = decoded.v1;
         Word v2 = decoded.v2;

         // Values of Rs1 and Rs2 fields of the instr, signed versions
         Word_S  s_v1 = unpack(v1);
         Word_S  s_v2 = unpack(v2);

         // Value of CSR field of instr (if a valid CSR address)
         Maybe #(Word) m_v_csr = fv_read_csr(fields.csr);

         // ----------------------------------------------------------------
         // Instructions for Upper Immediate

         function Action fa_exec_LUI();
            action
               Bit#(32)    v32   = { fields.imm20_U, 12'h0 };
               Word_S      iv    = extend(unpack(v32));
               let         value = pack(iv);

               fa_finish_with_Rd(fields.rd, value);
               if (cfg_verbose > 2) $display("[%7d] fa_exec  : PC = 0x%h, *** lui %s, 0x%h", csr_cycle, decoded.pc, regNameABI[fields.rd], value[31:12]);
            endaction
         endfunction: fa_exec_LUI

         function Action fa_exec_AUIPC();
            action
               Word_S  iv    = extend(unpack({ fields.imm20_U, 12'b0}));
               Word_S  pc_s  = unpack(pcEpoch);
               Word    value = pack(pc_s + iv);

               fa_finish_with_Rd(fields.rd, value);
               if (cfg_verbose > 2) $display("[%7d] fa_exec  : PC = 0x%h, *** auipc %s, 0x%h", csr_cycle, decoded.pc, regNameABI[fields.rd], value[31:12]);
            endaction
         endfunction: fa_exec_AUIPC

         // ----------------------------------------------------------------
         // Instructions for control-transfer

         function Action fa_exec_JAL();
            action
               Word_S offset  = extend(unpack(fields.imm21_UJ));
               Addr   next_pc = pack(unpack(pcEpoch) + offset);

               fa_finish_jump(fields.rd, fv_fall_through_pc(pcEpoch), next_pc);
               if (cfg_verbose > 2) $display("[%7d] fa_exec  : PC = 0x%h, *** jal %s, 0x%h", csr_cycle, decoded.pc, regNameABI[fields.rd], next_pc);
            endaction
         endfunction: fa_exec_JAL

         function Action fa_exec_JALR();
            action
               Word_S offset  = extend(unpack(fields.imm12_I));
               Addr   next_pc = {truncateLSB(pack(s_v1 + offset)), 1'b0};

               fa_finish_jump(fields.rd, fv_fall_through_pc(pcEpoch), next_pc);
               if (cfg_verbose > 2) $display("[%7d] fa_exec  : PC = 0x%h, *** jalr %s, %s, %1d", csr_cycle, decoded.pc, regNameABI[fields.rd], regNameABI[fields.rs1], offset);
            endaction
         endfunction: fa_exec_JALR

         function Action fa_exec_BRANCH();
            action
               Word_S offset  = extend(unpack(fields.imm13_SB));
               Word   next_pc = pack(unpack(pcEpoch) + offset);

               case(decoded.opcode)
                  OP_BEQ   :  fa_finish_cond_branch(v1  == v2,    next_pc);
                  OP_BNE   :  fa_finish_cond_branch(v1  != v2,    next_pc);
                  OP_BLT   :  fa_finish_cond_branch(s_v1 <  s_v2, next_pc);
                  OP_BGE   :  fa_finish_cond_branch(s_v1 >= s_v2, next_pc);
                  OP_BLTU  :  fa_finish_cond_branch(v1  <  v2,    next_pc);
                  /* OP_BGEU */
                  default  :  fa_finish_cond_branch(v1  >= v2,    next_pc);
               endcase

               if (cfg_verbose > 2) begin
                  $display("[%7d] fa_exec  : PC = 0x%h, *** %s %s, %s, 0x%h", csr_cycle, decoded.pc,
                              case(decoded.opcode)
                                 OP_BEQ  : "beq";
                                 OP_BNE  : "bne";
                                 OP_BLT  : "blt";
                                 OP_BGE  : "bge";
                                 OP_BLTU : "bltu";
                                 OP_BGEU : "bgeu";
                              endcase,
                              regNameABI[fields.rs1],
                              regNameABI[fields.rs2],
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
               Word_S  imm_s    = extend(unpack(fields.imm12_I));
               Word    mem_addr = pack(s_v1 + imm_s);

               function Action fa_LD_Req(Mem_Data_Size sz);
                  action
                     let req = DMem_Req {mem_op:        MEM_OP_LOAD,
                                         mem_data_size: sz,
                                         addr:          mem_addr,
                                         data:          ?};
                     memory.dmem_req(req);
                     fa_finish_with_Ld(fields.rd, fields.funct3);
                  endaction
               endfunction

               case(decoded.opcode)
                  OP_LB    :  fa_LD_Req(BITS8);
                  OP_LBU   :  fa_LD_Req(BITS8);
                  OP_LH    :  fa_LD_Req(BITS16);
                  OP_LHU   :  fa_LD_Req(BITS16);
                  /*OP_LW*/
                  default  :  fa_LD_Req(BITS32);
               endcase

               if (cfg_verbose > 2) begin
                  $display("[%7d] fa_exec  : PC = 0x%h, *** %s %s, %s, %1d", csr_cycle, decoded.pc,
                              case(decoded.opcode)
                                 OP_LB  : "lb";
                                 OP_LBU : "lbu";
                                 OP_LH  : "lh";
                                 OP_LHU : "lhu";
                                 OP_LW  : "lw";
                              endcase,
                              regNameABI[fields.rd],
                              regNameABI[fields.rs1],
                              imm_s
                  );
               end
            endaction
         endfunction: fa_exec_LD_Req

         function Action fa_exec_ST_Req();
            action
               Word_S  imm_s    = extend(unpack(fields.imm12_S));
               Word    mem_addr = pack(s_v1 + imm_s);

               function Action fa_ST_req(Mem_Data_Size sz);
                  action
                     let req = DMem_Req {mem_op:        MEM_OP_STORE,
                                         mem_data_size: sz,
                                         addr:          mem_addr,
                                         data:          v2};
                     memory.dmem_req(req);
                     fa_finish_with_no_output;
                  endaction
               endfunction

               case(decoded.opcode)
                  OP_SB    :  fa_ST_req(BITS8);
                  OP_SH    :  fa_ST_req(BITS16);
                  /*OP_SW*/
                  default  :  fa_ST_req(BITS32);
               endcase

               if (cfg_verbose > 2) begin
                  $display("[%7d] fa_exec  : PC = 0x%h, *** %s %s, %s, %1d", csr_cycle, decoded.pc,
                              case(decoded.opcode)
                                 OP_SB  : "sb";
                                 OP_SH  : "sh";
                                 OP_SW  : "sw";
                              endcase,
                              regNameABI[fields.rd],
                              regNameABI[fields.rs1],
                              imm_s
                  );
               end
            endaction
         endfunction: fa_exec_ST_Req

         // ----------------------------------------------------------------
         // Instructios for Register-Immediate alu ops

         function Action fa_exec_OP_IMM();
            action
               Word                v2    = zeroExtend(fields.imm12_I);
               Word_S              s_v2  = signExtend(unpack(fields.imm12_I));
               Bit#(TLog#(XLEN))   shamt = truncate(fields.imm12_I);

               case(decoded.opcode)
                  OP_ADDI  :  fa_finish_with_Rd(fields.rd, pack(s_v1 + s_v2));
                  OP_SLTI  :  fa_finish_with_Rd(fields.rd, ((s_v1 < s_v2) ? 1 : 0));
                  OP_SLTIU :  fa_finish_with_Rd(fields.rd, ((v1  < pack(s_v2))  ? 1 : 0));
                  OP_XORI  :  fa_finish_with_Rd(fields.rd, pack(s_v1 ^ s_v2));
                  OP_ORI   :  fa_finish_with_Rd(fields.rd, pack(s_v1 | s_v2));
                  OP_ANDI  :  fa_finish_with_Rd(fields.rd, pack(s_v1 & s_v2));
                  OP_SLLI  :  fa_finish_with_Rd(fields.rd, (v1 << shamt));
                  OP_SRLI  :  fa_finish_with_Rd(fields.rd, (v1 >> shamt));
                  /*OP_SRAI*/
                  default  :  fa_finish_with_Rd(fields.rd, pack(s_v1 >> shamt));
               endcase

               if (cfg_verbose > 2) begin
                  $display("[%7d] fa_exec  : PC = 0x%h, *** %s %s, %s, 0x%h", csr_cycle, decoded.pc,
                        case(decoded.opcode)
                           OP_ADDI  : "addi";
                           OP_SLTI  : "slti";
                           OP_SLTIU : "sltiu";
                           OP_XORI  : "xori";
                           OP_ANDI  : "andi";
                           OP_SLLI  : "slli";
                           OP_SRLI  : "srli";
                           OP_SRAI  : "srai";
                        endcase,
                        regNameABI[fields.rd],
                        regNameABI[fields.rs1],
                        fields.imm12_I
                  );
               end
            endaction
         endfunction: fa_exec_OP_IMM

         // ----------------------------------------------------------------
         // Instructios for Register-Register alu ops

         function Action fa_exec_OP();
            action
               Bit#(TLog#(XLEN)) shamt = truncate(v2);    // NOTE: upper bits are unspecified in spec

               case(decoded.opcode)
                  OP_ADD   :  fa_finish_with_Rd(fields.rd, pack(s_v1 + s_v2));
                  OP_SUB   :  fa_finish_with_Rd(fields.rd, pack(s_v1 - s_v2));
                  OP_SLL   :  fa_finish_with_Rd(fields.rd, (v1 << shamt));
                  OP_SLT   :  fa_finish_with_Rd(fields.rd, ((s_v1 < s_v2) ? 1 : 0));
                  OP_SLTU  :  fa_finish_with_Rd(fields.rd, ((v1  < v2)  ? 1 : 0));
                  OP_XOR   :  fa_finish_with_Rd(fields.rd, pack(s_v1 ^ s_v2));
                  OP_SRL   :  fa_finish_with_Rd(fields.rd, (v1 >> shamt));
                  OP_SRA   :  fa_finish_with_Rd(fields.rd, pack(s_v1 >> shamt));
                  OP_OR    :  fa_finish_with_Rd(fields.rd, pack(s_v1 | s_v2));
                  /*OP_AND*/
                  default  :  fa_finish_with_Rd(fields.rd, pack(s_v1 & s_v2));
               endcase

               if (cfg_verbose > 2) begin
                  $display("[%7d] fa_exec  : PC = 0x%h, *** %s %s, %s, %s", csr_cycle, decoded.pc,
                        case(decoded.opcode)
                           OP_ADD  : "add";
                           OP_SUB  : "sub";
                           OP_SLL  : "sll";
                           OP_SLT  : "slt";
                           OP_SLTU : "sltu";
                           OP_XOR  : "xor";
                           OP_SRL  : "srl";
                           OP_SRA  : "sra";
                           OP_OR   : "or";
                           OP_AND  : "and";
                        endcase,
                        regNameABI[fields.rd],
                        regNameABI[fields.rs1],
                        regNameABI[fields.rs2]
                  );
               end
            endaction
         endfunction: fa_exec_OP

         // ----------------------------------------------------------------
         // Instructions for MISC-MEM
         // Currently implemented as no-ops (todo: fix)

         function Action fa_exec_MISC_MEM();
            action
               case(decoded.opcode)
                  OP_FENCE :  fa_finish_with_no_output;
                  /*OP_FENCE_I*/
                  default  :  fa_finish_with_no_output;
               endcase

               if (cfg_verbose > 2) $display("[%7d] fa_exec  : PC = 0x%h, *** %s (ignore)", csr_cycle, decoded.pc, decoded.opcode == OP_FENCE ? "fence" : "fence.i");
            endaction
         endfunction: fa_exec_MISC_MEM

         // ----------------------------------------------------------------
         // Instrucions for System-level ops

         function Action fa_exec_SYSTEM();
            action
               let csr_old_val = fromMaybe(?, m_v_csr);

               case(decoded.opcode)
                  OP_CSRRW :  begin
                                 fa_write_csr(fields.csr, v1);
                                 fa_finish_with_Rd(fields.rd, csr_old_val);
                              end

                  OP_CSRRS :  begin
                                 if (fields.rs1 != 0) begin
                                    Word csr_new_val = (csr_old_val | v1);
                                    fa_write_csr(fields.csr, csr_new_val);
                                 end
                                 fa_finish_with_Rd(fields.rd, csr_old_val);
                              end

                  /*OP_CSRRC*/
                  default  :  begin
                                 if (fields.rs1 != 0) begin
                                    Word csr_new_val = (csr_old_val & (~ v1));
                                    fa_write_csr(fields.csr, csr_new_val);
                                 end
                                 fa_finish_with_Rd(fields.rd, csr_old_val);
                              end
               endcase

               if (cfg_verbose > 2) begin
                  if ( (decoded.opcode == OP_CSRRS) && (fields.csr == csr_CYCLE) )
                     $display("[%7d] fa_exec  : PC = 0x%h, *** rdcycle %s", csr_cycle, decoded.pc, regNameABI[fields.rd]);
                  else if ( (decoded.opcode == OP_CSRRS) && (fields.csr == csr_INSTRET) )
                     $display("[%7d] fa_exec  : PC = 0x%h, *** rdinstret %s", csr_cycle, decoded.pc, regNameABI[fields.rd]);
                  else if ( (decoded.opcode == OP_CSRRW) && (fields.csr == csr_DCSR) )
                     $display("[%7d] fa_exec  : PC = 0x%h, *** csrw dcsr, %s", csr_cycle, decoded.pc, regNameABI[fields.rs1]);
                  else begin
                     $display("[%7d] fa_exec  : PC = 0x%h, *** %s %s, 0x%h, %s", csr_cycle, decoded.pc,
                           case(decoded.opcode)
                              OP_CSRRW : "csrrw";
                              OP_CSRRS : "csrrs";
                              OP_CSRRC : "csrrc";
                              //OP_CSRRWI : "csrrwi";
                              //OP_CSRRSI : "csrrsi";
                              //OP_CSRRCI : "csrrci";
                              default  : "Unsupport";
                           endcase,
                           regNameABI[fields.rd],
                           fields.csr,
                           regNameABI[fields.rs1]
                     );
                  end
               end
            endaction
         endfunction: fa_exec_SYSTEM

         // ----------------------------------------------------------------
         // Main body of fa_exec(), dispatching to the sub functions
         // based on major OPCODE

         case(decoded.opcode)
            OP_LUI      :  fa_exec_LUI();
            OP_AUIPC    :  fa_exec_AUIPC();
            OP_JAL      :  fa_exec_JAL();
            OP_JALR     :  fa_exec_JALR();

            OP_BEQ      ,
            OP_BNE      ,
            OP_BLT      ,
            OP_BGE      ,
            OP_BLTU     ,
            OP_BGEU     :  fa_exec_BRANCH();

            OP_LB       ,
            OP_LBU      ,
            OP_LH       ,
            OP_LHU      ,
            OP_LW       :  fa_exec_LD_Req();

            OP_SB       ,
            OP_SH       ,
            OP_SW       :  fa_exec_ST_Req();

            OP_ADDI     ,
            OP_SLTI     ,
            OP_SLTIU    ,
            OP_XORI     ,
            OP_ORI      ,
            OP_ANDI     ,
            OP_SLLI     ,
            OP_SRLI     ,
            OP_SRAI     :  fa_exec_OP_IMM();

            OP_ADD      ,
            OP_SUB      ,
            OP_SLL      ,
            OP_SLT      ,
            OP_SLTU     ,
            OP_XOR      ,
            OP_SRL      ,
            OP_SRA      ,
            OP_OR       ,
            OP_AND      :  fa_exec_OP();

            OP_FENCE    ,
            OP_FENCE_I  :  fa_exec_MISC_MEM();

            OP_CSRRW    ,
            OP_CSRRS    ,
            OP_CSRRC    :  fa_exec_SYSTEM();

            default     :  fa_finish_with_exception(exc_code_ILLEGAL_INSTRUCTION, ?);
         endcase
      endaction
   endfunction: fa_exec



   // ================================================================
   // The CPU's top-level logic

   // ---------------- FETCH
   // Issue instruction request and decode

   // ----------------------------------------------------------------
   // Instruction fetch
   //(* conflict_free="rl_fetch, rl_exec, rl_write_back" *)
   rule rl_fetch(cpu_enabled);
      if (cfg_verbose > 1) $display("[%7d] rl_fetch : Read instruction PC = 0x%08h", csr_cycle, pc);
      memory.imem_req(pc);

      let next_pc = fromMaybe(pc + 4, rw_pc.wget);
      pc <= next_pc;
   endrule

   // ----------------------------------------------------------------
   // Instruction decode
   rule rl_decode;
      let x <- memory.imem_resp;
      if (x matches tagged Resp .resp) begin
      let xPC = resp.pc;
      let instr = resp.instr;

      if (cfg_verbose > 1) $display("[%7d] rl_decode: PC = 0x%08h, instr = %h", csr_cycle, xPC, instr);
      Decoded_Instr decoded = fv_decode(xPC, instr, rf_GPRs);
      fifo_f2e.enq( decoded );
   end
   endrule


   // ---------------- EXECUTE
   // Receive instruction from IMem; handle exception if any, else execute it;

   //(* no_implicit_conditions *)
   rule rl_exec(fifo_f2e.notEmpty && fifo_e2w.notFull);
      let decoded = fifo_f2e.first;
      fifo_f2e.deq;

      // ----------------------------------------------------------------
      // Instruction fields decode
      Decoded_Fields fields   = fv_decode_fields(decoded.instr);


      // Calculate dependency register
      let score1 = ?;
      let score2 = ?;
      
      case(fields.opcode7)
         op_LUI, op_AUIPC, op_JAL, op_MISC_MEM  : begin
            score1 = False;
            score2 = False;
         end
         op_JALR, op_LOAD, op_OP_IMM, op_SYSTEM : begin
            score1 = fields.rs1 != 0 ? rg_scoreGPRs[fields.rs1][1] : False;
            score2 = False;
         end
         default/*op_BRANCH, op_OP, op_STORE*/ : begin
            score1 = fields.rs1 != 0 ? rg_scoreGPRs[fields.rs1][1] : False;
            score2 = fields.rs2 != 0 ? rg_scoreGPRs[fields.rs2][1] : False;
         end
      endcase

      let score_conflict = score1 || score2;

      if (cfg_verbose > 1) $display("[%7d] rl_exec  : PC = 0x%08h, instr = 0x%08h", csr_cycle, decoded.pc, decoded.instr);
      if (!score_conflict && decoded.pc == pcEpoch) begin
         if (cfg_verbose > 0) $display("[%7d] fa_exec  : PC = 0x%08h, instr = 0x%08h", csr_cycle, decoded.pc, decoded.instr);

         // Update dependency flag for $rd
         if (  fields.opcode7 != op_BRANCH
            && fields.opcode7 != op_STORE) begin
               rg_scoreGPRs[fields.rd][1] <= True;
         end

         if (cfg_verbose > 2) begin
            $write("[%7d] rl_exec: Scoreboard = [", csr_cycle);
            for(Integer i = 0; i < numRegs; i = i + 1) begin
               $write("%1d, ", rg_scoreGPRs[i][1] ? 1 : 0);
            end
            $write("], dst = %d, rs1 = %d, rs2 = %d\n", fields.rd, fields.rs1, fields.rs2);
         end

         fa_exec(decoded, fields);

         // ---------------- FINISH: increment csr_instret or record explicit CSRRx update of csr_instret
         csr_instret <= csr_instret + 1;
      end
      else begin
         if (cfg_verbose > 1) $display("[%7d] rl_exec  : Bypass PC = 0x%08h, instr = 0x%h, Epoch = 0x%08h", csr_cycle, decoded.pc, decoded.instr, pcEpoch);
      end
   endrule

   // ---------------- RegFile & DMem Write Back
   rule rl_write_back(fifo_e2w.notEmpty);
      let x = fifo_e2w.first;
      fifo_e2w.deq;
      let rd = x.rd;
      let rd_value = ?;

      case(x.rd_value) matches
         tagged Funct3 .funct3: begin
            let resp <- memory.dmem_resp;

            if (cfg_verbose > 0 && !isValid(resp)) begin
               $display("[%7d] rl_write_back: Memory read failed", csr_cycle);
               $finish;
            end

            let u = fromMaybe(?, resp);

            case (funct3)
               f3_LB:  begin
                           Int#(8)   s8    = unpack(truncate(u));
                           Word_S    s     = signExtend(s8);
                           Word      value = pack(s);
                           rd_value = value;
                       end
               f3_LBU: begin
                           Bit#(8)   u8    = truncate(u);
                           Word      value = zeroExtend(u8);
                           rd_value = value;
                       end
               f3_LH:  begin
                           Int#(16)  s16   = unpack(truncate(u));
                           Word_S    s     = signExtend(s16);
                           Word      value = pack(s);
                           rd_value = value;
                       end
               f3_LHU: begin
                           Bit#(16)  u16   = truncate(u);
                           Word      value = zeroExtend(u16);
                           rd_value = value;
                       end
               default: /*f3_LW:*/  begin
                           Int#(32)  s32   = unpack(truncate(u));
                           Word_S    s     = signExtend(s32);
                           Word      value = pack(s);
                           rd_value = value;
                       end
            endcase
         end
         tagged Value .value: begin
            rd_value = value;
         end
      endcase

      if (cfg_verbose > 1) $display("[%7d] rl_write_back: %s = %h, clear scoreGPRs[%1d] (= %1d)", csr_cycle, regNameABI[rd], rd_value, rd, rg_scoreGPRs[rd][0] ? 1 : 0);

      // NOTE: DOES NOT check register x0 because set value to Zero when read
      rf_GPRs.upd(rd, rd_value);
      rg_scoreGPRs[rd][0] <= False;
   endrule


   // ---------------- Increment csr_cycle according to external oracles

   rule rl_incr_cycle;
      csr_cycle <= csr_cycle + 1;
   endrule

   // ----------------------------------------------------------------
   // INTERFACE

   method Action start(Addr initial_pc) if (!cpu_enabled);
      pc          <= initial_pc;
      pcEpoch     <= initial_pc;
      cpu_enabled <= True;
   endmethod

   method ActionValue#(Word) cpuToHost() if (csr_dcsr matches tagged Valid .ret);
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

