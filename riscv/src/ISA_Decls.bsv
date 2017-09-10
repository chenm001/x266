// Copyright (c) 2017 Min Chen. All Rights Reserved
// Author: Min Chen

// ================================================================
// ISA defs for UC Berkeley RISC V
//
// References (from riscv.org):
//   "The RISC-V Instruction Set Manual
//    Volume I: User-Level ISA, Version 2.2, May 7, 2017"
//    Waterman, Lee, Patterson and Asanovic
//
//   "The RISC-V Instruction Set Manual
//    Volume II: Privileged Architecture, Version 1.10, May 7, 2017"
//    Waterman, Lee, Avizienis, Patterson and Asanovic
//
// ================================================================

package ISA_Decls;

// ================================================================
// BSV library imports

import DefaultValue :: *;
import Vector       :: *;
import BuildVector  :: *;

// ================================================================
// BSV project imports

// None

// ================================================================

typedef 32 XLEN;

typedef TMul#(2, XLEN)  XLEN_2;     // Double-width for multiplications
Integer xlen = valueOf(XLEN);

// ----------------

typedef  8  Bits_per_Byte;

typedef  Bit#(XLEN)  Word;          // Raw (unsigned) register data
typedef  Int#(XLEN)  Word_S;        // Signed register data

typedef  Word        Addr;          // addresses/pointers

typedef TDiv#(XLEN, Bits_per_Byte)  Bytes_per_Word;
typedef TLog#(Bytes_per_Word)       Bits_per_Word_Byte_Index;

// ================================================================
// Instruction fields

typedef  Bit#(32)    Instr;
typedef  Bit#(7)     Opcode;
typedef  Bit#(5)     RegName;       // 32 registers, 0..31
typedef  32          NumRegs;
Integer  numRegs = valueOf(NumRegs);

function  Opcode     instr_opcode (Instr x); return x[6:0]; endfunction

function  Bit#(3)    instr_funct3 (Instr x); return x[14:12]; endfunction
function  Bit#(5)    instr_funct5 (Instr x); return x[31:27]; endfunction
function  Bit#(7)    instr_funct7 (Instr x); return x[31:25]; endfunction
function  Bit#(10)   instr_funct10(Instr x); return { x[31:25], x[14:12] }; endfunction

function  RegName    instr_rd     (Instr x); return x[11:7]; endfunction
function  RegName    instr_rs1    (Instr x); return x[19:15]; endfunction
function  RegName    instr_rs2    (Instr x); return x[24:20]; endfunction
function  RegName    instr_rs3    (Instr x); return x[31:27]; endfunction     // {F,D} Extension
function  CSR_Addr   instr_csr    (Instr x); return unpack(x[31:20]); endfunction

function  Bit#(12)   instr_I_imm12 (Instr x); return x[31:20]; endfunction
function  Bit#(12)   instr_S_imm12 (Instr x); return { x[31:25], x[11:7] }; endfunction
function  Bit#(13)   instr_SB_imm13(Instr x); return { x[31], x[7], x[30:25], x[11:8], 1'b0 }; endfunction
function  Bit#(20)   instr_U_imm20 (Instr x); return x[31:12]; endfunction
function  Bit#(21)   instr_UJ_imm21(Instr x); return { x[31], x[19:12], x[20], x[30:21], 1'b0 }; endfunction

// For FENCE decode
function  Bit#(4)   instr_pred(Instr x); return x[27:24]; endfunction
function  Bit#(4)   instr_succ(Instr x); return x[23:20]; endfunction

// For AMO decode
function  Bit#(2)   instr_aqrl(Instr x); return x[26:25]; endfunction

// ----------------
// Decoded instructions

typedef struct {
   Opcode   opcode;

   RegName  rd;
   RegName  rs1;
   RegName  rs2;
   RegName  rs3;
   CSR_Addr csr;

   Bit#(3)  funct3;
   Bit#(5)  funct5;
   Bit#(7)  funct7;
   Bit#(10) funct10;

   Bit#(12) imm12_I;
   Bit#(12) imm12_S;
   Bit#(13) imm13_SB;
   Bit#(20) imm20_U;
   Bit#(21) imm21_UJ;

   //Bit#(4)  pred;
   //Bit#(4)  succ;

   //Bit#(2)  aqrl;

   Addr     pc;
} Decoded_Instr deriving(FShow, Bits);

function Decoded_Instr fv_decode(Addr _pc, Instr instr);
   return Decoded_Instr {
            opcode:     instr_opcode   (instr),
            rd:         instr_rd       (instr),
            rs1:        instr_rs1      (instr),
            rs2:        instr_rs2      (instr),
            rs3:        instr_rs3      (instr),
            csr:        instr_csr      (instr),

            funct3:     instr_funct3   (instr),
            funct5:     instr_funct5   (instr),
            funct7:     instr_funct7   (instr),
            funct10:    instr_funct10  (instr),

            imm12_I:    instr_I_imm12  (instr),
            imm12_S:    instr_S_imm12  (instr),
            imm13_SB:   instr_SB_imm13 (instr),
            imm20_U:    instr_U_imm20  (instr),
            imm21_UJ:   instr_UJ_imm21 (instr),

            //pred:      instr_pred     (instr),
            //succ:      instr_succ     (instr),

            //aqrl:      instr_aqrl     (instr),
            pc:         _pc
          };
endfunction

// ----------------
// Write Back Stage

typedef union tagged {
   Word     Value;
   Bit#(3)  Funct3;
} Exec2WbValue_t deriving(Bits);

typedef struct {
   RegName  rd;
   Exec2WbValue_t rd_value;
} Exec2Wb_t deriving(Bits);

// ================================================================
// Symbolic register names

RegName x0  =  0;    RegName x1  =  1;    RegName x2  =  2;    RegName x3  =  3;
RegName x4  =  4;    RegName x5  =  5;    RegName x6  =  6;    RegName x7  =  7;
RegName x8  =  8;    RegName x9  =  9;    RegName x10 = 10;    RegName x11 = 11;
RegName x12 = 12;    RegName x13 = 13;    RegName x14 = 14;    RegName x15 = 15;
RegName x16 = 16;    RegName x17 = 17;    RegName x18 = 18;    RegName x19 = 19;
RegName x20 = 20;    RegName x21 = 21;    RegName x22 = 22;    RegName x23 = 23;
RegName x24 = 24;    RegName x25 = 25;    RegName x26 = 26;    RegName x27 = 27;
RegName x28 = 28;    RegName x29 = 29;    RegName x30 = 30;    RegName x31 = 31;

// Register names used in calling convention

RegName reg_ra = 1;

RegName reg_s0 = 8;   RegName reg_s1 = 9;   RegName reg_s2  = 18; RegName reg_s3  = 19;
RegName reg_s4 = 20;  RegName reg_s5 = 21;  RegName reg_s6  = 22; RegName reg_s7  = 23;
RegName reg_s8 = 24;  RegName reg_s9 = 25;  RegName reg_s10 = 26; RegName reg_s11 = 27;

RegName reg_sp = 2;
RegName reg_tp = 4;

RegName reg_a0 = 10;  RegName reg_a1 = 11;  RegName reg_a2 = 12;  RegName reg_a3 = 13;
RegName reg_a4 = 14;  RegName reg_a5 = 15;  RegName reg_a6 = 16;  RegName reg_a7 = 17;

RegName reg_t0 = 5;   RegName reg_t1 = 6;   RegName reg_t2 = 7;   RegName reg_t3 = 28;
RegName reg_t4 = 29;  RegName reg_t5 = 30;  RegName reg_t6 = 31; 

RegName reg_gp = 3;

String regNameABI[32] = {
   "zero", "ra", "sp",  "gp",  "tp", "t0", "t1", "t2",
   "s0",   "s1", "a0",  "a1",  "a2", "a3", "a4", "a5",
   "a6",   "a7", "s2",  "s3",  "s4", "s5", "s6", "s7",
   "s8",   "s9", "s10", "s11", "t3", "t4", "t5", "t6"
};

// ================================================================
// Data sizes for LOAD/STORE

typedef enum {
   BITS8,
   BITS16,
   BITS32
} Mem_Data_Size deriving(Eq, Bits, FShow);

Addr imemSt = 'h00000;
Addr dmemSt = 'h10000;

// ================================================================
// LOAD/STORE instructions

Bit#(2) f3_SIZE_B = 2'b00;
Bit#(2) f3_SIZE_H = 2'b01;
Bit#(2) f3_SIZE_W = 2'b10;
Bit#(2) f3_SIZE_D = 2'b11;

// ----------------
// Load instructions

Opcode op_LOAD = 7'b00_000_11;

Bit#(3) f3_LB  = 3'b000;
Bit#(3) f3_LH  = 3'b001;
Bit#(3) f3_LW  = 3'b010;
Bit#(3) f3_LD  = 3'b011;
Bit#(3) f3_LBU = 3'b100;
Bit#(3) f3_LHU = 3'b101;
Bit#(3) f3_LWU = 3'b110;

// ----------------
// Store instructions

Opcode op_STORE = 7'b01_000_11;

Bit#(3) f3_SB  = 3'b000;
Bit#(3) f3_SH  = 3'b001;
Bit#(3) f3_SW  = 3'b010;
Bit#(3) f3_SD  = 3'b011;

// ================================================================
// Memory Model

Opcode op_MISC_MEM = 7'b00_011_11;

Bit#(3) f3_FENCE   = 3'b000;
Bit#(3) f3_FENCE_I = 3'b001;

// ================================================================
// Integer Register-Immediate Instructions

Opcode op_OP_IMM = 7'b00_100_11;

Bit#(3) f3_ADDI  = 3'b000;
Bit#(3) f3_SLLI  = 3'b001;
Bit#(3) f3_SLTI  = 3'b010;
Bit#(3) f3_SLTIU = 3'b011;
Bit#(3) f3_XORI  = 3'b100;
Bit#(3) f3_SRxI  = 3'b101; Bit#(3) f3_SRLI  = 3'b101; Bit#(3) f3_SRAI  = 3'b101;
Bit#(3) f3_ORI   = 3'b110;
Bit#(3) f3_ANDI  = 3'b111;


// ================================================================
// Integer Register-Register Instructions

Opcode op_OP = 7'b01_100_11;

Bit#(10) f10_ADD    = 10'b000_0000_000;
Bit#(10) f10_SUB    = 10'b010_0000_000;
Bit#(10) f10_SLL    = 10'b000_0000_001;
Bit#(10) f10_SLT    = 10'b000_0000_010;
Bit#(10) f10_SLTU   = 10'b000_0000_011;
Bit#(10) f10_XOR    = 10'b000_0000_100;
Bit#(10) f10_SRL    = 10'b000_0000_101;
Bit#(10) f10_SRA    = 10'b010_0000_101;
Bit#(10) f10_OR     = 10'b000_0000_110;
Bit#(10) f10_AND    = 10'b000_0000_111;

Bit#(7) f7_MUL_DIV_REM = 7'b000_0001;

Bit#(3) f3_MUL    = 3'b000;
Bit#(3) f3_MULH   = 3'b001;
Bit#(3) f3_MULHSU = 3'b010;
Bit#(3) f3_MULHU  = 3'b011;
Bit#(3) f3_DIV    = 3'b100;
Bit#(3) f3_DIVU   = 3'b101;
Bit#(3) f3_REM    = 3'b110;
Bit#(3) f3_REMU   = 3'b111;


// ================================================================
// LUI, AUIPC

Opcode op_LUI   = 7'b01_101_11;
Opcode op_AUIPC = 7'b00_101_11;

// ================================================================
// Control transfer

Opcode  op_BRANCH = 7'b11_000_11;

Bit#(3) f3_BEQ   = 3'b000;
Bit#(3) f3_BNE   = 3'b001;
Bit#(3) f3_BLT   = 3'b100;
Bit#(3) f3_BGE   = 3'b101;
Bit#(3) f3_BLTU  = 3'b110;
Bit#(3) f3_BGEU  = 3'b111;

Opcode op_JAL  = 7'b11_011_11;

Opcode op_JALR = 7'b11_001_11;

// ================================================================
// System Instructions
Opcode op_SYSTEM = 7'b11_100_11;

// sub-opcodes: (in funct3 field)
Bit#(3)   f3_PRIV       = 3'b000;
Bit#(3)   f3_CSRRW      = 3'b001;
Bit#(3)   f3_CSRRS      = 3'b010;
Bit#(3)   f3_CSRRC      = 3'b011;
Bit#(3)   f3_CSRRWI     = 3'b101;
Bit#(3)   f3_CSRRSI     = 3'b110;
Bit#(3)   f3_CSRRCI     = 3'b111;

// Wait for Interrupt
Bit#(12) f12_WFI       = 12'b0001_0000_0010;

function Bool is_SYSTEM_PRIV(Instr instr);
   return(   (instr_opcode(instr) == op_SYSTEM)
          && (instr_funct3(instr) == f3_PRIV));
endfunction


// ================================================================
// Control/Status register addresses

typedef Bit#(12) CSR_Addr;

// ----------------
// User-level CSRs

CSR_Addr   csr_FFLAGS   = 'h001;    // Floating-point accrued exceptions
CSR_Addr   csr_FRM      = 'h002;    // Floating-point Dynamic Rounding Mode
CSR_Addr   csr_FCSR     = 'h003;    // Floating-point Control and Status Register

CSR_Addr   csr_CYCLE    = 'hc00;    // Cycle counter for RDCYCLE
CSR_Addr   csr_TIME     = 'hc01;    // Timer for RDTIME
CSR_Addr   csr_INSTRET  = 'hc02;    // Instructions retired, for RDINSTRET

CSR_Addr   csr_CYCLEH   = 'hc80;    // Upper 32 bits of CYCLE (RV32I only)
CSR_Addr   csr_TIMEH    = 'hc81;    // Upper 32 bits of TIME (RV32I only)
CSR_Addr   csr_INSTRETH = 'hc82;    // Upper 32 bits of INSTRET (RV32I only)

// ----------------
// Machine-level CSRs

CSR_Addr   csr_MCPUID   = 'hF00;    // CPU description
CSR_Addr   csr_MIMPID   = 'hF01;    // Vendor ID and version number
CSR_Addr   csr_MHARTID  = 'hF10;    // Hardware thread ID

CSR_Addr   csr_MSTATUS  = 'h300;    // status
CSR_Addr   csr_MTVEC    = 'h301;    // trap handler base address
CSR_Addr   csr_MTDELEG  = 'h302;    // trap delegation
CSR_Addr   csr_MIE      = 'h304;    // interrupt-enable
CSR_Addr   csr_MTIMECMP = 'h321;    // wall-clock timer compare value

CSR_Addr   csr_DCSR     = 'h7B0;    // Test input / output register

// ----------------
// Bit-fields of the CSR_MSTATUS register

Integer mstatus_SD_index   = xlen-1;
Integer mstatus_VM_hi      = 21;        Integer mstatus_VM_lo      = 17;
Integer mstatus_MPRV_index = 16;
Integer mstatus_XS_hi      = 15;        Integer mstatus_XS_lo   = 14;
Integer mstatus_FS_hi      = 13;        Integer mstatus_FS_lo   = 12;
Integer mstatus_PRV3_hi    = 11;        Integer mstatus_PRV3_lo = 10;
Integer mstatus_IE3_index  =  9;
Integer mstatus_PRV2_hi    =  8;        Integer mstatus_PRV2_lo = 7;
Integer mstatus_IE2_index  =  6;
Integer mstatus_PRV1_hi    =  5;        Integer mstatus_PRV1_lo = 4;
Integer mstatus_IE1_index  =  3;
Integer mstatus_PRV_hi     =  2;        Integer mstatus_PRV_lo  = 1;
Integer mstatus_IE_index   =  0;


// ----------------
// Standard mtvec and reset vector values
// MTVEC reg can be hardwired to hi or lo value
// reset value should correspond.

Word mtvec_std_hi = (~ 'h01FF);    // = 0xFFFF_FFE00
Word mtvec_std_lo = 'h0100;

`ifdef MTVEC_STD_HI
   Word mtvec_reset_value = mtvec_std_hi;
`else
   Word mtvec_reset_value = mtvec_std_lo;
`endif

Word pc_reset_value = (mtvec_reset_value + 'h100);    // 'h0200 or 'hFFFF_FF00

// ----------------
// MIP and MIE fields (interrupt pending, interrupt enable)

// External interrupt
Integer mxi_index = 19;
Integer hxi_index = 18;
Integer sxi_index = 17;

// Timer interrupts
Integer mti_index = 7;
Integer hti_index = 6;
Integer sti_index = 5;    // Also in SIP reg

// Software interrupts
Integer msi_index = 3;
Integer hsi_index = 2;
Integer ssi_index = 1;    // Also in SIE reg

// ----------------
// MCAUSE (reason for exception)

Integer mcause_interrupt_index  = xlen - 1;
Integer mcause_zero_index       = 4;
Integer mcause_exc_code_hi      = 3;    Integer mcause_exc_code_lo   = 0;

typedef Bit#(4) Exc_Code;

// When Interrupt = 0 (trap)

Exc_Code exc_code_MISALIGNED_FETCH     = 4'h0;
Exc_Code exc_code_FAULT_FETCH          = 4'h1;
Exc_Code exc_code_ILLEGAL_INSTRUCTION  = 4'h2;
Exc_Code exc_code_BREAKPOINT           = 4'h3;

Exc_Code exc_code_MISALIGNED_LOAD      = 4'h4;
Exc_Code exc_code_FAULT_LOAD           = 4'h5;

Exc_Code exc_code_MISALIGNED_STORE_AMO = 4'h6;
Exc_Code exc_code_FAULT_STORE_AMO      = 4'h7;

Exc_Code exc_code_ECALL_FROM_U         = 4'h8;
Exc_Code exc_code_ECALL_FROM_S         = 4'h9;
Exc_Code exc_code_ECALL_FROM_H         = 4'hA;
Exc_Code exc_code_ECALL_FROM_M         = 4'hB;

// When Interrupt = 1 (interrupt)

Exc_Code exc_code_SW_INTERRUPT         = 4'h0;
Exc_Code exc_code_TIMER_INTERRUPT      = 4'h1;

Maybe#(Exc_Code) m_trap_none           = tagged Invalid;
Maybe#(Exc_Code) m_trap_illegal_instr  = tagged Valid exc_code_ILLEGAL_INSTRUCTION;

function Action show_Trap_Exc_Code (Exc_Code exc);
   action
      case (exc)
         exc_code_MISALIGNED_FETCH:     $write("MISALIGNED_FETCH");
         exc_code_FAULT_FETCH:          $write("FAULT_FETCH");
         exc_code_ILLEGAL_INSTRUCTION:  $write("ILLEGAL_INSTRUCTION");
         exc_code_BREAKPOINT:           $write("BREAKPOINT");
         exc_code_MISALIGNED_LOAD:      $write("MISALIGNED_LOAD");
         exc_code_FAULT_LOAD:           $write("FAULT_LOAD");
         exc_code_MISALIGNED_STORE_AMO: $write("MISALIGNED_STORE_AMO");
         exc_code_FAULT_STORE_AMO:      $write("FAULT_STORE_AMO");
         exc_code_ECALL_FROM_U:         $write("ECALL_FROM_U");
         exc_code_ECALL_FROM_S:         $write("ECALL_FROM_S");
         exc_code_ECALL_FROM_H:         $write("ECALL_FROM_H");
         exc_code_ECALL_FROM_M:         $write("ECALL_FROM_M");
         default:                       $write("unknown Exc_Code 0x%0h", exc);
      endcase
   endaction
endfunction

// ----------------
// Bit-fields of the CSR_SSTATUS register

function bit                  sstatus_sd     (Word sstatus_val); return sstatus_val[xlen-1]; endfunction
function Bit#(TSub#(XLEN,18)) sstatus_mbz_17 (Word sstatus_val); return sstatus_val [xlen-2:17]; endfunction
function bit                  sstatus_mprv   (Word sstatus_val); return sstatus_val[16]; endfunction
function Bit#(2)              sstatus_xs     (Word sstatus_val); return sstatus_val[15:14]; endfunction
function Bit#(2)              sstatus_fs     (Word sstatus_val); return sstatus_val[13:12]; endfunction
function Bit#(7)              sstatus_mbz_5  (Word sstatus_val); return sstatus_val[11:5]; endfunction
function bit                  sstatus_ps     (Word sstatus_val); return sstatus_val[4]; endfunction
function bit                  sstatus_pie    (Word sstatus_val); return sstatus_val[3]; endfunction
function Bit#(2)              sstatus_mbz_1  (Word sstatus_val); return sstatus_val[2:1]; endfunction
function bit                  sstatus_ie     (Word sstatus_val); return sstatus_val[0]; endfunction

// ----------------
// SCAUSE (reason for exception)

function bit scause_interrupt(Word scause_val); return scause_val[xlen-1]; endfunction
function Bit#(TSub#(XLEN,5)) scause_mbz_5(Word scause_val); return scause_val[xlen-2:4]; endfunction
function Bit#(4) scause_exception_code(Word scause_val); return scause_val[3:0]; endfunction

// ================================================================

endpackage
