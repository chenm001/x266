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

import RegFile   :: *;    // For RISC-V GPRs

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

// ----------------
// Write Back Stage

typedef union tagged {
   Word        Value;

   struct {
      Bit#(3)                          funct3;
      Bit#(Bits_per_Word_Byte_Index)   align;
   }           MemOp;
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

String regNameABI[numRegs] = {
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
   return(   (instr_opcode7(instr) == op_SYSTEM)
          && (instr_funct3 (instr) == f3_PRIV));
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

Bit#(4) exc_code_ILLEGAL_INSTRUCTION  = 4'h2;

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


typedef enum {
   // Load instructions
   OP_LB, 
   OP_LH, 
   OP_LW, 
   OP_LBU,
   OP_LHU,
   //OP_LD,

   // Store instructions
   OP_SB,
   OP_SH,
   OP_SW,
   //OP_SD,

   // Memory Model
   OP_FENCE,
   OP_FENCE_I,

   // Integer Register-Immediate Instructions
   OP_ADDI,
   OP_SLLI,
   OP_SLTI,
   OP_SLTIU,
   OP_XORI,
   OP_SRLI,
   OP_SRAI,
   OP_ORI,
   OP_ANDI,

   // Integer Register-Register Instructions
   OP_ADD,
   OP_SUB,
   OP_SLL,
   OP_SLT,
   OP_SLTU,
   OP_XOR,
   OP_SRL,
   OP_SRA,
   OP_OR,
   OP_AND,

   // M Extension
   //OP_MUL,
   //OP_MULH,
   //OP_MULHSU,
   //OP_MULHU,
   //OP_DIV,
   //OP_DIVU,
   //OP_REM,
   //OP_REMU,


   // LUI, AUIPC
   OP_LUI,
   OP_AUIPC,

   // Control transfer
   OP_BEQ,
   OP_BNE,
   OP_BLT,
   OP_BGE,
   OP_BLTU,
   OP_BGEU,
   OP_JAL,
   OP_JALR,

   // System Instructions
   //OP_PRIV,
   OP_CSRRW,
   OP_CSRRS,
   OP_CSRRC,
   //OP_CSRRWI,
   //OP_CSRRSI,
   //OP_CSRRCI,

   OP_ILLEGAL
} Instr_e deriving(FShow, Bits, Eq);

// ================================================================
// Instruction fields

typedef  Bit#(32)    Instr;
typedef  Bit#(7)     Opcode;
typedef  Bit#(5)     RegName;       // 32 registers, 0..31
typedef  32          NumRegs;
Integer  numRegs = valueOf(NumRegs);

function  Opcode     instr_opcode7(Instr x); return x[6:0]; endfunction

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

typedef struct {
   Instr_e opcode;
   Maybe#(RegName) rs1;
   Maybe#(RegName) rs2;
} Instr_s deriving(Bits, Eq);

// ----------------
// Decoded instructions

typedef struct {
   Instr    instr;
   Instr_s  op;
   Addr     pc;
} Decoded_Instr deriving(Bits);

typedef struct {
   Opcode   opcode7;

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
} Decoded_Fields deriving(Bits);

function Instr_s fv_decode_instr(Instr instr);
   let opcode7 = instr_opcode7 (instr);
   let rd      = instr_rd      (instr);
   let rs1     = instr_rs1     (instr);
   let rs2     = instr_rs2     (instr);
   let csr     = instr_csr     (instr);
   let funct3  = instr_funct3  (instr);
   let funct10 = instr_funct10 (instr);
   let imm12_I = instr_I_imm12 (instr);

   return case(opcode7)
      op_LUI      :  Instr_s {opcode: OP_LUI,   rs1: tagged Invalid,    rs2: tagged Invalid};
      op_AUIPC    :  Instr_s {opcode: OP_AUIPC, rs1: tagged Invalid,    rs2: tagged Invalid};
      op_JAL      :  Instr_s {opcode: OP_JAL,   rs1: tagged Invalid,    rs2: tagged Invalid};
      op_JALR     :  Instr_s {opcode: OP_JALR,  rs1: tagged Valid rs1,  rs2: tagged Invalid};
      op_BRANCH   :  Instr_s {opcode: case(funct3)
                                       f3_BEQ   :  OP_BEQ;
                                       f3_BNE   :  OP_BNE;
                                       f3_BLT   :  OP_BLT;
                                       f3_BGE   :  OP_BGE;
                                       f3_BLTU  :  OP_BLTU;
                                       f3_BGEU  :  OP_BGEU;
                                       default  :  OP_ILLEGAL;
                                    endcase,    rs1: tagged Valid rs1,  rs2: tagged Valid rs2};
      op_LOAD     :  Instr_s {opcode: case(funct3)
                                       f3_LB    :  OP_LB;
                                       f3_LBU   :  OP_LBU;
                                       f3_LH    :  OP_LH;
                                       f3_LHU   :  OP_LHU;
                                       f3_LW    :  OP_LW;
                                       default  :  OP_ILLEGAL;
                                    endcase,    rs1: tagged Valid rs1,  rs2: tagged Invalid};
      op_STORE    :  Instr_s {opcode: case(funct3)
                                       f3_SB    :  OP_SB;
                                       f3_SH    :  OP_SH;
                                       f3_SW    :  OP_SW;
                                       default  :  OP_ILLEGAL;
                                    endcase,    rs1: tagged Valid rs1,  rs2: tagged Valid rs2};
      op_OP_IMM   :  Instr_s {opcode: case(funct3)
                                       f3_ADDI  :  OP_ADDI;
                                       f3_SLTI  :  OP_SLTI;
                                       f3_SLTIU :  OP_SLTIU;
                                       f3_XORI  :  OP_XORI;
                                       f3_ORI   :  OP_ORI;
                                       f3_ANDI  :  OP_ANDI;
                                       f3_SLLI  :  (imm12_I[10] == 1'b0 ? OP_SLLI : OP_ILLEGAL);
                                       f3_SRxI  :  (imm12_I[10] == 1'b0 ? OP_SRLI : OP_SRAI);
                                       default  :  OP_ILLEGAL;
                                    endcase,    rs1: tagged Valid rs1,  rs2: tagged Invalid};
      op_OP       :  Instr_s {opcode: case(funct10)
                                       f10_ADD  :  OP_ADD;
                                       f10_SUB  :  OP_SUB;
                                       f10_SLL  :  OP_SLL;
                                       f10_SLT  :  OP_SLT;
                                       f10_SLTU :  OP_SLTU;
                                       f10_XOR  :  OP_XOR;
                                       f10_SRL  :  OP_SRL;
                                       f10_SRA  :  OP_SRA;
                                       f10_OR   :  OP_OR;
                                       f10_AND  :  OP_AND;
                                       default  :  OP_ILLEGAL;
                                    endcase,    rs1: tagged Valid rs1,  rs2: tagged Valid rs2};
      op_MISC_MEM :  Instr_s {opcode: case(funct3)
                                          f3_FENCE :  ( ( (rd == 0)
                                                       && (rs1 == 0)
                                                       && (truncateLSB(imm12_I) == 4'b0) ) ? OP_FENCE : OP_ILLEGAL);
                                          f3_FENCE_I: ( ( (rd == 0)
                                                       && (rs1 == 0)
                                                       && (imm12_I == 12'b0) ) ? OP_FENCE_I : OP_ILLEGAL);
                                          default:    OP_ILLEGAL;
                                    endcase,    rs1: tagged Invalid,    rs2: tagged Invalid};
      op_SYSTEM   :  Instr_s {opcode: case(funct3)
                                          f3_CSRRW :  OP_CSRRW;
                                          f3_CSRRS :  OP_CSRRS;
                                          f3_CSRRC :  OP_CSRRC;
                                          default  :  OP_ILLEGAL;
                                    endcase,    rs1: tagged Valid rs1,  rs2: tagged Invalid};
   endcase;
endfunction

function Decoded_Fields fv_decode_fields(Instr instr);
   return Decoded_Fields {
            opcode7  :  instr_opcode7  (instr),
            rd       :  instr_rd       (instr),
            rs1      :  instr_rs1      (instr),
            rs2      :  instr_rs2      (instr),
            rs3      :  instr_rs3      (instr),
            csr      :  instr_csr      (instr),

            funct3   :  instr_funct3   (instr),
            funct5   :  instr_funct5   (instr),
            funct7   :  instr_funct7   (instr),
            funct10  :  instr_funct10  (instr),

            imm12_I  :  instr_I_imm12  (instr),
            imm12_S  :  instr_S_imm12  (instr),
            imm13_SB :  instr_SB_imm13 (instr),
            imm20_U  :  instr_U_imm20  (instr),
            imm21_UJ :  instr_UJ_imm21 (instr)
          };
endfunction

function Decoded_Instr fv_decode(Addr pc, Instr instr, RegFile#(RegName, Word) gpr);
   return Decoded_Instr {
            instr    :  instr,
            op       :  fv_decode_instr(instr),
            pc       :  pc
          };
endfunction


// ================================================================

endpackage
