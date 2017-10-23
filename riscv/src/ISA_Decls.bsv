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
typedef 32 ALEN;

typedef TMul#(2, XLEN)  XLEN_2;     // Double-width for multiplications
Integer xlen = valueOf(XLEN);

// ----------------

typedef  8  Bits_per_Byte;

typedef  Bit#(XLEN)  Word;          // Raw (unsigned) register data
typedef  Int#(XLEN)  Word_S;        // Signed register data

typedef  Bit#(ALEN)  Addr;          // addresses/pointers

typedef TDiv#(XLEN, Bits_per_Byte)  Bytes_per_Word;
typedef TLog#(Bytes_per_Word)       Bits_per_Word_Byte_Index;

typedef 8 MemBanks;
Integer memBanks = valueOf(MemBanks);

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

CSR_Addr   csr_DSCRATCH = 'h7B2;    // Test input / output register

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
function  Bit#(6)    instr_funct6 (Instr x); return {x[27:25], x[14:12]}; endfunction
function  Bit#(7)    instr_funct7 (Instr x); return x[31:25]; endfunction
function  Bit#(10)   instr_funct10(Instr x); return { x[31:25], x[14:12] }; endfunction

function  RegName    instr_rd     (Instr x); return x[11:7]; endfunction
function  RegName    instr_rs1    (Instr x); return x[19:15]; endfunction
function  RegName    instr_rs2    (Instr x); return x[24:20]; endfunction
function  RegName    instr_rs3    (Instr x); return x[31:27]; endfunction     // {F,D} Extension
function  CSR_Addr   instr_csr    (Instr x); return unpack(x[31:20]); endfunction

function  Bit#(9)    instr_I_imm9  (Instr x); return {x[31:28], x[24:20]}; endfunction
function  Bit#(9)    instr_S_imm9  (Instr x); return {x[31:28], x[11:7]}; endfunction
function  Bit#(12)   instr_I_imm12 (Instr x); return x[31:20]; endfunction
function  Bit#(12)   instr_S_imm12 (Instr x); return { x[31:25], x[11:7] }; endfunction
function  Bit#(13)   instr_SB_imm13(Instr x); return { x[31], x[7], x[30:25], x[11:8], 1'b0 }; endfunction
function  Bit#(20)   instr_U_imm20 (Instr x); return x[31:12]; endfunction
function  Bit#(21)   instr_UJ_imm21(Instr x); return { x[31], x[19:12], x[20], x[30:21], 1'b0 }; endfunction

// For FENCE decode
function  Bit#(4)   instr_pred(Instr x); return x[27:24]; endfunction
function  Bit#(4)   instr_succ(Instr x); return x[23:20]; endfunction

typedef struct {
   RegName     rs1;
   RegName     rs2;

   union tagged {
      LdFunc      Ld;
      StFunc      St;
      BrFunc      Br;
      AluFunc     Alu;
      AluFunc     Alui;
      SysFunc     Sys;
      void        Auipc;
      void        Lui;
      void        Jal;
      void        Jalr;
      void        Illegal;
   } opcode;
} Instr_s deriving(Bits, Eq);

// ----------------
// Decoded instructions

// ================================================================
// LOAD/STORE instructions

// ----------------
// Load instructions
typedef enum {
   Lb    = 3'b000,
   Lh    = 3'b001,
   Lw    = 3'b010,
   Lbu   = 3'b100,
   Lhu   = 3'b101
} LdFunc deriving(Bits, Eq, FShow);

// ----------------
// Store instructions
typedef enum {
   Sb    = 3'b000,
   Sh    = 3'b001,
   Sw    = 3'b010,
   St_dummy = 3'b111
} StFunc deriving(Bits, Eq, FShow);


// ================================================================
// Control transfer
// These enumeration values match the bit values for funct3
typedef enum {
    Eq   = 3'b000,
    Neq  = 3'b001,
    //Jal  = 3'b010,
    //Jalr = 3'b011,
    Lt   = 3'b100,
    Ge   = 3'b101,
    Ltu  = 3'b110,
    Geu  = 3'b111
} BrFunc deriving(Bits, Eq, FShow);

// ================================================================
// Integer Register (w/wo Immediate) Instructions
// This encoding tries to match {inst[30], funct3}
typedef enum {
    Add  = 4'b0000,
    Sll  = 4'b0001,
    Slt  = 4'b0010,
    Sltu = 4'b0011,
    Xor  = 4'b0100,
    Srl  = 4'b0101,
    Or   = 4'b0110,
    And  = 4'b0111,
    Sub  = 4'b1000,
    Sra  = 4'b1101
} AluFunc deriving(Bits, Eq, FShow);

// ================================================================
// System Instructions
typedef enum {
    CSRRW   = 3'b001,
    CSRRS   = 3'b010,
    CSRRC   = 3'b011,
    CSRR,   // read-only CSR operation
    CSRW    // write-only CSR operation
} SysFunc deriving (Bits, Eq, FShow);


// ----------------
// Write Back Stage

typedef union tagged {
   Word  Value;
   struct { LdFunc ld_op; Bit#(5) lsb5; } MemOp;
} Exec2WbValue_t deriving(Bits);

typedef struct {
   RegName  rd;
   Exec2WbValue_t rd_value;
} Exec2Wb_t deriving(Bits);


// ================================================================
// 7-bits Opcode kind
typedef enum {
    Load    = 7'b00000_11,
    LoadFp  = 7'b00001_11,
    MiscMem = 7'b00011_11,
    OpImm   = 7'b00100_11,
    Auipc   = 7'b00101_11,
    OpImm32 = 7'b00110_11,
    Store   = 7'b01000_11,
    StoreFp = 7'b01001_11,
    Amo     = 7'b01011_11,
    Op      = 7'b01100_11,
    Lui     = 7'b01101_11,
    Op32    = 7'b01110_11,
    Fmadd   = 7'b10000_11,
    Fmsub   = 7'b10001_11,
    Fnmsub  = 7'b10010_11,
    Fnmadd  = 7'b10011_11,
    OpFp    = 7'b10100_11,
    Branch  = 7'b11000_11,
    Jalr    = 7'b11001_11,
    Jal     = 7'b11011_11,
    System  = 7'b11100_11
} OpKind deriving(Bits, Eq, FShow);

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

   // Vector Extension
   Bit#(6)  funct6;
   Bit#(9)  imm9_I;
   Bit#(9)  imm9_S;
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

   OpKind kind   = unpack(opcode7);

   return case(kind)
      Lui      :  Instr_s {opcode: tagged Lui,                                rs1: 0,     rs2: 0};
      Auipc    :  Instr_s {opcode: tagged Auipc,                              rs1: 0,     rs2: 0};
      Jal      :  Instr_s {opcode: tagged Jal,                                rs1: 0,     rs2: 0};
      Jalr     :  Instr_s {opcode: tagged Jalr,                               rs1: rs1,   rs2: 0};
      Branch   :  Instr_s {opcode: tagged Br    unpack(funct3),               rs1: rs1,   rs2: rs2};
      Load     :  Instr_s {opcode: tagged Ld    unpack(funct3),               rs1: rs1,   rs2: 0};
      Store    :  Instr_s {opcode: tagged St    unpack(funct3),               rs1: rs1,   rs2: rs2};
      OpImm    :  Instr_s {opcode: tagged Alui  unpack({(funct3 == 3'b101 ? instr[30] : 1'b0), funct3}), // check SRAI and SRLI
                                                                              rs1: rs1,   rs2: 0};
      Op       :  Instr_s {opcode: tagged Alu   unpack({instr[30], funct3}),  rs1: rs1,   rs2: rs2};
      System   :  Instr_s {opcode: tagged Sys   unpack(funct3),               rs1: rs1,   rs2: 0};
      default  :  Instr_s {opcode: Illegal, rs1: ?, rs2: ?};
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
            funct6   :  instr_funct6   (instr),
            funct7   :  instr_funct7   (instr),
            funct10  :  instr_funct10  (instr),

            imm9_I   :  instr_I_imm9   (instr),
            imm9_S   :  instr_I_imm9   (instr),
            imm12_I  :  instr_I_imm12  (instr),
            imm12_S  :  instr_S_imm12  (instr),
            imm13_SB :  instr_SB_imm13 (instr),
            imm20_U  :  instr_U_imm20  (instr),
            imm21_UJ :  instr_UJ_imm21 (instr)
          };
endfunction

function Decoded_Instr fv_decode(Addr pc, Instr instr);
   return Decoded_Instr {
            instr    :  instr,
            op       :  fv_decode_instr(instr),
            pc       :  pc
          };
endfunction


// ================================================================

endpackage
