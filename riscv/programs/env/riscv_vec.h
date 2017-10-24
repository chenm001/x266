#ifndef __RISCV_VEC_H__
#define __RISCV_VEC_H__

#define XCUSTOM_OPCODE(x) XCUSTOM_OPCODE_ ## x
#define XCUSTOM_OPCODE_0    0b0001011
#define XCUSTOM_OPCODE_1    0b0101011
#define XCUSTOM_OPCODE_2    0b1011011
#define XCUSTOM_OPCODE_3    0b1111011

#define EXTRACT(a, offset, size) (((~(~0 << size) << offset) & a) >> offset)

#define VLD32X8(rd, rs1, imm)                   \
  .word                                         \
  (0b0000011                                |   \
  (rd                   << (7))             |   \
  (0b111                << (7+5))           |   \
  (rs1                  << (7+5+3))         |   \
  (EXTRACT(imm, 0, 12)  << (7+5+3+5)))

#define VST32X8(rs2, rs1, imm)                  \
  .word                                         \
  (0b0100011                                |   \
  (EXTRACT(imm, 0, 5)   << (7))             |   \
  (0b100                << (7+5))           |   \
  (rs1                  << (7+5+3))         |   \
  (rs2                  << (7+5+3+5))       |   \
  (EXTRACT(imm, 5, 7)   << (7+5+3+5+5)))



// Mapping register name to index
#define _v0  (0)
#define _v1  (1)
#define _v2  (2)
#define _v3  (3)
#define _v4  (4)
#define _v5  (5)
#define _v6  (6)
#define _v7  (7)
#define _v8  (8)
#define _v9  (9)
#define _v10 (10)
#define _v11 (11)
#define _v12 (12)
#define _v13 (13)
#define _v14 (14)
#define _v15 (15)
#define _v16 (16)
#define _v17 (17)
#define _v18 (18)
#define _v19 (19)
#define _v20 (20)
#define _v21 (21)
#define _v22 (22)
#define _v23 (23)
#define _v24 (24)
#define _v25 (25)
#define _v26 (26)
#define _v27 (27)
#define _v28 (28)
#define _v29 (29)
#define _v30 (30)
#define _v31 (31)

#define _x0  (0)
#define _x1  (1)
#define _x2  (2)
#define _x3  (3)
#define _x4  (4)
#define _x5  (5)
#define _x6  (6)
#define _x7  (7)
#define _x8  (8)
#define _x9  (9)
#define _x10 (10)
#define _x11 (11)
#define _x12 (12)
#define _x13 (13)
#define _x14 (14)
#define _x15 (15)
#define _x16 (16)
#define _x17 (17)
#define _x18 (18)
#define _x19 (19)
#define _x20 (20)
#define _x21 (21)
#define _x22 (22)
#define _x23 (23)
#define _x24 (24)
#define _x25 (25)
#define _x26 (26)
#define _x27 (27)
#define _x28 (28)
#define _x29 (29)
#define _x30 (30)
#define _x31 (31)

#endif /* __RISCV_VEC_H__ */
