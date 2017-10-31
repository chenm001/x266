# See LICENSE for license details.

#*****************************************************************************
# vld32x8.S
#-----------------------------------------------------------------------------
#
# Test lw instruction.
#

#include "riscv_test.h"
#include "test_macros.h"
#include "riscv_vec.h"

RVTEST_RV32U
RVTEST_CODE_BEGIN

  #-------------------------------------------------------------
  # Basic tests
  #-------------------------------------------------------------

  TEST_CASE( 2, x3, 0x07060504, \
    la  x8, tdat1; \
    la  x9, tbuf1; \
    VLD32X8(_v1, _x8, 4, _v0); \
    VST32X8(_v1, _x9, 0); \
    lw  x3, 0(x9); \
  )

  # Test with negative offset



  TEST_PASSFAIL

RVTEST_CODE_END

  .data
RVTEST_DATA_BEGIN

  TEST_DATA

tdat:
tdat1:  .byte 0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08, 0x09, 0x0A, 0x0B, 0x0C, 0x0D, 0x0E, 0x0F
tdat2:  .byte 0x10, 0x11, 0x12, 0x13, 0x14, 0x15, 0x16, 0x17, 0x18, 0x19, 0x1A, 0x1B, 0x1C, 0x1D, 0x1E, 0x1F
tdat3:  .byte 0x20, 0x21, 0x22, 0x23, 0x24, 0x25, 0x26, 0x27, 0x28, 0x29, 0x2A, 0x2B, 0x2C, 0x2D, 0x2E, 0x2F
tbuf1:  .word 0, 0, 0, 0

RVTEST_DATA_END
