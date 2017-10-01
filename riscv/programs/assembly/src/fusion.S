# See LICENSE for license details.

#*****************************************************************************
# jalr.S
#-----------------------------------------------------------------------------
#
# Test jalr instruction.
#

#include "riscv_test.h"
#include "test_macros.h"

RVTEST_RV32U
RVTEST_CODE_BEGIN

  #-------------------------------------------------------------
  # Test 2: Basic test
  #-------------------------------------------------------------

test_2:
  li  TESTNUM, 2
  li  x31, 0
  auipc x2, %pcrel_hi(target_2)

linkaddr_2:
  jalr x19, x2, 5 * 4  #%pcrel_lo(target_2)
  nop
  nop

  j fail

target_2:
  la  x1, linkaddr_2
  addi x1, x1, 4
  bne x1, x19, fail




  TEST_PASSFAIL

RVTEST_CODE_END

  .data
RVTEST_DATA_BEGIN

  TEST_DATA

RVTEST_DATA_END
