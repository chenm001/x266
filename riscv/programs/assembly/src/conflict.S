# See LICENSE for license details.

#*****************************************************************************
# baseline.S
#-----------------------------------------------------------------------------
#
# This is the most basic self checking test. If your simulator does not
# pass thiss then there is little chance that it will pass any of the
# more complicated self checking tests.
#
# We test 100 nops

#include "riscv_test.h"
#include "test_macros.h"

RVTEST_RV32U
RVTEST_CODE_BEGIN

    TEST_CASE( 1, x1, 10, \
      li  x1, 1; \
      addi  x1, x1, 1; \
      addi  x1, x1, 1; \
      addi  x1, x1, 1; \
      addi  x1, x1, 1; \
      addi  x1, x1, 1; \
      addi  x1, x1, 1; \
      addi  x1, x1, 1; \
      addi  x1, x1, 1; \
      addi  x1, x1, 1; \
    )

    TEST_CASE( 2, x1, 1, \
      li  x1, 1; \
      addi  x1, x0, 1; \
      addi  x1, x0, 1; \
      addi  x1, x0, 1; \
      addi  x1, x0, 1; \
      addi  x1, x0, 1; \
      addi  x1, x0, 1; \
      addi  x1, x0, 1; \
      addi  x1, x0, 1; \
      addi  x1, x0, 1; \
      addi  x1, x0, 1; \
    )

    # Case: Fake DecodeStage register conflict
    TEST_CASE( 3, x1, 1, \
      la  x8, 1f;
      li  x1, 1; \
      jalr  x1, x8, 4; \
1:    addi  x1, x1, 4; \
2:    addi  x1, x1, 1; \
      sub   x1, x1, x8; \
    )

  TEST_PASSFAIL

RVTEST_CODE_END

  .data
RVTEST_DATA_BEGIN

  TEST_DATA

RVTEST_DATA_END
