// See LICENSE for license details.

//**************************************************************************
// Video benchmark -- SAD
//--------------------------------------------------------------------------
//
// This benchmark uses quicksort to sort an array of integers. The
// implementation is largely adapted from Numerical Recipes for C. The
// input data (and reference data) should be generated using the
// qsort_gendata.pl perl script and dumped to a file named
// dataset1.h

#include "util.h"
#include <limits.h>

#ifdef __llvm__
typedef long size_t;
#else
#include <string.h>
#endif

//--------------------------------------------------------------------------
// Input/Reference Data

#define type unsigned char
#include "dataset1.h"

int sad(type* input_data1, type* input_data2, size_t n)
{
    size_t i, j;
    int sum = 0;

    for(i = 0; i < n; ++i) {
        for(j = 0; j < n; ++j) {
            sum += abs((int)input_data1[i * n + j] - (int)input_data2[i * n + j]);
        }
    }
    return sum;
}

//--------------------------------------------------------------------------
// Main

int main( int argc, char* argv[] )
{
    uint32_t cycle = getCycle();
    int32_t insts = getInsts();

    // Do the sort
    int mySad = sad(input_data1, input_data2, SAD_SIZE);

    cycle = getCycle() - cycle;
    insts = getInsts() - insts;
    printStr("Cycles = "); printInt(cycle); printChar('\n');
    printStr("Insts  = "); printInt(insts); printChar('\n');

	// Check the results
    int ret = (mySad != verify_data[0]);
    printStr("Return "); printInt((uint32_t)ret); printChar('\n');
    return ret;
}
