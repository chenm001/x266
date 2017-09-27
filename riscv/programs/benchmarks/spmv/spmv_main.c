// See LICENSE for license details.

//**************************************************************************
// Float-precision sparse matrix-vector multiplication benchmark
//--------------------------------------------------------------------------

#include "util.h"

//--------------------------------------------------------------------------
// Input/Reference Data

#include "dataset1.h"

void spmv(int r, const float* val, const int* idx, const float* x,
          const int* ptr, float* y)
{
    for (int i = 0; i < r; i++)
    {
        int k;
        float yi0 = 0, yi1 = 0, yi2 = 0, yi3 = 0;
        for (k = ptr[i]; k < ptr[i+1]-3; k+=4)
        {
            yi0 += val[k+0]*x[idx[k+0]];
            yi1 += val[k+1]*x[idx[k+1]];
            yi2 += val[k+2]*x[idx[k+2]];
            yi3 += val[k+3]*x[idx[k+3]];
        }
        for ( ; k < ptr[i+1]; k++)
        {
            yi0 += val[k]*x[idx[k]];
        }
        y[i] = (yi0+yi1)+(yi2+yi3);
    }
}

//--------------------------------------------------------------------------
// Main

int main( int argc, char* argv[] )
{
    float y[R];

    uint32_t cycle = getCycle();
    uint32_t insts = getInsts();

    spmv(R, val, idx, x, ptr, y);

    cycle = getCycle() - cycle;
    insts = getInsts() - insts;
    printStr("Cycles = "); printInt(cycle); printChar('\n');
    printStr("Insts  = "); printInt(insts); printChar('\n');

    // Check the results
    int ret = verifyFloat( R, y, verify_data );
    printStr("Return "); printInt((uint32_t)ret); printChar('\n');
    return ret;
}
