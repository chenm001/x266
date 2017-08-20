// See LICENSE for license details.

// *************************************************************************
// multiply filter bencmark
// -------------------------------------------------------------------------
//
// This benchmark tests the software multiply implemenation. The
// input data (and reference data) should be generated using the
// multiply_gendata.pl perl script and dumped to a file named
// dataset1.h You should not change anything except the
// HOST_DEBUG and VERIFY macros for your timing run.

#include "util.h"

#include "multiply.h"

//--------------------------------------------------------------------------
// Input/Reference Data

#include "dataset1.h"

//--------------------------------------------------------------------------
// Main

int main( int argc, char* argv[] )
{
	int i;
	int results_data[DATA_SIZE];

	printStr("Benchmark multiply\n");

#if HOST_DEBUG
	// Output the input arrays
	printArray( "input1",  DATA_SIZE, input_data1  );
	printArray( "input2",  DATA_SIZE, input_data2  );
	printArray( "verify", DATA_SIZE, verify_data );
#endif

	uint32_t cycle = getCycle();
	uint32_t insts = getInsts();

	for (i = 0; i < DATA_SIZE; i++) {
		results_data[i] = multiply( input_data1[i], input_data2[i] );
	}

	cycle = getCycle() - cycle;
	insts = getInsts() - insts;
	printStr("Cycles = "); printInt(cycle); printChar('\n');
	printStr("Insts  = "); printInt(insts); printChar('\n');
  
#if HOST_DEBUG
	// Print out the results
	printArray( "results", DATA_SIZE, results_data );
#endif

	// Check the results
	int ret = verify( DATA_SIZE, results_data, verify_data );
	printStr("Return "); printInt((uint32_t)ret); printChar('\n');
	return ret;
}
