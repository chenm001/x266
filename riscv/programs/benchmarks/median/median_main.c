// See LICENSE for license details.

//**************************************************************************
// Median filter bencmark
//--------------------------------------------------------------------------
//
// This benchmark performs a 1D three element median filter. The
// input data (and reference data) should be generated using the
// median_gendata.pl perl script and dumped to a file named
// dataset1.h You should not change anything except the
// HOST_DEBUG and PREALLOCATE macros for your timing run.

#include "util.h"

#include "median.h"

//--------------------------------------------------------------------------
// Input/Reference Data
#include "dataset1.h"

//--------------------------------------------------------------------------
// Main

int main( int argc, char* argv[] )
{
	int results_data[DATA_SIZE];

	printStr("Benchmark median\n");

#if HOST_DEBUG
	// Output the input array
	printArray( "input",  DATA_SIZE, input_data  );
	printArray( "verify", DATA_SIZE, verify_data );
#endif

	uint32_t cycle = getCycle();
	uint32_t insts = getInsts();

	median( DATA_SIZE, input_data, results_data );

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
