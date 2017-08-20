// See LICENSE for license details.

#include <stdint.h>
#include <stdarg.h>
#include <limits.h>
#include "util.h"

#if HOST_DEBUG
// [sizhuo] when compile for x86
#include <stdio.h>

void printInt(uint32_t c) {
	printf("%d", c);
}
void printChar(uint32_t c) {
	printf("%c", (char)c);
}
void printStr(char *x) {
	printf("%s", x);
}
#else

// tag of data to host
enum ToHostTag {
	ExitCode = 0,
	PrintChar = 1,
	PrintIntLow = 2,
	PrintIntHigh = 3
};

void printInt(uint32_t c) {
	// print low 16 bits
	int lo = (c & 0x0000FFFF) | (((uint32_t)PrintIntLow) << 16);
	asm volatile ("csrw mtohost, %0" : : "r" (lo));
	// print high 16 bits
	int hi = (c >> 16) | (((uint32_t)PrintIntHigh) << 16);
	asm volatile ("csrw mtohost, %0" : : "r" (hi));
}

void printChar(uint32_t c) {
	c = (c & 0x0000FFFF) | (((uint32_t)PrintChar) << 16);
  asm volatile ("csrw mtohost, %0" : : "r" (c));
}

void printStr(char* x) {
  while(1) {
	 // get 4B aligned addr
     uint32_t* y = (uint32_t*)(((uint32_t)x) & ~0x03);
     uint32_t fullC = *y;
     uint32_t mod = ((uint32_t)x) & 0x3;
     uint32_t shift = mod << 3;
     uint32_t c = (fullC & (0x0FF << shift)) >> shift;
     if(c == (uint32_t)'\0')
       break;
     printChar(c);
     x++;
  }
}

void toHostExit(uint32_t ret) {
	ret = (ret & 0x0000FFFF) | (((uint32_t) ExitCode) << 16);
	asm volatile ("csrw mtohost, %0" : : "r" (ret));
	// stall here
	while(1);
}

long handle_trap(long cause, long epc, long regs[32]) {
  return epc+4;
}

int __attribute__((weak)) main(int argc, char** argv) {
  // single-threaded programs override this function.
  printStr("Implement main(), foo!\n");
  return -1;
}

void _init(int cid, int nc) {
  int ret = main(0, 0); // call main function
  toHostExit((uint32_t)ret);
}

#endif // HOST_DEBUG
