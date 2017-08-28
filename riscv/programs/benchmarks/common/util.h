// See LICENSE for license details.

#ifndef __UTIL_H
#define __UTIL_H

//--------------------------------------------------------------------------
// Macros

// Set HOST_DEBUG to 1 if you are going to compile this for a host
// machine (ie Athena/Linux) for debug purposes and set HOST_DEBUG
// to 0 if you are compiling with the smips-gcc toolchain.

#ifndef HOST_DEBUG
#define HOST_DEBUG 0
#endif

#include <stdint.h>

#if HOST_DEBUG

#include <stdio.h>
    static void printArray(const char name[], int n, const int arr[]) {
      int i;
      printf( " %10s :", name );
      for ( i = 0; i < n; i++ )
        printf( " %3d ", arr[i] );
      printf( "\n" );
    }
    static uint32_t getInsts() { return 0; }
    static uint32_t getCycle() { return 0; }
    static uint32_t getCoreId() { return 0; }

#else // HOST_DEBUG = 0

    static uint32_t getInsts() {
        uint32_t inst_num = 0;
        asm volatile ("csrr %0, instret" : "=r"(inst_num) : );
        return inst_num;
    }

    static uint32_t getCycle() {
        uint32_t cyc_num = 0;
        asm volatile ("csrr %0, cycle" : "=r"(cyc_num) : );
        return cyc_num;
    }

    static uint32_t getCoreId() {
        uint32_t id = 0;
        asm volatile ("csrr %0, mhartid" : "=r"(id) : );
        return id;
    }

#endif // HOST_DEBUG


void printInt(uint32_t c);
void printChar(uint32_t c);
void printStr(char *x);
int printf(const char* fmt, ...);
int sprintf(char* str, const char* fmt, ...);
void* memcpy(void* dest, const void* src, unsigned len);
void* memset(void* dest, int byte, unsigned len);
unsigned strnlen(const char *s, unsigned n);
int strcmp(const char* s1, const char* s2);
char* strcpy(char* dest, const char* src);
long atol(const char* str);


static int verify(int n, const volatile int* test, const int* verify) {
    // correct: return 0
    // wrong: return wrong idx + 1
    int i;
    for (i = 0; i < n; i++)
    {
        int t = test[i];
        int v = verify[i];
        if (t != v) return i+1;
    }
    return 0;
}

static int verifyFloat(int n, const volatile float* test, const float* verify)
{
    int i;
    // Unrolled for faster verification
    for (i = 0; i < n/2*2; i+=2)
    {
        float t0 = test[i], t1 = test[i+1];
        float v0 = verify[i], v1 = verify[i+1];
        int eq1 = t0 == v0, eq2 = t1 == v1;
        if (!(eq1 & eq2)) return i+1+eq1;
    }
    if (n % 2 != 0 && test[n-1] != verify[n-1])
        return n;
    return 0;
}

static int verifyDouble(int n, const volatile double* test, const double* verify)
{
    int i;
    // Unrolled for faster verification
    for (i = 0; i < n/2*2; i+=2)
    {
        double t0 = test[i], t1 = test[i+1];
        double v0 = verify[i], v1 = verify[i+1];
        int eq1 = t0 == v0, eq2 = t1 == v1;
        if (!(eq1 & eq2)) return i+1+eq1;
    }
    if (n % 2 != 0 && test[n-1] != verify[n-1])
        return n;
    return 0;
}

#ifdef __riscv
#include "encoding.h"
#endif

#endif //__UTIL_H
