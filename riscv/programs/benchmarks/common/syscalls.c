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
#ifdef __llvm__
    asm volatile ("csrrw x0, 0x7B0, %0" : : "r" (lo));
#else
    asm volatile ("csrw dcsr, %0" : : "r" (lo));
#endif
    // print high 16 bits
    int hi = (c >> 16) | (((uint32_t)PrintIntHigh) << 16);
#ifdef __llvm__
    asm volatile ("csrrw x0, 0x7B0, %0" : : "r" (hi));
#else
    asm volatile ("csrw dcsr, %0" : : "r" (hi));
#endif
}

void printChar(uint32_t c) {
    c = (c & 0x0000FFFF) | (((uint32_t)PrintChar) << 16);
#ifdef __llvm__
    asm volatile ("csrrw x0, 0x7B0, %0" : : "r" (c));
#else
    asm volatile ("csrw dcsr, %0" : : "r" (c));
#endif
}

void printStr(char* x) {
  while(1) {
     // get 4B aligned addr
     uint32_t* y = (uint32_t*)(((uint32_t)x) & ~0x3);
     uint32_t fullC = *y;
     uint32_t mod = ((uint32_t)x) & 0x3;
     uint32_t shift = mod << 3;
     uint32_t c = (fullC >> shift) & 0xFF;
     if(c == (uint32_t)'\0')
       break;
     printChar(c);
     x++;
  }
}

void printhex(uint64_t x)
{
  char str[17];
  int i;
  for (i = 0; i < 16; i++)
  {
    str[15-i] = (x & 0xF) + ((x & 0xF) < 10 ? '0' : 'a'-10);
    x >>= 4;
  }
  str[16] = 0;

  printStr(str);
}

static inline void printnum(void (*putch)(int, void**), void **putdat,
                    unsigned long long num, unsigned base, int width, int padc)
{
  unsigned digs[sizeof(num)*CHAR_BIT];
  int pos = 0;

  while (1)
  {
    digs[pos++] = num % base;
    if (num < base)
      break;
    num /= base;
  }

  while (width-- > pos)
    putch(padc, putdat);

  while (pos-- > 0)
    putch(digs[pos] + (digs[pos] >= 10 ? 'a' - 10 : '0'), putdat);
}

static unsigned long long getuint(va_list *ap, int lflag)
{
  if (lflag >= 2)
    return va_arg(*ap, unsigned long long);
  else if (lflag)
    return va_arg(*ap, unsigned long);
  else
    return va_arg(*ap, unsigned int);
}

static long long getint(va_list *ap, int lflag)
{
  if (lflag >= 2)
    return va_arg(*ap, long long);
  else if (lflag)
    return va_arg(*ap, long);
  else
    return va_arg(*ap, int);
}

static void vprintfmt(void (*putch)(int, void**), void **putdat, const char *fmt, va_list ap)
{
  register const char* p;
  const char* last_fmt;
  register int ch, err;
  unsigned long long num;
  int base, lflag, width, precision, altflag;
  char padc;

  while (1) {
    while ((ch = *(unsigned char *) fmt) != '%') {
      if (ch == '\0')
        return;
      fmt++;
      putch(ch, putdat);
    }
    fmt++;

    // Process a %-escape sequence
    last_fmt = fmt;
    padc = ' ';
    width = -1;
    precision = -1;
    lflag = 0;
    altflag = 0;
  reswitch:
    switch (ch = *(unsigned char *) fmt++) {

    // flag to pad on the right
    case '-':
      padc = '-';
      goto reswitch;

    // flag to pad with 0's instead of spaces
    case '0':
      padc = '0';
      goto reswitch;

    // width field
    case '1':
    case '2':
    case '3':
    case '4':
    case '5':
    case '6':
    case '7':
    case '8':
    case '9':
      for (precision = 0; ; ++fmt) {
        precision = precision * 10 + ch - '0';
        ch = *fmt;
        if (ch < '0' || ch > '9')
          break;
      }
      goto process_precision;

    case '*':
      precision = va_arg(ap, int);
      goto process_precision;

    case '.':
      if (width < 0)
        width = 0;
      goto reswitch;

    case '#':
      altflag = 1;
      goto reswitch;

    process_precision:
      if (width < 0)
        width = precision, precision = -1;
      goto reswitch;

    // long flag (doubled for long long)
    case 'l':
      lflag++;
      goto reswitch;

    // character
    case 'c':
      putch(va_arg(ap, int), putdat);
      break;

    // string
    case 's':
      if ((p = va_arg(ap, char *)) == NULL)
        p = "(null)";
      if (width > 0 && padc != '-')
        for (width -= strnlen(p, precision); width > 0; width--)
          putch(padc, putdat);
      for (; (ch = *p) != '\0' && (precision < 0 || --precision >= 0); width--) {
        putch(ch, putdat);
        p++;
      }
      for (; width > 0; width--)
        putch(' ', putdat);
      break;

    // (signed) decimal
    case 'd':
      num = getint(&ap, lflag);
      if ((long long) num < 0) {
        putch('-', putdat);
        num = -(long long) num;
      }
      base = 10;
      goto signed_number;

    // unsigned decimal
    case 'u':
      base = 10;
      goto unsigned_number;

    // (unsigned) octal
    case 'o':
      // should do something with padding so it's always 3 octits
      base = 8;
      goto unsigned_number;

    // pointer
    case 'p':
      //static_assert(sizeof(long) == sizeof(void*));
      lflag = 1;
      putch('0', putdat);
      putch('x', putdat);
      /* fall through to 'x' */

    // (unsigned) hexadecimal
    case 'x':
      base = 16;
    unsigned_number:
      num = getuint(&ap, lflag);
    signed_number:
      printnum(putch, putdat, num, base, width, padc);
      break;

    // escaped '%' character
    case '%':
      putch(ch, putdat);
      break;

    // unrecognized escape sequence - just print it literally
    default:
      putch('%', putdat);
      fmt = last_fmt;
      break;
    }
  }
}

int printf(const char* fmt, ...)
{
  va_list ap;
  va_start(ap, fmt);

  vprintfmt((void*)printChar, 0, fmt, ap);

  va_end(ap);
  return 0; // incorrect return value, but who cares, anyway?
}

static inline void sprintf_putch(int ch, void** data)
{
  char** pstr = (char**)data;
  **pstr = ch;
  (*pstr)++;
}

int sprintf(char* str, const char* fmt, ...)
{
  va_list ap;
  char* str0 = str;
  va_start(ap, fmt);

  vprintfmt(sprintf_putch, (void**)&str, fmt, ap);
  *str = 0;

  va_end(ap);
  return str - str0;
}

void* memcpy(void* dest, const void* src, unsigned len)
{
  if ((((uintptr_t)dest | (uintptr_t)src | len) & (sizeof(uintptr_t)-1)) == 0) {
    const uintptr_t* s = src;
    uintptr_t *d = dest;
    while (d < (uintptr_t*)(dest + len))
      *d++ = *s++;
  } else {
    const char* s = src;
    char *d = dest;
    while (d < (char*)(dest + len))
      *d++ = *s++;
  }
  return dest;
}

void* memset(void* dest, int byte, unsigned len)
{
  if ((((uintptr_t)dest | len) & (sizeof(uintptr_t)-1)) == 0) {
    uintptr_t word = byte & 0xFF;
    word |= word << 8;
    word |= word << 16;
    word |= word << 16 << 16;

    uintptr_t *d = dest;
    while (d < (uintptr_t*)(dest + len))
      *d++ = word;
  } else {
    char *d = dest;
    while (d < (char*)(dest + len))
      *d++ = byte;
  }
  return dest;
}

unsigned strlen(const char *s)
{
  const char *p = s;
  while (*p)
    p++;
  return p - s;
}

unsigned strnlen(const char *s, unsigned n)
{
  const char *p = s;
  while (n-- && *p)
    p++;
  return p - s;
}

int strcmp(const char* s1, const char* s2)
{
  unsigned char c1, c2;

  do {
    c1 = *s1++;
    c2 = *s2++;
  } while (c1 != 0 && c1 == c2);

  return c1 - c2;
}

char* strcpy(char* dest, const char* src)
{
  char* d = dest;
  while ((*d++ = *src++))
    ;
  return dest;
}

long atol(const char* str)
{
  long res = 0;
  int sign = 0;

  while (*str == ' ')
    str++;

  if (*str == '-' || *str == '+') {
    sign = *str == '-';
    str++;
  }

  while (*str) {
    res *= 10;
    res += *str++ - '0';
  }

  return sign ? -res : res;
}


void toHostExit(uint32_t ret) {
    ret = (ret & 0x0000FFFF) | (((uint32_t) ExitCode) << 16);
#ifdef __llvm__
    asm volatile ("csrrw x0, 0x7B0, %0" : : "r" (ret));
#else
    asm volatile ("csrw dcsr, %0" : : "r" (ret));
#endif
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
