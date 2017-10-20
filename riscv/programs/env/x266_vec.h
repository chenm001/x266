#ifndef __X266_RISCV_VEC_H__
#define __X266_RISCV_VEC_H__

/* Define the default attributes for the functions in this file. */
#define __DEFAULT_FN_ATTRS __attribute__((__always_inline__, __nodebug__))

typedef char __v32i8 __attribute__((aligned(32),vector_size(32)));

static __inline__ __v32i8 __DEFAULT_FN_ATTRS
__v_ld32x8(const char *__p, int __i)
{
    return (v32i8)__rv32_v_ld32x8(__p, __i);
}

static __inline__ void __DEFAULT_FN_ATTRS
__v_st32x8(__v32i8 __v, const char *__p, int __i)
{
    return __rv32_v_st32x8(__v, __p, __i);
}

#endif /* __X266_RISCV_VEC_H__ */
