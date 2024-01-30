#pragma once

#include <stdint.h>

typedef uint64_t QWORD;

/*
 * It is assumed that runtime is compiled for platform where
 * sizeof(pointer) == 8 byte, so we can make dirty casts:
 * some ptr type --> uint64_t
 */
typedef QWORD UL_value;

#define fixnum_mask  0b00000011
#define fixnum_tag   0b00000000
#define fixnum_shift 2
#define is_fixnum(f)     ((f & fixnum_mask) == fixnum_tag)
#define fixnum_to_int(f) (((int64_t) f) >> fixnum_shift)

#define bool_mask  0b1111111
#define bool_tag   0b0011111
#define bool_shift 7
#define is_bool(b)     ((b & bool_mask) == bool_tag)
#define bool_to_int(b) ((b >> bool_shift) & 0b1)

#define nil_mask 0b11111111
#define nil_tag  0b00101111
#define is_nil(v) ((v & nil_mask) == nil_tag)

#define cons_mask  0b00000111llu
#define cons_tag   0b00000001llu
#define cons_shift 3
#define is_cons(v) ((v & cons_mask) == cons_tag)
#define tagged_cons(v) (v | cons_tag)