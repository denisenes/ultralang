#pragma once

#include <stdint.h>

using U8  = uint8_t;
using U16 = uint16_t;
using I8  = int8_t;
using I16 = int16_t;

#define U16_FROM_BYTES(b1, b2) ((b1 << UINT8_WIDTH) | b2)
#define U16_LO(value)          ((U8)(value & 0xFF))
#define U16_HI(value)          ((U8)(value >> 8))
