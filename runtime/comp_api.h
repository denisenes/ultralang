#pragma once

#include "types.h"

inline val heapAllocCons(val car, val cdr) asm("ULTRA_cons");