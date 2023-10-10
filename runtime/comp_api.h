#pragma once

#include "types.h"

val 
UL_heapAllocCons(val car, val cdr) 
asm("ULTRA_cons");

void 
UL_fatalCoded(uint64_t code) 
asm("ULTRA_runtime_error");