#pragma once

#include "types.hpp"

UL_value 
heapAllocCons(UL_value car, UL_value cdr) 
asm("ULTRA_cons");

void 
fatalCoded(uint64_t code) 
asm("ULTRA_runtime_error");