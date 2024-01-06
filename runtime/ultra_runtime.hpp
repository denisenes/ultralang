#pragma once

#include <string>
#include <iostream>

#include "types.hpp"
#include "heap.hpp"
#include "utils.hpp"

typedef struct Globals {
    HeapDesc* heap;
} Globals;

// located in ultra.s
extern "C" UL_value ultra_entrypoint();

void termination(int code);