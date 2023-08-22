#pragma once

#include <stdio.h>

#include "types.h"
#include "heap.h"

typedef struct Globals {
    Heap* heap;
} Globals;

// located in ultra.s
extern val ultra_entrypoint();