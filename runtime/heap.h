#pragma once

#include <malloc.h>
#include <stdint.h>

#include "types.h"

#define HEAP_SIZE 104857600 // 100 Mb

/*  Current heap layout:
 *  ====================================================
 *  ||        used space      ||      free space      ||
 *  V=========================V=========================
 *  heap_start                bump_ptr                heap_end
 */  

typedef struct Heap {
    size_t* heap_start;
    size_t   bump_ptr;
    size_t* heap_end;
} Heap;

typedef struct ObjHeader {
    uint8_t tags;
    uint8_t sizebyte1;
    uint8_t sizebyte2;
    uint8_t sizebyte3;
} ObjHeader;

#pragma pack (4)
typedef struct ConsObj {
    ObjHeader header;
    val       car;
    val       cdr;
} ConsObj;

Heap* heapInit();
val   heapAllocCons(val car, val cdr);