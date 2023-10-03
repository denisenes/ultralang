#pragma once

#include <malloc.h>
#include <stdint.h>
#include <stdarg.h>
#include <assert.h>
#include <inttypes.h>

#include "types.h"

#define HEAP_SIZE 104857600 // 100 Mb

#define HEAP_ALIGNMENT 8

/*  Current heap layout:
 *  ====================================================
 *  ||        used space      ||      free space      ||
 *  V=========================V=========================
 *  heap_start                bump_ptr                heap_end
 */  

typedef struct Heap {
    uint8_t  bump_ptr;
    uint8_t* heap_start;
    uint8_t* heap_end;
} Heap;

typedef enum ObjType {
    CONS,
    CLOSURE,
    STRING,
    ARRAY
} ObjType;

/*
 * TAGS:
 * BIT1, BIT2 -> type
 * 
 * BIT3 -> markbit
 */
typedef struct ObjHeader {
    uint8_t tags;
    uint8_t sizebyte1;
    uint8_t sizebyte2;
    uint8_t sizebyte3;
} ObjHeader;

#pragma pack (8)
typedef struct ConsObj {
    ObjHeader header;
    val       car;
    val       cdr;
} ConsObj;

Heap* heapInit();

val   heapAllocCons(val car, val cdr);
val   heapCar(val cons);
val   heapCdr(val cons);

void dumpInit();
void dumpHeap();