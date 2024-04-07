#pragma once

#include <malloc.h>
#include <stdint.h>
#include <stdarg.h>
#include <assert.h>
#include <inttypes.h>

#include "types.hpp"

#define HEAP_SIZE 104857600 // 100 Mb

#define HEAP_ALIGNMENT 8

/*  Current heap layout:
 *  ====================================================
 *  ||        used space      ||      free space      ||
 *  V=========================V=========================
 *  heap_start                bump_ptr                heap_end
 */  

typedef struct HeapDesc {
    size_t  bump_ptr;
    uint8_t* heap_start;
    uint8_t* heap_end;
} HeapDesc;

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

// Layout is important, edit carefully
#pragma pack (8)
typedef struct Object
{
    ObjHeader header;
    UL_value fields[];
} Object;


// Layout is important, edit carefully
// Special kind of Object
#pragma pack (8)
typedef struct ConsObj {
    ObjHeader header;
    UL_value  car;
    UL_value  cdr;
} ConsObj;

HeapDesc* heapInit();

UL_value  heapAllocCons(UL_value car, UL_value cdr);

inline UL_value heapCar(UL_value cons) {
    assert(is_cons(cons));
    cons = cons & ~(cons_mask);
    return ((ConsObj *) cons)->car;
}

inline UL_value heapCdr(UL_value cons) {
    assert(is_cons(cons));
    cons = cons & ~(cons_mask);
    return ((ConsObj *) cons)->cdr;
}

void dumpInit();
void dumpHeap();