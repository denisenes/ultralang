#include "heap.h"
#include "utils.h"

static Heap* heapPtr; 

static inline void maybeTriggerGC() {
    size_t* heap_start   = heapPtr->heap_start;
    size_t  bump_pointer = heapPtr->bump_ptr;

    if (heap_start + bump_pointer >= heapPtr->heap_end) {
        fatal("GC is not implemented yet");
    }
}

static inline void updateObjSize(ObjHeader * header) {
    header->sizebyte1 = 0;
    header->sizebyte2 = 0;
    header->sizebyte3 = sizeof(ConsObj);
}

static inline void moveBumpPtr(size_t offset) {
    heapPtr->bump_ptr += offset;
}

Heap* heapInit() {
    size_t* h_ptr = (size_t*) malloc((size_t) HEAP_SIZE);
    Heap* heap = (Heap*) malloc(sizeof(Heap));

    if (heap == NULL || h_ptr == NULL) {
        fatal("Cannot initialize heap");
    }
    heapPtr = heap;

    heap->heap_start = h_ptr;
    heap->heap_end   = h_ptr + HEAP_SIZE;
    heap->bump_ptr   = 0;
    return heap;
}

inline val heapAllocCons(val car, val cdr) {
    maybeTriggerGC();

    ConsObj * cons = (ConsObj *) heapPtr->heap_start + heapPtr->bump_ptr;
    moveBumpPtr(sizeof(ConsObj));
    
    updateObjSize(&cons->header);
    cons->car = car;
    cons->cdr = cdr;

    return (val) cons;
}