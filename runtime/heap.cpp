#include "heap.hpp"
#include "utils.hpp"
#include "comp_api.hpp"

static HeapDesc* heapPtr; 
static FILE* dumpFile;

static inline void setType(ObjHeader* header, ObjType type) {
    header->tags |= type << 6;
}

static inline ObjType getType(ObjHeader* header) {
    return (ObjType) (header->tags >> 6);
}

static inline char * typeToStr(ObjType type) {
    switch (type) {
    case CONS:
        return "CONS";
    case CLOSURE:
    case STRING:
    case ARRAY:
    default:
        return "UNKNOWN OR NOT IMPLEMENTED TYPE";
    }
}

static inline void maybeTriggerGC() {
    uint8_t* heap_start   = heapPtr->heap_start;
    size_t  bump_pointer = heapPtr->bump_ptr;

    if (heap_start + bump_pointer >= heapPtr->heap_end) {
        fatal("GC is not implemented yet");
    }
}

static inline void updateObjSize(ObjHeader* header) {
    header->sizebyte1 = 0;
    header->sizebyte2 = 0;
    header->sizebyte3 = sizeof(ConsObj);
}

static inline int getObjSize(ObjType type) {
    switch (type) {
    case CONS:
        return sizeof(ConsObj);
    default:
        fatal("Unknown object type");
        return 0;
    }
}

static inline void moveBumpPtr(size_t offset) {
    heapPtr->bump_ptr += offset;
}

HeapDesc* heapInit() {
    uint8_t* h_ptr = (uint8_t*) aligned_alloc(HEAP_SIZE, HEAP_ALIGNMENT);
    HeapDesc* heapDesc = (HeapDesc*) malloc(sizeof(HeapDesc));

    if (heapDesc == NULL || h_ptr == NULL) {
        fatal("Cannot initialize heap");
    }
    heapPtr = heapDesc;

    heapDesc->heap_start = h_ptr;
    heapDesc->heap_end   = h_ptr + HEAP_SIZE;
    heapDesc->bump_ptr   = 0;

    //printf("[Heap] start: %p\n", heap->heap_start);
    //printf("[Heap] end:   %p\n", heap->heap_end);
    return heapDesc;
}

UL_value heapAllocCons(UL_value car, UL_value cdr) {
    maybeTriggerGC();

    ConsObj* cons = (ConsObj*) (heapPtr->heap_start + heapPtr->bump_ptr);
    moveBumpPtr(sizeof(ConsObj));
    
    updateObjSize(&cons->header);
    cons->car = car;
    cons->cdr = cdr;

    // printf("[Heap] cons (addr:%p) (car %"PRIu64") (cdr: %"PRIu64")\n",
    //     cons, cons->car, cons->cdr);

    assert((((uint64_t) cons) & cons_mask) == 0); // heap must be aligned by 8 byte
    return (UL_value) tagged_cons((uint64_t) cons);
}

static void dumpLog(const char* format, ...);

void dumpHeap() {
    if (heapPtr->bump_ptr == 0) {
        dumpLog("Heap is empty\n");
        return;
    }

    dumpLog("Heap start:   [%p]\n", heapPtr->heap_start);
    dumpLog("Heap end:     [%p]\n", heapPtr->heap_end);
    dumpLog("Bump pointer: [%p]\n", heapPtr->bump_ptr);

    for (uint8_t* ptr = heapPtr->heap_start; ptr < heapPtr->heap_start + heapPtr->bump_ptr;) {
        ObjHeader* header = (ObjHeader*) ptr;

        ObjType type = getType(header);
        int size = getObjSize(type);
        switch (type) {
            case CONS: {
                    assert((((uint64_t) ptr) & cons_mask) == 0); // heap must be aligned by 8 byte
                    UL_value currentCons = (UL_value) tagged_cons((uint64_t) ptr);
                    UL_value car = heapCar(currentCons);
                    UL_value cdr = heapCdr(currentCons);
                    dumpLog("Found cons cell in [%p], carval=[%p], cdrval=[%p], type=%d, size=%d\n", 
                        ptr, car, cdr, type, size);
                }
                break;
            default:
                dumpLog("Found object in [%p], type=%d, size=%d\n", ptr, type, size);
        }

        ptr += size;
    }
}

void dumpInit() {
    dumpFile = fopen("heapdump", "w");
    if (dumpFile == NULL) {
        fatal("Cannot create heap dump");
    }
}

static void dumpLog(const char* format, ...) {
    va_list args;
    va_start(args, format);

    vfprintf(dumpFile, format, args);

    va_end(args);
}