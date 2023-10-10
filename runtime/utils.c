#include "utils.h"
#include "comp_api.h"

void fatal(char * message) {
    fprintf(stderr, "%s\n", message);
    exit(1);
}

void UL_fatalCoded(uint64_t code) {
    switch (fixnum_to_int(code))
    {
    case 1:
        fprintf(stderr, "Cond variants are not exhaustive\n");
        break;
    default:
        fprintf(stderr, "Unknown runtime error\n");
        break;
    }
    exit(1);
}