#include "utils.hpp"
#include "comp_api.hpp"
#include "ultra_runtime.hpp"

void fatal(char * message) {
    fprintf(stderr, "%s\n", message);
    termination(1);
    exit(1);
}

void fatalCoded(uint64_t code) {
    switch (fixnum_to_int(code))
    {
    case 0:
        fprintf(stderr, "Internal error\n");
        break;
    case 1:
        fprintf(stderr, "Cond variants are not exhaustive\n");
        break;
    default:
        fprintf(stderr, "Unknown runtime error\n");
        break;
    }
    termination(1);
    exit(1);
}