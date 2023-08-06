#include <stdio.h>
#include "types.h"

void printValue(val v) {
	if (is_fixnum(v)) {
		printf("%lld\n", fixnum_to_int(v));
	} else if (is_bool(v)) {
		if (bool_to_int(v)) {
			printf("#t\n");
		} else {
			printf("#f\n");
		} 
	} else if (is_nil(v)) {
		printf("nil\n");
	} else {
		printf("Undefined type of value: %lld\n", v);
	}
}

// located int ultra.s
extern int ultra_entrypoint();

int main(int argc, char ** argv) {
	printValue(ultra_entrypoint());
	return 0;
}
