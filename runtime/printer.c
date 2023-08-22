#include <stdio.h>

#include "types.h"

void printValue(val v) {
	if (is_fixnum(v)) {
		printf("%lld\n", (long long) fixnum_to_int(v));
	} else if (is_bool(v)) {
		if (bool_to_int(v)) {
			printf("#t\n");
		} else {
			printf("#f\n");
		} 
	} else if (is_nil(v)) {
		printf("nil\n");
	} else {
		printf("Undefined type of value: %lld\n", (long long) v);
	}
}