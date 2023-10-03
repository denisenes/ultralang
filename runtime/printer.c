#include "printer.h"

void printCons(val v) {
	assert(is_fixnum(heapCar(v)));
	assert(is_fixnum(heapCdr(v)));

	printf("(%llu, %llu)\n",
		(unsigned long long) fixnum_to_int(heapCar(v)), 
		(unsigned long long) fixnum_to_int(heapCdr(v)));
}

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
	} else if (is_cons(v)) {
		printCons(v);
	} else {
		printf("Undefined type of value: %lld\n", (long long) v);
	}
}