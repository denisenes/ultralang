#include "printer.hpp"

std::string consToString(UL_value v) {
	UL_value car = heapCar(v);
	UL_value cdr = heapCdr(v);
	if (is_nil(cdr)) {
		return valueToString(car);
	} else {
		assert(is_cons(cdr));
		return valueToString(car) + ", " + consToString(cdr);
	}
}

std::string valueToString(UL_value v) {
	if (is_fixnum(v)) {
		int64_t int_value = fixnum_to_int(v);
		return std::to_string(int_value);
	} else if (is_bool(v)) {
		if (bool_to_int(v)) {
			return "#True";
		} else {
			return "#False";
		} 
	} else if (is_nil(v)) {
		return "[]";
	} else if (is_cons(v)) {
		return "[" + consToString(v) + "]";
	} else {
		printf("Undefined type of value: %lu\n", (uint64_t) v);
		fatal("internal error");
		return "";
	}
}