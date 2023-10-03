#include "ultra_runtime.h"
#include "printer.h"

Globals globals;

void runtimePreparation() {
	dumpInit();
	globals.heap = heapInit();
}

void termination() {
	dumpHeap();
}

int main(int argc, char ** argv) {
	runtimePreparation();

	printValue(ultra_entrypoint());

	termination();
	return 0;
}
