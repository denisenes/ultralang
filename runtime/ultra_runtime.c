#include "ultra_runtime.h"
#include "printer.h"

Globals globals;

void runtimePreparation() {
	globals.heap = heapInit();
}

int main(int argc, char ** argv) {
	runtimePreparation();

	printValue(ultra_entrypoint());
	return 0;
}
