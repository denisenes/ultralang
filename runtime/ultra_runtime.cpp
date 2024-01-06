#include "ultra_runtime.hpp"
#include "printer.hpp"

Globals globals;

void runtimePreparation() {
	dumpInit();
	globals.heap = heapInit();
}

void termination(int code) {
	dumpHeap();
	exit(code);
}

int main(int argc, char ** argv) {
	runtimePreparation();

	std::string res = valueToString(ultra_entrypoint());
	std::cout << res << std::endl;

	termination(0);
	return 0;
}
