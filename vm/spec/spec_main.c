#include "tiny_spec/tiny_spec.h"
#include "vm_spec.h"


int main(int argc, char **argv) {
	return verify_spec(vm_spec);
}

