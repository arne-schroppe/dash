#include "tiny_spec/tiny_spec.h"
#include "vm_spec.h"
#include "vm_equality_spec.h"


int main(int argc, char **argv) {
	verify_spec(vm_spec);
  verify_spec(vm_equality_spec);

  return 0;
}

