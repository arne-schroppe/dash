#include <stdio.h>
#include "opcodes.h"


vm_instruction program[] = {
  op_loadi(1, 5),
  op_loadi(2, 32),
  op_add(0, 1, 2),
  op_halt
};


int main (int argc, char *argv[]) {
  execute(program);
  return 0;
}
