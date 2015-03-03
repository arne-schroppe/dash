#include "heap.h"
#include <stdlib.h>

static vm_value *heap = 0;
static size_t initial_heap_size = 1024;
static heap_address next_free_address = 0;

void init_heap() {
  heap = calloc(initial_heap_size, sizeof(vm_value)); //TODO this obviously needs some improvement
}

heap_address heap_alloc(size_t size) {
  if (heap == 0) {
    init_heap();
  }
  heap_address old = next_free_address;
  next_free_address += size;
  return old;
}

vm_value *heap_get_pointer(heap_address addr) {
  return &heap[addr];
}

