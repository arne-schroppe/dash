#include "heap.h"
#include <stdlib.h>
#include <stdio.h>

static vm_value *heap = 0;
static size_t initial_heap_size = 4096;
static heap_address next_free_address = 0;

void init_heap() {
  if (heap) {
    free(heap);
  }
  next_free_address = 1; // address 0 is reserved to indicate that no address has been set
  heap = calloc(initial_heap_size, sizeof(vm_value)); //TODO this obviously needs some improvement
}

heap_address heap_alloc(size_t size) {
  heap_address old = next_free_address;
  next_free_address += size;
  if(next_free_address > initial_heap_size) {
    printf("Out of memory!\n");
    exit(-1);
  }
  return old;
}

vm_value *heap_get_pointer(heap_address addr) {
  if(addr > initial_heap_size) {
    printf("Illegal memory address: %zu!\n", addr);
    exit(-1);
  }
  return &heap[addr];
}

