#include <stdlib.h>
extern void* test_wrapper();

void* roc_alloc(size_t size, unsigned int alignment) {
    return malloc(size);
}

void* roc_realloc(void* ptr, size_t old_size, size_t new_size, unsigned int alignment) {
  return realloc(ptr, new_size);
}

void roc_dealloc(void* ptr, unsigned int alignment) {
    free(ptr);
}

// Having a main function seems to make it easier to convince tools
// to include everything in the output binary.
// Using C as the source language makes it easier to convince them
// to include libc for wasm target, not host.
// The returned int is the memory address of the test result
int main(void) {
    return test_wrapper();
}
