#include <stdio.h>
#include <string.h>

extern int rust_main();

int main() { return rust_main(); }

void *roc_memcpy(void *dest, const void *src, size_t n) {
  return memcpy(dest, src, n);
}

void *roc_memset(void *str, int c, size_t n) { return memset(str, c, n); }