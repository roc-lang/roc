#include <errno.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

//#define ROC_PLATFORM_DEBUG

void alloc_panic(size_t size);

void *roc_alloc(size_t size, unsigned int alignment) {
#ifdef ROC_PLATFORM_DEBUG
  printf("Allocating %llu (alignment %ud) ", (unsigned long long)size,
         alignment);
#endif
  void *result = malloc(size);

#ifdef ROC_PLATFORM_DEBUG
  printf("at: %p\n", result);
#endif

  if (result == NULL) {
    if (size == 0) { // <-  malloc is allowed to 'succeed' with NULL iff size == 0.
      return NULL;
    }
    // Otherwise, it is an indication of failure.
    alloc_panic(size);
  }

  return result;
}

void *roc_realloc(void *ptr, size_t new_size, size_t old_size,
                  unsigned int alignment) {
#ifdef ROC_PLATFORM_DEBUG
  printf("Rellocating %p (%llu -> %llu) (alignment %ud) ", ptr,
         (unsigned long long)old_size, (unsigned long long)new_size, alignment);
#endif

  void *result = realloc(ptr, new_size);

#ifdef ROC_PLATFORM_DEBUG
  printf("at: %p\n", result);
#endif

  if (result == NULL) {
    if (new_size ==
        0) { // <-  realloc is allowed to 'succeed' with NULL iff size == 0.
      return NULL;
    }
    // Otherwise, it is an indication of failure.
    alloc_panic(new_size);
  }

  return result;
}

void roc_dealloc(void *ptr, unsigned int alignment) {

#ifdef ROC_PLATFORM_DEBUG
  printf("Deallocating %p (alignment %ud)\n", ptr, alignment);
#endif
  free(ptr);
}

void roc_panic(void *ptr, unsigned int alignment) {
  char *msg = (char *)ptr;
  fprintf(stderr,
          "Application crashed with message\n\n    %s\n\nShutting down\n", msg);
  exit(0);
}

void alloc_panic(size_t size) {
  char msg[100];
  sprintf(msg, "Memory allocation failed. Could not allocate %llu bytes",
          (unsigned long long)size);

  roc_panic(msg, 0);
}

void *roc_memcpy(void *dest, const void *src, size_t n) {
#ifdef ROC_PLATFORM_DEBUG
  printf("memcpy %p -> %p (size: %llu)\n", src, dest, (unsigned long long)n);
#endif
  return memcpy(dest, src, n);
}

void *roc_memmove(void *dest, const void *src, size_t n) {
  return memmove(dest, src, n);
}

void *roc_memset(void *str, int c, size_t n) {
  return memset(str, c, n);
}

struct RocStr {
  char *bytes;
  size_t len;
  size_t capacity;
};

bool is_small_str(struct RocStr str) { return ((ssize_t)str.capacity) < 0; }

// Determine the length of the string, taking into
// account the small string optimization
size_t roc_str_len(struct RocStr str) {
  char *bytes = (char *)&str;
  char last_byte = bytes[sizeof(str) - 1];
  char last_byte_xored = last_byte ^ 0b10000000;
  size_t small_len = (size_t)(last_byte_xored);
  size_t big_len = str.len;

  // Avoid branch misprediction costs by always
  // determining both small_len and big_len,
  // so this compiles to a cmov instruction.
  if (is_small_str(str)) {
    return small_len;
  } else {
    return big_len;
  }
}

extern void roc__mainForHost_1_exposed_generic(struct RocStr *string);

int main() {
  struct RocStr str;
  roc__mainForHost_1_exposed_generic(&str);

  // Determine str_len and the str_bytes pointer,
  // taking into account the small string optimization.
  size_t str_len = roc_str_len(str);
  char *str_bytes;

  if (is_small_str(str)) {
    str_bytes = (char *)&str;
  } else {
    str_bytes = str.bytes;
  }

  // Write to stdout
  size_t written = fwrite(str_bytes, sizeof(char), str_len, stdout);
  if (fflush(stdout) == 0 && written == str_len) {
    // Writing succeeded!

    // dealllocate the roc string
    if (!(is_small_str(str))) {
        roc_dealloc(str_bytes - 8, 1);
    }

    return 0;
  } else {
    printf("Error writing to stdout: %s\n", strerror(errno));

    // dealllocate the roc string
    if (!(is_small_str(str))) {
        roc_dealloc(str_bytes - 8, 1);
    }

    return 1;
  }

}
