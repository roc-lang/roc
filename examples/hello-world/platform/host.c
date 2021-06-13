#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

void* roc_alloc(size_t size, unsigned int alignment) {
    return malloc(size);
}

void* roc_realloc(void* ptr, size_t old_size, size_t new_size, unsigned int alignment) {
    return realloc(ptr, new_size);
}

void roc_dealloc(void* ptr, unsigned int alignment) {
    free(ptr);
}

struct RocStr {
    char* bytes;
    size_t len;
};

struct RocCallResult {
    size_t flag;
    struct RocStr content;
};

extern void roc__mainForHost_1_exposed(struct RocCallResult *re);

const size_t MAX_STACK_STR_BYTES = 1024;

int main() {
    // make space for the result
    struct RocCallResult callresult;

    // call roc to populate the callresult
    roc__mainForHost_1_exposed(&callresult);
    struct RocStr str = callresult.content;

    // Convert from RocStr to C string (null-terminated char*)
    size_t len = str.len;
    char* c_str;

    // Allocate on the stack unless the string is particularly big.
    // (Don't want a stack overflow!)
    if (len <= MAX_STACK_STR_BYTES) {
        c_str = (char*)alloca(len + 1);
    } else {
        c_str = (char*)malloc(len + 1);
    }

    memcpy(c_str, str.bytes, len);

    // null-terminate
    c_str[len] = 0;

    // Print the string to stdout
    printf("%s\n", c_str);

    // Pointer to the beginning of the RocStr's actual allocation, which is
    // the size_t immediately preceding the first stored byte.
    size_t* str_base_ptr = (size_t*)str.bytes - 1;

    // If *str_base_ptr is equal to 0, then the string is in the
    // read-only data section of the binary, and can't be freed!
    if (*str_base_ptr != 0) {
        roc_dealloc(str_base_ptr, 8);
    }

    // If we malloc'd c_str, free it.
    if (len > MAX_STACK_STR_BYTES) {
        free(c_str);
    }

    return 0;
}
