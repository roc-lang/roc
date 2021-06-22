#include <stdlib.h>
#include <unistd.h>
#include <stdio.h>
#include <errno.h>
#include <string.h>

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

int main() {
    // Make space for the result
    struct RocCallResult callresult;

    // Call roc to populate the callresult
    roc__mainForHost_1_exposed(&callresult);

    struct RocStr str = callresult.content;

    // Write to stdout
    write(1, &str.bytes, 14);

    return 0;
}
