#include <stdlib.h>

struct RocStr {
    char* bytes;
    size_t len;
    size_t capacity;
};

extern void roc__mainForHost_1_exposed_generic(const struct RocStr *data);
