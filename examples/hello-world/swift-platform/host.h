#include <stdlib.h>

struct RocStr {
    char* bytes;
    size_t len;
    size_t capacity;
};

extern struct RocStr roc__mainForHost_1_exposed();
