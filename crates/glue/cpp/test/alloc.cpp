#include "internal.h"
#include "str.h"
#include <cstdlib>
#include <cstdio>
#include <cstring>

void *roc_alloc(size_t size, uint32_t _alignment)
{
    return malloc(size);
}

void *roc_realloc(void *ptr, size_t new_size, size_t _old_size, size_t _alignment)
{
    return realloc(ptr, new_size);
}

void roc_dealloc(void *ptr, uint32_t _alignment)
{
    free(ptr);
}

void roc_panic(const Roc::Str *message, uint32_t tag_id)
{
    const char* prefix;
    switch (tag_id) {
        case 0:
            prefix = "Roc crashed with:";
            break;
        case 1:
            prefix = "The program crashed with:";
            break;
        default:
            prefix = "An error occurred:";
            break;
    }
    fprintf(stderr, "%s\n\n\t%s\n", prefix, message->contents());
    exit(1);
}

void *roc_memset(void *ptr, int value, size_t num_bytes)
{
    return memset(ptr, value, num_bytes);
}
