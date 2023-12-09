#pragma once

#include <cstddef>
#include <cstdint>

#define REFCOUNT_ONE ((ptrdiff_t)1 << (sizeof(ptrdiff_t) * 8 - 1))

void *roc_alloc(size_t size, uint32_t alignment);
void *roc_realloc(void *ptr, size_t new_size, size_t old_size, size_t alignment);
void roc_dealloc(void *ptr, uint32_t alignment);
void roc_panic(const char *message, uint32_t _tag_id);
void *roc_memset(void *ptr, int value, size_t num_bytes);

#ifdef DEBUG
#include <cstdio>
#define DEBUG_PRINT(...) printf(__VA_ARGS__)
#else
#define DEBUG_PRINT(...)
#endif

namespace Roc
{
    /**
     * Base class for all Roc value types
     * Implements common logic like reference counting.
     * We don't use virtual functions because we want to avoid the vtable.
     */
    class Value
    {
    public:
        // Default implementation assumes no heap allocation
        // Valid for numbers, records, etc.
        bool rc_unique() const { return true; };
        void rc_increment() {};
        void rc_decrement() {};
    };
};
