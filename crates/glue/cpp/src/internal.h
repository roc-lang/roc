#pragma once

#include <cstddef>
#include <cstdint>

#define REFCOUNT_ONE ((ptrdiff_t)1 << (sizeof(ptrdiff_t) * 8 - 1))

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

    class Str;
};

extern "C" void *roc_alloc(size_t size, uint32_t alignment);
extern "C" void *roc_realloc(void *ptr, size_t new_size, size_t old_size, size_t alignment);
extern "C" void roc_dealloc(void *ptr, uint32_t alignment);
extern "C" void roc_panic(const Roc::Str *message, uint32_t _tag_id);
extern "C" void *roc_memset(void *ptr, int value, size_t num_bytes);
