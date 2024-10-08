#pragma once

#include <string.h>
#include "internal.h"

namespace Roc
{
    struct roc_big_str
    {
        char *bytes;
        size_t length;
        size_t capacity;
    };

    const size_t SMALL_STRING_SIZE = sizeof(struct roc_big_str);
    const size_t SMALL_STRING_MAXLEN = SMALL_STRING_SIZE - 1;
    const size_t REFCOUNT_SIZE = sizeof(size_t);

    class Str : Value
    {
        union
        {
            struct roc_big_str big;
            char small[SMALL_STRING_SIZE];
        };

        ptrdiff_t inline *refcount_ptr() const
        {
            return (ptrdiff_t *)big.bytes - 1;
        }

#ifdef DEBUG
        void debug_representation()
        {
            printf("Roc::Str\n");
            printf("    is_small_str: %s\n", is_small_str() ? "true" : "false");
            printf("    big.bytes: %p\n", big.bytes);
            printf("    big.length: %zu\n", big.length);
            printf("    big.capacity: %zu\n", big.capacity);
            printf("    small: ");
            for (size_t i = 0; i < SMALL_STRING_SIZE; i++)
            {
                printf("%02x ", (unsigned char)small[i]);
            }
            printf("\n");
        }
#endif

    public:
        Str(const char *cstr = "", size_t requested_capacity = 0)
        {
            size_t len = strlen(cstr);

            if (len < SMALL_STRING_SIZE)
            {
                big = {};
                memcpy(small, cstr, len);
                small[SMALL_STRING_SIZE - 1] = len | 0x80;
            }
            else
            {
                size_t capacity = requested_capacity < len ? len : requested_capacity;
                // round up the capacity to the next alignment boundary
                capacity = ((capacity + REFCOUNT_SIZE - 1) / REFCOUNT_SIZE) * REFCOUNT_SIZE;
                size_t alloc_size = REFCOUNT_SIZE + capacity;
                void *allocation = roc_alloc(alloc_size, REFCOUNT_SIZE);
                ptrdiff_t *refcount = (ptrdiff_t *)allocation;
                *refcount = REFCOUNT_ONE;

                big.bytes = (char *)allocation + REFCOUNT_SIZE;
                big.length = len;
                big.capacity = capacity;
            }
        }

        ~Str()
        {
            rc_decrement();
        }

        bool is_small_str() const
        {
            return small[SMALL_STRING_SIZE - 1] < 0;
        }

        size_t length() const
        {
            return is_small_str()
                       ? (size_t)(small[SMALL_STRING_SIZE - 1] & 0x7f)
                       : big.length;
        }

        size_t capacity() const
        {
            return is_small_str()
                       ? SMALL_STRING_SIZE - 1
                       : big.capacity;
        }

        const char *contents() const
        {
            return is_small_str() ? small : big.bytes;
        }

        bool rc_unique() const
        {
            if (is_small_str())
            {
                return true;
            }
            ptrdiff_t *refcount = (ptrdiff_t *)big.bytes - REFCOUNT_SIZE;
            return *refcount == REFCOUNT_ONE;
        }

        void rc_increment()
        {
            if (is_small_str())
                return;
            if (big.bytes == NULL)
            {
                const Str msg("Attempted to increment refcount of freed allocation");
                roc_panic(&msg, 0);
                return;
            }
            ptrdiff_t *refcount = refcount_ptr();
            *refcount += 1;
        }

        void rc_decrement()
        {
            if (is_small_str())
                return;
            if (big.bytes == NULL)
            {
                const Str msg("Attempted to decrement refcount of freed allocation");
                roc_panic(&msg, 0);
                return;
            }
            ptrdiff_t *refcount = refcount_ptr();
            if (*refcount == REFCOUNT_ONE)
            {
                // We're the only owner of this allocation, so we can just free it.
                // The refcount is at the beginning of the allocation.
                roc_dealloc(refcount, alignof(ptrdiff_t));
                big.bytes = NULL;
                big.length = 0;
                big.capacity = 0;
            }
            else
            {
                // There are other references to this allocation, so we need to reduce the refcount.
                *refcount -= 1;
            }
        }

        bool operator==(const Str &other) const
        {
            if (length() != other.length())
                return false;
            // Note: we do not require both to be big or small, and we ignore capacity.
            return memcmp(contents(), other.contents(), length()) == 0;
        }
    };
};
