#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

// Makes test runs take 50% longer, due to linking
#define ENABLE_PRINTF 0

typedef struct
{
    size_t length;
    size_t *elements[]; // flexible array member
} Vector;

// Globals for refcount testing
Vector *rc_pointers;
size_t rc_pointers_capacity;

// The rust test passes us the max number of allocations it expects to make,
// and we tell it where we're going to write the refcount pointers.
// It won't actually read that memory until later, when the test is done.
Vector *init_refcount_test(size_t capacity)
{
    rc_pointers_capacity = capacity;

    rc_pointers = malloc((1 + capacity) * sizeof(size_t *));
    rc_pointers->length = 0;
    for (size_t i = 0; i < capacity; ++i)
        rc_pointers->elements[i] = NULL;

    return rc_pointers;
}

#if ENABLE_PRINTF
#define ASSERT(condition, format, ...)                       \
    if (!(condition))                                        \
    {                                                        \
        printf("ASSERT FAILED: " #format "\n", __VA_ARGS__); \
        abort();                                             \
    }
#else
#define ASSERT(condition, format, ...) \
    if (!(condition))                  \
        abort();
#endif

size_t *alloc_ptr_to_rc_ptr(void *ptr, unsigned int alignment)
{
    size_t alloc_addr = (size_t)ptr;
    size_t rc_addr = alloc_addr + alignment - sizeof(size_t);
    return (size_t *)rc_addr;
}

//--------------------------

void *roc_alloc(size_t size, unsigned int alignment)
{
    void *allocated = malloc(size);

    if (rc_pointers)
    {
        ASSERT(alignment >= sizeof(size_t), "alignment %zd != %zd", alignment, sizeof(size_t));
        size_t num_alloc = rc_pointers->length + 1;
        ASSERT(num_alloc <= rc_pointers_capacity, "Too many allocations %zd > %zd", num_alloc, rc_pointers_capacity);

        size_t *rc_ptr = alloc_ptr_to_rc_ptr(allocated, alignment);
        rc_pointers->elements[rc_pointers->length] = rc_ptr;
        rc_pointers->length++;
    }

#if ENABLE_PRINTF
    if (!allocated)
    {
        fprintf(stderr, "roc_alloc failed\n");
        exit(1);
    }
    else
    {
        printf("roc_alloc allocated %d bytes with alignment %d at %p\n", size, alignment, allocated);
    }
#endif
    return allocated;
}

//--------------------------

void *roc_realloc(void *ptr, size_t new_size, size_t old_size,
                  unsigned int alignment)
{
#if ENABLE_PRINTF
    printf("roc_realloc reallocated %p from %d to %d with alignment %zd\n",
           ptr, old_size, new_size, alignment);
#endif
    return realloc(ptr, new_size);
}

//--------------------------

void roc_dealloc(void *ptr, unsigned int alignment)
{
    if (rc_pointers)
    {
        // Null out the entry in the test array to indicate that it was freed
        // Then even if malloc reuses the space, everything still works
        size_t *rc_ptr = alloc_ptr_to_rc_ptr(ptr, alignment);
        int i = 0;
        for (; i < rc_pointers->length; ++i)
        {
            if (rc_pointers->elements[i] == rc_ptr)
            {
                rc_pointers->elements[i] = NULL;
                break;
            }
        }
        int was_found = i < rc_pointers->length;
        ASSERT(was_found, "RC pointer not found %p", rc_ptr);
    }

#if ENABLE_PRINTF
    printf("roc_dealloc deallocated %p with alignment %zd\n", ptr, alignment);
#endif
    free(ptr);
}

//--------------------------

extern void send_panic_msg_to_rust(void* msg, uint32_t tag_id);

void roc_panic(void* msg, unsigned int tag_id)
{
    send_panic_msg_to_rust(msg, tag_id);
    exit(101);
}

//--------------------------

void roc_memcpy(void *dest, const void *src, size_t n)
{
    memcpy(dest, src, n);
}

//--------------------------

void *roc_memset(void *str, int c, size_t n)
{
    return memset(str, c, n);
}
