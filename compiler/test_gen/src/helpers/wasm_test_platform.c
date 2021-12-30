#include <stdio.h>

// Makes test runs take 50% longer, due to linking
#define ENABLE_PRINTF 0

// Globals for refcount testing
size_t **rc_pointers; // array of pointers to refcount values
size_t rc_pointers_len;
size_t rc_pointers_index;

// The rust test passes us the max number of allocations it expects to make,
// and we tell it where we're going to write the refcount pointers.
// It won't actually read that memory until later, when the test is done.
size_t **init_refcount_test(size_t max_allocs)
{
    rc_pointers = malloc(max_allocs * sizeof(size_t *));
    rc_pointers_len = max_allocs;
    rc_pointers_index = 0;
    for (size_t i = 0; i < max_allocs; ++i)
        rc_pointers[i] = NULL;

    return rc_pointers;
}

#if ENABLE_PRINTF
#define ASSERT(x)                   \
    if (!(x))                       \
    {                               \
        printf("FAILED: " #x "\n"); \
        abort();                    \
    }
#else
#define ASSERT(x) \
    if (!(x))     \
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
        ASSERT(alignment >= sizeof(size_t));
        ASSERT(rc_pointers_index < rc_pointers_len);

        size_t *rc_ptr = alloc_ptr_to_rc_ptr(allocated, alignment);
        rc_pointers[rc_pointers_index] = rc_ptr;
        rc_pointers_index++;
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
        for (; i < rc_pointers_index; ++i)
        {
            if (rc_pointers[i] == rc_ptr)
            {
                rc_pointers[i] = NULL;
                break;
            }
        }
        int was_found = i < rc_pointers_index;
        ASSERT(was_found);
    }

#if ENABLE_PRINTF
    printf("roc_dealloc deallocated %p with alignment %zd\n", ptr, alignment);
#endif
    free(ptr);
}

//--------------------------

void roc_panic(void *ptr, unsigned int alignment)
{
#if ENABLE_PRINTF
    char *msg = (char *)ptr;
    fprintf(stderr,
            "Application crashed with message\n\n    %s\n\nShutting down\n", msg);
#endif
    abort();
}

//--------------------------

void *roc_memcpy(void *dest, const void *src, size_t n)
{
    return memcpy(dest, src, n);
}

//--------------------------

void *roc_memset(void *str, int c, size_t n)
{
    return memset(str, c, n);
}
