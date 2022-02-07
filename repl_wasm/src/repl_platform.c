#include <stdio.h>

/*
    A bare-bones Roc "platform" for REPL code, providing heap allocation for builtins.
*/

// Enable/disable printf debugging. Leave disabled to avoid bloating .wasm files and slowing down Wasmer tests.
#define ENABLE_PRINTF 0

//--------------------------

void *roc_alloc(size_t size, unsigned int alignment)
{
    void *allocated = malloc(size);

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
