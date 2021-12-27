#include <stdio.h>

// If any printf is included for compilation, even if unused, test runs take 50% longer
#define DEBUG 0

//--------------------------

void *roc_alloc(size_t size, unsigned int alignment)
{
    void *allocated = malloc(size);
#if DEBUG
    if (!allocated)
    {
        fprintf(stderr, "roc_alloc failed\n");
        exit(1);
    }
    else
    {
        printf("roc_alloc allocated %d bytes at %p\n", size, allocated);
    }
#endif
    return allocated;
}

//--------------------------

void *roc_realloc(void *ptr, size_t new_size, size_t old_size,
                  unsigned int alignment)
{
    return realloc(ptr, new_size);
}

//--------------------------

void roc_dealloc(void *ptr, unsigned int alignment)
{
    free(ptr);
}

//--------------------------

void roc_panic(void *ptr, unsigned int alignment)
{
#if DEBUG
    char *msg = (char *)ptr;
    fprintf(stderr,
            "Application crashed with message\n\n    %s\n\nShutting down\n", msg);
#endif
    exit(1);
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
