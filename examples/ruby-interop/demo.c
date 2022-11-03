#include <errno.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>
#include <string.h>
#include <unistd.h>
#include <ruby.h>
#include "extconf.h"

void *roc_alloc(size_t size, unsigned int alignment) { return malloc(size); }

void *roc_realloc(void *ptr, size_t new_size, size_t old_size,
                  unsigned int alignment)
{
    return realloc(ptr, new_size);
}

void roc_dealloc(void *ptr, unsigned int alignment) { free(ptr); }

__attribute__((noreturn)) void roc_panic(void *ptr, unsigned int alignment)
{
    uint8_t *msg = (uint8_t *)ptr;
    fprintf(stderr,
            "Application crashed with message\n\n    %s\n\nShutting down\n", msg);
    exit(0);
}

void *roc_memcpy(void *dest, const void *src, size_t n)
{
    return memcpy(dest, src, n);
}

void *roc_memset(void *str, int c, size_t n) { return memset(str, c, n); }

// Reference counting

// If the refcount is set to this, that means the allocation is
// stored in readonly memory in the binary, and we must not
// attempt to increment or decrement it; if we do, we'll segfault!
const ssize_t REFCOUNT_READONLY = 0;
const ssize_t REFCOUNT_ONE = (ssize_t)PTRDIFF_MIN;
const size_t MASK = (size_t)PTRDIFF_MIN;

// Increment reference count, given a pointer to the first element in a collection.
// We don't need to check for overflow because in order to overflow a usize worth of refcounts,
// you'd need to somehow have more pointers in memory than the OS's virtual address space can hold.
void incref(uint8_t* bytes, uint32_t alignment)
{
    ssize_t *refcount_ptr = ((ssize_t *)bytes) - 1;
    ssize_t refcount = *refcount_ptr;

    if (refcount != REFCOUNT_READONLY) {
        *refcount_ptr = refcount + 1;
    }
}

// Decrement reference count, given a pointer to the first element in a collection.
// Then call roc_dealloc if nothing is referencing this collection anymore.
void decref(uint8_t* bytes, uint32_t alignment)
{
    if (bytes == NULL) {
        return;
    }

    size_t extra_bytes = (sizeof(size_t) >= (size_t)alignment) ? sizeof(size_t) : (size_t)alignment;
    ssize_t *refcount_ptr = ((ssize_t *)bytes) - 1;
    ssize_t refcount = *refcount_ptr;

    if (refcount != REFCOUNT_READONLY) {
        *refcount_ptr = refcount - 1;

        if (refcount == REFCOUNT_ONE) {
            void *original_allocation = (void *)(refcount_ptr - (extra_bytes - sizeof(size_t)));

            roc_dealloc(original_allocation, alignment);
        }
    }
}

struct RocStr
{
    uint8_t *bytes;
    size_t len;
    size_t capacity;
};

struct RocStr init_rocstr(uint8_t *bytes, size_t len)
{
    if (len == 0)
    {
        struct RocStr ret = {
            .len = 0,
            .bytes = NULL,
            .capacity = MASK,
        };

        return ret;
    }
    else if (len < sizeof(struct RocStr))
    {
        // Start out with zeroed memory, so that
        // if we end up comparing two small RocStr values
        // for equality, we won't risk memory garbage resulting
        // in two equal strings appearing unequal.
        struct RocStr ret = {
            .len = 0,
            .bytes = NULL,
            .capacity = MASK,
        };

        // Copy the bytes into the stack allocation
        memcpy(&ret, bytes, len);

        // Record the string's length in the last byte of the stack allocation
        ((uint8_t *)&ret)[sizeof(struct RocStr) - 1] = (uint8_t)len | 0b10000000;

        return ret;
    }
    else
    {
        struct RocStr ret;
        size_t refcount_size = sizeof(size_t);
        uint8_t *new_content = (uint8_t *)roc_alloc(len + refcount_size, alignof(size_t)) + refcount_size;

        memcpy(new_content, bytes, len);

        ret.bytes = new_content;
        ret.len = len;
        ret.capacity = len;

        return ret;
    }
}

bool is_small_str(struct RocStr str) { return ((ssize_t)str.capacity) < 0; }

// Determine the length of the string, taking into
// account the small string optimization
size_t roc_str_len(struct RocStr str)
{
    uint8_t *bytes = (uint8_t *)&str;
    uint8_t last_byte = bytes[sizeof(str) - 1];
    uint8_t last_byte_xored = last_byte ^ 0b10000000;
    size_t small_len = (size_t)(last_byte_xored);
    size_t big_len = str.len;

    // Avoid branch misprediction costs by always
    // determining both small_len and big_len,
    // so this compiles to a cmov instruction.
    if (is_small_str(str))
    {
        return small_len;
    }
    else
    {
        return big_len;
    }
}

extern void roc__mainForHost_1_exposed_generic(struct RocStr *ret, struct RocStr *arg);

VALUE hello(VALUE self, VALUE rb_arg)
{
    // Verify the argument is a Ruby string; raise a type error if not.
    if (TYPE(rb_arg) != T_STRING)
    {
        rb_raise(rb_eTypeError, "`hello` only accepts strings.");
    }

    struct RocStr arg = init_rocstr((uint8_t *)RSTRING_PTR(rb_arg), RSTRING_LEN(rb_arg));
    struct RocStr ret;

    // Call the Roc function to populate `ret`'s bytes.
    roc__mainForHost_1_exposed_generic(&ret, &arg);

    // Determine str_len and the str_bytes pointer,
    // taking into account the small string optimization.
    size_t str_len = roc_str_len(ret);

    if (is_small_str(ret))
    {
        // Create a rb_utf8_str from the (small) RocStr's stack-allocated bytes
        return rb_utf8_str_new((char *)&ret, str_len);
    }
    else
    {
        // Create a rb_utf8_str from the (large) RocStr's heap-allocated bytes
        VALUE ruby_str = rb_utf8_str_new((char *)ret.bytes, str_len);

        // Now that we've created our Ruby string, we're no longer referencing the RocStr.
        decref((void *)&ret, alignof(uint8_t *));

        return ruby_str;
    }
}

void Init_demo()
{
    printf("Ruby just required Roc. Let's get READY TO ROC.\n");

    VALUE roc_stuff = rb_define_module("RocStuff");
    rb_define_module_function(roc_stuff, "hello", &hello, 1);
}
