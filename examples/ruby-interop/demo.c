#include <errno.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
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
    char *msg = (char *)ptr;
    fprintf(stderr,
            "Application crashed with message\n\n    %s\n\nShutting down\n", msg);
    exit(0);
}

void *roc_memcpy(void *dest, const void *src, size_t n)
{
    return memcpy(dest, src, n);
}

void *roc_memset(void *str, int c, size_t n) { return memset(str, c, n); }

struct RocStr
{
    char *bytes;
    size_t len;
    size_t capacity;
};

struct RocStr init_rocstr(char *bytes, size_t len)
{
    struct RocStr ret;

    if (len < sizeof(struct RocStr))
    {
        // This is a small string. TODO do small string things.
        size_t refcount_size = sizeof(size_t);
        char *new_content = (char *)roc_alloc(len + refcount_size, alignof(size_t)) - refcount_size;

        roc_memcpy(new_content, bytes, len);

        ret.bytes = new_content;
        ret.len = len;
        ret.capacity = len;
    }
    else
    {
        size_t refcount_size = sizeof(size_t);
        char *new_content = (char *)roc_alloc(len + refcount_size, alignof(size_t)) - refcount_size;

        roc_memcpy(new_content, bytes, len);

        ret.bytes = new_content;
        ret.len = len;
        ret.capacity = len;
    }

    return ret;
}

bool is_small_str(struct RocStr str) { return ((ssize_t)str.capacity) < 0; }

// Determine the length of the string, taking into
// account the small string optimization
size_t roc_str_len(struct RocStr str)
{
    char *bytes = (char *)&str;
    char last_byte = bytes[sizeof(str) - 1];
    char last_byte_xored = last_byte ^ 0b10000000;
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

    struct RocStr arg = init_rocstr(RSTRING_PTR(rb_arg), RSTRING_LEN(rb_arg));
    struct RocStr ret;

    roc__mainForHost_1_exposed_generic(&ret, &arg);

    // Determine str_len and the str_bytes pointer,
    // taking into account the small string optimization.
    size_t str_len = roc_str_len(ret);
    VALUE ruby_str;

    if (is_small_str(ret))
    {
        ruby_str = rb_utf8_str_new((char *)&ret, str_len);
    }
    else
    {
        ruby_str = rb_utf8_str_new(ret.bytes, str_len);

        // TODO decrement refcount and then only dealloc if that was the last reference
        roc_dealloc(ret.bytes - sizeof(size_t), alignof(char *));
    }

    return ruby_str;
}

void Init_demo()
{
    printf("Ruby just required Roc. Let's get READY TO ROC.\n");

    VALUE roc_stuff = rb_define_module("RocStuff");
    rb_define_module_function(roc_stuff, "hello", &hello, 1);
}
