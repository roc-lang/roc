#include <errno.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>
#include <string.h>
#include <unistd.h>
#include <node_api.h>

napi_env napi_global_env;

void *roc_alloc(size_t size, unsigned int alignment) { return malloc(size); }

void *roc_realloc(void *ptr, size_t new_size, size_t old_size,
                  unsigned int alignment)
{
    return realloc(ptr, new_size);
}

void roc_dealloc(void *ptr, unsigned int alignment) { free(ptr); }

void roc_panic(void *ptr, unsigned int alignment)
{
    // WARNING: If roc_panic is called before napi_global_env is set,
    // the result will be undefined behavior. So never call any Roc
    // functions before setting napi_global_env!
    napi_throw_error(napi_global_env, NULL, (char *)ptr);
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

// RocBytes (List U8)

struct RocBytes
{
    uint8_t *bytes;
    size_t len;
    size_t capacity;
};

struct RocBytes init_rocbytes(uint8_t *bytes, size_t len)
{
    if (len == 0)
    {
        struct RocBytes ret = {
            .len = 0,
            .bytes = NULL,
            .capacity = MASK,
        };

        return ret;
    }
    else
    {
        struct RocBytes ret;
        size_t refcount_size = sizeof(size_t);
        uint8_t *new_content = (uint8_t *)roc_alloc(len + refcount_size, __alignof__(size_t)) + refcount_size;

        memcpy(new_content, bytes, len);

        ret.bytes = new_content;
        ret.len = len;
        ret.capacity = len;

        return ret;
    }
}

// RocStr

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
        // A large RocStr is the same as a List U8 (aka RocBytes) in memory.
        struct RocBytes roc_bytes = init_rocbytes(bytes, len);

        struct RocStr ret = {
            .len = roc_bytes.len,
            .bytes = roc_bytes.bytes,
            .capacity = roc_bytes.capacity,
        };

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

// Receive a string value from Node and pass it to Roc as a RocStr, then get a RocStr
// back from Roc and convert it into a Node string.
napi_value call_roc(napi_env env, napi_callback_info info) {
    napi_status status;

    // roc_panic needs a napi_env in order to throw a Node exception, so we provide this
    // one globally in case roc_panic gets called during the execution of our Roc function.
    //
    // According do the docs - https://nodejs.org/api/n-api.html#napi_env -
    // it's very important that the napi_env that was passed into "the initial
    // native function" is the one that's "passed to any subsequent nested Node-API calls,"
    // so we must override this every time we call this function (as opposed to, say,
    // setting it once during init).
    napi_global_env = env;

    // Get the argument passed to the Node function
    size_t argc = 1;
    napi_value argv[1];

    status = napi_get_cb_info(env, info, &argc, argv, NULL, NULL);

    if (status != napi_ok)
    {
        return NULL;
    }

    napi_value node_arg = argv[0];

    struct RocStr roc_arg;

    // Write the length of the Node string into `roc_arg_len`
    size_t roc_arg_len;

    status = napi_get_value_string_utf8(env, node_arg, NULL, 0, &roc_arg_len);

    if (status != napi_ok)
    {
        return NULL;
    }

    // Node always writes a null terminator, so we need to keep that in mind.
    size_t nul_terminated_str_bytes = roc_arg_len + 1;

    // Create a RocStr from the Node string
    size_t roc_arg_strlen;
    char* roc_arg_bytes;

    if (nul_terminated_str_bytes < sizeof(struct RocStr))
    {
        // If it can fit in a small string, use the string itself as the buffer.
        roc_arg_bytes = (char*)&roc_arg;
    }
    else
    {
        // It was too big for a small string, so do a heap allocation and write into that.
        roc_arg_bytes = (char*)roc_alloc(nul_terminated_str_bytes, __alignof__(char));
    }

    status = napi_get_value_string_utf8(env, node_arg, roc_arg_bytes, nul_terminated_str_bytes, &roc_arg_strlen);

    if (status != napi_ok)
    {
        return NULL;
    }

    struct RocStr roc_str_arg = init_rocstr((uint8_t*)roc_arg_bytes, roc_arg_strlen);

    // Call the Roc function to populate `roc_ret`'s bytes.
    struct RocStr roc_ret;

    roc__mainForHost_1_exposed_generic(&roc_ret, &roc_str_arg);

    // Create a Node string from the Roc string and return it.
    char* roc_str_contents;

    if (is_small_str(roc_ret))
    {
        // In a small string, the string itself contains its contents.
        roc_str_contents = (char*)&roc_ret;
    }
    else
    {
        roc_str_contents = (char*)roc_ret.bytes;
    }

    napi_value node_ret;

    status = napi_create_string_utf8(env, roc_str_contents, roc_str_len(roc_ret), &node_ret);

    if (status != napi_ok)
    {
        return NULL;
    }

    return node_ret;
}

napi_value init(napi_env env, napi_value exports) {
    napi_status status;
    napi_value fn;

    status = napi_create_function(env, NULL, 0, call_roc, NULL, &fn);

    if (status != napi_ok)
    {
        return NULL;
    }

    status = napi_set_named_property(env, exports, "hello", fn);

    if (status != napi_ok)
    {
        return NULL;
    }

    return exports;
}

NAPI_MODULE(NODE_GYP_MODULE_NAME, init)