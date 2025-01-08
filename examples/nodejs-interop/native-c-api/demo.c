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

void roc_dbg(char* loc, char* msg, char* src) {
  fprintf(stderr, "[%s] %s = %s\n", loc, src, msg);
}

void *roc_memset(void *str, int c, size_t n) { return memset(str, c, n); }

// Reference counting

// If the refcount is set to this, that means the allocation is
// stored in readonly memory in the binary, and we must not
// attempt to increment or decrement it; if we do, we'll segfault!
const ssize_t REFCOUNT_READONLY = 0;
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

// Decrement reference count, given a pointer to the first byte of a collection's elements.
// Then call roc_dealloc if nothing is referencing this collection anymore.
void decref_heap_bytes(uint8_t* bytes, uint32_t alignment)
{
    size_t extra_bytes = (sizeof(size_t) >= (size_t)alignment) ? sizeof(size_t) : (size_t)alignment;
    ssize_t *refcount_ptr = ((ssize_t *)bytes) - 1;
    ssize_t refcount = *refcount_ptr;

    if (refcount != REFCOUNT_READONLY) {
        *refcount_ptr = refcount - 1;

        if (refcount == 1) {
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

struct RocBytes empty_rocbytes()
{
    struct RocBytes ret = {
        .len = 0,
        .bytes = NULL,
        .capacity = 0,
    };

    return ret;
}

struct RocBytes init_rocbytes(uint8_t *bytes, size_t len)
{
    if (len == 0)
    {
        return empty_rocbytes();
    }
    else
    {
        struct RocBytes ret;
        size_t refcount_size = sizeof(size_t);
        uint8_t *new_refcount = (uint8_t *)roc_alloc(len + refcount_size, __alignof__(size_t));
        uint8_t *new_content = new_refcount + refcount_size;

        ((ssize_t *)new_refcount)[0] = 1;

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

struct RocStr empty_roc_str()
{
    struct RocStr ret = {
        .len = 0,
        .bytes = NULL,
        .capacity = MASK,
    };

    return ret;
}

// Record the small string's length in the last byte of the given stack allocation
void write_small_str_len(size_t len, struct RocStr *str) {
    ((uint8_t *)str)[sizeof(struct RocStr) - 1] = (uint8_t)len | 0b10000000;
}

struct RocStr roc_str_init_small(uint8_t *bytes, size_t len)
{
    // Start out with zeroed memory, so that
    // if we end up comparing two small RocStr values
    // for equality, we won't risk memory garbage resulting
    // in two equal strings appearing unequal.
    struct RocStr ret = empty_roc_str();

    // Copy the bytes into the stack allocation
    memcpy(&ret, bytes, len);

    write_small_str_len(len, &ret);

    return ret;
}

struct RocStr roc_str_init_large(uint8_t *bytes, size_t len, size_t capacity)
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

bool is_small_str(struct RocStr str) { return ((ssize_t)str.capacity) < 0; }

// Determine the length of the string, taking into
// account the small string optimization
size_t roc_str_len(struct RocStr str)
{
    uint8_t *bytes = (uint8_t *)&str;
    uint8_t last_byte = bytes[sizeof(str) - 1];
    uint8_t last_byte_xored = last_byte ^ 0b10000000;
    size_t small_len = (size_t)(last_byte_xored);
    size_t big_len = str.len & PTRDIFF_MAX; // Account for seamless slices

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

void decref_large_str(struct RocStr str)
{
    uint8_t* bytes;

    if ((ssize_t)str.len < 0)
    {
        // This is a seamless slice, so the bytes are located in the capacity slot.
        bytes = (uint8_t*)(str.capacity << 1);
    }
    else
    {
        bytes = str.bytes;
    }

    decref_heap_bytes(bytes, __alignof__(uint8_t));
}


// Turn the given Node string into a RocStr and return it
napi_status node_string_into_roc_str(napi_env env, napi_value node_string, struct RocStr *roc_str) {
    size_t len;
    napi_status status;

    // Passing NULL for a buffer (and size 0) will make it write the length of the string into `len`.
    // https://nodejs.org/api/n-api.html#napi_get_value_string_utf8
    status = napi_get_value_string_utf8(env, node_string, NULL, 0, &len);

    if (status != napi_ok)
    {
        return status;
    }

    // Node's "write a string into this buffer" function always writes a null terminator,
    // so capacity will need to be length + 1.
    // https://nodejs.org/api/n-api.html#napi_get_value_string_utf8
    size_t capacity = len + 1;

    // Create a RocStr and write it into the out param
    if (capacity < sizeof(struct RocStr))
    {
        // If it can fit in a small string, use the string itself as the buffer.
        // First, zero out those bytes; small strings need to have zeroes for any bytes
        // that are not part of the string, or else comparisons between small strings might fail.
        *roc_str = empty_roc_str();

        // This writes the actual number of bytes copied into len. Theoretically they should be the same,
        // but it could be different if the buffer was somehow smaller. This way we guarantee that
        // the RocStr does not present any memory garbage to the user.
        status = napi_get_value_string_utf8(env, node_string, (char*)roc_str, sizeof(struct RocStr), &len);

        if (status != napi_ok)
        {
            return status;
        }

        // We have to write the length into the buffer *after* Node copies its bytes in,
        // because Node will have written a null terminator, which we may need to overwrite.
        write_small_str_len(len, roc_str);
    }
    else
    {
        // capacity was too big for a small string, so make a heap allocation and write into that.
        uint8_t *buf = (uint8_t*)roc_alloc(capacity, __alignof__(char));

        // This writes the actual number of bytes copied into len. Theoretically they should be the same,
        // but it could be different if the buffer was somehow smaller. This way we guarantee that
        // the RocStr does not present any memory garbage to the user.
        status = napi_get_value_string_utf8(env, node_string, (char*)buf, capacity, &len);

        if (status != napi_ok)
        {
            // Something went wrong, so free the bytes we just allocated before returning.
            roc_dealloc((void *)&buf, __alignof__(char *));

            return status;
        }

        *roc_str = roc_str_init_large(buf, len, capacity);
    }

    return status;
}

// Consume the given RocStr (decrement its refcount) after creating a Node string from it.
napi_value roc_str_into_node_string(napi_env env, struct RocStr roc_str) {
    bool is_small = is_small_str(roc_str);
    char* roc_str_contents;

    if (is_small)
    {
        // In a small string, the string itself contains its contents.
        roc_str_contents = (char*)&roc_str;
    }
    else
    {
        roc_str_contents = (char*)roc_str.bytes;
    }

    napi_status status;
    napi_value answer;

    status = napi_create_string_utf8(env, roc_str_contents, roc_str_len(roc_str), &answer);

    if (status != napi_ok)
    {
        answer = NULL;
    }

    // Decrement the RocStr because we consumed it.
    if (!is_small)
    {
        decref_large_str(roc_str);
    }

    return answer;
}

// Create a Node string from the given RocStr.
// Don't decrement the RocStr's refcount. (To decrement it, use roc_str_into_node_string instead.)
napi_value roc_str_as_node_string(napi_env env, struct RocStr roc_str) {
    bool is_small = is_small_str(roc_str);
    char* roc_str_contents;

    if (is_small)
    {
        // In a small string, the string itself contains its contents.
        roc_str_contents = (char*)&roc_str;
    }
    else
    {
        roc_str_contents = (char*)roc_str.bytes;
    }

    napi_status status;
    napi_value answer;

    status = napi_create_string_utf8(env, roc_str_contents, roc_str_len(roc_str), &answer);

    if (status != napi_ok)
    {
        return NULL;
    }

    // Do not decrement the RocStr's refcount because we did not consume it.

    return answer;
}

extern void roc__main_for_host_1_exposed_generic(struct RocStr *ret, struct RocStr *arg);

// Receive a string value from Node and pass it to Roc as a RocStr, then get a RocStr
// back from Roc and convert it into a Node string.
napi_value call_roc(napi_env env, napi_callback_info info) {
    napi_status status;

    // roc_panic needs a napi_env in order to throw a Node exception, so we provide this
    // one globally in case roc_panic gets called during the execution of our Roc function.
    //
    // According to the docs - https://nodejs.org/api/n-api.html#napi_env -
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

    status = node_string_into_roc_str(env, node_arg, &roc_arg);

    if (status != napi_ok)
    {
        return NULL;
    }

    struct RocStr roc_ret;
    // Call the Roc function to populate `roc_ret`'s bytes.
    roc__main_for_host_1_exposed_generic(&roc_ret, &roc_arg);

    // Consume the RocStr to create the Node string.
    return roc_str_into_node_string(env, roc_ret);
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
