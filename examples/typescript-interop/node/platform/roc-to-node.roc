app "roc-to-node"
    packages { pf: "../../../../crates/glue/platform/main.roc" }
    imports [pf.Types.{ Types }, pf.File.{ File }, pf.Target.{ Architecture }, pf.Shape.{ Shape } ]
    provides [makeGlue] to pf

makeGlue : List Types -> Result (List File) Str
makeGlue = \typesByArch ->
    typesByArch
    |> List.joinMap \types ->
        # We need to generate separate .h and .d.ts files because of Nat.
        # The TS type for Nat on 64-bit targets is `BigInt`, but on 32-bit systems it's `number`.
        arch = (Types.target types).architecture

        [
            { name: cFileName arch, content: cFileContent types },
            { name: "TODO", content: tsTypeDefs types },
            { name: tsFileName arch, content: "export function hello(arg: string): string;" },
        ]
    |> List.concat [
        { name: "demo.c", content: cFile },
        { name: "roc_std.h", content: rocStdH },
        { name: "roc_napi.h", content: rocNapiH },
    ]
    |> Ok

tsTypeDefs : Types -> Str
tsTypeDefs = \types ->
    Types.walkShapes types "" \buf, shape, _ ->
        when shape is
            Function { functionName } ->
                fnType = tsType shape
                Str.concat buf "export function \(functionName)\(fnType);"

            RocSet "string" -> "TODO"

            RocBocks _ -> "TODO"

            _ ->
                Str.concat buf "TODO"
                # Str.concat buf "export const \(functionName)\(fnType);"

tsType : Shape -> Str
tsType = \shape ->
    when shape is
        RocStr -> "string"
        Bool -> "boolean"
        Num _rocNum -> "TODO"
        RocResult _okId _errId -> "TODO"
        RocList _elemId -> "TODO"
        RocDict _keyId _valId -> "TODO"
        RocSet _elemId -> "TODO"
        RocBox _elemId -> "TODO"
        TagUnion _rocTagUnion -> "TODO"
        EmptyTagUnion -> "TODO"
        Struct _ -> "TODO"
        TagUnionPayload _ -> "TODO"
        RecursivePointer _ -> "TODO"
        Function _ -> "TODO"
        Unit -> "{}"
        Unsized -> "never"


cFileName : Architecture -> Str
cFileName = \arch ->
    when arch is
        Aarch32 -> "arm.h"
        Aarch64 -> "aarch64.h"
        Wasm32 -> "wasm32.h"
        X86x32 -> "x86_32.h"
        X86x64 -> "x86_64.h"

tsFileName : Architecture -> Str
tsFileName = \arch ->
    when arch is
        Aarch32 -> "arm.d.ts"
        Aarch64 -> "arm64.d.ts"
        Wasm32 -> "wasm32.d.ts" # TODO: NodeJS's os.arch() never returns "wasm32", so this won't work!
        X86x32 -> "ia32.d.ts"
        X86x64 -> "x64.d.ts"

cFileContent : Types -> Str
cFileContent = \_types ->
    """
    #include <node_api.h>
    #include "roc_std.h"
    #include "roc_napi.h"

    extern void roc__mainForHost_1_exposed_generic(struct RocStr *ret, struct RocStr *arg);

    // Receive a string value from Node and pass it to Roc as a RocStr, then get a RocStr
    // back from Roc and convert it into a Node string.
    napi_value call_roc(napi_env env, napi_callback_info info)
    {
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
            // TODO throw an exception here instead
            return NULL;
        }

        napi_value node_arg = argv[0];

        struct RocStr roc_arg;

        status = node_string_into_roc_str(env, node_arg, &roc_arg);

        if (status != napi_ok)
        {
            // TODO throw an exception here instead
            return NULL;
        }

        struct RocStr roc_ret;
        // Call the Roc function to populate `roc_ret`'s bytes.
        roc__mainForHost_1_exposed_generic(&roc_ret, &roc_arg);

        // Consume the RocStr to create the Node string.
        return roc_str_into_node_string(env, roc_ret);
    }

    napi_value init(napi_env env, napi_value exports)
    {
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
    """

cFile : Str
cFile =
    """
    // Continue with the appropriate architecture-specific glue.
    #if defined(__x86_64__)
    #include "x86_64.h"
    #elif defined(__i386__)
    #include "x86_32.h"
    #elif defined(__arm__)
    #include "aarch32.h"
    #elif defined(__aarch64__)
    #include "aarch64.h"
    #elif defined(__wasm__)
    #include "wasm32.h"
    #endif
    """

rocNapiH : Str
rocNapiH =
    """
    #ifndef ROC_NAPI_H
    #define ROC_NAPI_H

    #include <stdbool.h>
    #include <stdlib.h>
    #include <string.h>
    #include <unistd.h>
    #include <math.h>
    #include <node_api.h>
    #include "roc_std.h"

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

    // Numbers

    // TODO convert Roc 128-bit integers to Node BigInts using
    // https://nodejs.org/api/n-api.html#napi_create_bigint_words

    napi_status roc_i64_to_bigint(napi_env env, int64_t roc_num, napi_value* out) {
        return napi_create_bigint_int64(env, roc_num, out);
    }

    napi_status roc_u64_to_bigint(napi_env env, uint64_t roc_num, napi_value* out) {
        return napi_create_bigint_uint64(env, roc_num, out);
    }

    napi_status bigint_to_roc_i64(napi_env env, napi_value bigint, int64_t* out, bool* lossless) {
        return napi_get_value_bigint_int64(env, bigint, out, lossless);
    }

    napi_status bigint_to_roc_u64(napi_env env, napi_value bigint, uint64_t* out, bool* lossless) {
        return napi_get_value_bigint_uint64(env, bigint, out, lossless);
    }

    napi_status roc_i32_to_number(napi_env env, int32_t roc_num, napi_value* out) {
        return napi_create_int32(env, roc_num, out);
    }

    napi_status roc_u32_to_number(napi_env env, uint32_t roc_num, napi_value* out) {
        return napi_create_uint32(env, roc_num, out);
    }

    // Node's functions to convert to raw integers silently swallow errors (by just setting
    // the answer to 0) such as the number having a fractional component or being non-finite.
    // So we do our own conversion, and throw a JS RangeError if the conversion fails.
    napi_status number_to_roc_u32(napi_env env, napi_value number, uint32_t* out) {
        napi_status status;
        double num_double;

        // Turn the Node number into a double.
        status = napi_get_value_double(env, number, &num_double);

        // This can fail if the value we got wasn't a Node number.
        if (status != napi_ok) {
            return status;
        }

        // Convert the double to an u32, and check if the conversion was lossy.
        uint32_t rounded = (uint32_t)num_double;

        if (num_double != (double)rounded) {
            // There were decimal values lost when rounding, or else the double was non-finite.
            return napi_throw_range_error(env, NULL, "Conversion from Node number to plain integer was lossy.");
        }

        *out = rounded;

        return napi_ok;
    }

    // Node's functions to convert to raw integers silently swallow errors (by just setting
    // the answer to 0) such as the number having a fractional component or being non-finite.
    // So we do our own conversion, and throw a JS RangeError if the conversion fails.
    napi_status number_to_roc_i32(napi_env env, napi_value number, int32_t* out) {
        napi_status status;
        double num_double;

        // Turn the Node number into a double.
        status = napi_get_value_double(env, number, &num_double);

        // This can fail if the value we got wasn't a Node number.
        if (status != napi_ok) {
            return status;
        }

        // Convert the double to an i32, and check if the conversion was lossy.
        int32_t rounded = (int32_t)num_double;

        if (num_double != (double)rounded) {
            // There were decimal values lost when rounding, or else the double was non-finite.
            return napi_throw_range_error(env, NULL, "Conversion from Node number to plain integer was lossy.");
        }

        *out = rounded;

        return napi_ok;
    }

    napi_status roc_i16_to_number(napi_env env, int16_t roc_num, napi_value* out) {
        return napi_create_int32(env, (int32_t)roc_num, out);
    }

    napi_status roc_u16_to_number(napi_env env, uint16_t roc_num, napi_value* out) {
        return napi_create_uint32(env, (uint32_t)roc_num, out);
    }

    napi_status roc_i8_to_number(napi_env env, int8_t roc_num, napi_value* out) {
        return napi_create_int32(env, (int32_t)roc_num, out);
    }

    napi_status roc_u8_to_number(napi_env env, uint8_t roc_num, napi_value* out) {
        return napi_create_uint32(env, (uint32_t)roc_num, out);
    }

    // Turn the given Node string into a RocStr and return it
    napi_status node_string_into_roc_str(napi_env env, napi_value node_string, struct RocStr *roc_str)
    {
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
            status = napi_get_value_string_utf8(env, node_string, (char *)roc_str, sizeof(struct RocStr), &len);

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
            uint8_t *buf = (uint8_t *)roc_alloc(capacity, __alignof__(char));

            // This writes the actual number of bytes copied into len. Theoretically they should be the same,
            // but it could be different if the buffer was somehow smaller. This way we guarantee that
            // the RocStr does not present any memory garbage to the user.
            status = napi_get_value_string_utf8(env, node_string, (char *)buf, capacity, &len);

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

        // First, decrement the refcount of the RocStr because we're going to consume it.
        // (Do this first in case there are errors later on; the Str will still have been consumed.)
        if (!is_small)
        {
            decref_large_str(roc_str);
        }

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

    #endif // ROC_NAPI_H
    """

rocStdH : Str
rocStdH =
    """
    #ifndef ROC_STD_H
    #define ROC_STD_H

    #include <stdlib.h>
    #include <stddef.h>
    #include <string.h>
    #include <unistd.h>

    const size_t REFCOUNT_ALIGN = __alignof__(size_t);

    // Reference counting operations need to know these types.
    void *roc_alloc(size_t size, unsigned int alignment);
    void roc_dealloc(void *ptr, unsigned int alignment);

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
    void incref(uint8_t *bytes, uint32_t alignment)
    {
        ssize_t *refcount_ptr = ((ssize_t *)bytes) - 1;
        ssize_t refcount = *refcount_ptr;

        if (refcount != REFCOUNT_READONLY)
        {
            *refcount_ptr = refcount + 1;
        }
    }

    // Decrement reference count, given a pointer to the first byte of a collection's elements.
    // Then call roc_dealloc if nothing is referencing this collection anymore.
    void decref_heap_bytes(uint8_t *bytes, uint32_t alignment)
    {
        size_t extra_bytes = (sizeof(size_t) >= (size_t)alignment) ? sizeof(size_t) : (size_t)alignment;
        ssize_t *refcount_ptr = ((ssize_t *)bytes) - 1;
        ssize_t refcount = *refcount_ptr;

        if (refcount != REFCOUNT_READONLY)
        {
            *refcount_ptr = refcount - 1;

            if (refcount == REFCOUNT_ONE)
            {
                void *original_allocation = (void *)(refcount_ptr - (extra_bytes - sizeof(size_t)));

                roc_dealloc(original_allocation, alignment);
            }
        }
    }

    // RocList

    struct RocList
    {
        void *elems;
        size_t len;
        size_t capacity;
    };

    struct RocList empty_list()
    {
        struct RocList ret = {
            .len = 0,
            .elems = NULL,
            .capacity = 0,
        };

        return ret;
    }

    // The refcount might take up more bytes if we need alignment padding due to
    // the elements having a larger alignment than the refcount.
    size_t list_refcount_size(size_t elem_align) {
        // C99 does not have a max() function, so we do this manually.
        return (REFCOUNT_ALIGN > elem_align) ? REFCOUNT_ALIGN : elem_align;
    }

    void *list_get(struct RocList list, size_t index, size_t elem_size, size_t elem_align)
    {
        return list.elems + list_refcount_size(elem_align) + (index * elem_size);
    }

    struct RocList init_list(void *elems, size_t elem_size, size_t elem_align, size_t len)
    {
        if (len == 0)
        {
            return empty_list();
        }
        else
        {
            size_t refcount_size = list_refcount_size(elem_align);

            // Allocate enough space for the refcount plus the elements
            size_t *new_refcount = (size_t *)roc_alloc(len + refcount_size, __alignof__(size_t));
            void *new_elems = new_refcount + refcount_size;

            ((ssize_t *)new_refcount)[0] = REFCOUNT_ONE;

            memcpy(new_elems, elems, len);

            struct RocList ret;

            ret.elems = new_elems;
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
        // A large RocStr is the same as a List U8  in memory.
        struct RocList list = init_list(bytes, sizeof(uint8_t), __alignof__(uint8_t), len);

        struct RocStr ret = {
            .len = list.len,
            .bytes = list.elems,
            .capacity = list.capacity,
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

    #endif // ROC_STD_H
    """
