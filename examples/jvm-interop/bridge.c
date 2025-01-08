#include <errno.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>
#include <string.h>
#include <unistd.h>
#include <stdalign.h>
#include <stdint.h>
#include <setjmp.h>

#include <jni.h>
#include "javaSource_Demo.h"

JavaVM* vm;

#define ERR_MSG_MAX_SIZE 256

jmp_buf exception_buffer;
char* err_msg[ERR_MSG_MAX_SIZE] = {0};

jint JNI_OnLoad(JavaVM *loadedVM, void *reserved)
{
    // https://docs.oracle.com/javase/8/docs/technotes/guides/jni/spec/invocation.html
    vm = loadedVM;
    return JNI_VERSION_1_2;
}

void *roc_alloc(size_t size, unsigned int alignment)
{
    return malloc(size);
}

void *roc_realloc(void *ptr, size_t new_size, size_t old_size,
                  unsigned int alignment)
{
    return realloc(ptr, new_size);
}

void roc_dealloc(void *ptr, unsigned int alignment)
{
    free(ptr);
}

void *roc_memset(void *str, int c, size_t n)
{
    return memset(str, c, n);
}

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

struct RocListI32
{
    int32_t *bytes;
    size_t len;
    size_t capacity;
};

struct RocListI32 init_roclist_i32(int32_t *bytes, size_t len)
{
    if (len == 0)
    {
        struct RocListI32 ret = {
            .len = 0,
            .bytes = NULL,
            .capacity = 0,
        };

        return ret;
    }
    else
    {
        size_t refcount_size = sizeof(size_t);
        ssize_t* data = (ssize_t*)roc_alloc(len + refcount_size, alignof(size_t));
        data[0] = REFCOUNT_ONE;
        int32_t *new_content = (int32_t *)(data + 1);

        struct RocListI32 ret;

        memcpy(new_content, bytes, len * sizeof(int32_t));

        ret.bytes = new_content;
        ret.len = len;
        ret.capacity = len;

        return ret;
    }
}
// RocListU8 (List U8)

struct RocListU8
{
    uint8_t *bytes;
    size_t len;
    size_t capacity;
};

struct RocListU8 init_roclist_u8(uint8_t *bytes, size_t len)
{
    if (len == 0)
    {
        struct RocListU8 ret = {
            .len = 0,
            .bytes = NULL,
            .capacity = 0,
        };

        return ret;
    }
    else
    {

        size_t refcount_size = sizeof(size_t);
        ssize_t* data = (ssize_t*)roc_alloc(len + refcount_size, alignof(size_t));
        data[0] = REFCOUNT_ONE;
        uint8_t *new_content = (uint8_t *)(data + 1);

        struct RocListU8 ret;

        memcpy(new_content, bytes, len * sizeof(uint8_t));

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
    if (len < sizeof(struct RocStr))
    {
        // Start out with zeroed memory, so that
        // if we end up comparing two small RocStr values
        // for equality, we won't risk memory garbage resulting
        // in two equal strings appearing unequal.
        struct RocStr ret = {
            .len = 0,
            .bytes = NULL,
            .capacity = 0,
        };

        // Copy the bytes into the stack allocation
        memcpy(&ret, bytes, len);

        // Record the string's len in the last byte of the stack allocation
        ((uint8_t *)&ret)[sizeof(struct RocStr) - 1] = (uint8_t)len | 0b10000000;

        return ret;
    }
    else
    {
        // A large RocStr is the same as a List U8 (aka RocListU8) in memory.
        struct RocListU8 roc_bytes = init_roclist_u8(bytes, len);

        struct RocStr ret = {
            .len = roc_bytes.len,
            .bytes = roc_bytes.bytes,
            .capacity = roc_bytes.capacity,
        };

        return ret;
    }
}

bool is_small_str(struct RocStr str)
{
    return ((ssize_t)str.capacity) < 0;
}

bool is_seamless_str_slice(struct RocStr str)
{
    return ((ssize_t)str.len) < 0;
}

bool is_seamless_listi32_slice(struct RocListI32 list)
{
    return ((ssize_t)list.capacity) < 0;
}

// Determine the len of the string, taking into
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

__attribute__((noreturn)) void roc_panic(struct RocStr *msg, unsigned int tag_id)
{
    char* bytes = is_small_str(*msg) ? (char*)msg : (char*)msg->bytes;
    const size_t str_len = roc_str_len(*msg);

    int len = str_len > ERR_MSG_MAX_SIZE ? ERR_MSG_MAX_SIZE : str_len;
    strncpy((char*)err_msg, bytes, len);

    // Free the underlying allocation if needed.
    if (!is_small_str(*msg)) {
        if (is_seamless_str_slice(*msg)){
            decref((uint8_t *)(msg->capacity << 1), alignof(uint8_t *));
        }
        else {
            decref(msg->bytes, alignof(uint8_t *));
        }
    }

    longjmp(exception_buffer, 1);
}

void roc_dbg(struct RocStr *loc, struct RocStr *msg, struct RocStr *src) {
    char* loc_bytes = is_small_str(*loc) ? (char*)loc : (char*)loc->bytes;
    char* src_bytes = is_small_str(*src) ? (char*)src : (char*)src->bytes;
    char* msg_bytes = is_small_str(*msg) ? (char*)msg : (char*)msg->bytes;
    fprintf(stderr, "[%s] %s = %s\n", loc_bytes, src_bytes, msg_bytes);
}

extern void roc__program_for_host_1__InterpolateString_caller(struct RocStr *name, char *closure_data, struct RocStr *ret);

extern void roc__program_for_host_1__MulArrByScalar_caller(struct RocListI32 *arr, int32_t *scalar, char *closure_data, struct RocListI32 *ret);

extern void roc__program_for_host_1__Factorial_caller(int64_t *scalar, char *closure_data, int64_t *ret);


JNIEXPORT jstring JNICALL Java_javaSource_Demo_sayHello
   (JNIEnv *env, jobject thisObj, jstring name)
{
    const char *jnameChars = (*env)->GetStringUTFChars(env, name, 0);
    // we copy just in case the jvm would try to reclaim that mem
    uint8_t *cnameChars = (uint8_t *)strdup(jnameChars);
    size_t nameLength = (size_t) (*env)->GetStringLength(env, name);
    (*env)->ReleaseStringUTFChars(env, name, jnameChars);


    struct RocStr rocName = init_rocstr(cnameChars, nameLength);
    struct RocStr ret = {0};

    // Call the Roc function to populate `ret`'s bytes.
    roc__program_for_host_1__InterpolateString_caller(&rocName, 0, &ret);
    jbyte *bytes = (jbyte*)(is_small_str(ret) ? (uint8_t*)&ret : ret.bytes);

    // java being java making this a lot harder than it needs to be
    // https://stackoverflow.com/questions/32205446/getting-true-utf-8-characters-in-java-jni
    // https://docs.oracle.com/javase/1.5.0/docs/guide/jni/spec/types.html#wp16542
    // but as i refuse converting those manually to their correct form, we just let the jvm handle the conversion
    // by first making a java byte array then converting the byte array to our final jstring
    jbyteArray byteArray = (*env)->NewByteArray(env, ret.len);
    (*env)->SetByteArrayRegion(env, byteArray, 0, ret.len, bytes);

    jstring charsetName = (*env)->NewStringUTF(env, "UTF-8");
    jclass stringClass = (*env)->FindClass(env, "java/lang/String");
    // https://docs.oracle.com/javase/7/docs/jdk/api/jpda/jdi/com/sun/jdi/doc-files/signature.html
    jmethodID stringConstructor = (*env)->GetMethodID(env, stringClass, "<init>", "([BLjava/lang/String;)V");
    jstring result = (*env)->NewObject(env, stringClass, stringConstructor, byteArray, charsetName);

    // cleanup
    if (!is_seamless_str_slice(ret)) {
      decref(ret.bytes, alignof(uint8_t *));
    }

    (*env)->DeleteLocalRef(env, charsetName);
    (*env)->DeleteLocalRef(env, byteArray);

    free(cnameChars);

    return result;
}


JNIEXPORT jintArray JNICALL Java_javaSource_Demo_mulArrByScalar
   (JNIEnv *env, jobject thisObj, jintArray arr, jint scalar)
{
    // extract data from jvm types
    jint* jarr = (*env)->GetIntArrayElements(env, arr, NULL);
    jsize len = (*env)->GetArrayLength(env, arr);

    // pass data to platform
    struct RocListI32 originalArray = init_roclist_i32(jarr, len);
    incref((void *)&originalArray, alignof(int32_t*));
    struct RocListI32 ret = {0};

    roc__program_for_host_1__MulArrByScalar_caller(&originalArray, &scalar, 0, &ret);

    // create jvm constructs
    jintArray multiplied = (*env)->NewIntArray(env, ret.len);
    (*env)->SetIntArrayRegion(env, multiplied, 0, ret.len, (jint*) ret.bytes);

    // cleanup
    (*env)->ReleaseIntArrayElements(env, arr, jarr, 0);

    if (is_seamless_listi32_slice(ret)) {
      decref((void *)(ret.capacity << 1), alignof(uint8_t *));
    }
    else {
      decref((void *)ret.bytes, alignof(uint8_t *));
    }

    return multiplied;
}

JNIEXPORT jlong JNICALL Java_javaSource_Demo_factorial
   (JNIEnv *env, jobject thisObj, jlong num)
{
    int64_t ret;
    // can crash - meaning call roc_panic, so we set a jump here
    if (setjmp(exception_buffer)) {
        // exception was thrown, handle it
        jclass exClass = (*env)->FindClass(env, "java/lang/RuntimeException");
        const char *msg = (const char *)err_msg;
        return (*env)->ThrowNew(env, exClass, msg);
    }
    else {
        int64_t n = (int64_t)num;
        roc__program_for_host_1__Factorial_caller(&n, 0, &ret);
        return ret;
    }
}
