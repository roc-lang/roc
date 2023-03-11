#include <errno.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>
#include <string.h>
#include <unistd.h>
#include <stdalign.h>
#include <stdint.h>

#include <jni.h>
#include "HelloJNI.h"


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

__attribute__((noreturn)) void roc_panic(void *ptr, unsigned int alignment)
{
   // TODO
   exit(0);
}

void *roc_memcpy(void *dest, const void *src, size_t n)
{
    return memcpy(dest, src, n);
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
        uint8_t *new_content = ((uint8_t *)roc_alloc(len + refcount_size, alignof(size_t))) + refcount_size;

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

extern void roc__mainForHost_1_exposed_generic(struct RocBytes *ret, struct RocBytes *arg);

JNIEXPORT jstring JNICALL Java_javaSource_HelloJNI_sayHello
   (JNIEnv *env, jobject thisObj, jint num)
{
    char native_string[256] = {0};
    sprintf(native_string, "%d", num);

    struct RocBytes arg = init_rocbytes((uint8_t *)native_string, strlen(native_string));
    struct RocBytes ret = {0};

    // Call the Roc function to populate `ret`'s bytes.
    roc__mainForHost_1_exposed_generic(&ret, &arg);

    decref((void *)&ret, alignof(uint8_t *));


    // java being java making this a lot harder than it needs to be
    // https://stackoverflow.com/questions/32205446/getting-true-utf-8-characters-in-java-jni
    // https://docs.oracle.com/javase/1.5.0/docs/guide/jni/spec/types.html#wp16542
    // but as i refuse converting those manually to their correct form, we just let the jvm handle the conversion
    // by first making a java byte array then converting the byte array to our final jstring
    jbyteArray byteArray = (*env)->NewByteArray(env, ret.len);
    (*env)->SetByteArrayRegion(env, byteArray, 0, ret.len, (jbyte*)ret.bytes);
    jbyte* bytes = (*env)->GetByteArrayElements(env, byteArray, 0);

    jstring charsetName = (*env)->NewStringUTF(env, "UTF-8");
    jclass stringClass = (*env)->FindClass(env, "java/lang/String");
    // https://docs.oracle.com/javase/7/docs/jdk/api/jpda/jdi/com/sun/jdi/doc-files/signature.html
    jmethodID stringConstructor = (*env)->GetMethodID(env, stringClass, "<init>", "([BLjava/lang/String;)V");

    jstring result = (*env)->NewObject(env, stringClass, stringConstructor, byteArray, charsetName);

    // probably unnecessary cause stuff created with env should just be dead when env is dead
    // also they're stored on the stack so truly not required but ig couldn't hurt..
    (*env)->DeleteLocalRef(env, (jobject)stringConstructor);
    (*env)->DeleteLocalRef(env, (jobject)stringClass);

    (*env)->DeleteLocalRef(env, charsetName);
    (*env)->ReleaseByteArrayElements(env, byteArray, bytes, 0);
    (*env)->DeleteLocalRef(env, byteArray);

    return result;
}
