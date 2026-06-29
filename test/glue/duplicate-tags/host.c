#include "roc_platform_abi.h"

#include <stdarg.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static size_t alloc_count = 0;
static size_t dealloc_count = 0;
static size_t failure_count = 0;
static char report[512];

static uintptr_t align_forward(uintptr_t value, size_t alignment) {
    return (value + alignment - 1u) & ~(uintptr_t)(alignment - 1u);
}

static void record_failure(const char *fmt, ...) {
    if (failure_count == 0) {
        va_list args;
        va_start(args, fmt);
        int prefix_len = snprintf(report, sizeof(report), "FAIL duplicate-tags CGlue: ");
        if (prefix_len > 0 && (size_t)prefix_len < sizeof(report)) {
            vsnprintf(report + prefix_len, sizeof(report) - (size_t)prefix_len, fmt, args);
        }
        va_end(args);
    }
    failure_count += 1;
}

void *roc_alloc(size_t length, size_t alignment) {
    const size_t total = length + alignment - 1u + sizeof(void *);
    uint8_t *raw = (uint8_t *)malloc(total == 0 ? 1 : total);
    if (raw == NULL) {
        record_failure("malloc failed");
        return NULL;
    }
    uintptr_t aligned = align_forward((uintptr_t)(raw + sizeof(void *)), alignment);
    ((void **)aligned)[-1] = raw;
    alloc_count += 1;
    return (void *)aligned;
}

void roc_dealloc(void *ptr, size_t alignment) {
    (void)alignment;
    if (ptr == NULL) {
        return;
    }
    free(((void **)ptr)[-1]);
    dealloc_count += 1;
}

void *roc_realloc(void *ptr, size_t new_length, size_t alignment) {
    void *new_ptr = roc_alloc(new_length, alignment);
    if (ptr != NULL) {
        roc_dealloc(ptr, alignment);
    }
    return new_ptr;
}

void roc_dbg(const uint8_t *bytes, size_t len) {
    fwrite(bytes, 1, len, stderr);
    fputc('\n', stderr);
}

void roc_expect_failed(const uint8_t *bytes, size_t len) {
    fwrite(bytes, 1, len, stderr);
    fputc('\n', stderr);
    record_failure("roc_expect_failed");
}

void roc_crashed(const uint8_t *bytes, size_t len) {
    fwrite(bytes, 1, len, stderr);
    fputc('\n', stderr);
    record_failure("roc_crashed");
    exit(1);
}

enum {
    TAG_OK = 1,
    TAG_OFFSET_VALUE = 32,
    TAG_OFFSET_RECORD = 56,
    TAG_OFFSET_NESTED = 40,
};

#define DEFINE_OK_HOSTED(name, ret_type, tag_offset) \
    ret_type name(RocStr arg0) {                     \
        (void)arg0;                                  \
        ret_type result;                             \
        memset(&result, 0, sizeof(result));          \
        result.bytes[(tag_offset)] = TAG_OK;         \
        return result;                               \
    }

DEFINE_OK_HOSTED(roc_a_unit, TryType0, TAG_OFFSET_VALUE)
DEFINE_OK_HOSTED(roc_a_str, TryType5, TAG_OFFSET_VALUE)
DEFINE_OK_HOSTED(roc_a_bytes, TryType6, TAG_OFFSET_VALUE)
DEFINE_OK_HOSTED(roc_a_record, TryType9, TAG_OFFSET_RECORD)
DEFINE_OK_HOSTED(roc_a_nested, TryType12, TAG_OFFSET_NESTED)

DEFINE_OK_HOSTED(roc_b_unit, TryType16, TAG_OFFSET_VALUE)
DEFINE_OK_HOSTED(roc_b_str, TryType21, TAG_OFFSET_VALUE)
DEFINE_OK_HOSTED(roc_b_bytes, TryType22, TAG_OFFSET_VALUE)
DEFINE_OK_HOSTED(roc_b_record, TryType25, TAG_OFFSET_RECORD)
DEFINE_OK_HOSTED(roc_b_nested, TryType28, TAG_OFFSET_NESTED)

DEFINE_OK_HOSTED(roc_c_unit, TryType32, TAG_OFFSET_VALUE)
DEFINE_OK_HOSTED(roc_c_str, TryType37, TAG_OFFSET_VALUE)
DEFINE_OK_HOSTED(roc_c_bytes, TryType38, TAG_OFFSET_VALUE)
DEFINE_OK_HOSTED(roc_c_record, TryType41, TAG_OFFSET_RECORD)
DEFINE_OK_HOSTED(roc_c_nested, TryType44, TAG_OFFSET_NESTED)

DEFINE_OK_HOSTED(roc_d_unit, TryType48, TAG_OFFSET_VALUE)
DEFINE_OK_HOSTED(roc_d_str, TryType53, TAG_OFFSET_VALUE)
DEFINE_OK_HOSTED(roc_d_bytes, TryType54, TAG_OFFSET_VALUE)
DEFINE_OK_HOSTED(roc_d_record, TryType57, TAG_OFFSET_RECORD)
DEFINE_OK_HOSTED(roc_d_nested, TryType60, TAG_OFFSET_NESTED)

DEFINE_OK_HOSTED(roc_fallible_unit, TryType64, TAG_OFFSET_VALUE)
DEFINE_OK_HOSTED(roc_fallible_str, TryType69, TAG_OFFSET_VALUE)
DEFINE_OK_HOSTED(roc_fallible_bytes, TryType70, TAG_OFFSET_VALUE)
DEFINE_OK_HOSTED(roc_fallible_record, TryType73, TAG_OFFSET_RECORD)
DEFINE_OK_HOSTED(roc_fallible_nested, TryType76, TAG_OFFSET_NESTED)

int main(int argc, char **argv) {
    (void)argc;
    (void)argv;
    roc_main();
    if (failure_count != 0) {
        fprintf(stderr, "%s\nalloc=%zu dealloc=%zu\n", report[0] == 0 ? "FAIL duplicate-tags CGlue: unknown failure" : report, alloc_count, dealloc_count);
        return 1;
    }
    fprintf(stderr, "PASS glue-runtime duplicate-tags CGlue native alloc=%zu dealloc=%zu\n", alloc_count, dealloc_count);
    return 0;
}
