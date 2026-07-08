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

DEFINE_OK_HOSTED(roc_a_unit, AUnitResult, TAG_OFFSET_VALUE)
DEFINE_OK_HOSTED(roc_a_str, AStrResult, TAG_OFFSET_VALUE)
DEFINE_OK_HOSTED(roc_a_bytes, ABytesResult, TAG_OFFSET_VALUE)
DEFINE_OK_HOSTED(roc_a_record, ARecordResult, TAG_OFFSET_RECORD)
DEFINE_OK_HOSTED(roc_a_nested, ANestedResult, TAG_OFFSET_NESTED)

DEFINE_OK_HOSTED(roc_b_unit, BUnitResult, TAG_OFFSET_VALUE)
DEFINE_OK_HOSTED(roc_b_str, BStrResult, TAG_OFFSET_VALUE)
DEFINE_OK_HOSTED(roc_b_bytes, BBytesResult, TAG_OFFSET_VALUE)
DEFINE_OK_HOSTED(roc_b_record, BRecordResult, TAG_OFFSET_RECORD)
DEFINE_OK_HOSTED(roc_b_nested, BNestedResult, TAG_OFFSET_NESTED)

DEFINE_OK_HOSTED(roc_c_unit, CUnitResult, TAG_OFFSET_VALUE)
DEFINE_OK_HOSTED(roc_c_str, CStrResult, TAG_OFFSET_VALUE)
DEFINE_OK_HOSTED(roc_c_bytes, CBytesResult, TAG_OFFSET_VALUE)
DEFINE_OK_HOSTED(roc_c_record, CRecordResult, TAG_OFFSET_RECORD)
DEFINE_OK_HOSTED(roc_c_nested, CNestedResult, TAG_OFFSET_NESTED)

DEFINE_OK_HOSTED(roc_d_unit, DUnitResult, TAG_OFFSET_VALUE)
DEFINE_OK_HOSTED(roc_d_str, DStrResult, TAG_OFFSET_VALUE)
DEFINE_OK_HOSTED(roc_d_bytes, DBytesResult, TAG_OFFSET_VALUE)
DEFINE_OK_HOSTED(roc_d_record, DRecordResult, TAG_OFFSET_RECORD)
DEFINE_OK_HOSTED(roc_d_nested, DNestedResult, TAG_OFFSET_NESTED)

DEFINE_OK_HOSTED(roc_fallible_unit, HostFallibleUnitResult, TAG_OFFSET_VALUE)
DEFINE_OK_HOSTED(roc_fallible_str, HostFallibleStrResult, TAG_OFFSET_VALUE)
DEFINE_OK_HOSTED(roc_fallible_bytes, HostFallibleBytesResult, TAG_OFFSET_VALUE)
DEFINE_OK_HOSTED(roc_fallible_record, HostFallibleRecordResult, TAG_OFFSET_RECORD)
DEFINE_OK_HOSTED(roc_fallible_nested, HostFallibleNestedResult, TAG_OFFSET_NESTED)

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
