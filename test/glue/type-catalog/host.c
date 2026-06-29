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
        int prefix_len = snprintf(report, sizeof(report), "FAIL type-catalog CGlue: ");
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

EmptyOrPairOrPayloadOrRecursiveType0 roc_catalog_roundtrip(EmptyOrPairOrPayloadOrRecursiveType0 arg0) {
    return arg0;
}

void roc_catalog_single_no_payload(void) {}

CatalogPayloadType3 roc_catalog_single_payload_roundtrip(CatalogPayloadType3 arg0) {
    return arg0;
}

static size_t roc_str_len(const RocStr *str) {
    if ((intptr_t)str->length < 0) {
        return ((const uint8_t *)str)[sizeof(RocStr) - 1u] ^ 0x80u;
    }
    return str->length;
}

static const uint8_t *roc_str_bytes(const RocStr *str) {
    if ((intptr_t)str->length < 0) {
        return (const uint8_t *)str;
    }
    return str->bytes;
}

static int roc_str_eq(const RocStr *str, const char *expected) {
    const size_t expected_len = strlen(expected);
    return roc_str_len(str) == expected_len && memcmp(roc_str_bytes(str), expected, expected_len) == 0;
}

static int32_t read_i32(const uint8_t *bytes) {
    int32_t value;
    memcpy(&value, bytes, sizeof(value));
    return value;
}

static uint64_t read_u64(const uint8_t *bytes) {
    uint64_t value;
    memcpy(&value, bytes, sizeof(value));
    return value;
}

enum {
    STRUCTURAL_NAME_OFFSET = 8,
    STRUCTURAL_NESTED_OFFSET = 32,
    A_RESULT_TAG_OFFSET = 24,
    B_RESULT_TAG_OFFSET = 32,
    B_RESULT_ERR_MESSAGE_OFFSET = 0,
    B_RESULT_ERR_CODE_OFFSET = 24,
    TAG_ERR = 0,
    TAG_OK = 1,
};

static void run_contract(void) {
    CatalogPointType16 point = roc_point();
    if (read_i32(point.bytes + 0) != -17 || read_i32(point.bytes + 4) != 42) {
        record_failure("point bytes mismatch");
    }

    AnonStruct19 structural = roc_structural();
    if (read_u64(structural.bytes + 0) != 19) {
        record_failure("structural count mismatch");
    }
    RocStr *name = (RocStr *)(void *)(structural.bytes + STRUCTURAL_NAME_OFFSET);
    if (!roc_str_eq(name, "catalog")) {
        record_failure("structural name mismatch");
    }
    if (structural.bytes[STRUCTURAL_NESTED_OFFSET] != 7 || structural.bytes[STRUCTURAL_NESTED_OFFSET + 1] != 1) {
        record_failure("structural nested bytes mismatch");
    }

    AResult result_a = roc_result_a();
    if (result_a.bytes[A_RESULT_TAG_OFFSET] != TAG_OK) {
        record_failure("A.Result tag mismatch");
    }
    RocStr *result_a_payload = (RocStr *)(void *)result_a.bytes;
    if (!roc_str_eq(result_a_payload, "alpha")) {
        record_failure("A.Result payload mismatch");
    }

    BResult result_b = roc_result_b();
    if (result_b.bytes[B_RESULT_TAG_OFFSET] != TAG_ERR) {
        record_failure("B.Result tag mismatch");
    }
    if (read_i32(result_b.bytes + B_RESULT_ERR_CODE_OFFSET) != 5) {
        record_failure("B.Result code mismatch");
    }
    RocStr *result_b_message = (RocStr *)(void *)(result_b.bytes + B_RESULT_ERR_MESSAGE_OFFSET);
    if (!roc_str_eq(result_b_message, "bravo")) {
        record_failure("B.Result message mismatch");
    }

    if (roc_dec(12.5) != 12.5) {
        record_failure("Dec identity mismatch");
    }
    if (roc_i128((__int128)-123456789) != (__int128)-123456789) {
        record_failure("I128 identity mismatch");
    }
    if (roc_u128((unsigned __int128)123456789u) != (unsigned __int128)123456789u) {
        record_failure("U128 identity mismatch");
    }
}

int main(void) {
    run_contract();
    if (failure_count != 0) {
        fprintf(stderr, "%s\nalloc=%zu dealloc=%zu\n", report[0] == 0 ? "FAIL type-catalog CGlue: unknown failure" : report, alloc_count, dealloc_count);
        return 1;
    }
    fprintf(stderr, "PASS glue-runtime type-catalog CGlue native alloc=%zu dealloc=%zu\n", alloc_count, dealloc_count);
    return 0;
}
