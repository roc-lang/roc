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

EmptyOrPairOrPayloadOrRecursive roc_catalog_roundtrip(EmptyOrPairOrPayloadOrRecursive arg0) {
    return arg0;
}

void roc_catalog_single_no_payload(void) {}

CatalogPayload roc_catalog_single_payload_roundtrip(CatalogPayload arg0) {
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

static void run_contract(void) {
    __typeof__(roc_point()) point = roc_point();
    if (point.x != -17 || point.y != 42) {
        record_failure("point bytes mismatch");
    }

    __typeof__(roc_structural()) structural = roc_structural();
    if (structural.count != 19) {
        record_failure("structural count mismatch");
    }
    if (!roc_str_eq(&structural.name, "catalog")) {
        record_failure("structural name mismatch");
    }
    if (structural.nested.byte != 7 || !structural.nested.flag) {
        record_failure("structural nested bytes mismatch");
    }

    AResult result_a = roc_result_a();
    if (result_a.tag != AResultTag_Ok) {
        record_failure("A.Result tag mismatch");
    }
    RocStr result_a_payload = AResult_payload_ok(&result_a);
    if (!roc_str_eq(&result_a_payload, "alpha")) {
        record_failure("A.Result payload mismatch");
    }

    BResult result_b = roc_result_b();
    if (result_b.tag != BResultTag_Err) {
        record_failure("B.Result tag mismatch");
    }
    BResultErr result_b_payload = BResult_payload_err(&result_b);
    if (result_b_payload.code != 5) {
        record_failure("B.Result code mismatch");
    }
    if (!roc_str_eq(&result_b_payload.message, "bravo")) {
        record_failure("B.Result message mismatch");
    }

    RocDec dec = { .num = (__int128)1250000000000000000LL };
    if (roc_dec(dec).num != dec.num) {
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
