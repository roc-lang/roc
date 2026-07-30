#include "roc_platform_abi.h"

#include <stddef.h>
#include <stdint.h>

#define WASM_EXPORT(name) __attribute__((export_name(name))) __attribute__((used))

static char report[512];
static size_t report_len = 0;
static size_t failure_count = 0;
static size_t alloc_count = 0;
static size_t dealloc_count = 0;
static uintptr_t heap_cursor = 0;

static size_t cstr_len(const char *bytes) {
    size_t len = 0;
    while (bytes[len] != 0) {
        len += 1;
    }
    return len;
}

static void memory_copy(void *dst_void, const void *src_void, size_t len) {
    uint8_t *dst = (uint8_t *)dst_void;
    const uint8_t *src = (const uint8_t *)src_void;
    for (size_t i = 0; i < len; i += 1) {
        dst[i] = src[i];
    }
}

static int memory_equal(const void *left_void, const void *right_void, size_t len) {
    const uint8_t *left = (const uint8_t *)left_void;
    const uint8_t *right = (const uint8_t *)right_void;
    for (size_t i = 0; i < len; i += 1) {
        if (left[i] != right[i]) {
            return 0;
        }
    }
    return 1;
}

static uintptr_t align_forward(uintptr_t value, size_t alignment) {
    return (value + alignment - 1u) & ~(uintptr_t)(alignment - 1u);
}

static void set_report(const char *prefix, const char *message) {
    if (failure_count != 0) {
        return;
    }
    const size_t prefix_len = cstr_len(prefix);
    const size_t message_len = cstr_len(message);
    size_t written = 0;
    while (written < prefix_len && written + 1u < sizeof(report)) {
        report[written] = prefix[written];
        written += 1;
    }
    for (size_t i = 0; i < message_len && written + 1u < sizeof(report); i += 1) {
        report[written] = message[i];
        written += 1;
    }
    report[written] = 0;
    report_len = written;
}

static void record_failure(const char *message) {
    set_report("FAIL type-catalog CGlue wasm32: ", message);
    failure_count += 1;
}

static void finish_pass(void) {
    const char *message = "PASS glue-runtime type-catalog CGlue wasm32";
    report_len = cstr_len(message);
    memory_copy(report, message, report_len);
    report[report_len] = 0;
}

void *roc_alloc(size_t length, size_t alignment) {
    const size_t page_size = 65536;
    if (heap_cursor == 0) {
        heap_cursor = (uintptr_t)(__builtin_wasm_memory_size(0) * page_size);
    }
    const uintptr_t ptr = align_forward(heap_cursor, alignment);
    const uintptr_t end = ptr + length;
    const size_t required_pages = ((size_t)end + page_size - 1u) / page_size;
    const size_t current_pages = __builtin_wasm_memory_size(0);
    if (required_pages > current_pages) {
        if (__builtin_wasm_memory_grow(0, required_pages - current_pages) == (size_t)-1) {
            record_failure("wasm memory grow failed");
            return 0;
        }
    }
    heap_cursor = end;
    alloc_count += 1;
    return (void *)ptr;
}

void roc_dealloc(void *ptr, size_t alignment) {
    (void)ptr;
    (void)alignment;
    dealloc_count += 1;
}

void *roc_realloc(void *ptr, size_t new_length, size_t alignment) {
    (void)ptr;
    return roc_alloc(new_length, alignment);
}

void roc_dbg(const uint8_t *bytes, size_t len) {
    (void)bytes;
    (void)len;
}

void roc_expect_failed(const uint8_t *bytes, size_t len) {
    (void)bytes;
    (void)len;
    record_failure("roc_expect_failed");
}

void roc_crashed(const uint8_t *bytes, size_t len) {
    (void)bytes;
    (void)len;
    record_failure("roc_crashed");
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
    const size_t expected_len = cstr_len(expected);
    return roc_str_len(str) == expected_len && memory_equal(roc_str_bytes(str), expected, expected_len);
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

WASM_EXPORT("wasm_main")
const uint8_t *wasm_main(void) {
    failure_count = 0;
    report_len = 0;
    report[0] = 0;
    run_contract();
    if (failure_count == 0) {
        finish_pass();
    } else if (report_len == 0) {
        set_report("FAIL type-catalog CGlue wasm32: ", "unknown failure");
    }
    return (const uint8_t *)report;
}

WASM_EXPORT("wasm_result_len")
size_t wasm_result_len(void) {
    return report_len;
}

WASM_EXPORT("wasm_alloc_count")
size_t wasm_alloc_count(void) {
    return alloc_count;
}

WASM_EXPORT("wasm_dealloc_count")
size_t wasm_dealloc_count(void) {
    return dealloc_count;
}
