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

static void memory_zero(void *dst_void, size_t len) {
    uint8_t *dst = (uint8_t *)dst_void;
    for (size_t i = 0; i < len; i += 1) {
        dst[i] = 0;
    }
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
    set_report("FAIL duplicate-tags CGlue wasm32: ", message);
    failure_count += 1;
}

static void finish_pass(void) {
    const char *message = "PASS glue-runtime duplicate-tags CGlue wasm32";
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

enum {
    TAG_OK = 1,
    TAG_OFFSET_VALUE = 16,
    TAG_OFFSET_RECORD = 28,
    TAG_OFFSET_NESTED = 20,
};

#define DEFINE_OK_HOSTED(name, ret_type, tag_offset) \
    ret_type name(RocStr arg0) {                     \
        (void)arg0;                                  \
        ret_type result;                             \
        memory_zero(&result, sizeof(result));        \
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

WASM_EXPORT("wasm_main")
const uint8_t *wasm_main(void) {
    failure_count = 0;
    report_len = 0;
    report[0] = 0;
    roc_main();
    if (failure_count == 0) {
        finish_pass();
    } else if (report_len == 0) {
        set_report("FAIL duplicate-tags CGlue wasm32: ", "unknown failure");
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
