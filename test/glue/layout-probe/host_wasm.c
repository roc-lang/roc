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

static int memory_equal(const void *lhs_void, const void *rhs_void, size_t len) {
    const uint8_t *lhs = (const uint8_t *)lhs_void;
    const uint8_t *rhs = (const uint8_t *)rhs_void;
    for (size_t i = 0; i < len; i += 1) {
        if (lhs[i] != rhs[i]) {
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
    set_report("FAIL layout-probe CGlue wasm32: ", message);
    failure_count += 1;
}

static void finish_pass(void) {
    const char *message = "PASS glue-runtime layout-probe CGlue wasm32";
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

ProbeLayoutProbe roc_probe_roundtrip(ProbeLayoutProbe arg0) {
    return arg0;
}

RocU8x16 roc_probe_roundtrip_u8x16(RocU8x16 arg0) { return arg0; }
RocI8x16 roc_probe_roundtrip_i8x16(RocI8x16 arg0) { return arg0; }
RocU16x8 roc_probe_roundtrip_u16x8(RocU16x8 arg0) { return arg0; }
RocI16x8 roc_probe_roundtrip_i16x8(RocI16x8 arg0) { return arg0; }
RocU32x4 roc_probe_roundtrip_u32x4(RocU32x4 arg0) { return arg0; }
RocI32x4 roc_probe_roundtrip_i32x4(RocI32x4 arg0) { return arg0; }
RocU64x2 roc_probe_roundtrip_u64x2(RocU64x2 arg0) { return arg0; }
RocI64x2 roc_probe_roundtrip_i64x2(RocI64x2 arg0) { return arg0; }
ProbeVectorRecord roc_probe_roundtrip_vector_record(ProbeVectorRecord arg0) { return arg0; }
ProbeVectorQuad roc_probe_roundtrip_vector_quad(ProbeVectorQuad arg0) { return arg0; }
ProbeVectorHva roc_probe_roundtrip_vector_hva(ProbeVectorHva arg0) { return arg0; }
ProbeVectorWrapper roc_probe_roundtrip_vector_wrapper(ProbeVectorWrapper arg0) { return arg0; }
ProbeVectorTag roc_probe_roundtrip_vector_tag(ProbeVectorTag arg0) { return arg0; }

AnonStructFbe9eaebfd8c38fd roc_probe_roundtrip_vector_tuple(ProbeRoundtripVectorTupleArgs arg0) {
    AnonStructFbe9eaebfd8c38fd result = { ._1 = arg0._1, ._2 = arg0._2, ._0 = arg0._0 };
    return result;
}

RocU8x16 roc_probe_exhaust_registers(
    int64_t arg0, int64_t arg1, int64_t arg2, int64_t arg3, int64_t arg4, int64_t arg5,
    double arg6, double arg7, double arg8, double arg9, double arg10, double arg11, double arg12, double arg13,
    RocU8x16 arg14) {
    (void)arg0; (void)arg1; (void)arg2; (void)arg3; (void)arg4; (void)arg5;
    (void)arg6; (void)arg7; (void)arg8; (void)arg9; (void)arg10; (void)arg11; (void)arg12; (void)arg13;
    return arg14;
}

ProbeNestedVectorHva roc_probe_spill_vector_hva(
    RocU8x16 arg0, RocU8x16 arg1, RocU8x16 arg2, RocU8x16 arg3,
    RocU8x16 arg4, RocU8x16 arg5, RocU8x16 arg6, ProbeNestedVectorHva arg7) {
    (void)arg0; (void)arg1; (void)arg2; (void)arg3; (void)arg4; (void)arg5; (void)arg6;
    return arg7;
}

ProbeNestedFloatHfa roc_probe_spill_float_hfa(
    double arg0, double arg1, double arg2, double arg3, double arg4, double arg5, double arg6,
    ProbeNestedFloatHfa arg7) {
    (void)arg0; (void)arg1; (void)arg2; (void)arg3; (void)arg4; (void)arg5; (void)arg6;
    return arg7;
}

ProbeIntegerPair roc_probe_spill_integer_pair(
    int64_t arg0, int64_t arg1, int64_t arg2, int64_t arg3, int64_t arg4, int64_t arg5, int64_t arg6,
    ProbeIntegerPair arg7) {
    (void)arg0; (void)arg1; (void)arg2; (void)arg3; (void)arg4; (void)arg5; (void)arg6;
    return arg7;
}

__int128 roc_probe_align_i128(int64_t arg0, __int128 arg1) { (void)arg0; return arg1; }
__int128 roc_probe_spill_i128(int64_t arg0, int64_t arg1, int64_t arg2, int64_t arg3, int64_t arg4, __int128 arg5) {
    (void)arg0; (void)arg1; (void)arg2; (void)arg3; (void)arg4;
    return arg5;
}
RocDec roc_probe_spill_dec(int64_t arg0, int64_t arg1, int64_t arg2, int64_t arg3, int64_t arg4, RocDec arg5) {
    (void)arg0; (void)arg1; (void)arg2; (void)arg3; (void)arg4;
    return arg5;
}
uint64_t roc_probe_compact_stack(
    int64_t arg0, int64_t arg1, int64_t arg2, int64_t arg3,
    int64_t arg4, int64_t arg5, int64_t arg6, int64_t arg7,
    uint8_t tiny, uint16_t short_value, uint32_t word) {
    (void)arg0; (void)arg1; (void)arg2; (void)arg3;
    (void)arg4; (void)arg5; (void)arg6; (void)arg7;
    return (uint64_t)tiny + (uint64_t)short_value + (uint64_t)word;
}

static void check_provided_abi(void) {
    const uint8_t expected[16] = {
        0x10, 0x21, 0x32, 0x43, 0x54, 0x65, 0x76, 0x87,
        0x98, 0xa9, 0xba, 0xcb, 0xdc, 0xed, 0xfe, 0x0f,
    };

#define CHECK_PROVIDED_VECTOR(TYPE, FN, LABEL) do { \
    TYPE input; \
    memory_copy(&input, expected, sizeof(input)); \
    TYPE output = FN(input); \
    if (!memory_equal(&output, expected, sizeof(output))) record_failure("provided " LABEL " mismatch"); \
} while (0)

    CHECK_PROVIDED_VECTOR(RocU8x16, roc_provide_u8x16, "U8x16");
    CHECK_PROVIDED_VECTOR(RocI8x16, roc_provide_i8x16, "I8x16");
    CHECK_PROVIDED_VECTOR(RocU16x8, roc_provide_u16x8, "U16x8");
    CHECK_PROVIDED_VECTOR(RocI16x8, roc_provide_i16x8, "I16x8");
    CHECK_PROVIDED_VECTOR(RocU32x4, roc_provide_u32x4, "U32x4");
    CHECK_PROVIDED_VECTOR(RocI32x4, roc_provide_i32x4, "I32x4");
    CHECK_PROVIDED_VECTOR(RocU64x2, roc_provide_u64x2, "U64x2");
    CHECK_PROVIDED_VECTOR(RocI64x2, roc_provide_i64x2, "I64x2");

#undef CHECK_PROVIDED_VECTOR

    RocU8x16 bytes;
    RocI16x8 words;
    RocI32x4 dwords;
    RocU32x4 udwords;
    RocI64x2 qwords;
    memory_copy(&bytes, expected, sizeof(bytes));
    memory_copy(&words, expected, sizeof(words));
    memory_copy(&dwords, expected, sizeof(dwords));
    memory_copy(&udwords, expected, sizeof(udwords));
    memory_copy(&qwords, expected, sizeof(qwords));

    ProbeVectorWrapper wrapper = { .only = bytes };
    ProbeVectorWrapper wrapper_back = roc_provide_vector_wrapper(wrapper);
    if (!memory_equal(&wrapper_back, &wrapper, sizeof(wrapper))) record_failure("provided vector wrapper mismatch");

    ProbeVectorRecord record = { .bytes = bytes, .words = dwords, .before = 0x1020304050607080ULL, .after = 0xa0b0c0d0U };
    ProbeVectorRecord record_back = roc_provide_vector_record(record);
    if (!memory_equal(&record_back, &record, sizeof(record))) record_failure("provided vector record mismatch");

    ProbeVectorQuad quad = { .a = { .value = bytes }, .b = { .value = words }, .c = { .value = udwords }, .d = { .value = qwords } };
    ProbeVectorQuad quad_back = roc_provide_vector_quad(quad);
    if (!memory_equal(&quad_back, &quad, sizeof(quad))) record_failure("provided vector quad mismatch");

    ProbeVectorHva hva = { .a = { .value = bytes }, .b = { .value = bytes }, .c = { .value = bytes }, .d = { .value = bytes } };
    ProbeVectorHva hva_back = roc_provide_vector_hva(hva);
    if (!memory_equal(&hva_back, &hva, sizeof(hva))) record_failure("provided vector HVA mismatch");

    AnonStructFbe9eaebfd8c38fd tuple = { ._1 = bytes, ._2 = words, ._0 = 0x1020304050607080ULL };
    AnonStructFbe9eaebfd8c38fd tuple_back = roc_provide_vector_tuple(tuple);
    if (!memory_equal(&tuple_back, &tuple, sizeof(tuple))) record_failure("provided vector tuple mismatch");

    ProbeVectorTagPairPayload pair = { ._1 = words, ._0 = 0x1020304050607080ULL };
    ProbeVectorTag tag = ProbeVectorTag_make_pair(pair);
    ProbeVectorTag tag_back = roc_provide_vector_tag(tag);
    ProbeVectorTagPairPayload pair_back = ProbeVectorTag_payload_pair(&tag_back);
    if (tag_back.tag != ProbeVectorTagTag_Pair
        || pair_back._0 != pair._0
        || !memory_equal(&pair_back._1, &pair._1, sizeof(pair._1))) {
        record_failure("provided host-constructed vector tag mismatch");
    }

    ProbeVectorTag roc_tag = roc_make_vector_tag();
    ProbeVectorTag roc_tag_back = roc_provide_vector_tag(roc_tag);
    ProbeVectorTagPairPayload roc_pair_back = ProbeVectorTag_payload_pair(&roc_tag_back);
    const uint8_t expected_roc_tag_vector[16] = {
        0xff, 0xee, 0xdd, 0xcc, 0xbb, 0xaa, 0x99, 0x88,
        0x77, 0x66, 0x55, 0x44, 0x33, 0x22, 0x11, 0x00,
    };
    if (roc_tag_back.tag != ProbeVectorTagTag_Pair
        || roc_pair_back._0 != 0x1020304050607080ULL
        || !memory_equal(&roc_pair_back._1, expected_roc_tag_vector, sizeof(roc_pair_back._1))) {
        record_failure("provided Roc-constructed vector tag mismatch");
    }

    RocU8x16 exhausted = roc_provide_exhaust_registers(
        1, 2, 3, 4, 5, 6,
        0.5, 1.5, 2.5, 3.5, 4.5, 5.5, 6.5, 7.5,
        bytes);
    if (!memory_equal(&exhausted, &bytes, sizeof(bytes))) record_failure("provided exhausted-register vector mismatch");
}

WASM_EXPORT("wasm_main")
const uint8_t *wasm_main(void) {
    failure_count = 0;
    report_len = 0;
    report[0] = 0;
    roc_main();
    check_provided_abi();
    if (failure_count == 0) {
        finish_pass();
    } else if (report_len == 0) {
        set_report("FAIL layout-probe CGlue wasm32: ", "unknown failure");
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
