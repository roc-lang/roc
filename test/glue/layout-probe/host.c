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
        int prefix_len = snprintf(report, sizeof(report), "FAIL layout-probe CGlue: ");
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
    memcpy(&input, expected, sizeof(input)); \
    TYPE output = FN(input); \
    if (memcmp(&output, expected, sizeof(output)) != 0) record_failure("provided " LABEL " mismatch"); \
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
    memcpy(&bytes, expected, sizeof(bytes));
    memcpy(&words, expected, sizeof(words));
    memcpy(&dwords, expected, sizeof(dwords));
    memcpy(&udwords, expected, sizeof(udwords));
    memcpy(&qwords, expected, sizeof(qwords));

    ProbeVectorWrapper wrapper = { .only = bytes };
    ProbeVectorWrapper wrapper_back = roc_provide_vector_wrapper(wrapper);
    if (memcmp(&wrapper_back, &wrapper, sizeof(wrapper)) != 0) record_failure("provided vector wrapper mismatch");

    ProbeVectorRecord record = { .bytes = bytes, .words = dwords, .before = 0x1020304050607080ULL, .after = 0xa0b0c0d0U };
    ProbeVectorRecord record_back = roc_provide_vector_record(record);
    if (memcmp(&record_back, &record, sizeof(record)) != 0) record_failure("provided vector record mismatch");

    ProbeVectorQuad quad = { .a = { .value = bytes }, .b = { .value = words }, .c = { .value = udwords }, .d = { .value = qwords } };
    ProbeVectorQuad quad_back = roc_provide_vector_quad(quad);
    if (memcmp(&quad_back, &quad, sizeof(quad)) != 0) record_failure("provided vector quad mismatch");

    ProbeVectorHva hva = { .a = { .value = bytes }, .b = { .value = bytes }, .c = { .value = bytes }, .d = { .value = bytes } };
    ProbeVectorHva hva_back = roc_provide_vector_hva(hva);
    if (memcmp(&hva_back, &hva, sizeof(hva)) != 0) record_failure("provided vector HVA mismatch");

    AnonStructFbe9eaebfd8c38fd tuple = { ._1 = bytes, ._2 = words, ._0 = 0x1020304050607080ULL };
    AnonStructFbe9eaebfd8c38fd tuple_back = roc_provide_vector_tuple(tuple);
    if (memcmp(&tuple_back, &tuple, sizeof(tuple)) != 0) record_failure("provided vector tuple mismatch");

    ProbeVectorTagPairPayload pair = { ._1 = words, ._0 = 0x1020304050607080ULL };
    ProbeVectorTag tag = ProbeVectorTag_make_pair(pair);
    ProbeVectorTag tag_back = roc_provide_vector_tag(tag);
    ProbeVectorTagPairPayload pair_back = ProbeVectorTag_payload_pair(&tag_back);
    if (tag_back.tag != ProbeVectorTagTag_Pair
        || pair_back._0 != pair._0
        || memcmp(&pair_back._1, &pair._1, sizeof(pair._1)) != 0) {
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
        || memcmp(&roc_pair_back._1, expected_roc_tag_vector, sizeof(roc_pair_back._1)) != 0) {
        record_failure("provided Roc-constructed vector tag mismatch");
    }

    RocU8x16 exhausted = roc_provide_exhaust_registers(
        1, 2, 3, 4, 5, 6,
        0.5, 1.5, 2.5, 3.5, 4.5, 5.5, 6.5, 7.5,
        bytes);
    if (memcmp(&exhausted, &bytes, sizeof(bytes)) != 0) record_failure("provided exhausted-register vector mismatch");

    ProbeNestedVectorHva nested_hva = { .wrapped = { .value = bytes }, .raw = { .value = bytes } };
    ProbeNestedVectorHva nested_hva_back = roc_provide_spill_vector_hva(
        bytes, bytes, bytes, bytes, bytes, bytes, bytes, nested_hva);
    if (memcmp(&nested_hva_back, &nested_hva, sizeof(nested_hva)) != 0) record_failure("provided spilled HVA mismatch");

    ProbeNestedFloatHfa nested_hfa = { .wrapped = 12.5, .raw = -7.25 };
    ProbeNestedFloatHfa nested_hfa_back = roc_provide_spill_float_hfa(
        0.5, 1.5, 2.5, 3.5, 4.5, 5.5, 6.5, nested_hfa);
    if (memcmp(&nested_hfa_back, &nested_hfa, sizeof(nested_hfa)) != 0) record_failure("provided spilled HFA mismatch");

    ProbeIntegerPair pair_value = { .first = -0x102030405060708LL, .second = 0x8877665544332211ULL };
    ProbeIntegerPair pair_value_back = roc_provide_spill_integer_pair(1, 2, 3, 4, 5, 6, 7, pair_value);
    if (memcmp(&pair_value_back, &pair_value, sizeof(pair_value)) != 0) record_failure("provided spilled integer pair mismatch");

    const __int128 wide_i128 = ((__int128)0x0011223344556677ULL << 64) | (__int128)0x8899aabbccddeeffULL;
    if (roc_provide_align_i128(9, wide_i128) != wide_i128) record_failure("provided aligned i128 mismatch");
    if (roc_provide_spill_i128(1, 2, 3, 4, 5, wide_i128) != wide_i128) record_failure("provided spilled i128 mismatch");
    RocDec wide_dec = { .num = ((__int128)0x0123456789abcdefULL << 64) | (__int128)0xfedcba9876543210ULL };
    RocDec wide_dec_back = roc_provide_spill_dec(1, 2, 3, 4, 5, wide_dec);
    if (wide_dec_back.num != wide_dec.num) record_failure("provided spilled Dec mismatch");
    const uint64_t compact = roc_provide_compact_stack(1, 2, 3, 4, 5, 6, 7, 8, 0x12, 0x3456, 0x789abcde);
    if (compact != 0x12ULL + 0x3456ULL + 0x789abcdeULL) record_failure("provided compact stack mismatch");
}

int main(void) {
    roc_main();
    check_provided_abi();
    if (failure_count != 0) {
        fprintf(stderr, "%s\n", report[0] == 0 ? "FAIL layout-probe CGlue: unknown failure" : report);
        return 1;
    }
    fprintf(stderr, "PASS glue-runtime layout-probe CGlue native alloc=%zu dealloc=%zu\n", alloc_count, dealloc_count);
    return 0;
}
