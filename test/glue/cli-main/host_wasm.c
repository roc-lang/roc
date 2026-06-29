#include "roc_platform_abi.h"

#include <stddef.h>
#include <stdint.h>

enum {
    MAX_ALLOCATIONS = 512,
    CANARY_SIZE = 16,
    CANARY_BYTE = 0xA5,
    POISON_BYTE = 0xCC,
    WASM_PAGE_SIZE = 65536,
};

#define WASM_EXPORT(name) __attribute__((export_name(name))) __attribute__((used))

typedef struct {
    uint8_t *raw;
    uint8_t *user;
    size_t length;
    size_t total;
    size_t alignment;
    int live;
} Allocation;

static Allocation allocations[MAX_ALLOCATIONS];
static uintptr_t heap_cursor = 0;
static size_t alloc_count = 0;
static size_t dealloc_count = 0;
static size_t live_alloc_count = 0;
static size_t allocator_error_count = 0;
static size_t failure_count = 0;
static size_t log_count = 0;
static char report[1024];
static size_t report_len = 0;

static size_t cstr_len(const char *bytes) {
    size_t len = 0;
    while (bytes[len] != 0) {
        len += 1;
    }
    return len;
}

static void memory_set(uint8_t *dst, uint8_t value, size_t len) {
    for (size_t i = 0; i < len; i += 1) {
        dst[i] = value;
    }
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

static int is_power_of_two(size_t value) {
    return value != 0 && (value & (value - 1u)) == 0;
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
    set_report("FAIL cli-main CGlue wasm32: ", message);
    failure_count += 1;
}

static void record_allocator_failure(const char *message) {
    allocator_error_count += 1;
    set_report("FAIL cli-main CGlue wasm32 allocator: ", message);
    failure_count += 1;
}

static int ensure_wasm_memory(uintptr_t end) {
    const size_t required_pages = ((size_t)end + (size_t)WASM_PAGE_SIZE - 1u) / (size_t)WASM_PAGE_SIZE;
    const size_t current_pages = __builtin_wasm_memory_size(0);
    if (required_pages <= current_pages) {
        return 1;
    }
    const size_t previous_pages = __builtin_wasm_memory_grow(0, required_pages - current_pages);
    return previous_pages != (size_t)-1;
}

static void *bump_alloc(size_t total, size_t alignment) {
    if (heap_cursor == 0) {
        heap_cursor = (uintptr_t)(__builtin_wasm_memory_size(0) * WASM_PAGE_SIZE);
    }
    const uintptr_t raw = align_forward(heap_cursor, alignment);
    if (raw > UINTPTR_MAX - total) {
        record_allocator_failure("bump allocation overflow");
        return NULL;
    }
    const uintptr_t end = raw + total;
    if (!ensure_wasm_memory(end)) {
        record_allocator_failure("wasm memory grow failed");
        return NULL;
    }
    heap_cursor = end;
    return (void *)raw;
}

static Allocation *find_allocation(void *ptr) {
    for (size_t i = 0; i < MAX_ALLOCATIONS; i += 1) {
        if (allocations[i].live && allocations[i].user == (uint8_t *)ptr) {
            return &allocations[i];
        }
    }
    return NULL;
}

static int check_canaries(const Allocation *allocation) {
    for (size_t i = 0; i < CANARY_SIZE; i += 1) {
        if (allocation->user[-(ptrdiff_t)CANARY_SIZE + (ptrdiff_t)i] != CANARY_BYTE) {
            record_allocator_failure("prefix canary changed");
            return 1;
        }
        if (allocation->user[allocation->length + i] != CANARY_BYTE) {
            record_allocator_failure("suffix canary changed");
            return 1;
        }
    }
    return 0;
}

static void *contract_alloc(size_t length, size_t alignment) {
    if (!is_power_of_two(alignment)) {
        record_allocator_failure("invalid alignment");
        return NULL;
    }
    if (length > SIZE_MAX - CANARY_SIZE - CANARY_SIZE - alignment) {
        record_allocator_failure("allocation size overflow");
        return NULL;
    }

    const size_t total = CANARY_SIZE + alignment - 1u + length + CANARY_SIZE;
    uint8_t *raw = (uint8_t *)bump_alloc(total == 0 ? 1 : total, alignment);
    if (raw == NULL) {
        return NULL;
    }

    uint8_t *user = (uint8_t *)align_forward((uintptr_t)(raw + CANARY_SIZE), alignment);
    if (((uintptr_t)user % alignment) != 0) {
        record_allocator_failure("returned pointer is not aligned");
        return NULL;
    }

    Allocation *slot = NULL;
    for (size_t i = 0; i < MAX_ALLOCATIONS; i += 1) {
        if (!allocations[i].live) {
            slot = &allocations[i];
            break;
        }
    }
    if (slot == NULL) {
        record_allocator_failure("allocation table exhausted");
        return NULL;
    }

    memory_set(user - CANARY_SIZE, CANARY_BYTE, CANARY_SIZE);
    memory_set(user, POISON_BYTE, length);
    memory_set(user + length, CANARY_BYTE, CANARY_SIZE);

    *slot = (Allocation){
        .raw = raw,
        .user = user,
        .length = length,
        .total = total,
        .alignment = alignment,
        .live = 1,
    };
    alloc_count += 1;
    live_alloc_count += 1;
    return user;
}

static void contract_dealloc(void *ptr, size_t alignment) {
    if (ptr == NULL) {
        return;
    }
    Allocation *allocation = find_allocation(ptr);
    if (allocation == NULL) {
        record_allocator_failure("unknown or double free");
        return;
    }
    if (allocation->alignment != alignment) {
        record_allocator_failure("dealloc alignment mismatch");
    }
    (void)check_canaries(allocation);
    memory_set(allocation->user, 0xDD, allocation->length);
    allocation->live = 0;
    dealloc_count += 1;
    live_alloc_count -= 1;
}

static void *contract_realloc(void *ptr, size_t new_length, size_t alignment) {
    if (ptr == NULL) {
        return contract_alloc(new_length, alignment);
    }
    Allocation *old = find_allocation(ptr);
    if (old == NULL) {
        record_allocator_failure("realloc unknown pointer");
        return NULL;
    }
    if (old->alignment != alignment) {
        record_allocator_failure("realloc alignment mismatch");
        return NULL;
    }
    if (check_canaries(old) != 0) {
        return NULL;
    }

    const size_t copy_length = old->length < new_length ? old->length : new_length;
    void *new_ptr = contract_alloc(new_length, alignment);
    if (new_ptr == NULL) {
        return NULL;
    }
    memory_copy(new_ptr, ptr, copy_length);
    if (!memory_equal(new_ptr, ptr, copy_length)) {
        record_allocator_failure("realloc did not preserve old bytes");
    }
    contract_dealloc(ptr, alignment);
    return new_ptr;
}

void *roc_alloc(size_t length, size_t alignment) {
    return contract_alloc(length, alignment);
}

void roc_dealloc(void *ptr, size_t alignment) {
    contract_dealloc(ptr, alignment);
}

void *roc_realloc(void *ptr, size_t new_length, size_t alignment) {
    return contract_realloc(ptr, new_length, alignment);
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

static RocStr roc_str_small(const char *bytes) {
    const size_t len = cstr_len(bytes);
    RocStr result = {0};
    if (len >= sizeof(RocStr)) {
        const size_t total = sizeof(size_t) + len;
        uint8_t *base = roc_alloc(total, ROC_ALIGNOF(size_t));
        if (base == NULL) {
            return result;
        }
        uint8_t *data = base + sizeof(size_t);
        *((intptr_t *)data - 1) = 1;
        memory_copy(data, bytes, len);
        result.bytes = data;
        result.capacity_or_alloc_ptr = len << 1;
        result.length = len;
        return result;
    }
    memory_copy(&result, bytes, len);
    ((uint8_t *)&result)[sizeof(RocStr) - 1u] = (uint8_t)(len | 0x80u);
    return result;
}

static int roc_str_is_small(RocStr str) {
    return (intptr_t)str.length < 0;
}

static size_t roc_str_len(const RocStr *str) {
    if (roc_str_is_small(*str)) {
        return ((const uint8_t *)str)[sizeof(RocStr) - 1u] ^ 0x80u;
    }
    return str->length;
}

static const uint8_t *roc_str_bytes(const RocStr *str) {
    if (roc_str_is_small(*str)) {
        return (const uint8_t *)str;
    }
    return str->bytes;
}

static uint8_t *roc_str_allocation_ptr(RocStr str) {
    if (roc_str_is_small(str)) {
        return NULL;
    }
    if ((str.capacity_or_alloc_ptr & 1u) != 0) {
        return (uint8_t *)(str.capacity_or_alloc_ptr & ~(uintptr_t)1u);
    }
    return str.bytes;
}

static void roc_str_decref(RocStr str) {
    uint8_t *alloc_ptr = roc_str_allocation_ptr(str);
    if (alloc_ptr == NULL) {
        return;
    }
    intptr_t *rc = (intptr_t *)(alloc_ptr - sizeof(intptr_t));
    if (*rc == 0) {
        return;
    }
    *rc -= 1;
    if (*rc == 0) {
        roc_dealloc(alloc_ptr - sizeof(size_t), ROC_ALIGNOF(size_t));
    }
}

static RocList make_args(void) {
    const size_t length = 2;
    const size_t header_bytes = 2 * sizeof(size_t);
    const size_t total = header_bytes + length * sizeof(RocStr);
    uint8_t *base = roc_alloc(total, ROC_ALIGNOF(size_t));
    if (base == NULL) {
        return (RocList){0};
    }
    RocStr *items = (RocStr *)(base + header_bytes);
    ((size_t *)items)[-2] = length;
    ((intptr_t *)items)[-1] = 1;
    items[0] = roc_str_small("alpha");
    items[1] = roc_str_small("beta");
    return (RocList){
        .elements = items,
        .length = length,
        .capacity_or_alloc_ptr = length << 1,
    };
}

RocStr roc_cli_read(void) {
    return roc_str_small("contract-input");
}

void roc_cli_log(RocStr arg0) {
    const char *expected = "roc saw contract-input argc=2 first=alpha";
    const size_t expected_len = cstr_len(expected);
    const size_t actual_len = roc_str_len(&arg0);
    const uint8_t *actual = roc_str_bytes(&arg0);
    if (actual_len != expected_len || !memory_equal(actual, expected, expected_len)) {
        record_failure("unexpected log payload");
    }
    log_count += 1;
    roc_str_decref(arg0);
}

TryType2 roc_cli_many(
    uint8_t arg0,
    uint16_t arg1,
    uint32_t arg2,
    uint64_t arg3,
    unsigned __int128 arg4,
    int8_t arg5,
    int16_t arg6,
    int32_t arg7,
    int64_t arg8,
    __int128 arg9,
    float arg10,
    double arg11,
    double arg12,
    bool arg13,
    RocStr arg14) {
    (void)arg0;
    (void)arg1;
    (void)arg2;
    (void)arg3;
    (void)arg4;
    (void)arg5;
    (void)arg6;
    (void)arg7;
    (void)arg8;
    (void)arg9;
    (void)arg10;
    (void)arg11;
    (void)arg12;
    (void)arg13;
    roc_str_decref(arg14);
    record_failure("roc_cli_many was called");
    return (TryType2){{0}};
}

CliHostNamedRecord roc_cli_shape(CircleOrEmptyOrRect arg0, AnonStruct22 arg1) {
    (void)arg0;
    (void)arg1;
    record_failure("roc_cli_shape was called");
    return (CliHostNamedRecord){{0}};
}

AnonStruct25 roc_cli_wide(double arg0, __int128 arg1, unsigned __int128 arg2) {
    (void)arg0;
    (void)arg1;
    (void)arg2;
    record_failure("roc_cli_wide was called");
    return (AnonStruct25){{0}};
}

static void reset_contract(void) {
    for (size_t i = 0; i < MAX_ALLOCATIONS; i += 1) {
        allocations[i] = (Allocation){0};
    }
    alloc_count = 0;
    dealloc_count = 0;
    live_alloc_count = 0;
    allocator_error_count = 0;
    failure_count = 0;
    log_count = 0;
    report_len = 0;
    report[0] = 0;
}

static void finish_pass(void) {
    const char *message = "PASS glue-runtime cli-main CGlue wasm32";
    report_len = cstr_len(message);
    memory_copy(report, message, report_len);
    report[report_len] = 0;
}

static void run_contract(void) {
    RocList args = make_args();
    if (failure_count == 0) {
        TryType29 result = roc_main(args);
        const uint8_t tag = result.bytes[4];
        if (tag != 1) {
            record_failure("roc_main returned Err");
        }
    }

    if (log_count != 1) {
        record_failure("expected one log call");
    }
    if (allocator_error_count != 0) {
        record_failure("allocator recorded errors");
    }
    if (live_alloc_count != 0) {
        record_failure("live allocations after scenario");
    }

    if (failure_count == 0) {
        finish_pass();
    } else if (report_len == 0) {
        set_report("FAIL cli-main CGlue wasm32: ", "unknown failure");
    }
}

WASM_EXPORT("wasm_main")
const uint8_t *wasm_main(void) {
    reset_contract();
    run_contract();
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
