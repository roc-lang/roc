#include "roc_platform_abi.h"

#include <stdarg.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

enum {
    MAX_ALLOCATIONS = 512,
    CANARY_SIZE = 16,
    CANARY_BYTE = 0xA5,
    POISON_BYTE = 0xCC,
};

typedef struct {
    uint8_t *raw;
    uint8_t *user;
    size_t length;
    size_t total;
    size_t alignment;
    int live;
} Allocation;

static Allocation allocations[MAX_ALLOCATIONS];
static size_t alloc_count = 0;
static size_t dealloc_count = 0;
static size_t live_alloc_count = 0;
static size_t allocator_error_count = 0;
static size_t failure_count = 0;
static char report[1024];

static void record_failure(const char *fmt, ...) {
    if (failure_count == 0) {
        va_list args;
        va_start(args, fmt);
        int prefix_len = snprintf(report, sizeof(report), "FAIL app-model: ");
        if (prefix_len > 0 && (size_t)prefix_len < sizeof(report)) {
            vsnprintf(report + prefix_len, sizeof(report) - (size_t)prefix_len, fmt, args);
        }
        va_end(args);
    }
    failure_count += 1;
}

static void record_allocator_failure(const char *fmt, ...) {
    allocator_error_count += 1;
    if (failure_count == 0) {
        va_list args;
        va_start(args, fmt);
        int prefix_len = snprintf(report, sizeof(report), "FAIL app-model: allocator: ");
        if (prefix_len > 0 && (size_t)prefix_len < sizeof(report)) {
            vsnprintf(report + prefix_len, sizeof(report) - (size_t)prefix_len, fmt, args);
        }
        va_end(args);
    }
    failure_count += 1;
}

static int is_power_of_two(size_t value) {
    return value != 0 && (value & (value - 1)) == 0;
}

static uintptr_t align_forward(uintptr_t value, size_t alignment) {
    return (value + alignment - 1u) & ~(uintptr_t)(alignment - 1u);
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
        record_allocator_failure("invalid alignment %zu", alignment);
        return NULL;
    }
    if (length > SIZE_MAX - CANARY_SIZE - CANARY_SIZE - alignment) {
        record_allocator_failure("allocation size overflow length=%zu alignment=%zu", length, alignment);
        return NULL;
    }

    const size_t total = CANARY_SIZE + alignment - 1u + length + CANARY_SIZE;
    uint8_t *raw = malloc(total == 0 ? 1 : total);
    if (raw == NULL) {
        record_allocator_failure("malloc failed length=%zu alignment=%zu", length, alignment);
        return NULL;
    }

    uint8_t *user = (uint8_t *)align_forward((uintptr_t)(raw + CANARY_SIZE), alignment);
    if (((uintptr_t)user % alignment) != 0) {
        free(raw);
        record_allocator_failure("returned pointer is not aligned to %zu", alignment);
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
        free(raw);
        record_allocator_failure("allocation table exhausted");
        return NULL;
    }

    memset(user - CANARY_SIZE, CANARY_BYTE, CANARY_SIZE);
    memset(user, POISON_BYTE, length);
    memset(user + length, CANARY_BYTE, CANARY_SIZE);

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
        record_allocator_failure("unknown or double free for %p", ptr);
        return;
    }
    if (allocation->alignment != alignment) {
        record_allocator_failure("dealloc alignment mismatch allocated=%zu freed=%zu", allocation->alignment, alignment);
    }
    (void)check_canaries(allocation);
    memset(allocation->user, 0xDD, allocation->length);
    free(allocation->raw);
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
        record_allocator_failure("realloc unknown pointer %p", ptr);
        return NULL;
    }
    if (old->alignment != alignment) {
        record_allocator_failure("realloc alignment mismatch allocated=%zu requested=%zu", old->alignment, alignment);
        return NULL;
    }
    if (check_canaries(old) != 0) {
        return NULL;
    }

    const size_t old_length = old->length;
    const size_t copy_length = old_length < new_length ? old_length : new_length;
    void *new_ptr = contract_alloc(new_length, alignment);
    if (new_ptr == NULL) {
        return NULL;
    }
    memcpy(new_ptr, ptr, copy_length);
    if (memcmp(new_ptr, ptr, copy_length) != 0) {
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

enum {
#if UINTPTR_MAX == UINT64_MAX
    MSG_TAG_OFFSET = 24,
    VIEW_TITLE_OFFSET = 0,
    VIEW_MODEL_OFFSET = 24,
    VIEW_MESSAGES_OFFSET = 32,
    VIEW_LIFECYCLE_TAG_OFFSET = 88,
#else
    MSG_TAG_OFFSET = 12,
    VIEW_TITLE_OFFSET = 0,
    VIEW_MODEL_OFFSET = 12,
    VIEW_MESSAGES_OFFSET = 16,
    VIEW_LIFECYCLE_TAG_OFFSET = 44,
#endif
    MSG_RESET_TAG = 3,
    LIFECYCLE_READY_TAG = 1,
};

static Msg reset_msg(void) {
    Msg msg = {{0}};
    msg.bytes[MSG_TAG_OFFSET] = MSG_RESET_TAG;
    return msg;
}

static void roc_box_decref(RocBox box) {
    if (box == NULL) {
        return;
    }
    uint8_t *data = (uint8_t *)box;
    intptr_t *rc = (intptr_t *)(data - sizeof(intptr_t));
    if (*rc == 0) {
        return;
    }
    *rc -= 1;
    if (*rc == 0) {
        roc_dealloc(data - sizeof(size_t), ROC_ALIGNOF(size_t));
    }
}

static RocList *view_messages(View *view) {
    return (RocList *)(void *)(view->bytes + VIEW_MESSAGES_OFFSET);
}

static RocBox *view_model(View *view) {
    return (RocBox *)(void *)(view->bytes + VIEW_MODEL_OFFSET);
}

static RocStr *view_title(View *view) {
    return (RocStr *)(void *)(view->bytes + VIEW_TITLE_OFFSET);
}

static void view_decref(View view) {
    RocStr *title = view_title(&view);
    roc_str_decref(*title);
    roc_box_decref(*view_model(&view));
}

static void run_app_model_contract(void) {
    RocBox initial = roc_init();
    RocBox updated = roc_update(initial, reset_msg());
    View view = roc_render(updated);

    RocStr *title = view_title(&view);
    const char *expected_title = "ready";
    const size_t expected_len = strlen(expected_title);
    if (roc_str_len(title) != expected_len || memcmp(roc_str_bytes(title), expected_title, expected_len) != 0) {
        record_failure("render title mismatch");
    }
    if (view.bytes[VIEW_LIFECYCLE_TAG_OFFSET] != LIFECYCLE_READY_TAG) {
        record_failure("render lifecycle expected Ready got %u", (unsigned)view.bytes[VIEW_LIFECYCLE_TAG_OFFSET]);
    }
    RocList *messages = view_messages(&view);
    if (messages->length != 0) {
        record_failure("render messages expected empty got %zu", messages->length);
    }
    view_decref(view);
}

int main(void) {
    run_app_model_contract();
    if (allocator_error_count != 0) {
        record_failure("allocator recorded %zu errors", allocator_error_count);
    }
    if (live_alloc_count != 0) {
        record_failure("live allocations after scenario: %zu", live_alloc_count);
    }

    if (failure_count != 0) {
        fprintf(stderr, "%s\n", report[0] == '\0' ? "FAIL app-model: unknown failure" : report);
        fprintf(stderr, "alloc_count=%zu dealloc_count=%zu live=%zu allocator_errors=%zu\n", alloc_count, dealloc_count, live_alloc_count, allocator_error_count);
        return 1;
    }

    fprintf(stderr, "PASS glue-runtime app-model CGlue native alloc=%zu dealloc=%zu\n", alloc_count, dealloc_count);
    return 0;
}
