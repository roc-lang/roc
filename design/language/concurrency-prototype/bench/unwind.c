// Microbenchmarks for the RFC's cost model ("Cost model", "Making it fast",
// and the decision "Records from roc_alloc, or from per-task buffers and
// per-thread free lists").
//
// Hand-written C that follows the lowering exactly: a status check after each
// suspendable call; on the pending path, a record holding the live values,
// the call site and its resume entry; resumes innermost first through
// indirect calls to resume entries. Compared against plain calls and against
// a stackful coroutine on its own stack.
//
// Build and run (optimized):
//   clang -O2 -o unwind unwind.c switch_arm64.S && ./unwind
//
// On wasm (no stackful baseline: wasm has no stack switching without the
// stack-switching proposal), with zig's clang and node's WASI:
//   zig cc -target wasm32-wasi -O2 -o unwind.wasm unwind.c && node run_wasi.mjs unwind.wasm

#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#define NOINLINE __attribute__((noinline))

typedef struct Rec Rec;
typedef struct Task Task;
typedef long (*Entry)(Task *, Rec *, long);
struct Rec { Rec *next; Entry entry; long a, b; int site; };

enum { ALLOC_MALLOC, ALLOC_BUMP, ALLOC_FREELIST };

struct Task {
    int pending;
    Rec *head, *tail;      // records, innermost first
    int alloc;
    char *bump, *bump_base;
    Rec *free;             // per-thread free list (one thread here)
};

static Task task_storage;
static Task *volatile task_ptr = &task_storage;

static inline Rec *rec_alloc(Task *t) {
    switch (t->alloc) {
    case ALLOC_MALLOC: return malloc(sizeof(Rec));
    case ALLOC_BUMP: { Rec *r = (Rec *)t->bump; t->bump += sizeof(Rec); return r; }
    default:
        if (t->free) { Rec *r = t->free; t->free = r->next; return r; }
        return malloc(sizeof(Rec));
    }
}

static inline void rec_free(Task *t, Rec *r) {
    switch (t->alloc) {
    case ALLOC_MALLOC: free(r); break;
    case ALLOC_BUMP: break;  // the buffer is reset when the chain is consumed
    default: r->next = t->free; t->free = r;
    }
}

static inline void rec_push(Task *t, Rec *r) {
    r->next = NULL;
    if (t->tail) t->tail->next = r; else t->head = r;
    t->tail = r;
}

// ---------------------------------------------------------------- hosts
NOINLINE long host_ready(Task *t, long x) { return x & 1; }           // ROC_READY
NOINLINE long host_pause(Task *t, long x) { t->pending = 1; return 0; } // ROC_PENDING

// ------------------------------------------- 1. cost of a call that does not pause
NOINLINE long fib_plain(Task *t, int n) {
    if (n < 2) return host_ready(t, n);
    return fib_plain(t, n - 1) + fib_plain(t, n - 2);
}

NOINLINE long fib_resume(Task *t, Rec *r, long v);

NOINLINE long fib_checked(Task *t, int n) {
    if (n < 2) return host_ready(t, n);
    long a = fib_checked(t, n - 1);
    if (__builtin_expect(t->pending, 0)) {
        Rec *r = rec_alloc(t); r->entry = fib_resume; r->site = 1; r->a = n; rec_push(t, r); return 0;
    }
    long b = fib_checked(t, n - 2);
    if (__builtin_expect(t->pending, 0)) {
        Rec *r = rec_alloc(t); r->entry = fib_resume; r->site = 2; r->a = a; rec_push(t, r); return 0;
    }
    return a + b;
}

NOINLINE long fib_resume(Task *t, Rec *r, long v) { return v; }  // never reached here

// ------------------------------------------- 2. pause and resume at depth D
// f(d, a) keeps two live values (a, b) across its call, as a typical
// effectful procedure keeps a few.
NOINLINE long f_resume(Task *t, Rec *r, long v);

NOINLINE long f_unwind(Task *t, int d, long a) {
    long b = a * 3 + d;
    long r = d == 0 ? host_pause(t, a) : f_unwind(t, d - 1, a + 1);
    if (__builtin_expect(t->pending, 0)) {
        Rec *rec = rec_alloc(t);
        rec->entry = f_resume; rec->site = d; rec->a = a; rec->b = b;
        rec_push(t, rec);
        return 0;
    }
    return a + b + r;
}

NOINLINE long f_resume(Task *t, Rec *rec, long v) {
    long a = rec->a, b = rec->b;
    rec_free(t, rec);
    return a + b + v;
}

static long resume_chain(Task *t, long v) {
    Rec *r = t->head;
    t->head = t->tail = NULL;
    t->pending = 0;
    while (r) {
        Rec *next = r->next;
        v = r->entry(t, r, v);
        r = next;
    }
    if (t->alloc == ALLOC_BUMP) t->bump = t->bump_base;
    return v;
}

// Plain calls of the same shape, with a leaf that completes at once.
NOINLINE long f_plain(Task *t, int d, long a) {
    long b = a * 3 + d;
    long r = d == 0 ? host_ready(t, a) : f_plain(t, d - 1, a + 1);
    return a + b + r;
}

// ------------------------------------------- stackful baseline
#ifndef __wasm__
extern void ctx_switch(void **save, void *to);
extern void ctx_trampoline(void);
static void *main_sp, *co_sp;
static int co_depth;
static long co_sink;

NOINLINE long f_stackful(int d, long a) {
    long b = a * 3 + d;
    long r;
    if (d == 0) { ctx_switch(&co_sp, main_sp); r = a & 1; }  // park; resumed later
    else r = f_stackful(d - 1, a + 1);
    return a + b + r;
}

static void co_main(void *arg) {
    for (;;) co_sink += f_stackful(co_depth, 1);
}

static void *new_stack(size_t size, void (*fn)(void *), void *arg) {
    char *base = malloc(size);
    uint64_t *top = (uint64_t *)(((uintptr_t)(base + size)) & ~(uintptr_t)15);
    uint64_t *frame = top - 20;  // 160 bytes, the layout ctx_switch restores
    memset(frame, 0, 160);
    frame[0] = (uint64_t)arg;           // x19
    frame[1] = (uint64_t)fn;            // x20
    frame[11] = (uint64_t)ctx_trampoline; // x30
    return frame;
}
#endif

// ---------------------------------------------------------------- timing
static double now(void) {
    struct timespec ts;
#ifdef __wasm__
    clock_gettime(CLOCK_MONOTONIC, &ts);
#else
    clock_gettime(CLOCK_MONOTONIC_RAW, &ts);
#endif
    return ts.tv_sec * 1e9 + ts.tv_nsec;
}

static int cmp(const void *a, const void *b) {
    double x = *(const double *)a, y = *(const double *)b;
    return (x > y) - (x < y);
}

#define TRIALS 15
static long sink;

static double median(double *v) { qsort(v, TRIALS, sizeof *v, cmp); return v[TRIALS / 2]; }

int main(int argc, char **argv) {
    int scale = argc > 1 ? atoi(argv[1]) : 1;
    Task *t = task_ptr;
    t->bump_base = t->bump = malloc(1 << 20);
    double v[TRIALS];

    // 1. calls that do not pause
    int n = 27;
    double plain, checked;
    for (int k = 0; k < TRIALS; k++) { double s = now(); sink += fib_plain(t, n); v[k] = now() - s; }
    plain = median(v);
    for (int k = 0; k < TRIALS; k++) { double s = now(); sink += fib_checked(t, n); v[k] = now() - s; }
    checked = median(v);
    long calls = 0; { long a = 1, b = 1; for (int i = 2; i <= n; i++) { long c = a + b + 1; a = b; b = c; } calls = b; }
    printf("calls that do not pause (fib %d, %ld calls)\n", n, calls);
    printf("  plain        %7.2f ns/call\n", plain / calls);
    printf("  flag test    %7.2f ns/call  (%+.1f%%)\n\n", checked / calls, 100 * (checked - plain) / plain);

    // 2. one pause and resume at depth D
    printf("one pause and resume, depth D, two live values per frame (ns per cycle)\n");
    printf("  %4s %9s %9s %9s %9s %9s\n", "D", "no pause", "malloc", "buffer", "freelist", "stackful");
    int depths[] = {1, 4, 16, 64};
    for (int di = 0; di < 4; di++) {
        int d = depths[di];
        int iters = 200000 * scale / (d < 4 ? 1 : d / 4);
        double res[5];
        for (int k = 0; k < TRIALS; k++) {
            double s = now();
            for (int i = 0; i < iters; i++) sink += f_plain(t, d, i);
            v[k] = (now() - s) / iters;
        }
        res[0] = median(v);
        for (int a = 0; a < 3; a++) {
            t->alloc = a;
            for (int k = 0; k < TRIALS; k++) {
                double s = now();
                for (int i = 0; i < iters; i++) {
                    f_unwind(t, d, i);
                    sink += resume_chain(t, i & 1);
                }
                v[k] = (now() - s) / iters;
            }
            res[1 + a] = median(v);
        }
#ifdef __wasm__
        res[4] = 0.0 / 0.0;
#else
        co_depth = d;
        co_sp = new_stack(1 << 20, co_main, NULL);
        ctx_switch(&main_sp, co_sp);  // run to the first park
        for (int k = 0; k < TRIALS; k++) {
            double s = now();
            for (int i = 0; i < iters; i++) ctx_switch(&main_sp, co_sp);
            v[k] = (now() - s) / iters;
        }
        res[4] = median(v);
#endif
        printf("  %4d %9.1f %9.1f %9.1f %9.1f %9.1f\n", d, res[0], res[1], res[2], res[3], res[4]);
    }
#ifdef __wasm__
    printf("\n(sink %ld)\n", sink & 1);
#else
    printf("\n(sink %ld %ld)\n", sink & 1, co_sink & 1);
#endif
    return 0;
}
