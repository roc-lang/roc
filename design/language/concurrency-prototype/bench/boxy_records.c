// How `.boxy` records hold dynamically sized values (RFC, "Decisions left to
// prototypes"): inline after a descriptor-sized header, or as separate
// allocations.
//
// A `.boxy` procedure keeps its dynamic locals in a dynamic frame region on
// the native stack, sized from descriptors in its prologue (design.md,
// "Dynamic Values In LIR"). A record may not hold addresses into that region
// (C4), so pausing copies each saved dynamic local's bytes out, and the resume
// entry, which builds a fresh region, copies them back.
//
//   inline    one record of header + the saved locals' aligned sizes, read
//             from their descriptors; one allocation per paused procedure
//   separate  a fixed record holding one pointer per saved local, each to its
//             own allocation of the descriptor's size
//
// Each runs with records (and, for `separate`, values) from a per-task bump
// buffer and from malloc. Every procedure in a chain of depth D holds K
// dynamic locals of S bytes across its call; the innermost one pauses; then
// the chain is resumed innermost first. Reported: ns per pause-and-resume.
//
//   clang -O2 -o boxy_records boxy_records.c && ./boxy_records

#include <alloca.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#define NOINLINE __attribute__((noinline))
enum { MAXK = 4 };

typedef struct { size_t size, align; } Desc;  // the part of a descriptor this needs

typedef struct Rec Rec;
struct Rec {
    Rec *next;
    int depth;
    void *sep[MAXK];  // separate: one allocation per saved local
    // inline: the saved locals' bytes follow the header
};

enum { INLINE, SEPARATE };
enum { BUMP, MALLOC };

typedef struct {
    int pending, layout, alloc, k;
    const Desc *desc;  // every local shares one descriptor here
    Rec *head, *tail;  // innermost first
    char *bump, *base;
} Task;

static volatile int sink;

static inline size_t aligned(const Desc *d) { return (d->size + d->align - 1) & ~(d->align - 1); }

static inline void *mem(Task *t, size_t n) {
    if (t->alloc == MALLOC) return malloc(n);
    void *p = t->bump;
    t->bump += (n + 15) & ~(size_t)15;
    return p;
}

static inline void unmem(Task *t, void *p) {
    if (t->alloc == MALLOC) free(p);
}

NOINLINE long host(Task *t, int pause) {
    if (pause) t->pending = 1;
    return 1;
}

// Saves the region's K locals into a record and links it outermost-last.
static void save(Task *t, int depth, char *region) {
    size_t s = aligned(t->desc);
    Rec *r;
    if (t->layout == INLINE) {
        r = mem(t, sizeof(Rec) + s * t->k);
        memcpy((char *)(r + 1), region, s * t->k);
    } else {
        r = mem(t, sizeof(Rec));
        for (int i = 0; i < t->k; i++) {
            r->sep[i] = mem(t, t->desc->size);
            memcpy(r->sep[i], region + s * i, t->desc->size);
        }
    }
    r->depth = depth;
    r->next = NULL;
    // innermost saves first; append so the chain stays innermost first
    if (t->tail) t->tail->next = r;
    else t->head = r;
    t->tail = r;
}

// The continuation after the call: reads every local, as code after a call would.
static inline long use(Task *t, char *region, long v) {
    size_t s = aligned(t->desc);
    for (int i = 0; i < t->k; i++) v += region[s * i] + region[s * i + t->desc->size - 1];
    return v;
}

NOINLINE long proc(Task *t, int depth, int pause) {
    size_t s = aligned(t->desc);
    char *region = alloca(s * t->k);  // the dynamic frame region
    for (int i = 0; i < t->k; i++) memset(region + s * i, depth + i, t->desc->size);
    long v = depth == 0 ? host(t, pause) : proc(t, depth - 1, pause);
    if (__builtin_expect(t->pending, 0)) {
        save(t, depth, region);
        return 0;
    }
    return use(t, region, v);
}

// The resume entry: a fresh region, filled from the record, then the continuation.
NOINLINE long entry(Task *t, Rec *r, long v) {
    size_t s = aligned(t->desc);
    char *region = alloca(s * t->k);
    if (t->layout == INLINE) {
        memcpy(region, (char *)(r + 1), s * t->k);
    } else {
        for (int i = 0; i < t->k; i++) {
            memcpy(region + s * i, r->sep[i], t->desc->size);
            unmem(t, r->sep[i]);
        }
    }
    unmem(t, r);
    return use(t, region, v);
}

static long resume(Task *t) {
    long v = 1;
    t->pending = 0;
    // A real host would also redo the innermost hosted call's result here.
    while (t->head) {
        Rec *r = t->head;
        t->head = r->next;
        v = entry(t, r, v);
    }
    t->tail = NULL;
    t->bump = t->base;  // the chain is consumed: reset the buffer
    return v;
}

static double now(void) {
    struct timespec ts;
    clock_gettime(CLOCK_MONOTONIC, &ts);
    return ts.tv_sec * 1e9 + ts.tv_nsec;
}

static int cmp(const void *a, const void *b) {
    double x = *(const double *)a, y = *(const double *)b;
    return (x > y) - (x < y);
}

static double bench(int layout, int alloc, int depth, int k, size_t size, int pause) {
    Desc d = {size, 8};
    Task t = {0};
    t.layout = layout, t.alloc = alloc, t.k = k, t.desc = &d;
    t.base = t.bump = malloc(1 << 22);
    enum { TRIALS = 15 };
    int iters = 2000000 / (depth + 1) / (size > 64 ? size / 64 : 1);
    double ts[TRIALS];
    for (int tr = 0; tr < TRIALS; tr++) {
        double t0 = now();
        long acc = 0;
        for (int i = 0; i < iters; i++) {
            acc += proc(&t, depth, pause);
            if (t.pending) acc += resume(&t);
        }
        sink = (int)acc;
        ts[tr] = (now() - t0) / iters;
    }
    free(t.base);
    qsort(ts, TRIALS, sizeof ts[0], cmp);
    return ts[TRIALS / 2];
}

int main(void) {
    const int depths[] = {1, 4, 16};
    const size_t sizes[] = {8, 64, 512};
    const int k = 2;
    printf("ns per pause and resume; every frame holds %d dynamic locals of S bytes\n", k);
    printf("%4s %5s %8s %12s %14s %12s %14s\n", "D", "S", "no pause", "inline/buf", "separate/buf", "inline/malloc",
           "separate/malloc");
    for (int di = 0; di < 3; di++)
        for (int si = 0; si < 3; si++) {
            int D = depths[di];
            size_t S = sizes[si];
            printf("%4d %5zu %8.1f %12.1f %14.1f %12.1f %14.1f\n", D, S, bench(INLINE, BUMP, D, k, S, 0),
                   bench(INLINE, BUMP, D, k, S, 1), bench(SEPARATE, BUMP, D, k, S, 1),
                   bench(INLINE, MALLOC, D, k, S, 1), bench(SEPARATE, MALLOC, D, k, S, 1));
        }
    return 0;
}
