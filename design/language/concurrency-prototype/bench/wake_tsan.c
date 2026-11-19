// The wake protocol on real threads, for ThreadSanitizer (RFC, "Runtime
// invariants": "the wake-state machine also run under ThreadSanitizer").
//
// A parent task runs `rounds` rounds of all!: it spawns `kids` children onto a
// pool of worker threads and awaits every report. Children write a plain
// payload, push a report, then wake the parent. The parent runs on whichever
// worker resumes it, so every handoff crosses threads.
//
// Checked by assertion counters:
//   R1  every park is readied exactly once; a task that continues or is
//       resumed always has a report to deliver
//   R2  the parent never runs on two threads at once
// Checked by ThreadSanitizer:
//   publish-before-wake (a report's payload is a plain write the parent reads)
//   P2, handoff ordering (the parent's plain state moves between threads)
//
// Builds (PROTOCOL): 0 counted wake word (the RFC), 1 flag, 2 flag cleared at
// each wait. -DBROKEN_HANDOFF hands the parent between threads with relaxed
// atomics, breaking P2. -DRELAXED_RESUME makes the runtime's own accesses to
// the wake word on the resume path relaxed (counted protocol only), so that
// only the host's handoff can order the parent's state.
//
//   clang -O1 -g -fsanitize=thread -DPROTOCOL=0 wake_tsan.c -o wake_tsan && ./wake_tsan

#include <pthread.h>
#include <sched.h>
#include <stdatomic.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

#ifndef PROTOCOL
#define PROTOCOL 0
#endif

enum { WORKERS = 4, MAX_KIDS = 8 };

static int rounds = 20000, kids = 4;

// ------------------------------------------------------------- violations
static atomic_long violations;
static atomic_flag reported = ATOMIC_FLAG_INIT;

static void violation(const char *what) {
    atomic_fetch_add(&violations, 1);
    if (!atomic_flag_test_and_set(&reported)) fprintf(stderr, "violation: %s\n", what);
}

// ------------------------------------------------------------- reports
typedef struct Report {
    struct Report *next;
    int child;
} Report;

static _Atomic(Report *) inbox;         // Treiber stack: children push
static Report nodes[MAX_KIDS];
static long payload[MAX_KIDS];          // plain: written by the child, read by the parent

static void push_report(int i) {
    Report *n = &nodes[i];
    n->child = i;
    Report *h = atomic_load_explicit(&inbox, memory_order_relaxed);
    do n->next = h;
    while (!atomic_compare_exchange_weak_explicit(&inbox, &h, n, memory_order_release, memory_order_relaxed));
}

// The parent's own state: plain, moved between threads by the handoff.
static struct {
    int round, spawned, remaining;
    long sum, parks;
    Report *local;
} P;

static Report *pop_report(void) {
    if (!P.local) P.local = atomic_exchange_explicit(&inbox, NULL, memory_order_acquire);
    Report *r = P.local;
    if (r) P.local = r->next;
    return r;
}

// ------------------------------------------------------------- run queue
// Lock-free, so that each handoff is ordered only by its own atomics: the
// parent is the only producer; workers claim entries by CAS on the head.
#ifdef BROKEN_HANDOFF
#define PUBLISH memory_order_relaxed
#define CLAIM memory_order_relaxed
#else
#define PUBLISH memory_order_release
#define CLAIM memory_order_acquire
#endif
enum { QSIZE = 1024 };
static int queue[QSIZE];
static atomic_long qhead, qtail;
static atomic_int parent_slot;  // 1: the parent is queued
static atomic_int finished;
static atomic_long readies;
static atomic_int running;      // R2

static void enqueue_child(int i) {
    long t = atomic_load_explicit(&qtail, memory_order_relaxed);
    queue[t % QSIZE] = i;
    atomic_store_explicit(&qtail, t + 1, PUBLISH);
}

static int dequeue_child(void) {
    long h = atomic_load_explicit(&qhead, memory_order_relaxed);
    for (;;) {
        if (h >= atomic_load_explicit(&qtail, CLAIM)) return -1;
        int i = queue[h % QSIZE];
        if (atomic_compare_exchange_weak_explicit(&qhead, &h, h + 1, memory_order_relaxed, memory_order_relaxed))
            return i;
    }
}

static void ready_parent(void) {
    atomic_fetch_add(&readies, 1);
    atomic_store_explicit(&parent_slot, 1, PUBLISH);
}

static int take_parent(void) {
    return atomic_exchange_explicit(&parent_slot, 0, CLAIM);
}

// ------------------------------------------------------------- wake word
static _Atomic uint32_t word;

#ifdef RELAXED_RESUME
#define RESUME_ORDER memory_order_relaxed
#define CONSUME_ORDER memory_order_release
#else
#define RESUME_ORDER memory_order_acquire
#define CONSUME_ORDER memory_order_acq_rel
#endif

#if PROTOCOL == 0
// Counted: a parked bit and the number of completions published but not consumed.
#define PARKED 0x80000000u

static void wake(void) {
    uint32_t old = atomic_load_explicit(&word, memory_order_relaxed), nw;
    do nw = (old & ~PARKED) + 1;
    while (!atomic_compare_exchange_weak_explicit(&word, &old, nw, memory_order_acq_rel, memory_order_relaxed));
    if (old & PARKED) ready_parent();
}

// Returns a report, or NULL after parking.
static Report *await_any(void) {
    for (;;) {
        uint32_t w = atomic_load_explicit(&word, RESUME_ORDER);
        if (w & ~PARKED) {
            atomic_fetch_sub_explicit(&word, 1, CONSUME_ORDER);
            Report *r = pop_report();
            if (!r) violation("R1: a counted completion with no report");
            return r;
        }
        // Once the park succeeds another thread may resume the parent at
        // once, so everything the parent writes happens before it.
        P.parks++;
        atomic_store_explicit(&running, 0, memory_order_release);
        uint32_t zero = 0;
        if (atomic_compare_exchange_strong_explicit(&word, &zero, PARKED, memory_order_acq_rel,
                                                    memory_order_acquire))
            return NULL;
        if (atomic_exchange(&running, 1)) violation("R2: re-entered");
        P.parks--;
    }
}

static Report *on_resume(void) { return await_any(); }

#else
// Flag: running, parked, woken while running, queued.
enum { RUNNING, PARKED_S, WOKEN, QUEUED };

static void wake(void) {
    uint32_t s = atomic_load_explicit(&word, memory_order_acquire);
    for (;;) {
        if (s == PARKED_S) {
            if (atomic_compare_exchange_weak_explicit(&word, &s, QUEUED, memory_order_acq_rel, memory_order_acquire)) {
                ready_parent();
                return;
            }
        } else if (s == RUNNING) {
            if (atomic_compare_exchange_weak_explicit(&word, &s, WOKEN, memory_order_acq_rel, memory_order_acquire))
                return;
        } else {
            return;  // already recorded
        }
    }
}

static Report *await_any(void) {
#if PROTOCOL == 2
    uint32_t woken = WOKEN;
    atomic_compare_exchange_strong(&word, &woken, RUNNING);
#endif
    Report *r = pop_report();
    if (r) return r;
    P.parks++;
    atomic_store_explicit(&running, 0, memory_order_release);
    uint32_t s = RUNNING;
    if (atomic_compare_exchange_strong_explicit(&word, &s, PARKED_S, memory_order_acq_rel, memory_order_acquire))
        return NULL;
    // Woken while running: continue, delivering a report.
    if (atomic_exchange(&running, 1)) violation("R2: re-entered");
    P.parks--;
    atomic_store(&word, RUNNING);
    r = pop_report();
    if (!r) violation("R1: continued with nothing to deliver");
    return r;
}

static Report *on_resume(void) {
    atomic_store(&word, RUNNING);
    Report *r = pop_report();
    if (!r) violation("R1: resumed with nothing to deliver");
    return r;
}
#endif

// ------------------------------------------------------------- tasks
static void run_child(int i) {
    payload[i] = (long)P.round * 100 + i;  // P.round is stable while children run
    push_report(i);
    wake();
}

static void deliver(Report *r) {
    if (payload[r->child] != (long)P.round * 100 + r->child) violation("stale payload");
    P.sum += payload[r->child];
    P.remaining--;
}

// Runs the parent until it parks or finishes.
static void resume_parent(int first) {
#ifdef RELAXED_RESUME
    if (atomic_exchange_explicit(&running, 1, memory_order_relaxed)) violation("R2: resumed while running");
#else
    if (atomic_exchange(&running, 1)) violation("R2: resumed while running");
#endif
    Report *r = first ? NULL : on_resume();
    if (r) deliver(r);
    else if (!first && atomic_load(&violations) == 0) violation("R1: resumed with nothing to deliver");
    for (;;) {
        if (P.remaining == 0) {
            if (P.round == rounds) {
                atomic_store(&running, 0);
                atomic_store(&finished, 1);
                return;
            }
            P.round++;
            P.remaining = kids;
            for (int i = 0; i < kids; i++) enqueue_child(i);
        }
        r = await_any();
        if (!r) {
            if (atomic_load(&violations)) {  // a lost report would hang the run
                atomic_store(&finished, 1);
            }
            return;  // parked; another thread resumes us
        }
        deliver(r);
    }
}

static void *worker(void *arg) {
    (void)arg;
    while (!atomic_load(&finished)) {
        if (take_parent()) {
            resume_parent(0);
            continue;
        }
        int i = dequeue_child();
        if (i >= 0) run_child(i);
        else sched_yield();
    }
    return NULL;
}

int main(int argc, char **argv) {
    if (argc > 1) rounds = atoi(argv[1]);
    if (argc > 2) kids = atoi(argv[2]);
    P.round = 0;
    P.remaining = 0;
    resume_parent(1);
    pthread_t th[WORKERS];
    for (int i = 0; i < WORKERS; i++) pthread_create(&th[i], NULL, worker, NULL);
    for (int i = 0; i < WORKERS; i++) pthread_join(th[i], NULL);
    long want = 0;
    for (int r = 1; r <= rounds; r++)
        for (int i = 0; i < kids; i++) want += (long)r * 100 + i;
    long v = atomic_load(&violations);
    if (!v && P.sum != want) violation("wrong sum");
    if (!v && atomic_load(&readies) != P.parks) violation("R1: readies != parks");
    printf("protocol %d%s: rounds %d, kids %d, parks %ld, readies %ld, violations %ld\n", PROTOCOL,
#ifdef BROKEN_HANDOFF
           " (broken handoff)",
#else
           "",
#endif
           rounds, kids, P.parks, atomic_load(&readies), atomic_load(&violations));
    return atomic_load(&violations) ? 1 : 0;
}
