# Concurrency prototype

An executable model of the
[concurrency RFC](../concurrency-explicit-suspension.md): its combinators, its
ownership ledger, the capture-by-unwinding lowering, the task runtime and the
host ABI. It is run against a stackful oracle, under fuzzed programs and
adversarial schedules. None of this is compiler code.

**Completion criterion.** Every row of the RFC's invariant and obligation
tables (G1–G8, C1–C10, R1–R7, P1–P10) has an entry in `coverage.py` naming
how the prototype establishes it and the regression tests that pin it. The
`Coverage` test fails if a row is missing or names a test that does not
exist. `python3 coverage.py` prints the matrix.

The reference-count atomicity side has its own models in
[rc-atomicity/](../rc-atomicity/README.md). `tests.py` runs both.

## Running

```bash
cd design/language/concurrency-prototype && python3 tests.py
```

```bash
cd design/language/concurrency-prototype && python3 check.py 2000 1
```

```bash
cd design/language/concurrency-prototype && python3 bench.py 300
```

```bash
cd design/language/concurrency-prototype/bench && clang -O2 -o unwind unwind.c switch_arm64.S && ./unwind
```

```bash
cd design/language/concurrency-prototype/maude && maude -no-banner -batch wake.maude < /dev/null
```

```bash
cd design/language/concurrency-prototype/bench && clang -O2 -o boxy_records boxy_records.c && ./boxy_records
```

```bash
cd design/language/concurrency-prototype && python3 conformance.py
```

```bash
cd design/language/concurrency-prototype/bench && clang -O1 -g -fsanitize=thread -DPROTOCOL=0 wake_tsan.c -o wake_tsan && ./wake_tsan
```

## What is modeled

| File | RFC section | Contents |
|---|---|---|
| `ir.py` | | An A-normal-form IR whose branches are terminators, so the code after a call is the rest of its block, plus its s-expression syntax. Frame-replacing calls go to a named procedure (`tail`) or through a closure (`tailc`) |
| `combinators.py` | "The `Concurrent` module" | `all!`, `both!`, `all_ok!`, `race!` and `supervise!`, written in the IR over `spawn`, `await` and `cancel` |
| `arc.py` | "What ARC must publish" | Places `dup` and `drop` statements, and publishes the suspension ledger. `certify` checks both by an independent ownership simulation |
| `lower.py` | "Suspendability", "Capture by unwinding", "Calls between suspendable procedures" | Computes suspendability once per procedure and site, under `.lss` (0-CFA lambda sets) and `.boxy` (every erased effectful call is a site), and certifies C2 and C3. Under `.boxy` it also publishes, as `tail_drive` would, which procedures may return with an erased call pending and which call sites hand it up or make it |
| `engines.py` | | Three engines: `NoneEngine` (a `None` build), `StackEngine` (a stackful oracle built on generators) and `UnwindEngine` (the lowering). Under `.boxy`, `UnwindEngine` defers each `tailc` through a pending cell in task storage, and its waiters make the pending calls |
| `runtime.py` | "Host ABI", "Crash mechanics" | The task tree, wake protocol, cancellation and crash propagation, with monitors for R1–R6 and the platform obligations |
| `host.py` | "Host ABI" | Hosted functions, and the `ready`, `pause`, `input_order` and `random` scheduling policies. `random` can complete an operation before the hosted function returns, cancel the root, and, in `ParallelTasks` builds, run other tasks' resumes from inside a hosted call |
| `gen.py`, `check.py` | "What to establish first" | A typed program generator built from small constructs, and the checks run on each generated program |
| `conformance.py` | "Platform obligations" | One host per way of breaking an obligation, each run over scenarios that give it the chance. The runtime's monitors and the heap check must catch every one. Three conforming hosts must pass: a plain one, one whose allocator is confined to a thread, and one that reclaims a crashed root's memory as a region |
| `coverage.py` | "Invariants" | The matrix from every invariant and obligation to the tests that pin it |
| `maude/wake.maude` | "Host ABI" | The wake protocol and hosted completion against cancellation, under real thread interleavings |
| `bench/unwind.c` | "Cost model" | C microbenchmarks of the lowering against plain calls and a stackful switch |
| `bench/boxy_records.c` | "Decisions left to prototypes" | `.boxy` records holding dynamically sized values: inline after a header sized from descriptors, against a separate allocation per value |
| `bench/wake_tsan.c` | "Runtime invariants" | The wake protocols on real threads under ThreadSanitizer: a parent awaiting children on a pool of workers |

The unwinding engine follows the RFC's scheme:
- Each suspension site has a status check after it.
- On the pending path, the site saves the values in its ledger into a record, then returns.
- A paused task is its chain of records, innermost first.
- Resuming runs the innermost record's resume entry and passes each result outward.
- A frame-replacing call has no check and leaves no record.
- A drop entry releases a record's saved values.
- A crash leaks only what the procedures on the native stack own.

## What each generated program is checked for

| Invariant | Check |
|---|---|
| C5 (static) | ARC certification: every use consumes an owned reference, nothing is left owned at a terminator, and each ledger equals what is owned after its statement |
| C2, C3 | A `never` procedure has no site; at run time, a pending flag after a non-site call is an error. Every site's ledger covers what its continuation uses |
| C4 | Records hold only values, and a record that would hold a pending cell outside task storage is an error |
| C7 | Generated tail loops include cycles through closures, which `.boxy` defers. Pinned tests show the record chain stays constant over 200 paused iterations |
| C1, C10 | The unwinding and stackful engines, under the same host and schedule, give identical results, effect traces and heap operations |
| C8 | `.lss` and `.boxy` give identical results, traces and heap operations |
| C9, G3 | A `None` build equals a `Tasks` build under the input-order schedule, unless a supervised child crashes |
| G4 | Programs without `race!`, `all_ok!` or crashes give one result, and the same per-task effects, under every schedule |
| C5 (dynamic) | Cancelling the root at each suspension in turn leaves the heap exact |
| R1–R6 | Monitored in every run, then checked when the run ends. Readiness happens only from parked; a task is never re-entered; no Roc code runs after cancellation or a crash; trees finish bottom-up; no run deadlocks |
| R7 | Monitored at every `supervise!` call in every run: a child is restarted only after it crashed, at most `max_restarts` times, and `supervise!` either returns its last child's result or crashes with its last child's message after exactly `max_restarts` restarts |
| R5 | After every run the heap is verified exactly. Every live object is held by a leaked native-frame value, and its count equals its references |

Six schedules are used per program:
- `ready`
- `pause`
- `input_order`
- `random`
- `random` in a `ParallelTasks` build
- `random` in a `ParallelTasks` build, with root cancellation

Each runs under `.lss` and `.boxy`, with both engines.

The platform obligations are checked separately, by `conformance.py`:

| Obligation | Broken hosts, all caught |
|---|---|
| P1 | resumes a task twice for one ready; resumes a parked task nothing readied |
| P3 | wakes twice for one result; wakes before writing `*out`; writes `*out` twice |
| P4 | writes `*out` after `roc_task_abort`; wakes after it |
| P5 | releases hosted arguments twice; never releases them; never releases an aborted operation's |
| P6 | runs tasks on several threads with an allocator that can release only on the allocating thread |
| P7 | never completes a pending operation (caught as R6) |
| P8 | frees a root twice; reads a crash message of a root that did not crash |
| P9 | reclaims a crashed root's memory while an aborted operation still holds its arguments |
| P10 | resumes another task inside a hosted call in a `Tasks` build |

P6 and P9 are modeled through the heap, which records each object's allocating task and thread:
- A resume nested inside another task's hosted call runs on another thread.
- A confined allocator raises when an object is released on a thread other than its own. The conforming confined host keeps every task on one thread.
- The conforming region host keeps aborted operations' arguments until those operations finish, as P5 allows. It then frees everything the crashed tree allocated, in bulk and without reading counts. That reclaims the crash leaks too: a crash below a caller that holds a value leaks one object under the plain host and none under the region host.

P2 needs real threads, and is tested under ThreadSanitizer below.

## Results

**Fuzzing: `check.py 2000 1`.** Of 2,000 generated programs, none violated any check. The run covered:
- 62,296 runs;
- 14,296 runs that cancelled the root at a suspension;
- 4,752 crashes;
- 22,218 deferred erased calls;
- 52,254 `supervise!` returns and 178 give-ups, each checked against R7;
- 1,488 leaked objects, each traced to a crashed task's native frame.

Each combinator, closure calls and frame-replacing calls through closures appeared in more than 1,000 programs. Tail cycles through a closure appeared in 529 and crashes in 305. The run takes about 80 seconds.

**Finding 1: the RFC's wake protocol is unsound.** The RFC describes the wake state as "running, parked, woken while running, or queued", where a wake that arrives during a run makes the next park continue instead. A wake is stale if its report has already been consumed, and the flag cannot tell. The task then continues, or is resumed, with nothing to deliver.
- The fuzzer found this within its first 30 programs. A parent cancels a parked child, the child's report arrives at once, and the parent reads it synchronously.
- The Maude model finds it with no cancellation at all, in a plain `all!` over two children on two threads.
- Clearing the flag at each wait does not fix it. Maude finds a stale wake from a report that an earlier wait consumed.
- What works is a counted wake word. A completion publishes its data and then increments the word in one atomic step, readying the task only if it was parked. A wait decrements the word. Parking is a compare-and-swap that succeeds only when the count is zero.
- Maude verifies the counted protocol over every interleaving: no stale delivery, no lost wake, no resume of a task that is not queued, and at most one ready per park.

**Finding 2: the host must wake inside the abort lock.** Maude's `HOSTOP` model races a hosted completion against cancellation.
- If the host wakes the task while holding the operation's lock (the lock `roc_task_abort` takes), everything is released exactly once.
- If the host wakes after unlocking, it can wake a freed task.

Obligation P4 already forbids a wake after `roc_task_abort` returns, so P4 implies this rule, and host authors should be told so.

**Finding 3: only running native frames leak on a crash.** The pinned test shows two cases:
- A procedure paused before a crash is a record, and its record is dropped exactly: nothing leaks.
- The same procedure on the native stack leaks its live values.

A stackful runtime leaks every frame of the crashed task.

**Finding 4: deferred erased calls pass through suspension with two rules.**
- A waiter that makes pending calls treats each call it makes as part of its own call site. If that call pauses, the waiter saves a record at the site, and the site's resume entry first goes on making pending calls.
- A site that hands a pending call up treats the pending result as an ordinary value. Its resume entry returns it outward until a waiter makes the call.
- A record would otherwise hold the address of a pending cell in a native frame, so C4 requires the cells to live in task storage. The mutation test shows that the check catches a cell kept in a frame.
- A paused cycle of deferred calls holds records only for the waiter and the procedure that paused: one fewer than `.lss`, whose direct tail call keeps the intermediate frame's record.

**Finding 5: the counted wake word orders every resume after a wake.**

`bench/wake_tsan.c` runs a parent awaiting four children per round on four worker threads, under ThreadSanitizer, with a lock-free run queue so that each handoff is ordered only by its own atomics:

| Host's handoff | Runtime's resume | Result |
|---|---|---|
| ordered | counted wake word | race-free; 36,093 parks, each readied once |
| relaxed (breaks P2) | counted wake word | races only on a child's first run |
| relaxed | relaxed too | races on the woken parent's state as well |
| ordered | relaxed | race-free |

Both flag protocols break R1 on real threads within a few dozen parks, as Maude predicts. P2 is therefore what orders a task's first resume after it is spawned, and the host's own data. The wake word orders every later resume.

**Lowering costs: `bench.py 300`.** These are counts, not timings, over 300 generated programs with every suspending call paused:

| Measure | `.lss` | `.boxy` |
|---|---|---|
| Procedures that may suspend | 4,911 of 6,475 | 4,932 of 6,475 |
| Suspension sites | 7,055 | 7,158 |
| Status checks executed | 53,345 | 53,534 (+0.4%) |
| Records | 49,403 | 49,564 |
| Values saved per record | 1.76 | 1.77 |
| Longest record chain | 8 | 8 |
| Frame-replacing calls through closures | 620 direct | 620 deferred, each made by a waiter |

When nothing pauses, records appear only where combinators wait on children.

**Timings: `bench/unwind.c`.** These were measured on an Apple M5 Pro with clang 21 at `-O2`, taking the median of 15 trials. Run-to-run variation is about 10%.

| Measure | Result |
|---|---|
| Status check after each call that does not pause, in a kernel made only of calls | +2.5% to +5% |

Each pause and resume at depth D, with two live values per frame, in ns per cycle:

| D | Plain calls | Records from `malloc` | Records from a per-task buffer | Records from a free list | Stackful switch |
|---|---|---|---|---|---|
| 1 | 1.7–1.9 | 25–27 | 4.7–5.2 | 4.8–5.1 | 21–23 |
| 4 | 3.0 | 59–63 | 12–13 | 12–14 | 25–29 |
| 16 | 10–12 | 197–239 | 37–47 | 40–47 | 36–44 |

- Records from a per-task buffer or a free list are about five times cheaper than `malloc`, which settles that row of the RFC's decisions table in favour of buffers.
- Unwinding is cheaper than a stackful switch up to about 16 paused frames, and about equal at 16.
- The stackful numbers do not include allocating each task's stack.

**Timings: `bench/boxy_records.c`.** A `.boxy` procedure keeps its dynamic locals in a dynamic frame region on the native stack. A record may not point into that region (C4), so pausing copies each saved local's bytes out, and the resume entry copies them into its fresh region. In this benchmark, every procedure in a chain of depth D holds two dynamic locals of S bytes across its call. Measured on the same machine at `-O2`, with medians of 15 trials and three runs, in ns per pause and resume:

| D | S | No pause | Inline, buffer | Separate, buffer | Inline, `malloc` | Separate, `malloc` |
|---|---|---|---|---|---|---|
| 1 | 8 | 10–23 | 22–48 | 30–60 | 42–74 | 89–156 |
| 1 | 512 | 32–36 | 101–114 | 107–121 | 126–161 | 222–276 |
| 4 | 64 | 37–47 | 89–113 | 109–160 | 155–226 | 336–474 |
| 16 | 64 | 109–200 | 259–445 | 350–479 | 544–607 | 1,124–1,519 |
| 16 | 512 | 306–445 | 901–1,313 | 940–1,371 | 1,173–1,726 | 1,931–2,916 |

Run-to-run variation is up to 1.5×, but in every row of every run the order is the same:
- Inline records from a per-task buffer are the cheapest.
- Separate allocations from the same buffer cost 5% to 35% more. The gap shrinks as values grow and copying dominates.
- With `malloc`, a separate allocation per value roughly doubles the cost of an inline record.

So `.boxy` records should hold dynamic values inline after a header sized from the descriptors, in the per-task buffer. An inline record's size varies, which suits a bump buffer; a free list would need size classes.
