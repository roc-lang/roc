# Concurrency prototype

An executable model of the
[concurrency RFC](../concurrency-explicit-suspension.md): its combinators, its
ownership ledger, the capture-by-unwinding lowering, the task runtime and the
host ABI. It also models the [sessions RFC](../concurrency-sessions.md),
which the shared state RFC has superseded: channels, mailboxes, the semaphore
and pool libraries, and services that span many roots. Those results (S1–S10,
`sessions.py`, the channel tests and the findings about channels and server
tasks) remain as evidence for the superseded design; the shared state RFC's
queues and replies are built on `Shared` instead. And it models the [shared state RFC](../concurrency-shared-variables.md):
`Shared(s)` handles updated atomically by pure functions (`update!`), waiting,
cleanup that runs at cancellation and crashes (`finally!`, `on_failure!`), and
poisoning, with the semaphore, pool and circuit breaker rewritten over it. It is run against a stackful oracle, under
fuzzed programs and adversarial schedules. None of this is compiler code.

**Completion criterion.** Every row of the RFC's invariant and obligation
tables (G1–G9, C1–C11, R1–R8, P1–P10), of the sessions RFC's (S1–S10) and of
the shared state RFC's (V1–V9) has an entry in `coverage.py` naming how the
prototype establishes it and the regression tests that pin it. The `Coverage`
test fails if a row is missing or names a test that does not exist.
`python3 coverage.py` prints the matrix. Every row is established; none is
pending. V9, that every cell is freed, is established by `check.py` with
`links`, whose generated states hold handles and jobs and form cycles.

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
cd design/language/concurrency-prototype/bench && zig cc -target wasm32-wasi -O2 -o unwind.wasm unwind.c && node run_wasi.mjs unwind.wasm
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
cd design/language/concurrency-prototype && python3 shared_model.py
```

```bash
cd design/language/concurrency-prototype && python3 check.py 2000 1 links
```

```bash
cd design/language/concurrency-prototype && python3 cycles.py 100000 7
```

```bash
cd design/language/concurrency-prototype && python3 bench/shared_vs_actor.py
```

```bash
cd design/language/concurrency-prototype/bench && clang -O1 -g -fsanitize=thread -DPROTOCOL=0 wake_tsan.c -o wake_tsan && ./wake_tsan
```

## What is modeled

| File | RFC section | Contents |
|---|---|---|
| `ir.py` | | An A-normal-form IR whose branches are terminators, so the code after a call is the rest of its block, plus its s-expression syntax. Frame-replacing calls go to a named procedure (`tail`) or through a closure (`tailc`) |
| `plans.py` | "Concurrent loops" | Plans as inert values (`Each`, `Cat`, `Limit`, `Shield`, `Background`), their flattening into entries in input order, and the limit at a plan's root, against which spawned entries count |
| `combinators.py` | "The `Concurrent` module" | `plan.all!()` and `plan.all_ok!()` (`plan_all!`, `plan_all_ok!`), which wrap each entry that is not background with its index; `all!`, `both!`, `all_ok!`, `race!` and `supervise!`, written in the IR over concurrent loops, with `all!(fs)` as `fs.concurrently().all!()` |
| `arc.py` | "What ARC must publish" | Places `dup` and `drop` statements, and publishes the suspension ledger. `certify` checks both by an independent ownership simulation |
| `lower.py` | "Suspendability", "Capture by unwinding", "Calls between suspendable procedures" | Computes suspendability once per procedure and site, under `.lss` (0-CFA lambda sets) and `.boxy` (every erased effectful call is a site), and certifies C2 and C3. Under `.boxy` it also publishes, as `tail_drive` would, which procedures may return with an erased call pending and which call sites hand it up or make it |
| `engines.py` | | Three engines: `NoneEngine` (a `None` build), `StackEngine` (a stackful oracle built on generators) and `UnwindEngine` (the lowering). Under `.boxy`, `UnwindEngine` defers each `tailc` through a pending cell in task storage, and its waiters make the pending calls |
| `runtime.py` | "Host ABI", "Crash mechanics" | The task tree, wake protocol, cancellation and crash propagation, with monitors for R1–R6 and the platform obligations |
| `host.py` | "Host ABI" | Hosted functions, and the `ready`, `pause`, `input_order` and `random` scheduling policies. `random` can complete an operation before the hosted function returns, cancel the root, and, in `ParallelTasks` builds, run other tasks' resumes from inside a hosted call. A host can keep an aborted operation's arguments until the operation finishes. `ServiceHost` runs a service root beside many request roots, with deadlock detection per root |
| `sessions.py` | Sessions RFC (superseded) | Channels, mailboxes and semaphore cells; ports (ends, `Post`s, `Inbox`es, permits) and what their release does; holder sets, and releasing a port no live task can use; releasing a `Shared` cell drops its state |
| `library.py` | Sessions RFC (superseded) and shared state RFC, "Library patterns" | The semaphore and the pool as server tasks, written in the IR as a library author would write them in Roc; the semaphore cell experiment; and the semaphore, pool and circuit breaker over `Shared` handles |
| `shared.py` | Shared state RFC | `Shared` cells: atomic updates by a pure function's result (`Set`, `Keep` or `Wait`), waiters, cleanup (`finally!`, which always runs, and `on_failure!`, which runs only on cancellation or a crash), and poisoning. The IR spells these as `shared`, `step`, `defer`, `errdefer` and `undefer`; the RFC's "What the prototype shows" gives the mapping. A pure evaluator with exact ownership runs the update functions. Each step moves its cells' states into the update form of its function, which `update_forms` makes before ARC by rewriting tails so that `Keep` and `Wait` return the states; probes run the function as written. `certify_defers` checks statically that shared operations appear only in effectful procedures, that steps call pure ones, and that every cleanup is matched on every path |
| `cycles.py` | Shared state RFC, "Rationale and alternatives" | The rejected type rules against cycles, on generated, possibly recursive and generic type declarations, decided three ways: with lambda sets, by the checker's rules on expanded types, and per declaration with summaries and requirements. Checked against a heap that counts references, with a leaking witness for every rejected type and two mutations it must catch |
| `shared_model.py` | Shared state RFC, "Runtime" | The `ParallelTasks` lock protocol for atomic updates, explored over every interleaving of locks, reads, writes, wakes and parks, with three broken protocols it must catch |
| `pipeline.py` | | Builds a program through ARC and both lowerings, with every static check |
| `gen.py`, `check.py` | "What to establish first" | A typed program generator built from small constructs, and the checks run on each generated program. With `links`, every program uses shared state and most make cells whose states hold handles and jobs, forming cycles the runtime must release (V9) |
| `conformance.py` | "Platform obligations" | One host per way of breaking an obligation, each run over scenarios that give it the chance. The runtime's monitors and the heap check must catch every one. Three conforming hosts must pass: a plain one, one whose allocator is confined to a thread, and one that reclaims a crashed root's memory as a region |
| `coverage.py` | "Invariants" | The matrix from every invariant and obligation to the tests that pin it |
| `maude/wake.maude` | "Host ABI" | The wake protocol and hosted completion against cancellation, under real thread interleavings |
| `bench/unwind.c` | "Cost model" | C microbenchmarks of the lowering against plain calls and a stackful switch; also builds for wasm, without the stackful switch |
| `bench/run_wasi.mjs` | | Runs a WASI program under Node |
| `bench/boxy_records.c` | "Decisions left to prototypes" | `.boxy` records holding dynamically sized values: inline after a header sized from descriptors, against a separate allocation per value |
| `bench/wake_tsan.c` | "Runtime invariants" | The wake protocols on real threads under ThreadSanitizer: a parent awaiting children on a pool of workers |
| `bench/shared_vs_actor.py` | Shared state RFC, "What the prototype shows" | Operation counts for the semaphore and the pool as server tasks, as a semaphore cell and as `Shared` handles, under three schedules |

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
| C8 | `.lss` and `.boxy` give identical results and traces, and identical heap operations in runs without a crash (Finding 16) |
| C9, G3 | A `None` build equals a `Tasks` build under its schedule, unless a supervised child crashes: the coroutine schedule for a program that can wait, and the input-order schedule for one that cannot. For a program that cannot wait, the coroutine `None` build must also give the one-stack results and effects |
| G9 | Monitored in every run: R8's limits, every entry of a loop over a limited plan (spawned ones included) against the limit at its root, and a loop's `Ended` only once every entry that is not background has arrived and every background entry has stopped. The generator composes background marks, limits at a plan's root, spawns and `plan.all!()` / `plan.all_ok!()` |
| G4 | Programs without `race!`, `all_ok!` or crashes give one result, and the same per-task effects, under every schedule |
| C5 (dynamic) | Cancelling the root at each suspension in turn leaves the heap exact |
| R1–R6 | Monitored in every run, then checked when the run ends. Readiness happens only from parked; a task is never re-entered; no Roc code runs after cancellation or a crash; trees finish bottom-up; no run deadlocks |
| R7 | Monitored at every `supervise!` call in every run: a child is restarted only after it crashed, at most `max_restarts` times, and `supervise!` either returns its last child's result or crashes with its last child's message after exactly `max_restarts` restarts |
| R5 | After every run the heap is verified exactly. Every live object is held by a leaked native-frame value, and its count equals its references |
| S1 | A second use of a port crashes the task, and the run goes on |
| S2, S3 | The exact heap counts a message as owned only while its `In` end or `Inbox` exists, and every receive, take and acquire finishes |
| S4 | A port in a crashed frame is released once its holder set is empty, so its peer finishes |
| S6 | A run that would hang is an error; a stalled tree crashes the receiver that waited last |
| S7 | Part of the C9 comparison above, deadlocks included |
| S9 | Every semaphore cell has all its permits back when the run ends |
| V1, V3 | Static: `Shared` operations only in effectful procedures, update and cleanup functions only pure, and every cleanup matched on every path |
| V2 | An update moves its cell's state into its pure function and stores the whole result; an update that crashes stores nothing and poisons the cell, and the exact heap finds the state it held in its crashed frames |
| V5 | Cancelling the root at each suspension in turn runs every pending cleanup once, after the task's descendants have finished; a task that finishes with cleanup pending is an error |
| V6 | An update or a cleanup that crashes poisons its handle, and later updates of it, and its parked waiters, crash with a message naming which |
| V8 | A `None` build, in which a `Wait` pauses its task and the next ready task runs, equals a `Tasks` build under the coroutine schedule |
| V10 | Every `Keep` and `Wait` of an update form puts back the very state it was given, and a waiter that a probe readied does not wait when it runs on the state the probe saw |
| Coroutines | The `None` build as coroutines on one stack, under both engines and both lowerings, which must agree; programs of G4 must give the `None` build's result; deadlocks it avoids or adds are counted |

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
- 63,926 runs, 15,926 more that cancelled the root at a suspension, and 8,000 as coroutine `None` builds;
- 33,512 crashes, 17,112 of them reaching a parent through a loop's link, and 24 reaching a running parent at its next suspension point;
- 52,534 deferred erased calls;
- 100,400 `supervise!` returns and 800 give-ups, each checked against R7;
- 5,472 leaked objects, each traced to a crashed task's native frame;
- 67,734 channels, 35,748 messages, 10,260 `Cancelled` and 7,858 deadlocks crashed;
- 8,532 mailboxes, 25,146 posts and 2,716 `Closed`;
- 8,864 semaphore cells and 23,964 permits, every one back by the end of its run;
- 1,822 ports released after a crash, and 10,560 receives abandoned by cancellation or a crash;
- 1,292 entries a `None` build ran on demand;
- 78,518 `Shared` cells, 164,554 updates, 134,168 stores and 15,568 `Wait`s, after which 3,726 updates ran again;
- 52,498 cleanups: 43,766 run, 6,208 of them while a cancelled or crashed task was dropped, and 8,732 `on_failure!` cleanups dropped unrun when their body returned;
- 730 cleanups that crashed, poisoning 602 cells, and 258 cancellations turned into crashes by them.

C9 compared 1,964 programs; the other 36 crash inside `supervise!`, where a `None` build stops the whole program by design. Each combinator, closure calls and frame-replacing calls through closures appeared in more than 1,000 programs. Tail cycles through a closure appeared in 513, crashes in 667, channels in 823, mailboxes in 317, semaphore cells in 268 and `Shared` handles in 628. The run takes about 220 seconds.

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

**Finding 6: one holder per port is not enough.** A crash leaks the ports in its running frames, so the runtime must release a port once no live task can use it. Recording one holder task per end is wrong once an end is shared between live tasks. `supervise!` shares its closure, and the ends it captured, with each attempt. When the first attempt crashed with an end in a running frame and the restart dropped it unused, the leaked reference kept it alive, and its receiver waited until deadlock detection crashed it. With a set of holders per port, which a task leaves when it finishes, the receiver gets `Cancelled` when the supervisor finishes.

**Finding 7: an abandoned receive must close its channel.** A task cancelled or crashed while waiting at a receive never takes its message. If the channel stays open, a message sent there later, such as a semaphore's grant, is stranded with the permit inside it. The exact heap caught this as a lost permit. Closing the channel drops the message when it arrives, and the permit returns.

**Finding 8: a port is released at its last use, not at the end of its scope.** ARC drops a value after its last use. A semaphore cell's `with_permit` that acquired a permit and never used it again released the permit at once, so `f` ran without it: three holders ran at once where one was allowed. Every library here uses its permit after `f` returns. This is evidence for a lint on a port never used after it is bound.

**Finding 9: a permit passed to a hosted call limits remote work.** With a host that keeps an aborted operation's arguments until the operation finishes, and the caller cancelled while a remote operation runs, two remote operations ran at once under some schedules when the permit stayed in the caller's scope. Passing the permit to the hosted call kept it to one under every schedule.

**Finding 10: a semaphore cell runs on one stack, for counting resources only.** A cell with a wait queue, whose permit returns when it is released, needs no turn-taking between tasks. It matches the input-order schedule, and every permit comes back under cancellation, crashes and fuzzing. It works because returning a permit is a fixed runtime action. A general cell's state change is Roc code, which a cancelled task cannot run (R4).

**Finding 11: replies bound a mailbox without a bounded queue.** Three senders of four messages each to a slow reader queued at most 3 messages when each sender waited for a reply before sending again, and 12 when they did not.

**Finding 12: deadlock detection per root must look at holders in other roots.** With a service root beside request roots, a request waiting on a port the service holds is not stalled. Detection that ignored holders outside the tree crashed requests that would have been served.

**Finding 13: a linked crash can reach a running parent.** In a `ParallelTasks` build an entry can crash while its parent runs. The crash is then pending, and the parent takes it at its next suspension point of any kind, where it suspends without running the operation and drops the arguments it consumed.

**Finding 14: coroutines on one stack run every turn-taking protocol.** (Measured on the superseded sessions RFC's channels; Finding 23 makes coroutines the `None` build of every program that can wait.) A `None` build as coroutines (`CoroutinePolicy`) is the unwinding lowering with hosted calls that never suspend, and a scheduler that resumes the ready task earliest in input order. It runs the semaphore, the pool and the request-reply-release server, which deadlock on one stack, and a real cycle of receives still deadlocks. Over the 2,000 fuzzed programs, it finished 32 of the 250 that deadlock on one stack and added no deadlock. In programs that use ports, 12,738 of 22,088 procedures (58%) may suspend under it, against 16,491 (75%) in a `Tasks` build. The sessions RFC proposes this for `None` builds of programs that use ports.

**Finding 15: supervise units of work, and a lifetime restart limit is enough.** A server supervised as a whole by `supervise!` with a limit of 3 died after four transient crashes far apart in 20 requests, running its handler 50 times. Supervising each request with the same limit served all 20 with 24 handler runs. So `supervise!` needs no time window, and no clock from the platform, in the first version.

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

**Timings: `bench/unwind.c` on wasm.** Built with `zig cc -target wasm32-wasi -O2` (zig 0.17.1) and run under Node v22.22.2 (V8) on the same machine. Wasm has no stack switching without the stack-switching proposal, so there is no stackful column; native numbers from the same session are given for comparison.

| Measure | Wasm | Native, same session |
|---|---|---|
| Status check after each call that does not pause, in a kernel made only of calls | +40% to +58% | within noise |

Each pause and resume at depth D, in ns per cycle:

| D | No pause | Records from a per-task buffer | Records from `malloc` | Native buffer | Native `malloc` | Native stackful |
|---|---|---|---|---|---|---|
| 1 | 4.8–5.3 | 14 | 23 | 5.1 | 27.5 | 21.1 |
| 4 | 10.2–10.4 | 32–35 | 54–55 | 12.6 | 60.4 | 25.2 |
| 16 | 25 | 108–109 | 198–201 | 40.1 | 200 | 36.7 |
| 64 | 319–322 | 612–616 | 932–937 | 420 | 1,022 | 330 |

On wasm, records from a per-task buffer stay about twice as cheap as `malloc`, but the status check costs far more than natively. That check falls on every procedure that may suspend, so it is the main cost of running `None` builds as coroutines on one stack (Finding 14). Why V8 makes it costly was not investigated.

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

**Finding 16: a crash leaks different frames under `.lss` and `.boxy`.** `.boxy` defers a frame-replacing call through a closure and lets a waiter make it, so the call runs under the waiter's native frame. `.lss` makes the same call directly, and the waiter stays a record. When the callee crashes, R5 leaks the native frame and drops the record, so the two lowerings leak different values. Both heaps verify exactly. The fuzzer found this once in 2,000 programs (seed 296), and C8 now compares heap operations only in runs without a crash, as C1 and C10 already did. The RFC states C8 over traces.

**Finding 17: `Shared` handles replace server tasks, if a store readies only one waiter that would not wait, the oldest.** `bench/shared_vs_actor.py` counts operations for 30 clients of a semaphore with one permit and of a pool of two, under the pause schedule. A resume is one dispatch of a task by the host, including a readied waiter that runs its update function, answers `Wait` and parks again; a probe is a call of a waiter's function by the storing task, whose result is discarded.

| Written as | Spawns | Resumes | Allocations | Messages | Update functions run | Probes | Comparisons |
|---|---|---|---|---|---|---|---|
| Semaphore, server task | 93 | 276 | 725 | 90 | | | |
| Semaphore cell | 30 | 120 | 189 | 0 | | | |
| Semaphore, `Shared`, every waiter readied | 30 | 526 | 654 | 0 | 495 | | |
| Semaphore, `Shared`, every waiter readied, equal states compared | 30 | 526 | 248 | 0 | 89 | | 406 |
| Semaphore, `Shared`, oldest waiter that would not wait | 30 | 120 | 683 | 0 | 89 | 435 | |
| Pool, server task | 93 | 276 | 758 | 90 | | | |
| Pool, `Shared`, every waiter readied | 30 | 497 | 777 | 0 | 466 | | |
| Pool, `Shared`, every waiter readied, equal states compared | 30 | 497 | 399 | 0 | 88 | | 378 |
| Pool, `Shared`, oldest waiter that would not wait | 30 | 119 | 889 | 0 | 88 | 406 | |

- A `Shared` handle needs no server task, no mailbox and no reply channel.
- Readying every waiter at each store dispatches each of them after every store, and all but one wait again: about n² resumes under contention.
- An earlier version of this finding proposed comparing a dispatched waiter's state with the one it waited on, which needs equality on the state type. The counts above show it saves running the function but not the dispatch: resumes are unchanged. That version reported resumes after subtracting the compared ones, which overstated the saving.
- Probing the waiters and readying only the first whose function would not wait makes resumes linear, matching the semaphore cell, with no equality. The cost moves to probes, about n² pure calls by the storing tasks, and to the allocations their discarded results make. A readied waiter that ends without storing probes on; the regression test cancels one before it runs and shows that without this, some schedule deadlocks.
- The probe order is oldest first, and a readied waiter that waits again keeps its place. An earlier version of this finding required most recent first: probing the oldest first broke C9 on the 344th fuzzed program, because a `None` build on one plain stack runs only the waiter on top of it. `None` builds of programs that can wait now run as coroutines (Finding 23), which follow either order, and oldest first passes. Most recent first gives the same counts in every column of the table, so the change costs nothing here and removes the starvation of an early waiter under steady contention.
- Committing the first eligible waiter's update for it at the store, as the semaphore cell hands over a permit, is not an option: a waiter cancelled before it runs would leave its update committed with no `finally!` to undo it.
- Under the coroutine schedule, which `None` builds of programs that can wait use (Finding 23), the server tasks finish with 247 resumes and the `Shared` versions with 61. Under the input-order schedule, which models one plain stack, the server tasks deadlock (Finding 14) and the `Shared` versions do not.

The rule is `Shared.wake_rule = "first"`, the default. Under it, with `None` builds of programs that can wait running as coroutines, the same 2,000 fuzzed programs pass every check: 1,965 compared under C9 (1,005 against the coroutine schedule, the rest against input order), 15,926 cancelled at a suspension point, 5,006 probes and 338 wakes passed on by a readied waiter dropped before it ran. Under `"latest"`, the earlier rule, the figures were 1,964, 15,924, 5,028 and 336. `"latest"`, `"all"` and `skip_unchanged` remain for the benchmark and the regression tests.

**Finding 18: a `None` build runs loop entries on demand at a `Wait`.** (Superseded for `None` builds by Finding 23, which runs a program that can wait as coroutines; `NoneEngine` keeps the one-stack behavior for comparison.) In a `None` build, an update that returns `Wait` runs loop entries that have not started, earliest in input order, until a store changes its cell, as a receive does. With none left, it crashes as a deadlock. The `Shared` semaphore and pool therefore run on one stack, where the server-task versions deadlock. A `Wait` that crashed at once would make them fail there too.

**Finding 19: a pool needs `on_failure!`, and the open belongs inside it.** A pool that takes back a resource with `finally!` lends it again in whatever state its user's crash or cancellation left it. With `on_failure!`, the pool counts the resource as lost, and the next borrower that finds none free opens a replacement. The pinned test shows the difference. The first version of the pool opened a replacement, a hosted call, before entering `on_failure!`: a borrower cancelled during the open lost the pool's slot for good. The open must be inside the body that `on_failure!` guards, right after the update that takes the slot.

**Finding 20: the lock protocol is correct, and each of its rules is needed.** `shared_model.py` explores every interleaving of four scenarios: three threads sharing a permit, a taker waiting for a giver, two footprints of two variables named in opposite orders, and two waiters fed by a step over two variables. The correct protocol finishes every run in a state a serial order gives (729, 25, 369 and 307 states). Locking each footprint in the thread's own order deadlocks the two-cell footprints (V4); the RFC takes one handle per update, so this matters only if updates over several handles are added. Registering as a waiter after releasing the locks loses a wakeup in every scenario (V7). Steps without locks lose an update or a wakeup (V2, V7).

**Finding 21: type rules against cycles cannot use lambda sets, so they would keep functions out of states.** The RFC no longer uses these rules (Finding 22); `cycles.py` measures what they would cost. `Shared` brings the first way for a Roc value to reach itself: a state that holds its own handle. Deciding this with lambda sets is exact: over 100,000 generated types, every accepted type ran 4 random programs (1,518,146 cells) without a leak, and every rejected type had a witness that leaked. But `.boxy` builds never compute lambda sets, and validity cannot depend on the lowering, so the checker must decide without seeing captures. Its rules (no function in a state, and no type that reaches its own handle through a state) accepted nothing the exact rule rejected, and rejected 8,359 of the 87,114 acyclic types, every one for a function in a state. Checked once per declaration, with summaries of earlier declarations and requirements on parameters, the rules gave the same answer as on the expanded type for every type, but only after three fixes the fuzzer found: a declared type's arguments are types the program writes even where a parameter is ignored; a phantom parameter carries no requirement; and a parameter that appears only inside a function's captured state still carries one.

**Finding 22: the runtime's holder sets free every cycle, so no type rule is needed.** Cells were already ports whose holders the runtime tracked, for deadlock detection, and a store already passed a cell's holders on to the cells in its new state. Making a cell's release drop its state frees every cycle through states once the last task that held it finishes. A `None` build on one stack releases its cells when the program ends; as coroutines (Finding 23), when the last task holding one finishes, as a `Tasks` build does. Over 2,000 generated programs whose states hold handles (their own included) and jobs that capture their own cells, every heap was exact after every run: 89,626 cells released, 79,876 of them when a child task finished, none used after release. Never releasing leaks; not passing holders on at a store releases a cell `main` can still reach, which the assertion at every use catches. Tracking an entry's captures as holders turned out not to matter for cells, since the capturing task runs inside a holder that outlives it.

**Finding 23: `None` builds run a program that can wait as coroutines, and an entry that has not begun waits to be asked for.** `run_none_build` checks whether a receive, take, acquire or `Shared` update is reachable from `main`. If one is, the `None` build is the unwinding lowering with hosted calls that never suspend, under `CoroutinePolicy`; if not, it is `NoneEngine` on one plain stack. Of 2,000 fuzzed programs, 1,005 can wait and 995 cannot. C9 compares each against its schedule, and for each program that cannot wait, the coroutine build must give the one-stack results and effects exactly. That last check failed on seed 20 with the RFC's first wording, which resumed "the ready task first in the task tree". A loop's entries are started, and given their places, when the loop begins. While its owner waited at a loop nested in the body, an outer entry the owner had not asked for came first in the tree and ran before the inner loop's entries, which one stack never does. The scheduler now treats an entry that has not begun as ready only once its owner waits at that entry's loop head, or when no other task can run. With that rule the two agree on all 995 programs, and the RFC's G3, C9 and `None`-build prose say it. It changes one pinned result: the sessions RFC's permit example, whose semaphore server is an entry of an outer loop, now loses the race to the timeout in a `None` build (the permit comes back as `Cancelled`) instead of granting it first. Both are results of the `Tasks` build.

**Finding 24: spawns under the root limit, background entries and `plan.all!()` compose without new failures, and add one class of deadlock.** A loop over `plan.limit(n)` counts spawned entries against `n`, monitored as G9 in every run, and a plan whose root is not a limit spawns under none. `next` ends a loop once only background entries are left, cancels them as an exit would, waits for them to stop, and drops their reports; a `None` build discards background entries that have not started once every unstarted entry is background. `plan_all!` and `plan_all_ok!` wrap every entry that is not background with its index, so `all!(fs)` is `fs.concurrently().all!()`. The generator composes all three. Spawning under the root limit adds a deadlock the runtime catches: if every slot holds an entry that waits for one the body spawns, the spawned entry never starts (pinned). A background entry that never ends and holds a slot of a limit would keep later entries from starting, which no deadlock check sees because it is busy; the generator makes no such entries. A `None` build runs a background entry placed before the others to completion first, as "run the earliest unstarted entry" says, so a deadline belongs at the end of the plan, as in the RFC's search example. Both mutations, background entries that keep the loop going and spawned entries outside the root limit, are caught by the fuzzer. Over 2,000 programs from the extended generator, every check passed: 64,086 runs, 1,960 compared under C9, 16,086 cancellations at a suspension point, 22,112 entries spawned under a root limit, and 61,818 loops that ended with background entries left, 107,256 of those entries cancelled and 11,894 discarded before they started.

**Finding 25: moving the state into an update changes no outcome except at a crash, and a crash then poisons.** Each step runs its function's update form, which `update_forms` makes before ARC by rewriting tails: `Keep(r)` and `Wait` built in the returning block gain the states, `Set` is left alone, and any other tail, a call included, becomes a `match` on its tag. ARC then keeps the state alive across a call whose tag may be `Keep` or `Wait`, which is the RFC's rule that a state built by another function is a copy. Over the same 2,000 generated programs, the move gives exactly the counts the borrowing runtime gave, and every `Keep` and `Wait` put back the state it was given. With crashing updates added to the generator, every check passed: 392 updates crashed and poisoned their cells in the plain run, and 1,556 in the run whose states hold handles, where 278 programs have an update that crashes while its `Keep` still needs the state, so the cells in that state are reachable only from crashed frames; every heap was exact. Probes found the waiter would not wait, and the update agreed on the same state, 2,576 and 4,950 times.
