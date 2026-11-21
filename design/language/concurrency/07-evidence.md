# Evidence

The design was built against an executable prototype, in
[`concurrency-prototype/`](../concurrency-prototype/README.md). This page says
what it is, what it checks, what it found, and how to run it. The full results
are in the prototype's [README](../concurrency-prototype/README.md#results)
and in the RFCs' [What to establish first](../concurrency-explicit-suspension.md#what-to-establish-first)
and [What the prototype shows](../concurrency-shared-variables.md#what-the-prototype-shows).

## What It Is

The prototype is a Python model of the design: a small IR, ARC with its
suspension ledger, both lowerings (`.lss` and `.boxy`), the task runtime, the
host ABI, and the libraries written over them. It runs fuzzed programs under
adversarial schedules against a stackful oracle. It's not compiler code, and
nothing in it runs Roc source.

It can't tell you how fast any of this is. The Python model counts operations
(resumes, probes, allocations, status checks), not time, and it doesn't count
copies. The only timings come from separate C microbenchmarks of hand-lowered
code, which measure the lowering, not a compiler. They stand only until
ReleaseFast benchmarks on a reference host replace them.

> Note that the prototype models the update form and the crash rule from
> commit 5c09149b97, but not the probe form: its probes run the waiter's
> function as written.

## What Is Modeled

| Part | Files | What it models |
|---|---|---|
| IR and plans | `ir.py`, `plans.py` | An A-normal-form IR with frame-replacing calls; plans as inert values (`Each`, `Cat`, `Limit`, `Shield`, `Background`) and their flattening |
| ARC | `arc.py` | `dup`/`drop` placement and the suspension ledger, checked by an independent ownership simulation |
| Lowering | `lower.py` | Suspendability under `.lss` (0-CFA lambda sets) and `.boxy` (every erased effectful call), the C2 and C3 checks, and `tail_drive`'s data for deferred erased calls |
| Engines | `engines.py` | `NoneEngine` (one plain stack), `StackEngine` (the stackful oracle) and `UnwindEngine` (the lowering) |
| Runtime and host | `runtime.py`, `host.py` | The task tree, wake protocol, cancellation and crash propagation with monitors; hosts with `ready`, `pause`, `input_order` and `random` schedules |
| Libraries | `combinators.py`, `library.py` | `all!`, `both!`, `all_ok!`, `race!`, `supervise!` and `plan.all!()` as loops; the semaphore, pool and circuit breaker over `Shared` |
| Shared state | `shared.py` | Cells, `Set`/`Keep`/`Wait`, waiters, `finally!`, `on_failure!`, poisoning, and static checks of purity and cleanup balance |
| Exhaustive models | `maude/wake.maude`, `shared_model.py` | The wake protocol under every thread interleaving; the `ParallelTasks` lock protocol for updates |
| Type rules | `cycles.py` | The rejected type rules against cycles, on generated type declarations |
| Hosts that break rules | `conformance.py` | One host per way of breaking a platform obligation |
| Coverage | `coverage.py` | A matrix from every invariant row to the tests that pin it |

It also still models the superseded sessions RFC (`sessions.py`: channels,
mailboxes, semaphore cells, ports), and the generator still composes those
constructs. Reference-count atomicity has separate models in
[rc-atomicity/](../rc-atomicity/README.md): Maude, Lean proofs and a Python
fuzzer. [Decision 5](03-decisions/05-refcount-atomicity.md) covers them.

## What Every Generated Program Is Checked For

Each program runs under six schedules (`ready`, `pause`, `input_order`,
`random`, `random` in a `ParallelTasks` build, and that with root
cancellation), each under both lowerings and both engines.

| Check | What must agree or hold | Rows |
|---|---|---|
| Static | ARC certification of every ledger; `never` procedures have no site; every ledger covers what its continuation uses; records hold no pending cell outside task storage | C2–C5 |
| Unwind against stack | The lowering and the stackful oracle, same host and schedule: identical results, effect traces and heap operations | C1, C10 |
| `.lss` against `.boxy` | Identical results and traces; identical heap operations in runs without a crash | C8 |
| `None` against `Tasks` | A `None` build equals a `Tasks` build under the coroutine schedule (programs that can wait) or the input-order schedule (programs that can't) | C9, G3, V8 |
| Coroutines against one stack | For a program that can't wait, the coroutine `None` build gives the one-stack results and effects, under both engines and lowerings | C9 |
| Cancel everywhere | Cancel the root at each suspension in turn: the heap stays exact, and every pending cleanup runs once, after the task's descendants | C5, V5 |
| Exact heap | After every run, every live object is held by a leaked native frame of a crashed task, and its count equals its references | R5 |
| Monitors | Readiness only from parked, no re-entry, no code after cancel or crash, bottom-up teardown, no hang; `supervise!` restarts; limits and background entries | R1–R8, G9 |
| Schedule independence | Programs without `race!`, `all_ok!`, crashes or arrival-dependent bodies give one result under every schedule | G4 |

The platform obligations are checked separately: `conformance.py` runs one
host per way of breaking P1 or P3–P10, and the monitors or the exact heap
catch every one. P2 needs real threads, so it's tested under ThreadSanitizer.

## How The Generator Builds Programs

`gen.py` composes small typed constructs at random, as
[AGENTS.md](../../../AGENTS.md) requires of fuzzers. Nothing is a handwritten
scenario. The constructs include:

- constants, arithmetic, tuples, tags, lists, branches, matches and crashes;
- hosted calls, suspending and not;
- direct calls, self-recursion and tail loops bounded by fuel;
- closures, frame-replacing calls through closures, and tail cycles through a
  closure (deferred erased calls under `.boxy`);
- every combinator, and loops over plans with limits, shields, background
  entries and spawns, whose bodies exit early, return errors or nest;
- shared-state workers that increment, peek, take with `Wait`, move between
  two cells, and pair takes with `finally!` or `on_failure!` gives, with
  crashes and cancellation between them.

Each value has one of a fixed set of types (`Int`, `Str`, `Try(Int, Str)`,
`List(Int)`, closures, `Shared(Int)` and others), so every program is
well-typed. With `links`, every program uses shared state, and most build
cells whose states hold handles (their own included) and jobs that capture
cells, so states form cycles the runtime must release.

## Results

The headline fuzzing run, `check.py 2000 1`, as the README reports it:

| | Count |
|---|---|
| Programs | 2,000, none violating any check |
| Runs | 63,926, plus 15,926 that cancelled the root at a suspension, and 8,000 as coroutine `None` builds |
| Crashes | 33,512, of which 17,112 reached a parent through a loop, and 24 a running parent |
| Deferred erased calls | 52,534 |
| Leaked objects | 5,472, each traced to a crashed task's native frame |
| `Shared` | 78,518 cells, 164,554 updates, 15,568 `Wait`s |
| Cleanups | 52,498; 6,208 ran while a cancelled or crashed task was dropped |
| Crashing cleanups | 730, poisoning 602 cells and turning 258 cancellations into crashes |

C9 compared 1,964 programs; the other 36 crash inside `supervise!`, where a
`None` build stops the whole program by design. The run takes about 220
seconds.

**The latest run.** `python3 check.py 2000 1` passed at commit 003f009461,
which composes spawns under a root limit, background entries and
`plan.all!()`. 963 programs that can't wait agreed between coroutines and one
stack, over 8,000 coroutine runs. Finding 24 in the README reports that
generator's totals: 64,086 runs, 1,960 compared under C9, 16,086
cancellations, and 22,112 entries spawned under a root limit.

**Crashing updates.** Once the prototype modeled updates that move the state
in and poison on a crash, the generator gained updates that crash. That
shifts every generated program, so the counts change. In a 2,000-program run,
392 updates crashed, 1,190 cells were poisoned, and C9 compared 1,961
programs over 64,096 runs. In a `links` run, 1,556 updates crashed and 2,576
cells were poisoned, and in 278 programs an update crashed while its `Keep`
still needed the state. Every heap was exact. With the old generator, the new
runtime gives the same counts as before on every line.

What the prototype found or settled:

- **The wake protocol.** A "woken while running" flag is unsound: the fuzzer
  found it within its first 30 programs, and Maude finds it in a plain `all!`
  over two children on two threads. One wake word per task fails too. One
  counted word per wait source is verified over every interleaving
  (Finding 1).
- **Real threads.** Under ThreadSanitizer, the counted word is race-free and
  readies each of 36,093 parks once; both flag protocols break R1 within a few
  dozen parks (Finding 5).
- **Hosts.** A host must wake a task inside the lock that `roc_task_abort`
  takes, or it can wake a freed task (Finding 2).
- **Crashes.** Only running native frames leak; a paused procedure's record
  is dropped exactly (Finding 3). `.lss` and `.boxy` leak different frames
  on a crash, so C8 compares heaps only in runs without one (Finding 16, seed
  296).
- **The `None` scheduler rule.** Seed 20 showed that resuming "the ready task
  first in the tree" let an unasked outer entry run before an inner loop's
  entries. An unbegun entry is now ready only once its loop asks, and the
  coroutine and one-stack builds agree (Finding 23).
- **Waking.** For 30 clients of a semaphore, readying every waiter takes 526
  resumes; probing and readying the oldest that wouldn't wait takes 120, the
  same as a built-in semaphore cell, with 435 probes. A server task took 276
  resumes and 90 messages (Finding 17).
- **Locks.** `shared_model.py` finds no deadlock, lost wakeup or lost update,
  and catches each of three broken protocols (Finding 20).
- **Cycles.** The type rules against cycles were sound but rejected 8,359 of
  87,114 acyclic types, every one for a function in a state (Finding 21).
  Holder sets released every cycle instead: over 2,000 `links` programs,
  64,094 runs, 0 leaks and 0 uses after release (Finding 22).
- **Combinators.** A lifetime restart limit is enough for `supervise!`
  (Finding 15), and spawns, background entries and `plan.all!()` compose with
  one new, caught, class of deadlock (Finding 24).

Lowering costs, counted over 300 generated programs with every suspending call
paused (`bench.py 300`):

| Measure | `.lss` | `.boxy` |
|---|---|---|
| Procedures that may suspend | 4,911 of 6,475 | 4,932 of 6,475 |
| Status checks executed | 53,345 | 53,534 (+0.4%) |
| Records | 49,403 | 49,564 |
| Values saved per record | 1.76 | 1.77 |
| Longest record chain | 8 | 8 |

Timings from `bench/unwind.c`, Apple M5 Pro, clang at `-O2`, hand-lowered:

| Measure | Result |
|---|---|
| Status check after each call that doesn't pause, natively | +2.5% to +5% |
| The same on wasm (V8) | +40% to +58% |
| Pause and resume at depth 1, 4, 16, records from a per-task buffer | about 5, 12–13, 37–47 ns |
| The same with records from `malloc` | about five times as much |
| Stackful switch at the same depths | 21–23, 25–29, 36–44 ns, before allocating stacks |

`bench/boxy_records.c` found that `.boxy` records should hold dynamic values
inline after a header, in the per-task buffer.

## How To Run It

Each line runs from the repository root:

```bash
cd design/language/concurrency-prototype && python3 tests.py        # regression tests, with rc-atomicity's
cd design/language/concurrency-prototype && python3 check.py 2000 1 # the fuzzer, about 220 s
cd design/language/concurrency-prototype && python3 check.py 2000 1 links  # cycles through states
cd design/language/concurrency-prototype && python3 coverage.py     # the invariant matrix
cd design/language/concurrency-prototype && python3 conformance.py  # hosts that break obligations
cd design/language/concurrency-prototype && python3 shared_model.py # the lock protocol
cd design/language/concurrency-prototype && python3 cycles.py 100000 7     # the rejected type rules
cd design/language/concurrency-prototype && python3 bench.py 300           # lowering counts
cd design/language/concurrency-prototype && python3 bench/shared_vs_actor.py
cd design/language/concurrency-prototype/maude && maude -no-banner -batch wake.maude < /dev/null
```

The C benchmarks:

```bash
cd design/language/concurrency-prototype/bench && clang -O2 -o unwind unwind.c switch_arm64.S && ./unwind
cd design/language/concurrency-prototype/bench && zig cc -target wasm32-wasi -O2 -o unwind.wasm unwind.c && node run_wasi.mjs unwind.wasm
cd design/language/concurrency-prototype/bench && clang -O2 -o boxy_records boxy_records.c && ./boxy_records
cd design/language/concurrency-prototype/bench && clang -O1 -g -fsanitize=thread -DPROTOCOL=0 wake_tsan.c -o wake_tsan && ./wake_tsan
```
