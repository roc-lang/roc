# Open Questions

This page lists what isn't settled. That covers four kinds of thing:

- decisions still open;
- costs nobody has measured;
- gaps the examples exposed;
- the order we'd build it in.

Everything else in the proposal is settled, either by the prototype or by an
argument written down in its [decision record](README.md#the-decisions).

## Names

Every name is a placeholder: `Concurrently`, `concurrently`, `limit`,
`shielded`, `background`, `spawn`, `Concurrent`, `supervise!`, `Shared`,
`update!`, `Set`, `Keep`, `Wait`, `finally!`, `on_failure!`, `get!`,
`modify!`, `suspending`, `concurrency: Tasks` and `ParallelTasks`. Names are a
matter for review, and no prototype can settle them.

## Open Decisions

| | Question | What would settle it |
|---|---|---|
| (a) | Detecting a wait that can never end when it crosses top-level tasks. Detection within one top-level task is implemented and tested; across them it's tested only for the superseded sessions RFC's ports, which used the same holder sets | Tests in the prototype |
| (b) | Contention under `ParallelTasks`: whether spinning, then yielding, is enough for a hot handle, and how a hot handle compares with a task that owns a semaphore | The real runtime |
| (c) | The cost of probing waiters: about n² pure calls under contention. Options are skipping a waiter whose state equals the one it waited on, or letting a library declare its waiters interchangeable. Separately, waiters that would `Keep` are released one at a time | The real runtime |
| (e) | Cycles in long-lived tasks. Release frees an abandoned cycle when the last task that held it finishes, so a task that runs for the whole program and keeps making and dropping cycles accumulates them | Programs written against the real runtime, and perhaps a debug diagnostic that reports cells freed by release |

Decision (d), about channel ends in states, became moot when queues replaced
channels. Decision (f), about changing large states in place, is settled in
[decision 10](03-decisions/10-in-place-updates.md).

The rest wait on implementation steps:

- whether records from per-task buffers keep their advantage over `roc_alloc`
  in compiled code;
- whether points-to analysis beats union-find for atomic counts on real
  programs;
- whether edge modes are worth adding for shared data that's path-copied;
- how hosts map tasks onto WASI 0.3 and onto JavaScript promises;
- crash exits on targets that `src/sljmp` doesn't cover yet, such as 32-bit
  x86.

## What It Costs

Nothing here has been measured in a Roc compiler. The only measurements are
microbenchmarks of hand-lowered C (Apple M5 Pro, clang `-O2`), noted below.
ReleaseFast benchmarks on a reference host will replace these figures.

| Situation | Cost | Measured? |
|---|---|---|
| Pure code | None | By construction |
| A program that can't wait, on a platform without tasks | None in `.lss` builds (see `.boxy` below) | By construction |
| A program that can wait, on a platform without tasks | A flag check after each call that may reach a wait, and pausing at the waits | Checks on 58% of procedures in the sessions RFC's measurements: within noise natively, 40% to 58% on a wasm kernel made only of calls |
| An effectful call that doesn't pause | One load and a predictable branch | 2.5% to 5% in a C kernel made only of calls |
| One pause and resume, two live values per frame | About 5 ns at depth 1, 12 ns at depth 4, 40 ns at depth 16, with records from a per-task buffer; five times as much from `malloc`. A stackful switch costs 21–44 ns over the same depths | In C, not in a compiler |
| Spawning a child task | One allocation, wake-state setup and a host enqueue: estimated tens of nanoseconds | Estimated |
| Code size | A save block per call site that can pause, and resume and drop entries, in effectful code only | Estimated |
| Values shared with child tasks | Nothing under `Tasks`; atomic counts on the positions children may share under `ParallelTasks` | Estimated |
| `.boxy` builds | A flag check after every erased effectful call, in `Tasks` and `None` builds, even in a `None` build of a program that never waits | Estimated |
| A `Set` with waiters | Probes until the first waiter that wouldn't wait: one pure call each, and no copy of the state | Counted in the prototype, not timed |
| A store of a state that can hold handles | A walk of the new state to update holder sets, O(n) for a large state full of handles | Estimated |
| Changing a large state | What changing the same value costs outside a handle, when the new state is built after the decision | Argued |

For I/O-bound work, these costs are small next to the system calls they wrap,
which take microseconds. Atomic counts and allocation will matter more than
pausing itself.

## Known Gaps

The [examples](examples.md) need no new primitive, but they expose these, in
rough order of importance:

1. **Waking many tasks is serialized.** When a barrier opens or a watched
   value changes, each waiter is readied only after the one before it has
   run. Part of open decision (c).
2. **Waiters on unrelated keys are probed.** In a cache, every store probes the
   waiters for every other key. One handle per key inside an outer handle
   avoids it, but nothing points you there.
3. **One entry can't be cancelled on its own.** A loop's entries have no names,
   by design. Background entries cover the commonest case, a deadline.
4. **Compensation can't run on cancellation.** Cleanups are pure, so a
   cancelled saga can't undo its bookings. It must run where nothing cancels
   it, or record each step durably for a recovery task.
5. **One handle per update.** State that changes together must share a handle.
   An update over two handles is a future possibility.
6. **No restart window or backoff for `supervise!`.** A lifetime limit fits
   supervising units of work, which the prototype supports. Backoff would be a
   field of the configuration record.
7. **No parallel evaluation of pure code.** It's a separate, deterministic
   feature.

Some drawbacks are deliberate rather than gaps:

- A crash leaks what its task's running frames owned.
- A loop exit waits for a long pure computation in an entry.
- A paused task has no native stack for a debugger to show.
- A reply nobody will answer ends in a deadlock crash.
- A local timeout gives a permit back before the remote operation stops.

Each decision record's "What It Costs" section explains its own.

## Future Possibilities

- **Parallel pure code,** as a deterministic `Iter` adapter.
- **Durable execution.** A paused task holds values only, never native
  addresses, so a platform could save it and resume it after a restart.
- **Deterministic simulation testing.** A test platform with a seeded,
  single-threaded scheduler would make every interleaving reproducible.
- **Platform functions that hold a permit,** so a timeout doesn't give a permit
  back while the remote operation still runs.
- **Updates over two handles,** locked in one global order.

## Implementation Plan

1. Model the task runtime and host contract, and check them. The prototype has
   done this.
2. Parse `concurrency` and `suspending`, and record which procedures and calls
   can pause.
3. Add the suspension ledger to ARC, and certify it.
4. Implement the pass that saves and restores frames, for `.lss` builds.
5. Add the task runtime, the host ABI, a conformance harness, and one native
   reference platform. Confirm per-task record buffers with an optimized
   compiler.
6. Implement plans, concurrent loops, the `Concurrent` library and `None`
   builds. Then implement atomic counts for `ParallelTasks`, measuring each
   step.
7. Add `Shared`: the builtins, the checker's warnings, updates and cleanups in
   the IR, and the runtime's cells, waiters, poisoning and holder sets.
8. Extend the pass to `.boxy` and erased calls.
9. Build browser and WASI 0.3 reference hosts.

[Semantics](06-semantics.md) lists the invariants each step must establish.
