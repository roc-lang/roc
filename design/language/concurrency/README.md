# Concurrency for Roc

| | |
|---|---|
| Status | Proposal. Not adopted; nothing here is part of `design.md` |
| Evidence | A Python model of the lowering, runtime and libraries, checked by fuzzing ([evidence](07-evidence.md)). Nothing has been built in the compiler |
| History | The working RFCs this was condensed from ([below](#history)) |

This proposal lets Roc programs run effects concurrently without adding
`async`, futures or a task type. You run effectful functions concurrently by
putting them in a list and looping over it:

```roc
for! result in replicas.map(|r| || Db.write!(r, data)).concurrently() {
    match result {
        Ok({}) => {
            $acks = $acks + 1
            if $acks == 2 { return Ok({}) }   # the third write is cancelled
        }
        Err(_) => {}
    }
}
```

And when several running tasks need the same changing state, they share a
handle and change it with pure functions:

```roc
permits.update!(|n| if n == 0 { Wait } else { Set(n - 1, {}) })
```

## The Design in Brief

- **No new kind of function.** Roc's effectful functions (`=>`) are the only
  ones that can pause, so the compiler already knows which code to transform.
  Code that can't pause compiles as it does today.
- **Structured.** Concurrent work is a tree of tasks. Nothing outlives the loop
  that started it, and leaving a loop cancels what's still running.
- **Crashes work like Erlang/OTP.** A crash ends its task and travels up to a
  supervisor. Expected errors stay `Err` values.
- **Shared state without locks.** A `Shared(s)` handle changes only through
  pure updates. They can't pause or do I/O, so they're atomic, they can't
  deadlock on a lock, and a big state changes in place.
- **Queues, not channels.** Streams between tasks, semaphores, pools and the
  rest are library code over `Shared`.
- **The platform schedules.** A platform opts in and decides how tasks run.
  On a platform that doesn't, the same program runs its own tasks on one
  thread and means the same thing.
- **It runs everywhere.** A paused task saves its live values into small heap
  records and returns, so the output runs on every WebAssembly engine, with no
  stack switching.

## What We're Asking You to Judge

1. **The semantics.** This is the main question. Is this how concurrency
   should look and behave in Roc? Is it fast, friendly and functional?
2. **The names.** Every name is a placeholder. It's probably best to discuss
   them separately, once the semantics are agreed.
3. **The costs.** They're estimated and argued, but not measured in a
   compiler. [Open questions](08-open-questions.md#what-it-costs) lists them.

## How to Read This

| If you want to… | Read |
|---|---|
| See what code looks like (20 minutes) | [The tour](01-tour.md) |
| Look up a signature | [The API](02-api.md) |
| Know why the design is this way | [The decisions](#the-decisions), in order |
| Compare it with Go, Rust, Swift, OTP or Haskell | [Comparison](04-comparison.md) |
| Get quick answers to the usual objections | [FAQ](faq.md) |
| Write or maintain a platform | [Platforms](05-platforms.md) |
| Judge whether it fits the compiler | [Semantics](06-semantics.md) and [evidence](07-evidence.md) |
| See it cover real programs | [Examples](examples.md): about forty patterns, from fan-out to Raft-style log replication |
| Know what's unsettled | [Open questions](08-open-questions.md) |

## The Decisions

Each decision record says what we chose, what we tried first, why, what it
costs, and what backs it. They're in the order the design was built, and each
builds on the ones before it.

| | Decision | Status |
|---|---|---|
| 1 | [Not stackful green threads](03-decisions/01-stackful-green-threads.md): why each task doesn't get its own stack | Settled by argument, after a prototype showed it works natively |
| 2 | [Explicit suspension](03-decisions/02-explicit-suspension.md): how tasks pause without `async` | Settled by prototype; cost argued, not measured |
| 3 | [Structured tasks](03-decisions/03-structured-tasks.md): plans, `for!` loops, cancellation and supervision | Settled by prototype |
| 4 | [One meaning per build](03-decisions/04-one-meaning-per-build.md): what happens on platforms without tasks | Settled by prototype |
| 5 | [Reference count atomicity](03-decisions/05-refcount-atomicity.md): which counts must be atomic, decided at compile time | Settled, with parts open: cost in compiled code, edge modes |
| 6 | [Shared state](03-decisions/06-shared-state.md): `Shared(s)` and pure updates, instead of actors or shared variables | Settled, with parts open: contention under `ParallelTasks` |
| 7 | [Waking waiters](03-decisions/07-waking-waiters.md): which waiting task runs when the state changes | Settled by prototype, with the cost of probing open |
| 8 | [Cycles](03-decisions/08-cycles.md): how states that hold each other are freed | Settled by prototype, with parts open: long-lived tasks, detection across roots |
| 9 | [Queues, not channels](03-decisions/09-queues-not-channels.md): how tasks stream values to each other | Settled by argument, on primitives the prototype checks |
| 10 | [In-place updates](03-decisions/10-in-place-updates.md): why a big state in a handle isn't copied | Settled by argument; not measured |

## History

These working documents hold the full reasoning, every alternative, and the
prototype's detailed results. They're long, and parts of them describe earlier
versions of the design. Where they differ from the pages above, the pages
above are current.

- [Suspendable effects and platform-scheduled concurrency](../concurrency-explicit-suspension.md):
  the main RFC.
- [Shared state](../concurrency-shared-variables.md): the companion RFC.
- [Atomic reference counts across tasks](../concurrency-rc-atomicity.md).
- [Sessions, channels and shared resources](../concurrency-sessions.md):
  superseded by shared state.
- [The prototype](../concurrency-prototype/README.md).
