# 7. Waking Waiters

| | |
|---|---|
| Status | Settled by prototype, with the cost of probing open (open decision (c)) |
| Builds on | [One meaning per build](04-one-meaning-per-build.md), [Shared state](06-shared-state.md) |
| Detail | [Waiting](../../concurrency-shared-variables.md#waiting), [Runtime](../../concurrency-shared-variables.md#runtime) (Waking), [Lowering](../../concurrency-shared-variables.md#lowering) (the probe form), [What the prototype shows](../../concurrency-shared-variables.md#what-the-prototype-shows), [Decisions left to review](../../concurrency-shared-variables.md#decisions-left-to-review) |

When an update answers `Set` and changes a cell's state, some tasks may be parked on that cell because their own updates answered `Wait`. Which of them run?

## The Decision

The runtime *probes* the waiters, oldest first, and readies only the first one that wouldn't wait.

1. Waiters are kept in the order they began waiting.
2. After a `Set`, while still holding the cell, the storing task calls the *probe form* of each waiter's function on the new state. The probe form decides the tag without building a new state or an answer, so a probe stores nothing and copies nothing ([decision 10](10-in-place-updates.md)).
3. The first waiter whose function wouldn't answer `Wait` is readied, and probing stops. That waiter then runs its function for real.
4. The readied waiter *carries the wake*. If it stores, its own `Set` probes the remaining waiters. If it ends without storing, because it answers `Keep`, answers `Wait` again (another task changed the state first), crashes, or is cancelled before it runs, the runtime probes on from there, so the wake is never lost with it.

Take ten tasks waiting for one permit:

```roc
permits.update!(|n| if n == 0 { Wait } else { Set(n - 1, {}) })
```

When a permit comes back, the runtime probes the oldest waiter, finds it would answer `Set`, and readies it alone. That's one wakeup per release, not ten.

A few more rules complete it:

- **A readied waiter that waits again keeps its place** in the order.
- **A probe that crashes counts as not waiting.** The waiter is readied and crashes when it runs.
- **Poisoning readies every waiter**, so that each one crashes.
- **A probe commits nothing** on a waiter's behalf, so a waiter cancelled after it was readied has taken nothing.

## What We Tried

| Option | What broke |
|---|---|
| Ready every waiter at each `Set` | All but one answer `Wait` again, so dispatches grow with the square of the waiters. With 20 clients contending for one permit, this took 251 task resumes and ran 190 updates again, against 80 and 19 with probing. It stays in the prototype as the regression switch. |
| Require equality on the state, and park again a dispatched waiter whose state equals the one it waited on | Replaces running the function with a comparison, but the waiter is still dispatched to make it, so resumes stay quadratic (526 for 30 semaphore clients, the same as readying everyone). It also restricts state types and keeps old states alive. |
| Ready only waiters whose state changed, compared at the `Set` | Never skips anything in the libraries: each `Set` that matters changes the state. The waste comes from waiters that run after the first one took what the `Set` gave. |
| Run the first eligible waiter's update at the `Set` and hand it the result, as the sessions RFC's semaphore cell hands over a permit | Commits an update for a task that hasn't run. If that task is cancelled first, its update stays committed and the `finally!` after it never runs, so a permit is lost. A semaphore cell can give a permit back; a pure update has no inverse. |
| Probe the most recent waiter first (the earlier rule) | Needed only while `None` builds ran every task on one plain stack, where only the most recent waiter can continue. Oldest first broke sequential equivalence (C9) against that stack on the 344th fuzzed program. Now that `None` builds pause waiting tasks as coroutines, the order is free, and most recent first lets an early waiter starve under steady contention. |
| Wake one waiter without probing, and pass the wake on if it waits again | Each `Set` by the woken waiter starts the chain again, so a semaphore still dispatches every waiter on every release. |

## Why This One

A *resume* is one dispatch of a task by the host. Every rule that readies a waiter blindly pays a resume just to learn that the waiter should keep waiting. Probing asks that question in the storing task instead, as a pure call on a state that's already in hand.

Purity is what makes the question safe to ask. An update function has type `s -> [Set(s, r), Keep(r), Wait]`, so given the same state and captures it gives the same answer, and the real run agrees with the probe. Probing is exact, not a guess.

Oldest first is fair: a waiter that could go ahead is never passed over for one that began waiting later. It also costs nothing over most recent first, which gives the same counts in every column of the benchmark below.

Passing the wake on is what keeps this correct under cancellation. Without it, a waiter readied and then cancelled before it ran would strand the next waiter that could go ahead, and the prototype finds a schedule that deadlocks.

## What It Costs

- **Probes are about n² pure calls under contention.** Dispatches become linear, but each `Set` may probe many waiters, each inside a crash boundary in the storing task, and under `ParallelTasks` while the cell's lock is held. Open decision (c) lists two ways to cut them: skip a waiter whose state equals the one it waited on (for state types with equality, which keeps old states alive), or let a library declare its waiters interchangeable so that one waiting probe ends the search. The real runtime should measure whether either is worth it.
- **Waiters that would `Keep` are released one at a time.** When a barrier opens, a watched value changes, a log gains an event, or a cache entry becomes ready, many waiters can go ahead without changing the state. Each is readied only after the one before it has run and passed the wake on, so releasing `n` of them takes `n` dispatches in sequence, and a `ParallelTasks` build can't run them at the same time. Readying together every waiter whose probe would `Keep`, with the first that would `Set`, would release them at once, at the cost of probing every waiter after each `Set`. This is also open decision (c).
- **Unrelated keys get probed.** In a single-flight cache held as one `Shared(Dict(k, Entry(v)))`, every store probes the waiters for every other key. One handle per key inside an outer handle, `Shared(Dict(k, Shared(Entry(v))))`, avoids it, but nothing points programmers there.
- **Stores run other tasks' functions**, so a task's `Set` costs more the more waiters its cell has.

## Evidence

`bench/shared_vs_actor.py` runs 30 clients of a semaphore with one permit under the pause schedule. These are counts of operations in the Python prototype, not timings:

| Semaphore written as | Resumes | Update functions run | Probes |
|---|---|---|---|
| Server task (sessions RFC) | 276 | | |
| The runtime's semaphore cell (sessions RFC) | 120 | | |
| `Shared`, every waiter readied | 526 | 495 | |
| `Shared`, every waiter readied, equal states compared | 526 | 89 | |
| `Shared`, oldest waiter that wouldn't wait | 120 | 89 | 435 |

A pool of two shows the same shape: 497 resumes when every waiter is readied, 119 with probing. Probing makes the `Shared` semaphore match the sessions RFC's built-in semaphore cell, with no server task and no equality.

- **20 clients, one permit:** 80 resumes and 19 updates run again with probing, against 251 and 190 when every waiter is readied.
- **Passing the wake on:** a waiter readied by a probe and cancelled by a race before it runs passes the wake on, and the other waiter gets the permit under every schedule. Without passing it on, some schedule deadlocks.
- **Fuzzing:** with oldest first as the default and `None` builds of programs that can wait running as coroutines, 2,000 fuzzed programs pass every check: 1,965 compared under C9 (1,005 against the coroutine schedule), 15,926 cancellations at a suspension point, 5,006 probes and 338 wakes passed on by a readied waiter dropped before it ran.
- **Most recent first** gives the same counts in every column, under this schedule and a random parallel one.
- **No lost wakeups (V7)** is checked by the `ParallelTasks` model for readying every waiter, and by tests and differential runs for readying the first.

Not modeled: the probe form itself. The prototype's probe calls the waiter's whole function and discards the result; the form that skips building the state and the answer came with [decision 10](10-in-place-updates.md) and is argued, not measured.
