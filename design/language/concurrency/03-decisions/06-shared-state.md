# 6. Shared State

| | |
|---|---|
| Status | Settled, with parts open. The API, cleanups and poisoning are checked by the prototype; contention under `ParallelTasks` is open |
| Builds on | [Structured tasks](03-structured-tasks.md), [One meaning per build](04-one-meaning-per-build.md), [Refcount atomicity](05-refcount-atomicity.md) |
| Detail | [Motivation](../../concurrency-shared-variables.md#motivation), [Atomic updates](../../concurrency-shared-variables.md#atomic-updates), [`update!`](../../concurrency-shared-variables.md#update), [`finally!` and `on_failure!`](../../concurrency-shared-variables.md#finally-and-on_failure), [Poisoning](../../concurrency-shared-variables.md#poisoning), [Drawbacks](../../concurrency-shared-variables.md#drawbacks), [Rationale and alternatives](../../concurrency-shared-variables.md#rationale-and-alternatives) |

The task tree from [decision 3](03-structured-tasks.md) shares only immutable values. How do running tasks share state that changes, such as a cap on database connections, a pool, or a circuit breaker that every request consults?

## The Decision

Roc gets one builtin opaque type, `Shared(s)`, and a few effectful functions on it. There's no new syntax and no new kind of name.

```roc
Shared.new! : s => Shared(s)
Shared.update! : Shared(s), (s -> [Set(s, r), Keep(r), Wait]) => r
Shared.finally! : Shared(s), (s -> s), (() => a) => a
Shared.on_failure! : Shared(s), (s -> s), (() => a) => a

Shared.get! : Shared(s) => s                   # update!(|s| Keep(s))
Shared.modify! : Shared(s), (s -> s) => {}     # update!(|s| Set(g(s), {}))
```

A *handle* is a value of type `Shared(s)`. It names a *cell* that holds a state of type `s`. You capture, pass and store a handle like any value, but only effectful functions can use it. It has no equality and no hashing, and `Inspect` shows `<Shared>`.

`update!` runs a pure function on the current state, atomically. The function answers with a tag:

| Tag | The state | `update!` returns | Waiting tasks |
|---|---|---|---|
| `Set(new, r)` | Becomes `new` | `r` | Woken ([decision 7](07-waking-waiters.md)) |
| `Keep(r)` | Unchanged | `r` | Not woken |
| `Wait` | Unchanged | Nothing yet: the function runs again after the state next changes | Not woken |

A semaphore needs nothing else:

```roc
with_permit! : Shared(U64), (() => a) => a
with_permit! = |permits, f| {
    permits.update!(|n| if n == 0 { Wait } else { Set(n - 1, {}) }) # Take a permit, or wait for one.
    permits.finally!(|n| n + 1, f) # Give it back however f ends.
}
```

- **`finally!(cleanup, body)`** calls `body`, then applies `cleanup` to the state however `body` ends: by returning, by cancellation or by a crash. **`on_failure!`** applies it only on cancellation or a crash. A pool needs the second: a connection whose user crashed is in an unknown state, so the pool counts it as lost instead of lending it again.
- **Cleanups are pure.** A cleanup has type `s -> s`, so it can't wait, suspend or do I/O. A cancelled task runs no more of its own code, but the runtime can run these while it drops the task. Pending cleanups run innermost first.
- **Poisoning.** An update or a cleanup that crashes *poisons* its handle: every later update of it crashes, and so does every task waiting on it. No code can test for poison or clear it. Restarting the scope that created the handle makes a new one, as restarting an OTP server does.
- **A library on top.** `Queue`, `Reply`, `Semaphore`, `Pool`, `Latch`, `Barrier`, `Watch`, `RwLock`, `Memo`, `Breaker` and `Bucket` are ordinary Roc code over these functions. Queues and replies are [decision 9](09-queues-not-channels.md).

Because the answer is a tag, an update can change the state and make a decision in one `match`:

```roc
decision = breaker.update!(|state| match state {
    Open(until) if now < until => Keep(Reject)
    Open(_) => Set(Probing, Probe)
    Probing => Keep(Reject)
    Closed(_) => Keep(Run)
})
```

The compiler warns about three shapes: a handle that never reaches another task (a `var` does the job), a read in one update followed by a write computed from it in another (a lost update), and a call that can suspend between an `update!` and the `finally!` or `on_failure!` that should follow it.

## What We Tried

| Option | What broke |
|---|---|
| Tasks that own the state, reached through mailboxes ([sessions RFC](../../concurrency-sessions.md#shared-resources-are-libraries)) | It works, as the prototype showed, but every resource needs a server task kept alive beside the program, plus a mailbox and a reply channel for every request: two messages and two task switches per acquire. Channel ends are used once, which Roc's types can't check, so misuse is a run-time crash. Because the end changes with every message, a server is a tail-recursive function, not a loop. |
| Shared variables: `var $$permits = 4`, `&$$permits` to pass the variable itself, and `wait` and `defer` keywords (an earlier draft) | Every access was marked, but the result was references in Roc: a new kind of name whose plain reads see other tasks' writes, an operator that aliases storage, and a statement-level rule for which code runs atomically. |
| Optimistic retries, as in software transactional memory | Pure updates make retries safe, but long updates would redo work and could starve under contention. An update touches one known cell, so a lock fits. |
| `update!` returning a pair `(new, r)`, as Haskell's `atomicModifyIORef` does | Can't say "unchanged", so every read would wake waiters. Can't wait. |
| A separate `when!(condition, f)` for waiting | Two functions where one tag does, and the condition and the change could disagree. |
| `defer` and `errdefer` keywords | Keywords, plus a choice between block scope and function scope. The closure passed to `finally!` settles the scope: `return` and `?` inside it leave `body`, and the cleanup runs after them. |
| Updates over several handles at once | The prototype supports them, locking cells in one global order, and its model checks that order. No library needed them, and one record covers what they do. Kept as a future `update2!`. |
| A `match` with no case for a state meaning `Wait` | A forgotten case would become a hang instead of a compile error. |
| Handles that can't escape their scope | Needs non-escaping closures in the type checker, and state shared across requests must escape anyway. |
| A way to clear poison | Restarting the scope that created the handle is the recovery, as in OTP. |

## Why This One

**`->` is the atomicity boundary.** The function passed to `update!` is pure, so it can't call `!` functions, suspend or do I/O, and nothing can happen in the middle of it. The type checker already enforces that, so no keyword or statement rule decides what's atomic. `!` marks where state can change between two calls, and `->` marks where it can't.

**Updates can't deadlock on locks.** In a `ParallelTasks` build, an update locks one cell and runs only pure code while holding it. A pure function can't call `update!`, so locks never nest, and lock-order deadlock is impossible without any ordering rule. No lock is ever held across a suspension. In `None` and `Tasks` builds there's no lock at all: the function can't suspend, and resumes are serial.

**A handle isn't a reference.** No expression reads "the current value of `permits`". The state appears only as the argument of a pure function the runtime calls. Nothing writes through a handle either: the new state is that function's result. Pure code can hold a handle but has no operation on it.

**Let-it-crash doesn't need actors.** Erlang/OTP's model needs a crash to stay isolated, which [decision 3](03-structured-tasks.md)'s crash links and `supervise!` already give, and a restart to begin from known-good state. Pure atomic updates give the second: no task ever sees a half-made state, and a broken update or cleanup becomes a crash at the handle's next use instead of a silent leak.

**Waiting is a tag, not a mechanism.** `Wait` is Hoare's conditional critical region, and STM's `retry`, applied to one cell. When the function is a `match`, the case that waits is written out, so a forgotten case is a compile error.

## What It Costs

- **Shared mutable state in a language built on values.** Every use is marked with `!`, but programs can now have logic races, such as reading in one update and acting in the next. The warning catches the common shape, not every one.
- **Handles have identity.** Two equal-looking handles can be different state. Roc programs already handle values like this (streams, connections), but until now only from platforms.
- **Transitions are lambdas.** `get!` and `modify!` cover reading and plain replacement, but an update that decides or waits is a function answering a tag, longer than an assignment.
- **One handle per update.** State that changes together must share a handle, so unrelated state put in one record for one update is contended by every update. A transfer between two accounts in separate handles can't be written atomically; the accounts must be one record.
- **Contention under `ParallelTasks`.** One hot handle serializes every task that updates it, and a long update delays them all. Whether spinning briefly and then yielding the thread is enough is open decision (b), and the Python prototype can't measure it.
- **A crash in an update loses the state** for every task that uses the handle, not just the one that hit the bug. See [decision 10](10-in-place-updates.md).
- **Code runs during cancellation.** Cleanups are pure, but they're still Roc code running in a dropped task.
- **A reply nobody will answer is a deadlock, not a value.** A cell has no ends, so nothing tells an asker that its server stopped. Stall detection eventually crashes it ([decision 9](09-queues-not-channels.md)).
- **A local timeout gives a permit back early.** When a task holding a permit is cancelled during a hosted call, the call is aborted locally, but the remote operation may still run. The permit counts local scopes, not remote operations. The sessions RFC fixed this by passing the permit's end to the hosted call; here, a hosted function that holds a permit is a future possibility.
- **Poison outside a supervised scope** crashes every restart until the limit. That's by design: the broken state lies outside the subtree being restarted.

## Evidence

The prototype spells the API as primitive operations (`shared.py`) and rewrites the semaphore, a pool with `on_failure!`, the same pool with `finally!`, and the circuit breaker over them. The `SharedVariables` (17 tests), `SharedConveniences` and `SharedModel` tests show:

- Every permit comes back under every schedule and when the root is cancelled at any suspension point, and at most one job holds the permit at once. Skipping cleanups at drops deadlocks.
- The pool with `on_failure!` counts a crashed user's connection as lost and opens one replacement; with `finally!` it lends the crashed user's connection again.
- A crashing cleanup poisons the handle, later updates and parked waiters crash, and a handle created inside the supervised function is new after a restart.
- Static checks reject unbalanced cleanups, impure update functions, and `Shared` operations in pure procedures.
- `shared_model.py` explores every interleaving of the `ParallelTasks` lock protocol and finds no deadlock, lost wakeup or lost update. It catches locking two-cell steps in each thread's own order, registering a waiter after unlocking, and dropping the locks.
- 2,000 fuzzed programs, 628 of them using shared state, ran under every schedule, both lowerings, a `None` build and cancellation at every suspension point, with exact heap accounting: 63,926 runs and 164,554 updates, of which 3,726 ran again after a `Wait` and 6,208 cleanups ran at a drop. Every check passed.
- For 30 clients of a one-permit semaphore, the sessions RFC's server task spawns 93 tasks and sends 90 messages; `Shared` spawns 30 and sends none. These are operation counts in Python, not timings.

Not measured: contention in a real `ParallelTasks` runtime. Poisoning by a crashing update is modeled: a restart's read crashes with the update's poison message under every schedule, a parked waiter crashes too, and the state the crashed update held is counted as a crashed-frame leak in the exact heap check.
