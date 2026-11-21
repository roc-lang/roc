# 3. Structured Tasks

| | |
|---|---|
| Status | Settled by prototype |
| Builds on | [Stackful green threads](01-stackful-green-threads.md), [explicit suspension](02-explicit-suspension.md) |
| Detail | Main RFC: [Concurrent loops](../../concurrency-explicit-suspension.md#concurrent-loops) (guide), [Concurrent loops](../../concurrency-explicit-suspension.md#concurrent-loops-1) (reference), [What concurrency means in Roc](../../concurrency-explicit-suspension.md#what-concurrency-means-in-roc), [Crashes](../../concurrency-explicit-suspension.md#crashes), [Crash mechanics](../../concurrency-explicit-suspension.md#crash-mechanics), [The `Concurrent` library](../../concurrency-explicit-suspension.md#the-concurrent-library), [Rationale and alternatives](../../concurrency-explicit-suspension.md#rationale-and-alternatives) |

What shape does concurrent work take in a Roc program: what starts a task, who waits for it, and what happens to it when nobody needs its result anymore?

## The Decision

Concurrent work is a tree of tasks that share only immutable values. A child task's only output is its result, and no task outlives the code that started it.

A _plan_ is an inert value of type `Concurrently(a)` that describes work which may overlap. `List.concurrently` builds one from a list of effectful functions. Building, storing or returning a plan runs nothing. A `for!` loop over a plan runs it:

```roc
# Done once 2 of 3 replicas acknowledge.
write_quorum! : List(Replica), Bytes => Try({}, [NoQuorum])
write_quorum! = |replicas, data| {
    var $acks = 0
    for! result in replicas.map(|r| || Db.write!(r, data)).concurrently() {
        match result {
            Ok({}) => {
                $acks = $acks + 1
                if $acks == 2 { return Ok({}) } # Cancels the third write.
            }
            Err(_) => {}
        }
    }
    Err(NoQuorum) # Reached only after every write has finished.
}
```

- Entering the loop starts the plan's entries. Each _entry_ is a child task.
- Each iteration binds one entry's result, in the order the entries finish.
- Leaving by `break`, `return` or `?` cancels the entries still running and waits until they've stopped. Reaching the end of the loop means every entry has finished.
- The loop's _running set_ has no name, so it can't be stored, captured or returned.

Plans compose. Each method is a pure function that returns a new plan:

| Method | Meaning |
|---|---|
| `plan.limit(n)` | At most `n` of the plan's entries run at once |
| `plan.shielded()` | A loop exit waits for the plan's entries that have begun instead of cancelling them |
| `plan.background()` | These entries don't keep the loop going: once every other entry has finished, they're cancelled and the loop ends. A deadline is a background entry |
| `plan.concat(other)` | `plan`'s entries, then `other`'s |

Inside a loop body, `spawn f` adds one more entry to the running loop. If the plan's root is a limit, spawned entries count against it, so a crawler written as `for! page in start.concurrently().limit(8)` never runs more than eight fetches, however many links it spawns.

The familiar combinators are ordinary library functions written with these loops: `Concurrent.all!`, `all_ok!`, `both!` and `race!`, and the plan methods `plan.all!()` and `plan.all_ok!()`, which return results in input order. `Concurrent.all!(fs)` is `fs.concurrently().all!()`. A type can define its own `concurrently` method, the way it defines `iter`, so it decides what runs and how it's grouped, while the runtime decides how it runs.

**Cancellation is silent.** A cancelled entry stops at its current suspension point (see [explicit suspension](02-explicit-suspension.md)), and its values are released. Effects it already performed stay performed. No code of its own runs afterward, since Roc has no `finally` or destructors. The [shared state](06-shared-state.md) design later adds pure cleanup functions, which do run.

**Crashes follow Erlang/OTP.** Expected failures stay `Err` values. A crash is for bugs, and:

- **A crash ends its task.** No Roc code catches it.
- **Links follow the structure.** When an entry crashes, the loop's caller crashes with the same message at once, wherever it's waiting, and its other entries are cancelled. The body never sees the crash. OTP calls this `one_for_all`.
- **`supervise!` restarts.** `Concurrent.supervise!({ max_restarts: 10 }, f)` runs `f` as a child and restarts it after a crash, up to the limit, then crashes itself. It never reports the crash to its caller, so it can't be used to catch errors. Wrapping each child of `all!` in its own `supervise!` gives OTP's `one_for_one`.
- **Platforms supervise roots.** A web server platform can fail only the crashed request and keep serving. A command-line platform can exit with an error.

## What We Tried

| Option | What it was | What it lost |
|---|---|---|
| Futures or task handles | A user-visible `Task` or `async` type | It adds a second function color. Roc already replaced `Task` with effectful functions |
| Detached tasks | `spawn` anywhere, with the task outliving its caller | Cancellation and errors become ad hoc. `spawn` exists only inside a loop body, where it adds to that loop |
| Task groups or nurseries as objects | A scoped handle passed to a closure, like Swift's `withTaskGroup` or a Trio nursery | A closure could capture the handle and keep it. Preventing that needs non-escaping types, which Roc doesn't have. A concurrent loop is a nursery whose scope is the loop, and its running set has no name to capture |
| `async let` (Swift) | A binding whose value is a child task's eventual result | The RFCs list it as prior art without a separate argument. The binding names a running task, which is the handle problem above; `Concurrent.both!` covers its common case |
| Combinators only | A fixed menu of `all!`, `race!` and friends (this RFC's earlier draft) | A closed menu: a quorum, the first success, hedged requests or a retry policy would each need a new builtin. A loop makes the policy a `match` |
| A fold with a step function | A step returning `[Continue(s), Break(s)]`, like `List.fold_until` | It works, but threads state through a step function. A loop body keeps state in `var`s and can perform effects of its own |
| A stream of arrivals as a value | The running set as a first-class value | It could be stored or returned with its entries still running, and leaving a loop over it would have to wait inside a drop, which can't suspend |
| Plans fixed when the loop starts | No `spawn` | The prototype's semaphore and pool libraries start a grant for each request as it arrives, which no fixed plan can express. Crawlers and hedged requests need the same |
| `spawn` outside any limit | Spawned entries ignore the plan's limit | A crawler would count its in-flight fetches by hand, and a loop over `plan.limit(8)` that spawns would look bounded and not be |
| Exits wait by default | Swift task groups on normal exit | Most loops leave because they have their answer. In the prototype, every library loop that leaves early (`race!`, `all_ok!`, a client timing out on a permit) wants cancellation, and none needed a shield |
| A loop ends only when every entry has | No background entries | A deadline or progress reporter would hold the loop open, so every such loop would count arrivals and `break` |
| Catching crashes | `try`/`catch`, or a combinator returning `Err(Crashed)` | It turns crashes into a second error channel |
| A crash stops the whole program | Today's behavior, kept in `Tasks` builds | One bad request would take down a server |
| A restart window for `supervise!` | OTP's restarts-per-period limit, needing a clock from the platform | In the prototype, a server supervised as a whole with a limit of 3 died after four transient crashes far apart in 20 requests. Supervising each request with the same limit served all 20 |

## Why This One

The structure is lexical, so cancellation and errors have exactly one place to happen: the loop exit. Nothing outlives the loop that started it, so nothing outlives a combinator call either. Exits are edges the compiler already knows, so waiting at one is an ordinary suspension point, and no drop ever has to wait.

A loop is open where a menu of combinators is closed. A quorum, a hedged request or a deadline is a `match` in a loop body, with state in `var`s. The [examples](../examples.md) write about 40 common patterns this way and need no new primitive.

Plans are values because that's how Roc already separates describing work from doing it. A pure function can build, combine and return a plan, but only effectful code can run one.

Sharing only immutable values means there are no data races between tasks. It also makes every execution a series-parallel order, which is what lets the compiler choose [atomic reference counts](05-refcount-atomicity.md) precisely.

Silent cancellation follows from Roc having no destructors. Cancelling can't run user code, and [explicit suspension](02-explicit-suspension.md) makes it release everything the task owned, unlike a parked stack.

OTP's split between errors and crashes is already how Roc works: expected failures are values, and crashes are bugs. Restarting from a known state, rather than catching, keeps it that way while letting a server survive a bad request.

## What It Costs

- **A third `for!` protocol.** The type of what you loop over decides whether the loop is concurrent. A plan built elsewhere doesn't show at the loop that it's concurrent; its type and name have to.
- **Exits wait.** Leaving a loop waits until its cancelled entries stop, and an entry stops only at a suspension point. A long pure computation in an entry delays every exit, a timeout's included.
- **One entry can't be cancelled on its own.** The debouncer example leaves a sleeping task for every event and ignores the stale ones, and graceful shutdown races each accept against a stop flag. Cancelling one entry would need a name for it, which the loop deliberately lacks. Background entries cover the commonest case, a deadline.
- **Compensation can't run on cancellation.** A saga whose caller cancels it can't undo the bookings it made, because cancellation runs no user code and cleanups are pure. A saga must run where nothing cancels it, keep going after a failure and compensate each later success (see [draining instead of cancelling](../../concurrency-explicit-suspension.md#draining-instead-of-cancelling)), or record each step durably for a recovery task.
- **Supervision has no restart window or backoff.** `supervise!` counts restarts for its whole life. A window matters for long-lived processes, which the task tree makes rare. Backoff would be a field of the configuration record, and isn't there yet.
- **Crashes leak.** The crashed task leaks what its running native frames owned; its paused records are dropped exactly. Erlang avoids this with per-process heaps, which Roc's shared heap can't have. Platforms that crash often need region allocation.
- **Crash isolation needs a non-local exit.** Native targets use `src/sljmp` and wasm uses exception handling. Elsewhere a crash still ends the process.
- **Limits add a deadlock.** If every slot of a limit holds an entry waiting for one the body spawns, the spawned entry never starts. The runtime detects it and crashes. A background entry that never ends and holds a slot keeps later entries from starting, and no deadlock check sees it, because it's busy.

## Evidence

The [prototype](../../concurrency-prototype/README.md) writes plans, loops, `spawn` and every combinator in its IR, and fuzzes generated programs that compose them. Settled by prototype:

- **Fuzzing.** Over 2,000 generated programs, no check failed: 63,926 runs, 15,926 more that cancelled the root at a suspension point, 33,512 crashes (17,112 of them reaching a parent through a loop's link), and 100,400 `supervise!` returns and 800 give-ups, each checked against the restart limit. The runtime invariants behind structured lifetimes, exact cancellation and crash containment (R3–R5) are monitored in every run, the heap is checked exactly after each one, and fixed result order (G4) is checked across schedules.
- **Plan methods.** A later generator composes limits at a plan's root, spawns, background entries and `plan.all!()`. Over 2,000 programs, every check passed: 64,086 runs, 22,112 entries spawned under a root limit, and 61,818 loops that ended with background entries left. The fuzzer catches both deliberately wrong variants: background entries that keep the loop going, and spawned entries outside the root limit.
- **Decisions.** `spawn`, cancel-on-exit and a lifetime restart limit are each settled by the prototype's libraries and supervision test, as the "What We Tried" table says ([Decisions left to prototypes](../../concurrency-explicit-suspension.md#decisions-left-to-prototypes)).
- **Coverage.** The [examples](../examples.md#what-the-examples-expose) cover the usual catalog of patterns with no new primitive. Their gaps are listed above.

Not measured: the cost of spawning in compiled code. The RFC estimates tens of nanoseconds per child; no compiler exists yet.
