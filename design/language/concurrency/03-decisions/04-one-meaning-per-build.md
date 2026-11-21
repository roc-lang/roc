# 4. One Meaning Per Build

| | |
|---|---|
| Status | Settled by prototype |
| Builds on | [Explicit suspension](02-explicit-suspension.md), [structured tasks](03-structured-tasks.md) |
| Detail | Main RFC: [On platforms that do not schedule tasks](../../concurrency-explicit-suspension.md#on-platforms-that-do-not-schedule-tasks), [Concurrent loops](../../concurrency-explicit-suspension.md#concurrent-loops-1) (the lowering table), [Language guarantees](../../concurrency-explicit-suspension.md#language-guarantees) (G3), [Compiler invariants](../../concurrency-explicit-suspension.md#compiler-invariants) (C9), [Cost model](../../concurrency-explicit-suspension.md#cost-model). Shared-state RFC: [On platforms that do not schedule tasks](../../concurrency-shared-variables.md#on-platforms-that-do-not-schedule-tasks), [Invariants](../../concurrency-shared-variables.md#invariants) (V8). Prototype: [Results](../../concurrency-prototype/README.md#results), Findings 14, 18, 23 and 24 |

What does a concurrent program do on a platform that doesn't schedule tasks, and how do we know every build means the same thing?

## The Decision

A platform's header says how it runs tasks, with a `concurrency` field. That gives three kinds of build:

| Build | Who runs tasks | Hosted calls |
|---|---|---|
| `None` | The program itself, one task at a time on one thread | Always finish before they return |
| `Tasks` | The host, one resume at a time; it may move tasks between threads | Those marked `suspending` may pause their task |
| `ParallelTasks` | The host, with resumes on several threads at once | As in `Tasks` |

A platform that doesn't opt in gets `None`. The guarantee is that **a `None` build behaves like one possible schedule of a `Tasks` build**: one thread, hosted calls that finish in place, and this rule for what runs next:

> Run the ready task that comes first in the task tree. A loop entry that hasn't begun counts as ready only once its loop asks for the next result, or when no other task can run.

A `None` build therefore never does anything a `Tasks` build couldn't. Packages can use concurrent loops and `Concurrent` freely: they get concurrency where the platform offers it, and sequential behavior elsewhere.

The compiler implements the rule two ways, and picks by whether any task can wait for another (at a shared-state `Wait`, such as waiting for a permit or a queue item):

- **A program that can't wait** runs on one plain stack with no tasks at all. A loop runs each entry to completion when it asks for the next result, in input order. In `.lss` builds the suspension pass does nothing, so this costs nothing; `.boxy` builds pay for erased calls, below.
- **A program that can wait** runs its tasks as coroutines. It's lowered as a `Tasks` build is, except that hosted calls never pause, and the program carries its own single-threaded scheduler. A task that waits pauses, and the next ready task runs.

Where no task waits, this means:

- `all!` and `both!` run every function and return the same results as anywhere else.
- `all_ok!` stops at the first `Err`, and later functions never start.
- `race!` returns its first function's result, and the others never start.
- Limits and shields change nothing, `spawn` appends an entry, and entries not yet started when the loop is left never start.

So a timeout still compiles and runs, but it can't interrupt a call that blocks:

```roc
fetch_with_timeout! = |url|
    match Concurrent.race!(|| Fetched(Http.get_utf8!(url)), [|| { Sleep.millis!(2_000); TimedOut }]) {
        Fetched(Ok(body)) => Ok(body) # In a None build the fetch always wins.
        Fetched(Err(HttpErr(code))) => Err(HttpErr(code))
        TimedOut => Err(TimedOut) # Never reached: the sleep never starts.
    }
```

If no task can run, nothing can ever end the wait, so a `None` build crashes with a deadlock message. A crash stops the whole program on these platforms, as it does today, and `supervise!` runs its function once.

## What We Tried

| Option | What happened |
|---|---|
| Combinators that run their functions in order (the main RFC's first draft) | Fine while no task could wait for another. Channels, and later shared state, let tasks wait |
| One plain stack, running entries on demand when a task waits | A task that waits sits under whatever it runs next, so only the most recent waiter can continue. Waking had to be most recent first: probing the oldest waiter first broke C9 on the 344th fuzzed program. And a protocol in which two tasks take turns, like a permit that must come back before its lender continues, deadlocks. The sessions RFC's semaphore, pool and request-reply-release server all did |
| A counting cell built into the runtime | A semaphore cell whose permit returns when released runs on one stack. It works only because returning a permit is a fixed runtime action; a general state change is Roc code, which a cancelled task can't run. So it covers counting resources only |
| Accept the limit | Every library in the sessions RFC's catalog would have needed a `Tasks` platform |
| Coroutines for programs that can wait, scheduling "the ready task first in the task tree" | Ran every turn-taking protocol, but failed the fuzzer on seed 20 (below) |
| Coroutines for every program | Costs status checks in programs that can't wait, where one plain stack gives the same results and effects |

The history in short: `None` builds first ran every task on one plain stack in input order. That forced waking to be most recent first, which lets an early waiter starve, and it broke as soon as tasks waited on each other. The superseded sessions RFC measured the options and chose coroutines for programs that use channels. With shared state replacing channels, every program that can wait now gets them.

**Seed 20.** For a program that can't wait, the prototype checks that the coroutine build gives exactly the one-stack results and effects. With the first wording of the rule, that failed on seed 20. A loop's entries get their places in the task tree when the loop begins. A task's loop body ran a nested loop and waited at its head. An outer entry that the task hadn't asked for yet came earlier in the tree than the inner loop's entries, so it ran first. One stack never does that: an outer entry starts only when the outer loop asks for it. The clause "counts as ready only once its loop asks for the next result, or when no other task can run" closes the gap.

## Why This One

One meaning matters because packages don't choose their platform. If `race!` or a queue meant one thing under `Tasks` and another under `None`, every package that used them would have to be tested, and maybe written, twice. Making the `None` build a schedule the `Tasks` build could also pick means there's one semantics, and the `None` build is just a deterministic choice within it.

Coroutines are what make that possible once tasks can wait for each other. They run turn-taking protocols that deadlock on one stack, and they free the waking order, so waiters are woken oldest first (see [waking waiters](07-waking-waiters.md)).

Keeping the plain stack for programs that can't wait follows the requirement that concurrency cost nothing on platforms that don't opt in. The scheduler rule's last clause is what makes the two implementations agree: with it, the coroutine schedule of a program that can't wait is exactly input order.

A deadlock is a crash, not a hang, because in a `None` build "no task can run" is exact: nothing outside the program can change anything the program waits on.

## What It Costs

- **A timeout can't interrupt a blocking call.** `race!` runs only its first entrant. If that entrant never finishes, neither does the race.
- **No overlap.** Requests run one after another, so total latency is their sum.
- **Plan order shows.** A `None` build runs entries in input order, so a background entry placed before the others runs to completion first. A deadline belongs at the end of the plan, as in the main RFC's search example.
- **Waiting costs something everywhere.** A program that can wait gets a status check after every call that may reach a wait, even on a platform that schedules nothing. In the sessions RFC's measurement, 58% of procedures in programs that used ports got one. Natively the check was within noise; on wasm (V8, `-O2`) it added 40% to 58% to a kernel made only of calls, and why V8 makes it costly wasn't investigated.
- **`.boxy` pays more.** A module can't know whether a module importing it waits, so in a `.boxy` `None` build every call through an erased effectful function value is a suspension point, even in a program that never waits.
- **Crashes stop the program.** `supervise!` doesn't restart in a `None` build, so the equivalence check excludes runs in which a supervised child crashes.
- **Schedule-dependent results can change.** Only one schedule is chosen. When the scheduler rule changed, the sessions RFC's permit example changed result in its `None` build: the semaphore server now loses the race to the timeout instead of granting the permit first. Both are results a `Tasks` build can give.

## Evidence

The prototype checks C9 and G3 on every generated program: a `None` build must equal a `Tasks` build under the schedule above. A program that can wait is compared against the coroutine schedule, and one that can't against input order. For each program that can't wait, the coroutine `None` build must also give exactly the one-stack results and effects. The shared-state RFC's V8 extends the check to `Wait`.

- **Seed 20 and after.** In the run that found seed 20, 1,005 of 2,000 programs could wait and 995 couldn't. With the new clause, the coroutine build agreed with one stack on all 995. A later 2,000-program run, at commit `003f009461`, had 963 programs that can't wait, and all agreed.
- **Comparisons per run.** About 1,960 of each 2,000 programs are compared under C9. The rest crash inside `supervise!`, where a `None` build stops the program by design.
- **Turn-taking.** As coroutines, the `None` build ran the semaphore, the pool and the request-reply-release server, all of which deadlock on one stack. Of 250 fuzzed programs that deadlocked on one stack, it finished 32, and it added no deadlock (measured on the sessions RFC's channels). A real cycle of waits still deadlocks.
- **Libraries.** Under the coroutine schedule, 30 clients of a semaphore and of a pool finish: the server-task versions with 247 resumes, the `Shared` versions with 61. Under input order, which models one stack, the server tasks deadlock.

Not measured: the status checks' cost in compiled code. The wasm figure comes from a hand-lowered C microbenchmark, and the 58% from the Python prototype.
