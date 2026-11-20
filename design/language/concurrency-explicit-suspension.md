# RFC: Suspendable Effects and Platform-Scheduled Concurrency

| | |
|---|---|
| Status | Draft. Not adopted; nothing here is part of `design.md`. |
| Scope | Plans and concurrent loops, a builtin library, platform and hosted declarations, host ABI, one LIR pass |
| Companion | [Sessions, Channels and Shared Resources](concurrency-sessions.md): the rest of the same design |
| Supersedes | The stackful prototype on `origin/greenthreads` |

## Summary

This RFC lets Roc programs run effects concurrently without adding `async`,
futures or a task type to the language. It has four parts:

1. **Plans and concurrent loops.** A plan is an inert value describing work
   that may overlap: building, storing or returning one runs nothing. A `for!`
   loop over a plan runs it. Each entry becomes a child task, the loop body sees
   each result as it arrives, and every way out of the loop cancels or waits for
   what is still running. The familiar combinators (`all!`, `both!`, `all_ok!`,
   `race!`) are ordinary library functions written with these loops.
2. **A platform opt-in.** A platform declares that it schedules tasks and marks
   the hosted functions that may *suspend*: return to the host before their
   result is ready. The host schedules suspended work however its domain needs.
   On a platform that does not opt in, the program schedules its own tasks on
   one thread, and hosted calls finish before they return, so a program means
   the same thing on every platform.
3. **A local compiler pass.** Tasks run on the ordinary native stack. When a
   task pauses, each procedure on its stack saves what it still needs into a
   small heap record and returns; resuming continues from the innermost
   record. Only call sites that can pause are touched, and a call that does
   not pause costs one flag test.
4. **Crash semantics from Erlang/OTP.** A crash ends the task it happens in,
   cancels the tasks linked to it through the concurrency structure, and
   propagates upward until a supervisor (the platform, or a `supervise!` call)
   decides whether to restart or give up. Roc code never catches a crash.

Roc already separates pure functions (`->`) from effectful ones (`=>`), and has
no effect polymorphism. Only effectful code can suspend, so the compiler knows
which procedures need the transformation without any new annotation in
application code. The output is ordinary code that returns to its host, so it
runs on every WebAssembly engine, with no stack switching.

This RFC is half of one design. Its model is a tree of tasks with a session
between each child and its parent, and here every session carries exactly one
message: the child's result. The companion RFC,
[Sessions, Channels and Shared Resources](concurrency-sessions.md), lets
sessions carry protocols, makes cancellation a message the other end of a
session receives, and builds synchronization from shared sessions. It adds to
this RFC. Where it extends one of this RFC's invariants (C2, C6, C9 and R6),
the invariant's row says so.

## Motivation

### The gap

`docs/langref/platforms.md` promises platform-scheduled concurrency "after
0.1.0" and notes that pieces needed to make it practical do not exist yet.
Today a Roc program performs one effect at a time per host call. The examples
in this RFC use a hypothetical platform with
`Http.get_utf8! : Str => Try(Str, [HttpErr(U16)])` and
`Sleep.millis! : U64 => {}`:

```roc
# Three requests, one after another. Total latency is the sum.
fetch_all! : List(Str) => Try(List(Str), [HttpErr(U16)])
fetch_all! = |urls| {
    var $bodies = []
    for url in urls {
        $bodies = $bodies.append(Http.get_utf8!(url)?)
    }
    Ok($bodies)
}
```

Common programs need more: fetching several resources at once, a server
handling many requests per core, a GUI that keeps rendering while it waits on
I/O, a script with a timeout.

### What exists today

- **Thread-safe reference counting.** Allocations the host can see use atomic
  counts; allocations proven confined to Roc code use plain counts in
  optimized builds (design.md, "Thread-Confined Reference Counts"). The last
  release is decided by the atomic decrement itself ("Releasing The Last
  Reference"). Any concurrency design rests on this.
- **A stated threading model.** Roc code is single-threaded within one host
  call; the host may share values between threads.
- **A stackful prototype.** `origin/greenthreads` (two commits, September 2025,
  unmerged) gives each green thread a memory-mapped stack with a guard page and
  switches between them with hand-written assembly for aarch64 and x86_64.

### Why not stackful green threads

Stackful coroutines are the obvious way to make direct-style code concurrent,
and the prototype shows they work natively. They are a poor foundation for Roc:

- **WebAssembly.** Core wasm cannot switch stacks. The stack-switching proposal
  (WasmFX) is at phase 3 and was left out of WebAssembly 3.0; Wasmtime supports
  it only on x86_64 Linux. JSPI works in Chrome 137+ and Safari 27 but only in
  browsers. WASI 0.3 shipped the stackless "callback" async lift; the stackful
  lift is still behind a flag.
- **Cancellation.** A parked stack holds reference-counted values the compiler
  can no longer see. Abandoning it leaks them, and Roc has no unwinding to
  release them.
- **Memory.** Each task needs a fixed stack: 1 MB of address space plus a guard
  page in the prototype. Roc's native code uses the C ABI, so stacks cannot be
  moved to grow them the way Go's are.
- **Portability.** Every architecture needs its own context switch.

### Requirements

A design must:

1. add no second function color: effectful code stays direct-style;
2. leave scheduling policy to the platform;
3. run on native targets and on every wasm engine;
4. cancel tasks without leaking or running user code;
5. cost nothing for pure code and for platforms that do not opt in;
6. contain a crash to the work it happens in, as Erlang/OTP does;
7. follow the compiler's rules: later stages consume explicit data, backends
   follow explicit reference-count statements, and nothing is guessed.

## Guide-level explanation

### Running effects concurrently

The `Concurrent` builtin module runs effectful functions concurrently and
returns when they have all finished:

```roc
fetch_pair! : Str, Str => Try((Str, Str), [HttpErr(U16)])
fetch_pair! = |a, b| {
    (first, second) = Concurrent.both!(|| Http.get_utf8!(a), || Http.get_utf8!(b))
    Ok((first?, second?))
}
```

Each argument is an ordinary effectful function that takes no arguments.
Nothing about `Http.get_utf8!` changes; it is the same function a sequential
program calls.

### Concurrent loops

When a program decides what to do as results arrive, it loops over a plan:

```roc
# Done once 2 of 3 replicas acknowledge; the third write is cancelled.
write_quorum! : List(Replica), Bytes => Try({}, [NoQuorum])
write_quorum! = |replicas, data| {
    var $acks = 0
    for! result in replicas.map(|r| || Db.write!(r, data)).concurrently() {
        match result {
            Ok({}) => {
                $acks = $acks + 1
                if $acks == 2 { return Ok({}) }
            }
            Err(_) => {}
        }
    }
    Err(NoQuorum)
}
```

`List.concurrently` turns a list of effectful functions into a plan, a value of
type `Concurrently(a)`. Nothing runs until a loop runs it:

- Entering the loop starts the plan's entries as child tasks.
- Each iteration binds one entry's result, in the order the entries finish.
- Leaving the loop by `break`, `return` or `?` cancels the entries still
  running and waits until they have stopped. Reaching the end of the loop
  means every entry has finished.
- If an entry crashes, the others are cancelled and the crash propagates to
  the loop's caller. The body never sees it.

State lives in `var`s, so the body is ordinary effectful code: it can log,
call other functions, and run loops of its own.

Plans compose. Each of these is a pure function that returns a new plan:

| | |
|---|---|
| `plan.limit(n)` | At most `n` of the plan's entries run at once; the next one starts as soon as one finishes |
| `plan.shielded()` | A loop exit waits for the plan's entries that have begun instead of cancelling them |
| `plan.background()` | The plan's entries do not keep the loop going: once every other entry has finished, they are cancelled and the loop ends |
| `plan.concat(other)` | `plan`'s entries, then `other`'s |

A deadline is a background entry. This search returns what the shards have
answered after 200 milliseconds, or sooner if they all answer:

```roc
search! = |shards, query| {
    var $hits = []
    deadline = [|| { Sleep.millis!(200); TimedOut }].concurrently().background()
    for! arrival in shards.map(|s| || Answered(Shard.search!(s, query))).concurrently().concat(deadline) {
        match arrival {
            Answered(result) => { $hits = $hits.concat(result ?? []) }
            TimedOut => break
        }
    }
    $hits
}
```

A type can define a `concurrently` method that returns a plan, the way it can
define `iter` for `for` loops. A sharded store might run one entry per shard,
limited to the connections it has. Custom types decide what runs and how it is
grouped; the runtime always decides how it runs.

Inside a loop body, `spawn f` adds one more entry to the running loop. If the
loop's plan is a limited plan, as in `for! page in start.concurrently().limit(8)`,
spawned entries count against it, so a crawler that spawns a fetch for every
link it finds still runs at most eight at once. A hedged request sends a backup
only when the primary is slow:

```roc
hedged_get! = |primary, backup, key| {
    for! arrival in [|| Got(Kv.get!(primary, key)), || { Sleep.millis!(50); Slow }].concurrently() {
        match arrival {
            Got(Ok(value)) => return Ok(value)
            Got(Err(_)) => {}
            Slow => spawn || Got(Kv.get!(backup, key))
        }
    }
    Err(AllFailed)
}
```

The entries return different tags, and the anonymous union `[Got(…), Slow]`
holds them all. That is how a loop combines different kinds of work: each entry
tags its result, and the body matches.

### Errors

`all_ok!` takes functions that return `Try`. The first `Err` to arrive cancels
the others and is returned; otherwise the results come back in input order:

```roc
fetch_all! : List(Str) => Try(List(Str), [HttpErr(U16)])
fetch_all! = |urls| Concurrent.all_ok!(urls.map(|url| || Http.get_utf8!(url)))
```

`List.map` is pure, and so is the function passed to it: it only creates
effectful closures, and creating a function value is not an effectful call.
`all!` is the same without the `Try` handling. Both are library functions
written with a concurrent loop. Both are also methods of plans, so a limited
plan's results come back in input order:

```roc
fetch_all_politely! = |urls| urls.map(|url| || Http.get_utf8!(url)).concurrently().limit(8).all_ok!()
```

### Racing and timeouts

`race!` returns whichever function finishes first and cancels the rest. It
takes its first function separately, so it can never be called with nothing to
race. Tagging each entrant says which one won:

```roc
fetch_with_timeout! : Str => Try(Str, [HttpErr(U16), TimedOut])
fetch_with_timeout! = |url|
    match Concurrent.race!(|| Fetched(Http.get_utf8!(url)), [|| { Sleep.millis!(2_000); TimedOut }]) {
        Fetched(Ok(body)) => Ok(body)
        Fetched(Err(HttpErr(code))) => Err(HttpErr(code))
        TimedOut => Err(TimedOut)
    }
```

### Draining instead of cancelling

Cancellation stops an entry at its current suspension point, and effects it
already performed stay performed. When a program must account for every
effect, as a saga that compensates each completed step must, leaving the loop
at the first failure is wrong: a cancelled step may or may not have happened.
Such a loop keeps going after a failure and compensates each later success as
it arrives, or shields the entries whose outcome it must know.

### What concurrency means in Roc

- **Interleaving happens only at suspension points.** A function runs without
  interruption until it calls a hosted function that suspends, or waits at a
  concurrent loop's head or exit. Pure code is never interrupted.
- **There are no data races.** Concurrent functions share only immutable
  values. A nested function can read a `var` but never reassign it, and it sees
  the value from when it was created, so there is no shared mutable state.
- **Effect order between entries is unspecified.** Within one entry, effects
  happen in program order, as always. A loop body sees results in the order
  they arrive; the combinators return them in input order.
- **Cancellation is silent.** A cancelled entry stops at its current
  suspension point. No code of its own runs afterward (Roc has no `finally` or
  destructors), and its values are released. Effects it already performed stay
  performed.
- **The structure is lexical.** A loop's entries, and everything they start,
  finish or are cancelled before the loop is left. Nothing outlives the loop
  that started it, so nothing outlives a combinator call either.
- **Pure code is untouched.** A pure function can build plans and closures but
  cannot run them, and it compiles exactly as it does today. Parallel
  evaluation of pure code is a separate, deterministic feature (see "Future
  possibilities").

### Crashes

Roc follows Erlang/OTP's "let it crash" philosophy. Code does not defend
against bugs it cannot handle; a crash ends the work it happens in, and a
supervisor restarts or abandons that work from a known state.

- **A crash ends its task.** No Roc code catches it, and no code of the
  crashed task runs again.
- **Links follow the structure.** Every entry a loop starts is linked to the
  loop's caller. When an entry crashes, the caller crashes with the same
  message at once, wherever it is waiting: at that loop's head, inside a
  nested loop, or on a hosted call, which is aborted. It does not first take
  results that other entries have already returned. The crash then cancels
  the caller's other entries, as every crash does. This is OTP's `one_for_all`
  behavior, and it matches what structured concurrency requires: a loop
  cannot deliver a result it does not have.
- **Supervisors stop the propagation.** The platform supervises every root
  task. A web server platform can fail only the crashed request and keep
  serving, much as an OTP supervisor restarts one worker. A command-line
  platform can exit with an error.
- **`supervise!` puts a supervisor inside a program.** It runs a function as a
  child task and restarts it after a crash, up to a limit. Past the limit, it
  crashes itself, escalating to its own supervisor. It never reports the crash
  to the code that called it, so it cannot be used to catch errors:

```roc
# Keep a queue consumer running across bugs in individual messages.
run_consumer! : Queue => {}
run_consumer! = |queue|
    Concurrent.supervise!({ max_restarts: 10 }, || consume_forever!(queue))
```

`supervise!` composes with the other combinators. Wrapping each child in its
own `supervise!` gives OTP's `one_for_one` behavior:

```roc
Concurrent.all!(queues.map(|queue| || Concurrent.supervise!({ max_restarts: 10 }, || consume_forever!(queue))))
```

Errors that a program expects, like a failed request, remain `Err` values.
Crashes are for bugs and for conditions the program cannot handle, as they are
today.

### On platforms that do not schedule tasks

On a platform that does not opt in, the program runs its tasks itself, one at
a time on one thread, and hosted calls finish before they return. The next
task to run is always the ready one that comes first in the task tree. So in a
program where no task waits for another, a loop runs its entries one at a time,
in order. Each starts only when the loop asks for the next result, and runs to
completion before the body sees it. Limits and shields change nothing, `spawn`
appends an entry, and the entries not yet started when the loop is left never
start. So:

- `all!` and `both!` run every function and return the same results;
- `all_ok!` stops at the first `Err`, and later functions never start;
- `race!` returns the first function's result, and the others never start.

So the timeout example still returns the fetch result, but the timeout cannot
interrupt a call that blocks.

A task can also wait for another task: at a receive or a `Wait`, under the
companion RFCs. A waiting task pauses, as it would on a task platform, and
the next ready task runs. So a protocol in which two tasks take turns, such
as a permit that must come back before its lender continues, runs on every
platform. If no task can run, nothing can ever end the wait, and the program
crashes with a deadlock message. A crash still stops the whole program on these
platforms, as it does today, and `supervise!` runs its function once. Packages
can use concurrent loops and `Concurrent` freely: they get concurrency where
the platform offers it and sensible sequential behavior elsewhere.

### For platform authors

A platform opts in with a `concurrency` field in its header:

```roc
platform ""
    requires { main! : List(Str) => Try({}, [Exit(I8), ..]) }
    exposes [Stdout, Http, Sleep]
    packages {}
    provides { "roc_main": main_for_host! }
    hosted {
        "roc_stdout_line": Stdout.line!,
        "roc_http_get_utf8": Http.get_utf8!,
        "roc_sleep_millis": Sleep.millis!,
    }
    concurrency: Tasks
    targets: { … }
```

It marks each hosted function that may suspend where that function is
declared, in its hosted type module:

```roc
# Http.roc
Http := [].{
    suspending get_utf8! : Str => Try(Str, [HttpErr(U16)])
}
```

A platform whose host resumes tasks on several threads at once declares
`concurrency: ParallelTasks` instead. Everything this RFC says about `Tasks`
builds applies to both values, except reference counting ("Reference counting
across tasks"): only `ParallelTasks` makes values shared with child tasks
atomic.

A hosted function that is not marked, like `Stdout.line!`, always completes
before it returns and costs nothing extra. A `suspending` declaration in a
platform without `concurrency: Tasks` is a compile error. The platform's host
implements the task ABI below and a scheduler.

## Reference-level explanation

### Terms

- A **suspension point** is a call to a `suspending` hosted function, a call
  through an erased effectful function value in a `Tasks` build without
  lambda-set specialization (see below), a concurrent loop's head or exit, or
  `supervise!`'s wait.
- A procedure is **suspendable** if it may reach a suspension point.
- A **task** is one running effectful function, with its native frames while
  it runs and its records while it is paused. The host starts
  root tasks through `provides` entries; each entry a concurrent loop runs is a
  child task.
- A **wait source** is something a task can park on: its pending hosted
  operation, or the running set of one of its concurrent loops.

### Explicit inputs

Suspension is decided only from explicit data:

- the platform's `concurrency` field. It is part of the compilation key of
  every unit, like the target, because it changes generated code in modules
  (Builtin and packages included) that never import the platform;
- the `suspending` marker on hosted declarations, which is part of the
  declaring module's checked output and so visible to every importer;
- function types: Roc has no effect polymorphism, so `->` versus `=>` decides
  whether a function value can suspend.

### Suspendability

Suspendability is computed over the post-specialization LIR call graph and
recorded on each LIR procedure (`LirProcSpec.suspends: never | may`) and on
each call. Later stages read it and never rediscover it.

- In `None` builds hosted calls never suspend. The suspension points are the
  waits of the companion RFCs (receives and `Wait`), and the loop heads,
  loop exits and `supervise!` waits of procedures whose children may reach
  one. So the pass does nothing for a program that cannot wait.
- Under `.lss`, lambda sets give every call's possible targets, so the answer
  is exact. Calls through erased callables at host boundaries are suspension
  points exactly when the callable's type is effectful.
- Under `.boxy`, a generic body cannot know its callees, and its output must
  depend only on its module and its imports (design.md, "Per-Module Output").
  So in a `Tasks` build every call through an erased effectful function value
  is a suspension point. This is decided by the type and the build's mode
  alone. The same holds in a `None` build, since a module cannot know whether
  a module that imports it waits.
- A pure procedure is never suspendable.

### Capture by unwinding

Tasks run on the native stack of whichever thread resumes them, exactly like
code today. A task's state is captured only when it actually pauses, by
unwinding: each suspendable procedure on the native stack saves what it still
needs into a **record** and returns. The chain of records is the paused task:
a one-shot continuation, captured only on demand. This is the technique of
Pettyjohn et al.'s *generalized stack inspection*, and of Binaryen's Asyncify,
restricted to the procedures Roc knows can pause.

The pass is local to each **suspendable call site**: a call to a suspendable
procedure, a suspending hosted call, an erased effectful call in a `.boxy`
`Tasks` build, a concurrent loop's head or exit, or `supervise!`'s wait. It
needs no whole-program frame layout.

- **Hidden task parameter.** Every suspendable procedure takes the current task
  as a hidden parameter, the way a procedure that may leave a call pending
  takes its caller's pending cell. Erased effectful callables take it too in
  `Tasks` builds.
- **Status check.** After each suspendable call site the caller tests a flag
  in the task header. If it is clear, execution continues as normal; this is
  the entire cost of a call that does not pause.
- **Pending edge.** If the flag is set, the caller saves into a new record the
  values that are live after the call (except the call's own result), a mask
  saying which of them it owns, and the call site's identity. Then it returns.
  Its own caller sees the flag and does the same, up to the task's entry.
- **Resume entry.** Each suspendable procedure gets one resume entry. Given a
  record and the paused call's result, it restores the saved values into a
  fresh native frame and jumps to the code just after that call site. Nothing
  before the call runs again.
- **Drop entry.** For each call site, the pass emits a drop procedure that
  releases the record's saved values selected by its owned mask.
- **Hosted result slots.** Before a suspending hosted call, the caller reserves
  the result's space in the task's record storage and passes that address as
  `out`. On the ready path it reads the result and releases the space at once.
  On the pending path the space becomes part of the record, so the host's later
  write lands in stable memory.

**Resuming innermost first.** Records link from the innermost paused call
outward. Resuming a task runs the innermost record's resume entry. When that
procedure returns a value, the runtime passes it to the next record's resume
entry, and so on outward. Each resume costs only the work that is actually
left, not a re-entry from the task's root. If a resumed procedure pauses
again, the records it creates link to the record that was next in line.

A task has exactly one paused path, because concurrency always goes through
child tasks. A child task that has not started yet is represented by a start
record holding its closure; dropping it releases the closure.

### Position in the pipeline

The pass runs after every pass that changes ownership or frame storage:

```text
LIR construction -> ARC -> frame_callables (.boxy) -> tail_drive
  -> reachable_procs_after_arc -> immortal_locals -> comptime_value_guards
  -> suspension lowering -> diagnostics, backends, LirImage
```

The output is ordinary LIR: flag tests, stores into and loads from records,
calls, returns, a switch at each resume entry, and explicit reference-count
statements. Backends, the interpreter and LirImage need no knowledge of
suspension.

### What ARC must publish

Each drop entry needs to know which saved values its procedure owns at that
call site. ARC does not publish ownership per program point today, and
recovering it from the emitted reference-count statements would be
reconstruction. ARC therefore records an explicit **suspension ledger**: for
each suspendable call site, the values live after it and which of those are
owned. The debug certifier checks the ledger as it checks the rest of ARC's
placement. `immortal_locals` removes elided units from the ledger along with
their statements.

### Values, never native addresses

A resumed procedure gets a new native frame, generally at a different address.
Records therefore hold LIR values, never addresses of native-frame storage.
By-pointer argument passing is a backend matter below LIR, so restoring a
value restores its bytes into the new frame. Two LIR constructs do hold
native-frame addresses, and both are handled with explicit data:

- **Frame callables** (design.md, "Frame Callables"). `frame_callables` does
  not build a callable in the native frame when it is passed to a suspendable
  call site; such a callable is allocated normally. Suspendability is computed
  at LIR construction, before that pass runs.
- **Pending cells** (design.md, "Pending Cells"). A suspendable procedure's
  pending cell lives in task storage instead of its caller's frame.

### Example

```roc
fetch_two! = |a, b| {
    x = Http.get_utf8!(a)?
    y = Http.get_utf8!(b)?
    Ok(Str.concat(x, y))
}
```

lowers to, in pseudo-LIR:

```text
fetch_two(task, a, b):
  slot = reserve(task, Try(Str, [HttpErr(U16)]))
  roc_http_get_utf8(task, slot, a)               # a moves to the host
  if task.pending { save(task, site 1, slot, {b}, owned: {b}); return }
site 1:
  x = take(slot)?                                # Err: decref b, return
  slot = reserve(task, Try(Str, [HttpErr(U16)]))
  roc_http_get_utf8(task, slot, b)               # b moves to the host
  if task.pending { save(task, site 2, slot, {x}, owned: {x}); return }
site 2:
  y = take(slot)?                                # Err: decref x, return
  return Ok(Str.concat(x, y))

resume entry (record):  restore saved values; jump to record.site
drop entry site 1:      decref b
drop entry site 2:      decref x
```

If neither request pauses, the procedure runs straight through with two flag
tests added, and no record is created.

### Calls between suspendable procedures

- **Direct calls and lambda sets** need nothing beyond the status check; each
  callee unwinds itself.
- **Recursion** needs nothing special. A deep effectful recursion that pauses
  leaves one record per level, as much state as the native frames it replaces.
- **Frame-replacing calls** keep their guarantee of constant space. A
  frame-replacing call has no code after it, so it has no status check and the
  caller leaves no record: the pending flag passes straight up. Under the
  driver schemes used on wasm and LLVM-on-wasm (design.md, "Frame-Replacing
  Calls"), the driver loop treats the flag like any suspendable call site.
- **Erased calls** check the flag like direct calls. A record names its call
  site's resume and drop entries directly, so resuming never needs the erased
  callee's static type.
- **Deferred erased calls** (design.md, "Deferred Erased Calls") need two
  rules. First, a waiter that makes pending calls treats each call it makes
  as part of its own call site: if that call pauses, the waiter saves a record
  at that site, and the site's resume entry first goes on making pending calls
  with the value it is given. Second, a site that hands a pending call up
  (`handed_up`) treats the pending result as an ordinary value: its resume
  entry returns it, and it reaches the next record out like any other result,
  until a waiter makes the call. Because the pending cell is written by a
  procedure that may have paused and resumed since its waiter's native frame
  was replaced by a record, suspendable procedures' pending cells live in task
  storage (C4). No call is ever pending while a task is parked: a pending call
  arises at a return and is made before anything else can pause. A paused
  cycle of deferred calls holds records only for the waiter and the procedure
  that paused: a procedure that handed its call up has already returned.

### Host ABI

Shown in C. Only `concurrency: Tasks` platforms see any of it. Compiled Roc
code and hosts share linker symbols, with no operation table (design.md:
`RocOps` "is not part of any host ABI"). The new runtime symbols join the fixed
set in `host_abi.ExternHostFns` and the glue templates for opting-in
platforms.

```c
typedef struct RocTask RocTask;                          // opaque, owned by Roc
typedef enum { ROC_READY, ROC_PENDING } RocPoll;
typedef enum {
    ROC_TASK_PENDING, ROC_TASK_DONE, ROC_TASK_CANCELLED, ROC_TASK_CRASHED
} RocTaskStatus;

// A `provides` entry, under its header symbol, starts a root task.
// The result is written to result_out when the task completes.
RocTask *roc_main(RocList args, void *result_out);

// Exported by the Roc runtime.
RocTaskStatus roc_task_resume(RocTask *task);
void roc_task_wake(RocTask *task);      // a pending hosted result has been written
void roc_task_cancel(RocTask *root);    // request cancellation of a root task
void roc_task_free(RocTask *root);      // after DONE, CANCELLED or CRASHED
RocStr roc_task_crash_message(RocTask *root);  // after CRASHED; borrowed

// Implemented by the host.
void roc_task_ready(RocTask *task);     // resume this task once, on any thread
void roc_task_abort(RocTask *task);     // abandon the operation it is waiting on

// A suspending hosted function gains its task and its result slot.
RocPoll roc_http_get_utf8(RocTask *task, HttpResult *out, RocStr url);
```

**What the Roc runtime guarantees.** Every wait source has an atomic wake word:
a parked bit and a count of completions that have been published but not yet
consumed. A task's pending hosted operation has one, and so does each running
set; a task parks on exactly one at a time.

- A completion (a hosted result, or a report from an entry of a running set)
  first publishes its data, then increments its own source's count in one
  atomic step that also clears the parked bit. It calls `roc_task_ready` only
  if the task was parked on that source.
- Each wait consumes one completion from its source and decrements that count.
  Parking is a compare-and-swap on the source's word that succeeds only when
  its count is zero; otherwise the task continues with the completion that is
  waiting for it.
- So `roc_task_ready` is called at most once per park, a host never resumes a
  task twice for one wake or resumes a running task, and a completion that
  arrives while the task is still running (an entry finishing on another
  thread, or a hosted result arriving before the hosted function has
  returned) is never lost.
- One word per task is not enough. A loop body can wait on a hosted call while
  the loop's entries keep finishing, and nested loops wait on different
  running sets. With one word, a report readies, or lets continue, a task whose
  current wait has nothing to deliver. The
  [prototype](concurrency-prototype/README.md) finds this in its fuzzer, and its
  Maude model finds it in every interleaving of one hosted call and one entry.
- A single "woken while running" flag is not enough either, even if each wait
  clears it: it cannot tell a pending wake from one whose report an earlier
  wait has already consumed, and the task then resumes with nothing to
  deliver. The model finds this in a plain `all!` over two children on two
  threads.
- An entry counts as finished, and reports to its parent, only when it and
  every task it started have completed or been dropped.
- Roc owns child tasks. After `roc_task_resume` returns for a child, the host
  forgets it.

**What the host guarantees.** "Platform obligations" under Invariants restates
these rules as the targets of the conformance harness.

1. **Resume.** It resumes a root once after creating it and once per
   `roc_task_ready`. A resume on another thread must happen-after the call that
   made it necessary; any mutex or queue handoff provides this. The plain
   reference counts of values confined to one task depend on it.
2. **Completion.** After a suspending hosted function returns `ROC_PENDING`,
   the host writes `*out` exactly once and then calls `roc_task_wake(task)`.
   Arguments are owned by the host either way, as for every hosted call.
3. **Abort.** After `roc_task_abort(t)` returns, the host never writes `t`'s
   result slot or wakes `t`. If the abandoned operation finishes anyway, the
   host releases its result.
4. **Roots.** After a root returns `ROC_TASK_DONE`, `ROC_TASK_CANCELLED` or
   `ROC_TASK_CRASHED`, the host reads `result_out` (only for `DONE`) or the
   crash message (only for `CRASHED`), and then calls `roc_task_free`. To
   cancel a root, it calls `roc_task_cancel` and keeps resuming the root when it
   is readied until it reports a terminal status. A crashed root is the
   platform's supervision decision: it may start the entry again, fail one
   request, or exit.
5. **No nested resumes.** A hosted function never calls `roc_task_resume`, and
   a resume never runs inside another resume on the same thread.
6. **Allocation across threads.** When tasks can move between threads, an
   allocation made on one thread may be released on another. The platform's
   `roc_alloc`, `roc_dealloc` and `roc_realloc` must allow that, or its
   scheduler must keep every task, and every value tasks share, on one
   allocator's thread.

Breaking these rules is a host bug with undefined behavior, as with a
`roc_alloc` that returns null today. Debug builds of the runtime check every
wake-state transition and trap on an invalid one.

A minimal single-threaded host loop:

```zig
var result: MainResult = undefined;
const root = roc_main(args, &result);
scheduler.push(root);
while (scheduler.pop()) |task| switch (roc_task_resume(task)) {
    .pending => {}, // a hosted completion or child will ready it again
    .done => if (task == root) { finish(result); roc_task_free(root); },
    .cancelled => if (task == root) roc_task_free(root),
    .crashed => if (task == root) { report(roc_task_crash_message(root)); roc_task_free(root); },
};
// roc_task_ready(task) pushes onto `scheduler`.
```

### Concurrent loops

**Plans.** `Concurrently(a)` is an opaque builtin type built by pure functions:

```roc
List.concurrently : List(() => a) -> Concurrently(a)
Concurrently.limit : Concurrently(a), U64 -> Concurrently(a)    # crashes if the limit is 0
Concurrently.shielded : Concurrently(a) -> Concurrently(a)
Concurrently.background : Concurrently(a) -> Concurrently(a)
Concurrently.concat : Concurrently(a), Concurrently(a) -> Concurrently(a)
```

A plan is a tree of entries, limits, shields and background marks. It owns its
closures and is reference counted like any value. It holds no tasks.

Two effectful methods run a plan without a loop body, and return its results in
input order:

```roc
Concurrently.all! : Concurrently(a) => List(a)
Concurrently.all_ok! : Concurrently(Try(a, err)) => Try(List(a), err)
```

**The `for!` protocol.** `for!` already calls `stream` on what it loops over. A
value of type `Concurrently(a)` is a third case, selected by its type. The loop
creates a **running set** that exists only inside it. No expression names the
running set, so it cannot be stored, captured or returned.

**Lowering.** Before the suspension pass, a concurrent loop is lowered to four
internal operations, each with explicit `RcEffect` data:

| Operation | Meaning | In a `None` build where no entry can wait |
|---|---|---|
| `group(plan)` | Flatten the plan into entries in input order, fix each entry's place in the task tree, and start every entry whose limits have room | Record the entries; start nothing |
| `next(g)` | A suspension point: wait for the next finished entry and report `Arrived(v)` or `Cancelled`. Once no entry is left but background ones, cancel those, wait until they have stopped, and report `Ended` | Run the earliest unstarted entry to completion and report `Arrived(v)`. Once every unstarted entry is background, discard them and report `Ended` |
| `spawn_in(g, f)` | Add an entry under the limit at the root of the loop's plan, if its root is a limit, and start it when that limit has room | Append an entry |
| `cancel_group(g)` | Discard unstarted entries; cancel running ones unless they are shielded and have begun | Discard unstarted entries |

The middle column is the meaning in every build. A `None` build runs it with
the program's own scheduler, which resumes the ready task that comes first in
the task tree. Where no entry can wait, that schedule starts each entry only
when the loop asks for the next result and runs it to completion, so a `None`
build of such a loop needs no tasks: the last column gives the same results,
and the compiler uses it.

A loop becomes:

```text
g = group(plan)
head:
  match next(g)
    Arrived(v) => body      # an edge back to the head continues the loop;
                            # every edge out of the body runs exit(g) first
    Ended      => after the loop

exit(g): cancel_group(g); repeat next(g) until Ended, discarding Arrived and
         Cancelled
```

The body never sees `Cancelled`; only the exit sequence does. `next` never
reports a crash. Once one of a task's entries has crashed, the task takes no
more results: the runtime crashes it where it waits, and a `next` called while
a crash is pending suspends so that the runtime can crash it there (see "Crash
mechanics"). Exits are edges
the compiler already knows, so waiting at an exit is an ordinary suspension
point, and no drop ever waits.

**Entries.**

- Each entry's place in the task tree is fixed by `group` or `spawn_in`, in
  input order. A child's identity does not depend on when the schedule starts
  it.
- A limit counts the entries running under it. When one of them finishes, the
  runtime starts the next eligible entry at once, without waiting for the loop
  head. An entry added with `spawn` counts against the limit at the root of
  the loop's plan, so `for! x in plan.limit(8)` never runs more than eight
  entries, however many the body spawns. A plan whose root is not a limit,
  such as `a.limit(2).concat(b)`, puts spawned entries under no limit.
- A **background** entry does not keep its loop going. The loop reaches its end
  once every other entry has finished, and the background entries still
  running are then cancelled, as at an exit. A deadline or a progress reporter
  is a background entry. Spawned entries are never background.
- An entry has **begun** once its task has run any code. A loop exit cancels an
  entry that has not begun even if it is shielded, so it never starts. This
  keeps a `None` build's behavior a schedule of a `Tasks` build (C9).
- A crash ends the loop's task like any crash, which cancels every entry,
  shielded ones included. Shields protect entries from exits, not from crashes.
- An entry's crash crashes the loop's task at once, wherever it waits, and the
  task takes no more arrivals (see "Crash mechanics").

**Sessions.** Each entry is the parent's end of a session whose protocol is a
single message: the entry's result. `Arrived(v)` is that message, and `Ended`
says every session in the set has closed. The companion RFC lets an entry's
protocol carry more messages; arrivals keep this shape.

### The `Concurrent` library

```roc
Concurrent :: [].{
    both! : (() => a), (() => b) => (a, b)
    all! : List(() => a) => List(a)
    all_ok! : List(() => Try(a, err)) => Try(List(a), err)
    race! : (() => a), List(() => a) => a
    supervise! : { max_restarts : U64 }, (() => a) => a
}
```

- `Concurrently.all!` wraps each entry so its result carries its index, runs
  a loop over the plan, and stores each arrival by index.
  `Concurrently.all_ok!` is the same loop, leaving it at the first `Err` to
  arrive. Limits, shields and background marks keep their meaning, and the
  results of background entries are not in the list.
- `Concurrent.all!(fs)` is `fs.concurrently().all!()`, and likewise
  `all_ok!`. `both!` is `all!` over two results wrapped in a tag union.
- `race!` leaves its loop at the first arrival.
- `supervise!` is the one place that reacts to a crash, so it is not written
  with a loop. It uses the runtime's internal `task_spawn` and
  `task_await_any`, whose `Crashed` report programs cannot reach. It spawns its
  function, awaits it, and spawns it again after a crash, until `max_restarts`
  restarts have happened. Then it crashes with the last message.

In `None` builds these give the behavior described above with no separate
implementation, because they are loops.

### Crash mechanics

A crash happens wherever a builtin, an overflow check or `crash` stops
execution, usually deep inside native frames of a running task, not at a
suspension point. In `Tasks` builds, the runtime takes a non-local exit back
to the `roc_task_resume` that is running the task: through `src/sljmp` on
native targets, and through WebAssembly exception handling (part of
WebAssembly 3.0) on wasm. Where neither is available, a crash ends every task
in the process, as crashes do today. Then:

1. The crashed task is marked dead and never resumed. Values held only by its
   native frames, the procedures that were running when it crashed, are
   leaked, because their state at the moment of the crash is unknown. Freeing
   nothing that might still be reachable keeps this safe. The task's remaining
   records were saved at call sites and are consistent, so they are dropped
   exactly. Any children it had already started are cancelled and dropped
   exactly, and the crash is reported upward only after they are gone.
2. If the crashed task was a loop entry, its parent crashes with the same
   message. A parent that is paused, whether at that loop's head, at another
   loop's head or exit, or on a hosted call, crashes there at once: its pending
   hosted operation is aborted, and arrivals it has not taken are dropped. Its
   state is entirely in consistent records, which are dropped exactly, not
   leaked. A parent that is running, which happens only in `ParallelTasks`
   builds where the entry crashed on another thread, takes no result
   afterwards: its next suspension point, a `next` included, suspends, and the
   runtime crashes it there in the same way. Either way its loops' other
   entries are cancelled and dropped exactly. A child of
   `supervise!` is not a loop entry: its crash is reported to `supervise!` at
   its wait, as a `Crashed` report that programs cannot reach.
3. Propagation stops at a `supervise!`, or at a root, which reports
   `ROC_TASK_CRASHED` to the host.

Only the task in which the crash happened can leak, and only what its running
native frames owned. A platform that wants that memory back can use per-root region
allocation, as the `nea` server does per request, as long as it frees a region
only when nothing outside the crashed tree can reach it.

### Reference counting across tasks

[Atomic Reference Counts Across Tasks](concurrency-rc-atomicity.md) derives
this section from first principles. [rc-atomicity/](rc-atomicity/README.md)
checks and measures it with three tools:
- a Maude model searched over every interleaving;
- Lean proofs of its two theorems;
- a Python prototype with a typed program fuzzer.

**What must be atomic.** A count update must be atomic only when another
thread may update the same count at the same moment. Handing an allocation
from one task to another is not sharing: host rule 1 orders the two tasks, so
a value moved into a child, or returned from one, can keep plain counts. An
allocation needs atomic counts only if two tasks that may run in parallel can
both reach it, or if the host can.

**Parallelism is an explicit platform input.** The `concurrency` field takes
one of three values:

| Value | Meaning |
|---|---|
| `None` | No tasks; concurrent loops run their entries one at a time |
| `Tasks` | The host runs at most one resume at a time (P10). It may move tasks between threads, under host rule 1 |
| `ParallelTasks` | Resumes may run at the same time on several threads |

In `Tasks` builds no two tasks' updates can race, so spawning adds nothing,
and the rest of this section does not apply. That covers wasm hosts without
threads, JavaScript event loops and single-threaded `epoll` or `io_uring`
loops. Only `ParallelTasks` builds pay for sharing between tasks.

**Why the current analysis is not enough.** The existing analysis (design.md,
"Thread-Confined Reference Counts"; `computeVisibilityFromFacts` in
`src/lir/arc_solve.zig`) joins values into classes with a union-find. It
links containers with their contents in both directions, and links every call
site of a procedure specialization with its parameters. Seeded at every
spawn, one shared value would therefore make atomic every value that passes
through a specialization the shared value also passes through, together with
everything those values contain or are contained in.

Suppose a child captures a `Dict(Str, U64)`, and the program also builds
dictionaries of that type with `Dict.insert`. Then every dictionary of that
type gets atomic counts, including in code that has nothing to do with
concurrency. At worst every count becomes atomic. That cost has been
measured: always-atomic counts (roc-lang/roc#7718, March 2025) slowed the
previous compiler's benchmarks by up to 62.5%:

| Benchmark | Slowdown |
|---|---|
| `n_queens` | 62.5% |
| `r_b_tree_ck` | 34.8% |
| `deriv` | 25.6% |
| `c_fold` | 7.6% |
| `quicksort` | not significant |

**The proposal: modes, solved as a least fixpoint.**

- **Modes.** Each local gets a mode for each *path* into its value. A mode is
  plain or atomic, and a path is a sequence of field, element and payload
  steps, with recursive layouts collapsed.
  - Atomic is always sound, so the analysis looks for the least assignment.
  - Plain and atomic updates may be mixed on one allocation as long as they
    never race. This replaces design.md's stronger requirement that every
    statement touching an allocation agrees on its atomicity.
- **Seeds at spawns.** In `ParallelTasks` builds, a child's captured
  parameter is seeded atomic only if its argument may alias an argument
  of a sibling. A capture that no sibling can reach is a move, and stays
  plain however large it is.
  - The aliasing question needs a points-to analysis that keeps call sites
    apart.
  - The existing union-find is too coarse for this decision. In the prototype,
    two trees that merely passed through the same helper were treated as
    shared.
- **Seeds at the host.** Host seeds keep today's equality classes. The host
  can receive an allocation that existing aliases still hold, so aliases made
  before the hosted call need the mode too. The Maude model finds a race when
  host seeds propagate forward only.
- **Forward propagation.** Every other edge points forward, from source to
  target:
  - copy, call argument to parameter, and return to result;
  - a stored value to its field of the new container;
  - a field of a value to the local projected out of it.

  None of these edges points back, which is what ends spreading:
  - a new container holding a shared value is not itself shared;
  - code that builds a value before the fork, or uses it after the join, keeps
    plain counts.
- **Mode specialization.** Procedures are specialized by their parameters'
  modes, as Roc already specializes them by type. In practice this means one
  copy for code inside children and one for code outside. Specialization is
  necessary: without it, a helper shared with the children keeps the
  parent's private work atomic.
- **Lending.** Combinators await every child before returning (G5), and a
  parent is dropped only after its children (R3), so the parent's paused
  record keeps every capture alive while any child runs. Children therefore
  borrow their captures instead of owning them.
  - A child that only reads shared data updates none of its counts. Only
    handles a child keeps in its result can still race.
  - Spawned closures need a borrowing variant of the erased-callable
    convention, whose arguments are all owned today.

**Results.**

- **Theorem A (soundness).** Any object two parallel children both touch is
  shared, and both touches are atomic. It is proved in Lean (`window_sound`)
  and checked in Maude over every interleaving.
- **Theorem B (optimality).** The least solution lies below every solution,
  so it is optimal for any monotone cost, and solving it is linear-time
  reachability (`reach_optimal`).
- **Static limit.** One case is beyond any static mode: a recursive layout
  whose nodes are shared in one place and new in another, as when a child
  path-copies a shared tree. *Edge modes* close it, if a benchmark shows it
  matters: one bit in each handle at such positions, set when the handle is
  stored and tested only there.

The prototype counted the updates each analysis makes atomic although they
cannot race. These are counts of events; no timings were measured.

| Program | Current analysis | Forward | Forward + mode specialization | Per-object marking |
|---|---|---|---|---|
| A helper shared with children does the parent's private work | 2,081 | 1,950 | 2 | 3, plus a test on all 2,139 updates |
| Children read a shared tree | 705 | 0 | 0 | 23 |
| Children path-copy a shared tree | 899 | 459 | 70 | 23 |
| 3,000 generated programs | 121,736 | 77,370 | 70,968 | 59,004, plus a test on all 213,337 updates and 14,352 objects walked |

With lending, 2 of the 166 racing updates remain when children read a shared
tree.

**Alternatives.** Lean 4 and Koka choose atomicity per object at run time. A
negative count marks an object as shared, a spawn marks everything its
closure reaches, and every count update tests the sign. For Roc that means:

- **A test on every count update in the program.** It is cheap when it
  predicts well: the previous compiler's per-object bit ran about 4% behind
  plain counts (commit `f901946455`).
- **A walk at every spawn,** over moved data as well as shared data.
- **Permanent marks.** Data shared once keeps atomic counts after the
  children finish.
- **Lost in-place updates.** Lean (`lean_is_exclusive`) and Koka
  (`kk_block_is_unique`) never treat a marked object as unique. The
  previous compiler's dynamic bit stalled on the same interaction: it never
  marked values read out of shared containers.

Outside host data, whose equality classes this proposal leaves as they are,
everything per-object marking catches that the least static solution misses
is the static limit above. Edge modes cover it without walks or permanent
marks.

Biased reference counting is not proposed either. It keeps a non-atomic count
for an owner thread and an atomic one for the others, as free-threaded
CPython does. That adds an owner field and a merge protocol to every
allocation, to handle unstructured sharing that structured tasks do not have.

**Real sharing costs the most either way.** When two threads update one
count, its cache line moves between cores. A C microbenchmark (clang `-O3`,
Apple M5 Pro, noisy to about ±50%) gave these per-node costs relative to a
plain fold of the same 1,000-node list:

| Update | Relative cost |
|---|---|
| Uncontended atomic | about 1.2–2 times |
| Sign-tested, on unmarked data | within noise |
| Contended: two threads folding the list | 3–25 times |

No choice of atomicity makes contended updates cheaper; lending removes them.

**What changes in the compiler.**

- **ARC.** The thread-confined analysis keeps its host seeds and equality
  classes. It adds:
  - spawn seeds, through explicit `RcEffect` data on `group`, `spawn_in` and
    `task_spawn`: a plan's closures, and what they capture, are what entries
    may share;
  - forward edges over (local, path) facts;
  - mode specialization.
- **Helper plans** are selected by layout and mode, where a mode is a set of
  positions closed under going deeper, instead of by a single atomicity flag.
- **Backends** keep following the explicit atomicity on each RC statement.
- **The debug certifier** checks the constraints locally, statement by
  statement (C6).

### Compile-time evaluation and the interpreter

Effectful code never runs at compile time, so compile-time evaluation is
unaffected. The interpreter runs the lowered LIR like any backend.

### Cost model

Apart from the microbenchmarks under "Making it fast", nothing here has been
measured. The figures are first-principles estimates to be replaced by
ReleaseFast benchmarks on the reference host.

| Situation | Cost |
|---|---|
| Pure code | None |
| A program that cannot wait, on a platform without `concurrency: Tasks` | None: the pass does nothing |
| A program that can wait, on a platform without `concurrency: Tasks` | A flag check after each call that may reach a wait, and pausing and resuming at the waits themselves. Hosted calls never pause. The companion RFC measured checks on 58% of procedures in such programs, within noise natively and 40% to 58% on a wasm kernel made only of calls |
| Effectful call that does not pause | One load and predictable branch on the task's pending flag; suspending hosted calls return their result through `out` |
| Code size | A save block per suspendable call site, plus resume and drop entries, in effectful code only. Asyncify's roughly 50% comes from instrumenting conservatively; this pass touches only code that can pause, usually a small share of a program |
| Spawning a child task | One allocation for the task header and its first record, wake-state setup, and a host enqueue: estimated tens of nanoseconds |
| Entering a concurrent loop | Flattening the plan, plus one child task per entry as it starts. The loop head and each exit wait are suspension points |
| Pausing and resuming | Saving and restoring a few values per paused procedure, one or two compare-and-swaps for the wake protocol (more under cross-core contention), the host's queue, and a `setjmp` per resume for crash exits |
| Values shared with child tasks | None in `Tasks` builds. In `ParallelTasks` builds, atomic counts on the positions children may share, and in the code that runs on them, but not in code before the fork or after the join. Children borrow their captures, so read-only sharing updates no counts. Truly contended updates cost the most, and only lending removes them ("Reference counting across tasks") |
| `.boxy` dev builds | A flag check after every erased effectful call |

For I/O-bound work these costs are small next to the system calls they wrap,
which take microseconds. Atomic counts and allocation will matter more than
pausing itself.

### Making it fast

1. **Don't pause when you don't have to.** A hosted function that can finish at
   once (buffered data, a cache hit) returns ready, which costs one branch.
2. **Resume innermost first** (above), so resuming costs the work that is left,
   not a re-entry from the task's root.
3. **Reuse memory.** Records follow a predictable lifetime order, so each task
   keeps them in a reusable buffer, and task headers come from per-thread free
   lists. That keeps `roc_alloc` off the hot path, as Swift's task allocator
   does.
4. **Cheap wake-ups.** A completion on the task's own thread needs no atomics;
   other threads need one compare-and-swap per park. Host schedulers can run a
   parent woken by its last child next, like tokio's "LIFO slot".
5. **Contain atomic counts.** Declare `Tasks` rather than `ParallelTasks`
   unless the host really runs tasks in parallel. In `ParallelTasks` builds,
   seed only captures a sibling can reach, propagate modes forward, specialize
   procedures by mode, and let children borrow their captures ("Reference
   counting across tasks").
6. **Batch spawns.** `group` allocates all of a plan's entries in one block.

A stackful switch saves a fixed set of registers at any depth; unwinding saves
only live values, but once per paused procedure. A hand-lowered C
microbenchmark ([concurrency-prototype/bench](concurrency-prototype/README.md))
measures both, on an Apple M5 Pro with clang at `-O2`:
- The status check after each call that does not pause costs 2.5% to 5% in a
  kernel made only of calls.
- One pause and resume, with two live values per frame, costs about 5 ns at
  depth 1, 12 ns at depth 4 and 40 ns at depth 16 with records from a per-task
  buffer or free list. With records from `malloc` it costs five times as much.
- A stackful switch costs 21–44 ns over the same depths, before allocating
  each task's stack. Unwinding is cheaper up to about 16 paused procedures.

These measure the lowering, not a compiler, and stand until ReleaseFast
benchmarks on the reference host replace them.

## Invariants

The design rests on four layers of invariants:

- **Language guarantees** are what Roc programmers may rely on.
- **Compiler invariants** hold for every compiled program.
- **Runtime invariants** hold for the task runtime, whatever program runs on it.
- **Platform obligations** are what every opting-in host must satisfy.

Every language guarantee holds if the compiler and runtime invariants hold and
the platform meets its obligations. Each invariant below names how it is
established:

- **Certify**: checked by debug-build LIR certification on every compiled
  program. Following AGENTS.md, these are compiler invariants: debug
  assertions in debug builds and `unreachable` in release builds.
- **Model**: proved once on a small executable model of the runtime and the
  host contract, explored exhaustively over bounded numbers of tasks, threads
  and event orders (for example with TLA+, or a Zig schedule explorer).
- **Differential**: the same programs run two ways must produce the same
  results, effect traces and allocation balance.
- **Conformance**: a debug harness that a host links against records every ABI
  event and checks the platform obligations as they happen, driven by a suite
  of adversarial scenarios. Violations are host bugs, not compiler invariants.

### Language guarantees

| | Guarantee | Rests on |
|---|---|---|
| G1 | **Pure code is never interrupted.** Tasks interleave only at suspension points. | C1, C2, R2 |
| G2 | **No data races.** Tasks share only immutable values, and counts on shared values are atomic. | C6, R2, P2, P6, P10 |
| G3 | **Concurrency does not change meaning.** On a platform without tasks, a program behaves as it would on a task platform with one thread, whose hosted calls finish before they return, and which always resumes the ready task that comes first in the task tree. A program in which no task waits for another therefore starts each loop entry only when its loop asks for the next result, and runs it to completion. | C1, C9 |
| G4 | **Fixed result order.** Combinators return results in input order. Only effect interleaving and the order in which loop bodies see arrivals depend on scheduling, and through them which `Err` arrives first and which `race!` entrant wins. | R1, R3, Model of combinators |
| G5 | **Structured lifetimes.** No task outlives the loop that started it: every way out of a loop cancels or waits for each of its entries, and the running set never escapes the loop. | C11, R3, R4 |
| G6 | **Silent, exact cancellation.** A cancelled task runs no more of its own code and releases everything it owns. | C5, R4, P3, P4 |
| G7 | **OTP crash semantics.** A crash ends its task, propagates at once through links to the nearest supervisor, is never observable to other code, and does not affect tasks outside that supervisor's subtree. | R5, R7, P1, P8 |
| G8 | **Tail calls stay constant-space**, including between effectful functions. | C7 |
| G9 | **Plan properties.** No more than `n` entries under `limit(n)` run at once, counting those spawned into a loop over a limited plan; a loop exit waits for a shielded entry that has begun instead of cancelling it; and a loop ends once only background entries are left, cancelling them. | R8 |

### Compiler invariants

| | Invariant | Established by |
|---|---|---|
| C1 | **Transparency.** If every suspending call completes immediately, the lowered program's results, effects and crashes equal those of the program without the pass. | Differential: a host that always returns `READY`, against a `None` build |
| C2 | **Suspension only where declared.** A procedure marked `never` contains no suspendable call site; the pending flag is set only at a suspension point; and every suspension point traces to a `suspending` marker, an erased effectful call in a `.boxy` `Tasks` build, a concurrent loop's head or exit, or `supervise!`'s wait. The companion RFC adds channel and inbox receives. | Certify |
| C3 | **Record completeness.** At every suspendable call site, the pending edge saves every value the procedure uses after that call, other than the call's result. | Certify |
| C4 | **No native addresses in records.** A record never holds the address of native-frame storage: frame callables passed to suspendable call sites are heap-allocated, and suspendable procedures' pending cells live in task storage. | Certify |
| C5 | **Ownership conservation.** At each suspendable call site, the suspension ledger equals what ARC owns after the call. Dropping a record releases each owned value exactly once, and so does resuming it to completion. | Certify, plus cancel-at-every-site tests with allocation tracking |
| C6 | **Atomicity soundness.** An allocation that two tasks able to run in parallel can update counts on, or that the host can reach, uses atomic counts. Hosted arguments and results are seeds, closed under today's equality classes. In `ParallelTasks` builds, an entry's capture that a sibling may alias is a seed too, and modes propagate forward along copies, calls, returns, construction and projection; child results are not seeds. Every count statement on a position whose mode is atomic is atomic, plain and atomic updates of one allocation never race, and a borrowed capture is never released by a child. The companion RFC adds sent messages as seeds (S8). | Certify, checking each forward constraint locally (extends the existing rule) |
| C7 | **Constant-space tail calls.** A frame-replacing call has no status check and leaves no record, so a paused tail cycle holds records only for the procedures actually live. | Certify, plus record-count tests on deep tail cycles that pause |
| C8 | **Strategy equivalence.** `.lss` and `.boxy` builds produce the same traces under the same deterministic schedule. | Differential |
| C9 | **Sequential equivalence.** A `None` build's results and effects equal a `Tasks` build's under the coroutine schedule: one thread, hosted calls that finish in place, and the ready task first in the task tree resumed next. Where no entry can wait, this is the input-order schedule, which starts each loop entry only when its loop asks for the next result and runs it to completion, and a `None` build runs it without tasks. The companion RFCs' waits need no change to the schedule (S7). | Differential: a deterministic test host with that policy, against a `None` build |
| C10 | **Resume fidelity.** A resume entry continues at exactly the call site its record names, with every restored value equal to the saved one. Records resume innermost first, and each result reaches the record that was next in line, exactly as the returns would have. | Certify the entry tables; Differential: a host that pauses every suspending call, against one that never pauses |
| C11 | **Loop exits.** Every edge out of a concurrent loop's body, other than a crash, passes through the loop's exit sequence, and a running set is used only by its own loop's operations. | Certify |

### Runtime invariants

All of these are safety properties except R6, which is liveness. All are
established by **Model**, with the wake-state machine also run under
ThreadSanitizer with real threads.

| | Invariant |
|---|---|
| R1 | **Exactly-once readiness.** For each park there is exactly one `roc_task_ready`, from a completion of the source the task parked on. A completion that arrives before the task waits on its source is never lost: it is counted in that source's wake word, and the task continues instead of parking when it waits there. No wake readies a task twice, and no task is resumed or continues without a completion to deliver. |
| R2 | **Exclusive execution.** Given P1 and P2, a task never runs on two threads at once and is never re-entered while running. |
| R3 | **Tree shape.** Live tasks form a forest. A task reaches a terminal state only after all of its descendants have, and a parent processes a child's report only after that child's subtree is gone. |
| R4 | **Cancellation finality.** After cancellation is requested, a task runs no more Roc code once it next parks (at once, if already parked). Its drop runs exactly once and runs no user code. |
| R5 | **Crash containment.** After a crash in task `t`: `t` never runs again and is never dropped; children `t` started are dropped exactly; propagation stops at the nearest `supervise!` or root; tasks outside that supervisor's subtree are unaffected; nothing reachable is freed; `t`'s records are dropped exactly; and the only leak is what `t`'s running native frames owned. |
| R6 | **Progress.** Tasks wait only on their own hosted operation or their own loops' entries, so waits form no cycles. Given P7, every loop whose entries' work terminates is left, crashes or is cancelled. Receives (companion RFC) can wait in cycles; a task tree stalled on them crashes the receiver that waited last instead (S6). |
| R7 | **Bounded restarts.** `supervise!` restarts at most `max_restarts` times and then crashes with the last crash message. |
| R8 | **Limits.** A running set never has more than `n` running entries under a `limit(n)`, and starts an eligible entry as soon as one under the same limit finishes. |

### Platform obligations

Each is established by **Conformance**. Breaking P1–P6 or P8–P10 is
undefined behavior; breaking P7 can only hang tasks.

| | Obligation |
|---|---|
| P1 | **Resume discipline.** Resume a root once after creating it and a task once per `roc_task_ready`. Never resume from inside a hosted function or inside another resume. |
| P2 | **Handoff ordering.** A resume on another thread happens-after the event that readied the task. The counted wake word already orders every resume that follows a wake, since the resume reads the word with acquire ordering after the completion's release, so P2 is what orders a task's first resume after it is spawned, and the host's own data. |
| P3 | **Completion.** For each `ROC_PENDING`, write `*out` exactly once, then call `roc_task_wake` exactly once, and do neither after an abort. |
| P4 | **Abort finality.** After `roc_task_abort` returns, never touch that task's result slot or wake it, and release any result that arrives late. A host meets this by writing the result and calling `roc_task_wake` while holding the operation's lock that `roc_task_abort` also takes; waking after releasing that lock can wake a freed task. |
| P5 | **Ownership.** Release hosted arguments exactly once, even when the operation completes later; ownership of what is written to `*out` passes to Roc. |
| P6 | **Cross-thread allocation.** Allow an allocation to be released on a different thread, or confine tasks and shared values to one allocator's thread. |
| P7 | **Liveness.** Eventually resume every readied task, and eventually complete or abort every pending operation. |
| P8 | **Root lifecycle.** After a terminal status, read `result_out` only for `DONE` and the crash message only for `CRASHED`, then free the root exactly once and never use it again. |
| P9 | **Region safety**, for platforms that reclaim a crashed tree's memory in bulk: free a region only when nothing outside the crashed tree can reach it. |
| P10 | **Serial resumes**, for `concurrency: Tasks` platforms: never run two resumes at the same time. Each resume happens-after the previous one ends. |

### What to establish first

The wake protocol and crash containment (R1, R3–R5) are the parts where a
subtle mistake corrupts memory under rare interleavings, and they can be
settled on a model before the compiler pass exists. Record completeness,
native-address freedom and ownership conservation (C3–C5) are next: they carry
memory safety for every program and fit the existing certifier. The platform obligations become
testable once the first reference host exists.

[concurrency-prototype/](concurrency-prototype/README.md) establishes the
first group on models:
- `maude/wake.maude` searches every thread interleaving of the wake protocol,
  of a hosted completion racing cancellation, and of a loop body waiting on a
  hosted call while an entry reports. It finds the flag protocol and one wake
  word per task unsound, and one counted word per wait source sound.
- An executable Python model of the lowering, runtime and host runs fuzzed
  programs against a stackful oracle under adversarial schedules, checking
  C1–C5, C7–C11, G3–G5, G9 and R1–R8, with exact heap accounting after every
  run, including after crashes and cancellation at every suspension. It
  includes deferred erased calls and their pending cells, and concurrent loops
  over generated plans whose bodies leave early, return errors, spawn more
  entries and nest. The `Concurrent` library is written with those loops.
- A conformance harness runs hosts that each break one of P1 or P3–P10, and
  the runtime's monitors or the exact heap catch every one. Conforming hosts
  pass, including one with a thread-confined allocator and one that reclaims
  a crashed root's memory as a region.
- A coverage matrix maps every row of the tables above to the regression
  tests that establish it, and a test keeps the two in step.
- `bench/wake_tsan.c` runs the counted wake word on real threads under
  ThreadSanitizer: it is race-free and readies each park exactly once, and
  both flag protocols break R1 there too.
- The same model runs the companion RFC's channels, mailboxes, semaphore
  libraries and many-root services, and establishes its S1–S7, S9 and S10 in
  the same way, with the same coverage matrix. Its S8, that sent values are
  seeds, is established in the atomicity prototype.

Fuzzing composes small typed effectful constructs (loops, matches, closures,
recursion, plans, concurrent loops, combinators) with randomly placed
suspension points, crashes and cancellations, as AGENTS.md requires, and
checks C1, C5, C8 and C10 on what it generates.

## Drawbacks

- **New compiler and runtime work**: a local pass, a suspension ledger in ARC,
  and a host ABI that every opting-in platform must implement correctly.
- **`.boxy` pays more.** In `Tasks` builds, and in `None` builds, every
  erased effectful call is a suspension point, so dev builds check the flag at
  more call sites than optimized builds do. In a `None` build this happens
  even in a program that never waits.
- **Waiting costs something on every platform.** A program that waits is
  lowered for pausing even where the platform schedules nothing, because
  that is what lets one task wait for another there.
- **Code size.** Each suspendable call site gains a save block, and each
  suspendable procedure a resume entry and per-site drop entries.
- **Pausing costs grow with depth.** Pausing saves one record per procedure on
  the native stack, and resuming restores one per procedure as the work
  continues. A stackful switch costs the same at any depth.
- **No preemption.** A long pure computation keeps its thread until it
  finishes. Platforms that need fairness use several threads.
- **Exits wait.** Leaving a loop waits until its cancelled entries stop, and an
  entry stops only at a suspension point. A long pure computation in an entry
  delays every exit, including a timeout's.
- **A third `for!` protocol.** Whether a loop is concurrent is chosen by the
  type of what it loops over. A plan built elsewhere does not show at the loop
  that it is concurrent; its type and its name have to.
- **Debugging.** A parked task has no native stack. Its records name their call
  sites, so its waiting path can be printed, but debuggers need new tooling to
  show it.
- **Hot reload** across a change to a paused procedure's call sites or saved
  values is not supported.
- **Crashes leak.** The task a crash happens in leaks what its running native
  frames owned. Erlang avoids this with per-process heaps; Roc's shared,
  reference-counted heap cannot. Platforms that crash often need region
  allocation to get the memory back.
- **Crash isolation needs a non-local exit** on every target. Native targets
  rely on `src/sljmp`, and wasm on exception handling; elsewhere a crash still
  ends the process.

## Rationale and alternatives

| Alternative | Why not |
|---|---|
| Stackful green threads, as prototyped | No portable wasm story, leaky cancellation, fixed stacks, per-architecture assembly (see Motivation) |
| JSPI as the wasm lowering | Browsers only |
| WasmFX stack switching | Phase 3, not in WebAssembly 3.0; one Wasmtime platform |
| Asyncify itself | Runs after code generation, on wasm only, and instruments conservatively (about 50% code-size and speed cost). This RFC uses the same idea inside the compiler, on every backend, touching only call sites that can pause |
| State machines with frames embedded in callers (rustc, Kotlin; this RFC's earlier draft) | Needs a whole-program frame layout, unions over lambda sets, special cases for recursion and tail groups, and a frame-size ABI for erased callables, and keeps every value live across a pause in memory even when nothing pauses |
| LLVM coroutine intrinsics (`llvm.coro.*`) | Only the LLVM backend would get them; the dev and wasm backends and the interpreter would each need their own version |
| Copying stack segments in and out (as Python's greenlet does) | Impossible on wasm, whose value stack is not addressable |
| A user-visible `Task` or `async` type | Adds a second function color; Roc replaced `Task` with effectful functions |
| Platform-defined concurrency modules | Packages using concurrency would be tied to one platform |
| `None` builds on one plain stack, running each entry to completion | A task that waits sits under whatever it runs next, so only the most recent waiter can continue. Waking must then be most recent first, which is unfair, and a protocol in which two tasks take turns, like a permit that must come back before its lender continues, deadlocks. Running tasks as coroutines where they can wait gives every platform one meaning, at the cost of status checks in programs that wait |
| `spawn` outside any limit | A crawler would have to count what it has in flight by hand to stay bounded, and a loop over `plan.limit(8)` that spawns would look bounded and not be |
| Ending a loop only when every entry has finished | A deadline or a reporter entry would keep the loop going after the real work is done, so every such loop would count arrivals and `break` |
| Unstructured `spawn` | Tasks could outlive their caller, making cancellation and errors ad hoc. `spawn` exists only inside a concurrent loop's body, where it adds to that loop |
| Combinators as the only primitives (this RFC's earlier draft) | A closed menu: a quorum, the first success, hedged requests or a retry policy would each need a new builtin. A loop makes the policy a `match` |
| A fold whose step returns `[Continue(s), Break(s)]`, like `List.fold_until` | Works, but threads state through a step function. A loop body keeps state in `var`s and can perform effects of its own |
| A stream of arrivals as a value | It could be stored or returned with its entries still running, and leaving a loop over it would have to wait inside a drop, which cannot suspend |
| A scoped handle passed to a closure, like Swift's `withTaskGroup` | A closure could capture the handle and keep it; preventing that needs non-escaping types, which Roc does not have |
| A new keyword for concurrent loops | The type of what the loop runs over selects the protocol, as `iter` and `stream` already do. Only `spawn` needs a keyword |
| Exits that wait for entries instead of cancelling them (Swift task groups on normal exit) | Most loops leave because they have their answer, and the rest of the work is wasted. Shields give the waiting behavior to the entries that need it |
| The host polls tasks (Rust `Future::poll`) | A wake-and-ready protocol fits io_uring, IOCP, JavaScript promises and callback-based APIs directly |
| Suspension as a property of every hosted call | Every effectful call site would pay the flag check and save block, including calls that never pause |
| Suspending functions listed in the platform header | Packages and `.boxy` units do not import the platform root, so the information must live with the declaration |
| Catching crashes in Roc (`try`/`catch`, or a combinator returning `Err(Crashed)`) | Turns crashes into a second error channel. OTP's approach keeps expected errors as values and handles crashes only by restarting from a known state |
| Crash stops the whole program in `Tasks` builds too | One bad request would take down a server; OTP-style isolation is the main reliability benefit of tasks |

The decisive argument: Kotlin and Rust need visible `suspend` and `async`
markers because their compilers must know which functions to transform. Roc's
`=>` already carries that information for its own reasons, and the absence of
effect polymorphism makes it exact. Roc gets the portability of stackless
coroutines without their usual cost to users. Capturing by unwinding then keeps
the compiler side small: the transformation is local to each call site that
can pause, and code that does not pause runs as it does today.

## Prior art

| Work | Relevance |
|---|---|
| de Moura and Ierusalimschy, *Revisiting Coroutines* (TOPLAS 2009) | Stackful vs. stackless terminology; asymmetric coroutines are as expressive as one-shot continuations |
| Leijen, *Type Directed Compilation of Row-Typed Algebraic Effects* (POPL 2017) | Selective CPS of only the code whose types say it can perform effects |
| Xie and Leijen, *Generalized Evidence Passing for Effect Handlers* (ICFP 2021) | Effect handlers compiled to plain C with no stack switching |
| Pettyjohn et al., *Continuations from Generalized Stack Inspection* (ICFP 2005) | Capturing continuations by unwinding the stack into heap records and reinstating them, on platforms that cannot switch stacks; the basis of this design |
| Binaryen Asyncify | The same unwind-and-rewind technique applied to compiled wasm; selective instrumentation is what keeps its cost down |
| rustc coroutine transform | State machines over MIR after drop elaboration; the main alternative this RFC rejects |
| Kotlin coroutines | CPS state machines; results that are either a value or a suspension marker, like the pending flag here |
| LLVM coroutines | Switched-resume, returned-continuation and async lowerings inside one backend |
| Lean 4 and Koka | Per-object atomicity: a negative count marks an object shared, a spawn marks everything its closure reaches, every count update tests the sign, and shared objects lose in-place reuse. The alternative this RFC rejects in favor of static modes |
| Choi, Shull and Torrellas, *Biased Reference Counting* (PACT 2018); CPython PEP 703 | Owner-thread counts updated non-atomically, other threads' counts atomically |
| Rust `Rc` and `Arc` | Atomicity chosen statically per type, written by the programmer; mode specialization infers the same choice per position |
| Swift concurrency (SE-0304, SE-0317) | Structured task groups and `async let`; frames from a per-task stack-discipline allocator. A task group is consumed with `for await` in completion order. On normal exit it waits for its remaining children without cancelling them, and cancels them only when its body throws; concurrent loops cancel at every exit unless shielded |
| Osvald et al., *Gentrification Gone too Far? Affordable 2nd-Class Values for Fun and (Co-)Effect* (OOPSLA 2016) | Second-class values that can be used but not kept; a running set is one, enforced by having no name |
| Zig 0.6–0.10 async | Stackless frames of known size; recursion made frame sizes uncomputable; removed in 0.11 |
| Java structured concurrency (JEP 505) | Children scoped to a block and cancelled on failure |
| Trio nurseries | Children cannot outlive their scope; a concurrent loop is a nursery whose scope is the loop |
| Erlang/OTP | "Let it crash"; links propagate abnormal exits; supervisors trap exits and restart children (`one_for_one`, `one_for_all`); per-process heaps make a crash's memory reclaimable |
| js_of_ocaml and wasm_of_ocaml | Partial CPS for effects on JavaScript and wasm, with JSPI and stack-switching modes as alternatives |

## Decisions left to prototypes

Syntax spellings (`suspending`, `concurrency: Tasks`, `ParallelTasks`,
`concurrently`, `limit`, `shielded`, `background`, `spawn`) are placeholders and are not
decided here: names are a matter for review, which no prototype can settle.

The prototype settles every other decision this RFC left open:

| Decision | Result |
|---|---|
| Records from `roc_alloc`, or from per-task buffers and per-thread free lists | Buffers and free lists. Natively they are about five times cheaper than `malloc`, and on wasm (V8, `-O2`) 1.5 to 1.8 times cheaper at every depth measured |
| How `.boxy` records hold dynamically sized values: inline after a descriptor-sized header, or as separate allocations | Inline, in the per-task buffer: separate allocations cost 5% to 35% more from the same buffer, and about twice as much from `malloc` |
| How a deferred erased call whose target is suspendable hands its pending call up through a suspension | The two rules under "Calls between suspendable procedures" |
| How precise the may-alias analysis behind spawn seeds must be | A points-to analysis that keeps call sites apart: it removes every false atomic the union-find causes in the prototype |
| Whether edge modes are needed for path-copied shared data | They remove the path-copy residual that the static analyses leave. Whether that residual costs enough to add them is measured at step 6 of the implementation plan |
| Whether `spawn` belongs in the first version, or only plans fixed when the loop starts | `spawn` belongs. The companion RFC's semaphore and pool libraries (`library.py`) start a grant for each request as it arrives, which no plan fixed when the loop starts can express. Crawlers and hedged requests need it for the same reason |
| Whether `supervise!` needs a time window for its restart limit, which would require a clock from the platform | No, not in the first version. In the prototype, a server supervised as a whole with a limit of 3 died after four transient crashes far apart in 20 requests, running its handler 50 times for 20 requests. Supervising each request with the same limit served all 20 with one restart per crash. A lifetime limit fits supervision of units of work, which the task tree makes the natural structure. OTP needs a window because its supervisors restart long-lived processes |
| Whether a loop exit should cancel its entries (this RFC) or wait for them (Swift) by default | Cancel. Every loop the prototype's libraries leave early wants it: `race!` and `all_ok!` are written as loops whose exit cancels the rest, and a client that times out with `race!` must stop waiting for its permit or resource, which the semaphore and pool tests rely on. None of the library code needed a shield. Under waiting by default, each of these would have to opt into cancellation instead |

Five open points are not prototype decisions: each needs the compiler or a
platform, and is settled at its step of the implementation plan.

Four later changes are not yet reflected in the prototype:
- `None` builds that run as coroutines wherever a task can wait. The
  companion RFC's prototype already does this for programs that use channels
  (its decision (a)), with the evidence given there.
- Spawned entries counting against the limit at the root of the loop's plan.
- Background entries.
- `all!` and `all_ok!` as methods of plans.

The prototype's `None` engine, its input-order policy and its spawn and plan
operations should be brought in line with them before G3, G9 and C9 are
claimed for them.

## Future possibilities

- **Sessions, channels and shared resources**, in the companion RFC.
- **Concurrent streams**, such as a `Stream` whose items come from a limited
  plan, built on the same operations.
- **Parallel evaluation of pure code.** Purity lets a platform run pure work
  across cores, and on wasm this already works with shared memory, atomics and
  workers. It must be deterministic, so it differs from a concurrent loop in
  three ways. Results come in input order. When several entries crash, the
  crash reported is the lowest-indexed one, reported only after every lower
  index has finished, so a sequential program that would have looped forever
  first still does. And abandoning pure work is unobservable, so nothing needs
  to wait: it can be an ordinary `Iter` adapter, a first-class value, which
  pure code can use.
- **Durable execution.** C4 already keeps native addresses out of records, so
  a paused task is values only. With the same binary, a platform could write a
  paused task to storage and resume it after a restart, which long-running
  sagas need.
- **Deterministic simulation testing.** A test platform with a seeded,
  single-threaded scheduler makes every interleaving reproducible.
- **Alternative lowerings.** JSPI or WasmFX could become explicit,
  target-selected lowerings if measurements show a benefit. They would be
  chosen by target data, never as runtime fallbacks.

## Implementation plan

1. Model the task runtime and host contract, and check R1–R7 on it.
2. Parse `concurrency` and `suspending`; add the mode to unit keys; record
   suspendability on LIR procedures and calls.
3. Add the ARC suspension ledger and its certification (C2–C7, C10).
4. Implement the pass for `.lss`: flag checks, save blocks, resume and drop
   entries, hosted result slots, the `frame_callables` rule and task-held
   pending cells.
5. Add the task runtime with its wake protocol and crash exit, the host ABI,
   glue, the conformance harness, and one native reference platform. With an
   optimized compiler, confirm that records from per-task buffers keep their
   advantage over `roc_alloc` in compiled code. Port crash exits to targets
   that `src/sljmp` does not cover yet, such as 32-bit x86, or accept
   process-wide crashes there.
6. Implement plans, concurrent loops and their lowering, then the `Concurrent`
   library over them, including `supervise!`, and `None` builds: without
   tasks where nothing can wait, and with the program's own scheduler
   otherwise.
   In `ParallelTasks` builds, add spawn seeds with forward propagation and
   mode specialization, then borrowed captures, measuring each. Compare
   points-to with the union-find on real programs, by the share of count
   statements left plain (a developer diagnostic in `base.dev_diagnostics`).
   Run the roc-lang/roc#7718 benchmarks inside `all!` children that path-copy
   a shared input, in ReleaseFast `ParallelTasks` builds, against the
   all-atomic upper bound and a `Tasks` build, and add edge modes only if the
   path-copy residual measurably costs.
7. Extend the pass to `.boxy` and erased calls.
8. Build browser and WASI 0.3 reference hosts, which settles how hosts map
   tasks onto WASI 0.3 and onto JavaScript promises.

## References

- de Moura and Ierusalimschy, [Revisiting Coroutines](https://www.cs.tufts.edu/~nr/cs257/archive/roberto-ierusalimschy/revisiting-coroutines.pdf), TOPLAS 31(2), 2009
- Leijen, [Type Directed Compilation of Row-Typed Algebraic Effects](https://popl17.sigplan.org/details/POPL-2017-papers/69/Type-Directed-Compilation-of-Row-Typed-Algebraic-Effects), POPL 2017
- Xie and Leijen, [Generalized Evidence Passing for Effect Handlers](https://www.microsoft.com/en-us/research/uploads/prod/2021/08/genev-icfp21.pdf), ICFP 2021
- Phipps-Costin et al., [Continuing WebAssembly with Effect Handlers](https://www.microsoft.com/en-us/research/publication/continuing-webassembly-with-effect-handlers/), OOPSLA 2023
- Pettyjohn, Clements, Marshall, Krishnamurthi and Felleisen, [Continuations from Generalized Stack Inspection](https://cs.brown.edu/~sk/Publications/Papers/Published/pcmkf-cont-from-gen-stack-insp/), ICFP 2005
- LLVM, [Coroutines in LLVM](https://www.llvm.org/docs/_sources/Coroutines.md.txt)
- Ullrich and de Moura, [Counting Immutable Beans](https://arxiv.org/abs/1908.05647) (IFL 2019); [Lean reference counting](https://lean-lang.org/doc/reference/4.21.0/Run-Time-Code/Reference-Counting/); [`lean.h`](https://github.com/leanprover/lean4/blob/8009d0a64d2860606c6a5ac5ff6702622b24921e/src/include/lean/lean.h)
- Reinking, Xie, de Moura and Leijen, [Perceus](https://www.microsoft.com/en-us/research/uploads/prod/2020/11/perceus-tr-v4.pdf) (PLDI 2021); Koka [`kklib.h`](https://github.com/koka-lang/koka/blob/b66091b2a5a7ea20c5ed9498ad1e1f03caee9589/kklib/include/kklib.h)
- Choi, Shull and Torrellas, [Biased Reference Counting](https://iacoma.cs.uiuc.edu/iacoma-papers/pact18.pdf), PACT 2018; [PEP 703](https://peps.python.org/pep-0703/)
- roc-lang/roc, [#7718: switch roc to atomic refcounts](https://github.com/roc-lang/roc/pull/7718)
- tokio, [multi-thread worker and LIFO slot](https://codebrowser.dev/rust/crates/tokio/src/runtime/scheduler/multi_thread/worker.rs.html)
- rustc, [coroutine transform](https://doc.rust-lang.org/nightly/nightly-rustc/rustc_mir_transform/coroutine/index.html) and [layout](https://doc.rust-lang.org/nightly/nightly-rustc/rustc_mir_transform/coroutine/layout/index.html)
- Kotlin, [Asynchronous programming with coroutines](https://kotlinlang.org/spec/asynchronous-programming-with-coroutines.html) (language specification)
- Swift, [SE-0304 discussion](https://forums.swift.org/t/se-0304-structured-concurrency/45314), [SE-0317 discussion](https://forums.swift.org/t/se-0317-async-let/48848), [task allocator](https://sources.debian.org/src/swiftlang/6.0.3-2/swift/stdlib/public/Concurrency/TaskAlloc.cpp)
- Zig, [async in 0.11](https://zig.guide/0.11/async/introduction)
- OpenJDK, [JEP 505: Structured Concurrency](https://openjdk.org/jeps/505)
- Smith, [Notes on structured concurrency, or: Go statement considered harmful](https://vorpus.org/blog/notes-on-structured-concurrency-or-go-statement-considered-harmful/), 2018
- Swift Evolution, [SE-0304: Structured concurrency](https://github.com/swiftlang/swift-evolution/blob/main/proposals/0304-structured-concurrency.md)
- Osvald, Essertel, Wu, González Alayón and Rompf, [Gentrification Gone too Far? Affordable 2nd-Class Values for Fun and (Co-)Effect](https://doi.org/10.1145/2983990.2984004), OOPSLA 2016
- Erlang/OTP, [supervisor](https://www.erlang.org/docs/24/man/supervisor) and [supervision principles](https://www.erlang.org/docs/19/design_principles/sup_princ.html); Hébert, [Who Supervises The Supervisors?](https://learnyousomeerlang.com/supervisors)
- Erlang, [binary handling and process heaps](https://www.erlang.org/docs/22/efficiency_guide/binaryhandling)
- WebAssembly, [Wasm 3.0 completed](https://webassembly.org/news/2025-09-17-wasm-3.0/)
- [js_of_ocaml effect handlers](https://ocsigen.org/js_of_ocaml/latest/js_of_ocaml/effects.html)
- Bytecode Alliance, [WASI 0.3 launched](https://bytecodealliance.org/articles/WASI-0.3)
- [Wasmtime proposal status](https://docs.wasmtime.dev/stability-wasm-proposals.html)
- V8, [JSPI](https://v8.dev/blog/jspi); [caniuse: JSPI](https://caniuse.com/mdn-webassembly_jspi)
- Zakai, [Asyncify in Practice](https://kripken.github.io/talks/2022/asyncify.html)
