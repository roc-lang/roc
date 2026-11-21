# Semantics

This is the reference for compiler contributors: what the compiler does to a
program that can suspend, what the runtime guarantees, and which invariants
hold the design together. It condenses the reference-level sections of the
[main RFC](../concurrency-explicit-suspension.md#reference-level-explanation)
and the [shared-state RFC](../concurrency-shared-variables.md#reference-level-explanation).
Each section links the RFC text it comes from. Nothing here is implemented
yet: "the compiler does" means "the proposal says the compiler does".

## Compiler Vocabulary

This page uses terms from [design.md](../../../design.md). Each one, once:

| Term | Meaning |
|---|---|
| LIR | The low-level IR that ARC, the backends, the interpreter and LirImage consume ([Direct LIR Lowering](../../../design.md#direct-lir-lowering)) |
| ARC | The pass that inserts explicit `incref`, `decref` and `free` statements into LIR; backends follow them mechanically ([ARC](../../../design.md#arc)) |
| Certify | Debug-build re-checking of LIR, such as the [debug borrow certifier](../../../design.md#debug-borrow-certifier). A certified property is a compiler invariant |
| `.lss` | The lambda-set-specializing strategy: every call's possible targets are known ([`.lss` Runtime Lowering](../../../design.md#lss-runtime-lowering)) |
| `.boxy` | The layout-polymorphic strategy: each template compiles once, and generic bodies call through erased callables ([`.boxy` Runtime Lowering](../../../design.md#boxy-runtime-lowering)) |
| LirImage | The mapped interpreter image that carries LIR and its static data ([LirImage And Hosted Functions](../../../design.md#lirimage-and-hosted-functions)) |
| Frame callable, pending cell | A callable built in the native frame ([Frame Callables](../../../design.md#frame-callables)); where a procedure leaves a deferred erased call for its waiter to make ([Pending Cells](../../../design.md#pending-cells)) |
| Frame-replacing call | A tail call that runs in constant space ([Frame-Replacing Calls](../../../design.md#frame-replacing-calls)) |

## Terms

These follow [Terms](../concurrency-explicit-suspension.md#terms), with the
additions from [Changes to the companion RFC](../concurrency-shared-variables.md#changes-to-the-companion-rfc).

- A _task_ is one running effectful function: its native frames while it runs,
  its records while it's paused.
- A _root_ is a task the host starts through a `provides` entry.
- A _plan_ is an inert value of type `Concurrently(a)` that describes work
  that may overlap. Building, storing or returning one runs nothing.
- An _entry_ is one function in a plan. A `for!` loop over a plan runs each
  entry as a child task of the loop's task.
- A _running set_ is the set of entries one loop has started. Only that loop's
  operations can name it.
- A _suspension point_ is a call to a `suspending` hosted function; a call
  through an erased effectful function value in a `.boxy` build; a
  concurrent loop's head or exit; `supervise!`'s wait; or `update!`, which the
  shared-state RFC adds.
- A procedure is _suspendable_ if it may reach a suspension point.
- A _wait source_ is something a task can park on: its pending hosted
  operation, the running set of one of its loops, or (shared-state RFC) the
  handle it waits on. A task parks on exactly one at a time.
- A _handle_ is a value of type `Shared(s)`. It points to a reference-counted
  _cell_ that holds the state.

## Explicit Inputs

Suspension is decided only from explicit data
([Explicit inputs](../concurrency-explicit-suspension.md#explicit-inputs)).
The platform's `concurrency` field (`None`, `Tasks` or `ParallelTasks`) is
part of every unit's compilation key, like the target, because it changes code
in modules that never import the platform. The `suspending` marker on hosted
declarations is part of the declaring module's checked output. And Roc has no
effect polymorphism, so `->` versus `=>` decides whether a function value can
suspend.

## Suspendability

Suspendability is computed once, over the post-specialization LIR call graph,
and recorded on each procedure (`LirProcSpec.suspends: never | may`) and on
each call. Later stages read it and never rediscover it
([Suspendability](../concurrency-explicit-suspension.md#suspendability)).

| Build | Suspension points |
|---|---|
| `None` | Hosted calls never suspend. Only `update!` calls that can answer `Wait` do, plus the loop heads, exits and `supervise!` waits of procedures whose children may reach one. Under `.lss`, a program that can't wait gets no transformation; under `.boxy`, erased effectful calls are still suspension points |
| `.lss` | Exact: lambda sets give every call's targets. Erased calls at host boundaries suspend exactly when their type is effectful |
| `.boxy` | Every call through an erased effectful function value, in `Tasks` builds and in `None` builds. A generic body can't know its callees, and its output must depend only on its module and imports ([Per-Module Output](../../../design.md#per-module-output)) |

A pure procedure is never suspendable.

## Capture By Unwinding

Tasks run on the native stack of whichever thread resumes them. Only when a
task pauses does each suspendable procedure on its stack save what it still
needs into a heap _record_ and return. The chain of records, a one-shot
continuation, is the paused task. It's the technique of generalized stack
inspection and Binaryen's Asyncify, restricted to procedures that can pause
([Capture by unwinding](../concurrency-explicit-suspension.md#capture-by-unwinding)).

The pass is local to each _suspendable call site_ and needs no
whole-program frame layout. It adds:

- **A hidden task parameter** to every suspendable procedure, and to every
  erased effectful callable in a `Tasks` build or a `.boxy` `None` build.
- **A flag test** after each site, on the task header's pending flag. Clear
  means continue: one load and branch is the whole cost of a call that doesn't
  pause.
- **A pending edge.** Set means save the values live after the call (not its
  result), a mask of which ones it owns, and the site's identity into a new
  record, then return. Each caller does the same, up to the task's entry.
- **A resume entry** per procedure, which restores a record's values into a
  fresh frame and jumps to just after the call. Nothing before it runs again.
- **A drop entry** per site, which releases the saved values the mask owns.
- **A hosted result slot** reserved in the task's record storage and passed as
  `out`, so a host's later write lands in stable memory.

Resuming runs the innermost record first and passes each result outward, so
a resume costs only the work that's left. An unstarted entry is a _start
record_ holding its closure.

### Position In The Pipeline

The pass runs after every pass that changes ownership or frame storage
([Position in the pipeline](../concurrency-explicit-suspension.md#position-in-the-pipeline)):

```text
LIR construction -> ARC -> frame_callables (.boxy) -> tail_drive
  -> reachable_procs_after_arc -> immortal_locals -> comptime_value_guards
  -> suspension lowering -> diagnostics, backends, LirImage
```

Its output is ordinary LIR with explicit reference-count statements.
Backends, the interpreter and LirImage need no knowledge of suspension.

### What ARC Must Publish

A drop entry must know which saved values its procedure owns. ARC doesn't
publish ownership per program point today, and reading it back from the RC
statements would be reconstruction. So ARC records an explicit _suspension
ledger_: per suspendable site, the values live after it and which are owned.
The certifier checks it, and `immortal_locals` edits it with its statements
([What ARC must publish](../concurrency-explicit-suspension.md#what-arc-must-publish)).

### Values, Never Native Addresses

A resumed procedure gets a new frame at a different address, so records hold
LIR values, never native-frame addresses
([Values, never native addresses](../concurrency-explicit-suspension.md#values-never-native-addresses)).
The two constructs that hold such addresses get explicit rules: a frame
callable passed to a suspendable site is heap-allocated instead, and a
suspendable procedure's pending cell lives in task storage.

A frame-replacing call has no code after it, so it gets no flag test and
leaves no record, and tail calls stay constant-space. Deferred erased calls
need [two more rules](../concurrency-explicit-suspension.md#calls-between-suspendable-procedures).

### Example

```roc
fetch_two! = |a, b| {
    x = Http.get_utf8!(a)?
    y = Http.get_utf8!(b)?
    Ok(Str.concat(x, y))
}
```

In pseudo-LIR:

```text
fetch_two(task, a, b):
  slot = reserve(task, Try(Str, [HttpErr(U16)]))
  roc_http_get_utf8(task, slot, a)               # a moves to the host
  if task.pending { save(task, site 1, slot, {b}, owned: {b}); return }
site 1:
  x = take(slot)?                                # Err: decref b, return
  slot = reserve(task, Try(Str, [HttpErr(U16)]))
  roc_http_get_utf8(task, slot, b)
  if task.pending { save(task, site 2, slot, {x}, owned: {x}); return }
site 2:
  y = take(slot)?                                # Err: decref x, return
  return Ok(Str.concat(x, y))

drop entry site 1:  decref b
drop entry site 2:  decref x
```

If neither request pauses, the procedure runs straight through with two flag
tests added, and no record is made.

## Concurrent Loops

Before the suspension pass, a `for!` loop over a plan lowers to four internal
operations, each carrying explicit `RcEffect` data
([Concurrent loops](../concurrency-explicit-suspension.md#concurrent-loops-1)):

| Operation | Meaning in every build (run by the host's scheduler in `Tasks`) | In a `None` build where no entry can wait |
|---|---|---|
| `group(plan)` | Flatten the plan into entries in input order, fix each entry's place in the task tree, start every entry whose limits have room | Record the entries; start nothing |
| `next(g)` | Suspension point. Wait for the next finished entry and report `Arrived(v)` or `Cancelled`. Once only background entries are left, cancel them, wait until they've stopped, report `Ended` | Run the earliest unstarted entry to completion and report `Arrived(v)`. Once every unstarted entry is background, discard them and report `Ended` |
| `spawn_in(g, f)` | Add an entry under the limit at the root of the plan, if the root is a limit, and start it when that limit has room | Append an entry |
| `cancel_group(g)` | Discard unstarted entries; cancel running ones unless they're shielded and have begun | Discard unstarted entries |

A loop becomes:

```text
g = group(plan)
head:
  match next(g)
    Arrived(v) => body      # back to head continues; every edge out runs exit(g)
    Ended      => after the loop

exit(g): cancel_group(g); repeat next(g) until Ended, discarding the reports
```

The body never sees `Cancelled`; only the exit does. Exits are edges the
compiler already knows, so waiting at one is an ordinary suspension point and
no drop ever waits.

- **Placement** is fixed by `group` or `spawn_in`, in input order, so a
  child's identity doesn't depend on when it starts.
- **Limits** start the next eligible entry as soon as one under them finishes.
  Spawned entries count against the limit at the plan's root, if there is one.
- **Background entries** don't keep their loop going: once only they are
  left, the loop cancels them as an exit would. Spawned entries never are.
- **Begun.** An entry has _begun_ once it has run any code. An exit cancels
  an unbegun entry even if it's shielded, which keeps a `None` build a
  schedule of a `Tasks` build (C9).

`Concurrent.all!`, `both!`, `all_ok!` and `race!` are library functions written
as these loops, so they need no separate `None` implementation. `supervise!` is
the exception: it uses the runtime's internal `task_spawn` and
`task_await_any`, whose `Crashed` report programs can't reach
([The `Concurrent` library](../concurrency-explicit-suspension.md#the-concurrent-library)).

### The `None` Scheduler

A `None` build runs its tasks itself, on one thread, with hosted calls that
finish before they return. _The next task to run is the ready task that comes
first in the task tree. An entry that hasn't begun is ready only once its loop
asks for the next result, or when no other task can run._

Where no entry can wait, that schedule is the table's last column, so the
build needs no tasks. Where a wait is reachable, the build is the unwinding
lowering under this scheduler: coroutines on one stack. If no task can run,
the program crashes with a deadlock message
([On platforms that do not schedule tasks](../concurrency-explicit-suspension.md#on-platforms-that-do-not-schedule-tasks)).

The second half of the rule came from the prototype: without it, an outer
entry the loop hadn't asked for ran ahead of an inner loop's entries, which
one stack never does ([evidence](07-evidence.md)).

## Crashes And Cancellation

### Cancellation

A cancelled task runs no more of its own code once it next parks, or at once
if it's parked. Its hosted operation is aborted (`roc_task_abort`), its
descendants are dropped (R3), and its records go through their drop entries,
exactly once. Effects it already performed stay performed. The only code a
drop runs is pending `finally!` and `on_failure!` cleanups, innermost first:
pure functions of one handle's state. A task parked in `Wait` leaves its
waiter list, having taken nothing.

### Crash Mechanics

A crash happens deep in native frames, not at a suspension point. In `Tasks`
builds the runtime takes a non-local exit back to the `roc_task_resume` that
is running the task: `src/sljmp` on native targets, WebAssembly exception
handling on wasm. Where neither exists, a crash ends every task in the process,
as today. Then
([Crash mechanics](../concurrency-explicit-suspension.md#crash-mechanics)):

1. **The crashed task is dead.** Values held only by its running frames leak,
   since their state is unknown. Its records are consistent, so they're
   dropped exactly. Its children are cancelled and dropped exactly before the
   crash goes upward, and its pending cleanups then run, innermost first.
2. **Links propagate.** A loop entry's parent crashes with the same message.
   A parked parent crashes where it waits, aborting its hosted operation and
   dropping untaken arrivals. A running parent (`ParallelTasks` only) takes no
   more results, and its next suspension point suspends so it crashes there.
3. **Propagation stops** at a `supervise!`, which restarts up to
   `max_restarts` times, or at a root, which reports `ROC_TASK_CRASHED`.

Only the crashed task leaks, and only what its running frames owned. A
platform can reclaim that with per-root region allocation.

## Shared State

### `update!` Step By Step

`h.update!(f)` runs as if no other task ran during it
([`update!`](../concurrency-shared-variables.md#update)):

1. Acquire `h`.
2. If `h` is poisoned, release it and crash.
3. Move the state out of `h` into `f`'s update form, so a state nothing
   else references changes in place.
4. `Set(new, r)`: store `new`, wake waiters, release `h`, return `r`.
5. `Keep(r)`: put back the state `f` was given, release `h`, return `r`.
6. `Wait`: put the state back, register as a waiter while still holding `h`,
   release `h` and park. A readied waiter starts again at step 1.

If `f` crashes, `h` stays empty and is poisoned, its waiters are readied so
they crash, `h` is released, and the task crashes at the call. The state `f`
held is in the crashed frames, which crash mechanics leaks.

### The Update Form And The Probe Form

`Shared` functions are builtins whose structure is explicit before LIR
([Lowering](../concurrency-shared-variables.md#lowering)). Each function
literal passed to `update!` is lowered twice:

- **The update form.** Each tail position, followed through `if`, `match`
  and blocks, is rewritten: a `Keep(r)` or `Wait` tail also returns the
  parameter, a `Set(new, r)` tail is unchanged, and any other tail `e` becomes
  a `match` on `e` that does both. ARC then runs as usual, so on a `Set` path
  a unique state changes in place.
- **The probe form.** A `Set` or `Keep` tail evaluates neither `new` nor `r`
  and answers "would go ahead"; a `Wait` tail answers "would wait"; any other
  tail calls `e` and inspects the tag. A probe builds no state, so it never
  copies one.

The rule reads only the literal's own body, so it's the same under `.lss` and
`.boxy`. A function passed as a value is called from a tail, which keeps the
state alive across the call, so a `Set` built inside it copies.

In `None` and `Tasks` builds, acquire and release are omitted: the function is
pure, can't suspend, and resumes are serial.

### `finally!` And `on_failure!`

`h.finally!(cleanup, body)` pushes an entry (handle and cleanup) on the
task's cleanup stack, calls `body`, pops it and applies the cleanup as an
update that sets the state. `on_failure!` drops the entry unrun on return. On
cancellation or a crash, pending entries run innermost first, after the
task's descendants are gone. Push and pop are certified balanced, and an entry
is owned like a record, so C5 covers it
([`finally!` and `on_failure!`](../concurrency-shared-variables.md#finally-and-on_failure)).

A cleanup that crashes stores nothing and poisons its handle. The remaining
entries still run. A cancelled task whose cleanup crashes now crashes; an
already-crashing task keeps its first crash, with the cleanup's message
attached.

### Waking By Probes

Waiters are kept in the order they began waiting. After a `Set`, while still
holding the cell, the runtime calls each waiter's probe form on the new state,
oldest first, and readies the first that wouldn't answer `Wait`. A readied
waiter that waits again keeps its place
([Runtime](../concurrency-shared-variables.md#runtime)).

The readied waiter carries the wake: if it ends without storing (`Keep`,
`Wait` again, a crash, or cancellation before it runs), the runtime probes on.
A crashing probe counts as not waiting. Poisoning readies every waiter.
Probing is exact because the functions are pure, and commits nothing for the
waiter. It keeps dispatches linear; the cost moves to probes, about n² pure
calls under contention.

### Releasing Cells

Every reference cycle passes through a cell. The runtime records, per cell, a
_holder set_: the tasks that may hold a reference to it
([Releasing cells](../concurrency-shared-variables.md#releasing-cells)).

- `Shared.new!` makes a cell held by the current task.
- A value crossing to another task (an entry's captures, a child's result,
  hosted arguments) adds the receiver to every cell it reaches. The sender
  stays a holder unless it reached the cell only through count-1 objects.
- A store passes holders on: every cell reachable from the new state gains the
  stored cell's holders, recursively through states.
- A task leaves every holder set when it finishes.

When a live cell has no holders left, no task can ever reach it, so the
runtime _releases_ it: it drops the state with the type's generated drop
helper and leaves the cell empty. If that led back to the cell, the cycle
unwinds and reference counting frees the rest. A released cell is never
updated again. The walk is generated from the stored type and visits nothing
for a type that can't contain a handle.

Holder sets over-approximate, so release isn't the usual way cells are freed:
an acyclic cell is freed by its count when its last handle goes. `None` builds
track no holders and release every live cell when the program ends.

### Stall Detection

`Wait` can wait forever. A root's task tree is _stalled_ when none of its
tasks is queued, running or waiting on a hosted operation, at least one waits
on a cell, and every holder of every cell waited on is in the tree. The
runtime then crashes the waiter that began waiting last (ties go to the task
created later) with a deadlock message. That crash readies other tasks, so one
detection is enough to make progress. A cell held by another root or the host
may still change, so a tree waiting on one isn't stalled
([Deadlocks and liveness](../concurrency-shared-variables.md#deadlocks-and-liveness)).

### Runtime Per Build

A cell holds the state, a poisoned flag and its waiters. The rest depends on
the build:

| Build | Acquiring a cell | Waiters | Holders |
|---|---|---|---|
| `None` | Nothing: one thread runs every task | A list per cell | None tracked; release at program end |
| `Tasks` | Nothing: the function is pure and resumes are serial (P10) | A list per cell | A set per cell |
| `ParallelTasks` | Lock the cell | A list per cell, under its lock | A set per cell, under its lock |

An update holds one lock and runs only pure code while holding it, and a pure
function can't call `update!`, so locks never nest. A task waiting for a lock
isn't at a suspension point: it spins briefly, then yields its thread.

### Poisoning

Each cell carries a poisoned flag, set by an update or cleanup that crashes.
Every later update of the handle crashes with a message naming the source
location of the function that crashed, and so does every waiter. No code can
test for poison or clear it
([Poisoning](../concurrency-shared-variables.md#poisoning)).

## Reference Counting Across Tasks

A count update must be atomic only when another thread may update the same
count at once. Handing a value to or from a child isn't sharing, since the
host orders the two tasks, and in `Tasks` builds nothing changes. In
`ParallelTasks` builds, a capture is seeded atomic only if a sibling may alias
it, modes propagate forward, procedures are specialized by mode, and children
borrow their captures. Host seeds keep today's equality classes, and cell
stores are seeds, so cells always use atomic counts there. [Decision 5](03-decisions/05-refcount-atomicity.md)
gives the choice and its evidence, and
[Atomic Reference Counts Across Tasks](../concurrency-rc-atomicity.md) derives
it from first principles.

## How It Keeps The Compiler's Rules

[AGENTS.md](../../../AGENTS.md) and design.md's
[Core Principles](../../../design.md#core-principles) set rules every stage
must follow. Here's what in this design relies on each.

| Rule | What relies on it |
|---|---|
| **Later stages consume explicit data**; nothing is recovered, guessed or reconstructed | The `concurrency` field in the compilation key and the `suspending` marker in checked output. Suspendability recorded on `LirProcSpec` and each call, never rediscovered. ARC's suspension ledger, instead of reading ownership back from RC statements. `RcEffect` data on `group`, `next`, `spawn_in`, `cancel_group` and `task_spawn`, which also carries spawn seeds. The handed-up and waiter data `tail_drive` publishes for deferred erased calls. `Shared` builtins whose structure is explicit before LIR, and an update-form rewrite that reads only the literal's own body |
| **No heuristics or fallbacks** | `.boxy` suspension decided by type and build mode alone. The `None` build's shape (no tasks, or coroutines) decided by computed suspendability. The crash exit chosen by target. Future lowerings such as JSPI would be chosen by target data, never as runtime fallbacks |
| **Backends follow only explicit `incref`/`decref`** | The pass emits ordinary LIR with explicit RC statements; drop entries are generated procedures. Each RC statement carries its atomicity explicitly. `Shared` lowers to builtin calls plus ordinary RC statements. Cell release is a runtime action through the type's generated drop helper. The design adds no exception beside the existing `.boxy` descriptor one |
| **Compiler invariants are a debug assertion, and `unreachable` in release** | Every "Certify" row below (C2–C7, C10, C11, V3, V10, cleanup push/pop balance). Nothing here is a release-build check |
| **Developer diagnostics only through `base.dev_diagnostics`** | The implementation plan's count of statements left plain; any report of cells freed by release (open question (e) in the shared-state RFC) would be the same kind |

Two kinds of failure aren't compiler invariants. Host bugs (breaking
P1–P10) are undefined behavior, like a `roc_alloc` that returns null; debug
runtimes trap on invalid wake-state transitions. Program bugs at run time (a
deadlock, a poisoned handle) are ordinary Roc crashes with messages.

## Invariants

The main RFC has four layers of [invariants](../concurrency-explicit-suspension.md#invariants);
the shared-state RFC adds [V1–V10](../concurrency-shared-variables.md#invariants).
Every language guarantee holds if the compiler and runtime invariants hold and
the platform meets its obligations. Rows are established by:

- **Certify**: debug-build LIR certification on every compiled program.
- **Model**: an executable model of the runtime and host contract, explored
  exhaustively over bounded tasks, threads and event orders.
- **Differential**: one program run two ways gives the same results, effect
  traces and allocation balance.
- **Conformance**: a debug harness a host links against checks obligations as
  ABI events happen.

Rows marked _Extended_ are ones the shared-state RFC changes, shown with the
change.

### Language Guarantees (G)

| | Guarantee | Rests on |
|---|---|---|
| G1 | Pure code is never interrupted: tasks interleave only at suspension points | C1, C2, R2 |
| G2 | _Extended._ No data races: tasks share immutable values and `Shared` handles, every change to shared state is an atomic update, and counts on shared values are atomic | C6, R2, P2, P6, P10 |
| G3 | Concurrency doesn't change meaning: a `None` build behaves as a one-thread task platform under the `None` scheduler rule | C1, C9 |
| G4 | Combinators return results in input order; only effect interleaving and arrival order depend on scheduling | R1, R3, Model |
| G5 | No task outlives the loop that started it, and the running set never escapes | C11, R3, R4 |
| G6 | A cancelled task runs no more of its own code and releases everything it owns | C5, R4, P3, P4 |
| G7 | OTP crash semantics: a crash ends its task, propagates through links to the nearest supervisor, is never observable to other code, and doesn't affect tasks outside that subtree | R5, R7, P1, P8 |
| G8 | Tail calls stay constant-space, including between effectful functions | C7 |
| G9 | At most `n` entries run under `limit(n)`, spawned ones included; an exit waits for a begun shielded entry; a loop ends once only background entries are left, cancelling them | R8 |

The shared-state RFC leaves G5 and G7 unchanged: a cell can outlive its loop,
but no task does, and poisoning turns a crash into later crashes that no code
can observe.

### Compiler Invariants (C)

| | Invariant | Established by |
|---|---|---|
| C1 | Transparency: if every suspending call completes at once, results, effects and crashes equal the program without the pass | Differential: always-`READY` host against a `None` build |
| C2 | _Extended._ Suspension only where declared: a `never` procedure has no suspendable site, the flag is set only at a suspension point, and each traces to a `suspending` marker, a `.boxy` erased effectful call, a loop head or exit, `supervise!`, or `update!` | Certify |
| C3 | Record completeness: the pending edge saves every value used after the call, except its result | Certify |
| C4 | No native addresses in records: frame callables at suspendable sites are heap-allocated, and suspendable pending cells live in task storage | Certify |
| C5 | Ownership conservation: the ledger equals what ARC owns after the call, and dropping or completing a record releases each owned value exactly once | Certify, plus cancel-at-every-site tests with allocation tracking |
| C6 | _Extended._ Atomicity soundness: anything two parallel tasks or the host can update counts on is atomic. Seeds are hosted arguments and results (with equality classes), sibling-aliased captures in `ParallelTasks`, and stores into cells; modes propagate forward; plain and atomic updates never race; a borrowed capture is never released by a child | Certify, each forward constraint locally |
| C7 | Constant-space tail calls: a frame-replacing call has no flag test and leaves no record | Certify, plus record-count tests on paused tail cycles |
| C8 | Strategy equivalence: `.lss` and `.boxy` give the same traces under the same schedule | Differential |
| C9 | _Extended (by V8)._ Sequential equivalence: a `None` build equals a `Tasks` build under the `None` scheduler rule; where no entry can wait, that's the input-order schedule without tasks. `update!` waits need no change to the schedule | Differential against a deterministic host with that policy |
| C10 | Resume fidelity: a resume entry continues at the named site with every value equal to the saved one, innermost first | Certify entry tables; Differential: always-pause host against never-pause host |
| C11 | Loop exits: every non-crash edge out of a loop body passes through the exit, and a running set is used only by its own loop | Certify |

### Runtime Invariants (R)

All are established by Model; the wake-state machine also runs under
ThreadSanitizer on real threads. R6 is liveness; the rest are safety.

| | Invariant |
|---|---|
| R1 | Exactly-once readiness: one `roc_task_ready` per park, from a completion of the source parked on; an early completion is counted in that source's wake word, never lost |
| R2 | Exclusive execution: given P1 and P2, a task never runs on two threads at once and is never re-entered |
| R3 | Tree shape: live tasks form a forest; a task ends only after its descendants, and a parent handles a child's report only after that subtree is gone |
| R4 | _Extended._ Cancellation finality: after cancellation, no more Roc code once the task next parks; its drop runs exactly once and runs no user code except its pending cleanups |
| R5 | Crash containment: the crashed task never runs again; its children and records are dropped exactly; propagation stops at the nearest `supervise!` or root; nothing reachable is freed; the only leak is what its running frames owned |
| R6 | _Extended._ Progress: waits on hosted operations and loop entries form no cycles, so given P7 every loop whose entries terminate is left, crashes or is cancelled. Waits on shared state can cycle; a stalled tree crashes the last waiter |
| R7 | Bounded restarts: `supervise!` restarts at most `max_restarts` times, then crashes with the last message |
| R8 | Limits: never more than `n` running entries under `limit(n)`, and an eligible entry starts as soon as one under the same limit finishes |

### Platform Obligations (P)

All are established by Conformance. Breaking P7 can only hang tasks; breaking
any other is undefined behavior. [Platforms](05-platforms.md) explains each to
platform authors.

| | Obligation |
|---|---|
| P1 | Resume a root once after creating it and a task once per `roc_task_ready`; never from inside a hosted function or another resume |
| P2 | A resume on another thread happens-after the event that readied the task (orders first resumes after spawn, and host data) |
| P3 | For each `ROC_PENDING`, write `*out` once, then call `roc_task_wake` once; neither after an abort |
| P4 | After `roc_task_abort` returns, never touch the slot or wake the task; release late results. Wake while holding the lock abort takes |
| P5 | Release hosted arguments exactly once; ownership of `*out` passes to Roc |
| P6 | Allow release on another thread, or confine tasks and shared values to one allocator's thread |
| P7 | Eventually resume every readied task and complete or abort every pending operation |
| P8 | After a terminal status, read only what that status allows, then free the root exactly once |
| P9 | Region reclamation frees a region only when nothing outside the crashed tree can reach it |
| P10 | `Tasks` platforms never run two resumes at once |

### Shared-State Invariants (V)

| | Invariant | Kind | Established by |
|---|---|---|---|
| V1 | Shared state is reached only through `Shared` functions, all effectful | Language | Typing rules |
| V2 | An update sees one state and stores its `Set` whole; a crashing update stores nothing and poisons the handle | Language | Model; differential runs under random schedules |
| V3 | Update and cleanup functions are pure, so they contain no suspension point; `Wait` suspends only after the update is abandoned | Compiler | Certify |
| V4 | An update holds one lock, runs only pure code under it, and releases it before any suspension | Runtime | Model |
| V5 | Cleanups run exactly once: `finally!` on every exit, `on_failure!` on cancellation and crash only | Language | Certify push/pop; cancel at every suspension point; crash at every update |
| V6 | After an update or cleanup crashes, every update of its handle crashes, and so does every waiter | Language | Tests |
| V7 | No lost wakeups: a waiter registers before releasing its cell, and a readied waiter that ends without storing probes on | Runtime | Model; tests and differential runs |
| V8 | C9 holds for programs that use `Shared`: a `None` build's `Wait` pauses its task, and deadlocks only where the coroutine schedule does | Language | Differential |
| V9 | Every cell is freed, by its count or by release; a released cell is never updated again | Runtime | Exact heap accounting on generated cyclic programs; assertion at every use; mutations |
| V10 | The probe form answers `Wait` exactly when the update form does, and the update form puts back the state it was given on `Keep` and `Wait` | Compiler | Certify; differential runs of probes against updates |

[Evidence](07-evidence.md) says which of these rows the prototype checks, and
how.
