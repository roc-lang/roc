# RFC: Suspendable Effects and Platform-Scheduled Concurrency

| | |
|---|---|
| Status | Draft. Not adopted; nothing here is part of `design.md`. |
| Scope | One builtin module, platform and hosted declarations, host ABI, one LIR pass |
| Supersedes | The stackful prototype on `origin/greenthreads` |

## Summary

This RFC lets Roc programs run effects concurrently without adding `async`,
futures or a task type to the language. It has three parts:

1. **A builtin `Concurrent` module** with structured combinators (`both!`,
   `all!`, `all_ok!`, `race!`) that take ordinary effectful functions.
2. **A platform opt-in.** A platform declares that it schedules tasks and marks
   the hosted functions that may *suspend*: return to the host before their
   result is ready. The host schedules suspended work however its domain needs.
3. **A compiler pass.** Every procedure that can reach a suspension point is
   compiled into a stackless state machine with an explicit frame and an
   explicit drop procedure, before the backends run.

Roc already separates pure functions (`->`) from effectful ones (`=>`), and has
no effect polymorphism. Only effectful code can suspend, so the compiler knows
which procedures need the transformation without any new annotation in
application code. The output is ordinary code that returns to its host, so it
runs on every WebAssembly engine, with no stack switching.

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
6. follow the compiler's rules: later stages consume explicit data, backends
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

### Errors

`all_ok!` takes functions that return `Try`. The first `Err` to arrive cancels
the others and is returned; otherwise the results come back in input order:

```roc
fetch_all! : List(Str) => Try(List(Str), [HttpErr(U16)])
fetch_all! = |urls| Concurrent.all_ok!(urls.map(|url| || Http.get_utf8!(url)))
```

`List.map` is pure, and so is the function passed to it: it only creates
effectful closures, and creating a function value is not an effectful call.
`all!` is the same without the `Try` handling.

### Racing and timeouts

`race!` returns whichever function finishes first and cancels the rest. It
takes its first function separately, so it can never be called with nothing to
race:

```roc
fetch_with_timeout! : Str => Try(Str, [HttpErr(U16), TimedOut])
fetch_with_timeout! = |url|
    Concurrent.race!(
        || Ok(Http.get_utf8!(url)?),
        [|| {
            Sleep.millis!(2_000)
            Err(TimedOut)
        }],
    )
```

### What concurrency means in Roc

- **Interleaving happens only at suspension points.** A function runs without
  interruption until it calls a hosted function that suspends, or waits on
  `Concurrent`. Pure code is never interrupted.
- **There are no data races.** Concurrent functions share only immutable
  values. A nested function can read a `var` but never reassign it, and it sees
  the value from when it was created, so there is no shared mutable state.
- **Effect order between branches is unspecified.** Within one branch, effects
  happen in program order, as always. Results are returned in a fixed order.
- **Cancellation is silent.** A cancelled branch stops at its current
  suspension point. No code of its own runs afterward (Roc has no `finally` or
  destructors), and its values are released. Effects it already performed stay
  performed.
- **The structure is lexical.** Every combinator returns only after every
  function it started, and everything those started, has finished or been
  cancelled. Nothing outlives the call that started it.

### On platforms that do not schedule tasks

On a platform that does not opt in, the combinators run their functions one at
a time, in order, starting each only when the previous one has finished:

- `all!` and `both!` run every function and return the same results;
- `all_ok!` stops at the first `Err`, and later functions never start;
- `race!` returns the first function's result, and the others never start.

So the timeout example still returns the fetch result, but the timeout cannot
interrupt a call that blocks. Packages can use `Concurrent` freely: they get
concurrency where the platform offers it and sensible sequential behavior
elsewhere.

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

A hosted function that is not marked, like `Stdout.line!`, always completes
before it returns and costs nothing extra. A `suspending` declaration in a
platform without `concurrency: Tasks` is a compile error. The platform's host
implements the task ABI below and a scheduler.

## Reference-level explanation

### Terms

- A **suspension point** is a call to a `suspending` hosted function, a call
  through an erased effectful function value in a `Tasks` build without
  lambda-set specialization (see below), or a wait inside `Concurrent`.
- A procedure is **suspendable** if it may reach a suspension point.
- A **task** is one running effectful function and its frames. The host starts
  root tasks through `provides` entries; each function a combinator runs is a
  child task.

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

- In `None` builds nothing is suspendable, and this pass does nothing.
- Under `.lss`, lambda sets give every call's possible targets, so the answer
  is exact. Calls through erased callables at host boundaries are suspension
  points exactly when the callable's type is effectful.
- Under `.boxy`, a generic body cannot know its callees, and its output must
  depend only on its module and its imports (design.md, "Per-Module Output").
  So in a `Tasks` build every call through an erased effectful function value
  is a suspension point. This is decided by the type and the build's mode
  alone.
- A pure procedure is never suspendable.

### Position in the pipeline

The pass runs after every pass that changes ownership or frame layout:

```text
LIR construction -> ARC -> frame_callables (.boxy) -> tail_drive
  -> reachable_procs_after_arc -> immortal_locals -> comptime_value_guards
  -> suspension lowering -> diagnostics, backends, LirImage
```

It runs after ARC because each frame's drop procedure needs ARC's answer to
what each state owns. Rust's coroutine transform runs after drop elaboration in
MIR for the same reason. The output is ordinary LIR: loads and stores into frame
memory, a switch on the state, calls, returns and explicit reference-count
statements. Backends, the interpreter and LirImage need no knowledge of
suspension.

### What ARC must publish

The drop procedures need, for each suspension point, the set of values owned at
that point. ARC does not publish ownership per program point today, and
recovering it from the emitted RC statements would be reconstruction. ARC
therefore records an explicit **suspension ledger**: for each suspension point,
the units it owns and their release helpers. The debug certifier checks the
ledger as it checks the rest of ARC's placement. `immortal_locals` removes
elided units from the ledger along with their statements.

### Storage that lived in the native frame

Several things the compiler places in a procedure's native frame must survive
a suspension. The pass moves each one into the coroutine frame when it is live
across a suspension point:

- **frame callables** (design.md, "Frame Callables"), which are built in the
  frame of the procedure that packs them;
- **pending cells** (design.md, "Pending Cells") of suspendable procedures;
- every other local that is live across the point.

Calls between suspendable procedures stop being native calls after lowering
(see below), so per-backend storage such as the LLVM tail-group argument blocks
never spans a suspension.

### Frames

For each suspendable procedure the pass produces:

- a **frame layout**: a state tag, the arguments, every value live across some
  suspension point, and a result slot for each suspending call. Values never
  live at the same suspension point share storage, as in rustc's coroutine
  layout;
- a **resume procedure** that switches on the state and runs to the next
  suspension point or to completion;
- a **drop procedure** that switches on the state and releases what the
  suspension ledger says that state owns. State 0, started but never resumed,
  owns the arguments.

Example:

```roc
fetch_two! = |a, b| {
    x = Http.get_utf8!(a)?
    y = Http.get_utf8!(b)?
    Ok(Str.concat(x, y))
}
```

lowers to, in pseudo-LIR:

```text
frame fetch_two { state: u8, a: Str, b: Str, x: Str, slot: Try(Str, [HttpErr(U16)]) }

resume(f, task):
  switch f.state
    0: poll = roc_http_get_utf8(task, &f.slot, f.a)   # a moves to the host
       if poll == pending { f.state = 1; return pending }
       goto 1
    1: x = f.slot?                                     # Err: decref f.b, return
       f.x = x
       poll = roc_http_get_utf8(task, &f.slot, f.b)   # b moves to the host
       if poll == pending { f.state = 2; return pending }
       goto 2
    2: y = f.slot?                                     # Err: decref f.x, return
       return ready(Ok(Str.concat(f.x, y)))

drop(f):
  switch f.state
    0: decref f.a; decref f.b     # never started
    1: decref f.b                 # waiting on the first request
    2: decref f.x                 # waiting on the second request
```

When a hosted call completes immediately (`ready`), execution continues without
returning. Values only need to be stored in the frame on the pending edge, so an
operation that does not actually suspend costs a status check.

### Calls between suspendable procedures

- **Direct calls** embed the callee's frame in the caller's frame. Under
  `.lss` every size is known, so a whole task usually needs one allocation.
  When a call has several possible targets (a lambda set), the embedded frame
  is the union of their frames.
- **Recursion.** In a strongly connected component of suspendable procedures,
  each call that closes a cycle allocates the callee's frame on the heap. The
  pass records this on the call (`frame: inline | heap`). Unlike Rust's
  `Box::pin`, users write nothing. Frame-replacing calls are exempt (next
  item), and self-tail calls are already loops.
- **Frame-replacing calls** keep their guarantee of constant space. The members
  of a tail group share one frame slot, sized for the largest member and tagged
  with the member that occupies it. A frame-replacing call drops the caller's
  state and starts the callee in the same slot.
- **Erased calls** allocate the callee's frame at the call. The
  erased-callable record of a suspendable callee carries its frame size and its
  start, resume and drop entry points.

A task has exactly one suspended path, because concurrency always goes
through child tasks. Resuming a task re-enters its root resume procedure,
which follows the states down to the innermost frame. Resume cost is linear in
the depth of embedded frames, as in Rust.

### Host ABI

Shown in C. Only `concurrency: Tasks` platforms see any of it. Compiled Roc
code and hosts share linker symbols, with no operation table (design.md:
`RocOps` "is not part of any host ABI"). The new runtime symbols join the fixed
set in `host_abi.ExternHostFns` and the glue templates for opting-in
platforms.

```c
typedef struct RocTask RocTask;                          // opaque, owned by Roc
typedef enum { ROC_READY, ROC_PENDING } RocPoll;
typedef enum { ROC_TASK_PENDING, ROC_TASK_DONE, ROC_TASK_CANCELLED } RocTaskStatus;

// A `provides` entry, under its header symbol, starts a root task.
// The result is written to result_out when the task completes.
RocTask *roc_main(RocList args, void *result_out);

// Exported by the Roc runtime.
RocTaskStatus roc_task_resume(RocTask *task);
void roc_task_wake(RocTask *task);      // a pending hosted result has been written
void roc_task_cancel(RocTask *root);    // request cancellation of a root task
void roc_task_free(RocTask *root);      // after DONE or CANCELLED

// Implemented by the host.
void roc_task_ready(RocTask *task);     // resume this task once, on any thread
void roc_task_abort(RocTask *task);     // abandon the operation it is waiting on

// A suspending hosted function gains its task and its result slot.
RocPoll roc_http_get_utf8(RocTask *task, HttpResult *out, RocStr url);
```

**What the Roc runtime guarantees.** Every task has an atomic wake state:
running, parked, woken while running, or queued.

- `roc_task_ready` is called at most once per park, so a host never resumes a
  task twice for one wake and never resumes a running task. A wake that arrives
  while the task is still running (a child finishing on another thread, or a
  hosted result arriving before the hosted function has returned) is recorded,
  and the task continues instead of parking.
- A child counts as finished, and wakes its parent, only when it and every task
  it started have completed or been dropped.
- Roc owns child tasks. After `roc_task_resume` returns for a child, the host
  forgets it.

**What the host guarantees.**

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
4. **Roots.** After a root returns `ROC_TASK_DONE` or `ROC_TASK_CANCELLED`, the
   host reads `result_out` (for `DONE`) and calls `roc_task_free`. To cancel a
   root, it calls `roc_task_cancel` and keeps resuming the root when it is
   readied until it reports `CANCELLED`.

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
};
// roc_task_ready(task) pushes onto `scheduler`.
```

### The `Concurrent` module

```roc
Concurrent :: [].{
    both! : (() => a), (() => b) => (a, b)
    all! : List(() => a) => List(a)
    all_ok! : List(() => Try(a, err)) => Try(List(a), err)
    race! : (() => a), List(() => a) => a
}
```

They are written in Roc over three internal low-level operations, each with
explicit `RcEffect` data:

| Operation | Meaning in `Tasks` builds | Meaning in `None` builds |
|---|---|---|
| `task_spawn(f)` | Create a child task and ready it | Record `f`; run nothing yet |
| `task_await_any(children)` | Suspend until any child finishes; report which, with its result or `Cancelled` | Run the earliest unstarted child to completion and report it |
| `task_cancel(child)` | Request cancellation; the child later reports `Cancelled` | Discard an unstarted child |

With these definitions, one implementation of each combinator gives both the
concurrent and the sequential behavior described above:

- `all!` spawns every function, awaits each finish, and stores results by
  index. `both!` is `all!` over two results wrapped in a tag union.
- `all_ok!` cancels the remaining children at the first `Err` to finish (in
  `None` builds, the lowest index), awaits their cancellation, and returns it.
- `race!` cancels every other child as soon as one finishes, awaits their
  cancellation, and returns the winner.

Combinators always await cancelled children before returning. Together with the
runtime's subtree rule, this is what keeps the structure lexical.

### Reference counting across tasks

A child task may run on another thread, so the closure passed to `task_spawn`
seeds host visibility in the thread-confined analysis, as a hosted-call
argument does. Its captures and everything reachable from them use atomic
counts, and so does each child's result. `RcEffect` gains explicit
`host_visible_args` and `host_visible_result` masks for this, following
`result_shares_args`. Values that never leave one task stay confined even when
the task moves between threads, because of host rule 1.

### Compile-time evaluation and the interpreter

Effectful code never runs at compile time, so compile-time evaluation is
unaffected. The interpreter runs the lowered LIR like any backend.

### Verification

- **Debug LIR certification**, extended: in every state, a frame's drop
  procedure releases each ledger unit exactly once, and no single-thread
  reference-count statement names a value reachable from a `task_spawn`
  capture.
- **Cancellation at every state.** For each suspension point in a test
  program, suspend there, cancel, drop, and check that no allocation is
  leaked or released twice.
- **Strategy equivalence.** `.lss` and `.boxy` produce the same results and
  effect traces under a deterministic scheduler.
- **Wake races under ThreadSanitizer**, with tasks resumed on random threads
  and hosted results completed before and after the hosted function returns,
  in the style of `zig build run-test-builtins-tsan`.
- **Fuzzing** that composes small typed effectful constructs (loops, matches,
  closures, recursion, combinators) with randomly placed suspension points, as
  AGENTS.md requires.

## Drawbacks

- **A large new compiler pass**, plus a suspension ledger in ARC and a new host
  ABI that every opting-in platform must implement correctly.
- **`.boxy` pays more.** In `Tasks` builds every erased effectful call is a
  suspension point, so dev builds transform more procedures than optimized
  ones.
- **Code size.** Each suspendable procedure gains resume and drop code.
- **Overhead in effectful code.** Spills on pending edges, state dispatch on
  resume, and resume cost linear in frame depth. Pure code pays nothing.
- **Recursive effectful functions allocate** a frame per level at each call
  that closes a cycle.
- **No preemption.** A long pure computation keeps its thread until it
  finishes. Platforms that need fairness use several threads.
- **Debugging.** A parked task has no native stack, so debuggers and crash
  traces cannot show where it is waiting without new tooling.
- **Hot reload** across a change to a suspended procedure's frame layout is not
  supported.

## Rationale and alternatives

| Alternative | Why not |
|---|---|
| Stackful green threads, as prototyped | No portable wasm story, leaky cancellation, fixed stacks, per-architecture assembly (see Motivation) |
| JSPI as the wasm lowering | Browsers only |
| WasmFX stack switching | Phase 3, not in WebAssembly 3.0; one Wasmtime platform |
| Asyncify | About 50% code-size and speed cost from conservative instrumentation; Roc knows which procedures to transform |
| A user-visible `Task` or `async` type | Adds a second function color; Roc replaced `Task` with effectful functions |
| Platform-defined concurrency modules | Packages using concurrency would be tied to one platform |
| Unstructured `spawn` | Tasks could outlive their caller, making cancellation and errors ad hoc |
| The host polls tasks (Rust `Future::poll`) | A wake-and-ready protocol fits io_uring, IOCP, JavaScript promises and callback-based APIs directly |
| Suspension as a property of every hosted call | Platforms would pay for state machines on calls that never suspend |
| Suspending functions listed in the platform header | Packages and `.boxy` units do not import the platform root, so the information must live with the declaration |

The decisive argument: Kotlin and Rust need visible `suspend` and `async`
markers because their compilers must know which functions to transform. Roc's
`=>` already carries that information for its own reasons, and the absence of
effect polymorphism makes it exact. Roc gets the portability of stackless
coroutines without their usual cost to users.

## Prior art

| Work | Relevance |
|---|---|
| de Moura and Ierusalimschy, *Revisiting Coroutines* (TOPLAS 2009) | Stackful vs. stackless terminology; asymmetric coroutines are as expressive as one-shot continuations |
| Leijen, *Type Directed Compilation of Row-Typed Algebraic Effects* (POPL 2017) | Selective CPS of only the code whose types say it can perform effects |
| Xie and Leijen, *Generalized Evidence Passing for Effect Handlers* (ICFP 2021) | Effect handlers compiled to plain C with no stack switching |
| rustc coroutine transform | State machines over MIR after drop elaboration; storage overlap between states |
| Kotlin coroutines | CPS state machines; results that are either a value or a suspension marker |
| Swift concurrency (SE-0304, SE-0317) | Structured task groups and `async let`; frames from a per-task stack-discipline allocator |
| Zig 0.6–0.10 async | Stackless frames of known size; recursion made frame sizes uncomputable; removed in 0.11 |
| Java structured concurrency (JEP 505) | Children scoped to a block and cancelled on failure |
| Trio nurseries | Children cannot outlive their scope |
| js_of_ocaml and wasm_of_ocaml | Partial CPS for effects on JavaScript and wasm, with JSPI and stack-switching modes as alternatives |

## Unresolved questions

1. Is `suspending` the right spelling for the declaration marker, and does
   `concurrency: Tasks` belong in the header, or should it follow from the
   presence of a `suspending` declaration?
2. How should `Concurrent` behave when a branch crashes? Today a crash stops the
   program, and that may stay the rule.
3. Should frames come from `roc_alloc`, or from a per-task allocator hook like
   Swift's task allocator?
4. How should a host map tasks onto WASI 0.3 and onto JavaScript promises?
   Both need a scheduler on the host side of the boundary. Reference hosts would
   answer this.
5. How do `.boxy` frames hold dynamically sized values: inline after a
   descriptor-sized header, or as separate allocations?
6. How does a deferred erased call (design.md, "Deferred Erased Calls") whose
   target is suspendable hand its pending call up through a suspension?
7. Does `race!` need a variant that reports which branch won?

## Future possibilities

- **Dynamic nurseries** (`Concurrent.scope!` with `spawn!` and `join!`) for
  workloads like crawlers that discover work as they go.
- **Concurrent streams**, such as a `Stream.map_concurrent!` with a
  concurrency limit, built on the same operations.
- **Data parallelism for pure code.** Purity lets a platform run pure work
  across cores; on wasm this already works with shared memory, atomics and
  workers.
- **Deterministic simulation testing.** A test platform with a seeded,
  single-threaded scheduler makes every interleaving reproducible.
- **Alternative lowerings.** JSPI or WasmFX could become explicit,
  target-selected lowerings if measurements show a benefit. They would be
  chosen by target data, never as runtime fallbacks.

## Implementation plan

1. Parse `concurrency` and `suspending`; add the mode to unit keys; record
   suspendability on LIR procedures and calls.
2. Add the ARC suspension ledger and its certification.
3. Implement the pass for `.lss`: frames, resume, drop, embedded and heap
   frames, tail groups, frame callables and pending cells.
4. Add the task runtime with its wake protocol, the host ABI, and glue for one
   native reference platform.
5. Implement `Concurrent` and its low-level operations, including `None`
   behavior.
6. Extend the pass to `.boxy` and erased calls.
7. Build browser and WASI 0.3 reference hosts.

## References

- de Moura and Ierusalimschy, [Revisiting Coroutines](https://www.cs.tufts.edu/~nr/cs257/archive/roberto-ierusalimschy/revisiting-coroutines.pdf), TOPLAS 31(2), 2009
- Leijen, [Type Directed Compilation of Row-Typed Algebraic Effects](https://popl17.sigplan.org/details/POPL-2017-papers/69/Type-Directed-Compilation-of-Row-Typed-Algebraic-Effects), POPL 2017
- Xie and Leijen, [Generalized Evidence Passing for Effect Handlers](https://www.microsoft.com/en-us/research/uploads/prod/2021/08/genev-icfp21.pdf), ICFP 2021
- Phipps-Costin et al., [Continuing WebAssembly with Effect Handlers](https://www.microsoft.com/en-us/research/publication/continuing-webassembly-with-effect-handlers/), OOPSLA 2023
- rustc, [coroutine transform](https://doc.rust-lang.org/nightly/nightly-rustc/rustc_mir_transform/coroutine/index.html) and [layout](https://doc.rust-lang.org/nightly/nightly-rustc/rustc_mir_transform/coroutine/layout/index.html)
- Kotlin, [Asynchronous programming with coroutines](https://kotlinlang.org/spec/asynchronous-programming-with-coroutines.html) (language specification)
- Swift, [SE-0304 discussion](https://forums.swift.org/t/se-0304-structured-concurrency/45314), [SE-0317 discussion](https://forums.swift.org/t/se-0317-async-let/48848), [task allocator](https://sources.debian.org/src/swiftlang/6.0.3-2/swift/stdlib/public/Concurrency/TaskAlloc.cpp)
- Zig, [async in 0.11](https://zig.guide/0.11/async/introduction)
- OpenJDK, [JEP 505: Structured Concurrency](https://openjdk.org/jeps/505)
- Smith, [Notes on structured concurrency](https://vorpus.org/blog/notes-on-structured-concurrency-or-go-statement-considered-harmful/)
- [js_of_ocaml effect handlers](https://ocsigen.org/js_of_ocaml/latest/js_of_ocaml/effects.html)
- Bytecode Alliance, [WASI 0.3 launched](https://bytecodealliance.org/articles/WASI-0.3)
- [Wasmtime proposal status](https://docs.wasmtime.dev/stability-wasm-proposals.html)
- V8, [JSPI](https://v8.dev/blog/jspi); [caniuse: JSPI](https://caniuse.com/mdn-webassembly_jspi)
- Zakai, [Asyncify in Practice](https://kripken.github.io/talks/2022/asyncify.html)
