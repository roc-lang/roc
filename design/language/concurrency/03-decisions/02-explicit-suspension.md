# 2. Explicit Suspension

| | |
|---|---|
| Status | Settled by prototype, on an executable model of the lowering. Its cost in a real compiler is argued, not measured |
| Builds on | [1. Stackful green threads](01-stackful-green-threads.md), which this replaces |
| Detail | [Suspendability](../../concurrency-explicit-suspension.md#suspendability), [Capture by unwinding](../../concurrency-explicit-suspension.md#capture-by-unwinding), [Cost model](../../concurrency-explicit-suspension.md#cost-model), [Drawbacks](../../concurrency-explicit-suspension.md#drawbacks), [Rationale and alternatives](../../concurrency-explicit-suspension.md#rationale-and-alternatives) |

Without a stack per task, and without adding `async`/`await` to the language, how does a task pause partway through a call and pick up again later?

## The Decision

A task pauses only at a *suspension point*: a call that may return to the host before its result is ready. The compiler knows where every one is, from information Roc programs already carry.

**Only effectful code can pause.** Roc already separates pure functions (`->`) from effectful ones (`=>`), and has no effect polymorphism. A pure function can't call an effectful one, so it can't reach a suspension point, and it compiles exactly as it does today.

**Platforms mark what can pause.** A platform author marks each hosted function that may pause with `suspending`, where it's declared:

```roc
# Http.roc, in the platform
Http := [].{
    suspending get_utf8! : Str => Try(Str, [HttpErr(U16)])
}
```

**Application code doesn't change.** This function calls `Http.get_utf8!` twice, and nothing in it says that either call might pause:

```roc
fetch_two! = |a, b| {
    x = Http.get_utf8!(a)? # The task may pause here...
    y = Http.get_utf8!(b)? # ...and here.
    Ok(Str.concat(x, y))
}
```

The complete list of suspension points is: a call to a `suspending` hosted function, a concurrent loop's head or exit, `supervise!`'s wait, an `update!` (from [shared state](06-shared-state.md); in a `None` build, only one whose function can answer `Wait`), and, in `.boxy` builds, a call through an erased effectful function value (below). A procedure is *suspendable* if it may reach one.

### Capture by Unwinding

A task runs on the native stack of whichever thread resumes it. Its state is saved only when it actually pauses, by *unwinding*:

1. After each call that may pause, the caller tests a flag in the task. If it's clear, the call didn't pause, and execution goes on as usual.
2. If the flag is set, the caller saves the values it still needs into a small heap *record*, along with which call it was in, and returns.
3. Its own caller sees the flag and does the same, all the way out to the task's entry. The chain of records is the paused task.
4. To resume, the runtime runs the innermost record's resume entry, which restores its values into a fresh frame and continues just after the paused call. When that procedure returns, its result goes to the next record out.

Resuming innermost first means a resume costs only the work that's left, not a re-entry from the task's root. Cancelling a paused task drops its records, and each record knows exactly which of its values it owns, so nothing leaks.

The technique is Pettyjohn et al.'s [generalized stack inspection](https://cs.brown.edu/~sk/Publications/Papers/Published/pcmkf-cont-from-gen-stack-insp/), the same idea as Binaryen's [Asyncify](https://kripken.github.io/talks/2022/asyncify.html), restricted to the procedures Roc knows can pause. The output is ordinary code that returns to its host, so it runs on every WebAssembly engine, with no stack switching, and every backend and the interpreter run it without knowing about suspension.

How the pass fits into the compiler pipeline, and what ARC publishes for it, is in [semantics](../06-semantics.md).

### Under `.boxy`

Under `.lss`, lambda sets give every call's possible targets, so the compiler knows exactly which calls through function values can pause. Under `.boxy`, a generic body can't know its callees, and its output must depend only on its module and its imports.

So under `.boxy`, every call through an erased effectful function value is a suspension point, decided by its type alone. That holds in `None` builds as well as `Tasks` builds, since a module can't know whether a module that imports it waits.

## What We Tried

| Alternative | Why not |
|---|---|
| `async`/`await`, or a user-visible `Task` or future type | Adds a second function color. Roc replaced `Task` with effectful functions |
| Stackful green threads, as prototyped | No portable wasm story, leaky cancellation, fixed stacks, per-architecture assembly ([decision 1](01-stackful-green-threads.md)) |
| Continuation-passing style for the whole program | Transforms pure code too, which [requirement 5](../../concurrency-explicit-suspension.md#requirements) rules out: pure code must cost nothing. Koka's selective CPS of only the code whose types say it can perform effects is closer to what Roc does |
| State machines with frames embedded in callers, as rustc and Kotlin do (this RFC's earlier draft) | Needs a whole-program frame layout, unions over lambda sets, special cases for recursion and tail groups, and a frame-size ABI for erased callables. It also keeps every value that lives across a pause in memory even when nothing pauses |
| Zig's stackless async frames (Zig 0.6–0.10) | Not proposed for Roc, but a warning: recursion made frame sizes uncomputable, and Zig removed it in 0.11. Records don't have this problem, since a deep recursion that pauses leaves one record per level |
| LLVM coroutine intrinsics (`llvm.coro.*`) | Only the LLVM backend would get them; the dev and wasm backends and the interpreter would each need their own version |
| Asyncify itself | Runs after code generation, on wasm only, and instruments conservatively, at about a 50% cost in code size and speed. Roc uses the same idea inside the compiler, on every backend, touching only call sites that can pause |
| JSPI or WasmFX for wasm | JSPI works only in browsers. WasmFX is at phase 3, isn't in WebAssembly 3.0, and runs on one Wasmtime platform |
| Every hosted call may suspend | Every effectful call site would pay the flag check and save code, including calls that never pause |
| Suspending functions listed in the platform header | Packages and `.boxy` units don't import the platform's root module, so the marker has to live with the declaration |

## Why This One

Kotlin and Rust need visible `suspend` and `async` markers because their compilers must know which functions to transform. Roc's `=>` already carries that information, for its own reasons, and the absence of effect polymorphism makes it exact. So Roc gets the portability of stackless coroutines without their usual cost to the people writing application code.

Capturing by unwinding then keeps the compiler side small. The transformation is local to each call site that can pause, and needs no whole-program frame layout. Code that doesn't pause runs as it does today, plus one flag test after each call that could have.

### Isn't This Function Coloring?

*Function coloring* is where a language splits functions into two kinds, such as sync and `async`, and a function of one kind can only be called from the other kind through some adapter. Each color spreads: making one function async forces its callers to change.

Roc already has two kinds of function, `->` and `=>`, and that split exists for purity, not concurrency. The requirement this design holds to is that concurrency adds no *second* color:

- **Effectful code stays direct-style.** Calling a `suspending` hosted function looks exactly like calling any other effectful function. There's no `await`, no future to unwrap, and no new type.
- **Nothing spreads.** When a platform marks a hosted function `suspending`, no application code changes. The compiler works out which procedures can reach it, and those procedures' callers are already effectful.
- **The same function works both ways.** `Http.get_utf8!` is the same function in a sequential program and inside `Concurrent.both!`. A package can use concurrency freely and runs on platforms that don't schedule tasks.

The one marker this adds, `suspending`, is written by platform authors on hosted declarations. Application and package authors never write it.

## What It Costs

| Situation | Cost (first-principles estimates, except where measured) |
|---|---|
| Pure code | None |
| A program that can't wait, on a platform that doesn't schedule tasks | None in `.lss` builds: the pass does nothing. `.boxy` builds still check after erased effectful calls |
| An effectful call that doesn't pause | One load and a predictable branch on the task's flag |
| Pausing and resuming | Saving and restoring a few values per paused procedure, plus the wake protocol and the host's queue |
| Code size | Save code at each call site that can pause, and resume and drop entries, in effectful code only |
| `.boxy` dev builds | A flag check after every erased effectful call |

The drawbacks, without softening:

- **New compiler and runtime work.** A pass, a ledger in ARC of which values each paused call site owns, and a host ABI that every opting-in platform must implement correctly.
- **Pausing costs grow with depth.** A pause saves one record per procedure on the native stack. A stackful switch costs the same at any depth.
- **`.boxy` pays more.** Dev builds check the flag at more call sites than optimized builds, and in a `None` build they do so even in a program that never waits.
- **Waiting costs something on every platform.** A program that can wait, for example on shared state, is compiled for pausing even where the platform schedules nothing, because that's what lets one task wait for another there ([decision 4](04-one-meaning-per-build.md)).
- **No preemption.** A long pure computation keeps its thread until it finishes, and a cancelled entry stops only at a suspension point.
- **Debugging.** A parked task has no native stack. Its records name their call sites, so its waiting path can be printed, but debuggers need new tooling to show it.
- **Hot reload** across a change to a paused procedure's call sites or saved values isn't supported.

## Evidence

- **The lowering matches a stackful oracle.** The [Python prototype](../../concurrency-prototype/README.md) runs fuzzed programs through an unwinding engine and a stackful engine under the same host and schedule, and requires identical results, effect traces and heap operations. It checks record completeness, ownership (cancelling at every suspension point leaves the heap exact) and resume fidelity on every generated program, under `.lss` and `.boxy`. See [evidence](../07-evidence.md).
- **Native microbenchmarks.** A hand-lowered C benchmark (Apple M5 Pro, clang `-O2`) puts the flag check at 2.5% to 5% in a kernel made only of calls, and one pause and resume at about 5 ns, 12 ns and 40 ns at depths 1, 4 and 16, with records from a per-task buffer. Records from `malloc` cost five times as much.
- **Wasm microbenchmarks.** The same benchmark on wasm (V8 in Node, `-O2`) shows the flag check costing 40% to 58% in a kernel made only of calls. Why V8 makes it costly wasn't investigated.
- **What isn't measured.** These measure hand-lowered code, not a compiler. The rest of the cost model is first-principles estimates, to be replaced by ReleaseFast benchmarks of the real pass on a reference host.
