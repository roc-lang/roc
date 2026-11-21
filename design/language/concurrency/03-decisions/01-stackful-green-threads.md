# 1. Stackful Green Threads

| | |
|---|---|
| Status | Settled by argument. A prototype showed stackful tasks work natively; the reasons against them are about wasm, cancellation, memory and portability |
| Builds on | Nothing: this is where the design starts |
| Detail | [Why not stackful green threads](../../concurrency-explicit-suspension.md#why-not-stackful-green-threads), [Rationale and alternatives](../../concurrency-explicit-suspension.md#rationale-and-alternatives) |

Why not give each task its own stack and switch between stacks, as Go does and as Roc's own `origin/greenthreads` prototype did?

## The Decision

Roc doesn't give tasks their own stacks. Every task runs on the ordinary native stack of whichever thread resumes it, exactly like Roc code today.

A *stackful* task, or green thread, is one that owns a whole call stack: pausing it means saving a few registers and switching to another stack, at any call depth. A *stackless* task owns no stack, so pausing it means saving what each paused function still needs somewhere else. Roc takes the stackless route, and [decision 2](02-explicit-suspension.md) says how.

## What We Tried

### The `origin/greenthreads` Prototype

Richard Feldman built a stackful runtime in two commits on `origin/greenthreads` (September 2, 2025 and September 19, 2025, unmerged). It's a Zig library with its own tests, not wired into the compiler:

- **A stack per green thread.** `spawn` memory-maps a stack of 1 MB by default (`mmap` on Unix, `VirtualAlloc` on Windows) plus one guard page.
- **A guard page.** The lowest page of each stack is made inaccessible (`mprotect` with no permissions, or `VirtualProtect` on Windows), so a stack overflow faults instead of silently writing over other memory. A test installs a signal handler and checks that writing to the guard page faults.
- **A hand-written context switch.** `switch_context_impl` saves the callee-saved registers of the current context and loads the target's, in one assembly file per architecture: 7 words on x86_64 (`r12`–`r15`, `rbx`, `rbp`, `rsp`) and 21 on aarch64.
- **Spawn, yield and resume.** A thread-local `ThreadStore` holds the green threads, with tests for one context switch, a thread that yields, and several yields and resumes.

The second commit moved the files into `src/greenthreads/` and extended the store. Together the two commits add about 600 lines.

The prototype answered the question it was built for: stackful switching works on native targets, and it's cheap. It didn't answer the questions below, which no stackful design can answer well for Roc.

### Other Ways to Get a Stack

| Alternative | What it is | Why not |
|---|---|---|
| Stackful green threads, as prototyped | A fixed native stack per task, switched with assembly | No portable wasm story, leaky cancellation, fixed stacks, per-architecture assembly (below) |
| Copying stack segments in and out, as Python's [greenlet](https://greenlet.readthedocs.io/) does | Tasks share one stack; a pausing task's part of it is copied to the heap and copied back to resume | Impossible on wasm, whose value stack isn't addressable |
| [JSPI](https://v8.dev/blog/jspi) as the wasm lowering | The browser suspends the wasm stack when it calls a JavaScript promise | Browsers only |
| WasmFX stack switching | A wasm proposal that adds stack switching to the engine | Phase 3 and not in WebAssembly 3.0; Wasmtime supports it on one platform |

## Why This One

Stackful coroutines are the obvious way to make direct-style code concurrent. They're a poor foundation for Roc for four reasons.

### WebAssembly

Roc compiles to wasm, and a design that doesn't run there would split the language. Core wasm can't switch stacks, and each way around that covers only part of the targets:

- **Stack switching (WasmFX)** is at phase 3 and was left out of [WebAssembly 3.0](https://webassembly.org/news/2025-09-17-wasm-3.0/). [Wasmtime](https://docs.wasmtime.dev/stability-wasm-proposals.html) supports it only on x86_64 Linux.
- **JSPI** works in Chrome 137+ and Safari 27, but only in browsers.
- **[WASI 0.3](https://bytecodealliance.org/articles/WASI-0.3)** shipped the stackless "callback" async lift. The stackful lift is still behind a flag.

So the wasm engine a Roc program targets would decide whether it could be concurrent at all. A stackless design returns to its host like any other code, so it runs on every engine.

### Cancellation

A parked stack holds reference-counted values that the compiler can no longer see. They live in native frames, at addresses and in registers that only the code that was running knows how to read.

Cancelling a stackful task therefore means abandoning its stack, and abandoning it leaks everything on it. Other languages release such values by unwinding the stack and running destructors. Roc has no unwinding to release them, and by design no `finally` or destructors to run.

The same holds for crashes. A stackful runtime leaks every frame of the crashed task. With [explicit suspension](02-explicit-suspension.md), a paused procedure's state is a record whose contents the compiler wrote down, so it's dropped exactly, and a crash leaks only what the frames that were running at that moment owned.

### Memory

Each task needs a fixed stack: 1 MB of address space plus a guard page in the prototype. A server with thousands of requests in flight pays that for each one.

Go avoids fixed stacks by starting small and copying a stack to a bigger one when it fills up. That works because Go's runtime knows every pointer into the stack and can fix it up. Roc's native code uses the C ABI, where nothing tracks the pointers into a stack, so stacks can't be moved to grow them.

### Portability

Every architecture needs its own context switch, in assembly. The prototype has two. Each new target would need another, and wasm can't have one at all.

## What It Costs

Giving up stacks has real costs, which [decision 2](02-explicit-suspension.md) takes on:

- **The compiler must do the work.** Something has to save a paused task's state. Without a stack to keep, the compiler transforms every procedure that can pause.
- **Pausing costs grow with depth.** A stackful switch saves a fixed set of registers at any depth. A stackless pause saves only live values, but once per paused procedure. In the microbenchmark below, unwinding is cheaper up to about 16 paused procedures and about equal at 16.
- **Debugging.** A parked stackful task keeps a real stack that a debugger can show. A stackless one doesn't, so debuggers need new tooling to show where it's waiting.

## Evidence

The decision rests on argument. Nothing measured shows stackful tasks failing on native targets; the prototype shows the opposite.

What the sources do record:

- **The prototype works natively.** `origin/greenthreads` spawns, yields and resumes green threads, and its guard page catches overflow, on aarch64 and x86_64.
- **Switching is cheap, but allocating stacks isn't counted.** A hand-lowered C microbenchmark ([`bench/unwind.c`](../../concurrency-prototype/README.md), Apple M5 Pro, clang `-O2`) puts a stackful switch at 21–44 ns over depths 1 to 16, against about 5 ns, 12 ns and 40 ns for one pause and resume by unwinding at depths 1, 4 and 16. The stackful figures don't include allocating each task's stack. These measure hand-written C, not a compiler.
- **Crashes leak more on stacks.** The [Python prototype](../../concurrency-prototype/README.md) pins this in a test (its Finding 3): a procedure paused before a crash is a record and is dropped exactly, while the same procedure on the native stack leaks its live values.
- **Stackful stays the reference.** The Python prototype checks the stackless lowering against a stackful oracle built on generators, and requires the two to give identical results, effect traces and heap operations. Stackful tasks remain the meaning Roc aims for; they're just not how Roc gets there.
- **The wasm facts** are platform facts, cited above, current as of the main RFC.

What replaced stacks is [decision 2: explicit suspension](02-explicit-suspension.md). Tasks run on the native stack, and a task that pauses saves each paused procedure into a small heap record and returns.
