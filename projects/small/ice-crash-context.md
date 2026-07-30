# Crash-Context Handler for Internal Compiler Errors

## Problem

When the compiler itself panics — an `unreachable`, a failed
`std.debug.assert`, an explicit `std.debug.panic("… invariant violated: …")`,
or a segfault — the user sees a bare Zig stack trace. The stack trace names
Zig functions and addresses, but it never says the one thing that turns an
internal compiler error (ICE) into an actionable bug report: **what the
compiler was doing.** Which phase was running? Which module was in flight?
Which definition or proc was being processed? And — most importantly for
anyone triaging — what single command reproduces it?

That information exists, in structured form, at the moment of the crash. The
compile coordinator knows the module's name, its filesystem path, and its
`Phase`; the backend knows which proc it is lowering; the interpreter knows
its entry point. But none of it reaches the panic output, so every ICE report
starts with a round-trip: "which file were you compiling? can you share it?
what command did you run?" A crash that printed *"panicked while type-checking
module `Json.Decode` (/…/Json/Decode.roc); reproduce with `roc check
/…/Json/Decode.roc`"* before the stack trace would collapse that round-trip to
zero.

This project adds a small, zero-allocation, thread-local **crash-context
stack** that each phase pushes onto at cheap boundaries, plus a `pub const
panic` override that prints the panicking thread's context frames (and a
copy-pasteable repro command) ahead of the normal stack trace. It changes no
behavior on any non-crash path.

## Current state

There is no custom panic handler in the CLI binary. `src/cli/main.zig`'s
`main` (`:1019`) installs the signal handler for stack overflow
(`base.stack_overflow.installForCurrentThread()`, `:1031`) but declares no
`pub const panic`, so every explicit panic and every `unreachable` in a
Debug or ReleaseSafe build routes to `std.debug.defaultPanic` and prints only
a stack trace. `src/cli/main.zig` alone contains 28 `std.debug.panic(
"… invariant violated: …")` sites (of 46 `std.debug.panic` calls in the
file) — for example `:1680` (`"default roc command invariant violated:
hosted section size {d} differs from checked hosted catalog size {d}"`),
`:2628` (`"default roc command invariant violated: no platform entrypoints
in checked LIR root metadata"`), `:2751` (`"interpreter run invariant
violated: missing LIR shared-memory handle"`), `:3363` (`"default app run
invariant violated: no platform entrypoints"`), and `:5549` (`"dev run
invariant violated: LIR proc {d} was not compiled before image symbol
publication"`) — each one a place where an ICE can surface today with no
context.

Across `src/` (559 `.zig` files) the explicit-abort surface is large:

- 99 `@panic` sites.
- 627 `std.debug.panic` sites.
- 726 combined explicit panic sites (`@panic` + `std.debug.panic`).
- 1751 `unreachable` sites.

A caveat on `unreachable`: reaching it in `ReleaseFast` is undefined
behavior, not a panic, so a context handler cannot reliably fire there. The
handler's value is therefore concentrated on the paths that *do* trap
deterministically: Debug and ReleaseSafe builds (where `unreachable` and
failed `assert` panic), and explicit `@panic`/`std.debug.panic` in every
build mode. Those are exactly the paths a developer or CI runs when an ICE is
first seen, so that is the right coverage target.

## Background — the pieces already in the repo

Everything this project needs already exists in some form; the work is to
connect them.

- **A custom `pub const panic` override works here.**
  `src/snapshot_tool/main.zig:36` declares
  `pub const panic = std.debug.FullPanic(panicHandler)`. Its handler
  (`:46`) reads thread-local state (`panic_jmp`, `panic_msg` at `:38`–`:39`),
  does its work, then chains to `std.debug.defaultPanic(msg, @returnAddress())`
  (`:60`) when no special handling applies. This is the exact shape the CLI
  override should take: inspect thread-local state, print, then delegate to the
  default handler so the stack trace still appears.

- **The coordinator already holds the "what am I doing" facts, per module,
  in structured form.** `src/compile/coordinator.zig` defines the `Phase`
  enum (`:418`): `Parse → Parsing → Canonicalize → WaitingOnImports →
  WaitingOnPlatformRequirements → TypeCheck → Done`. Each `ModuleState`
  carries `name` (`:460`), `path` (`:462`), and `phase` (`:484`). Worker
  threads pull tasks in `workerThread` (`:4830`) and dispatch them in
  `executeTaskInline` (`:2734`), whose three arms —
  `.parse → executeParse`, `.canonicalize → executeCanonicalize`,
  `.type_check → executeTypeCheck` (`:2736`–`:2738`) — are the natural
  push/pop boundaries: each task knows its module the whole time it runs.

- **Signal handling for faults is already installed per thread.**
  `src/base/signal_handler.zig` installs process-wide SIGSEGV/SIGBUS/SIGFPE
  handlers with per-thread alternate stacks and stack bounds, dispatching to
  callbacks (`installForCurrentThread`, `:216`). `src/base/stack_overflow.zig`
  wires those callbacks and is installed on the main thread
  (`main.zig:1031`), on every coordinator worker (`coordinator.zig:4831`),
  and in the shared thread pool (`base/parallel.zig:48`). A segfault therefore
  already runs Roc-authored code before the process dies — the hook point for
  the stretch goal below.

- **The house style for contextual diagnostics is set by the interpreter.**
  `src/eval/interpreter.zig`'s `invariantFailed` (`:819`) prints a formatted
  message and asserts in Debug, `unreachable` in release — a terse, one-line,
  context-carrying failure. The crash-context block should read the same way:
  plain text, no ceremony, the facts and nothing else.

## Solution design

A small module — suggest `src/base/crash_context.zig` (visible to the
coordinator, backends, and interpreter) or `src/cli/crash_context.zig` if kept
CLI-local — that owns a **thread-local, fixed-depth, zero-allocation stack of
context frames**, plus a `pub const panic` override in `src/cli/main.zig`
(and optionally the LSP and snapshot binaries) that prints the frames.

### The frame

A frame is a small fixed-size value — no owned allocations, no slices into
freed memory:

```zig
pub const Frame = struct {
    phase: Phase,            // Parse | Canonicalize | TypeCheck | Postcheck | Codegen | Interpret
    module_path: ?[]const u8 = null, // borrowed; valid for the frame's lifetime
    module_name: ?[]const u8 = null,
    item: ?[]const u8 = null,        // def / proc identifier, when known
    extra: ?[]const u8 = null,       // optional short note, e.g. a stage name
};
```

The strings are *borrowed*, not copied: a frame is only live while the code
that pushed it is on the stack, so the module path/name it points at (owned by
`ModuleState`) outlives the frame by construction. Pushing and popping copy the
small struct into and out of a fixed array — no heap traffic, so it is
affordable even in release builds.

### The thread-local stack

```zig
const max_depth = 16; // deepest legitimate nesting is small; overflow drops the newest frame
threadlocal var frames: [max_depth]Frame = undefined;
threadlocal var depth: usize = 0;

pub fn push(frame: Frame) void { if (depth < max_depth) { frames[depth] = frame; } depth += 1; }
pub fn pop() void { depth -= 1; }
```

Using RAII-style guards keeps push/pop balanced across early returns and
errors:

```zig
pub fn enter(frame: Frame) Guard { push(frame); return .{}; }
pub const Guard = struct { pub fn leave(_: Guard) void { pop(); } };
```

Callers write `const g = crash_context.enter(.{ … }); defer g.leave();`. A
`depth` that saturates past `max_depth` is tolerated (the extra frames simply
are not stored) so a runaway recursion cannot corrupt memory; the printed
block notes when frames were dropped.

### Where frames are pushed

Push at cheap boundaries where the identity of the work is already known — one
push per unit of work, `defer`-popped:

- **Coordinator worker tasks** (`coordinator.zig` `executeTaskInline`,
  `:2734`): each arm pushes `{ .phase = .Parse/.Canonicalize/.TypeCheck,
  .module_path = mod.path, .module_name = mod.name }` for the duration of
  `executeParse` / `executeCanonicalize` / `executeTypeCheck`.
- **Post-check stage entry** — the stage sequence driven by
  `lowerCheckedModulesToLir` (`src/lir/checked_pipeline.zig:214`), whose
  `postcheck.*.run` calls fire in order (`:235`–`:273`: Monotype lower,
  MonotypeLifted lift, SpecConstr, LambdaSolved solve, SolvedLirLower):
  one push per stage (`.phase = .Postcheck, .extra = "<stage name>"`, with
  the module carried through), so a Monotype/Lambda-Solved/LIR-lowering
  crash names its stage.
- **Backend codegen per proc**: at `MonoLlvmCodeGen.compileProcBody`
  (`src/backend/llvm/MonoLlvmCodeGen.zig:1375`),
  `LirCodeGen.compileProcSpec` (`src/backend/dev/LirCodeGen.zig:14303`), and
  `WasmCodeGen.compileProcSpecBody` (`src/backend/wasm/WasmCodeGen.zig:7988`),
  push `{ .phase = .Codegen, .item = "<proc name/id>" }`.
- **Interpreter entry points** (`src/eval/interpreter.zig`): push
  `{ .phase = .Interpret, .item = "<function/expr>" }` at the top-level eval
  entry so an interpreter ICE names what it was evaluating.

Each push site is a single line plus a `defer`. None allocates.

### The panic override

In `src/cli/main.zig`, mirror the snapshot tool:

```zig
pub const panic = std.debug.FullPanic(iceHandler);

fn iceHandler(msg: []const u8, ret_addr: ?usize) noreturn {
    crash_context.printFramesForCurrentThread(msg);
    std.debug.defaultPanic(msg, ret_addr orelse @returnAddress());
}
```

`printFramesForCurrentThread` reads only the calling thread's `frames`/`depth`
(they are thread-local, so it prints exactly the panicking thread's work and
nothing from any other worker) and writes the block below to stderr before the
default handler prints the stack trace. It uses a fixed stack buffer and
direct stderr writes — no allocation, safe to run mid-panic.

### What the printed block looks like

```
────────────────────────────────────────────────────────
Roc hit an internal compiler error (this is a compiler bug).

  While: type-checking module `Json.Decode`
   File: examples/json/Json/Decode.roc
  Stage: TypeCheck

Context (innermost last, thread "coord-worker-3"):
  1. Parse         Json.Decode   examples/json/Json/Decode.roc
  2. Canonicalize  Json.Decode   examples/json/Json/Decode.roc
  3. TypeCheck     Json.Decode   examples/json/Json/Decode.roc

Reproduce with:
  roc check examples/json/Json/Decode.roc

Please report this at https://github.com/roc-lang/roc/issues with the
command above and the stack trace that follows.
────────────────────────────────────────────────────────
<the normal Zig stack trace prints here>
```

The **repro command** is derived from the innermost frame that names a module
path: a crash during any of parse / canonicalize / type-check maps to
`roc check <path>`; a crash during codegen or interpretation maps to the run
command for the in-flight module (`roc <path>` / `roc dev <path>`), falling
through to `roc check <path>` when only the checked-phase module is known. If
no frame carries a module path (a crash before any push), the block prints the
phase and thread only and omits the repro line rather than inventing one.

### Multi-threading

Context frames are thread-local, so the handler prints only the frames of the
thread that panicked — which is the thread that matters, because Zig's panic
runs on the faulting thread. Other workers' in-flight modules are irrelevant
noise and are never printed. The block names the thread (`std.Thread`'s
current name, when set for coordinator workers) so a report from a parallel
build is unambiguous about which module's work tripped the ICE.

### Signal crashes (stretch goal)

A segfault does not go through `pub const panic`; it goes through the SIGSEGV
handler in `signal_handler.zig`. The access-violation callback in
`stack_overflow.zig` (`handleAccessViolation`, `:72`) already runs
signal-safe, allocation-free code before the process exits. As a stretch goal,
have that callback also call `crash_context.printFramesForCurrentThread` (it
only reads thread-local fixed storage and writes to stderr, so it is
signal-safe) so a compiler segfault prints the same context block. This is
scoped separately because signal-safety review is stricter than the
panic-path work; the panic path is the required deliverable and the signal
path is an additive follow-on.

## Scope boundaries

- **No serialization, no telemetry.** Frames are printed to stderr and never
  written to disk, sent anywhere, or persisted.
- **No behavior change on non-crash paths.** Push/pop is a fixed-array store;
  the only observable effect is the crash-time output. Snapshot output,
  diagnostics, exit codes, and timing on success are unchanged.
- **Comment hygiene.** Suggested comments must not use the banned word that
  the CI semantic-audit gate rejects (the one starting "fall-"), and must
  describe only the current behavior — never what any code "previously" did.

## Implementation steps

Small; on the order of a few days.

1. **Add `crash_context.zig`** with `Frame`, the thread-local
   `frames`/`depth` stack, `push`/`pop`/`enter`/`Guard`, and
   `printFramesForCurrentThread(msg)` (fixed-buffer, allocation-free, stderr).
   Include the repro-command derivation from the innermost path-bearing frame.
2. **Push at the coordinator task boundaries** — the three arms of
   `executeTaskInline` (`coordinator.zig:2734`). This alone covers the most
   common ICEs (parse/canonicalize/type-check).
3. **Add the `pub const panic` override** in `src/cli/main.zig` that prints
   frames and chains to `std.debug.defaultPanic`. Optionally add the same
   override to the LSP binary and reconcile with the snapshot tool's existing
   override (it can call `printFramesForCurrentThread` before its longjmp path).
4. **Push at post-check stage entry and backend codegen per proc**
   (`MonoLlvmCodeGen.zig:1375`, dev `LirCodeGen.zig:14303`,
   `WasmCodeGen.zig:7988`), and at the interpreter's top-level eval entry.
5. **Stretch:** call `printFramesForCurrentThread` from the access-violation
   callback in `stack_overflow.zig` so segfaults print context too.
6. **Tests** (below).

## What success looks like

Every criterion below must hold; the project is not done until all do:

- A forced panic in each covered phase — parse, canonicalize, type-check,
  post-check, backend codegen, interpreter — prints a context block naming the
  correct phase and the correct module (and, where applicable, the def/proc),
  followed by the normal stack trace.
- The printed block includes a copy-pasteable repro command that, run on a
  clean checkout, re-triggers the same ICE — verified for at least the
  `roc check <module>` case.
- On a multi-threaded build, a panic on a worker thread prints only that
  thread's frames (no other worker's module appears) and names the thread.
- **Zero overhead on the non-crash path.** A full snapshot-corpus run and a
  representative `roc check`/`roc build` produce byte-identical output and no
  measurable wall-time change versus `main`. Confirm the push/pop is a plain
  fixed-array store: no allocation appears in a Debug allocator count diff
  across a compile with and without the frames pushed, and CI benchmark timings
  are within noise. (Do not benchmark locally; use the CI perf check.)
- The full CLI test suite, snapshot suite, and `zig build test` stay green,
  and the semantic-audit gate passes (no banned comment words; no comments
  referencing prior code states).
- The `pub const panic` override coexists with the existing stack-overflow
  signal handler and with the snapshot tool's own override — none is
  disabled or double-installed.

## How to evaluate the result (long-term)

### Correctness ideal

Every deterministic compiler abort — explicit `@panic`/`std.debug.panic`, and
`unreachable`/failed `assert` in Debug and ReleaseSafe — carries the phase +
module (+ item) that was in flight and a working repro command, on the exact
thread that failed. A new phase or backend added later gets context for free
once it adds one push at its work boundary; the missing-context failure mode is
a single missing `enter` call at an entry point, which is easy to review for.

### Performance ideal

None at runtime beyond a fixed-array store and a `defer` per work unit — no
allocation, no locking, no cross-thread coordination (frames are thread-local).
The crash path may do as much work as it likes since the process is dying.
Guard against regression by keeping the "zero allocation on push/pop" property
under test and re-checking CI benchmark parity when push sites are added.

### Coverage evaluation

Periodically confirm the push sites still bracket the real work: the three
coordinator task arms, each post-check stage, each backend's per-proc entry,
and the interpreter entry. A grep for new `compileProc*`/stage entry points not
wrapped in `crash_context.enter` flags gaps.

## Tests to add

- **A Debug-only child-process test per phase.** Extend the existing CLI test
  runner — `src/cli/test/parallel_cli_runner.zig` already spawns child
  compiler processes and asserts on their stderr (`stderr_exact`,
  `expected_build_stderr_contains`, and `not_contains` with
  `.{ .stream = .stderr, .text = "panic" }` / `"invariant violated"`, e.g. the
  #9588 case at `:749`). Add spec entries that compile inputs which trip a
  *controlled* ICE in each phase (behind a debug-only test hook that forces a
  panic when a sentinel module/def is seen), and assert the child's stderr
  **contains** the expected context lines — the phase name, the module name,
  and the `Reproduce with: roc check …` line — ahead of the stack trace.
- **A thread-name assertion.** One spec that forces a panic on a coordinator
  worker during a parallel build and asserts the block names a worker thread
  and shows only that module's frames.
- **A unit test for `printFramesForCurrentThread`** that pushes a known frame
  stack, captures the formatted output into a fixed buffer, and asserts the
  rendered block and the derived repro command, including the depth-overflow
  note when more than `max_depth` frames are pushed.
- **A balance test** that a `push`/`Guard.leave` pair leaves `depth` at zero
  across an early `return` and an error return.

## Related work in the repo

- `src/snapshot_tool/main.zig:36` — the working `pub const panic` precedent to
  mirror (thread-local state read in the handler, chain to the default).
- `src/base/signal_handler.zig` / `src/base/stack_overflow.zig` — the
  per-thread fault-handling hooks the segfault stretch goal extends.
- `src/eval/interpreter.zig:819` (`invariantFailed`) — the house style for a
  terse, context-carrying failure the printed block should match.
