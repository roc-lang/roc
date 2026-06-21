# Interpreter Overview

This directory contains Roc's interpreter. It powers the REPL, the interpreter
shim (for the default `roc` command and `roc build` in interpreter mode), and
the evaluation tests. This document introduces the core pieces so a new contributor can
navigate the code without prior context.

## High-Level Architecture

The interpreter works by consuming ARC-inserted LIR and interpreting that
program directly. In the public post-check pipeline, CIR is never given to the
interpreter or interpreter shim; the parent compiler lowers through checked
modules, post-check IRs, LIR, and ARC first.

```
checked modules → post-check IRs → LIR → TRMC/TCE → ARC → Interpret
```

### Core Modules

- **`interpreter.zig`** exports `LirInterpreter`. Each Roc call gets a frame
  whose local slots are looked up through the proc's sorted `frame_locals`
  span. Within a frame, `execStmtChain` walks the statement chain in a flat
  loop — `join`/`jump` execute as loops without growing any stack — while
  `assign_call` recurses natively into the callee (bounded by the call-depth
  cap below).

- **`value.zig`** defines `Value` — a raw pointer to bytes in memory. Values
  carry no runtime type information; the layout is always tracked separately
  via `layout.Idx`.

## Evaluation Flow

1. **Published inputs** — Consumers (REPL, tests, CLI) type check source and
   publish checked modules plus explicit roots.
2. **Lowering** — The checked-module pipeline lowers through post-check IRs
   and LIR, rewrites tail recursion via TRMC/TCE (`src/lir/trmc.zig`), and
   inserts ARC, producing a `LirStore`, committed layouts, and explicit root
   procedures.
3. **Interpretation** — `LirInterpreter.init()` creates the interpreter, then
   `eval()` (by proc id) or `runEntrypoint()` (by platform entrypoint ordinal,
   for LIR images) evaluates a root procedure.
4. **Statement walk** — `execStmtChain` executes each frame's statement chain
   iteratively, dispatching per `CFStmt` variant; low-level ops go through
   `evalLowLevel`.
5. **Crash handling** — Crash/expect expressions delegate to the host via
   `RocOps.crash`. Hosts supply a `CrashContext` (see `crash_context.zig`) to
   record messages.

All RocOps interactions (alloc, dealloc, crash, expect, dbg) happen through the
`RocOps` pointer. This keeps host integrations consistent.

## Evaluation Limits

- **Call-depth cap** — `LirInterpreter` crashes ("stack overflow") after 1024
  nested Roc calls (`max_call_depth`). Tail-recursive and
  constructor-tail-recursive functions don't hit this: the TRMC/TCE pass
  (`src/lir/trmc.zig`) rewrites them into join-point loops before the
  interpreter ever sees them.
- **Debug value validation** — In Debug builds, `setLocal` walks values to
  check they match their layouts. The walk is best-effort: it stops at
  `max_debug_value_depth` (64) nested values and after
  `max_debug_value_visits` (16) heap cells (so deep lists can't overflow the
  native stack and wide trees don't make every assignment O(structure size)),
  and inside TRMC-transformed procs a null box pointer is accepted as a legal
  not-yet-filled hole.

## Host Integrations

- **Interpreter shim** (`src/interpreter_shim/main.zig`) — Provides a
  C-callable entry point (`roc_entrypoint`) that maps/views an ARC-inserted LIR
  image and evaluates it via the interpreter.

## Tests

Evaluation coverage lives in `src/eval/test/`:

- `parallel_runner.zig` — Runs every data-driven `TestCase` on all enabled
  backends (interpreter, dev, wasm; llvm with `--llvm`) in forked
  subprocesses, requiring byte-identical `Str.inspect` output.
- `eval_tests.zig` — Aggregates the `TestCase` tables from the
  `eval_*_tests.zig` files (recursive data, closures, low-level ops,
  polymorphism, issue repros, ...). `eval_trmc_tests.zig` holds the TRMC/TCE
  stack-safety gates and the CFold/NQueens/RBTreeCk benchmark ports.
- `trmc_lir_test.zig`, `lir_inline_test.zig` — Standalone binaries asserting
  on LIR structure (TRMC pointer ops, detection/transform outcomes, inlining).
- `host_effects_runner.zig` / `host_effects_tests.zig` — Runtime host-effect
  coverage.
- `RuntimeHostEnv.zig` — Test host implementing `RocOps` with allocation
  tracking (`checkForLeaks()`) and crash capture.

Run tests with:

```bash
zig build run-test-eval                 # interpreter + dev + wasm
zig build run-test-eval -- --llvm       # also the LLVM backend
zig build run-test-zig-trmc-lir         # LIR-level TRMC tests
zig build run-test-zig-lir-inline       # LIR-level inlining tests
```

## Debugging

The interpreter supports a compile-time tracing flag that enables detailed
evaluation output. To build with tracing enabled:

```bash
zig build -Dtrace-eval=true
```

When enabled, the interpreter outputs detailed information about evaluation
steps. Both tracing flags default to `false`.

### Refcount Tracing

For debugging memory management issues, use the `-Dtrace-refcount` flag:

```bash
zig build -Dtrace-refcount=true
```

When enabled, this outputs detailed refcount operations to stderr:

```
[REFCOUNT] DECREF str ptr=0x1234 len=5 cap=32
[REFCOUNT] DECREF list ptr=0x5678 len=3 elems_rc=1 unique=1
[REFCOUNT] INCREF str ptr=0x1234 len=5 cap=32
```

Expect a large volume of output: every incref/decref is logged.
