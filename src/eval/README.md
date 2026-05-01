# Interpreter Overview

This directory contains Roc's interpreter. It powers the REPL, the interpreter
shim (for `roc run` and `roc build` in interpreter mode), and the evaluation
tests. This document introduces the core pieces so a new contributor can
navigate the code without prior context.

## High-Level Architecture

The interpreter works by consuming ARC-inserted LIR and interpreting that
program directly. In the public post-check pipeline, CIR is never given to the
interpreter or interpreter shim; the parent compiler lowers through checked
artifacts, MIR, IR, LIR, and ARC first.

```
checked artifacts → MIR → IR → LIR → ARC → Interpret
```

### Core Modules

- **`interpreter.zig`** exports `LirInterpreter`. Each instance evaluates LIR
  expressions using a stack-safe iterative architecture with two explicit stacks:
  - A `WorkStack` of items to evaluate (expressions, control-flow statements,
    and continuations).
  - A `ValueStack` of results from completed sub-expressions.
  - A flat `ArrayList` of bindings modeling lexical scopes (push on entry, trim
    on exit — no cloning).

- **`work_stack.zig`** defines the `WorkItem` and `Continuation` types that
  drive the stack-safe eval engine. `WorkItem` has three variants: `eval_expr`,
  `eval_cf_stmt`, and `apply_continuation`. There are ~25 continuation variants
  covering function calls, aggregate construction, control flow, loops, etc.

- **`value.zig`** defines `Value` — a raw pointer to bytes in memory. Values
  carry no runtime type information; the layout is always tracked separately
  via `layout.Idx`.

## Evaluation Flow

1. **Published inputs** — Consumers (REPL, tests, CLI) type check source and
   publish checked artifacts plus explicit roots.
2. **Lowering** — The checked-artifact pipeline lowers through MIR, IR, LIR, and
   ARC, producing a `LirStore`, committed layouts, and explicit root procedures.
3. **Interpretation** — `LirInterpreter.init()` creates the interpreter, then
   `eval()` or `evalEntrypoint()` runs the stack-safe engine.
4. **Stack-safe engine** — `evalStackSafe()` is the main loop. It pops work
   items, dispatches expression evaluation, and pushes continuations + values.
   Immediates (literals, lookups) push values directly; compound expressions
   schedule continuations for post-evaluation assembly.
5. **Crash handling** — Crash/expect expressions delegate to the host via
   `RocOps.crash`. Hosts supply a `CrashContext` (see `crash_context.zig`) to
   record messages.

All RocOps interactions (alloc, dealloc, crash, expect, dbg) happen through the
`RocOps` pointer. This keeps host integrations consistent.

## Host Integrations

- **Interpreter shim** (`src/interpreter_shim/main.zig`) — Provides a
  C-callable entry point (`roc_entrypoint`) that receives a serialized
  ARC-inserted LIR runtime image via shared memory or embedded data and
  evaluates it via the interpreter.

## Tests

Interpreter-specific coverage lives in `src/eval/test/`:

- `eval_test.zig` — End-to-end tests that parse, canonicalize, lower, and
  evaluate Roc expressions.
- `arithmetic_comprehensive_test.zig` — Comprehensive numeric operation tests.
- `list_refcount_*.zig` — Reference counting tests for list operations.
- `cor_pipeline_test.zig`, `parallel_runner.zig`, `anno_only_interp_test.zig`
  — Targeted test suites for cor-style lowering, inspect-only backend parity,
  and interpreter-specific features.

Run tests with:

## Debugging

The interpreter supports a compile-time tracing flag that enables detailed
evaluation output. To build with tracing enabled:

```bash
zig build -Dtrace-eval=true
```

This flag is automatically enabled in Debug builds (`-Doptimize=Debug`). When
enabled, the interpreter outputs detailed information about evaluation steps.

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

Unlike `-Dtrace-eval`, this flag defaults to `false` even in Debug builds due to
the volume of output it produces.
