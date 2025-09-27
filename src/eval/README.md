# Interpreter Overview

This directory contains Roc's interpreter. It is the implementation that powers
the REPL, snapshot tooling, and the evaluation tests that exercise the
type-carrying runtime. This document introduces the core pieces so a new
contributor can navigate the code without prior context.

## High-Level Architecture

- **`src/eval/interpreter.zig`** exports `Interpreter`. Each instance owns the
  runtime state needed to evaluate expressions:
  - A runtime `types.Store` where compile-time Vars are translated and unified.
  - A runtime `layout.Store` plus an O(1) `var_to_layout_slot` cache that maps
    runtime Vars to layouts.
  - A translation cache from `(ModuleEnv pointer, compile-time Var)` to runtime
    Var so we never duplicate work across expressions.
  - A polymorphic instantiation cache keyed by function id + runtime arg Vars to
    avoid repeatedly re-unifying hot polymorphic calls.
  - A small `stack.Stack` used for temporary values, a binding list that models
    lexical scopes, and helper state for closures and boolean tags.

- **`src/eval/StackValue.zig`** describes how values live in memory during
  evaluation. Each `StackValue` pairs a layout with a pointer (if any) and knows
  how to copy, move, and decref itself using the runtime layout store.

- **`src/eval/render_helpers.zig`** renders values using the same type
  information the interpreter carries. The interpreter delegates to these
  helpers for REPL output and tests.

## Evaluation Flow

1. **Canonical inputs** – Consumers (REPL, tests, snapshot tool) parse and
   canonicalize Roc source, then hand a `ModuleEnv` and canonical expression idx
   to the interpreter.
2. **Initialization** – `Interpreter.init` translates the initial module types
   into the runtime store, ensures the slot cache is sized appropriately, and
   sets up the auxiliary state (stack, binding list, poly cache).
3. **Minimal evaluation** – `evalMinimal` drives evaluation by calling
   `evalExprMinimal`. The interpreter pattern-matches on canonical expression
   tags (records, tuples, pattern matches, binops, calls, etc.), evaluates
   children recursively, and produces a `StackValue` annotated with layout.
4. **Type translation on demand** – When an expression needs type information
   (e.g. to render a value or create a layout), `translateTypeVar` copies the
   compile-time Var into the runtime store and caches the result.
5. **Layouts on demand** – `getRuntimeLayout` looks up or computes the layout
   for a runtime Var using the slot cache. Layouts are stored in the runtime
   layout store so subsequent lookups are cheap.
6. **Polymorphic calls** – Before a function call, `prepareCall` consults the
   poly cache. The interpreter only re-runs the runtime unifier if it has not
   seen that combination of function id + argument Vars before.
7. **Crash handling** – Crash/expect expressions stash a message in
   `crash_message` and set `has_crashed`. Callers can query the state via
   `hasCrashed()`/`getCrashMsg()`.

All RocOps interactions (alloc, dealloc, crash, expect) happen through the
`RocOps` pointer passed into `evalMinimal`. This keeps host integrations (REPL,
snapshot tool, CLI) consistent.

## Rendering

`renderValueRoc` and `renderValueRocWithType` assemble human-readable strings
using the same type information the interpreter evaluated with. Rendering only
reads from `StackValue` and the runtime layout store, so callers should decref
the evaluated value *after* rendering.

## Extending the Interpreter

- **New expression forms** – Add cases to `evalExprMinimal`. Most cases follow a
  pattern: translate sub-expressions, obtain or build layouts, then use the
  helpers in `StackValue` to initialize the result.
- **New data shapes** – Extend layout translation in
  `translateTypeVar`/`getRuntimeLayout` and teach `StackValue` how to copy or
  decref the shape.
- **Rendering** – Update `render_helpers.zig` and ensure the interpreter calls
  the appropriate helper.

When making changes, run `zig build test`. Interpreter-specific coverage lives
in:

- `src/eval/test/interpreter_style_test.zig` – End-to-end Roc-syntax tests that
  parse, canonicalize, evaluate, and render.
- `src/eval/test/interpreter_polymorphism_test.zig` – Scenarios that exercise
  the polymorphism cache and runtime unifier.
- `src/repl/repl_test.zig` – Integration-style tests that ensure the REPL uses
  the interpreter correctly.

## Host Integrations

- **REPL** (`src/repl/Repl.zig`) constructs a fresh interpreter per evaluation,
  feeds it a canonical expression, then renders values through the interpreter’s
  helpers.
- **Snapshot tool** (`src/snapshot_tool/main.zig`) uses the same interpreter to
  evaluate each snapshot input with optional tracing.
- **Interpreter shim** (`src/interpreter_shim/main.zig`) provides a C-callable
  entry point that deserializes a `ModuleEnv`, constructs an interpreter, and
  returns rendered output.

## Tips for Contributors

- Use the provided helpers (`StackValue.copyToPtr`, `StackValue.decref`, render
  functions) instead of manipulating raw pointers—this keeps refcounting
  correct.
- The runtime stores (`runtime_types`, `runtime_layout_store`) are owned by the
  interpreter instance. Reuse the same interpreter when evaluating multiple
  expressions inside a single host context so caches pay off.
- When debugging type translation, the `tests/interpreter_*` suites have targeted
  examples that illustrate expected behaviour.
