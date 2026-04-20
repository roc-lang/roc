# Debugging Backend Bugs (Interpreter / Dev / WASM)

This guide walks through the workflow for reproducing, tracing, and fixing
bugs that surface in the eval backends — the LIR interpreter, dev (native)
code generator, or WASM code generator.

## Overview

The eval test runner (`zig-out/bin/eval-test-runner`) exercises all the
backends on many test cases 1000+. Each test is parsed, canonicalized, type-checked,
lowered through a shared pipeline (CIR → LIR → RC insertion), and then
executed by each backend independently. Results are compared via `Str.inspect`.

When a backend crashes or produces the wrong answer, the workflow is:

1. Add a minimal test case that reproduces the bug
2. Build with trace flags to see what the pipeline is doing
3. Read the trace output to find the failure point
4. Fix the bug
5. Run the full suite to check for regressions

## Two test systems

There are **two separate test systems** — don't mix them up:

| System | Build command | How to filter | What it tests |
|--------|--------------|---------------|---------------|
| **Eval test runner** | `zig build test-eval` | `--filter "pattern"` | Cross-backend comparison (interp, dev, wasm) via `Str.inspect` |
| **Unit tests** | `zig build test` | `--test-filter "pattern"` | Sequential Zig tests (`helpers.zig`, `fx_platform_test.zig`, etc.) |

The eval test runner is a standalone binary. You build it once, then run it
directly — there's no need to rebuild between runs unless you change source.

## 1. Add a reproducing test case

Test cases live in `src/eval/test/eval_tests.zig`. Add a new entry to the
`tests` array:

```zig
.{ .name = "List.concat with strings", .source = "List.concat([\"hello\", \"world\"], [\"foo\", \"bar\"]).len()", .expected = .{ .i64_val = 4 } },
```

Key fields:
- **`name`** — descriptive name, used by `--filter`
- **`source`** — a Roc expression (single expression, not a module)
- **`expected`** — one of:
  - `.i64_val`, `.u64_val`, `.f64_val`, `.bool_val`, `.dec_val` — typed value check (interpreter only) + cross-backend `Str.inspect` comparison
  - `.str_val` — string value check
  - `.inspect_str` — only compare `Str.inspect` output across backends
- **`skip`** — optionally skip specific backends: `.skip = .{ .wasm = true }`

Rebuild the test runner after adding your test:

```sh
zig build test-eval
```

## 2. Run the failing test

**Build once, then run the binary directly** — this is much faster than
rebuilding via `zig build test-eval` each time:

```sh
# Build (only needed once, or after source changes):
zig build test-eval

# Run a single test by name:
./zig-out/bin/eval-test-runner --filter "List.concat with strings" --verbose

# Or combine build + run in one command (passes options after --):
zig build test-eval -- --filter "List.concat with strings" --verbose
```

The output tells you the outcome and which backends were reached:

```
CRASH List.concat with strings  (21.5ms)
      attempt to use null values
      backends: interp=not_reached dev=not_reached wasm=not_reached
```

- **`not_reached`** for all backends means the crash is in the shared lowering
  pipeline or in the first backend (interpreter) before cross-backend comparison.
- **`interp=22ms dev=not_reached`** means the interpreter succeeded but the
  crash is in the dev backend.

Use `--threads 1` for deterministic sequential output when debugging.

### Unit tests (fx platform tests, etc.)

For tests in the Zig unit test system (not the eval runner), use `--test-filter`:

```sh
# Run a specific fx platform test:
zig build test -- --test-filter "list_append_stdin_uaf"

# Run all fx interpreter tests:
zig build test -- --test-filter "fx platform IO spec tests (interpreter)"
```

Note the different flag: `--test-filter` (not `--filter`).

## 3. Build with trace flags

There are two independent comptime trace flags. They are compiled out when
disabled, so normal builds have zero overhead.

**Important**: Trace flags require a rebuild — they are comptime options passed
to `zig build`, not runtime flags. After rebuilding with trace flags, you run
the binary as normal.

### `-Dtrace-eval=true` — Lowering + interpreter eval tracing

Traces the full pipeline:
- Lowering stages: canonical lowering → LIR → RC insertion
- Interpreter eval loop: every work item dispatched (expression, continuation, low-level op)
- RC plan execution in the interpreter

```sh
# Build with tracing enabled:
zig build test-eval -Dtrace-eval=true

# Then run your specific test:
./zig-out/bin/eval-test-runner --filter "my test" --verbose --threads 1
```

Example output:
```
[lower] === Monomorphize ===
[lower] monomorphize done: 2 proc instances
[lower] === LIR lowering ===
[lower] LIR done: lir_expr=@enumFromInt(29)
[interp] eval_expr @enumFromInt(18): low_level
[interp] list_concat: elem_width=24 align=8 rc=true
```

### `-Dtrace-refcount=true` — Memory + refcount tracing

Traces every allocation, deallocation, reallocation, and refcount operation:

```sh
zig build test-eval -Dtrace-refcount=true
./zig-out/bin/eval-test-runner --filter "my test" --verbose --threads 1
```

Example output:
```
[rc] alloc: ptr=0x7f3e07030 size=64 align=8 buf_offset=64
[rc] realloc: old=0x7f3e07040 new=0x7f3e070b0 old_size=64 new_size=112 align=8
[rc] list_decref: bytes=0x7f3e070b8 len=4 cap=4 alloc_ptr=0x7f3e070b8 has_child=true elem_align=8
[rc] str_incref: bytes=0x6f6c6c6568 len=0 cap=... count=1
```

This is invaluable for catching:
- Mismatched allocation headers (e.g. `elements_refcounted` mismatch between alloc and realloc)
- Use-after-free or double-free
- `old_size=0` in realloc (the allocation lookup failed)
- Null pointer dereferences in decref

### Combining both flags

```sh
zig build test-eval -Dtrace-eval=true -Dtrace-refcount=true
./zig-out/bin/eval-test-runner --filter "my test" --verbose --threads 1
```

## 4. Reading the trace output

### Identifying crash location

The last trace line before `CRASH` tells you where things went wrong.
For example:

```
[interp] performRcPlan: plan=list_decref val.ptr=u8@...
CRASH ...
```

This means the crash happened inside `list_decref` in `performRcPlan`.
