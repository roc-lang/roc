# LIR Interpreter Bug Fix Guide

You are debugging the Roc LIR interpreter at `src/eval/interpreter.zig`.
This document lists all known outstanding bugs, how to reproduce them,
and recommendations for fixing each one.

## Architecture Context

The LIR interpreter uses a **WorkStack + ValueStack** continuation-passing
architecture. The main eval loop is `evalStackSafe` (~line 3685). Each
iteration pops a work item and either:

- `eval_expr` → calls `scheduleExprEval` to push sub-work
- `eval_cf_stmt` → calls `scheduleCFStmtEval` for control-flow statements
- `apply_continuation` → calls `applyContinuation` to consume values

Function calls go through `enterFunction` which increments `call_depth`,
pushes a `call_cleanup` continuation, binds params, and schedules the body.

There are also **two legacy recursive paths** that bypass the stack-safe engine:
- `callProcSpec` → `evalCFStmt`: used by `evalEntrypoint` and sort comparators
- These manage `call_depth` via `defer` and call `eval()` for sub-expressions,
  which re-enters `evalStackSafe`

### Test Infrastructure

There are two test paths that exercise the interpreter:

1. **Parallel eval test runner** (`zig build test-eval`):
   - Binary at `src/eval/test/parallel_runner.zig`
   - Test cases defined in `src/eval/test/eval_tests.zig` (~1177 tests)
   - Runs all backends (interpreter via LIR pipeline, dev, wasm) and compares
     results via `Str.inspect` string comparison
   - Crash protection via `setjmp`/`longjmp` + signal handlers
   - The interpreter backend uses `helpers.lirInterpreterInspectedStr` which
     does CIR → MIR → LIR → RC lowering, then `LirInterpreter.eval()`
   - For typed value tests, also uses `helpers.lirInterpreterEval` to check
     raw values (int, float, str, bool, dec) against expected
   - Current status: **1101 passed, 0 failed, 0 crashed, 80 skipped**

2. **Unit tests** (`zig build test`):
   - Sequential tests in `src/eval/test/helpers.zig` (low_level_interp_test,
     anno_only_interp_test, comptime_eval_test, etc.)
   - fx platform tests in `src/cli/test/fx_platform_test.zig`

### Key Files

- `src/eval/interpreter.zig` — LIR interpreter implementation
- `src/eval/cir_to_lir.zig` — CIR → MIR → LIR → RC lowering (`LirProgram`)
- `src/eval/value.zig` — `Value` type (raw bytes pointer) and `LayoutHelper`
- `src/eval/work_stack.zig` — WorkStack, ValueStack, continuation types
- `src/eval/test/helpers.zig` — `lirInterpreterEval`, `lirInterpreterInspectedStr`
- `src/eval/test/parallel_runner.zig` — parallel test runner binary
- `src/eval/test/eval_tests.zig` — consolidated eval test definitions

---

## fx `list_append_stdin_uaf.roc` — integer overflow

### Reproduce

```sh
# This test runs inside the IO spec test suite (a single Zig test that loops
# over all .roc files). Run the suite and look for the file in output:
zig build test -- --test-filter "fx platform IO spec tests (interpreter)"
```

### Symptoms

Integer overflow panic (signal 6, exit code 134). The test:
```roc
main! = || {
    lines = [].append(Stdin.line!())
    List.for_each!(lines, |line| Stdout.line!(str(line)))
}
```

### Analysis

The integer overflow is at an address in generated/interpreter code, not at
`call_depth -= 1` (that bug was fixed). This suggests an arithmetic overflow
in a different part of the interpreter — possibly in list capacity/length
calculations or in the effectful function dispatch path.

The test involves:
1. An effectful call (`Stdin.line!()`) producing a string
2. `List.append` on an empty list with that string
3. Iterating with `for_each!` and a closure

### Debugging recommendations

1. The `[].append(value)` pattern creates a list of capacity 1. Check if
   `evalListAppend` or the underlying `roc_builtins.list.append` handles
   the empty-list-to-singleton case correctly.
2. Check whether the effectful call result (from `Stdin.line!()`) returns
   a properly-sized value — size mismatch could cause overflow in memcpy
   length calculations.
3. The comment in the test says `[Stdin.line!()]` works but
   `[].append(Stdin.line!())` doesn't — this points to a difference in how
   list-literal vs append paths handle refcounted elements.

---

## fx `issue8866.roc` — crash with opaque type containing Str

### Reproduce

```sh
# Runs inside the IO spec test suite — look for issue8866.roc in output:
zig build test -- --test-filter "fx platform IO spec tests (interpreter)"
```

### Symptoms

Exit code 134 (crash). The test:
```roc
MyRecord := { name : Str }.{}

main! = || {
    result_init : List(MyRecord)
    result_init = []
    var $result = result_init
    $result = List.append($result, { name: "first" })
    $result = List.append($result, { name: "second" })
    Stdout.line!("Done: ${List.len($result).to_str()}")
}
```

### Analysis

This involves `List.append` on a list of opaque types that contain strings.
The opaque type `MyRecord` wraps `{ name : Str }`. The crash likely occurs
because:
1. The opaque type's layout size/alignment is miscalculated, OR
2. The `List.append` path doesn't properly handle the indirection of opaque
   types when copying/increfing elements, OR
3. The mutable variable `$result` reassignment doesn't properly decref the
   old list before replacing it.

### Debugging recommendations

1. Check layout resolution for opaque types wrapping structs with strings.
2. Look at how `cell_store` (mutable variable update) handles the old value —
   does it decref before overwriting?
3. Try a simpler reproduction: `List.append([], { name: "hello" })` in the
   eval test suite to isolate whether it's the opaque wrapper or the mutation.

---

## fx `all_syntax_test.roc` — "Called a function that could not be resolved"

### Reproduce

```sh
zig build test -- --test-filter "all_syntax_test.roc prints expected output (interpreter)"
```

### Symptoms

Most output is correct, then: `Roc crashed: Called a function that could not be resolved`

### Analysis

The interpreter can evaluate most of the syntax test but fails on a specific
function call. This typically means a `proc_call` references a `ProcSpec`
that the interpreter can't find — either the proc wasn't lowered, the
specialization ID is wrong, or it's a higher-order function passed as a
value that the interpreter doesn't resolve correctly.

### Debugging recommendations

1. Run the test and check which line of output is last before the crash to
   narrow down which expression fails.
2. Search for "could not be resolved" in `interpreter.zig` to find where
   this error message is generated.
3. Check whether the failing function is a closure, a module function, or
   a platform-provided function.

---

## fx `repeating pattern segfault` — stack overflow

### Reproduce

```sh
zig build test -- --test-filter "repeating pattern segfault (interpreter)"
```

### Symptoms

```
Roc crashed: This Roc program overflowed its stack memory.
```
Output before crash: `11-22` (partial expected output).

### Analysis

This is a recursion or pattern-matching test that triggers stack overflow.
Since the LIR interpreter uses `call_depth` with a max of 1024, this either:
1. Legitimately exceeds the recursion limit (may need higher limit), OR
2. Has infinite recursion due to incorrect pattern matching or join point handling.

### Debugging recommendations

1. Read the test file `test/fx/repeating_pattern_segfault.roc` (or similar
   name — glob for it) to understand what it does.
2. If the program is tail-recursive, check whether the LIR lowering produces
   join points that the interpreter handles correctly (jump → body re-execution).

---

## fx `string interpolation type mismatch` — wrong output

### Reproduce

```sh
zig build test -- --test-filter "string interpolation type mismatch (interpreter)"
```

### Symptoms

Test expects output containing `"two:"` but it's missing from stdout.

### Analysis

String interpolation compiles to `str_concat` with parts that include
`int_to_str`, `float_to_str`, etc. The "type mismatch" aspect suggests
the interpolation of a non-string value (like a number or custom type)
doesn't produce the expected string.

### Debugging recommendations

1. Read the test's `.roc` file to see what interpolation expression is used.
2. Check `evalStrConcat` and the `str_concat_collect` continuation.
3. Check `int_to_str` / `float_to_str` / `dec_to_str` handlers.

---

## General Debugging Tips

### Running tests

There are **two separate test systems** — use the right one:

**Eval test runner** (cross-backend comparison, 1000+ tests):
```sh
# Build once (or after source changes):
zig build test-eval

# Run a single test by name:
./zig-out/bin/eval-test-runner --filter "pattern" --verbose

# Or build + run combined (options go after --):
zig build test-eval -- --filter "pattern" --verbose
```

**Unit tests** (fx platform tests, sequential Zig tests):
```sh
zig build test -- --test-filter "list_append_stdin_uaf"
zig build test -- --test-filter "fx platform IO spec tests (interpreter)"
```

Note: eval runner uses `--filter`, unit tests use `--test-filter`.

### Trace flags

Trace flags are **comptime build options** — they require a rebuild, then you
run the binary as normal:

```sh
# Build with tracing:
zig build test-eval -Dtrace-eval=true -Dtrace-refcount=true

# Run single test with tracing output:
./zig-out/bin/eval-test-runner --filter "my test" --verbose --threads 1
```

See `CONTRIBUTING/debugging_backend_bugs.md` for full details on trace output.

### Other tools

- **Hex dumps**: Set `dump_generated_code_hex = true` in `helpers.zig`
- **INT3 breakpoints**: Insert `0xCC` in `ExecutableMemory.zig` before
  `makeExecutable()` for gdb breakpoints
- **Bypass fork**: Modify `helpers.zig` to skip fork for direct gdb debugging
- **Invoke the debug-interpreter skill** (`/debug-interpreter`) for additional
  interpreter-specific debugging guidance
