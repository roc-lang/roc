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
   - Current status: **1092 passed, 0 failed, 9 crashed, 80 skipped**
   - The 9 crashes are all "type mismatch" tests that crash during CIR→LIR
     lowering (before any backend runs)
   
```
$ ./zig-out/bin/eval-test-runner

=== Eval Test Results ===
  CRASH decode: I32.decode type mismatch crash  (17.2ms)
        bindPatternMonotypes(tuple): expected tuple monotype, found 'unit'
        backends: interp=not_reached dev=not_reached wasm=not_reached
  CRASH Dec + Int: plus - type mismatch  (13.8ms)
        reached unreachable code
        backends: interp=not_reached dev=not_reached wasm=not_reached
  CRASH Dec + Int: minus - type mismatch  (12.0ms)
        reached unreachable code
        backends: interp=not_reached dev=not_reached wasm=not_reached
  CRASH Dec + Int: times - type mismatch  (9.3ms)
        reached unreachable code
        backends: interp=not_reached dev=not_reached wasm=not_reached
  CRASH Dec + Int: div_by - type mismatch  (16.4ms)
        reached unreachable code
        backends: interp=not_reached dev=not_reached wasm=not_reached
  CRASH Int + Dec: plus - type mismatch  (15.6ms)
        reached unreachable code
        backends: interp=not_reached dev=not_reached wasm=not_reached
  CRASH Int + Dec: minus - type mismatch  (14.9ms)
        reached unreachable code
        backends: interp=not_reached dev=not_reached wasm=not_reached
  CRASH Int + Dec: times - type mismatch  (17.0ms)
        reached unreachable code
        backends: interp=not_reached dev=not_reached wasm=not_reached
  CRASH Int + Dec: div_by - type mismatch  (11.0ms)
        reached unreachable code
        backends: interp=not_reached dev=not_reached wasm=not_reached

=== Performance Summary (ms) ===
  Phase         Min      Max     Mean   Median   StdDev      P95    Total     N
  -------- -------- -------- -------- -------- -------- -------- --------   ---
  parse         0.1     18.0      0.9      0.3      1.8      4.9   1013.6   1168
  can           0.1     10.6      0.5      0.3      1.1      0.8    574.7   1168
  check         0.3     24.5      1.6      0.9      2.5      6.4   1899.6   1168
  interp        4.1   1271.1     26.1     14.5     53.3     82.5  27615.6   1058
  dev          10.2   1385.8     47.4     37.9     60.7     99.3  51040.2   1077
  wasm          8.7   1823.3     50.0     36.2     84.7    106.3  52215.7   1045

  Slowest 5 tests:
    1. focused: polymorphic additional specialization via List.append  (5623.1ms)  [parse:0.4 can:0.7 check:2.1 interp:1271.1 dev:1385.8 wasm:1706.8]
    2. recursive function with record - stack memory  (4663.8ms)  [parse:0.3 can:0.4 check:2.7 interp:677.0 dev:1056.1 wasm:1823.3]
    3. list fold_rev i64 dev regression  (1228.2ms)  [parse:2.6 can:0.5 check:3.3 interp:280.6 dev:309.3 wasm:351.3]
    4. closure: recursive function in let binding  (1062.0ms)  [parse:0.3 can:0.3 check:4.7 interp:223.6 dev:281.0 wasm:305.7]
    5. recursive factorial function  (1031.2ms)  [parse:0.3 can:0.4 check:1.6 interp:214.7 dev:268.1 wasm:289.8]

1088 passed, 0 failed, 9 crashed, 80 skipped (1177 total) in 10541ms using 16 thread(s)
```

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
zig build test --summary all -- --test-filter "fx platform IO spec tests (interpreter)"
```

Look for `list_append_stdin_uaf.roc` in the output.

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

Same as Bug 2 (runs in the IO spec test suite). Look for `issue8866.roc`.

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
zig build test --summary all -- --test-filter "all_syntax_test.roc prints expected output (interpreter)"
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
zig build test --summary all -- --test-filter "repeating pattern segfault (interpreter)"
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
zig build test --summary all -- --test-filter "string interpolation type mismatch (interpreter)"
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

- **Parallel runner filters**: `zig build test-eval --summary all -- --filter "pattern" --verbose`
- **Sequential test filters**: `zig build test --summary all -- --test-filter "pattern"`
- **Hex dumps**: Set `dump_generated_code_hex = true` in `helpers.zig`
- **INT3 breakpoints**: Insert `0xCC` in `ExecutableMemory.zig` before
  `makeExecutable()` for gdb breakpoints
- **Bypass fork**: Modify `helpers.zig` to skip fork for direct gdb debugging
- **Invoke the debug-interpreter skill** (`/debug-interpreter`) for additional
  interpreter-specific debugging guidance
