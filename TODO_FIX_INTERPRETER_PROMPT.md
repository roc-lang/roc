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

### CIR Interpreter Reference (main branch)

The CIR interpreter on `main` (`src/eval/interpreter.zig` on main, ~20k lines)
uses a similar WorkStack+ValueStack design but differs in key ways:

1. **No `call_depth` counter** — overflow is detected by checking
   `work_stack.items.len > 10_000` after every iteration.
2. **Unwinding is a tight inner drain loop** — when `early_return` or
   `break_from_loop` fires, it runs a `while` loop that pops and cleans up
   work items until hitting the boundary sentinel (`call_cleanup` or
   `for_body_done`/`while_loop_body_done`), then pushes the sentinel back.
   Normal dispatch then picks it up on the next iteration.
3. **No flag-based unwinding** — the CIR interpreter does NOT use a
   `self.unwinding` state flag checked at the top of each iteration.
   Instead, the drain happens entirely inside the continuation handler.

The LIR interpreter's flag-based unwinding (`self.unwinding` checked at loop
top) is architecturally different and was the source of a recent
double-dispatch bug (fixed: missing `continue` after clearing unwinding state).
Consider whether the LIR interpreter should adopt the CIR's inner-drain-loop
pattern for robustness.

---

## Bug 1: Segfault in `List.concat` with refcounted elements

### Reproduce

```sh
zig build test-eval --summary all -- --test-filter "List.concat with strings"
```

### Symptoms

Segfault at a stack address. The test:
```roc
x = List.concat(["hello", "world"], ["foo", "bar"])
len = List.len(x)
```
Expected: `len == 4`.

### Analysis

The segfault occurs during `List.concat` evaluation with string (refcounted)
elements. `List.concat` is a low-level builtin handled in `evalLowLevel`.
The likely issue is in how the interpreter manages memory for the concatenated
list — specifically, the refcounting of string elements during the concat
operation.

### Debugging recommendations

1. Run with the test filter and check where the segfault occurs:
   ```sh
   zig build test-eval --summary all -- --test-filter "List.concat with strings"
   ```
2. Look at `evalListConcat` in `interpreter.zig` — search for `list_concat`.
3. Check whether the shallow clone + memcpy of refcounted elements properly
   increfs the copied string pointers.
4. Compare with `List.concat` handling in the CIR interpreter on `main`.
5. The non-string version (`List.concat with nested lists`) may also fail —
   test it too.

---

## Bug 2: fx `list_append_stdin_uaf.roc` — integer overflow

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

## Bug 3: fx `issue8866.roc` — crash with opaque type containing Str

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

## Bug 4: fx `all_syntax_test.roc` — "Called a function that could not be resolved"

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

## Bug 5: fx `repeating pattern segfault` — stack overflow

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
2. Check if the CIR interpreter on main passes this test — it may require
   a higher call depth limit.
3. If the program is tail-recursive, check whether the LIR lowering produces
   join points that the interpreter handles correctly (jump → body re-execution).

---

## Bug 6: fx `string interpolation type mismatch` — wrong output

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

## Bug 7 (flaky): Segfault in `matchPattern` during `cf_match_dispatch`

### Reproduce

```sh
zig build test-eval --summary all -- --test-filter "string list aliased return from original"
```

This is **flaky** — sometimes passes, sometimes segfaults.

### Symptoms

Segfault at address `0x10` (null-ish pointer) in `allocRocDataWithRc`,
called from `matchPattern` in the `cf_match_dispatch` continuation.

The test:
```roc
{
    lst1 = ["a", "b"]
    _lst2 = lst1
    match lst1 { [first, ..] => first, _ => "" }
}
```

### Analysis

The flakiness + address 0x10 strongly suggests a **use-after-free**. The
list `lst1` is aliased to `_lst2`. If `_lst2` is immediately decreffed
(unused binding with `_` prefix), the list's refcount drops to 0 and its
memory is freed. Then `match lst1` tries to access the freed memory.

This is a **refcount insertion bug**, not an interpreter eval bug. The RC
insertion pass (`src/lir/rc_insert.zig`) needs to keep the list alive
through the match expression.

### Debugging recommendations

1. Check `rc_insert.zig` — specifically `emitBlockIncrefsForPattern` and
   `emitBlockDecrefsForPattern`.
2. When a symbol is both consumed (by `_lst2 = lst1`) AND borrowed later
   (by `match lst1`), the incref count formula `consumed_uses - 1` may
   not account for the borrow.
3. Compare with how the CIR interpreter on `main` handles this — the CIR
   pass uses `patternMatchesBind` with a temp bindings list, but the
   refcounting decision happens earlier in the pipeline.

---

## General Debugging Tips

- **Hex dumps**: Set `dump_generated_code_hex = true` in `helpers.zig`
- **INT3 breakpoints**: Insert `0xCC` in `ExecutableMemory.zig` before
  `makeExecutable()` for gdb breakpoints
- **Bypass fork**: Modify `helpers.zig` to skip fork for direct gdb debugging
- **Test filters**: `zig build test-eval --summary all -- --test-filter "pattern"`
- **Invoke the debug-interpreter skill** (`/debug-interpreter`) for additional
  interpreter-specific debugging guidance

### Last results from `zig build minici`

```
$ zig build minici
---- minici: running `zig build fmt` ----
---- minici: running zig lints ----
Checking for separator comments...
Checking for pub declarations without doc comments...
Checking for top level comments in new Zig files...
[OK] All lints passed!
---- minici: running tidy checks ----
[OK] All tidy checks passed!
---- minici: checking test wiring ----
Checking test wiring in src/ directory...
└─ minici-inner
Step 1: Finding all potential test files...
Found 190 potential test files

Step 2: Extracting test references from mod.zig files...
Found 322 file references in mod.zig files and build.zig test roots

Step 3: Checking if all test files are properly wired...

[OK] All tests are properly wired!

---- minici: running `zig build` ----
Roc cache not found (nothing to clear)
Roc cache not found (nothing to clear)
---- minici: checking Builtin.roc formatting ----
All formatting valid.
Took 18.7 ms.
---- minici: running `zig build snapshot` ----
Build succeeded!
---- minici: checking for snapshot changes ----
---- checking fx platform test coverage ----
All 102 .roc files in test/fx/ have tests.
---- minici: running `zig build test` ----
Roc cache not found (nothing to clear)
Build succeeded!
test
└─ tests_summary
   └─ run test lsp stderr
===== DOC COMMENTS TEST=====
=== HOVER TEXT ===
Multiplies two numbers.

```roc
I64, I64 -> I64
```
=== END ===
test
└─ tests_summary
   └─ run test eval failure
Segmentation fault at address 0x7fffa0fc65c0
???:?:?: 0x7fffa0fc65c0 in ??? (???)
Unwind information for `???:0x7fffa0fc65c0` was not available, trace may be incomplete


error: while executing test 'test.low_level_interp_test.test.low_level - List.concat with strings (refcounted elements)', the following command terminated with signal 6 (expected exited with code 0):
./.zig-cache/o/6ac14057e490ecc6302c2b8aa4ea317f/eval --cache-dir=./.zig-cache --seed=0x23f8f2d2 --listen=-
test
└─ tests_summary
   └─ run test fx_platform_test 50/60 passed, 4 failed, 6 skipped
error: 'fx_platform_test.test.fx platform IO spec tests (interpreter)' failed: Test failed with exit code 134
STDERR:
=== PANIC (no stack trace) ===
integer overflow at address 0x337b85


[FAIL] test/fx/list_append_stdin_uaf.roc (--opt=interpreter): error.TestFailed
       Description: Regression test: List.append with effectful call on big string (24+ chars)
Test failed with exit code 134
STDERR:
This Roc application overflowed its stack memory and crashed.



[FAIL] test/fx/issue8866.roc (--opt=interpreter): error.TestFailed
       Description: Regression test: List.append with opaque type containing Str (issue #8866)

65/67 IO spec tests passed (2 failed) [opt=--opt=interpreter]
/home/lbw/Documents/Github/roc/src/cli/test/fx_platform_test.zig:256:9: 0x11271b1 in runIoSpecTests__anon_18436 (fx_platform_test.zig)
        return error.SomeTestsFailed;
        ^
/home/lbw/Documents/Github/roc/src/cli/test/fx_platform_test.zig:261:5: 0x1127596 in test.fx platform IO spec tests (interpreter) (fx_platform_test.zig)
    try runIoSpecTests("--opt=interpreter");
    ^
error: 'fx_platform_test.test.fx platform all_syntax_test.roc prints expected output (interpreter)' failed: Run failed with exit code 1
STDOUT: Hello, world!
Hello, world! (using alias)
{ diff: 5, div: 2, div_trunc: 2, eq: False, gt: True, gteq: True, lt: False, lteq: False, neg: -10, neq: True, prod: 50, rem: 0, sum: 15 }
{}
{}
{}
The color is red.
{}
Success
Line 1
Line 2
Line 3
Unicode escape sequence:  
This is an effectful function!

STDERR:
Roc crashed: Called a function that could not be resolved

/home/lbw/Documents/Github/roc/src/cli/test/util.zig:172:17: 0x1170007 in checkSuccess (fx_platform_test.zig)
                return error.RunFailed;
                ^
/home/lbw/Documents/Github/roc/src/cli/test/fx_platform_test.zig:300:5: 0x1175eb3 in test.fx platform all_syntax_test.roc prints expected output (interpreter) (fx_platform_test.zig)
    try util.checkSuccess(run_result);
    ^
error: 'fx_platform_test.test.fx platform string interpolation type mismatch (interpreter)' failed: /home/lbw/bin/zig-x86_64-linux-0.15.2/lib/std/testing.zig:607:14: 0x1177599 in expect (std.zig)
    if (!ok) return error.TestUnexpectedResult;
             ^
/home/lbw/Documents/Github/roc/src/cli/test/fx_platform_test.zig:648:5: 0x117faf4 in test.fx platform string interpolation type mismatch (interpreter) (fx_platform_test.zig)
    try testing.expect(std.mem.indexOf(u8, run_result.stdout, "two:") != null);
    ^
error: 'fx_platform_test.test.fx platform repeating pattern segfault (interpreter)' failed: Run failed with exit code 1
STDOUT: 11-22

STDERR:
Roc crashed: This Roc program overflowed its stack memory. This usually means there is very deep or infinite recursion somewhere in the code.

/home/lbw/Documents/Github/roc/src/cli/test/util.zig:172:17: 0x1170007 in checkSuccess (fx_platform_test.zig)
                return error.RunFailed;
                ^
/home/lbw/Documents/Github/roc/src/cli/test/fx_platform_test.zig:1083:5: 0x118d643 in test.fx platform repeating pattern segfault (interpreter) (fx_platform_test.zig)
    try util.checkSuccess(run_result);
    ^
error: while executing test 'fx_test_specs.test.find by path works', the following test command failed:
./.zig-cache/o/556d3fdbf9408d62bc090783b84c8337/fx_platform_test --cache-dir=./.zig-cache --seed=0x23f8f2d2 --listen=-

Build Summary: 108/112 steps succeeded; 2 failed; 3356/3370 tests passed; 10 skipped; 4 failed
test transitive failure
└─ tests_summary transitive failure
   ├─ run test eval failure
   └─ run test fx_platform_test 50/60 passed, 4 failed, 6 skipped

error: the following build command failed with exit code 1:
.zig-cache/o/f209260fd558ab083c7e83299cd7cdbf/build /home/lbw/bin/zig-x86_64-linux-0.15.2/zig /home/lbw/bin/zig-x86_64-linux-0.15.2/lib /home/lbw/Documents/Github/roc .zig-cache /home/lbw/.cache/zig --seed 0x23f8f2d2 -Z97f39e5ba03ee647 test
minici
└─ minici-inner failure
error: `zig build test` failed with exit code 1

Build Summary: 0/2 steps succeeded; 1 failed
minici transitive failure
└─ minici-inner failure

error: the following build command failed with exit code 1:
.zig-cache/o/f209260fd558ab083c7e83299cd7cdbf/build /home/lbw/bin/zig-x86_64-linux-0.15.2/zig /home/lbw/bin/zig-x86_64-linux-0.15.2/lib /home/lbw/Documents/Github/roc .zig-cache /home/lbw/.cache/zig --seed 0xd2b5aa5b -Zc9c1e73b6e06bdb7 minici
```
