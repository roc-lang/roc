# LIR Interpreter Bug Fix Guide

You are debugging the Roc LIR interpreter at `src/eval/interpreter.zig`.
This document lists all known outstanding bugs, how to reproduce them,
and recommendations for fixing each one.

**Important**: Fix root causes, not symptoms. Do not paper over bugs with
fallbacks or workarounds in later pipeline phases (e.g. MIR→LIR or interpreter).
If a bug originates in monomorphization, fix it there.

## Architecture Context

The LIR interpreter uses a **WorkStack + ValueStack** continuation-passing
architecture. All evaluation goes through a single stack-safe engine:

- `eval` / `evalStackSafe` — evaluate an expression
- `evalProcStackSafe` — call a proc (used by `evalEntrypoint`, sort comparator)
- Both seed the work stack then delegate to `runWorkLoop`

The main loop (`runWorkLoop`) pops work items and dispatches:

- `eval_expr` → calls `scheduleExprEval` to push sub-work
- `eval_cf_stmt` → calls `scheduleCFStmtEval` for control-flow statements
- `apply_continuation` → calls `applyContinuation` to consume values

Function calls go through `enterFunction` which pushes a `call_cleanup`
continuation, binds params, and schedules the body. The caller's work loop
processes the scheduled items — no Zig recursion.

### Test Infrastructure

There are two test paths that exercise the interpreter:

1. **Parallel eval test runner** (`zig build test-eval`):
   - Binary at `src/eval/test/parallel_runner.zig`
   - Test cases defined in `src/eval/test/eval_tests.zig` (~1174 tests)
   - Runs all backends (interpreter, dev, wasm) and compares results via
     `Str.inspect` string comparison
   - **All backend evaluation runs in forked child processes** — each backend
     call is wrapped in `forkAndEval` which forks, runs the eval function in
     the child, and pipes the result string back. Crashes in any backend are
     safely contained (the parent sees a non-zero exit or signal via waitpid).
   - The interpreter backend uses `helpers.lirInterpreterInspectedStr` which
     does CIR → MIR → LIR → RC lowering, then `LirInterpreter.eval()`
   - Current status: **1287 passed, 0 failed, 0 crashed, 16 skipped**

2. **Unit tests** (`zig build test`):
   - Sequential tests in `src/eval/test/helpers.zig` (low_level_interp_test,
     anno_only_interp_test, comptime_eval_test, etc.)
   - fx platform tests in `src/cli/test/fx_platform_test.zig`

### Key Files

- `src/eval/interpreter.zig` — LIR interpreter implementation
- `src/eval/cir_to_lir.zig` — CIR → MIR → LIR → RC lowering (`LirProgram`)
- `src/eval/value.zig` — `Value` type (raw bytes pointer) and `LayoutHelper`
- `src/eval/work_stack.zig` — WorkStack, ValueStack, continuation types
- `src/eval/test/helpers.zig` — `lirInterpreterInspectedStr`, backend eval fns
- `src/eval/test/parallel_runner.zig` — parallel test runner binary
- `src/eval/test/eval_tests.zig` — consolidated eval test definitions
- `src/mir/Monomorphize.zig` — monomorphization pass (type specialization)
- `src/mir/Lower.zig` — CIR → MIR lowering
- `src/mir/Monotype.zig` — monotype resolution from type variables
- `src/lir/MirToLir.zig` — MIR → LIR lowering (literal creation, low-level ops)
- `src/lir/TailRecursion.zig` — tail-call optimization pass
- `src/build/roc/Builtin.roc` — per-type associated items (methods like `is_eq`, `plus`, `to_str`)
- `src/build/builtin_compiler/main.zig` — maps builtin methods to low-level ops

---

## Monomorphization: wrong dispatch for numeric ops in specialized functions — FIXED (root cause)

### Summary

When a polymorphic function like `count_down = |n| n - 1` is specialized for
U64, the binop dispatch for `minus` was selecting the **Dec-specific** template
instead of the U64 one. This caused numeric literals to get Dec monotype
(value × 10^18), producing infinite recursion / wrong results.

### Root cause fix applied (Check.zig)

The root cause was `finalizeNumericDefaults` in `src/check/Check.zig` permanently
unifying generalized (polymorphic) `from_numeral` flex type variables with Dec.
This corrupted the polymorphic template so the monomorphizer couldn't create
non-Dec specializations.

**Fix**: Renamed `finalizeNumericDefaults` → `verifyNumericDefaults`. Instead of
persistently unifying from_numeral flex vars with Dec, it now creates a **copy**
of each flex var, unifies the copy with Dec (for constraint validation/error
reporting), and leaves the original polymorphic. The actual defaulting to Dec
happens during CIR → MIR lowering via `Monotype.zig:fromTypeVar` which already
had a `hasNumeralConstraint()` fallback to Dec.

An earlier monomorphizer-side workaround (guards in
`resolveAssociatedMethodProcInstForTypeVar` and
`resolveAssociatedMethodDispatchTargetForTypeVar`) was removed since the root
cause is now fixed.

### Tests fixed

- **fx test**: `repeating pattern segfault (interpreter)` ✓
- **Eval tests**: U8/U16 large-value arithmetic (30 tests unskipped) ✓
- **Eval test total**: 1287 passed (up from 1102), 0 failed, 0 crashed

---

## fx `all_syntax_test.roc` — "Called a function that could not be resolved"

### Reproduce

```sh
zig build test -- --test-filter "all_syntax_test.roc prints expected output (interpreter)"
```

### Symptoms

Actual output vs expected:
```
Hello, world!                                    ← correct
Hello, world! (using alias)                      ← correct
{ diff: 5, div: 2, ... }                         ← correct (number_operators)
{}                                                ← WRONG: should be { bool_and_keyword: False, ... }
{}                                                ← WRONG: should be "One Two"
{}                                                ← WRONG: should be "Three Four"
The color is red.                                 ← correct
{}                                                ← WRONG: should be 78
Success                                           ← correct
Line 1 / Line 2 / Line 3                         ← correct
Unicode escape sequence: [NBSP]                   ← correct
This is an effectful function!                    ← correct
Roc crashed: Called a function that could not be resolved  ← CRASH
```

Two issues:
1. **`Str.inspect` returns `{}` for many types** (Bool records, Str, U64)
2. **Crash after "This is an effectful function!"** — the next call is
   `question_postfix(["1", "not a number", "100"])` which uses the `?` operator.

### Analysis — crash

The crash message is generated in `MirToLir.zig:3806`. When a `lookup` callee
has no lambda-set resolution, a `crash` LIR expression is emitted:
```zig
if (func_mir_expr == .lookup) {
    const msg = try self.lir_store.strings.insert(self.allocator, "Called a function that could not be resolved");
    return self.lir_store.addExpr(.{ .crash = ... }, region);
}
```

This is a **MIR→LIR lowering issue**, not an interpreter bug. The function call
in `question_postfix` (which uses `.first()` and `I64.from_str()` with the `?`
try operator) doesn't get its lambda set resolved during lowering.

The `question_postfix` function:
```roc
question_postfix = |strings| {
    first_str = strings.first()?
    first_num = I64.from_str(first_str)?
    Ok(first_num)
}
```

### Analysis — Str.inspect returning `{}`

`Str.inspect` works for some record types (like `number_operators` result) but
returns `{}` for others (Bool records, bare Str values, U64). This is likely a
separate issue in how `str_inspect` is expanded during CIR→MIR lowering for
certain types.

### Debugging recommendations

1. For the crash: check why `question_postfix`'s internal function calls
   (`.first()`, `I64.from_str()`) don't get lambda set resolution. The `?`
   operator desugars to pattern matching on `[Ok, Err]`, so the issue may be
   in how the try operator's function calls are lowered.
2. For `Str.inspect` `{}`: compare the MIR output for `number_operators` (works)
   vs `boolean_operators` (broken) to find why inspection fails for Bool fields.

---

## fx `string interpolation type mismatch` — wrong output

### Reproduce

```sh
zig build test -- --test-filter "string interpolation type mismatch (interpreter)"
```

### Symptoms

Test runs `test/fx/num_method_call.roc` with `--allow-errors`:
```roc
main! = || {
    one : U8
    one = 1
    two : U8
    two = one.plus(one)
    Stdout.line!("two: ${two}")
}
```

The test expects:
- Exit code 0
- stderr contains TYPE MISMATCH and COMPTIME EVAL ERROR
- stdout contains `"two:"`

Actual: exit code 0, stderr errors are correct, but **stdout is empty**.

### Analysis

The program produces no stdout because the COMPTIME EVAL ERROR prevents the
program from running:
```
COMPTIME EVAL ERROR: Numeric literal cannot be used as this type
  (type doesn't support from_numeral)
```

This is the same root cause as the monomorphization bug above: `U8` numeric
literals don't resolve correctly. The `one = 1` definition fails comptime
evaluation because the literal `1` can't be evaluated as `U8`.

### Fix

This should be fixed by the same monomorphization fix as the `repeating pattern
segfault` bug. Once numeric literals correctly resolve to the target type (U8
in this case), the comptime evaluator should be able to evaluate `one = 1`.

---

## Skipped Eval Tests — FIXED (all SKIP_ALL removed)

All `SKIP_ALL` tests have been fixed. The 18 previously-skipped tests were broken
due to incorrect test sources (wrong method names, missing `.Dec` type suffixes),
not actual compiler/backend bugs:

- **Signed→Unsigned conversions**: used `to_u16()` instead of `to_u16_wrap()` (3 tests)
- **Float→Int wrap, Dec→Int wrap, Dec→F32 wrap**: were already working after the
  monomorphization fix; just needed unskipping (12 tests)
- **Dec literal tests**: needed `.Dec` type suffix (e.g. `3.7.Dec.to_i64_wrap()`) (6 tests)
- **`_try` variant**: already working after monomorphization fix (1 test)

Current: **16 skipped** (all are backend-specific, not SKIP_ALL).

### Tests unskipped in this round

- **31 "dev only" tests**: were skipping interpreter+wasm but now pass on all backends
  (Bool formatting, U32 ops, while loops, List ops, Str ops, polymorphic HOFs)
- **3 match regressions**: were skipping wasm+llvm, now pass on wasm too (skip llvm only)
- **1 `early return: ? in closure passed to List.fold`**: was skipping all backends,
  now passes on all backends (fixed by prior monomorphization fix)

### Remaining skips (16 total)

- 2 Str.contains (skip wasm — hangs)
- 2 abs (skip dev — dev returns wrong sign)
- 4 List.drop_at / List.sort_with (skip dev+wasm — crash on wasm, wrong result on dev)
- 1 U64→I8 wrapping (skip wasm — wasm returns unsigned 200 instead of signed -56)
- 4 I*/I32 numeric wrapping + I32→Dec conversion (skip wasm — wrong sign handling)
- 1 I32→Dec conversion (skip wasm)
- 2 known compiler bugs (type errors in test programs, skip all backends):
  - `polymorphic tag union payload substitution - extract payload`
  - `polymorphic tag union payload substitution - multiple type vars`

---

## General Debugging Tips

### Running tests

There are **two separate test systems** — use the right one:

**Eval test runner** (cross-backend comparison, 1000+ tests):
```sh
# Build and run all tests:
zig build test-eval --summary all

# Filter by name:
zig build test-eval --summary all -- --test-filter "pattern"

# Verbose output (shows PASS/SKIP):
zig build test-eval --summary all -- --test-filter "pattern" --verbose

# Single-threaded (easier to debug output):
zig build test-eval --summary all -- --test-filter "pattern" --threads 1
```

**Unit tests** (fx platform tests, sequential Zig tests):
```sh
zig build test -- --test-filter "list_append_stdin_uaf"
zig build test -- --test-filter "fx platform IO spec tests (interpreter)"
```

Note: eval runner uses `--test-filter`, unit tests use `--test-filter`.

### Process isolation in the test runner

Every backend evaluation (interpreter, dev, wasm) runs in a **forked child
process**. The child writes its result string through a pipe and exits. If
the child crashes (segfault, illegal instruction) or hangs (killed by the
30s watchdog), the parent reports the failure without being affected.

This means:
- A crash in one backend does NOT crash the test runner.
- You can safely test changes that might segfault — the runner will report
  `signal: N` for that backend and continue.
- Tests that previously "hung" the runner are now safely killed after 30s.
- `stderr` output from child processes (e.g. debug prints) appears on the
  runner's stderr, so `std.debug.print` works for debugging.

### Trace flags

Trace flags are **comptime build options** — they require a rebuild, then you
run the binary as normal:

```sh
# Build with tracing:
zig build test-eval -Dtrace-eval=true -Dtrace-refcount=true

# Run single test with tracing output (use --threads 1 to avoid interleaved output):
zig build test-eval -- --test-filter "my test" --verbose --threads 1
```

See `CONTRIBUTING/debugging_backend_bugs.md` for full details on trace output.

### Other tools

- **Hex dumps**: Set `dump_generated_code_hex = true` in `helpers.zig`
- **INT3 breakpoints**: Insert `0xCC` in `ExecutableMemory.zig` before
  `makeExecutable()` for gdb breakpoints
- **Invoke the debug-interpreter skill** (`/debug-interpreter`) for additional
  interpreter-specific debugging guidance

---

## Wasm Backend: Host Function Delegation Status

The wasm eval tests use bytebox host function imports instead of linking
`roc_builtins.o` via wasm-ld. This avoids expensive per-expression linker
invocation. Each host function marshals between wasm32 and native memory
layouts, then delegates to the shared builtin implementation.

See `src/eval/test/helpers.zig` for the implementation and
`TODO_RELOC_WASM_OBJ_BUILTIN.md` for the full wasm-ld linking plan.

### Delegating to shared builtins (correct)

These host functions call the same code as the dev/interpreter backends:

**Dec/i128 operations:**
- `hostDecMul` → `RocDec.mulWithOverflow()`
- `hostDecToStr` → `RocDec.format_to_buf()`
- `hostDecDiv` → `RocDec.div()` (via WasmRocEnv)
- `hostDecDivTrunc` → `builtins.dec.divTruncC()` (via WasmRocEnv)
- `hostDecToI128` → `builtins.dec.toIntWrap(i128, ...)`
- `hostDecToU128` → `builtins.dec.toIntWrap(u128, ...)`
- `hostDecToF32` → `builtins.dec.toF32()`
- `hostI128ToDec` → `RocDec.fromWholeInt()`
- `hostU128ToDec` → `RocDec.fromWholeInt()`
- `hostI128DivS` → `i128h.divTrunc_i128()`
- `hostI128ModS` → `i128h.rem_i128()`
- `hostU128Div` → `i128h.divTrunc_u128()`
- `hostU128Mod` → `i128h.rem_u128()`
- `hostI128ToStr` → `i128h.i128_to_str()`
- `hostU128ToStr` → `i128h.u128_to_str()`
- `hostFloatToStr` → `i128h.f64_to_str()`

**String operations (via nativeRocStr translation layer):**
- `hostStrEq` → `builtins.str.strEqual()`
- `hostStrTrim` → `builtins.str.strTrim()`
- `hostStrTrimStart` → `builtins.str.strTrimStart()`
- `hostStrTrimEnd` → `builtins.str.strTrimEnd()`
- `hostStrWithAsciiLowercased` → `builtins.str.strWithAsciiLowercased()`
- `hostStrWithAsciiUppercased` → `builtins.str.strWithAsciiUppercased()`
- `hostStrReleaseExcessCapacity` → `builtins.str.strReleaseExcessCapacity()`
- `hostStrDropPrefix` → `builtins.str.strDropPrefix()`
- `hostStrDropSuffix` → `builtins.str.strDropSuffix()`
- `hostStrConcat` → `builtins.str.strConcat()`
- `hostStrRepeat` → `builtins.str.repeatC()`
- `hostStrReserve` → `builtins.str.reserve()`
- `hostStrWithCapacity` → `builtins.str.withCapacityC()`
- `hostStrCaselessAsciiEquals` → `builtins.str.strCaselessAsciiEquals()`
- `hostStrSplit` → `builtins.str.strSplitOn()`
- `hostStrJoinWith` → `builtins.str.strJoinWith()`
- `hostStrWithPrefix` → `builtins.str.strConcat()` (prefix, str)

**Parsing (already delegating):**
- `hostIntFromStr` → `builtins.num.parseIntFromStr()`
- `hostDecFromStr` → `builtins.dec.fromStr()`
- `hostFloatFromStr` → `builtins.num.parseFloatFromStr()`

### TODO: Not yet delegating (potential divergence risk)

These host functions implement logic directly instead of calling builtins.
They may diverge from the dev/interpreter backends if the builtin logic changes.

**List operations** — require CopyFallbackFn/CompareFn callbacks that bridge
wasm↔native, and listSortWith calls back into wasm for comparisons:
- `hostListEq` — byte-wise comparison (simple, low risk)
- `hostListStrEq` — element-by-element string comparison
- `hostListListEq` — nested list comparison
- `hostListAppendUnsafe` — raw byte copy (simple, low risk)
- `hostListSortWith` — insertion sort with wasm callback (complex)
- `hostListReverse` — element reversal (no builtin exists)

**String operations:**
- `hostStrFromUtf8` — UTF-8 validation with error reporting. Already uses
  `builtins.str.numberOfNextCodepointBytes()` for error detection. The main
  validation path uses `std.unicode.utf8ValidateSlice()` which should match.

**Primitive operations** (no builtin wrapper needed):
- `hostI32ModBy` — `@mod(i32, i32)`
- `hostI64ModBy` — `@mod(i64, i64)`

### Host-specific (must stay as imports)

These bridge to the host environment and cannot be replaced by builtins:
- `hostRocAlloc`, `hostRocDealloc`, `hostRocRealloc`
- `hostRocDbg`, `hostRocExpectFailed`, `hostRocCrashed`
