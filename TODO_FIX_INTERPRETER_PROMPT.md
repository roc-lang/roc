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
   - Current status: **1064 passed, 0 failed, 0 crashed, 110 skipped**

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

## Monomorphization: wrong monotype for numeric literals in specialized functions

This is the **root cause** of the `repeating pattern segfault` fx test failure,
the U8/U16 large-value arithmetic hangs (30 skipped eval tests), and likely
several other skipped eval tests involving non-Dec numeric types.

### Reproduce

Minimal reproducer (`test/fx/test_recurse_u64.roc`):
```roc
app [main!] { pf: platform "./platform/main.roc" }
import pf.Stdout

count_down = |n| match n {
    0 => "done"
    _ => count_down(n - 1)
}

main! = || {
    n : U64
    n = 3
    result = count_down(n)
    Stdout.line!(result)
}
```

```sh
zig build roc && ./zig-out/bin/roc --opt=interpreter test/fx/test_recurse_u64.roc
# Roc crashed: This Roc program overflowed its stack memory.
```

The same code with Dec (default numeric type) works correctly:
```roc
# This works — outputs "done"
result = count_down(3)
```

### Symptoms

Infinite recursion → stack overflow. The match `0 => ...` never matches because
`n - 1` subtracts 10^18 (the Dec representation of 1) instead of 1.

### Root cause: verified

The monomorphization pass (`Monomorphize.zig`) assigns the wrong monotype to
numeric literals inside polymorphic functions that are specialized for non-Dec
types.

**Verified execution trace** (from debug instrumentation):

1. `count_down` is specialized for U64. Parameter `n` correctly gets monotype U64.

2. The literal `1` in `n - 1` gets monotype **Dec** instead of U64.
   - Confirmed via debug in `lowerInt` (MirToLir.zig):
     ```
     lowerInt: mono_idx=8, target_layout=u64, value=3   ← n=3, correct
     lowerInt: mono_idx=14, target_layout=dec, value=1   ← literal 1, WRONG
     ```

3. `lowerInt` sees `target_layout=dec` for the literal `1`, so it correctly
   (from its perspective) creates a `dec_literal` with value `1 * 10^18`.

4. At runtime, `num_minus` reads both operands as U64 (8 bytes, from the first
   arg's layout). The dec_literal's 16-byte value is truncated to 8 bytes,
   yielding `1_000_000_000_000_000_000` instead of `1`.
   - Confirmed: `numBinOp sub: a_u64=3, b_u64=1000000000000000000`

5. Result: `3 - 10^18` wraps to a huge U64. The pattern `0` never matches.
   Recursion continues until `call_depth` hits 1024 → stack overflow.

### Where the wrong monotype originates

The monotype does NOT come from the `type_var_seen` path in `resolveMonotype`
(Lower.zig:3682). It comes from `lookupMonomorphizedExprMonotype` — the
**monomorphization result itself** stores Dec for this expression.

Confirmed via debug in `resolveMonotype`:
```
resolveMonotype via monomorphized: expr=10182, mono_tag=prim, prim=dec
```

The monomorphization stores expr monotypes via `recordCurrentExprMonotype`
(Monomorphize.zig:4763). For function call arguments, this happens at line 4704:
```zig
// Monomorphize.zig ~line 4701-4704
for (actual_args.items, 0..) |arg_expr_idx, i| {
    const param_mono = result.monotype_store.getIdxSpanItem(fn_mono.args, i);
    try self.bindCurrentExprTypeRoot(result, module_idx, arg_expr_idx, param_mono, proc_inst.fn_monotype_module_idx);
    try self.recordCurrentExprMonotype(result, module_idx, arg_expr_idx, param_mono, proc_inst.fn_monotype_module_idx);
}
```

The `param_mono` comes from the resolved function's (`minus`) monotype. If the
`minus` dispatch resolves to `Dec, Dec -> Dec` instead of `U64, U64 -> U64`,
then ALL arguments (including the literal `1`) get Dec monotype.

### Key code paths in the monomorphization

1. **`scanExprChildren`** (Monomorphize.zig:1920): `.e_num` is in the no-op
   case — numeric literals don't trigger any type binding during the scan phase.

2. **`exprUsesContextSensitiveNumericDefault`** (Monomorphize.zig:1772):
   Returns `true` for `.e_num`, `.e_dec`, `.e_dec_small`. This causes
   `resolveExprMonotypeIfExactResolved` to return `.none` (unresolved) for
   numeric literals, deferring their type to the call-site binding.

3. **`inferDispatchProcInst`** (Monomorphize.zig:4554): This is where binop
   dispatch (like `minus`) is resolved. It creates bindings from the actual
   argument types to the template's parameter types. If the dispatch resolves
   the wrong specialization (Dec instead of U64), all downstream monotypes
   will be wrong.

4. **`fromTypeVar`** (Monotype.zig:432): When a flex type variable with a
   numeral constraint has no binding, it defaults to Dec (line 455-456):
   ```zig
   if (hasNumeralConstraint(types_store, flex.constraints))
       return self.primIdx(.dec);
   ```

### What needs to be fixed

The monomorphization's dispatch resolution for `n - 1` inside `count_down<U64>`
must resolve `minus` as `U64, U64 -> U64`, not `Dec, Dec -> Dec`. The parameter
`n` is known to be U64 at this point, and that should propagate to the operator
dispatch and hence to the literal argument.

The fix should be in `Monomorphize.zig`, likely in how `inferDispatchProcInst`
or its callers determine the function monotype for binary operators when one
operand has a known concrete type and the other is a numeral literal.

### What NOT to do

- Do NOT fix this in `lowerInt` / `lowerDec` (MirToLir.zig) by checking the
  surrounding operation's layout. That masks the root cause.
- Do NOT fix this in the interpreter's `numBinOp` by detecting mismatched
  layouts at runtime. Same reason.
- There is currently a `lowerDec` function in MirToLir.zig (line 2736) that
  was added during investigation as defense-in-depth. It converts Dec literals
  to integers when the monotype says integer. This should be removed once
  the root cause is fixed in Monomorphize.zig, since it should never be needed.

### Tests this will fix

- **fx test**: `repeating pattern segfault (interpreter)`
- **Skipped eval tests**: U8/U16 large-value arithmetic (30 tests) — same root
  cause: numeric literals in arithmetic expressions get Dec monotype when the
  operation is specialized for U8/U16, causing 10^18 values that infinite-loop.
- Likely also: `List of typed ints` (2 tests), `U128 subtraction` (1 test),
  and potentially others involving non-Dec numeric operations.

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

## Skipped Eval Tests (SKIP_ALL — all backends)

These are tests in `src/eval/test/eval_tests.zig` that are skipped across **all**
backends (interpreter, dev, wasm, llvm). Total: **~80 tests** in 10 categories.

**Workflow**: Fix one category at a time. After fixing, unskip the tests, run them
to verify, commit, then **remove the resolved section from this document**.

---

### U8/U16 large-value arithmetic (30 tests)

Some of these hang on x86_64-linux CI (infinite loop in interpreter).

| Category | Tests |
|----------|-------|
| U8 plus  | `200+50`, `255+0`, `128+127` |
| U8 minus | `200-50`, `255-100`, `240-240` |
| U8 times | `15*17`, `128*1`, `16*15` |
| U8 div_by | `240//2`, `255//15`, `200//10` |
| U8 rem_by | `200%13`, `255%16`, `128%7` |
| U16 plus | `40000+20000`, `65535+0`, `32768+32767` |
| U16 minus | `50000-10000`, `65535-30000`, `50000-50000` |
| U16 times | `256*255`, `32768*1`, `255*256` |
| U16 div_by | `60000//3`, `65535//257`, `40000//128` |
| U16 rem_by | `50000%128`, `65535%256`, `40000%99` |

**Root cause**: Same monomorphization bug as `repeating pattern segfault`.
Numeric literals in arithmetic expressions get Dec monotype when the operation
is specialized for U8/U16. The Dec-scaled values (10^18 x n) cause arithmetic
to produce wrong results, which can infinite-loop in comparison-based operations.

---

### U128 subtraction (1 test)

- `U128: minus: 1e29 - 1e29` → expected 0

---

### Narrowing/wrapping numeric conversions (8 tests)

Crash across all backends:
- `U64 to U8 wrapping` (300→44), `U64 to I8 wrapping` (200→-56)
- `I64 to U8 wrapping` (256→0), `I64 to I8 wrapping` (300→44)
- `U32 to U8 wrapping` (300→44)
- `I128 to I8 wrapping` (300→44), `U128 to U8 wrapping` (300→44)
- Signed-to-unsigned: `I64 to U64`, `I64 to U32`, `I64 to U16`

---

### Float-to-int / float narrowing conversions (13 tests)

Crash across all backends:
- F64 → I64, I32, I16, I8, U64, U32, U16, U8
- F64 → F32
- F32 → I64, I32, U64, U32

---

### Dec-to-int / Dec-to-F32 conversions (11 tests)

Crash across all backends:
- Dec → I64, I32, I16, I8, U64, U32, U16, U8, I128, U128, F32

---

### List of typed ints (2 tests)

- `list of I32 len` — `[1.I32, 2.I32, 3.I32].len()`
- `list of U8 len` — `[10.U8, 20.U8, 30.U8].len()`

**Root cause**: Likely same monomorphization bug — typed integer literals in
list context get wrong monotype.

---

### I128/U128 shift operations (2 tests)

- `shift left I128` — `1.I128.shift_left_by(10.U8)` → 1024
- `shift left U128` — `1.U128.shift_left_by(16.U8)` → 65536

---

### Known compiler bugs (3 tests)

These are upstream compiler/specialization bugs, not interpreter-specific:
- `early return: ? in closure passed to List.fold`
- `polymorphic tag union payload substitution - extract payload`
- `polymorphic tag union payload substitution - multiple type vars`

---

## WIP: `lowerDec` in MirToLir.zig

During investigation of the monomorphization bug, a `lowerDec` function was
added at `MirToLir.zig:2736`. It converts Dec literals to the correct integer
type when the monotype says integer. The `.dec` case at line 2578 now calls
`self.lowerDec(v, mono_idx, region)` instead of directly emitting `dec_literal`.

**This is a workaround, not a fix.** Once the monomorphization root cause is
fixed, `lowerDec` should be unnecessary because the monotype will already be
correct. At that point, either:
- Remove `lowerDec` and revert to the original `self.lir_store.addExpr(.{ .dec_literal = v.num }, region)`
- Or keep it as defense-in-depth (but document it as such)

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
