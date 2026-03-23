# Migrating Eval Tests to the Parallel Runner

## Goal

Migrate all eval tests from the old per-file Zig test format into
`src/eval/test/eval_tests.zig` — the data-driven table consumed by the
parallel test runner (`zig build test-eval`).

The parallel runner exercises **every backend** (interpreter, dev, wasm,
llvm) on each test and compares results, so every migrated test
automatically gets cross-backend coverage.

## Progress

### Completed

- **eval_test.zig**: 306 test blocks migrated → 524 TestCase entries.
  62 test blocks remain (use unsupported helpers — see "Remaining Work").
- **closure_test.zig**: 53 test blocks migrated → 53 TestCase entries. File deleted.

### Remaining Work

**eval_test.zig** — 62 test blocks still use unsupported helpers:

| Helper | Count | Example tests |
|--------|-------|---------------|
| `runExpectRecord` | ~25 | `List.fold with record accumulator - *`, `focused: fold *` |
| `runExpectListI64` | ~16 | `for loop - *`, `List.map - *`, `List.append - *`, `List.repeat - *` |
| `runExpectListZst` | ~5 | `List.map - empty list`, `List.append - zst case`, `focused: list append zst` |
| `runExpectIntDec` | ~5 | `List.sum - *`, `simple fold without records - Dec result` |
| `runExpectSuccess` | ~5 | `decimal literal evaluation`, `float literal evaluation`, `string literals and interpolation` |
| `runExpectTuple` | 1 | `tuples` |
| `runExpectEmptyListI64` | 1 | `List.repeat - empty case` |
| Custom infra | 2 | `ModuleEnv serialization`, `crash message storage` |
| Manually skipped | 3 | `TODO RE-ENABLE` tests, `early return: ? in closure passed to List.fold` |

---

## Ground Rules

1. **Work in small batches.** Migrate one test file (or one logical group
   within a large file) at a time. Run `zig build test-eval -- --verbose`
   after each batch. Commit when green.

2. **Do not modify `parallel_runner.zig` or `helpers.zig`** unless you need
   to add a new `Expected` variant (see "Adding New Expected Variants"
   below). The runner and helpers are shared infrastructure.

3. **Delete old tests as you port them.** After each batch, remove the
   migrated `test "..."` blocks from the old file. If every test in a file
   has been ported, delete the file entirely and remove its `refAllDecls`
   line from `src/eval/mod.zig`. This keeps the remaining work obvious —
   whatever is left in the old files is what still needs porting.

4. **Preserve test names.** Use the old test name (the string inside
   `test "..."`) as the `.name` field. Prefix with the source file for
   disambiguation if needed (e.g. `"closure: lambda capturing one local
   variable"`).

5. **One TestCase per assertion.** The old tests sometimes have multiple
   `runExpect*` calls inside a single `test` block. Each call becomes its
   own `TestCase` entry. Append a short suffix to the name to distinguish
   them (e.g. `"eval simple number: 1"`, `"eval simple number: 42"`).

---

## Critical: Unsuffixed Numeric Literals Default to Dec, Not I64

**This is the most common migration mistake.** In Roc, unsuffixed numeric
literals like `1`, `42`, `1 + 2` evaluate to **Dec** (decimal), not I64.
Only literals with an explicit suffix like `42.I64`, `255.U8`, `3.U32`
produce integer types.

The old `runExpectI64` helper silently converted Dec values to integers,
masking the actual runtime type. **Do not replicate this behavior.** Use
the correct `Expected` variant:

```zig
// WRONG — "42" produces Dec, not I64:
.{ .name = "...", .source = "42", .expected = .{ .i64_val = 42 } },

// CORRECT — unsuffixed literal is Dec:
.{ .name = "...", .source = "42", .expected = .{ .dec_val = 42 * RocDec.one_point_zero_i128 } },

// CORRECT — suffixed literal is I64:
.{ .name = "...", .source = "42.I64", .expected = .{ .i64_val = 42 } },
```

### How to decide `.i64_val` vs `.dec_val`

Trace the **result type** of the expression. The result type is determined
by the final expression that gets returned, not just the source literals.

**Use `.dec_val = N * RocDec.one_point_zero_i128`** when the result comes from:
- Unsuffixed numeric literals: `"42"`, `"1 + 2"`, `"-5"`
- Record field access on unsuffixed values: `"{x: 42}.x"`
- Arithmetic on unsuffixed values: `"100 // 20"`, `"7 % 3"`
- Conditionals returning unsuffixed values: `"if (1 == 1) 42 else 99"`
- Match branches returning unsuffixed values: `"match Ok(10) { Ok(n) => n + 5, Err(_) => 0 }"`
- Function calls where the result chain is all unsuffixed: `"factorial(5)"`
- Hex/binary literals without suffix: `"0xFF"`, `"0b1010"`

**Use `.i64_val = N`** when the result comes from:
- Suffixed integer literals: `"42.I64"`, `"255.U8"`, `"1000.U32"`
- Arithmetic on suffixed values: `"(|x| x + 1.I64)(5.I64)"`
- `.len()` calls (returns U64, an integer type)
- `.to_i64()` conversions
- Any expression where type inference resolves to an integer type through
  suffixed literals in the call chain

**Edge cases:**
- `"(|x| x)(42)"` → Dec (42 is unsuffixed, identity doesn't change type)
- `"(|x| x)(42.I64)"` → I64 (42.I64 is suffixed)
- `"List.len([1, 2, 3])"` → I64 (len returns U64)
- `"[1.I64, 2.I64, 3.I64].len()"` → I64 (len returns U64)
- `"if True { x = 0; x } else 99"` → Dec (0 and 99 are unsuffixed)

**When in doubt:** Run the test with `.i64_val`. If it fails with
`"expected integer layout"`, the result is Dec — change to `.dec_val`.

---

## The TestCase Format

```zig
// src/eval/test/eval_tests.zig
const TestCase = @import("parallel_runner.zig").TestCase;
const RocDec = @import("builtins").dec.RocDec;

pub const tests = [_]TestCase{
    // --- integers (suffixed) ---
    .{ .name = "integer: I64 literal", .source = "42.I64", .expected = .{ .i64_val = 42 } },

    // --- decimals (unsuffixed numeric literals default to Dec) ---
    .{ .name = "eval simple number: 42", .source = "42", .expected = .{ .dec_val = 42 * RocDec.one_point_zero_i128 } },

    // --- booleans ---
    .{ .name = "bool: true literal", .source = "True", .expected = .{ .bool_val = true } },

    // --- strings ---
    .{ .name = "str: hello", .source = "\"hello\"", .expected = .{ .str_val = "hello" } },

    // --- decimals (explicit Dec suffix) ---
    .{ .name = "dec: 1.5", .source = "1.5", .expected = .{ .dec_val = 1500000000000000000 } },

    // --- floats ---
    .{ .name = "f32: literal", .source = "1.5.F32", .expected = .{ .f32_val = 1.5 } },
    .{ .name = "f64: literal", .source = "2.5.F64", .expected = .{ .f64_val = 2.5 } },

    // --- errors ---
    .{ .name = "err: crash", .source = "{ crash \"test feature\" 0 }", .expected = .{ .err_val = error.Crash } },

    // --- problems (parse/type errors expected) ---
    .{ .name = "problem: undefined variable", .source = "undefinedVar", .expected = .{ .problem = {} } },

    // --- type mismatch crash ---
    .{ .name = "type mismatch crash: ...", .source = "...", .expected = .{ .type_mismatch_crash = {} } },

    // --- dev backend only ---
    .{ .name = "dev only: ...", .source = "...", .expected = .{ .dev_only_str = "..." } },
};
```

### Skipping Backends

Use the optional `skip` field to disable specific backends for a test.
Skipped backends are excluded from cross-backend comparison. If **any**
backend is skipped, the test reports as **SKIP** rather than PASS — the
baseline goal is 100% of backends testing 100% of tests, and skip makes
it visible that a test isn't there yet.

```zig
// Skip wasm and llvm backends (e.g. known codegen bug)
.{ .name = "str: concat edge case",
   .source = "\"a\" ++ \"b\"",
   .expected = .{ .str_val = "ab" },
   .skip = .{ .wasm = true, .llvm = true },
},

// Skip all compiled backends — interpreter only
.{ .name = "interp only: complex pattern",
   .source = "...",
   .expected = .{ .dec_val = 42 * RocDec.one_point_zero_i128 },
   .skip = .{ .dev = true, .wasm = true, .llvm = true },
},
```

Available skip flags: `.interpreter`, `.dev`, `.wasm`, `.llvm`.

### Available `Expected` Variants

| Variant | Old helper | Notes |
|---------|-----------|-------|
| `.i64_val` | `runExpectI64` | i64 value. **Only for suffixed int literals** (e.g. `42.I64`). See "Critical" section above. |
| `.u8_val` | `runExpectI64` | u8 value. For `: U8` annotated expressions. |
| `.u16_val` | `runExpectI64` | u16 value. For `: U16` annotated expressions. |
| `.u32_val` | `runExpectI64` | u32 value. For `: U32` annotated expressions. |
| `.u64_val` | `runExpectI64` | u64 value. For `: U64` annotated expressions. |
| `.u128_val` | `runExpectI64` | u128 value. For `: U128` annotated expressions. |
| `.i8_val` | `runExpectI64` | i8 value. For `: I8` annotated expressions. |
| `.i16_val` | `runExpectI64` | i16 value. For `: I16` annotated expressions. |
| `.i32_val` | `runExpectI64` | i32 value. For `: I32` annotated expressions. |
| `.i128_val` | `runExpectI64` | i128 value. For `: I128` annotated expressions. |
| `.bool_val` | `runExpectBool` | `true` or `false`. |
| `.str_val` | `runExpectStr` | Expected string content. |
| `.dec_val` | `runExpectDec` | Raw i128 Dec representation (scaled by 10^18). Use `N * RocDec.one_point_zero_i128` for whole numbers. |
| `.f32_val` | `runExpectF32` | f32 with epsilon tolerance. |
| `.f64_val` | `runExpectF64` | f64 with epsilon tolerance. |
| `.err_val` | `runExpectError` | `error.Crash`, etc. |
| `.problem` | `runExpectProblem` | Expects parse/type problem. No value. |
| `.type_mismatch_crash` | `runExpectTypeMismatchAndCrash` | Expects crash from type mismatch. |
| `.dev_only_str` | `runDevOnlyExpectStr` | Str.inspect output from dev backend only. |

---

## Mapping Old Helpers → TestCase

### Direct mappings (migrate these)

```zig
// OLD (suffixed — result is I64):
try runExpectI64("(|x| x + 1.I64)(5.I64)", 6, .no_trace);
// NEW:
.{ .name = "...", .source = "(|x| x + 1.I64)(5.I64)", .expected = .{ .i64_val = 6 } },

// OLD (unsuffixed — result is Dec, NOT I64):
try runExpectI64("1 + 2", 3, .no_trace);
// NEW:
.{ .name = "...", .source = "1 + 2", .expected = .{ .dec_val = 3 * RocDec.one_point_zero_i128 } },

// OLD:
try runExpectBool("True", true, .no_trace);
// NEW:
.{ .name = "...", .source = "True", .expected = .{ .bool_val = true } },

// OLD:
try runExpectStr("\"hello\"", "hello", .no_trace);
// NEW:
.{ .name = "...", .source = "\"hello\"", .expected = .{ .str_val = "hello" } },

// OLD:
try runExpectF32("1.5.F32", 1.5, .no_trace);
// NEW:
.{ .name = "...", .source = "1.5.F32", .expected = .{ .f32_val = 1.5 } },

// OLD:
try runExpectF64("2.5.F64", 2.5, .no_trace);
// NEW:
.{ .name = "...", .source = "2.5.F64", .expected = .{ .f64_val = 2.5 } },

// OLD:
try runExpectDec("1.5", 1500000000000000000, .no_trace);
// NEW:
.{ .name = "...", .source = "1.5", .expected = .{ .dec_val = 1500000000000000000 } },

// OLD:
try runExpectError("{ crash \"boom\" 0 }", error.Crash, .no_trace);
// NEW:
.{ .name = "...", .source = "{ crash \"boom\" 0 }", .expected = .{ .err_val = error.Crash } },

// OLD:
try runExpectProblem("undefinedVar");
// NEW:
.{ .name = "...", .source = "undefinedVar", .expected = .{ .problem = {} } },

// OLD:
try runExpectTypeMismatchAndCrash("...");
// NEW:
.{ .name = "...", .source = "...", .expected = .{ .type_mismatch_crash = {} } },

// OLD:
try runDevOnlyExpectStr("...", "42");
// NEW:
.{ .name = "...", .source = "...", .expected = .{ .dev_only_str = "42" } },
```

### Multiline source strings

Old tests use Zig multiline string literals (`\\` prefix). In the test
table, use the same syntax:

```zig
// OLD:
try runExpectI64(
    \\{
    \\    x = 10.I64
    \\    y = 20.I64
    \\    x + y
    \\}
, 30, .no_trace);

// NEW (suffixed .I64 → i64_val):
.{ .name = "block: x + y",
   .source =
       \\{
       \\    x = 10.I64
       \\    y = 20.I64
       \\    x + y
       \\}
   ,
   .expected = .{ .i64_val = 30 },
},

// OLD (unsuffixed):
try runExpectI64(
    \\{
    \\    x = 10
    \\    y = 20
    \\    x + y
    \\}
, 30, .no_trace);

// NEW (unsuffixed → dec_val):
.{ .name = "block: x + y unsuffixed",
   .source =
       \\{
       \\    x = 10
       \\    y = 20
       \\    x + y
       \\}
   ,
   .expected = .{ .dec_val = 30 * RocDec.one_point_zero_i128 },
},
```

### The `.no_trace` / `.trace` parameter

The old `should_trace` parameter is dropped — the parallel runner does not
support tracing. Just ignore it when migrating.

---

## What NOT to Migrate

Some test files use custom infrastructure that doesn't fit the data-driven
table. **Skip these entirely** — they will continue running via the old
`zig build test` path or be migrated separately later.

| File | Reason |
|------|--------|
| `comptime_eval_test.zig` | Uses `ComptimeEvaluator` API, not expression eval |
| `low_level_interp_test.zig` | Module-level eval via custom `evalModuleAndGet*` |
| `interpreter_style_test.zig` | Direct `Interpreter.init` + `renderValueRoc` |
| `interpreter_polymorphism_test.zig` | Direct `Interpreter.init` + `renderValueRocWithType` |
| `anno_only_interp_test.zig` | Module-level `ComptimeEvaluator` with crash counting |
| `mono_emit_test.zig` | Tests the RocEmitter, not eval behavior |
| `stack_test.zig` | Tests the stack allocator, not eval behavior |

### Tests requiring new `Expected` variants

These old helpers have **no TestCase variant yet**. Do not migrate them
until a variant is added (see "Adding New Expected Variants" below):

| Old helper | What it checks | Remaining count in eval_test.zig |
|-----------|---------------|----------------------------------|
| `runExpectRecord` | Record with named fields + i128 values | ~25 |
| `runExpectTuple` | Tuple with indexed i128 elements | 1 |
| `runExpectListI64` | List of i64 values | ~16 |
| `runExpectListZst` | List of ZST elements (checks length only) | ~5 |
| `runExpectEmptyListI64` | Empty i64 list | 1 |
| `runExpectIntDec` | Dec value compared as truncated integer | ~5 |
| `runExpectSuccess` | Evaluation succeeds (no value check) | ~5 |
| `runExpectUnit` | Unit value `{}` | 0 |

When you encounter a test that uses one of these, **skip it** and leave a
comment in your commit message noting the count skipped and why.

### Also not migrateable

These test blocks in eval_test.zig use custom infrastructure or are
manually skipped. They cannot be expressed as TestCase entries:

| Test | Reason |
|------|--------|
| `crash message storage and retrieval - host-managed context` | Direct `TestEnv`/`RocCrashed` API |
| `ModuleEnv serialization and interpreter evaluation` | Full serialization round-trip with file I/O |
| `early return: ? in closure passed to List.fold` | Manually skipped (`return error.SkipZigTest`) |
| `TODO RE-ENABLE: ...` (2 tests) | Known compiler crash, skip-guarded |

---

## Files to Migrate (in recommended order)

### Batch 1: eval_test.zig — DONE (partially)

306 of 368 test blocks migrated. 62 remain using unsupported helpers.

### Batch 2: closure_test.zig — DONE

53 tests migrated. All used unsuffixed literals → `.dec_val` for numeric
results. File deleted.

### Batch 3: arithmetic_comprehensive_test.zig — DONE

226 test entries migrated (82 test blocks, each with multiple assertions).
Added new Expected variants (`.u8_val`, `.u16_val`, `.u32_val`, `.u64_val`,
`.u128_val`, `.i8_val`, `.i16_val`, `.i32_val`, `.i128_val`) to the
parallel runner. File deleted.

### Batch 4: list_refcount_*.zig (11 files) — DONE

105 tests migrated from 10 files (all unsuffixed → `.dec_val`).
`list_refcount_builtins.zig` was a placeholder — deleted with no tests.
All 11 files deleted.

---

## Step-by-Step Workflow

For each batch:

### 1. Read the source file

Open the old test file. Identify all `test "..."` blocks and the
`runExpect*` calls inside them.

### 2. Convert to TestCase entries

For each `runExpect*` call, create a `.{ .name = ..., .source = ...,
.expected = ... }` entry. Follow the mapping rules above.

**For `runExpectI64` calls:** Check whether the source expression produces
an integer type (suffixed literals like `.I64`, `.U8`, or `.len()` calls)
or a Dec type (unsuffixed literals). Use `.i64_val` or `.dec_val`
accordingly. See the "Critical" section above.

Skip any calls that use unsupported helpers (record, tuple, list, unit).

### 3. Append to eval_tests.zig

Add the new entries to the `tests` array in `src/eval/test/eval_tests.zig`.
Keep them grouped by source file with a comment header:

```zig
    // --- from closure_test.zig ---
    .{ .name = "closure: lambda capturing one local variable", ... },
    .{ .name = "closure: lambda capturing two local variables", ... },
```

### 4. Build and verify

```sh
zig build test-eval -- --verbose
```

All tests should pass. If any fail, check:
- **"expected integer layout"** → The result is Dec, not I64. Change to
  `.dec_val = N * RocDec.one_point_zero_i128`.
- Source string escaping (especially `\"` inside strings)
- Dec values (must be raw i128 scaled by 10^18)
- Float epsilon (f32 uses 0.0001, f64 uses 0.000000001)

### 5. Delete the old tests you just ported

Remove the migrated `test "..."` blocks from the old file. If the file is
now empty of tests, delete it and remove its `refAllDecls` line from
`src/eval/mod.zig`.

### 6. Commit

```
git add src/eval/test/eval_tests.zig src/eval/test/<old_file>.zig src/eval/mod.zig
git commit -m "Migrate <file> eval tests to parallel runner (<N> tests)"
```

### 7. Repeat

Move to the next batch.

---

## Adding New Expected Variants

When you're ready to support `runExpectRecord`, `runExpectListI64`, etc.:

1. Add a new variant to `TestCase.Expected` in `parallel_runner.zig`:
   ```zig
   list_i64: []const i64,
   ```

2. Add a handler in `runSingleTestInner` that calls a new `runTestListI64`
   function.

3. Implement `runTestListI64` following the same pattern as `runTestI64`:
   run the interpreter, check the value, then call `compareAllBackends`.

4. Add tests using the new variant to `eval_tests.zig`.

5. Run `zig build test-eval -- --verbose` to verify.

---

## Lessons Learned

### The `runExpectI64` trap

The old `runExpectI64` helper accepted both integer and Dec results by
silently converting Dec→int via `@divTrunc(dec.num, one_point_zero)`.
This masked type bugs — a test could pass with `.i64_val` even though
the expression actually produced Dec. The parallel runner's `.i64_val`
variant correctly requires an integer layout, so you must determine the
actual result type when migrating.

### Batch size

The eval_test.zig migration was done as one large batch (306 test blocks
→ 524 TestCase entries). This worked well because the conversion is
mechanical. For files with complex or unusual test patterns, smaller
batches are safer.

### Programmatic conversion

For large batches, a Python script to do the `.i64_val` → `.dec_val`
fixup was essential. After the initial migration, running the tests
identified 132 failures (all "expected integer layout"), and a script
replaced the expected variants in bulk based on the failing test names.
This is a reliable workflow: migrate optimistically, run tests, fix
failures programmatically.

### Test names from multi-assertion blocks

When a single `test "foo"` block has multiple `runExpect*` calls, each
becomes a separate TestCase. The naming convention used was:
`"foo: distinguishing suffix"` where the suffix describes the specific
case (e.g. `"eval simple number: 1"`, `"eval simple number: 42"`).

---

## Final Cleanup (after all tests are migrated)

Once every portable test is migrated and green, the old test files should
already be deleted (you deleted them as you went). Verify:

1. No old test files remain in `src/eval/test/` (except `helpers.zig`,
   `TestEnv.zig`, `parallel_runner.zig`, `eval_tests.zig`, and any
   skipped files from the "What NOT to Migrate" table).
2. `zig build test-eval` passes.
3. Commit any final cleanup.
