# Migrating Eval Tests to the Parallel Runner

## Goal

Migrate all eval tests from the old per-file Zig test format into
`src/eval/test/eval_tests.zig` — the data-driven table consumed by the
parallel test runner (`zig build test-eval`).

The parallel runner exercises **every backend** (interpreter, dev, wasm,
llvm) on each test and compares results, so every migrated test
automatically gets cross-backend coverage.

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

## The TestCase Format

```zig
// src/eval/test/eval_tests.zig
const TestCase = @import("parallel_runner.zig").TestCase;
const RocDec = @import("builtins").dec.RocDec;

pub const tests = [_]TestCase{
    // --- integers ---
    .{ .name = "eval simple number: 1", .source = "1", .expected = .{ .i64_val = 1 } },

    // --- booleans ---
    .{ .name = "bool: true literal", .source = "True", .expected = .{ .bool_val = true } },

    // --- strings ---
    .{ .name = "str: hello", .source = "\"hello\"", .expected = .{ .str_val = "hello" } },

    // --- decimals ---
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
   .expected = .{ .i64_val = 42 },
   .skip = .{ .dev = true, .wasm = true, .llvm = true },
},
```

Available skip flags: `.interpreter`, `.dev`, `.wasm`, `.llvm`.

### Available `Expected` Variants

| Variant | Old helper | Notes |
|---------|-----------|-------|
| `.i64_val` | `runExpectI64` | i64 value. Only for suffixed int literals (e.g. `42.I64`). Unsuffixed literals default to Dec — use `.dec_val` with `N * RocDec.one_point_zero_i128` instead. |
| `.bool_val` | `runExpectBool` | `true` or `false`. |
| `.str_val` | `runExpectStr` | Expected string content. |
| `.dec_val` | `runExpectDec` | Raw i128 Dec representation (scaled by 10^18). |
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
// OLD:
try runExpectI64("1 + 2", 3, .no_trace);
// NEW:
.{ .name = "...", .source = "1 + 2", .expected = .{ .i64_val = 3 } },

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
    \\    x = 10
    \\    y = 20
    \\    x + y
    \\}
, 30, .no_trace);

// NEW:
.{ .name = "block: x + y",
   .source =
       \\{
       \\    x = 10
       \\    y = 20
       \\    x + y
       \\}
   ,
   .expected = .{ .i64_val = 30 },
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

| Old helper | What it checks |
|-----------|---------------|
| `runExpectRecord` | Record with named fields + i128 values |
| `runExpectTuple` | Tuple with indexed i128 elements |
| `runExpectListI64` | List of i64 values |
| `runExpectListZst` | List of ZST elements (checks length only) |
| `runExpectEmptyListI64` | Empty i64 list |
| `runExpectUnit` | Unit value `{}` |

When you encounter a test that uses one of these, **skip it** and leave a
comment in your commit message noting the count skipped and why.

---

## Files to Migrate (in recommended order)

Migrate these files. Each contains tests that use `runExpectI64`,
`runExpectBool`, `runExpectStr`, `runExpectF32`, `runExpectF64`,
`runExpectDec`, `runExpectError`, `runExpectProblem`,
`runExpectTypeMismatchAndCrash`, or `runDevOnlyExpectStr`.

### Batch 1: eval_test.zig (the big one — do in sub-batches)

~371 tests. Work through it in groups of ~30-50 tests at a time. Suggested
sub-batches based on the test names / logical sections:

1. Simple numbers, if-else, nested if-else, records (field access)
2. Arithmetic, comparisons, boolean logic
3. Let bindings, closures, function application
4. String operations
5. Dec / float operations
6. Pattern matching (when/match)
7. Tags and tag unions
8. Remaining `runExpectI64` / `runExpectBool` tests
9. `runExpectStr` tests
10. `runExpectError`, `runExpectProblem`, `runExpectTypeMismatchAndCrash`
11. `runDevOnlyExpectStr` tests
12. **Skip** `runExpectRecord`, `runExpectTuple`, `runExpectListI64`,
    `runExpectListZst`, `runExpectEmptyListI64` tests (no variant yet)

### Batch 2: closure_test.zig

~53 tests. All use `runExpectI64` or `runExpectStr` — fully portable.

### Batch 3: arithmetic_comprehensive_test.zig

~82 tests. Uses `runExpectI64`, `runExpectF32`, `runExpectF64`,
`runExpectDec`, `runExpectStr`, `runExpectTypeMismatchAndCrash`.

### Batch 4: list_refcount_*.zig (8 files)

These all use `runExpectI64` — fully portable. Migrate all 8 files
together or one at a time.

- `list_refcount_basic.zig`
- `list_refcount_simple.zig`
- `list_refcount_nested.zig`
- `list_refcount_pattern.zig`
- `list_refcount_alias.zig`
- `list_refcount_complex.zig`
- `list_refcount_conditional.zig`
- `list_refcount_containers.zig`

---

## Step-by-Step Workflow

For each batch:

### 1. Read the source file

Open the old test file. Identify all `test "..."` blocks and the
`runExpect*` calls inside them.

### 2. Convert to TestCase entries

For each `runExpect*` call, create a `.{ .name = ..., .source = ...,
.expected = ... }` entry. Follow the mapping rules above.

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

## Final Cleanup (after all tests are migrated)

Once every portable test is migrated and green, the old test files should
already be deleted (you deleted them as you went). Verify:

1. No old test files remain in `src/eval/test/` (except `helpers.zig`,
   `TestEnv.zig`, `parallel_runner.zig`, `eval_tests.zig`, and any
   skipped files from the "What NOT to Migrate" table).
2. `zig build test-eval` passes.
3. Commit any final cleanup.
