# Improving Eval Test Coverage via Data-Driven Tests

## Goal

Improve code coverage of `src/eval/interpreter.zig` (and other `src/eval/`
files) **exclusively by adding new test cases** to
`src/eval/test/eval_tests.zig`. Do not modify interpreter source code,
do not modify the test runner, and do not modify helpers.

Current coverage: **~50%**. Target: maximize coverage by exercising
uncovered interpreter branches through Roc expressions.

## How It Works

1. Run `zig build coverage-eval` to generate coverage data.
2. Run the analysis script (see below) to identify uncovered code.
3. Read the uncovered interpreter source to understand what Roc expression
   would trigger it.
4. Write a `TestCase` entry in `eval_tests.zig`.
5. Run `zig build test-eval` to verify — if a test **crashes or fails**,
   mark it `.skip = SKIP_ALL` with a `// TODO:` comment and move on.
6. Repeat until diminishing returns.

---

## Critical Rules

### 1. Never debug failures — SKIP and move on

You are writing tests to improve **coverage**, not to fix bugs. Many
uncovered branches will expose interpreter bugs. When a test fails or
crashes:

```zig
// TODO: narrowing conversions crash in interpreter
.{
    .name = "coverage: U64 to U8 wrapping",
    .source = "{ 300.U64.to_u8() }",
    .expected = .{ .u8_val = 44 },
    .skip = SKIP_ALL,
},
```

**Do not:**
- Investigate why the crash happens
- Modify interpreter.zig to fix it
- Modify parallel_runner.zig or helpers.zig
- Spend more than 30 seconds deciding if a test is correct
- Remove a skipped test — leave it for someone to fix later

**Do:**
- Include the error message or crash location in the TODO comment
- Keep the test so the bug is documented
- Move on to the next uncovered region immediately

### 2. Work in small batches

Add 5–15 tests at a time, then run `zig build test-eval`. This catches
crashes early before you waste time writing tests that depend on broken
features.

### 3. Only modify eval_tests.zig (unless fixing runner bugs)

The primary file you should edit is `src/eval/test/eval_tests.zig`. Do not
touch:
- `helpers.zig`
- `interpreter.zig`
- `build.zig`

If you discover a bug in `parallel_runner.zig` itself (e.g. skip logic
not working), fixing the runner is acceptable — but don't modify it just
to make a failing test pass.

### 4. Roc syntax gotchas

**Type conversions use method syntax**, NOT `Num.toX()`:
- WRONG: `Num.toF64(42.I32)`, `Num.toI8Wrapping(300.I64)`
- RIGHT: `{ 42.I32.to_f64() }`, `{ 300.I64.to_i8() }`

Wrap single-expression method calls in `{ }` blocks for clarity.
Check existing tests in eval_tests.zig for syntax examples before
writing new ones.

### 5. Commit after each successful batch

After each batch of tests passes (or is properly SKIPped), commit:
```
git add src/eval/test/eval_tests.zig
git commit -m "Add N eval coverage tests for <feature area>"
```

---

## The Analysis Workflow

### Step 1: Generate coverage

```sh
zig build coverage-eval
```

This runs all eval tests under kcov and produces coverage data in
`kcov-output/eval/eval-test-runner/`.

### Step 2: Identify uncovered code

Run the analysis script below to find the largest uncovered regions:

```sh
python3 scripts/eval_coverage_gaps.py
```

This prints uncovered ranges in `interpreter.zig` with source context,
sorted by size. Focus on the largest gaps first — they give the most
coverage improvement per test.

### Step 3: Read the uncovered source

The script output shows line numbers and source snippets. Read the
uncovered code in `src/eval/interpreter.zig` to understand:
- What Roc language feature triggers this code path?
- What expression would cause the interpreter to enter this branch?

Common patterns in uncovered interpreter code:

| Uncovered code pattern | Roc expression to trigger |
|----------------------|--------------------------|
| `.i64_to_i128` (widening) | `{ 42.I64.to_i128() }` |
| `.i32_to_f64` (int→float) | `{ 42.I32.to_f64() }` |
| `.list_swap` | `List.swap([1,2,3], 0, 2)` |
| `.str_split` | `Str.split("a,b,c", ",")` |
| Comparison operators on specific types | `5.I32 > 3.I32` |
| Specific match patterns | `match (1, 2) { (a, b) => a + b }` |
| `for ... in` with index | `for item, idx in [1,2,3] { ... }` |
| Record update syntax | `{ ..rec, field: newVal }` |
| Numeric binary ops for specific types | `1.I32 + 2.I32` |

### Step 4: Write the test

```zig
// --- coverage: <feature area> ---
.{
    .name = "coverage: <descriptive name>",
    .source = "<roc expression>",
    .expected = .{ .<variant> = <value> },
},
```

### Step 5: Verify

```sh
zig build test-eval
```

If any new test fails, add `.skip = SKIP_ALL` and a TODO comment.

### Step 6: Re-measure

```sh
zig build coverage-eval
python3 scripts/eval_coverage_gaps.py
```

Confirm the gap shrank. Move to the next uncovered region.

---

## TestCase Format Reference

```zig
const TestCase = @import("parallel_runner.zig").TestCase;
const RocDec = @import("builtins").dec.RocDec;

// Convenience constant for skipping all backends (test documents a bug)
const SKIP_ALL: TestCase.Skip = .{
    .interpreter = true,
    .dev = true,
    .wasm = true,
    .llvm = true,
};

pub const tests = [_]TestCase{
    .{ .name = "coverage: example", .source = "42", .expected = .{ .dec_val = 42 * RocDec.one_point_zero_i128 } },
};
```

### Expected Variants

| Variant | When to use | Example |
|---------|------------|---------|
| `.dec_val` | Unsuffixed numeric result (`1 + 2`, `42`) | `.dec_val = 3 * RocDec.one_point_zero_i128` |
| `.i64_val` | `.I64`-suffixed result | `.i64_val = 42` |
| `.i8_val` | `.I8`-suffixed result | `.i8_val = -1` |
| `.i16_val` | `.I16`-suffixed result | `.i16_val = 100` |
| `.i32_val` | `.I32`-suffixed result | `.i32_val = 100` |
| `.i128_val` | `.I128`-suffixed result | `.i128_val = 100` |
| `.u8_val` | `.U8`-suffixed result | `.u8_val = 255` |
| `.u16_val` | `.U16`-suffixed result | `.u16_val = 100` |
| `.u32_val` | `.U32`-suffixed result | `.u32_val = 100` |
| `.u64_val` | `.U64`-suffixed result | `.u64_val = 100` |
| `.u128_val` | `.U128`-suffixed result | `.u128_val = 100` |
| `.bool_val` | Boolean result | `.bool_val = true` |
| `.str_val` | String result | `.str_val = "hello"` |
| `.f32_val` | `.F32`-suffixed result | `.f32_val = 1.5` |
| `.f64_val` | `.F64`-suffixed result | `.f64_val = 2.5` |
| `.err_val` | Expected error (crash, etc.) | `.err_val = error.Crash` |
| `.problem` | Parse/type error expected | `.problem = {}` |

### Unsuffixed literals are Dec, not I64

This is the #1 mistake. In Roc, `42` is Dec (decimal), not I64.
Only `42.I64` is I64. When your expression uses unsuffixed numbers,
use `.dec_val = N * RocDec.one_point_zero_i128`.

### Multiline source

```zig
.{
    .name = "coverage: for loop with index",
    .source =
        \\{
        \\    var $sum = 0.I64
        \\    for _item, idx in [10, 20, 30] {
        \\        $sum = $sum + idx.to_i64()
        \\    }
        \\    $sum
        \\}
    ,
    .expected = .{ .i64_val = 3 },
},
```

### Skipping backends

```zig
// Skip specific backends
.skip = .{ .wasm = true },

// Skip ALL backends (test documents a bug, still contributes to coverage tracking)
.skip = SKIP_ALL,
```

---

## Coverage Priority Guide

Focus on these areas in order (largest coverage gaps first):

### Tier 1: Numeric type conversions (lines ~4000–4600)
Massive block of `intConvertWrap`, `intConvertTry`, `intToFloat`,
`intToDec` for every type combination.

**IMPORTANT: Roc uses method-style syntax for conversions, not `Num.toX()`.**
The correct syntax is `value.to_target_type()`:
```zig
// Widening conversions (these WORK):
.{ .name = "coverage: I32 to F64", .source = "{ 42.I32.to_f64() }", .expected = .{ .f64_val = 42.0 } },
.{ .name = "coverage: I64 to I128", .source = "{ 42.I64.to_i128() }", .expected = .{ .i128_val = 42 } },
.{ .name = "coverage: U16 to U32", .source = "{ 42.U16.to_u32() }", .expected = .{ .u32_val = 42 } },

// Narrowing/wrapping conversions (these CRASH — skip them):
// TODO: narrowing conversions crash in interpreter
.{ .name = "coverage: U64 to U8", .source = "{ 300.U64.to_u8() }", .expected = .{ .u8_val = 44 }, .skip = SKIP_ALL },

// Signed-to-unsigned conversions (these CRASH — skip them):
// TODO: signed-to-unsigned conversions crash in interpreter
.{ .name = "coverage: I64 to U64", .source = "{ 42.I64.to_u64() }", .expected = .{ .u64_val = 42 }, .skip = SKIP_ALL },
```

**Known working conversions:** widening int→int (e.g. I8→I64, U16→I128),
int→float (e.g. I32→F64, U64→F64), small-to-large same-sign (e.g. U32→U64).

**Known crashing conversions:** any narrowing (e.g. I64→I8, U64→U8),
any signed→unsigned (e.g. I64→U64, I64→U32), any wrapping variant.

### Tier 2: Low-level numeric operations (lines ~3000–4000)
Bitwise ops, shift ops, comparison ops for specific types:
```zig
.{ .name = "coverage: bitwise and I64", .source = "Num.bitwiseAnd(0xFF.I64, 0x0F.I64)", .expected = .{ .i64_val = 15 } },
.{ .name = "coverage: shift left", .source = "Num.shiftLeftBy(1.I64, 4.U8)", .expected = .{ .i64_val = 16 } },
```

### Tier 3: String/List builtins (lines ~5000–6000, ~13000–14000)
String operations and list operations that aren't tested yet:
```zig
.{ .name = "coverage: Str.split", .source = "Str.split(\"a,b\", \",\").len().to_str()", .expected = .{ .str_val = "2" } },
```

### Tier 4: Method dispatch / binop fallbacks (lines ~17000–18000)
Numeric method dispatch on various types. These work well and cover
large gaps:
```zig
.{ .name = "coverage: U32 addition method", .source = "1.U32 + 2.U32", .expected = .{ .u32_val = 3 } },
.{ .name = "coverage: I32 greater than", .source = "5.I32 > 3.I32", .expected = .{ .bool_val = true } },
.{ .name = "coverage: I64 division", .source = "20.I64 // 4.I64", .expected = .{ .i64_val = 5 } },
.{ .name = "coverage: I64 remainder", .source = "17.I64 % 5.I64", .expected = .{ .i64_val = 2 } },
```

### Tier 5: Pattern matching edge cases (lines ~11000–12000, ~15000–16000)
Complex match patterns, nested destructuring:
```zig
.{
    .name = "coverage: match with guard",
    .source =
        \\match 5 {
        \\    x if x > 3 => "big"
        \\    _ => "small"
        \\}
    ,
    .expected = .{ .str_val = "big" },
},
```

### Tier 6: render_helpers.zig (18.5% covered)
The `Str.inspect` path for various value types. Exercised by adding
tests whose results go through inspect:
```zig
.{ .name = "coverage: inspect list of strings", .source = "[\"a\", \"b\"].to_str()", .expected = .{ .str_val = "[\"a\", \"b\"]" } },
```

---

## What NOT to Test

- **Compiler internals** — don't try to trigger type checker or parser
  code from eval tests. Coverage only measures `src/eval/` files.
- **Error recovery paths** — paths guarded by `unreachable` or that
  require malformed IR won't be reachable from valid Roc expressions.
- **Already-covered code** — check the coverage report before writing
  tests. Don't duplicate existing coverage.
- **Module-level features** — the eval runner evaluates single
  expressions, not full modules. You can't test `import`, `module`,
  `app`, etc.

---

## Analysis Script

Save this as `scripts/eval_coverage_gaps.py` and run it after
`zig build coverage-eval`:

```python
#!/usr/bin/env python3
"""Analyze kcov coverage data for eval tests and report uncovered gaps.

Usage:
    zig build coverage-eval
    python3 scripts/eval_coverage_gaps.py [--file FILE] [--min-gap N] [--context N]

Options:
    --file FILE     Analyze a specific file (default: interpreter.zig)
    --min-gap N     Minimum gap size to report (default: 3)
    --context N     Lines of source context to show (default: 3)
    --all           Show all files, not just the specified one
"""

import json
import argparse
import sys
from pathlib import Path


def find_coverage_json():
    """Find the codecov.json file in kcov output."""
    base = Path("kcov-output/eval/eval-test-runner")
    # Follow symlink if needed
    if base.is_symlink():
        base = base.resolve()
    codecov = base / "codecov.json"
    if not codecov.exists():
        print("ERROR: Coverage data not found. Run 'zig build coverage-eval' first.",
              file=sys.stderr)
        sys.exit(1)
    return codecov


def find_source_file(basename):
    """Find the full path to a source file given its basename."""
    # Search in src/eval/
    for p in Path("src/eval").rglob(basename):
        return p
    return None


def parse_coverage(codecov_path, target_file):
    """Parse codecov.json and return (covered_lines, uncovered_lines) for target."""
    with open(codecov_path) as f:
        data = json.load(f)

    coverage = data.get("coverage", {})
    if target_file not in coverage:
        # Try matching by basename
        matches = [k for k in coverage if k.endswith(target_file) or target_file.endswith(k)]
        if not matches:
            print(f"ERROR: '{target_file}' not found in coverage data.", file=sys.stderr)
            print(f"Available files: {', '.join(sorted(coverage.keys()))}", file=sys.stderr)
            sys.exit(1)
        target_file = matches[0]

    lines = coverage[target_file]
    covered = sorted(int(k) for k, v in lines.items() if not v.startswith("0/"))
    uncovered = sorted(int(k) for k, v in lines.items() if v.startswith("0/"))
    return target_file, covered, uncovered


def group_ranges(line_numbers):
    """Group line numbers into contiguous ranges."""
    if not line_numbers:
        return []
    ranges = []
    start = prev = line_numbers[0]
    for l in line_numbers[1:]:
        if l == prev + 1:
            prev = l
        else:
            ranges.append((start, prev))
            start = prev = l
    ranges.append((start, prev))
    return ranges


def read_source_lines(filepath, start, end, context=0):
    """Read source lines from a file."""
    try:
        with open(filepath) as f:
            all_lines = f.readlines()
        # Adjust for 0-based indexing
        s = max(0, start - 1 - context)
        e = min(len(all_lines), end + context)
        result = []
        for i in range(s, e):
            line_num = i + 1
            marker = "  " if start <= line_num <= end else "  "
            if start <= line_num <= end:
                marker = ">>"
            result.append(f"  {marker} {line_num:5d} | {all_lines[i].rstrip()}")
        return "\n".join(result)
    except FileNotFoundError:
        return f"  (source file not found: {filepath})"


def print_summary(target_file, covered, uncovered):
    """Print coverage summary."""
    total = len(covered) + len(uncovered)
    pct = 100 * len(covered) / total if total > 0 else 0
    print(f"\n{'='*60}")
    print(f"COVERAGE GAPS: {target_file}")
    print(f"{'='*60}")
    print(f"  Covered:   {len(covered):5d} lines")
    print(f"  Uncovered: {len(uncovered):5d} lines")
    print(f"  Total:     {total:5d} lines")
    print(f"  Coverage:  {pct:.1f}%")


def print_all_files_summary(codecov_path):
    """Print summary for all files."""
    with open(codecov_path) as f:
        data = json.load(f)

    coverage = data.get("coverage", {})
    print(f"\n{'='*60}")
    print("ALL FILES COVERAGE SUMMARY")
    print(f"{'='*60}")

    rows = []
    for fname, lines in sorted(coverage.items()):
        total = len(lines)
        uncovered = sum(1 for v in lines.values() if v.startswith("0/"))
        covered = total - uncovered
        pct = 100 * covered / total if total > 0 else 0
        rows.append((fname, covered, uncovered, total, pct))

    # Sort by uncovered count descending
    rows.sort(key=lambda r: r[2], reverse=True)
    for fname, covered, uncovered, total, pct in rows:
        bar = "#" * int(pct / 2) + "." * (50 - int(pct / 2))
        print(f"  {fname:40s} {pct:5.1f}%  {bar}  ({uncovered} uncovered)")
    print()


def main():
    parser = argparse.ArgumentParser(description="Analyze eval test coverage gaps")
    parser.add_argument("--file", default="interpreter.zig",
                        help="File to analyze (default: interpreter.zig)")
    parser.add_argument("--min-gap", type=int, default=3,
                        help="Minimum gap size to report (default: 3)")
    parser.add_argument("--context", type=int, default=3,
                        help="Lines of source context (default: 3)")
    parser.add_argument("--all", action="store_true",
                        help="Show summary for all files")
    args = parser.parse_args()

    codecov_path = find_coverage_json()

    if args.all:
        print_all_files_summary(codecov_path)

    target_file, covered, uncovered = parse_coverage(codecov_path, args.file)
    print_summary(target_file, covered, uncovered)

    # Find source file
    source_path = find_source_file(target_file)

    # Group into ranges
    ranges = group_ranges(uncovered)
    ranges.sort(key=lambda r: r[1] - r[0], reverse=True)

    # Filter by min-gap
    ranges = [(s, e) for s, e in ranges if (e - s + 1) >= args.min_gap]

    print(f"\n  {len(ranges)} uncovered ranges of {args.min_gap}+ lines:\n")

    for i, (start, end) in enumerate(ranges):
        size = end - start + 1
        print(f"  --- Gap #{i+1}: lines {start}-{end} ({size} lines) ---")
        if source_path:
            print(read_source_lines(str(source_path), start, end, context=args.context))
        print()

        # Stop after 50 gaps to avoid overwhelming output
        if i >= 49:
            remaining = len(ranges) - 50
            print(f"  ... and {remaining} more gaps. Use --min-gap to filter.\n")
            break


if __name__ == "__main__":
    main()
```

---

## Example Session

```
$ zig build coverage-eval
$ python3 scripts/eval_coverage_gaps.py --min-gap 10 --context 2

============================================================
COVERAGE GAPS: interpreter.zig
============================================================
  Covered:    4629 lines
  Uncovered:  4927 lines
  Total:      9556 lines
  Coverage:   48.4%

  42 uncovered ranges of 10+ lines:

  --- Gap #1: lines 17681-17729 (49 lines) ---
       17679 |                     // Handle numeric arithmetic via type-aware ...
       17680 |                     if (ba.method_ident.eql(self.root_env.idents.plus)) {
    >> 17681 |                         const result = try self.evalNumericBinop(.add, ...
    ...

# I see this is numeric binop dispatch for method syntax on non-Dec types.
# Let me write tests for +, -, *, >, <, >= on I32/I64/U32/U64:

.{ .name = "coverage: I32 addition via method", .source = "1.I32 + 2.I32", .expected = .{ .i32_val = 3 } },
.{ .name = "coverage: I32 greater than", .source = "5.I32 > 3.I32", .expected = .{ .bool_val = true } },
.{ .name = "coverage: I64 division", .source = "20.I64 // 4.I64", .expected = .{ .i64_val = 5 } },

$ zig build test-eval    # all pass!
$ zig build coverage-eval
$ python3 scripts/eval_coverage_gaps.py --min-gap 10 --context 2
# Gap #1 is now smaller or gone. Move to next gap.
```

---

## Naming Convention

Prefix all coverage tests with `"coverage: "` so they're easily
identifiable:

```zig
.{ .name = "coverage: <area>: <specific thing>", ... },
```

Examples:
- `"coverage: num convert: u64 to i8 wrapping"`
- `"coverage: bitwise: shift left I64"`
- `"coverage: str: split comma"`
- `"coverage: match: nested tuple destructure"`
- `"coverage: for loop: with index variable"`

---

## Known Interpreter Crash Patterns

These patterns are known to crash the interpreter. Write the test anyway
with `.skip = SKIP_ALL` to document the bug, then move on.

| Pattern | Example | Status |
|---------|---------|--------|
| Narrowing int conversions | `{ 300.U64.to_u8() }` | Crash |
| Signed→unsigned conversions | `{ 42.I64.to_u64() }` | Crash |
| Wrapping conversions | `{ 300.I64.to_i8() }` | Crash |

**Conversions that DO work:** widening same-sign int→int (U16→U32,
I8→I64), int→float (I32→F64, U64→F64), int→I128 from any type.

**Arithmetic that works:** `+`, `-`, `*`, `//`, `%`, `>`, `<`, `>=`,
`<=`, `==`, `!=` on I32, I64, U32, U64 types all pass.

---

## Tracking Progress

After each session, note the coverage percentage. The goal is steady
improvement, not perfection. Many uncovered lines are unreachable error
handlers or type combinations that can't be triggered from valid Roc
expressions.

Good stopping point: when most remaining gaps are `unreachable`,
error handlers, or require features not supported in the expression
evaluator (modules, imports, etc.).
