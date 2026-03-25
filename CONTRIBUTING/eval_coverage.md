# Eval Interpreter Coverage

Measure line-level code coverage for the Roc eval interpreter using [kcov](https://github.com/SimonKagworst/kcov). This helps identify untested interpreter code so new eval tests can be written to increase coverage.

## Prerequisites

Coverage is supported on:
- **macOS** (arm64 and x86_64)
- **Linux arm64**

Linux x86_64 is **not supported** due to a Zig 0.15.2 DWARF bug. The script will tell you if your platform isn't supported.

On Linux arm64, install the required libraries:
```bash
sudo apt install libdw-dev libcurl4-openssl-dev
```

No extra dependencies are needed on macOS — kcov is built from source by the Zig build system.

## Quick Start

From the repo root:

```bash
# Full run: build kcov, run all eval tests under instrumentation, print summary
python3 CONTRIBUTING/eval_coverage.py

# Takes a while — eval tests run single-threaded under kcov.
# Once done, reuse the cached data for fast queries:
python3 CONTRIBUTING/eval_coverage.py --use-last-run
```

## Output Formats

### Summary (default)

```bash
python3 CONTRIBUTING/eval_coverage.py --use-last-run
```

Prints a table of files ranked by uncovered line count:

```
Eval coverage: 51.35% (5727/11153 lines)

File                                Coverage  Covered   Total  Uncovered
------------------------------------------------------------------------
interpreter.zig                       50.03%     4781    9556       4775
render_helpers.zig                    18.53%       78     421        343
StackValue.zig                        76.49%      527     689        162
...
```

### Lines — uncovered source with context

```bash
python3 CONTRIBUTING/eval_coverage.py --use-last-run --format lines --file interpreter
```

Shows the actual uncovered source code, marked with `>`, with surrounding context:

```
## interpreter.zig — 50.03% covered (4775 uncovered lines)

### Lines 119-120 (uncovered)
   116 |                 i -= 1;
   117 |                 if (alloc_ptrs[i] == ptr) return alloc_sizes[i];
   118 |             }
>  119 |             return 0;
>  120 |         }
   121 |
   122 |         fn reset() void {
   123 |             offset = 0;
```

Use `--context N` to control how many lines of surrounding context to show (default: 2).

### JSON — structured data

```bash
python3 CONTRIBUTING/eval_coverage.py --use-last-run --format json
```

Outputs structured JSON with per-file uncovered line ranges:

```json
{
  "overall": {
    "percent_covered": 51.35,
    "covered_lines": 5727,
    "total_lines": 11153
  },
  "files": [
    {
      "file": "interpreter.zig",
      "percent_covered": 50.03,
      "uncovered_lines": 4775,
      "uncovered_ranges": [
        {"start": 63, "end": 63},
        {"start": 65, "end": 66}
      ]
    }
  ]
}
```

## Useful Flag Combinations

```bash
# Focus on a specific file
python3 CONTRIBUTING/eval_coverage.py --use-last-run --format lines --file StackValue

# Top 3 files with the most uncovered code
python3 CONTRIBUTING/eval_coverage.py --use-last-run --top 3

# More context around uncovered lines
python3 CONTRIBUTING/eval_coverage.py --use-last-run --format lines --file interpreter --context 5

# Include test infrastructure files (excluded by default)
python3 CONTRIBUTING/eval_coverage.py --use-last-run --include-test-files

# JSON for a specific file (good for piping to other tools)
python3 CONTRIBUTING/eval_coverage.py --use-last-run --format json --file interpreter
```

## Using Coverage to Write Tests

The typical workflow:

1. **Run coverage** to collect data:
   ```bash
   python3 CONTRIBUTING/eval_coverage.py
   ```

2. **Identify gaps** — look at the summary to find files with low coverage, then drill into the uncovered lines:
   ```bash
   python3 CONTRIBUTING/eval_coverage.py --use-last-run --format lines --file interpreter --context 5
   ```

3. **Write eval tests** that exercise the uncovered code paths. Eval tests are defined in `src/eval/test/eval_tests.zig` — each entry is a small Roc expression with an expected result:

   ```zig
   .{ .name = "str: hello", .source = "\"hello\"", .expected = .{ .str_val = "hello" } },
   ```

   Available expected types include `.dec_val`, `.bool_val`, `.str_val`, `.f32_val`, `.f64_val`, `.i64_val`, `.u8_val`, `.u64_val`, `.inspect_str` (Str.inspect output), and `.problem` (for compile errors).

4. **Re-run coverage** to verify your new tests hit the target lines:
   ```bash
   python3 CONTRIBUTING/eval_coverage.py
   ```

## Skipped Tests

Tests can skip specific backends using the `skip` field:

```zig
// Skip interpreter and wasm — only runs on dev backend
.{ .name = "dev only: U32 literal", .source = "15.U32",
   .expected = .{ .inspect_str = "15" },
   .skip = .{ .interpreter = true, .wasm = true } },
```

A test with *any* skip reports as **SKIP** rather than PASS, even if the non-skipped backends pass. This keeps partial backend coverage visible — the goal is every backend passing every test.

In coverage mode, only the interpreter backend runs. Tests that skip the interpreter (e.g. `skip = .{ .interpreter = true }`) will always report as SKIP and won't contribute to interpreter coverage. The 110 skipped tests in a typical run are mostly dev-only tests that exercise features the interpreter doesn't support yet.

## How It Works

Under the hood, `zig build coverage-eval` does the following:

1. Builds kcov from source (a lazy Zig dependency)
2. On macOS, codesigns kcov for `task_for_pid` access
3. Builds `eval-coverage-runner` — a separate binary compiled with `-Dcoverage=true`
4. Runs: `kcov --include-pattern=/src/eval/ kcov-output/eval eval-coverage-runner`

The `coverage=true` build option is a comptime flag that:
- **DCEs the dev and wasm backends** — they're never compiled, so the build is faster
- **Disables fork isolation** — eval runs in-process so kcov can trace it
- **Forces single-threaded execution** — required for accurate coverage

The kcov output (JSON and HTML) lands in `kcov-output/eval/eval-coverage-runner/`.

The Python script parses kcov's JSON output files and reformats them. You can also browse the full HTML report directly at `kcov-output/eval/eval-coverage-runner/index.html`.
