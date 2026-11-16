# Issue 13081: opaque encoder allocation regression

Source: https://github.com/roc-lang/roc/issues/13081 (body fetched with
`gh issue view 13081 --repo roc-lang/roc --comments`; there were no comments).

Baseline: inherited omnibus PR #13086 head
`ae9f9dbdb7274edf247dc225824d5007309fd859`, preserved without reset or compiler
changes. The built compiler reports `Roc compiler version debug-ae9f9dbd`.

## Focused regression

From the repository root:

```sh
zig build run-test-cli -j2 -- --suite subcommands --filter 'issue 13081' --llvm-only --verbose
```

Requires Zig 0.17.1 and the normal compiler build dependencies, including LLVM.
Validated on macOS arm64. The build step prepares the existing `alloc-count`
platform's host libraries in an isolated fixture tree; no downloaded Roc
platform, allocation interposer, or manually published host is needed.
`--llvm-only` is essential: without it the runner's default backend selection
excludes both cases and executes zero tests.

The fixture preserves the issue's writer package, opaque function-containing
encoder state, derived record/list encoder, and carry-state callbacks. The
driver replaces stdin/repetition with the existing allocation-count host.
It encodes 64 and 1024 values derived from runtime input, measuring only the
encodes, not input construction. The issue's `0.until(count)` requires an
explicit `0.U64` in this baseline.

The harness requires `correct True, amortized True`: the lengths and final
bytes must be correct, and the 16x larger encode may use at most 32 additional
allocation/reallocation calls. This allows geometric buffer growth but rejects
per-item copies. The booleans are checked by the CLI harness, not Roc `expect`,
which optimized builds erase. There are no skip or expected-failure markers.

## Observations

The focused command above exited 1: **0 passed, 2 run failed, 0 skipped**.
Both programs compiled and ran successfully; the harness rejected their output
because it did not contain `issue 13081: correct True, amortized True`.

```text
speed: issue 13081: correct True, amortized False, allocations 196 3076
size:  issue 13081: correct True, amortized False, allocations 477 7197
```

The output checks pass, but the large encode exceeds the allocation allowance
of 228 for speed and 509 for size. Speed uses exactly 3 additional allocations
per additional element; size uses 7. These are allocation-count failures, not
type errors, missing dependencies, crashes, or timing thresholds.

An initial `expect`-based fixture incorrectly passed because optimized builds
erase those assertions; the final harness checks the returned booleans.

No compiler fix, root-cause investigation, or fix recommendation is included.
