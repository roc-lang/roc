# RC2 high-priority fixes: validation

Issues: #13165, #13101, #13100. The original reproduction assertions are
unchanged. The adjacent issue directories retain the pre-fix investigations
and describe the implemented contracts.

## Correctness

The following completed successfully:

- `zig build roc -j2`
- `zig build run-test-zig-module-can -j2` (the full canonicalization suite)
- `zig build run-test-eval-host-effects -j2 -- --filter 'issue 13101'`
  (three cases, interpreter and dev execution)
- `zig build run-test-zig-module-postcheck -j2 -Dtest-filter='producer interface containment' -- --test-filter 'producer interface containment'`
- `zig build run-test-zig-lir-inline -j2 -Dtest-filter='nominal record' -- --test-filter 'nominal record'`
- LIR-inline tests filtered to `closed direct method calls`,
  `nested iterator results`, and `stored codec restore`
- Snapshot output and expected-output checks for
  `generalize_alias_self_reference.md`,
  `issue/self_referential_variable_8942.md`,
  `nominal/nominal_recursive_local_def_generalization.md`, and
  `match_expr/tuple_patterns.md`
- `zig build run-check-zig-lints -j2`, `zig fmt --check`, and `typos` on
  changed sources and reports

Direct CLI checks confirm:

- The #13165 source exits 1 with `invalid assignment to itself`.
- The #13101 application prints `1` with `--specialize=yes --no-cache`.
- Both original #13100 test files pass with specialization enabled.
- `Issue13100RecordLayoutMetadata.roc` passes all seven expectations with
  both specialization strategies.
- `Issue13052FieldNamesRenamedInMethod.roc` passes both expectations.
- `JsonEncodeRoundTrip.roc` passes all 34 expectations with specialization.

Full MiniCI, Windows testing, and optimized-compiler benchmarking were not
run. The CLI fixtures were executed directly; the full CLI harness was not
run.

## Whole-command performance sample

Before/after binaries were built from the same worktree before and after
the production edits, using Zig 0.17.1's default Debug mode on macOS ARM64.
The measurements cover the combined changes, not isolated attribution to
each fix. No Git history comparison or source reset was used.

Each workload had one warmup per binary, followed by six measured runs per
binary in alternating before/after order. `/usr/bin/time -l` supplied CPU
and peak RSS; a monotonic clock measured wall time. No builds from this
thread ran during the measurement. Other machine activity was uncontrolled.

| Workload | Before median | After median | Before range | After range |
| --- | ---: | ---: | ---: | ---: |
| Check 2,001 sequential bindings, no cache | 336.5 ms | 334.8 ms | 328.0–368.5 ms | 317.2–377.7 ms |
| Check 1,024-element destructuring, no cache | 192.6 ms | 190.0 ms | 177.2–210.9 ms | 179.3–210.0 ms |
| Test 256 closed method calls, specialize, no cache | 334.2 ms | 325.5 ms | 290.7–445.2 ms | 295.3–409.5 ms |
| JSON round-trip tests, specialize, no cache | 3,086.4 ms | 3,027.7 ms | 2,883.7–3,395.2 ms | 2,832.1–3,454.2 ms |
| Check sequential bindings, warm cache | 68.0 ms | 69.8 ms | 67.2–69.2 ms | 69.2–70.8 ms |

Median user+system CPU times were unchanged at the timer's precision for
the sequential, wide, closed-call, and warm-cache cases. JSON CPU time was
5.795 s before and 5.870 s after. Median peak RSS ranged from 58–245 MiB;
each after median was 0.1–0.5 MiB below its before median.

The sequential input chains `n0 = input + 1` through `n2000`. The wide
input destructures 1,024 copies of a function argument, then sums the
bindings. The closed-call input sums 256 calls of the same record-backed
nominal's scalar-returning method and checks the result. The JSON input
is `test/cli/JsonEncodeRoundTrip.roc`. Local binaries, generated inputs,
raw logs, and results are retained in the ignored `.tmp/rc2-perf/`.

These results show no large end-to-end regression in the sampled Debug
workloads. They do not establish a speedup, statistical equivalence, or
global optimality. In particular, the warm-cache median increased by
1.8 ms, and optimized production compiler workloads remain unmeasured.
