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
binary in alternating before/after order. `/usr/bin/time -l` supplied CPU,
retired instructions, and peak RSS; a monotonic clock measured wall time. No builds from this
thread ran during the measurement. Other machine activity was uncontrolled.
These are the final measurements after review removed no-op cache writes
and added the allocation-free singleton initializer path.

| Workload | Before median | After median | Before range | After range |
| --- | ---: | ---: | ---: | ---: |
| Check 2,001 sequential bindings, no cache | 303.3 ms | 306.4 ms | 287.5–312.9 ms | 293.7–321.3 ms |
| Check 1,024-element destructuring, no cache | 162.5 ms | 166.8 ms | 156.9–178.6 ms | 166.0–177.0 ms |
| Test 256 closed method calls, specialize, no cache | 300.8 ms | 304.0 ms | 290.3–313.9 ms | 298.0–310.1 ms |
| JSON round-trip tests, specialize, no cache | 2,717.0 ms | 2,734.3 ms | 2,564.0–2,803.4 ms | 2,536.3–2,987.4 ms |
| Check sequential bindings, warm cache | 80.9 ms | 89.8 ms | 75.0–84.5 ms | 83.5–107.2 ms |

Median retired-instruction changes were +0.24% for sequential checking,
-0.36% for wide destructuring, +0.23% for closed calls, +0.06% for JSON,
and +1.66% for warm-cache checking. The singleton path reduced the
sequential case's roughly 1% instruction increase in the preceding candidate.

Median user+system CPU times before/after were 0.285/0.290 s, 0.150/0.150 s,
0.280/0.280 s, 5.680/5.725 s, and 0.060/0.070 s respectively.
Median peak RSS ranged from 58–245 MiB; each after median was 0.1–0.4 MiB
below its before median.

The sequential input chains `n0 = input + 1` through `n2000`. The wide
input destructures 1,024 copies of a function argument, then sums the
bindings. The closed-call input sums 256 calls of the same record-backed
nominal's scalar-returning method and checks the result. The JSON input
is `test/cli/JsonEncodeRoundTrip.roc`. Local binaries, generated inputs,
raw logs, and results are retained in the ignored `.tmp/rc2-perf/`.

The uncached samples show small instruction-count changes and overlapping
wall-time ranges. The warm-cache median increased by 8.9 ms, with a 1.66%
instruction increase; that result is not evidence of performance neutrality.
These uncontrolled Debug samples do not establish a speedup, statistical
equivalence, or global optimality. Optimized production compiler workloads
remain unmeasured.
