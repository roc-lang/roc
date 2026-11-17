# ARC ownership-query and shell-metadata follow-up

## Outcome

The optimization removes repeated borrow-chain walks, repeated operand-pair
comparisons, and repeated view/container overlap scans. It preserves the
ownership and LIR contracts verified by the existing certifier.

Two controlled comparisons found no statistically significant whole-command
wall-time change on the syntax corpus. This supports retaining the scaling
improvement, not claiming an end-to-end speedup or a universally fastest
implementation. The original 84-module app from #13061 was unavailable.

## Correctness

- `zig build run-test-zig-module-lir --summary new`: 691/691 tests passed,
  including the two new context/path-isolation tests.
- `zig build roc`: Debug compiler built successfully.
- `zig fmt --check src/lir/arc.zig`: passed.
- `roc check --no-cache` on #13043 passed under both lowering strategies.
- All three saved reproducers passed under the Debug compiler with
  `--specialize=yes/no` and `--opt=interpreter/dev`: five expects for #13043,
  one for each #13061 form.
- Both optimized executables completed every benchmark command successfully.

The entire repository test suite, snapshot suite, and MiniCI were not run.

## Measurement protocol

Linux x86_64, Zig 0.17.1. Both compiler executables were built using
`zig build roc -Doptimize=fast`; the baseline was captured before editing
`src/lir/arc.zig`. No revision checkout or history comparison was used.
These measurements preceded the publication-time merge of `origin/main`;
the optimized comparison was not repeated after that merge.

Binary SHA-256 identities:

```text
baseline  47d9c3341865864922733fab436656212d16669d28456c2783dcb505feec730a
candidate 06a125ce3d04b7e2786011e40c749e76778bc5b3c22a2ba15351a1b2fe1b45e4
```

The CI driver's pinned nightly could not be downloaded (`release not found`).
`benchmark.py` is therefore an explicitly separate local measurement:

- 32 cases, with 30 randomized baseline/candidate pairs per case, two warmups.
- Both lowering strategies.
- Cold no-cache check, interpreter run, dev run, and dev build on the single-file
  and multi-module CI syntax fixtures.
- Warm unchanged, entry-edited, leaf-edited, and widely-used-module-edited
  multi-module check and dev run, with separate source trees/caches per side.
- Median paired ratios and distribution-free median confidence intervals,
  Bonferroni-corrected to 95% across the 32 cases.
- GNU time peak RSS; command startup and compiler/runtime children included.
- The CI driver's cold-empty-cache scenario is not covered.

An initial comparison between differently placed artifacts showed a 4–14%
slowdown and lower RSS. It did not reproduce after copying both executables
into equivalent benchmark locations. The runner now always copies both
inputs before warming them; only the controlled runs below support the outcome.

```sh
python3 test/repros/issue-13043/benchmark.py \
  --base .zig-cache/arc-perf/base/roc \
  --candidate .zig-cache/arc-perf/head/roc \
  --out .zig-cache/arc-perf/confirmation.tsv --pairs 30
```

| Controlled run | Range of median time ratios | Range of median RSS ratios | Cases whose time interval excludes 1 |
| --- | --- | --- | --- |
| First | 0.9864–1.0180 | 0.9978–1.0055 | 0/32 |
| Confirmation | 0.9699–1.0111 | 0.9953–1.0054 | 0/32 |

These ranges describe the observed medians, not bounds on every workload's
true performance. Individual intervals are wider. Raw paired observations and
per-case summaries are local build artifacts in `.zig-cache/arc-perf/`
(`comparison.tsv/.txt` and `confirmation.tsv/.txt`).

## Hardware counters

After the paired benchmark, `perf stat -r 7 -e instructions,cycles,minor-faults`
measured a cold dev run of `test/perf/all_syntax_modules/main.roc`.
These batches were sequential, not randomized pairs:

| Strategy | Baseline instructions | Candidate instructions | Baseline cycles | Candidate cycles |
| --- | ---: | ---: | ---: | ---: |
| `--specialize=yes` | 507,976,524 | 507,660,838 | 434,505,313 | 437,974,437 |
| `--specialize=no` | 269,232,313 | 269,381,105 | 255,007,735 | 260,787,044 |

Instruction counts changed by approximately -0.06% and +0.06%; cycles increased
by approximately 0.8% and 2.3%. These counters do not demonstrate a CPU
throughput win. The justification for the change is the bounded work of the
new queries, with no significant wall-time regression detected in the paired
whole-command comparisons. Larger ARC-heavy applications remain important
follow-up workloads.
