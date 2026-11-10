# Table storage: measurements and audit

## Status

This is a **draft implementation, not a completed performance gate**. It
implements the storage primitives, growing type columns, selected sparse and
compact columns, reusable visit sets, ARC groupings, and interpreter slot
initialization changes. The wider optional-index and epoch sweeps remain
incomplete. Proposed conversions are not automatically valid: retained output,
tagged operands, and once-per-graph tables have different lifetimes.

The branch incorporates `origin/main` and uses its required Zig 0.17.1.
The merged tree's full Zig suite is not green. Earlier passing runs and
measurements below are historical evidence, not a claim that the merged tree
passes the same integration gate.

## Measurement method

Linux x86-64; ReleaseFast; CPU affinity 0-15; `perf stat` using
`cpu_core/instructions/`, `task-clock`, minor faults and major faults.
`/usr/bin/time -v` supplies peak RSS. One heavy job runs at a time under a
shared lock, with a 12 GiB measurement limit; optimized builds use 24 GiB.
Wall time is not used to claim speedups on this CPU-noisy host.

Host policy for these runs:

- THP enabled: `always [madvise] never`
- THP defrag: `always defer defer+madvise [madvise] never`
- `vm.overcommit_memory=0`, `vm.max_map_count=1048576`
- `RLIMIT_AS` unlimited

Each comparison starts with an exclusively created compiler cache and then
three warm runs. Unless noted otherwise, phase comparisons use three
alternating before/after pairs; cold numbers are medians of three and warm
numbers medians of nine. Cache files are disposable; raw counters, commands,
binary hashes, diagnostics and host settings remain in the checkout's ignored
`.tmp/storage-measure` directory.

### Build/test performance decision scope

Richard clarified that only `roc build` and `roc test` performance determines
this choice. The check-only measurements below remain historical diagnostic
evidence, not a basis for selecting storage backing.

Cold and warm correctness preflights passed for all three saved variants on:

- randomized-spa WASM `build --opt=dev`, with specialization enabled and disabled;
- roc-pg `examples/small/main.roc` native `build --opt=dev --specialize=no`;
- roc-pg `test --opt=dev --specialize=no package/main.roc`: all 383 tests passed;
- the isolated roc-pdf compatibility fixture's `test --opt=dev package/main.roc`,
  with specialization enabled and disabled: all 605 tests passed, including
  its local Unicode dependency.

Build artifacts were byte-identical across variants and cold/warm preflights.
Exit 2 in roc-pg and roc-pdf denotes warnings, not failed tests or builds.
The native roc-pg workload uses `--replace-dep` with an isolated copy of its
basic-cli platform: interpolation annotations consume `Iter(Str)`, and the
unknown stream length is `Err(Unknown)`, matching this compiler's APIs.
The original platform package and application repositories are unchanged.
Every variant receives the identical compatibility fixture.

Excluded from this bounded comparison, not treated as successful workloads:
specialized roc-pg build/full test exceeded the 600-second preflight limit;
randomized-spa test reported 108 failed assertions in both strategies.
The unadapted native roc-pg platform had five type errors.

The build/test runner uses all six variant orders, cold plus three warm runs,
and the same affinity and memory cap across variants. It records `duration_time`
in nanoseconds from perf alongside task-clock, instructions, faults and peak
RSS, preserves raw output, and checks diagnostic equivalence, build artifact
hashes, and unchanged workload input hashes. A fresh explicit quiet/ready
confirmation is required before this separate measurement window.

#### Build/test results, 2026-11-09

After Richard confirmed the machine was quiet, all 432 invocations completed.
The other thread remained paused until the window was explicitly released.
Each variant/workload/strategy cell has six cold samples and eighteen warm
samples, arranged in six balanced experimental rounds. Warm repeats within
one round are not independent rounds. Cold means a fresh compiler cache,
not a flushed OS page cache.

Median elapsed seconds from perf's `duration_time`:

| Workload | Baseline cold / warm | Recycling cold / warm | Non-recycling cold / warm |
| --- | ---: | ---: | ---: |
| SPA WASM build, specialized | 3.2465 / 1.8091 | 3.2284 / 1.8010 | 3.2515 / 1.8099 |
| SPA WASM build, Boxy | 2.2390 / 0.07379 | 2.2296 / 0.07217 | 2.2321 / 0.07153 |
| roc-pg native build, Boxy | 2.5638 / 0.09044 | 2.5477 / 0.08874 | 2.5491 / 0.08766 |
| roc-pg test, Boxy | 2.7921 / 0.04860 | 2.7867 / 0.04630 | 2.7983 / 0.04624 |
| roc-pdf test, specialized | 10.6404 / 0.11867 | 10.6443 / 0.11761 | 10.6683 / 0.11755 |
| roc-pdf test, Boxy | 4.6022 / 0.11946 | 4.6127 / 0.11890 | 4.6041 / 0.11775 |

Changes in median wall time relative to baseline; negative means faster:

| Workload | Recycling cold / warm | Non-recycling cold / warm |
| --- | ---: | ---: |
| SPA WASM build, specialized | -0.56% / -0.45% | +0.16% / +0.05% |
| SPA WASM build, Boxy | -0.42% / -2.20% | -0.31% / -3.06% |
| roc-pg native build, Boxy | -0.63% / -1.88% | -0.57% / -3.07% |
| roc-pg test, Boxy | -0.19% / -4.72% | +0.22% / -4.84% |
| roc-pdf test, specialized | +0.04% / -0.89% | +0.26% / -0.95% |
| roc-pdf test, Boxy | +0.23% / -0.47% | +0.04% / -1.43% |

Peak RSS medians in MiB:

| Workload | Baseline cold / warm | Recycling cold / warm | Non-recycling cold / warm |
| --- | ---: | ---: | ---: |
| SPA WASM build, specialized | 1143.32 / 945.59 | 1162.88 / 953.12 | 1169.92 / 959.31 |
| SPA WASM build, Boxy | 644.43 / 269.52 | 659.16 / 283.82 | 662.87 / 284.33 |
| roc-pg native build, Boxy | 814.89 / 380.07 | 832.43 / 393.55 | 832.04 / 394.84 |
| roc-pg test, Boxy | 808.61 / 248.96 | 825.71 / 260.60 | 834.88 / 261.65 |
| roc-pdf test, specialized | 3101.74 / 472.65 | 3117.42 / 484.70 | 3143.81 / 485.83 |
| roc-pdf test, Boxy | 976.34 / 472.11 | 988.89 / 484.47 | 986.66 / 485.49 |

Cold wall-time changes are below 0.7% with overlapping ranges: no clear cold
winner. The strongest positive signal is warm Boxy builds/tests. Both compact
variants improved warm SPA Boxy build in every matched round; recycling
improved warm roc-pg build in every round, and both improved warm roc-pg test
in five of six rounds. These short warm invocations save roughly 1–3 ms.
The small differences between recycling and non-recycling do not establish
a universal backing-policy winner.

The RSS increases are about 0.5–5.5%, or 8–42 MiB, not an independent rejection
criterion. Both compact variants remain candidates on this build/test evidence.
No check timing contributes to this conclusion.

All expected exits and diagnostic comparisons passed. All 216 generated
artifact hashes matched within each build workload/strategy across variants
and cache states, and workload input hashes were unchanged. Raw outputs,
all counters, ranges, and matched-round comparisons are retained in the
attached worktree at `.tmp/storage-build-test/results-20261109-115800`.
This is the completed cold/warm matrix described above, not evidence for
the excluded specialized roc-pg cases or post-edit performance.

### Historical check-only comparison

The compact-index sweep's noisy-host roc-pg runs were roughly
instruction-neutral with about 1.5% higher median peak RSS. This is provisional
evidence, not a rejection criterion or a wall-time result. A meaningful
wall-time improvement can justify a modest RSS increase; evaluate speed and
memory together rather than requiring every metric to improve. An excessive
memory increase still needs separate consideration.

When the machine is quiet, compare these three variants:

| Variant | Representation and backing | Saved artifact in `.tmp/storage-measure` |
| --- | --- | --- |
| Baseline | Before the first compact-index sweep | `pre-sweep-fixed-roc` |
| Recycling | Compact indices; allocator-backed, explicitly filled columns | `compact-sweep-filled-roc` |
| Non-recycling | Same compact indices; fresh sparse mappings above the existing small-table threshold | `compact-sweep-first-roc` |

The backing comparison changes only the two selected/promoted callable-binding
columns and the method declaration-statement column. Both compact variants
retain the same other conversions and added callable-template index; comparing
either with baseline measures the batch, not encoding alone.

Saved baseline SHA-256:
`847366e0368a66317f4a44aef4ea2d5fd705b503339420f52af72162ba44cae5`.
Saved non-recycling SHA-256:
`e3c1981a24a23f09f38836f8bfd6f23dffa045a43b72158e5d3b45bb9c0f36e5`.
Saved recycling SHA-256:
`e746edf187ba4f5260c98e473fa17a831119faec619d7e1b8e91634e2d9fe0f7`.
The backing-policy source variants are saved as `sweep1-filled-check.zig` and
`sweep1-sparse-check.zig`. The recycling native build passed with
`zig build roc -Doptimize=ReleaseFast -j1 --summary failures`.
The broader default install target failed in echo-WASM on `HostSplice`;
native build success does not imply that target passes.

Preparation produced uniformly stripped copies of the three saved binaries.
Every runtime-allocated ELF section was verified unchanged by stripping;
the originals remain intact. Original and stripped hashes are recorded
together. This removes the debug-metadata difference between saved artifacts
without changing their executable code.

Use the same Zig 0.17.1 ReleaseFast configuration, corpus inputs, commands,
worker counts, affinity, memory limit, and host policy across all variants.
Rotate execution order across repeated three-way rounds, with independent
compiler caches and equivalent cold, warm, and post-edit workloads. Report
wall-time distributions alongside peak RSS, instructions, task-clock, and
page faults, and verify equivalent diagnostics and outputs. Record host
load; defer wall-time conclusions if contention returns. Do not choose a
backing policy from the current noisy-host results alone.

The first coordinated window covers roc-pg and randomized-spa `check`, with
specialization enabled and disabled, across all six permutations of variant
order. Each case has a fresh compiler cache followed by three warm runs.
This is not the full build/test/post-edit matrix. Start only after the other
thread explicitly confirms quiet/ready, and release the window explicitly
after all measurements finish or the run is abandoned.

#### Coordinated results, 2026-11-09

The first window completed all 288 invocations. The other thread independently
verified quiet/ready before execution and resumed work only after the window
was explicitly released. CPU and memory pressure were zero before the run;
minor desktop activity remained. High reported I/O wait was traced to two
interruptibly sleeping Ghostty `io_cqring_wait` threads on CPUs 10 and 16:
their CPU ticks were unchanged, block-I/O delay was zero, and physical disks
had essentially no traffic or queued requests. This is a recorded environment
caveat, not evidence that those CPUs were doing competing work.

Cold compiler-cache wall time, median [minimum, maximum] seconds across six
balanced rounds:

| Workload | Baseline | Recycling | Non-recycling |
| --- | ---: | ---: | ---: |
| roc-pg, specialized | 8.215 [8.190, 8.240] | 8.220 [8.170, 8.290] | 8.150 [8.130, 8.180] |
| roc-pg, Boxy | 2.460 [2.440, 2.480] | 2.450 [2.440, 2.460] | 2.480 [2.450, 2.480] |
| randomized-spa, specialized | 0.705 [0.690, 0.740] | 0.695 [0.690, 0.730] | 0.715 [0.690, 0.750] |
| randomized-spa, Boxy | 1.105 [1.090, 1.130] | 1.115 [1.100, 1.130] | 1.115 [1.100, 1.150] |

Cold peak RSS medians in MiB, with change relative to baseline:

| Workload | Baseline | Recycling | Non-recycling |
| --- | ---: | ---: | ---: |
| roc-pg, specialized | 3515.63 | 3524.29 (+0.25%) | 3526.73 (+0.32%) |
| roc-pg, Boxy | 780.59 | 783.18 (+0.33%) | 785.03 (+0.57%) |
| randomized-spa, specialized | 425.18 | 425.25 (+0.02%) | 426.60 (+0.33%) |
| randomized-spa, Boxy | 516.86 | 518.76 (+0.37%) | 521.33 (+0.87%) |

Recycling's cold instruction-count changes were -0.085%, -0.329%, -0.652%,
and -0.034%, respectively. Non-recycling's were -0.011%, +0.004%, -0.018%,
and +0.276%. Warm wall-time medians across 18 samples per cell were identical
across variants: 0.350, 0.040, 0.300, and 0.040 seconds respectively. The
external timer reports only hundredths of a second, so these equal medians
do not establish equality at finer resolution.

The modest memory increases are not a reason to reject either variant.
Non-recycling improved specialized roc-pg wall time by about 0.79%; its six
samples were all faster than the six baseline samples. Other cold results
have overlapping ranges and mixed directions. There is no demonstrated
across-workload wall-time winner from this check-only window. Keep the
variants available for the remaining build/test/post-edit comparisons rather
than selecting a universal backing policy from instruction counts alone.

All expected exit codes and diagnostic comparisons passed. Comparison ignores
only the duration in the CLI's `No errors found in ...` success line; raw output
is retained. An initial attempt stopped on that variable duration, and none
of its measurements are included above. The completed run is saved in the
attached worktree at `.tmp/storage-quiet/results-20261109-020529`, including
the full counter/distribution summary and binary-hash manifest. Environment
samples are in `.tmp/storage-quiet/environment`. "Cold" means a fresh compiler
cache, not a flushed OS page cache.

## Phased results before the main merge

These binaries used Zig 0.17.0. Rows are separate adjacent-phase comparisons,
not an additive decomposition of a single run. Counts are retired instructions,
in billions, for `roc-pg check package/main.roc`.

| Change | Cold before | Cold after | Warm before | Warm after |
| --- | ---: | ---: | ---: | ---: |
| Borrow checked-type view metadata | 227.925 | 213.487 | 8.082 | 7.570 |
| Reserved type-store columns | 213.572 | 212.351 | 7.569 | 7.520 |
| Sparse columns (one pair only) | 214.510 | 211.492 | 7.520 | 7.522 |
| Later compact columns, epochs and ARC groups | 212.739 | 212.450 | 7.518 | 7.514 |

Borrowing metadata saved about 6.33% cold and 6.34% warm instructions.
The original assembly contained repeated 304-byte metadata copies in
`CheckedTypeStoreView.canonicalId`. The optimization preserves owned snapshots.

Reserved columns reduced median cold minor faults from 386,822 to 373,920
and warm minor faults from 62,914 to 57,875. The single sparse comparison
reduced cold minor faults from 399,630 to 351,101, but the before run also
had three major faults. That single pair is not a robust causal attribution.

CPU-clock sampling after kernel symbols became available attributed about
2.4% of samples inclusively to page-fault handling; page initialization's
1.93% overlaps that total. The borrowed-view profile changed memcpy's sampled
share from 8.67% to 4.90%. These are sampled shares, not instruction counts
or independently additive costs.

For `randomized-spa` wasm builds, the later migration batch was approximately
neutral with specialization enabled (+0.0006% cold instructions, +0.0096%
warm). Without specialization it measured +0.258% cold and +0.292% warm in
one before/after pair. This is not evidence of an ARC speedup or proof of
non-regression. The emitted wasm was byte-identical within each strategy:

- Specialized: `b0d63e42c0d90313e9d4e64fc1c7dc499453001d93f31bd9831c2e19bda425ee`
- Boxy: `3ae53a5127f538ec30dbf4de1fc0d4eefe11f490fece5e01fa6262a55dc39f0a`

## Biased values versus reserving slot zero

The [reproducible benchmark](../ci/benchmarks_zig/optional_index/README.md)
uses production columns and validates both logical checksums.

With Zig 0.17.1, 1,048,576 entries, 100 lookup rounds, 25% absent entries,
and three alternating pairs:

| Lookup | Biased instructions | Reserved-zero instructions | Change |
| --- | ---: | ---: | ---: |
| Read the indexed payload | 545,996,002 | 468,470,789 | -14.20% |
| Consume the index | 493,494,059 | 284,071,118 | -42.44% |

Both use 4,194,304 logical column bytes. The payload occupies 6,291,456
logical bytes in the biased variant and 6,291,464 in the direct variant.
Allocator capacity and resident memory are not implied by those logical sizes.
The earlier Zig 0.17.0 experiment found the two direct lookup functions
73 bytes smaller in total; code-size savings are toolchain-dependent.

The checker declaration-slot A/B comparison was effectively neutral:
-0.00495% cold and +0.02265% warm instructions across three pairs.
Reserved zero is selected for that private domain, not for shared checked IDs.
Every parallel array pays for its unused entry; every traversal of actual
declarations skips it. The benchmark does not justify global ID renumbering.

## Merged-tree measurements

These are fresh measurements with Zig 0.17.1, **not storage-only comparisons**
against the older binaries: the merge includes other compiler changes.
One cold run and the median of three warm runs:

| Command | Cold instructions | Warm instructions | Cold RSS KiB | Warm RSS KiB |
| --- | ---: | ---: | ---: | ---: |
| roc-pg check | 209,752,848,549 | 7,541,726,903 | 3,611,188 | 527,004 |
| roc-pg check --specialize=no | 39,800,725,553 | 260,770,469 | 819,008 | 260,572 |
| randomized-spa check | 15,065,453,712 | 3,336,089,498 | 447,488 | 348,044 |

The measured ReleaseFast binary SHA-256 is
`1477ea88669051c927e06796a57627b3f2c6f5428d58e344b3a25bbf9ad7657d`.
roc-pg produced zero errors and 21 warnings (expected exit 2); randomized-spa
produced exit 0.

A separate ReleaseFast `-Ddev-diagnostics` build, cold roc-pg check with
`ROC_STORAGE_WORK=1`, reported:

| Counter | Value |
| --- | ---: |
| Initialization bytes filled by the new primitives | 19,497,358 |
| Growth bytes copied by the new primitives | 16,289,604 |
| Allocator growth requests | 937 |
| Fresh mappings / mapped bytes | 5 / 1,781,760 |
| Reservations / cumulative virtual bytes reserved | 38 / 236,978,176 |
| Commit-ahead calls | 0 |
| Pages trimmed | 34,492 |
| Relocations / transitions to reservation storage | 6 / 30 |
| Epoch resets / wraps | 28,134 / 0 |
| Scratch bytes requested / resets | 286 / 1 |

These counters cover the primitives, not every allocation or memset in the
compiler. Reserved bytes are cumulative virtual capacity, including alignment
tails, **not live virtual usage or RSS**. There is no comparable instrumented
old-container baseline, so these totals alone cannot prove each migration's
kill criterion. Commit-ahead remains disabled; its tuning experiment is open.

## Optional-index, epoch, and write-first conversions, 2026-11-09

ReleaseFast, pinned to the performance cores, 180 invocations on a quiet
machine. The baseline is the tree before the conversions. Corpora: roc-pg,
roc-pdf at its upstream head (which checks without errors), and a generated
1,500-function module that checks without errors. Compiler caches were on
disk. Cold means a fresh compiler cache.

Current tree against baseline, change in instructions (one cold run, median
of three warm runs):

| Workload | Cold | Warm |
| --- | ---: | ---: |
| roc-pg test, specialized | +0.18% | -4.66% |
| roc-pg test, Boxy | +0.01% | -2.16% |
| roc-pg build of `examples/small`, Boxy | +0.09% | -1.13% |
| roc-pdf test, specialized | +0.13% | -4.09% |
| roc-pdf test, Boxy | -0.03% | -4.48% |
| roc-pg check, specialized | -0.28% | +0.03% |
| roc-pdf check, specialized | +0.49% | +0.81% |

Cold build and test are unchanged within noise. The warm build and test rows
are runs of 0.06 to 0.2 seconds. Peak RSS is between 7.4% lower and 1.7% higher.

Four tuning variants were compared against the current tree on `roc check`
only, three alternating cold pairs per corpus, so they are diagnostic and do
not bear on the build/test decision. None moved instructions by more than
0.5% in either direction: the slot kind packed into the slot word, Linux
commit-ahead, small-store thresholds of 64K and 1M entries, and a 25%
reservation estimate. All four settings are unchanged.

Storage work for one cold `roc check` of roc-pg, from the `-Ddev-diagnostics`
ReleaseFast build of the current tree:

| Counter | Value |
| --- | ---: |
| bytes_filled | 86,589,346 |
| bytes_copied | 16,289,604 |
| growth_allocations | 937 |
| mappings / mapped_bytes | 25 / 9,908,224 |
| reservations / reserved_bytes | 38 / 236,978,176 |
| reservation_crossings / relocations | 30 / 6 |
| pages_trimmed | 34,492 |
| commit_calls / zero_resets | 0 / 0 |
| epoch_resets / epoch_wraps | 16,977,168 / 0 |
| scratch_bytes / scratch_resets | 11,568 / 33 |

## Whole-branch comparison with post-edit runs and attribution, 2026-11-10

ReleaseFast with debug information kept, pinned to the performance cores, 168
invocations. The baseline is main at 1e003132f5 merged with the checker
codec-entry and diagnostics repair (477821ff0b) and no storage work; the two
trees differ only in the storage files. The measured tree is 0d1b3f5a07, which
includes the optional-index sweep recorded under "Lifecycle audit". Host
policy as above: transparent huge pages `madvise`, defrag `madvise`,
overcommit 0, no address-space limit.

Corpora, each compiling with zero errors on both binaries: roc-pdf at its
upstream head, and a copy of roc-pg whose `RowFormat` was adapted to the
format-owned parser error rule (operations return `RowFormat.Err`, and the
format has a `missing_field` method). The roc-pg `examples/small` build is
not used: its platform package reports type errors. The build rows are
roc-pdf's `examples/business-report`.

Each row is one cold run, the median of three warm runs, and the median of
three post-edit runs. A post-edit run starts from the warm cache after a new
top-level `expect` is appended to one mid-level module (`Lex.roc` in roc-pg,
`KernelDeflate.roc` in roc-pdf), a different `expect` each time. Change in
instructions, measured tree against baseline:

| Workload | Cold | Warm | Post-edit |
| --- | ---: | ---: | ---: |
| roc-pg test, specialized | -5.00% | -10.37% | -3.82% |
| roc-pg test, Boxy | -2.50% | -8.80% | -1.10% |
| roc-pdf build, specialized | -6.43% | -7.15% | -6.80% |
| roc-pdf build, Boxy | -2.73% | -7.12% | -3.41% |
| roc-pdf test, specialized | -4.75% | -13.25% | -3.93% |
| roc-pdf test, Boxy | -0.40% | -13.32% | -0.08% |
| roc-pg check (diagnostic) | -4.72% | -5.02% | -3.88% |
| roc-pdf check (diagnostic) | -6.87% | -7.69% | -7.19% |

The three post-edit runs of a row differ from each other by under 1% on the
baseline and under 1.7% on the measured tree. Peak RSS is 0.9% to 8.4% lower
in every row. Minor faults are lower in every row except cold roc-pg check
(+3.0%); the Boxy rows drop 15% to 25% post-edit and the warm test rows 37% to
40%. Warm test and warm Boxy build rows are runs of 0.07 to 0.33 CPU seconds.

Attribution from `perf record` with LBR call graphs on the cold specialized
test runs, share of cycles:

| Symbol | roc-pg base | roc-pg measured | roc-pdf base | roc-pdf measured |
| --- | ---: | ---: | ---: | ---: |
| `memcpy` | 7.44% | 4.59% | 4.79% | 2.99% |
| `memset` | 0.49% | 0.56% | 0.33% | 0.36% |
| kernel page zeroing (`kernel_init_pages`) | 1.95% | 2.15% | 0.69% | 0.75% |
| page fault entry | 0.40% | 0.43% | 0.30% | 0.30% |
| all kernel | 7.79% | 8.29% | 4.94% | 5.19% |

Fill time and fault time were under 3% of the baseline together and did not
move. The gain is copy time. By immediate caller, the `memcpy` samples that
disappear are in Monotype lowering's `scopedNode`, `probeInstNode` and
`runInst` and in `CheckedTypeStoreView.canonicalId` (2.6% of roc-pg cycles on
the baseline, 0.2% on the measured tree; 1.3% to 0.1% on roc-pdf). Those
functions call the checked type store view's key and canonical-id methods,
which take the view by pointer on this branch and took it by value, copying
the view on every call, on the baseline. No other caller's `memcpy` share
moves by more than 0.06%, so growth copies in the converted tables are not a
measurable part of either profile.

Storage work for cold roc-pg runs of the measured tree, from the
`-Ddev-diagnostics` ReleaseFast build:

| Counter | check | test, specialized |
| --- | ---: | ---: |
| bytes_filled | 89,981,721 | 158,520,329 |
| bytes_copied | 16,207,716 | 16,207,716 |
| growth_allocations | 933 | 933 |
| mappings / mapped_bytes | 25 / 9,908,224 | 25 / 9,908,224 |
| reservations / reserved_bytes | 38 / 236,978,176 | 38 / 236,978,176 |
| reservation_crossings / relocations | 30 / 6 | 30 / 6 |
| pages_trimmed | 34,492 | 34,492 |
| epoch_resets / epoch_wraps | 16,977,172 / 0 | 27,989,383 / 0 |
| scratch_bytes / scratch_resets | 11,568 / 33 | 11,568 / 33 |

`bytes_filled` counts only fills made through the shared primitives, so it
rises as tables move onto them.

## Lifecycle audit

| Area | Decision and reason |
| --- | --- |
| Type-store slot, descriptor, rank, root, variable, record and tag columns | Reserved storage; append writes, rollback, allocation failure, sealing and serialization are tested. Declared serialization order changes cache admission. |
| Sealed-key identity bytes | Sparse storage for large domains, recycled allocation plus fill for small domains; atomic reads/writes unchanged. The separately developed key-slot backend is not replaced. |
| Frozen Monotype visit state | Fresh sparse stamps only above the shared small-table threshold; small domains stay on the allocator. |
| Checker SCC tables | Once per graph, read-first. Keep sentinel initialization; use retained scratch for the migrated callers. |
| Checker declaration slots | Compact direct indices in a private one-based domain; shared IDs retain their numbering. |
| Request scheduling, reachable-procedure/static-data remaps, interpreter frame lookup | Compact typed optional columns. |
| Formal masks, containment visits, ARC node/loop visits, ordered-use searches | Shared epoch/bitset reset rules; retained capacities do not imply fresh zeros. |
| ARC mention/root/write groups | Contiguous dense grouping with explicit insertion order. |
| ARC liveness seed/node/loop groups and place/benefit groups | Sorted sparse grouping; no table proportional to the entire key domain. |
| Range-proof sum chains | Keep chains: sums are linked and queried during the evolving analysis, not built once then traversed. |
| Interpreter frame values | No release fill; safety-build assignment checks and zero-sized value initialization remain. |
| Dominance `order` / `idom` | Keep discovery sentinels, which are read before assignment. |
| Body-analysis node kinds/flags | Unreached entries are meaningful; not whole-table write-first outputs. |
| Body-analysis merge/mutation spans | Retained output indexed within a body, not reset-many scratch. Span payloads are not single indices. |
| Codec identity `component` / `visit_index` | One SCC walk of a derivation graph, not per-root resets. |
| Type-key `local_globals` | Dense partition block IDs, all assigned and copied into `block_existing`; an epoch does not remove that dense copy. |
| Layout `keys` / `digests` / `redirect` | Digests or tagged canonical/local references, not single u32 indices. |
| Boxy `dicts` / `composed` operands | Tagged operands, not single indices. |
| Padding, return buffers, static data, image bytes, broadcast fills | Preserve required bytes. |

The source-level semantic lint resolves file-local storage aliases and
declarations. It rejects fills through those owners and optional allocations
for named index types with an existing compact column in that file. It does
not perform cross-file Zig type inference or prove lifetimes.

The optional-index sweep is complete: every table whose entries are a single
typed index is a four-byte column or a slice of four-byte optional ids, in
Boxy, Monotype, solved lowering, static data, compile-time finalization, the
checker, the parser and the CLI test planner. The tables that keep an
optional or sentinel form hold pointers, structs, spans, strings, tagged
operands, addresses or flag bytes, or are append-only stacks and lists never
looked up by index. The statement and projection sentinel tables in
`arc_dismantle.zig` are four bytes already and filled once per pass.

roc-pdf's Unicode iterator compatibility fix was validated in an isolated
copy, not persisted to its repository. Observatory's saved baseline has 77
errors and is not a successful workload. The synthetic 20,000-function source
is valid, but its type-variable count has not established the proposed
million-variable stress threshold.

## Validation

Before merging main: full snapshots/Zig checks passed (8,232 passed, 14
skipped); the ReleaseFast strategy differential suite completed 2,624 cases;
378 CLI crash cases passed. Those results do not validate the merged tree.

After merging main, with Zig 0.17.1:

- Tidy, Zig lints, semantic audit (20 new lint cases), unreachable rationales,
  native/wasm serialization sizes, and snapshots passed.
- Focused collections/base/types/check/eval suites: 2,656 passed, 2 skipped.
- ReleaseFast compiler and diagnostic compiler builds passed.
- Full Zig run: 8,205 passed, 14 skipped, 12 failed, 81 crashed; one additional
  test executable failed compilation against the changed `DynOp.adapt` API.
  That fixture now consumes an explicit plan argument; its 19 TRMC tests pass.
- Remaining integration failures include missing static-data image addresses,
  incomplete Boxy root values, range-proof expectations, and synthetic fixture
  key-slot/ownership invariants. No assertion was disabled to hide them.
- Native Windows/macOS execution and the full compiler-WASM suite have not
  been run. Earlier cross-compiles and the custom wasm storage smoke are
  narrower checks, not substitutes.

Do not merge this draft until the merged-tree integration failures and the
remaining implementation/performance gates are resolved.
