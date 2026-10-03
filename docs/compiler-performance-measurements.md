# Flagged compiler performance measurements

## Status

**The revised all-on suite is net beneficial on every measured main workload.**
Cold checks improve by 49–57%; unchanged warm builds improve by 92–98.5%; a genuine
app-only edit improves by 81%. The initial suite failed cold acceptance, and that
failure is retained below rather than concealed. Profile-guided, independently
validated inhabitedness witness pruning removes its cold regression.

Recommend the revised suite for broader rollout acceptance, not the saved
initial all-on binary. These results establish the combined configuration's
benefit on these workloads, not every flag's isolated benefit or universal
performance/correctness.

This document preserves the combined-suite study. The subsequent final-source
all-on versus one-flag-removed study is in `compiler-performance-flag-ablations.md`.
It resolves the final marginal-comparison gap discussed below, without replacing
these measurements or claiming a full factorial interaction study.

## Protocol

Compare fresh ReleaseFast binaries from identical current compiler sources,
with the rollout gates disabled versus `-Dperf-all`. Every Zig build uses
`-j2`; every Roc workload uses `--jobs=2`; all runs are serialized. Keep Zig
cache directories and installation prefixes under `.tmp` with names other than
`.zig-cache` and `zig-out`, avoiding the periodic generated-directory cleanup.

The runner is `.tmp/compiler-performance-measurements/measure.py`. Each invocation
creates a unique run directory containing command metadata, complete stdout,
complete stderr plus `/usr/bin/time -l` wall/RSS output, and exit status. Successful
small-app builds also retain executable stdout/stderr and their hashes.
Separate `ROC_CACHE_DIR` roots belong to each source/compiler pair. Cold checks
use `--no-cache`; warm builds require two successful warmups before measurement.
Checked-module hits do not establish object reuse: report early specialization
hits, body contexts, and stage timings separately.

Measurements ran on macOS arm64, a 10-core machine with 32 GiB RAM, using Zig
0.16.0 and ReleaseFast. This is a shared machine, not a controlled laboratory.
Cold means compiler artifacts disabled, not flushed filesystem caches. Three
replicates per main row are reported as median wall seconds with observed range;
RSS is the median `/usr/bin/time -l` maximum resident set size, in MiB. Off/all
ordering was reversed in the second round. The no-views follow-up was collected
later, so its comparison is not simultaneously randomized with the primary pair.
Stage times are nested and must not be added to the outer type-checking time.
The small-app output is a new owned path on every invocation.

The app accepts a database URL. Running without arguments validates only the
usage path, not database interactions or generated query correctness.

## Inputs

The read-only original, selected inline-null refactor, and shared-null roots are
listed in `compiler-performance-rollout.md`. Immediately before runs, the runner
verifies Actions, Node, and small-app hashes against the retained identities.
Initial verification passed for all three copies.

The parent compared this checkout against the integrated compiler across 2,466
source paths and reported zero differences. After the worker explicitly released
the window, the parent authorized synchronization of three already-reviewed
proof test/documentation additions. Their six-file final hashes were verified
before building. No compiler production behavior was changed by this worker.
The source manifest is retained as `compiler-source-manifest.json`.

| Binary | Gates | SHA-256 |
|---|---|---|
| `roc-off` | All disabled | `661ca9c1422e16cd735c98d397049f2981dd3d6331256ac9253aa00e33f42471` |
| `roc-all` | `-Dperf-all` | `7a57dd4612b5cebc59bb0155a151ca59cc5ed64ad6df8d941de03fd255bbdb52` |
| `roc-no-views` | All, `-Dperf-nominal-views=false` | `4c5052adfd1499f9e09bc4222a1118b736e49aa66274f661aac4aed8b3210990` |
| `roc-no-memo` | All, `-Dperf-inhabitedness-memo=false` | `b27a3e3a042989ee641395ec9d892fe87a2712fb04a650890af1984aa4a148f2` |

## Revised suite: final paired acceptance

The correctness worker supplied a validated witness-pruning revision, which the
parent reviewed and authorized for deliberate synchronization. Among the 2,466
manifest paths, **only `src/check/exhaustive.zig` differs** from the saved initial
compiler: SHA-256 `ab1aea43ada9dbb86e37f768d944a97e4819d65f750d949d2f9919ea20ce0efb`.
The corresponding design proof has SHA-256
`da469d4a5d9c9d9bd04eefafe1055859d22011ca6b54a3095be77debcd5f6de0`.
This worker synchronized that exact source, without implementing another change.
The complete revised source manifest is retained separately.

Fresh revised off/all binaries come from identical sources and differ only in
build gates. Revised off is byte-identical to saved off, SHA-256
`661ca9c1422e16cd735c98d397049f2981dd3d6331256ac9253aa00e33f42471`.
Revised all SHA-256 is
`659e880eb0b5bebe0d6f772e43eed13e5d6426e22b91106decd9598086573f61`.
Every measured cold and warm pair was repeated three times; warm caches were
fresh and separately primed twice. These are the final acceptance rows:

| Workload | Off wall median [range], s | Revised all wall median [range], s | Off / all RSS, MiB | Wall reduction |
|---|---:|---:|---:|---:|
| Cold original check | 62.05 [61.89–63.66] | 31.54 [31.06–32.66] | 8570 / 6106 | 49.2% |
| Cold inline check | 44.03 [43.82–44.45] | 19.10 [19.02–19.15] | 5106 / 4208 | 56.6% |
| Cold shared check | 44.06 [43.85–45.01] | 19.24 [18.97–19.87] | 5069 / 4125 | 56.3% |
| Warm original build | 32.66 [32.30–32.77] | 1.81 [1.41–1.81] | 10400 / 1115 | 94.5% |
| Warm inline build | 17.71 [17.68–17.74] | 1.42 [1.42–1.42] | 5928 / 1041 | 92.0% |
| Warm shared build | 90.39 [90.30–91.17] | 1.40 [1.40–1.50] | 5689 / 1007 | 98.5% |
| App-edited inline build | 22.05 [22.02–22.09] | 4.21 [4.21–4.22] | 6459 / 2067 | 80.9% |

The original cold revised-all first run was paired with saved initial all
and saved no-views drift controls: **32.66 / 299.76 / 36.15 s**. The latter two
agree with their earlier medians, so elapsed-time improvement cannot reasonably
be attributed to machine drift. Revised all also beats the earlier no-views
medians on inline/shared (19.10/19.24 versus 26.41/25.20 s).
Those no-views comparisons use the preserved initial-source binary, not a new
same-source no-views build; the fresh final off/all pair is the acceptance basis.

Cold shared-stage medians remain essentially unchanged: off/all original
24.577/24.584 s, inline 15.827/15.789 s, shared 15.814/15.964 s.
Witness pruning fixes checking overhead, not cold evaluator execution. After
the correction, shared lowering/CTFE is again the dominant cold residual.
Warm shared-stage medians are original 29.051→1.237 s, inline
17.239→1.238 s, shared 89.921→1.230 s. Their revised-all Monotype medians
are approximately 1.11 s. The former approximately 25 s warm CTFE cost is
therefore removed as repeated body work, not merely renamed or hidden.

All final unchanged builds report 61/61 checked hits; all final app-edited builds
report 60/61. Revised all has 339 early shared specialization hits unchanged
and 503 after app edits. Representative third-run body contexts are
284012→42118 original, 210703→42128 inline, 210834→42088 shared,
and 242240→68157 app-edited inline. Eligibility remains proof-gated and its
full denominator is not exposed; these are neither “all objects reused” claims
nor eligibility percentages.

The final off/all parser runs each pass **383/383 expectations** (52.81/26.08 s
reported test duration), with caches separate from timing measurements. All
18 final small checks pass with the same 0.01/0.06/0.02 s control medians as
before; all final four/five Boxy/JSON expectations pass. Across both study
rounds, every one of 148 main/warmup/ablation/edit records exits zero, and all
100 built-app usage executions have identical stdout and empty stderr.
The correctness worker separately validated the compiler checker/snapshot matrix;
those are reported in the rollout document, not substituted for these workflows.

After five warm builds, final off/all aggregate cache bytes are original
2,473,871,174 / 339,101,638; inline 1,081,667,357 / 294,704,941; shared
1,061,862,505 / 274,900,089 (166 files in each root).
Memory and artifact reductions accompany, rather than excuse, the wall-time gains.

**Flag assessment:** early reuse has direct body-skip and end-to-end evidence;
revised nominal views have a successful causal repair and favorable combined
cold comparisons. Memoization mitigated the initial views cost, but was not
isolated after pruning. Individual benefits of projection, scratch, completion,
and post-pruning memoization remain unproven; none is declared universally
beneficial from one counter. No flag is demonstrated net negative in the final
measured suite, but a complete individual ablation matrix was intentionally not
run. Shared-null is repaired for both measured cold and warm workflows; cold
lowering and cache-ineligible behavior still warrant future profiling.

## Initial suite: retained diagnostic baseline

### Cold package check

Command: `roc check package/main.roc --no-cache --jobs=2 --timings`.

| Source | Gates | Wall median [range], s | RSS, MiB |
|---|---|---:|---:|
| Original | Off | 61.72 [61.38–65.32] | 8482 |
| Original | All | 297.67 [297.54–299.62] | 6128 |
| Original | All except views | 36.03 [36.00–36.24] | 6994 |
| Inline refactor | Off | 44.19 [43.83–45.21] | 5083 |
| Inline refactor | All | 120.17 [119.90–122.10] | 4215 |
| Inline refactor | All except views | 26.41 [26.17–27.22] | 4516 |
| Shared null | Off | 43.62 [43.52–44.02] | 5078 |
| Shared null | All | 121.05 [119.69–121.99] | 4121 |
| Shared null | All except views | 25.20 [25.05–25.30] | 4500 |

The cold shared lowering/CTFE stage remains roughly unchanged: original
24.52/24.51/24.26 s, inline 16.14/15.71/16.38 s, shared
15.53/15.51/15.60 s (off/all/no-views medians). The large all-on
regression is elsewhere within checking, not CTFE execution. Reduced RSS does
not compensate for the speed regression.

### Warm small-app build

Command: `roc build examples/small/main.roc --opt=dev --jobs=2 --timings
--verbose --output=<unique-owned-path>`. Every measured row reports **61/61
checked module hits**, zero modules built, and 61 canonical cache hits.

| Source | Gates | Wall median [range], s | RSS, MiB |
|---|---|---:|---:|
| Original | Off | 32.77 [32.52–33.47] | 10472 |
| Original | All | 1.79 [1.43–1.84] | 1087 |
| Original | All except views | 1.69 [1.68–1.69] | 2859 |
| Inline refactor | Off | 18.93 [18.81–18.98] | 5917 |
| Inline refactor | All | 1.71 [1.69–1.75] | 1043 |
| Inline refactor | All except views | 1.51 [1.50–1.52] | 1659 |
| Shared null | Off | 91.87 [91.84–91.90] | 5717 |
| Shared null | All | 1.70 [1.69–1.73] | 1004 |
| Shared null | All except views | 1.51 [1.51–1.51] | 1629 |

The historical approximately 25 s “CTFE” warm cost was predominantly repeated
lowering, not evaluator execution. Current off original shared-stage median is
28.98 s, including 27.02 s Monotype lowering, with zero execution/store time.
All-on reduces these to 1.239/1.110 s. Inline drops from
17.205/15.492 s to 1.217/1.095 s; shared null drops from
90.176/88.528 s to 1.225/1.100 s. No-views retains the same approximately
1.2 s shared-stage cost.

Representative third-run shared counters provide independent body-skip evidence:

| Source | Off early hits / body contexts | All early hits / body contexts |
|---|---:|---:|
| Original | 0 / 284015 | 339 / 42139 |
| Inline refactor | 0 / 210685 | 339 / 42106 |
| Shared null | 0 / 210803 | 339 / 42103 |

These are early shared-Monotype specialization hits, not module hit counts.
They do **not** establish that every specialization is eligible, or that all
native objects are reused. Eligibility is proof-gated; the timings output does
not expose a complete eligible/rejected denominator. Residual shared lowering,
graph construction, closure lifting, specialization, and linking still occur.
For example, the inline all-on third run creates 1,510,742 shared graph nodes
and 42,106 body contexts despite the 339 early hits.

Shared-null's **warm rebuilding pathology is removed on this workload**, to the
same measured wall time as inline nulls. This is not proof that every cold or
cache-ineligible shared-null program is improved. Indeed, the shared off second
warmup worsens from the initial cold-build 70.46 s to 91.14 s after checked
constants are stored, confirming why a single warmup would mischaracterize it.

### App-only edit

`edit-case.py` copies the complete inline fixture into owned `.tmp` directories,
warms twice, then changes only an app comment before each of three measured
builds. All package `.roc` hashes are verified unchanged after every invocation.
The edit is a semantic no-op but a genuine app cache invalidation.

| Gates | Wall median [range], s | RSS, MiB | Third-run early hits | Third-run body contexts |
|---|---:|---:|---:|---:|
| Off | 21.07 [21.06–21.19] | 6441 | 0 | 242246 |
| All | 4.02 [4.01–4.08] | 2063 | 503 | 68145 |
| All except views | 4.15 [4.13–4.18] | 2687 | 503 | 68143 |

Every edited build reports **60/61 checked hits and one module rebuilt**.
The all-on third run spends 3.232 s in shared lowering/CTFE, 0.402 s specializing,
and 0.237 s in the native backend. App recompilation genuinely costs more than
unchanged checked replay, but early hits still remove most repeated body work.

### Correctness and small controls

`roc test package/main.roc --jobs=2` passes all **383 expectations**, independently
with off, all, and no-views compilers, using separate correctness caches. Reported
test durations are 50.12/125.20/31.42 s; these are correctness workflows, not part
of the benchmark medians.

Three existing CLI sources were checked three times with each compiler:
`BoxyProjectedNominalConstruction`, `JsonNestedNominalContract`, and
`ProjectedNominalLoopCondition`. All 27 checks pass. Median wall times are
respectively 0.01/0.06/0.02 s for all three configurations. At this timer precision
they show no detectable global overhead, not a statistical proof of zero overhead.
The first two sources additionally pass four and five runtime expectations with
every compiler. The loop source contains zero expectations, so its `roc test`
success is only compilation evidence.

All successful small-app invocations exit zero with identical usage output:
stdout SHA-256 `8caa21bec334f2978fd2ebb44209f57700cbbd159f1de69adffdd190c61e6140`,
empty stderr. No PostgreSQL integration execution was performed.

### Causal ablation and profile

The minimal cold inline ablation disables one gate at a time:
all-minus-views 27.22 s; all-minus-memo 149.66 s (one replicate for this diagnostic
row). Views cause the dominant suite regression here; memoization mitigates it
rather than causing it. No claim is made about memoization in isolation.

A separate ReleaseFast all-on **unstripped** binary was built solely for sampling,
SHA-256 `c991f0832e015fc148c5590e74a8ecf496455a1aa0392f432a60628bf215f7b5`.
Its cold inline check was sampled for 20 s at approximately 25 s after start.
The profile is excluded from benchmark medians. Leaf samples include
`InhabitedGraph.rowNode` 3987, `typeNode` 1141, row-key map growth 935,
`solveInhabitedGraph` 714, `type_view.view` 382, dependency construction 19,
environment interning 72, and projection 6. Hashing also features prominently.
This sampling window supports repeated inhabitedness graph/row work as the
dominant target, not dependency initialization as the dominant sampled leaf.
It is not a whole-run percentage attribution; sleeping threads and recursive
inclusive counts must not be interpreted as CPU shares.

### Artifact cost and rollout recommendation

After equal five-build warm inline workflows, aggregate cache file bytes are
1,081,667,357 off, 294,704,941 all, and 623,725,021 no-views (166 files each).
These include all cache artifacts, not just serialized type variables or objects.
Lower memory and artifact volume are real benefits but separate from elapsed time.
The current output does not report checker query counts/typevar growth, so those
cannot be inferred from RSS or Monotype graph counters.

The initial recommendation was not to ship that `perf-all` configuration and
to retain all-minus-views as the measured candidate. That blocker is resolved by
the revised acceptance above, not by deleting these failed measurements.
Early CTFE reuse has positive end-to-end and body-skip evidence.
Constructor/tag projection, settled scratch, completion, and memoization have
combined-suite coverage; their individual net benefit remains unproven because
this deliberately avoids a 128-configuration factorial experiment.

## Reproduction and retained evidence

All evidence is under `.tmp/compiler-performance-measurements/`:

- `build.sh`, `matrix.sh`, `ablate.sh`: exact build gates and serialized main runs.
- `measure.py`, `edit-case.py`, `controls.py`: full logs and owned fixtures.
- `revised-build.sh`, `revised-matrix.sh`, `parser-expectations.py`: final
  clean builds, serialized paired matrix, and independent parser controls.
- `summarize.py`, `measurements.csv`: 148 raw primary/ablation/edit/warmup records.
- `binaries/`, `runs/`, `correctness/`, `controls/`: preserved executables,
  command JSON, hashes, complete stdout/stderr, RSS, and exit status.
- `complete-text-evidence.jsonl`: 1,064 complete text artifacts with individual
  hashes, including every run's full logs, avoiding dependence on replication
  of generated directories. Binaries and caches remain machine-local.
- `profile-inline-all-symbols/`: symbolized sample and profile command; the first
  stripped sample is separately retained, not used for function attribution.

No Git diagnostics, historical-input edits, source-specific workarounds, or
weakened expectations were used. Disk availability initially blocked safe long
build planning; the proof worker reclaimed only completed disposable artifacts,
after which 78 GiB was available. No ENOSPC occurred in these measurements.
Both correctness-to-measurement handoffs were explicit. After final acceptance,
the heavy window was explicitly released to the parent; no build or benchmark
process remains active in this checkout.
