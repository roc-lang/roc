# Compiler cache follow-up results

## Outcome

Two cache trials were implemented and remeasured. Neither establishes a
convincing additional SQL wall-time improvement over the seven-feature
checkpoint bundle. Higher-order reuse passes its individual correctness
acceptance; finalized literal reuse works for its narrow successful,
observation-free class but fails full off/on observational equivalence.
Both remain default-off experimental.

The requested lazy nominal-row optimization is **unfinished**. Its persistent
storage and semantic baselines are tested, but no production descriptor,
admission or consumer path is installed. Supporting APIs and prepared tests
are not an optimization result.

No combined rollout is accepted. Boxy caching remains out of scope.

## Rollback checkpoint and publication

Both repositories were committed and pushed on
`sql-parser-cache-checkpoint-20261003-185309` before the follow-up trials:

- Roc: `9baf864b2cddd6cd87009ac6d7655b5a61b0db33`.
- roc-pg: `287f87939223dd2d36c2841cf1683dbaea084628`.

Those refs were not moved. Follow-up source, tests and reports are in the
worktrees; the parent did not commit or push the follow-up experiments or open
a PR. Generated caches and large diagnostic logs are not source deliverables.

## Measurement rules

Each trial uses its own frozen, same-source ReleaseFast off/on compilers.
The baseline is mask 127: all seven checkpoint features enabled. Higher-order
on is mask 639; literal on is mask 383. These are separate experiments, not
stock-Roc comparisons or a combined feature configuration.

App builds use `--opt=dev`. Commands run serially with `zig build -j2` and
Roc `--jobs=2`, fresh caches, alternating order, two discarded warmups and three
measured repetitions. App edits preserve package hashes. Native output is
checked exactly. SQL runtime checks exercise the usage path, not a live
database. Failed runs and diagnostic traces do not contribute to timing medians.
Outliers remain in the distributions.

## Higher-order reuse

The later key combines explicit reservation provenance with the completed
procedure identity. It reuses the existing portable plain-ABI admission rather
than treating an arrow type as complete callable identity.

The first full SQL build exposed a real producer error: representation-only
and inlined `List.map` reservations were published as specialization roots
without standalone bodies. The correction defers publication until canonical
code demand and implementation completion are established. Inlining, existing
reachability and native assertions remain unchanged.

The old compiler fails the compact regression; the corrected compilers pass.
All three SQL input variants pass cold/warm/app-edit/uncached workflows and
all six 383-expectation runs. There are 231 successful corrected measurement
records.

| Input | Cold off/on seconds | Warm off/on seconds | App-edit off/on seconds |
|---|---:|---:|---:|
| Original | 52.74 / 52.57 | 1.48 / 1.48 | 4.19 / 4.18 |
| Refactored | 31.95 / 31.96 | 1.48 / 1.47 | 4.25 / 4.24 |
| Shared-null | 40.38 / 41.42 | 1.46 / 1.46 | 4.16 / 4.14 |

Focused helper reuse skips one LIR body and reduces native procedures 3 → 2,
bytes 1,264 → 992. Returned-closure reuse skips two, reducing procedures 5 → 2,
bytes 1,520 → 1,008. Their medians remain 0.03 seconds in both configurations.
Preceding Monotype work is unchanged.

SQL app-edit native procedures decrease 754 → 727, but elapsed-time differences
are noise-scale. Original cold peak-RSS medians increase
8,926,396,416 → 9,509,011,456 bytes, approximately 6.5%. SQL packs save
71,063 bytes per variant. These costs and small savings do not justify enabling
the feature by default.

Full evidence: [higher-order report](compiler-performance-late-callable-cache.md).

## Finalized literal reuse

The implementation retains exact read-owner provenance and completion facts.
Only already-completed early owners whose entire literal set is successful,
non-callable, observation-free and portable can request cache-publication code.
It preserves caller inlining and completes requested bodies before offering
them. Existing converter and program-symbol filters were not relaxed.

This required fixes at clone/view ownership boundaries, explicit current-source
certificate validation, semantic identity equality, versioned admission, and
transactional lazy-reader readiness. Feature-on packs use v6 and identity domain
3; gate-off v4 and the checkpoint domain remain unchanged. Required development
packs remain strict; an unavailable ordinary cache exposes no partial offers.

The actual named `PlainValues.pair` workflow passes in Debug and ReleaseFast:
certified hits, reduced owner-body work, zero warm literal evaluations, exact
output after app edits, same-path caller-order reversal and converter edits.
There are 160 successful RF commands, 130 exact native-output checks and
30 package checks.

| Input | Cold off/on seconds | Warm off/on seconds | App-edit off/on seconds |
|---|---:|---:|---:|
| Original | 50.05 / 50.21 | 1.43 / 1.44 | 4.07 / 4.08 |
| Refactored | 31.40 / 31.08 | 1.44 / 1.43 | 4.26 / 4.20 |
| Shared-null | 41.78 / 41.86 | 1.47 / 1.49 | 4.16 / 4.19 |

Tiny literal medians are 0.04/0.04 seconds uncached, 0.06/0.06 cold,
0.03/0.03 warm and 0.04/0.03 app-edited. The last difference is one printed
timer quantum, not a robust 25% speedup.

Cold native executables grow by 17,568 bytes for each SQL variant. Original
native packs grow 33,939 bytes; refactored/shared packs grow 32,710 bytes.
Extra standalone code and proof/publication work are included in measurements.
Shared-null cold RSS has a higher on median with overlapping, variable ranges;
the report does not invent an isolated allocation cause.

**Full observational equivalence is rejected:** observed converters print four
debug messages in both cold configurations, but fully warm prints zero off and
four on. The ordinary folded-entry path loses observer attribution and caches
`main!`; explicit on-feature provenance refuses it. Observed entries have no
certified hits. No debug suppression or filter relaxation was applied to hide
the difference. A common observer eligibility or persisted-replay contract is
still required.

Always-rejecting converters and failed certificates remain unsupported; this
does not change the language's report-and-continue/runtime-crash policy.

Full evidence: [literal report](compiler-performance-finalized-literal-cache.md).

## Nominal-row attempt

Persistent opening/fragment storage, serialization, rollback, ownership and OOM
tests were implemented. The accepted Debug/ReleaseFast × gate-off/on matrix
passes 17 types and 17 checker tests per configuration: 136 executions.
Subsequent binding-delta and contextual-copy controls pass 19 types and
20 checker tests in Debug gate-off only.

Key-only import correspondence was rejected: contextual alias replacement can
discard immutable wrapper information and has different template/owned
freshening semantics. Rank alone also cannot establish immutable ownership.
Prepared characterization exposes the proposed unchanged capture path's risk
of writing a nongeneralized schema tail through an owned reference, affecting
another opening. That characterization is not counted as an executed regression.

The immutable producer and complete logical copy/rank/generalization/import/
consumer integration were not completed. No nominal speedup or SQL admissibility
count was measured. Version 108 and its validated serialization golden remain;
speculative version 109 and placeholder admission were removed.

Full evidence: [nominal attempt](compiler-performance-lazy-nominal-rows.md).

## Verification and infrastructure corrections

- Some initial filters selected zero tests or only a harness. Their pass claims
  were withdrawn. Explicit discovery and selected-name/count checks then ran
  the actual tests; fixture lifetime and allocator errors were repaired.
- A 12.46 GB traced log left Python regex processing busy for over three hours
  after Roc had finished. Only that postprocessor was stopped. Streaming,
  bounded parsing and command deadlines replaced the unbounded processing.
- A broad 707-step install attempt timed out and is excluded. Later execution
  uses narrow targets, configured-prefix compiler hashes and a verified native
  host dependency.
- Context-window failures required source recovery and fresh acceptance rather
  than assuming donor results applied to the recovered checkout.
- A transient worktree-materialization error prevented execution, then ceased
  reproducing. No unsupported attribution or alternate-checkout editing was used.
- The real independent artifact-byte test passes both x64 and AArch64 loops
  after discovery/fixture repairs. This verifies a codegen/extraction boundary,
  not full compiler reproducibility. No production address-normalization defect
  was found.

## Recommended disposition

Keep the published seven-feature checkpoint as the established result.
Do not enable either new cache gate by default: higher-order reuse has little
SQL payoff and a memory cost; literal reuse has narrow valid hits but an unresolved
observation contract and no convincing SQL time benefit.

Do not treat the nominal foundation as the requested optimization. A further
implementation decision is needed before broad solver mutation or rollout.
No combined/default-on rollout is claimed.
