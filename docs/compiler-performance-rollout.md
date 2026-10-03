# Compiler performance rollout

## Status

**The revised suite is ready for flagged PR review. All seven gates remain
opt-in and default off; no PR has been opened or merged.** Targeted correctness
gates and clean whole-workload acceptance pass. This is not a claim of exhaustive
compiler correctness, cross-platform validation, or every flag's isolated benefit.
The detailed protocol, phases, memory, artifact sizes, rejected initial results,
and reproducible evidence are in `compiler-performance-measurements.md`.

The subsequent final-source **all-on versus exactly one flag removed** matrix is
complete in `compiler-performance-flag-ablations.md`: eight fresh binaries, 430
raw runs, and all 383 parser expectations passing in every configuration.
Nominal views, constructor projection, and early CTFE reuse are strong marginal
wins. Memoization has a modest cold benefit; tag projection and settled scratch
are approximately neutral; constant completion's marginal elapsed-time benefit
is unproven. No meaningful net-negative component is established, but not every
component is a demonstrated speed win. Keep the suite under the requested
retain-unless-negative rule, with neutral components justified separately by
engineering value rather than a claimed timing improvement.

These marginal comparisons are conditional on the other six gates remaining
enabled. They close the final per-flag comparison gap, not the full factorial
interaction or universal workload question.

### Final paired outcome

Three-replicate medians from fresh same-source ReleaseFast off/all builds:

| Workload | Flags off | Revised all flags |
|---|---:|---:|
| Original parser, cold check | 62.05 s | 31.54 s |
| Refactored parser, cold check | 44.03 s | 19.10 s |
| Shared-null parser, cold check | 44.06 s | 19.24 s |
| Original parser, warm build | 32.66 s | 1.81 s |
| Refactored parser, warm build | 17.71 s | 1.42 s |
| Shared-null parser, warm build | 90.39 s | 1.40 s |
| App-only edited refactored build | 22.05 s | 4.21 s |

The original warm build's median peak RSS falls from 10,400 to 1,115 MiB.
App-only edits retain 60/61 checked hits, produce 503 early object-specialization
hits, and reduce body contexts from 242,240 to 68,157 in the representative third
run. Unchanged builds retain 61/61 checked hits and approximately 1.2 seconds of
shared lowering/CTFE work. This is demonstrated body reuse, not merely replay of
an unchanged application's completed constants.

All 148 measurement records succeed and all 100 executable usage-path outputs
match. Both final configurations pass 383 parser expectations and the small
source/Boxy/JSON controls. Live database integration was not rerun for these
compiler changes. The combined suite is beneficial on every measured main
workflow. That initial study did not include final marginal ablations; the
subsequent eight-configuration study above now supplies them.

### Four requested investigations

1. **Shared `Node.null`:** native profiling identified completion-relation
   active-path hash probing after tombstone accumulation, plus repeated recursive
   inhabitedness. The container fix preserves cycle semantics without caching
   provisional answers. Early reuse also removes repeated package-body work;
   the measured cached shared-null workload falls from 90.39 to 1.40 seconds.
2. **Constructor representation:** fresh producer-owned single-tag construction
   checks declaration membership and selected payload obligations without cloning
   unrelated variants. The nominal remains the full representation authority.
   Monotype and Boxy consumers now honor that explicit edge, including Bool,
   nested/opaque payloads, descriptor ownership, and pattern miss analysis.
3. **Object-cache timing:** eligible hits occur before Monotype body lowering,
   under the same compile-time observation proof as Direct LIR. Literal,
   program-local, signature, identity, ABI, and relocation restrictions remain.
   Some requests remain ineligible; these results do not claim zero lowering or
   reuse of every object.
4. **Fable productionalization:** memoization, immutable nominal views, selected
   constructor/context projection, and scoped settled scratch are retained behind
   gates, alongside the completion-container and early-cache fixes. Review found
   lifetime, recursive/shared-DAG, record-tail, and downstream representation gaps;
   those were corrected and pinned. A serious cold regression was profiled and
   fixed with exact unconditional-witness pruning rather than accepted as a
   memory/speed tradeoff.

Independent reviews found no concrete remaining defect within their stated
scopes. The final parent base/context/postcheck matrix passes 795/795 tests in
Debug with all gates on and off (159 base, 14 context, 622 postcheck each).
Full checker Debug/ReleaseFast on/off, the 80-snapshot gate, and the 104-case CLI
matrix provide complementary coverage below. Broader CI and platform validation
remain the next rollout step, not evidence already obtained.

### Rejected initial configuration and causal repair

The first clean three-replicate matrix shows warm-build gains but a blocking
cold-check regression. The benchmark worker reports all 48 runs succeeded:

| Workload | Flags off median | All flags on median | All except nominal views |
|---|---:|---:|---:|
| Original parser, warm build | 32.77 s | 1.79 s | 1.69 s |
| Refactored parser, warm build | 18.93 s | 1.71 s | 1.51 s |
| Shared-null parser, warm build | 91.87 s | 1.70 s | 1.51 s |
| Original parser, cold check | 61.72 s | 297.67 s | 36.03 s |
| Refactored parser, cold check | 44.19 s | 120.17 s | 26.41 s |
| Shared-null parser, cold check | 43.62 s | 121.05 s | 25.20 s |

These are retained measurements of the rejected pre-pruning implementation, not
the final shipping recommendation. Reduced memory did not offset its cold-time
failure. Disabling nominal views provided a useful interim configuration and
isolated the regression; the revised all-on suite above subsequently passed
acceptance.

The app-only-edit control deliberately invalidates the app while retaining
**60/61 checked-module hits**. Its measured times are 21.07 s off, 4.02 s all-on,
and 4.15 s without views. Early object hits are respectively **0 / 503 / 503**;
Monotype body-context counts are **242,246 / 68,145 / 68,143**. This demonstrates
actual early object reuse and reduced body work, not just unchanged-app replay.
It does not establish that every remaining body is independently cacheable.
All **383 parser expectations** pass in each configuration; Boxy and nested-JSON
controls also pass. Small source checks are indistinguishable at the timer's
0.01-second precision. Usage-path executable outputs match; live database
integration was not exercised in these compiler measurements.

Source-only review identifies possible cold-cost multipliers, not a measured
cause: reader/dependency setup repeats at each exhaustiveness site; absent-payload
collection and the following match can use separate reader lifetimes; fixed-point
graphs are rebuilt per query; and decoded union/tag scans are not shared merely
because nominal opening is cached. The graph-linear proof is per query, not per
module. Profiles and gate ablations must distinguish these costs before choosing
an optimization. Reuse must remain within a mutation-free session with exact
mode, assumptions, and reader identity; caching recursive cutoffs is not an
acceptable shortcut.

The first causal ablation isolates nominal views: disabling only that flag
reduces the refactored cold check to 27.22 s; disabling only memoization instead
takes 149.66 s. Those are initial ablation runs, not the three-replicate medians
above. Package expectations pass with flags off, all flags on, and views disabled.
A separate symbolized ReleaseFast profile, excluded from timings, samples a
20-second cold-check window starting at 25 seconds. Its active leaf samples are
dominated by fixed-point row/graph construction and hashing, not dependency setup.

The revised implementation targets proven OR witnesses: an explicit zero-argument
tag is inhabited independently of unrelated recursive payloads, subject to exact
known-empty assumptions. That proof can avoid eagerly building irrelevant graph
branches; provisional coinductive truth cannot. The change has passed targeted
and full-checker Debug/ReleaseFast validation with
flags on and off, plus the exact 80-snapshot comparison. Nullary witnesses are
detected before payload scheduling; an explicit proof bit propagates unconditional
suffix/shared-row witnesses without treating recursive cutoffs as proof. New
tests cover allocation failures, aliases/known-empty precedence, mode policies,
and a pure-recursive shared DAG without a finite witness. The exact tested source
is integrated and formatting passes. Independent source review found no concrete
defect in intrinsic-proof propagation, mode/assumption precedence, SCC handling,
query-local sharing, OOM cleanup, or graph-work bounds. That bounded review is not
performance evidence; the separate clean paired measurements above establish the
measured performance benefit.

The original-source measurements and experimental evidence are recorded in
roc-pg's `docs/actions-performance-analysis.md`; the separately completed
generator cleanup is recorded in its `docs/actions-refactor-report.md`.
Compiler measurements must distinguish original and refactored roc-pg sources.

### Targeted validation recorded so far

- Constructor/expected-tag projection and settled scratch: individual feature
  filters, combined projection/nominal filters, and flags-off controls passed.
  Independent source review found no concrete counterexample. The final parser
  expectations and CLI/runtime controls pass; the earlier generator's 40,107-input
  differential corpus and live database integration were not rerun for this
  compiler suite and remain useful broader rollout coverage.
- Early CTFE object reuse: the enabled and disabled real-cache workflows passed,
  along with closed-specialization, literal-rejection, and recursive-callback
  regressions. Enabled tests demonstrate actual evaluator splicing, fewer
  Monotype body contexts, debug replay, and identical cold/warm diagnostics.
  **Nominal-lowering correction:** the parent rerun with `-Dperf-all` aborted
  with signal 6 during the first build of the early-cache CLI workflow. A direct
  cold reproduction recovered the full error: `compiler bug: instantiation
  widened a closed tag union` in `solve.zig`'s `unifyTagRows`, reached while
  lowering a nominal backing through exact type constraints. Every object
  lookup in that run misses, so this is not evidence of a cache-hit failure.
  The sealed nominal path incorrectly demanded full-row equality of a sparse
  producer-owned child. The worker's correction uses the existing
  constructor-aware lowering path, without relaxing exact unification.
  The minimal loop-condition regression and real early-cache CLI workflow pass
  **2/2 tests** with all flags enabled and disabled. The workflow covers cold
  evaluation, an app-only edit with early hits and reduced body work, checked
  replay, and diagnostics. Parent review confirms reuse of the existing
  constructor-aware relation, not a weakened solver rule. All three integrated
  fix files match the worker source hashes, and parent formatting checks pass.
- Boxy projection consumers: the original nominal/object-cache/literal-root
  selection plus two new cases passes **104/104** with all flags on, constructor
  projection alone, and all flags off. The focused all-on cohort passes
  **35/35**, including interpreter/dev inspection, nested and recursive
  descriptors, nested-Try equality, and opaque construction.
  The first integration run had 12 failures out of 102 with all flags on and
  none off; constructor-only isolation identified the representation contract.
  Miss analysis now follows the same producer representation as lowering. Bool
  patterns no longer copy full storage into sparse zero-sized backing. Explicit
  nominal-to-tag construction writes directly into the nominal-owned target,
  using declaration schema, application descriptor identity, and formal scope.
  This avoids publishing template-owned intermediates. No assertions are relaxed.
  Independent source review found no concrete projection-specific counterexample
  or defect in the direct construction path. The root exclusion proof is bounded:
  projection accepts a closed singleton, while committed tag layouts retain
  variant count/discriminant structure, so a projected singleton and a complete
  multi-variant declaration do not share a layout merely because payload storage
  matches. Generic payloads alone do not make a closed union dynamic.
  This is not a universal payload-descriptor equivalence theorem. The proposed
  tag-universe-only rewrite was rejected because it conflated producer selection
  with contextual payload interpretation. Existing contextual payload adaptation
  remains unchanged. Final proof tests now pin closed singleton expression and
  pattern children and exercise 15 layout cases across arity 0/1/2 and zero-sized,
  numeric, string, dynamic, and recursive payloads. The narrow suite passes
  **9/9** with all flags on, constructor projection alone, and all flags off.
  Parent hash verification and formatting confirm integration of the exact
  tested sources; production lowering did not change after the 104/104 matrix.
- Readonly views: the initial targeted Debug/ReleaseFast and 80-snapshot matrix
  passed, but independent review then found exponential shared-DAG cases.
  The worker reports that the replacement greatest-fixed-point implementation
  passes the strengthened 215-test Debug matrix with views alone, views plus
  memoization, and flags off; ReleaseFast with views plus memoization also passes.
  All 80 scratch snapshots pass EXPECTED with the features on and off. Four
  outputs differ only in lower constraint-function variable IDs and normalize
  identically. At that integration stage, the parent complete checker suite
  passed **1,599/1,599 tests** in Debug with `-Dperf-all` and with all features
  disabled. The targeted combined-feature filters and formatting checks passed.
  **Record-tail correction:** review found that required empty fields reached
  through record extensions were ignored. The worker has confirmed a source
  reproduction: a nominal record with an impossible field supplied through its
  row argument incorrectly required an otherwise impossible match arm. The
  correction preserves record-tail policy rather than treating an open tail as
  an ordinary payload. Its final Debug checker suite passes **1,602/1,602 tests**
  with `-Dperf-all` and with all flags off, including the source regression.
  Views-only and views-plus-memo targeted runs each passed **26/26 tests** before
  the source regression was added. Parent source review and hash verification
  confirm that the exact tested patch is integrated; the parent formatting check
  also passes. Subsequent unconditional-witness pruning reran the full checker
  in Debug and ReleaseFast with gates on and off, and repeated the exact snapshot
  gate; those passes apply to the final source hash.
- Completion-path container: enabled focused tests and both ReleaseFast builds
  passed. The parent completion-path/relation filter passes **3/3 tests** with
  `-Dperf-all` and with all features disabled. Final combined-build measurements
  pass; the isolated container-only timing below remains separately qualified.

The targeted passes complement the clean whole-workload acceptance above.
The first parent ReleaseFast
build was interrupted by the tool's 600-second limit during LLVM object emission,
without a compiler diagnostic; it did not produce a benchmark binary. A separate
benchmark worker subsequently built clean binaries with sufficient time and
isolated output/cache paths, completing the paired acceptance runs.
After preserving evidence, that worker removed only its completed generated
cache/output directories, restoring available disk space from 6 GiB to 78 GiB.

The integrated `src/check/exhaustive.zig`, including the record-tail correction,
matches the validated worker SHA-256
`ab1aea43ada9dbb86e37f768d944a97e4819d65f750d949d2f9919ea20ce0efb`.
The reader, memo, reader tests, checker module registration, and checker README
also match their worker handoff hashes. Before integration, a guarded comparison
established that the entire difference in `exhaustive.zig` was the intended
fixed-point revision; no unrelated parent edits were discarded. That earlier
revision had SHA-256
`7535437491c9539716795ed968d796080164985d8ab9881958e15be85286ba7d`
and passed the 1,599-test parent runs above. The record-tail revision had SHA-256
`493901662410fb5293951697f71f228b1c515f79381ebef58edca1e746b03b96`;
the current hash additionally includes the validated unconditional-witness
pruning change.

Parent integration commands:

```sh
zig build run-test-zig-module-check -j2 -Dperf-all --summary all
zig build run-test-zig-module-check -j2 --summary all
zig build run-test-zig-module-postcheck -j2 -Dperf-all --summary all -- \
  --test-filter 'completion path' --test-filter 'completion relation'
zig build run-test-zig-module-postcheck -j2 --summary all -- \
  --test-filter 'completion path' --test-filter 'completion relation'
# Original parent run aborted; the worker's corrected on/off runs pass:
zig build run-test-cli -j2 -Dperf-all --summary all -- \
  --test-filter 'early CTFE cache'
```

### Snapshot selection and comparison

The original readonly-view gate selects nonrecursive `*.md` files under
`test/snapshots/nominal/` (68) and `test/snapshots/nominal_decl/` (5), plus these
seven files directly under `test/snapshots/`:

```text
assoc_recursive_nominal.md
nominal_destructure_decl.md
nominal_destructure_pattern.md
nominal_destructure_record_decl.md
record_optional_access_chains.md
record_optional_defaulted_fields.md
record_optional_destructure.md
```

The sorted project-relative paths with LF terminators have SHA-256
`2deb39efc57b4c65bb03d7e7509d243b3d59e69e5e262ffa53d92e8c1b270fbf`.
Copy snapshots to separate scratch roots; rebase only EXPECTED diagnostic display
filename prefixes to those roots. Run `run-snapshot-tool --check-expected` with
views+memo enabled and flags off, each with `-j2` and tool `--threads 2`.
Never update expectations to conceal a difference. Compare complete generated
outputs after path normalization; the earlier four solver-ID-only differences
are separately identified above. Scratch copies from the first gate were
turn-local and not retained; the selection is reproducible without them.

## Build configuration and cache isolation

Every `zig build` command in this work uses `-j2`. Features are selected at build
time, not by program-size thresholds or ad-hoc environment variables:

| Build option | Intended implementation |
|---|---|
| `-Dperf-inhabitedness-memo` | Memoize complete inhabitedness queries within a stable analysis |
| `-Dperf-nominal-views` | Read nominal templates through immutable substituted analysis views |
| `-Dperf-constructor-projection` | Check producer-owned nominal constructors through selected payloads |
| `-Dperf-tag-projection` | Copy only selected expected-tag payload context |
| `-Dperf-settled-scratch` | Isolate settled-row validation's visited storage |
| `-Dperf-early-ctfe-cache` | Take proven-safe object-cache hits before Monotype body lowering |
| `-Dperf-const-completion` | Avoid tombstone accumulation in completion-relation active paths |

`-Dperf-all` enables the suite; an explicit per-feature `=false` overrides that
default. The shared feature registry supplies the build options and compiler
gates. Its versioned mask contributes to the compiler artifact hash, preventing
checked artifacts and object packs from mixing between configurations.

These flags are rollout controls, not alternate language modes. Differences in
accepted programs or diagnostics require a documented correctness explanation
and a focused regression, not silently updated snapshots.

## Inputs retained for final benchmarks

A read-only inventory verified these source copies without builds or Git
history. Paths below are machine-local reproducibility references, not required
repository layout:

| Input | Retained root |
|---|---|
| Original package and small app | `/Users/rtfeldman/code/roc-pg/.delta/worktrees/9g3vmbn2qc0m/roc-pg/local/baseline` |
| Selected refactor, inline nulls | `/Users/rtfeldman/code/roc-pg/.delta/worktrees/vme4vvh6eg3a/roc-pg/local/null-study/inline` |
| Shared-null experiment | `/Users/rtfeldman/code/roc-pg/.delta/worktrees/vme4vvh6eg3a/roc-pg/local/null-study/shared` |

Verified Actions SHA-256 identities, respectively:

```text
cdd2d59be288ecce0cbadebd0d703f575aae9bdc359a6ceb4ad697ef89b79120
d04e9915ab6971f52932a088202cf5500781855ed4c689f88ee22740ea2536c8
2575bfbbb0387545c0977e44a97de29aa50b054dc5b561980f95797a313181b3
```

Inline and shared copies have identical Node and small-app sources. The original
Node hash is `581da93bb16cc146b5f86d5402fdd753abbfa56250c8d0cfce0a307b7b2b874c`;
the selected Node hash is
`f1bf21d8ec43fe8d5d4a0a049d77caad34f6579c0eac53fc48e9a734369f0603`.
The retained baseline compiler is
`/Users/rtfeldman/code/roc/.delta/worktrees/w0bxep5v818j/roc/.tmp/roc-profile-rf`,
SHA-256 `0cdc72d4240438ef29c6b7c2f53a1880dad3ade8b155bff242bf17074c32db0e`.
Reverify source and binary identities immediately before measuring.

The current attached roc-pg source could not be terminal-hashed because Delta
failed to materialize the checkout before executing the command. This is not
evidence of a source writer or changing hashes. The verified inline copy is a
usable selected-refactor input while that infrastructure issue is unresolved.

Use fresh, separate cache roots for each source/compiler configuration. The
retained null-study cache mixes experiments and is not a clean warm-cache input.
Compare no-cache cold checks separately from verified checked-cache rebuilds;
do not compare the shared variant's first warmup with its later cached-module
build. These are prepared inputs, not new performance results.

## Shared-constant investigation

The source distinction is between the qualified nominal constructor `Node.Null`,
the annotated immutable binding `Node.null : Node`, and a contextual inline
`Null` expression. Both qualified construction and the binding have outer type
`Node`; the constructor's internal structural row is not an outer
annotation-polarity difference.

A direct-PID native profile of the shared-value variant placed approximately
85% of sampled worker time under `requestCompletionRelation`, including
uninhabitedness probes. Disassembly localized most of that function's self time
to the active-path hash table's metadata probing loop. The old table repeatedly
inserts and removes entries during recursive enter/leave traversal. Historical
tombstones can therefore make an absent lookup scan a large table even when
few keys remain live.

The initial production candidate changes the active-path data structure to a
deletion-repairing array hash map. It does **not** memoize provisional recursive
answers or change the cycle rule. Recursive unchanged/completed/mismatch tests
and a wide sibling enter/leave stress case accompany the change. Final paired
timings are still required.

Retained instrumented binaries provided a controlled warm-cache comparison:
94.599 s Monotype with the old container versus 65.975 s with deletion repair;
the inline-null control with deletion repair took 15.162 s. Both shared-value
runs had 61/61 checked-cache hits. The enabled residual profile still spends
substantial time in recursive uninhabitedness.

These are intermediate measurements, not final production timings: the two
binaries retain identical disabled diagnostic plumbing, and sampling detected
nonzero TLS-access overhead from it. Production source no longer has those
hooks. See `shared-null-lowering-analysis.md` for the cache-state controls,
profiles, and remaining cost.

## Constructor representation contract

The producer-owned constructor relation is restricted to fresh, syntactically
known single-tag construction under an actual nominal relation. It validates
membership and arity and preserves full payload obligations, while avoiding
materialization of unrelated variants into the fresh child row. The outer
nominal remains authoritative for the full type and representation.

This is not permission to truncate arbitrary shared structural rows. Their
complement rows still matter. Contextual scheme guidance alone cannot establish
a nominal result. Unknown tags, wrong arity, opacity, inverse rewrapping,
payload mismatches, and off-root dispatch requirements need dedicated coverage.
The authoritative rules are in `design.md`.

## Read-only analysis contract

Views carry declaration-scoped root/name substitution and exact effective
identity. Irrelevant substitutions may be normalized away for immutable closed
structure; private unknowns retain application ownership. Recursive resets and
permutations must converge under the existing nominal-growth validity rule,
without a depth budget or declaration-only approximation.

Only actual solver roots may leave analysis as mutable blockers. View identities
must not survive in returned diagnostics. Shared DAG nodes are not active
recursive cycles; caching an answer obtained only by cutting an active cycle is
not valid. Tests cover that distinction and allocation/lifetime boundaries.

## Cache contract

Completed constant values, compiled object specializations, and `roc test`
outcomes are different caches. Eligible code does not become uncacheable merely
because an `expect` or schema conversion reaches it.

Early Monotype hits must obey the same compile-time exhaustiveness-observation
proof as late Direct LIR hits. They must not bypass that proof through the hosted
placeholder path. Existing pack restrictions on literal conversion, program-local
dependencies, identity, ABI, and relocations remain in force.

Required evidence includes a real warm object hit, skipped Monotype body work,
unchanged output/debug observations, and identical cold/warm failures. Cache-hit
counts alone do not demonstrate the requested performance improvement.

## Final acceptance checklist

- Individual flags, relevant interacting flags, all flags, and flags off.
- Targeted unit tests, allocation-failure tests, and representation-growth tests.
- Existing checker, nominal, alias, record/default, dispatch, CTFE, and cache
  regression suites; snapshot differences classified rather than bulk accepted.
- Debug and ReleaseFast validation where applicable.
- Original and refactored roc-pg workloads, cold and warm, with identical compiler
  mode, two workers, verified cache states, and no overlapping heavy workloads.
- Data-structure fixes and compiler-wide overhead evaluated separately from the
  single large parser example.
- No temporary profiling hooks, source-specific shortcuts, or dependency symlinks
  left in production files.
- Every unresolved correctness/performance issue explicitly reported before any PR.
