# Compiler cache follow-up implementation log

## Status

The measured experimental stages are closed, not accepted for default-on rollout.
Higher-order reuse passes individual correctness acceptance without a convincing
SQL time benefit. Finalized literal reuse has valid narrow success hits but fails
full observational equivalence. Lazy nominal rows remain unactivated and unfinished.
No combined rollout is claimed. Boxy caching is excluded.

The concise final outcome, timings, failures and disposition are in
[the follow-up results report](compiler-cache-overnight-report.md).
The sections below retain the implementation history; early unbuilt/partial
statuses are historical, not claims that supporting APIs complete an optimization.

## Published rollback checkpoints

Branch in both repositories: `sql-parser-cache-checkpoint-20261003-185309`.

- Roc: `9baf864b2cddd6cd87009ac6d7655b5a61b0db33`.
- roc-pg: `287f87939223dd2d36c2841cf1683dbaea084628`.

Both were pushed to their configured GitHub source remotes. Source, tests,
generator changes, and reports are committed. Generated caches and large local
evidence bundles are not committed; `.ctfe-investigation/` is now ignored in both
repositories. Follow-ups use `sql-parser-cache-followups-20261003-185309` in Roc,
leaving the checkpoint refs unchanged.

## Measurement and acceptance rule

Validate each implementation separately before combining it with another trial.
Use same-source ReleaseFast feature-off/on pairs, serialized two-job workloads,
fresh cache roots, two warmups and three measured repetitions. Record phase
times, memory, artifact bytes, actual object hits and skipped body work; do not
substitute checked-module hits for object reuse.

Include original/refactored/shared-null cold and warm workloads, a genuine
app-only edit, the particular new feature's regression workflow, and ordinary
small controls. Invalid correctness configurations do not count as performance
comparisons. Preserve unsuccessful approaches and overhead, not just wins.

## Stage 0: independent follow-up gates

The measured `-Dperf-all` bundle remains the original seven gates (mask 127).
Three new gates require explicit opt-in and are not enabled by that bundle:

| Build option | Runtime feature | Intended trial |
|---|---|---|
| `-Dperf-lazy-nominal-rows` | `lazy_nominal_rows` | General late structural-row/nominal unification |
| `-Dperf-late-callable-cache` | `late_callable_cache` | Lookup after complete higher-order identity exists |
| `-Dperf-finalized-literal-cache` | `finalized_literal_cache` | Reuse finalized literal code/results/report facts |

The mask is widened to 16 bits with a versioned fixed little-endian encoding.
This intentionally isolates artifact identities from the prior encoding.
Explicit per-feature true/false overrides remain authoritative. The two narrow
registry/cache-identity tests pass, and formatting passes. Reserving gates is not
implementation or speed evidence for the trials they name.

## Stage 1: late nominal rows

The first implemented foundation is an owned resumable opening with root/name
substitutions, a shared template-to-cell map, explicit demand/materialization,
private-cell isolation, and OOM invalidation. Supporting tests cover late bare
helpers and generalized uses. It has no production call sites yet and is not
the requested optimization.

Inspection confirmed that `Store.resolveVar` is infallible and allocation-free.
Deferred payloads cannot be disguised as flex rows or allocated during that
observation. The next representation must explicitly expose latent schema edges
to semantic traversals. Per-template-root rank/quantification history and
speculative map rollback are required; one opening-wide rank is insufficient.

The worker has added explicit template/owned traversal references, demand-map
rollback, and per-template rank history. Fourteen expanded foundation cases
passed serially in Debug and ReleaseFast with the gate off and on. These are
correctness runs, not optimizer timings; the API has no production activation.

The publication decision is resolved: persist explicit opening/fragment tables
with versioned type-store serialization and importer contracts. The existing
Store serializer writes complete arrays, so forcing all orphan complements to
materialize there would merely move the copy cost. No assumed live-root packing
or heuristic orphan detection is used. The foundation and storage source owners
have handed off to the production activation stage.

The persistent table layout now has source implementation, including Store
ownership, typed and relocated serialization, map/head rollback, creation
provenance, retained immutable schema translations and explicit stored-map
demand. Checked-artifact layout version 108 accompanies this real data change;
the corresponding golden was regenerated and validated as
`5d6613482fce9c685f6798812bba6921d5c87bcb032fce1196faae97c4493f8e`.
Seven persistent tests now pass, covering rollback, resumed demand after source
destruction, capture/demand/clone/deserialization OOM cleanup, nonidentity
translations, relocated serialization, recursive constraint/effect sharing, and
poison/capacity-independent serialized bytes.

The bounded Debug/ReleaseFast × gate-off/on matrix passed 17 types tests and
17 checker tests in each configuration: 136 passing executions. Types contain
8 foundation cases, 7 persistent cases and 2 harness cases; checker contains
15 semantic cases, the golden and 1 harness case. Two prepared fixture defects
were corrected before activation: Relay helpers needed an explicit exported
namespace, and the eager omitted-payload cycle reports `Anonymous Recursion`.
This validates storage and the eager semantic baseline, not delayed inference.
No latent solver descriptor or production admission is installed yet.

Importing an already-known nominal declaration must retain an explicit immutable
schema-root translation; a discarded per-copy map cannot later translate opening
bindings. That translation is separate from each opening's private mutable state.
The importer producer and scheme-copy integration remain pending, and their exact
contract must be declared before activation.

Source inspection also identified a scaling risk in persisted demand: rebuilding
scratch on every call and checking every reconstructed binding against a linked
list can make repeated/full demand quadratic. This is not a measured regression.
Activation measurements must include it. Before adding a runtime index, evaluate
whether the demand producer can publish an exact new-binding delta and avoid
rechecking previously retained entries, preserving alias updates and rollback.

Schema capture will use existing copy machinery without cloning or scanning the
whole Store. Its mapping still has a known sparse-topology cost: `DenseMap`'s
outer chunk vector spans the lowest through highest touched chunk IDs, so a
small reachable graph does not guarantee proportionally small scratch storage.
That allocation/span cost belongs in measurement; paging alone is not an
O(reachable-roots) memory proof. A global mapping redesign is not part of this
initial correctness-first trial.

## Stage 2: later higher-order hits

The scoped implementation uses a separate reservation seed combined with the
completed procedure identity in a versioned late-key namespace. Early lookup and
its existing key remain unchanged. Lookup and publication use the same late key.
The pack's 32-byte key/ARC table can remain unchanged.

Admission preserves current finite/plain ABI, capture, return-reuse, compile-time
observation, dependency portability and literal restrictions. App-derived
specializations remain app-filed rather than being misrepresented as package-only
code. No extra early whole-program analysis or Boxy cache support is introduced.

Narrow validation demonstrates an app-only comment-edit hit with a matching
LIR-body-skip trace and exact runtime output. Individual Debug gate-off/on
identity, pack-exclusion, edited-app, capture, invalidation and diagnostic
workflows now pass. Inlining is not globally disabled to force a benchmark hit.
Independent source review found no correctness-blocking issue; it identified
possible missed skips after procedure interning and cache-hit inlining effects
as measurement questions. ReleaseFast bundle127-versus639 acceptance uses the
hash-verified isolated HOF snapshot, not other unvalidated trial code.

Whole-SQL acceptance subsequently found a correctness blocker in that frozen
snapshot. Both ReleaseFast configurations pass the focused callable workflow and
original package checks, but the first original cold application build passes
with mask 127 and aborts with mask 639: demanded non-hosted procedures 27 and 29
have no statement body. The successful off run took 52.52 seconds; the aborted
on run is excluded from timing comparisons. No source or benchmark input was
changed before preserving this counterexample. Raw evidence is retained under
`.tmp/late-callable-rf-pair/runs/sql-build-cold-original-on-1/`.
The trial remains unaccepted; body skipping, artifact availability and native
demand must agree before measurements resume.

The preserved traced diagnostic narrows that description: procedures 27 and 29
are `list_map_can_reuse` and `Builtin.List.map`, both bodyless, and its complete
scan contains no late lookup/hit/skip markers. This is not a demonstrated
cache-hit body-skip failure. Source review identified unconditional late
specialization publication as a producer error: representation/inline-only
reservations enter `spec_procs`, and existing reachability treats those entries
as code roots. The proposed fix defers late offers until the final lowering
closure proves canonical demand and completed body or supplied artifact.
Interned aliases must use canonical demand/completion, not only the owner's
reach bit. Existing demand authority and inlining policy remain unchanged.
Execution now closes this correctness blocker on the pure corrected 862-file
source. The old mask-639 binary fails the compact closed `List.map` regression;
corrected mask-127/639 compilers pass publication/identity/key/pack tests and the
full focused callable workflow. A fresh original SQL cold gate-on build succeeds
with exact runtime output in 53.47 seconds. The new alternating off/on
original/refactored/shared-null no-cache check, cold/warm/app-edit matrix also
passes runtime output and package hash guards throughout. Warm focused workflows
retain named actual hits and skipped bodies. Final controls, parser expectations
and distributions are now complete: all six 383-expectation runs and small
controls pass. The single 53.47-second sample is not a cross-snapshot speed
comparison or a final median.

Corrected source manifest:
`2ed01a44ea269794e1ad1d16327a4bde123adc30712576ef7c05b72686836f95`.
New evidence uses `.tmp/late-callable-rf-remeasure/`, preserving the original
failed snapshot, cache and logs separately.

### Corrected higher-order timing result

Same-source medians after two warmups and three measured repetitions:

| SQL workload | Cold off/on seconds | Warm off/on seconds | App-edit off/on seconds |
|---|---:|---:|---:|
| Original | 52.74 / 52.57 | 1.48 / 1.48 | 4.19 / 4.18 |
| Refactored | 31.95 / 31.96 | 1.48 / 1.47 | 4.25 / 4.24 |
| Shared-null | 40.38 / 41.42 | 1.46 / 1.46 | 4.16 / 4.14 |

The shared-null gate-on cold set has high variance, including a 53.24-second
sample. It is retained, not trimmed. Warm and app-edit differences are
noise-scale; the SQL matrix establishes no broad speedup.

Focused helper/capture medians remain 0.03 seconds in both configurations.
Real work avoidance is nevertheless demonstrated: the imported helper's native
procedures decrease from 3 to 2 and generated bytes from 1264 to 992; returned
captures decrease from 5 to 2 procedures and 1520 to 1008 bytes. Monotype body
contexts remain 29 and 36 respectively, so this later lookup does not avoid
the preceding specialization work.

SQL app-edit runtime native emission decreases from 754 to 727 procedures and
3,821,116 to 3,778,048 bytes; preceding Monotype body contexts remain about
68,140–68,178. Persisted SQL packs save 71,063 bytes per variant. These are real
but small savings, not a time improvement.

Original cold peak-RSS medians increase from 8,926,396,416 to 9,509,011,456
bytes (8.926 to 9.509 decimal GB). This approximately 6.5% increase is retained
as a measured cost; ranges overlap, but it prevents a general memory-saving
claim. Refactored/shared-null cold memory changes are much smaller. The full
stage report retains exact byte counts and binary GiB conversions.

Recommendation: retain the gate as **default-off experimental**, with completed
correctness acceptance but no demonstrated whole-SQL timing benefit. Preserve
the artifact/work savings and memory tradeoffs separately from speed claims.
The build window is released to actual literal CLI acceptance.

Completed pre-counterexample measurements are retained, not discarded. The
following medians use repetitions 3–5 after two warmups, alternating on/off
order. Both tiny higher-order fixtures verify exact native output on every
measured build. The compiler is ReleaseFast; app code generation uses `--opt=dev`.

| Workload | Mask 127 seconds | Mask 639 seconds | Peak RSS MiB, off/on |
|---|---:|---:|---:|
| Imported helper, no cache | 0.030 | 0.030 | 66.3 / 66.4 |
| Imported helper, unchanged warm app | 0.030 | 0.030 | 59.6 / 60.0 |
| Imported helper, app-only edit | 0.030 | 0.030 | 64.8 / 64.8 |
| Returned captures, no cache | 0.030 | 0.030 | 66.2 / 66.2 |
| Returned captures, unchanged warm app | 0.030 | 0.030 | 60.0 / 59.7 |
| Returned captures, app-only edit | 0.030 | 0.030 | 64.6 / 64.5 |
| Original SQL package, no-cache check | 32.700 | 32.720 | 6143.4 / 6111.5 |

The tiny-fixture wall times all fall in the same 10-ms reporting bucket; they
do not establish a speed benefit. The SQL check ranges are 32.65–32.79 seconds
off and 32.57–33.12 seconds on, also not a demonstrated difference. Diagnostic
runs establish actual late hits and body skips separately from these timings.
These are provisional scoped results from a configuration that fails whole-SQL
application acceptance, not evidence to enable the feature.

Measured compiler SHA-256 identities: off
`05f33fbd8093edc52c49ff65d33ad4db05b8211a7ba64500ed773f6269c99567`;
on `72e5f319f90397d590add65673aa8026eea326e12ae4ced2b241f0e79dcdce23`.
Raw commands, results and runtime checks are in the isolated higher-order
checkout's `.tmp/late-callable-rf-pair/runs/`.

The traced diagnostic also exposed a measurement-harness failure. Roc had
already finished, but whole-file regular-expression parsing of its
12,464,993,583-byte log occupied Python for over three hours. A process sample
confirmed Python's regex search/match stack; the parent stopped only that
postprocessor and preserved the raw log and compiler artifacts. The diagnostic's
compiler time was about 34 seconds, not three hours, and is not comparable to
the fresh cold sample. Bounded/streaming parsing is required before resuming the
harness. The build window was then released to actual storage validation.

The current callable ABI retains producer member order. Reversing a joined
multi-target callback set can change its completed identity and correctly miss.
Sorting only the hash would falsely equate different discriminant mappings; no
such normalization was added. Single-target request-order stability is tested
separately. Order-independent multi-target ABI production is additional work,
not a cache-consumer patch.

## Stage 3: finalized literal code

The normal finalization producer has completed typed values and failure/report
facts; independent module-pack lowering may still contain unevaluated conditional
conversion code. Only the former is eligible for this trial. Keep the latter's
withholding until a real finalization certificate exists.

The producer-retention stage now has source code and focused tests prepared,
but has not yet been built. It retains stable literal source/evaluation identities
and owned outcome/debug facts before host/prepared state is released. Pack admission
also needs the explicit owning specialization association; an evaluation-root
identity alone is not that proof. Shared Monotype propagation waits for the
validated late-key API handoff.

Existing checked-root reporting authority and deduplication remain authoritative.
Unknown-origin crashes, unsupported callable results, and nonportable data are
not admitted by inference. No converter re-execution is intended for a certified
same-key finalized result. This producer work is not yet an end-to-end cache hit.

Literal roots can be deduplicated across multiple owning specializations, so the
association is an explicit root-to-owner relation rather than one owner field.
Past embedding/report authority cannot suppress a current program's report.
Failed CTFE reads and conditional debug-demand ordering need portable attribution;
those entries remain excluded until that contract is proven. The first intended
admission is successful, observation-free, non-callable frozen values.

The isolated literal trial now has source for worker-propagated read/write
context, many-owner publication, early closed owner keys, owned completion facts,
runtime-use rows before folding, and a gate-separated certificate codec. None
has been built yet, and pack admission is unchanged. The worker's interrupted
turn did not merge that source; its checkout was retained for continuation.

An additional producer requirement was identified and implemented in the isolated
source: each actual literal read carries its originating logical owner through
the immutable value descriptor. A shared root's owner list cannot select that
owner, particularly after inlining or when early and unsupported higher-order
owners share a root.

The isolated source now also forwards decoded certificate pointers, validates
early hits against the current key/artifact/module/expression, propagates validated
external certificates, and builds per-closure certificates after the existing
converter and program-symbol filters. The initial path accepts only validated
early provenance; higher-order certificate offers cannot bypass that validation.
The bounded first validation window now passes eval tests with the gate off/on,
the real one/two-worker producer and corrupted-authority
decline workflow, and borrowed-prefix reflection tests. Exact selected
counts still need an explicit summary capture; quiet successful commands are not
being converted into invented counts.

An explicit summary exposed a CLI test-discovery gap: the earlier 1/1 filter
result did not execute the prepared pack-store/certificate units. The runner
now references those modules explicitly. The first actual six-test run reached
the new assertions and found a namespace-fixture allocation-size mismatch
caused by freeing a sentinel real-path allocation as a nonsentinel slice.
That fixture is corrected; clean rerun and exact selected counts are required.
The earlier CLI-unit pass claim is withdrawn.

The same summary audit found that the earlier backend filter selected zero
tests. Its codec and independent-address-test pass claims are withdrawn too.
Explicit test-mode references now discover those modules. Real compilation
exposed an enum-literal assertion needing an explicit enum type; the assertion
and both target loops remain intact. Actual backend execution now passes 5/5
tests with clean debug allocator checks: four codec cases and the independent
address test's two target loops. Real execution also required fixture lifetime
repairs for a returned certificate pointer and allocator-correct
`TestLayoutState` destruction/partial-init cleanup.

Validation exposed four missing owner-pool lifecycle edges: BodyShard frozen-field
initialization, the Lifted owned view, the Lambda Mono owned view, and the solved
debug clone. They are corrected, with the validated slice selectively applied to
the parent without undoing deferred higher-order publication. The mixed-owner
fixture also required live callback/marker results: its ignored callback allowed
the unsupported read to disappear. Record-field callbacks must be bound before
calling to avoid method-dispatch syntax.

The first actual configured-prefix CLI workflow fails acceptance: warm builds
still evaluate four roots, and the intended literal-owning keys miss. Unrelated
plain/converter hits are not counted as success. The normal finalized runtime
pack contains nine artifacts but only one offered specialization and no withheld
entries; the independent unfinalized module pack withholds its four candidates.
The actual gap is inlined-away owner publication, not demonstrated overstrict
converter filters. The producer unit's broad `keep_specialization_procs=true`
setting did not prove the production publication path.

The next scoped experiment considers explicit additional cache-publication
code demand for successfully certified early literal owners, preserving caller
inlining and fully completing implementations before offering them. In the
fixture, `PlainValues.pair` owns the literals; wrapper names alone are not a
reason to retain transitive callers. Extra cold code, analysis and memory costs
must be measured. Existing converter/program-symbol filters remain unchanged.
This experiment now passes the actual configured-prefix Debug workflow:
`PlainValues.pair` has a named certified early hit, owning-body construction is
reduced, and warm literal evaluation is zero. Exact uncached/cold/app-edit output,
same-path reversed caller order and a real converter implementation edit pass.
The 1/1 workflow took 9.003 seconds in total; that is not a per-build timing or an
off/on performance comparison. Compiler SHA-256:
`ed4277231029f16f9dfa399a0ecc76408484047195149b2195fdfb3e1b21faab`,
mask 383, format v6, literal identity domain 3. Production-setting producer tests
also pass with `keep_specialization_procs=false`.

Independent source review found an additional production lazy-reader boundary
gap: initialization catches loading/indexing failures and can serve partial
indices after setting readiness too early. Direct loader tests did not cover
that boundary. Acceptance is held for a coherent no-partial-offer failure rule
and production-path regressions. This is an error/state contract defect, not
evidence that the successful valid-pack workflow returned incorrect values.
Recovered-source acceptance now passes the configured Debug Pair workflow
(22.104 seconds for the entire workflow), and fresh ReleaseFast Pair verification
also passes (4.897 seconds for the entire workflow). Neither duration is a
single build timing. All 160 fresh RF matrix commands complete with exact native
outputs: 40 literal-fixture builds and 120 SQL check/build commands.

| SQL input | Warm off/on seconds | App-edit off/on seconds |
|---|---:|---:|
| Original | 1.43 / 1.44 | 4.07 / 4.08 |
| Refactored | 1.44 / 1.43 | 4.26 / 4.20 |
| Shared-null | 1.47 / 1.49 | 4.16 / 4.19 |

These medians do not establish a convincing SQL wall-time gain. The tiny literal
fixture medians are 0.04/0.04 seconds uncached, 0.06/0.06 cold, 0.03/0.03 warm,
and 0.04/0.03 app-edited; coarse timer resolution limits that last comparison.

Full observational-equivalence acceptance is **not met**. A subsequent untimed
observed-converter audit prints four `dbg` events in both cold configurations,
but fully warm prints zero off and four on, despite matching native output and
no certified hits for the observed owner. The harness's assumed warm-four
baseline is withdrawn. Raw logs and the frozen timing stage are preserved;
default-off remains mandatory while the differential is investigated. Rejection,
dormant, current-source and callable workflows pass under both RF gates.

The CLI preparation also timed out a broad `install` graph of 707 steps at
360 seconds. That infrastructure attempt is excluded from verification and
application timings; subsequent builds use narrow compiler/runner targets and
explicit verified native host dependencies.

## Recovered measurement inputs

The legacy benchmark worktree directories disappeared. The current attached
roc-pg package still matches the selected Actions/Node/main hashes, and
`.ctfe-investigation/baseline/` retains the exact original Actions/Node pair.
Fresh owned benchmark fixtures copy the current package/example and replace
only that pair for the original case.

The historical shared-null Actions bytes were reconstructed in memory by changing
only eligible generated-prefix null expressions and leaving `Err(Null)`, patterns,
and the hand-written suffix unchanged. The result has exactly 179 replacements,
765,534 bytes, and the original shared SHA-256
`2575bfbbb0387545c0977e44a97de29aa50b054dc5b561980f95797a313181b3`.
This is hash-guarded recovery of a fixed benchmark input, not a new textual
compiler/generator optimization or a production source edit.

## Address determinism audit

Source-only review found no process-address leak in normal specializing pack
production. Object mode emits symbolic relocations; extraction preserves explicit
data addends and normalizes AArch64 detached-call displacement. Native addresses
are bound when the receiving execution image is linked, not when packs are saved.
Serialization preserves ordered inputs, rather than canonicalizing arbitrary
artifact insertion orders. Normal parallel emission commits in demand order.

Existing round-trip and relocation tests do not compare independent producer
captures. The added differing-placement byte-equality test's initial apparent
pass was a zero-test filter and was withdrawn. Discovery and fixture lifetime
are corrected; actual execution now passes both target loops in the clean 5/5
backend run. No production normalization correction was needed.
This fixture bounds the codegen/extraction boundary, not
full source/compiler/demand-order reproducibility.
The separate check-then-write publication path uses one shared `.tmp` filename
per destination, creating a concurrent-writer staging collision risk. That finding
does not establish nondeterministic pack contents or explain the measured slowdown.

## Remaining report obligations

- Complete the actual delayed-row optimization, not only the opening foundation.
- Prove real late object hits and exact callback/closure behavior.
- Prove finalized literal success/rejection code reuse with current-source
  diagnostics and no lost report-and-continue semantics.
- Review each change independently, re-measure, then run combined acceptance.
- Distinguish reproducibility/relocation tests from unsupported-dependency limits.
- Record final source/binary identities and recommendations before publication.
