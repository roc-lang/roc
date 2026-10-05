# Finalized literal cache recovery and acceptance

## Status

The recovered source now passes targeted off/on guards and actual Debug and
ReleaseFast named Pair workflows. A fresh same-source RF matrix completed
160 compiler commands, with 130 exact native-output checks and 30 package checks.
There is no convincing SQL wall-time improvement. **Full acceptance is rejected**:
the observed-converter audit produces four compile-time debug messages on a
fully warm literal-on build but none on the corresponding literal-off build.
Native output is unchanged; observed entries have zero certified hits.

Keep the experiment default-off. The narrow successful, noncallable,
observation-free class has actual reuse evidence; this is not evidence of
equivalence for failed or observed classes. No production changes were made
after the measured RF source freeze to normalize the diagnostic difference.

The experiment remains default-off and outside the original seven-feature
`perf-all` bundle (mask 127). Literal-on uses mask 383, pack codec 6, and feature
identity domain 3; gate-off retains codec 4 and domain 2.

## Preserved evidence

The read-only donor is
`/Users/rtfeldman/code/roc/.delta/worktrees/7gabg1zkz4hz/roc`.
Its historical stage log is `docs/compiler-performance-finalized-literal-cache.md`.
Raw workflow and selected-test manifests/logs are in
`.tmp/literal-validation/pair-workflow/` and
`.tmp/literal-validation/selected-tests/` beneath that donor.

The actual Pair workflow passed 1/1 in 9.003 seconds **for the entire workflow**,
not per build. Compiler SHA-256:
`ed4277231029f16f9dfa399a0ecc76408484047195149b2195fdfb3e1b21faab`.
It demonstrated a named certified early `PlainValues.pair` object hit,
reduced owning-body construction, zero warm actual literal evaluations, exact
uncached/cold/app-edited/reversed-caller-order output, and correct output after
a real converter suffix edit from `!` to `?`. Producer regressions also passed
with production `keep_specialization_procs=false`.

Earlier actual workflow acceptance failed: four warm literal evaluations
remained because caller inlining removed the owner before publication.
Unrelated converter/plain hits did not count. The finalized runtime pack had
nine artifacts, one offered specialization, zero withheld; the independent
unfinalized module pack had sixteen artifacts, four offers, four withheld.
The repair adds explicit code-publication demand for qualified direct owners;
it does not relax portability filters or retain every reservation.

Earlier CLI 1/1 and backend zero-test apparent passes were withdrawn. Explicit
test references subsequently discovered actual cases. Historical clean counts
include backend 5/5 (four codec cases plus independent x64/AArch64 address
loops), CLI 6/6, postcheck 4/4, and producer 2/2. Exact logs/manifests, rather
than quiet build success, are authoritative. Latest IO/artifact-first reader
changes and recovered source need new counts.

## Recovery boundary

Recovery uses reviewed source hunks and `apply_patch`, not a whole-checkout copy.
The recovered 25-file literal source inventory initially matched the donor
byte-for-byte, before the approved test addition and validation repairs below.
Checked-key factory/delegation hunks are integrated separately, preserving the
current checked-layout golden. Current nominal/type/import semantics and
corrected HOF publication are preserved. Aggregate/HOF/nominal reports remain
parent-owned and are not replaced by donor versions.

The generated recovery inventory `.tmp/literal-recovery/source-manifest.json`
records 925 source/build files and both current/donor SHA-256 values. Its recovery
checkpoint SHA-256 is
`382f5df5edd522d86884ecf3f1b91ede5f03c55285691f3ec8da90662adf6ba6`.
Seven intentional source differences remain: the checked-layout golden, current
import/nominal semantics and their tests. This is a source checkpoint, not a
compiler-build or performance identity.

After that recovery checkpoint, the parent approved one test-only review-gap
addition. The existing actual-file lazy-load/OOM sweep now runs both
specialization-first and artifact-first initialization. Artifact-first success
must load a complete ready collection and serve the expected artifact; every
terminal OOM must return no artifact, deny repeated lookups through both gates,
retain zero served counts and avoid retrying. Production loader policy and code
are unchanged. This addition subsequently passed in the actual off/on CLI
guard runs below.

`.tmp/literal-recovery/artifact-first-test-delta.json` records the exact test diff.
The corresponding 925-file `source-after-artifact-first.json` SHA-256 is
`35e69e7c25d931b937d9f846b822d17aa98e9b8e3fa4e8af9f8a31b38be11aeb`.
The only post-recovery source change is `src/cli/pack_store.zig`, from
`2ef6fbe99b8b769bad9212e95dd7375d131ec3f6bf969190ef2290b2c9a6895e` to
`d345ba0c94d55045565f0f17e98b033f341b8811b6c506d0de80c78d053cfe6b`.

The immutable prepared function-table token scopes local owner IDs and never
persists. Per-read descriptor ownership and many-owner completion rows avoid
choosing an arbitrary owner of a deduplicated root. Publication demands require
every retained owner outcome to qualify under the same scope and complete key.
Canonical bodies or validated external artifacts must complete before receiving
the publication-root stamp; user roots and caller inlining are unchanged.
Compaction preserves the complete specialization row, including late provenance.

The constant-plan reverse-edge proof is tri-state: callable, noncallable, or
incomplete. Nested callable values are excluded even if consumed into a plain
owner return. Failed, observed, callable, unknown, late/null, Boxy, and
nonportable classes are excluded. Literal rejection still reports and continues
compilation with its erroneous runtime path; failed-certificate support and
mandatory build abortion are not established.

Optional pack offers decline incompatible versions per file. Explicit pack sets
remain strict. Lazy loading is pending/ready/unavailable and becomes ready only
after complete load and indexing success. Failure retains its cause, cannot
retry, and gates both specialization and artifact lookup; owned decoded arenas
and partial maps remain inaccessible until destruction.

## Acceptance protocol

The explicitly released heavy slot was used for targeted off/on reader
IO/artifact-first/OOM, owner/scope/pool, namespace/codec and production guards.
Actual selected-name logs and debug allocator checks, rather than quiet
command success, establish coverage. Both named Pair workflows used verified
configured-prefix binaries.

Fresh same-source RF off/on compilers differ only in the literal feature and
its declared format/identity contract. Each workload has two discarded warmups
and three measured alternating-order samples. Cold builds use a fresh cache
for every sample; warm builds reuse their configuration's cache; app-edit builds
change only an app comment. Child Roc jobs are two, and every Zig build used
`-j2`. Every measured child has a separate 600-second deadline. No HOF timings
from another source snapshot are reused.

Account separately for cold standalone code/body construction, O(nodes + edges)
plan proof, O(owners * specializations) publication stamping and quadratic
`appendUnique` closure deduplication. No indexing redesign is justified before
measurement unless it actually blocks acceptance. Measured results follow;
the aggregate elapsed values do not isolate those individual algorithms.

## Execution blocker after recovery

Three lightweight terminal attempts failed before command execution with:
`the requested worktree version was not materialized because the checkout kept changing`.
This includes a `pwd`/native-host-directory probe, not a compiler failure.
The source tools continued to work. The parent has been notified; no alternate
checkout or donor was edited to bypass the attached-worktree boundary.
The next-turn associated terminal read succeeded, and the parent independently
confirmed materialization was available again. No cause or external actor was
established. This infrastructure event is not a compiler verification failure.

The parent subsequently released the heavy slot explicitly. Validation and
measurement resumed in this associated checkout; no donor or alternate checkout
was edited.

## Actual validation and recovery repairs

| Selected group | Literal off | Literal on | Actual scope |
| --- | ---: | ---: | --- |
| CLI pack guards | 6/6 | 6/6 | Namespace, incompatible peers, strict input, malformed/IO/OOM, artifact-first, identity union and owned encoding |
| Eval | 7/7 | 7/7 | Literal counters, graph proof, owner publication, owned facts and current-source rejection replay |
| Compile producer | 2/2 | 2/2 | Off completed-value case; on production keep-all-false producer, workers one/two and bad-authority decline |
| Postcheck | 4/4 | 4/4 | Borrowed-pool reflection/OOM, frozen clone and late canonical publication |
| Backend | 5/5 | 5/5 | Four codec cases and independent x64/AArch64 artifact producers |
| Feature registry | 4/4 | 4/4 | Two harnesses plus actual legacy vectors and feature identity assertions |
| Publication compaction | 3/3 | 3/3 | Root/provenance preservation plus two harness/import tests |
| Neutral portability | 2/2 | 2/2 | Actual owner/observation/noncallable projection plus core harness |
| Named certified trace | 1/1 | 1/1 | Selected-key binding, not an unrelated ordinary hit |
| Checked-key delegation | 2/2 | 2/2 | Normal/explicit compiler-identity equality plus harness |

Counts include harness/import tests. Some feature-on codec/root assertions
deliberately return without exercising that branch when the gate is off.
No unexpected skips or allocator leaks were reported. Raw commands, deadlines,
binary hashes, summaries and actual selected names are retained under
`.tmp/literal-recovery/validation/`.

Actual gate-off producer compilation found an additional defect:
`publication_requests` inferred a pointer to a zero-length array when the feature
was disabled, but `Allocator.free` requires a slice. The repair is one explicit
`[]const PublicationRequest` type annotation, not a publication-policy change.
The failing log remains `off-producer-build-run.log`; the successful retry is
`off-producer-build-run-attempt2.log`. Producer tests passed again on both gates.

The first registry selection executed only two harnesses; the first LIR selection
executed zero tests. Those apparent passes are withdrawn. The base root now
explicitly references the feature-spec tests; the LIR selector includes its
actual `lir tests` aggregator, and neutral facts run through `lir_core`.
The final selected-name counts above describe actual regressions.

The recovered Debug Pair workflow passed 1/1 in **22.104 seconds total**.
The recovered RF Pair workflow passed 1/1 in **4.897 seconds total**.
Neither is a per-build timing or an off/on comparison. Their unchanged assertions
require certified owning-body reuse, fewer body contexts, zero warm literal
evaluation, exact uncached/cold/app-edit output, same-path reversed caller order,
and correct output after a real converter suffix edit.

Rejection workflows passed 2/2 for each RF configuration. They include dormant
unspecialized code, repeated cached and uncached rejection, current checked/root
attribution, and a rejected callable-containing value. The every-build workflow
checks report count and location, not a universal build-abortion rule.
Failed literal admission remains unsupported; report-and-continue followed by
an erroneous runtime path is not claimed to be replaced.
AllError/always-rejecting converters are explicitly outside the success-only
certificate class. This is a present limitation, not a promise of future
failed-certificate support.

## Frozen identities

The post-guard 925-file inventory is `source-after-guards.json`, SHA-256
`634415c8e718378228623c82848fb6973acbd15180d9df9d569918f4ac99be1a`.
Relative to recovery, its only new source changes are the approved artifact-first
test, the feature-spec test discovery reference and the publication slice type.
The separate compiler/fixture input manifest contains 903 `.zig`/`.roc` and
build files; SHA-256
`08a42ab00dc88a6cf9426d3f83c40537cad189ca631bc1ac4926268c0da05422`.
The inventory and executable-input manifest have different declared scopes.

| Compiler | Mask / codec / feature domain | SHA-256 |
| --- | --- | --- |
| Debug on | 383 / 6 / 3 | `1f93c8aff4e5150848ce7a628de7f935ddb38dab49894ac30e869f33228c5ab9` |
| RF off | 127 / 4 / 2 | `17790a3cd5c36ea3b39fd8e11b60d0126f42620861ffbf4e3159be9f3118d0d1` |
| RF on | 383 / 6 / 3 | `cd43d84804ea3809d7a62651523d5f3057c697f8d7875188a0f3ada9295f24b4` |

Configured options were inspected and their masks asserted before execution.
RF builds took 673.106/674.139 seconds; these are compiler build costs excluded
from application timings. RF executable bytes are 229,729,856/229,795,520.
The native FX host source was byte-equal to the donor; archive SHA-256
`d1c251fa221e22ebab68161849a5ad3ff90c1c227ef5eeccd28f7b1f8c8277d3`
was rechecked and `lipo` confirmed arm64. This is generated-dependency
provenance, not a test pass.

SQL input Actions/Node hashes were checked against the recovered protocol.
Original Actions:
`cdd2d59be288ecce0cbadebd0d703f575aae9bdc359a6ceb4ad697ef89b79120`;
refactored Actions:
`d04e9915ab6971f52932a088202cf5500781855ed4c689f88ee22740ea2536c8`;
shared-null Actions:
`2575bfbbb0387545c0977e44a97de29aa50b054dc5b561980f95797a313181b3`.
`inline` in raw directory labels means the refactored fixture, not a new compiler
transformation. Native SQL checks execute only the exact usage path; database
integration is outside this acceptance.

## Same-source RF measurements

Values are medians of samples 3–5; samples 1–2 are discarded warmups.
Wall time is `/usr/bin/time -l`, with 10 ms printed resolution.
RSS is maximum resident set size in MiB. Diagnostic traces are separate and
excluded. All 160 compiler commands completed without deadlines or compilation
failures; all 130 generated native executions matched exact expected stdout and
empty stderr. Thirty remaining commands are package checks.

| Workload | Off seconds | On seconds | On/off | Off / on RSS MiB |
| --- | ---: | ---: | ---: | ---: |
| Literal no-cache | 0.040 | 0.040 | 1.000 | 69.5 / 69.2 |
| Literal cold | 0.060 | 0.060 | 1.000 | 72.4 / 72.4 |
| Literal warm | 0.030 | 0.030 | 1.000 | 58.4 / 58.6 |
| Literal app edit | 0.040 | 0.030 | 0.750 | 67.4 / 67.0 |
| SQL original check, no-cache | 30.900 | 30.890 | 1.000 | 6102.5 / 6078.0 |
| SQL original cold build | 50.050 | 50.210 | 1.003 | 9029.4 / 9089.5 |
| SQL original warm build | 1.430 | 1.440 | 1.007 | 1102.6 / 1102.2 |
| SQL original app edit | 4.070 | 4.080 | 1.002 | 2136.3 / 2137.4 |
| SQL refactored check, no-cache | 19.050 | 19.170 | 1.006 | 4132.7 / 4156.4 |
| SQL refactored cold build | 31.400 | 31.080 | 0.990 | 6266.9 / 6220.2 |
| SQL refactored warm build | 1.440 | 1.430 | 0.993 | 1043.4 / 1047.8 |
| SQL refactored app edit | 4.260 | 4.200 | 0.986 | 2073.8 / 2076.4 |
| SQL shared-null check, no-cache | 19.080 | 18.930 | 0.992 | 4157.2 / 4150.7 |
| SQL shared-null cold build | 41.780 | 41.860 | 1.002 | 5416.5 / 6123.5 |
| SQL shared-null warm build | 1.470 | 1.490 | 1.014 | 1005.9 / 1011.1 |
| SQL shared-null app edit | 4.160 | 4.190 | 1.007 | 2026.9 / 2038.6 |

The literal app-edit difference is one timer quantum and is not a robust 25%
speedup claim. SQL results move in both directions and show no convincing
whole-compiler wall gain. Shared-null cold off wall samples span 41.57–48.16 s
versus on 41.35–41.89 s. Its RSS spans 5353.9–6116.9 MiB off versus
5479.2–6157.0 MiB on: the higher on median is a measured cost concern, but these
variable, overlapping resident footprints do not isolate allocator consumption
or establish its cause.

Raw results retain all sample values, user/system time, RSS, counters, source
checks, compiler identity and commands. `timing-summary.json` preserves ranges;
`runs/` contains per-command records and boundedly parsed timing logs.

### Measured distributions

Wall triples are samples 3, 4, 5 in order, in seconds. RSS triples are
minimum, median, maximum across those samples, in MiB. All 160 original records,
including discarded warmups, remain in the isolated checkout's `runs/`.

| Workload | Off wall samples | On wall samples | Off RSS min / median / max | On RSS min / median / max |
| --- | --- | --- | --- | --- |
| Literal no-cache | 0.04 / 0.04 / 0.04 | 0.04 / 0.04 / 0.04 | 69.4 / 69.5 / 69.7 | 68.9 / 69.2 / 69.5 |
| Literal cold | 0.06 / 0.06 / 0.06 | 0.06 / 0.06 / 0.05 | 72.3 / 72.4 / 72.4 | 72.0 / 72.4 / 72.5 |
| Literal warm | 0.03 / 0.03 / 0.03 | 0.03 / 0.03 / 0.03 | 58.4 / 58.4 / 58.5 | 58.4 / 58.6 / 58.6 |
| Literal app edit | 0.04 / 0.04 / 0.03 | 0.03 / 0.03 / 0.03 | 67.3 / 67.4 / 67.6 | 67.0 / 67.0 / 67.3 |
| Original check | 30.81 / 31.05 / 30.90 | 30.89 / 30.71 / 30.94 | 5997.9 / 6102.5 / 6175.1 | 6016.0 / 6078.0 / 6111.9 |
| Original cold | 51.04 / 50.05 / 50.04 | 51.36 / 50.09 / 50.21 | 8990.8 / 9029.4 / 9093.8 | 9086.9 / 9089.5 / 9094.2 |
| Original warm | 1.44 / 1.43 / 1.43 | 1.45 / 1.44 / 1.44 | 1100.2 / 1102.6 / 1117.1 | 1085.6 / 1102.2 / 1132.8 |
| Original app edit | 4.08 / 4.07 / 4.07 | 4.07 / 4.08 / 4.10 | 2130.9 / 2136.3 / 2143.7 | 2114.9 / 2137.4 / 2152.7 |
| Refactored check | 18.95 / 19.09 / 19.05 | 19.14 / 19.17 / 19.22 | 4115.0 / 4132.7 / 4213.8 | 4142.8 / 4156.4 / 4174.9 |
| Refactored cold | 31.15 / 31.90 / 31.40 | 30.81 / 31.49 / 31.08 | 6221.5 / 6266.9 / 6321.8 | 6219.3 / 6220.2 / 6248.2 |
| Refactored warm | 1.48 / 1.42 / 1.44 | 1.43 / 1.42 / 1.43 | 1028.8 / 1043.4 / 1077.6 | 1038.6 / 1047.8 / 1067.8 |
| Refactored app edit | 4.18 / 4.30 / 4.26 | 4.20 / 4.25 / 4.15 | 2068.1 / 2073.8 / 2089.4 | 2074.3 / 2076.4 / 2115.6 |
| Shared-null check | 19.06 / 19.08 / 19.30 | 18.92 / 18.93 / 19.22 | 4090.0 / 4157.2 / 4171.8 | 4140.5 / 4150.7 / 4202.8 |
| Shared-null cold | 48.16 / 41.57 / 41.78 | 41.86 / 41.35 / 41.89 | 5353.9 / 5416.5 / 6116.9 | 5479.2 / 6123.5 / 6157.0 |
| Shared-null warm | 1.45 / 1.47 / 1.47 | 1.47 / 1.49 / 1.49 | 999.4 / 1005.9 / 1020.6 | 1002.7 / 1011.1 / 1033.4 |
| Shared-null app edit | 4.16 / 4.14 / 4.16 | 4.20 / 4.19 / 4.18 | 1997.5 / 2026.9 / 2034.7 | 2036.0 / 2038.6 / 2081.9 |

## Reuse and cost evidence

The separate RF app-edit trace joins both exact `PlainValues.pair` census keys
to certified hits (`bedc5efa7fd1a1a7`, `a3e3aa4804a81da8`) and records zero
actual literal evaluations. A fully unchanged warm main can consume its own
certified closure instead, without demanding Pair again; it is not counted
as a named Pair hit. Each SQL warm diagnostic records one certified hit and
zero actual literal evaluations, with exact usage output. Trace-only SQL logs
are about 104 KB each, not multi-gigabyte census dumps.

Literal-on cold/no-cache evaluates four actual literal roots; warm and app-edit
evaluate zero. SQL-on cold evaluates twenty, versus zero warm. The actual-literal
counter is gate-dependent: missing off counters are **unavailable**, not zero.
For original SQL sample 3, warm shared Monotype body contexts are 42,136 off
and 42,134 on. Literal warm body contexts are three in both configurations.
These aggregate counts do not imply a broad lowering reduction.

| Cold footprint, bytes | Literal off / on | Original off / on | Refactored off / on | Shared-null off / on |
| --- | ---: | ---: | ---: | ---: |
| Native `.rpk` packs | 38,004 / 40,562 | 20,578,913 / 20,612,852 | 17,425,860 / 17,458,570 | 17,447,968 / 17,480,678 |
| All regular cache files | 1,317,167 / 1,319,725 | 339,129,542 / 339,163,481 | 294,732,413 / 294,765,107 | 274,927,561 / 274,960,255 |
| Generated native executable | 432,784 / 449,456 | 5,401,488 / 5,419,056 | 5,401,488 / 5,419,056 | 5,401,488 / 5,419,056 |

Native pack bytes were captured per timed run. Total cache bytes were counted
afterwards in the unique cold caches, each used only once; all three measured
cold samples have the same footprint per configuration. The initial legacy
`.rcache`-suffix counter did not capture extensionless current cache files;
its zero values are not interpreted as an empty checked cache.
Native pack bytes include metadata/certificates, not just machine code;
executable size also includes alignment. No instruction-only attribution or
isolated allocation profile is claimed.

Intentional cold costs remain: standalone owner body construction and code
publication even when callers inline; the linear reverse-edge constant-plan
proof; owner-by-specialization stamping scans; and per-closure quadratic
`appendUnique` deduplication. Warm hits save literal evaluation and eligible
body work, but the data does not justify a new indexing implementation or
default-on rollout.

## Observed-converter equivalence failure

`Values.roc` produces `[dbg] "left"`, `[dbg] "right"` twice during cold conversion.
Both configurations execute the generated app with the same exact stdout.
The fully warm compile-time transcript differs:

| Observed converter | Off | On |
| --- | ---: | ---: |
| Cold debug messages | 4 | 4 |
| Fully warm debug messages | 0 | 4 |
| App-only edit debug messages | 4 | 4 |
| Fully warm certified observed hits | 0 | 0 |
| Fully warm shared body contexts | 3 | 33 |
| Fully warm worker literal tasks | 0 | 4 |
| Cold finalized runtime pack offers / withheld | 1 / 0 | 0 / 1 |

This is not a certified observed hit or partial-reader failure. Off warm consumes
ordinary `main!` key `8393f5263d09f429`; on main key `54756e5840721ff8` misses
because its closure is withheld. The unchanged converter's debug transcript
then reappears through actual evaluation. Cache namespaces and exact compiler
identities remain fixed throughout each configuration.

The [normal publication filters](../src/cli/main.zig#L8998) inspect surviving
converter and program-symbol references. After values are embedded, those
references are gone. Only literal-on applies the additional
[runtime-use completion index](../src/cli/main.zig#L9003), whose
[ineligible-emitter rule](../src/cli/finalized_literal_packs.zig#L68) rejects
debug-observed outcomes and their artifact closures.

Literal native debug events are
[emitted immediately](../src/eval/compile_time_finalization.zig#L2494) and
optionally retained in the ephemeral literal outcome store. They are not the
checked-root events represented by the existing
[persisted/replayed debug store](../src/eval/compile_time_finalization.zig#L148),
which uses checked `ComptimeRootId` authority. Ordinary folded entry reuse
therefore lacks portable literal-observer attribution.

The initial trace driver wrongly assumed four warm off messages and stopped.
That assertion is withdrawn, not normalized into a pass. Separate bounded
captures established the off/on difference and preserved both raw transcripts.
`observation-equivalence.json` explicitly records acceptance as false.
The trace collector now compares configurations rather than inventing a
baseline replay policy. No language rule granting zero warm observations
has been assumed.

A coherent common repair needs producer-owned observer eligibility/provenance
or a declared persisted replay/demand-attribution contract across both
configurations. Suppressing on debug or ignoring observed runtime-use rows
would merely relax the admission proof. That broader unsupported-observation
contract was not implemented as a local workaround. Failed, expect/debug,
callable, incomplete, late/null and Boxy classes remain outside the admitted
success class; this report does not promise support for them.

## Recommendation and handoff

Retain default-off experimental source, preserve the successful narrow-class
reuse and reader/lifecycle repairs, and **do not mark the trial fully accepted**.
There is neither convincing SQL wall benefit nor full observed-diagnostic
equivalence. The rejected-stage RF source, binary hashes, raw measurements and
failure evidence remain intact for review; any future repair needs a new source
identity and fresh measurements rather than overwriting these results.

Compact raw evidence is preserved as
`.tmp/literal-recovery/rejected-stage-evidence.tar.gz` in
`/Users/rtfeldman/code/roc/.delta/worktrees/px2f4v4snxbe/roc`.
It contains 1,137 metadata/log/output files, excludes compiler/application
binaries and cache contents, and is 1,272,600 bytes. SHA-256:
`021733f7f02862062b261cbe3ccfb57ebeff2a6cc12bb744db4ce8bd5f1426bb`.
`evidence-archive.json` records its scope and location. The source inventory,
903-input freeze and RF binaries were reverified unchanged at handoff;
`zig fmt --check` passed for the post-recovery changed Zig files.

No Git diagnostics, history operations, publishing or external-service comments
were performed. Current nominal/import/golden and HOF fixes were preserved.
