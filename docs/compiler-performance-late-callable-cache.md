# Late callable object reuse: implementation evidence

The opt-in `late_callable_cache` path retains the early higher-order exclusion.
Monotype publishes an explicit reservation seed carrying the existing source,
request, evidence and codec identity. Direct LIR combines it with its completed
procedure identity in `roc.object.late-callable.v1`, using the same key for
lookup and publication. The pack table and its ARC signature encoding are
unchanged; app-derived artifacts remain app-filed. No solver mutation, earlier
callable analysis, inlining-policy change or Boxy portability extension is added.

## Individual acceptance

Debug compilers built with the gate independently on and off passed:

- Procedure identity and late-key unit tests, including nested callback fields,
  nominal backings, returned function signatures, capture ABI, and recursive
  unfolding/query-order stability.
- Existing pack tests withholding function-pointer constants, specialized
  literal-conversion closures and Boxy program-local dependencies.
- `test/cli/late_callable_cache/check.py`, with `--expect-hits` for the on
  compiler and without it for the off compiler.
- `zig fmt --check` for all changed Zig files.

Every Zig build used `-j2` and custom `.tmp` cache directories; every Roc child
used `--jobs=2`. The workflow stages its own sources/cache, preserves package
SHA-256 hashes through app-only edits, and checks exact runtime output against
an uncached oracle. It also checks compile-time debug replay, failed expect and
observed incomplete-match diagnostics, and cached/repeated/uncached rejection
of the existing literal-conversion fixtures.

Observed on-feature work counts:

| Workflow | Late hits | Remaining LIR bodies skipped |
| --- | ---: | ---: |
| App-only comment edit, imported higher-order helper | 1 | 1 |
| Same closure code, distinct runtime capture values; returned closures | 2 | 2 |
| Callback body edited, unchanged arrow/layout | 0 (1 miss, new identity) | 0 |
| Comment edit after callback change | 1 | 1 |

Off-feature runs emitted no late lookups or late body skips. Successful execution
also exercises the existing native-codegen invariant: an external procedure
must already have its artifact spliced, and does not enter native body emission.

## Measured limits

The existing plain finite admission remains unchanged: no procedure captures,
SpecConstr clone, erased ABI or return-reuse ABI. The nested-record/nominal-field
fixture produces four constructor-pattern SpecConstr clones; tracing identifies
them as explicitly ineligible, and neither cold nor warm performs late lookup.
Identity support does not waive that admission restriction.

A joined finite callable set can share one procedure across both callbacks.
Reversing caller order can change its producer-owned member order and thus its
tag ABI; the completed identity changes and correctly misses. Repeated
single-target requests retain identity across reversed build order. Sorting
only the identity would incorrectly alias code with different discriminants,
so no such normalization was introduced.

The feature avoids work only after complete identity is available. It does not
skip Monotype, lambda solving or other preceding analysis, and unchanged-app
checked replay is not the acceptance workflow. The ReleaseFast checkpoint-bundle
pair below confirms work avoidance but does not establish a material SQL
wall-clock benefit. Keep the gate off by default and independent of `perf-all`.

## Reproducible commands

```sh
ROC_CACHE_DIR="$PWD/.tmp/late-callable-build-roc-cache" \
zig build -j2 roc run-test-zig-module-postcheck \
  --cache-dir .tmp/late-callable-zig-cache \
  --global-cache-dir .tmp/late-callable-global-cache \
  --prefix .tmp/late-callable-on -Dperf-late-callable-cache=true \
  -- --test-filter 'procedure identity' --test-filter 'late callable cache key'

python3 test/cli/late_callable_cache/check.py \
  --roc .tmp/late-callable-on/bin/roc --expect-hits
```

Repeat with `-Dperf-late-callable-cache=false`, a separate off prefix, and omit
`--expect-hits`. The existing closure tests are selected with
`run-test-zig-cli-main -- --test-filter 'pack withholds' --test-filter 'pack offers'`.
Native execution requires the fixture's `fx-open` host library; prepare it with
the targeted `build-test-hosts -Dplatform=fx-open` step.

## ReleaseFast checkpoint-bundle counterexample

The first ReleaseFast pair used all seven checkpoint gates: mask 127 with late
reuse off and mask 639 with it on. Both compilers passed the identity/key/pack
unit tests and the original small higher-order workflow. Paired helper and
captured-closure no-cache, warm and comment-edited runs were retained, with
separate caches, two warmups and three measured repetitions.

The original SQL workload then exposed an admission bug: the first mask-127
cold app build succeeded, while mask 639 aborted at native emission for
procedure 27 (`list_map_can_reuse`) and procedure 29 (`Builtin.List.map`).
Both procedures had no standalone statement body. The aborted 45.39-second
run is excluded from performance comparisons.

The source contract, not a backend relaxation, explains the failure. Worker
representation preparation reserves procedures for inlined calls without
requesting their standalone bodies. Late publication initially offered those
reservations unconditionally. `runKeepingSpecializations` treats every offered
specialization as an explicit pack root, promoting these unused reservations
into invalid native demands. The diagnostic reproduced the missing-body rows
without any late lookup or body-skip trace; it is therefore not evidence that
an actual cache hit skipped a required callback.

The scoped correction retains a pending key on each reservation and publishes
it after final lowering only when emitted demand queued the canonical owner
and that owner completed its implementation. A reached alias can queue its
owner without setting the owner's own reach bit, so publication uses the
canonical queue/completion proof. Body or external-artifact authority is an
assertion after demand, never a body-null eligibility filter. Ordinary keys,
inlining, `ReachableProcs` closure, native assertions and lookup keys remain
unchanged. The publication unit pins reserved-only versus completed normal or
external implementations, including alias-before-owner demand. The compact
closed-`List.map` pack fixture fails with the rejected RF639 binary (missing-body
procedures 8 and 10) and passes cold, warm and uncached with the corrected
compiler. The original SQL cold-build counterexample also passes after the fix.

The rejected snapshot is physically retained at
`.tmp/late-callable-rf-pair/frozen-source`, with its complete 860-file manifest
SHA-256 `8408a6a9c2d3dad7765dc1a652c51e5552c152d831a02bf4def24cfb37c0f7d1`.
The original failed command, cache, binary hashes and raw logs remain unchanged.
The diagnostic log is 12,464,993,583 bytes; a whole-file metrics regex stalled
after Roc had exited. That harness failure is not compiler elapsed time.
The corrected stage uses linear, bounded-line streaming metadata parsing and
a new `.tmp/late-callable-rf-remeasure` evidence directory.

## Corrected ReleaseFast acceptance and measurements

The corrected source is a separately frozen 862-file manifest:
`2ed01a44ea269794e1ad1d16327a4bde123adc30712576ef7c05b72686836f95`.
Only the publication correction, its unit regression and focused fixture/driver
additions differ from the rejected 860-file compiler/fixture snapshot.
No literal-certification or delayed-row trial source was introduced.

Both ReleaseFast compilers used `-Dperf-all=true`; late reuse was explicitly
false for mask 127 and true for mask 639. The generated build options were
checked against those exact masks. Both binaries are 229,713,440 bytes:

| Mask | Compiler SHA-256 |
| --- | --- |
| 127 | `030bc18a015466dce72bb5e7783f02fc9ef911449b9bbbec7e230cdcd6ce8369` |
| 639 | `1829a98446b38f594bceefa81c2bdef38d71c07fb5e5aac6a9bebaae906b8dd5` |

Corrected publication/identity/key/pack-exclusion unit tests pass in both RF
configurations, and the corrected publication unit also passes Debug/on.
The complete CLI workflow passes RF/off and RF/on: exact runtime output,
callback-body invalidation, ordered multi-target misses, single-target order
stability, captured/returned closures, explicit SpecConstr exclusion,
compile-time observations, literal rejection and closed-map pack production.
The original SQL cold RF639 regression succeeds with exact runtime output.
All original/refactored/shared package expectation runs pass all 383 expects
under both masks. The three small projection/nominal controls pass tests and
uncached checks under both masks.

### Measurement protocol

Each measured group has two excluded warmups and three measured repetitions,
with alternating off/on order and separate caches. Cold builds use a fresh
cache per repetition; warm builds reuse an unchanged checked app; edited builds
change only an app comment. Timed runs disable census/pack diagnostic output.
Separate diagnostic builds connect named procedures to actual late lookup and
LIR-body skip identities. Every SQL build executes the no-database usage path
and checks exact stdout and empty stderr; no database/network workload is run.

SQL inputs were restored from the current read-only `roc-pg` snapshot at
`/Users/rtfeldman/code/roc-pg/.delta/worktrees/w64q0023902x/roc-pg`. Three generated
copies live under the measurement root's `inputs/sql/{original,inline,shared}`.
Original replaces only `Actions.roc` and `Node.roc` with the frozen
`.ctfe-investigation/baseline` files. Inline copies the current package unchanged.
Shared restores the historical action file by changing exactly 179 eligible
`Null` tokens before the unique hand-written suffix marker, preserving error
tags and match patterns, then appending the original suffix unchanged. It
changes 162 lines and produces exactly 765,534 bytes. Full hashes, not this
reconstruction recipe alone, admit each input:

| Input | Actions SHA-256 | Node SHA-256 |
| --- | --- | --- |
| Original | `cdd2d59be288ecce0cbadebd0d703f575aae9bdc359a6ceb4ad697ef89b79120` | `581da93bb16cc146b5f86d5402fdd753abbfa56250c8d0cfce0a307b7b2b874c` |
| Inline/refactored | `d04e9915ab6971f52932a088202cf5500781855ed4c689f88ee22740ea2536c8` | `f1bf21d8ec43fe8d5d4a0a049d77caad34f6579c0eac53fc48e9a734369f0603` |
| Shared null | `2575bfbbb0387545c0977e44a97de29aa50b054dc5b561980f95797a313181b3` | `f1bf21d8ec43fe8d5d4a0a049d77caad34f6579c0eac53fc48e9a734369f0603` |

The unchanged app hash is
`3c938c3e3e572ddb22b44c91a28d9edc4af9313153ceacc8d7ec07750ac70685`.
Actual staged package hashes are checked after app edits; historical inputs
are never edited. The linear streaming metrics reader bounds diagnostic lines;
each measured compiler invocation has a 600-second process-group deadline.

### Wall-clock distributions

Seconds below are median `[minimum, maximum]`, excluding both warmups.

| SQL input | Mode | Mask 127 | Mask 639 |
| --- | --- | --- | --- |
| Original | Uncached package check | 32.61 [32.59, 32.65] | 32.75 [32.66, 32.96] |
| Original | Cold app build | 52.74 [52.42, 57.86] | 52.57 [52.03, 53.20] |
| Original | Unchanged warm build | 1.48 [1.48, 1.51] | 1.48 [1.48, 1.50] |
| Original | App-only comment edit | 4.19 [4.17, 4.20] | 4.18 [4.18, 4.19] |
| Inline/refactored | Uncached package check | 19.62 [19.16, 20.37] | 20.06 [19.25, 20.31] |
| Inline/refactored | Cold app build | 31.95 [31.85, 32.38] | 31.96 [31.57, 32.34] |
| Inline/refactored | Unchanged warm build | 1.48 [1.47, 1.51] | 1.47 [1.47, 1.48] |
| Inline/refactored | App-only comment edit | 4.25 [4.23, 4.27] | 4.24 [4.15, 4.25] |
| Shared null | Uncached package check | 19.90 [19.85, 19.99] | 19.90 [19.87, 20.29] |
| Shared null | Cold app build | 40.38 [40.14, 41.66] | 41.42 [40.87, 53.24] |
| Shared null | Unchanged warm build | 1.46 [1.46, 1.47] | 1.46 [1.45, 1.47] |
| Shared null | App-only comment edit | 4.16 [4.14, 4.17] | 4.14 [4.12, 4.22] |

The shared-null cold distribution includes a 53.24-second on-feature outlier;
it is retained, not discarded. Three samples do not establish a reliable
speedup or zero overhead. The nominal/projection uncached-check controls have
equal off/on medians of 0.01, 0.06 and 0.02 seconds respectively.

### Higher-order work avoidance

The tiny helper/capture workloads have a 0.03-second median for both masks in
uncached, unchanged-warm and comment-edited modes. Timer granularity and linking
dominate these workloads; their result is code-work avoidance, not wall speedup.
Their measured wall ranges are 0.03–0.03 seconds, except the uncached on-feature
helper range of 0.03–0.04 seconds; that sample remains included.

| Edited workload | Named late hits / LIR bodies skipped | Native procedures emitted, off → on | Native bytes emitted, off → on | Shared Monotype body contexts |
| --- | --- | --- | --- | --- |
| Imported helper | `Helpers.apply`: 1 / 1 | 3 → 2 | 1,264 → 992 | 29 in both |
| Distinct capture values / returned closures | `Helpers.apply`, `Callbacks.make`: 2 / 2 | 5 → 2 | 1,520 → 1,008 | 36 in both |

The captured callback's implementation can be carried in an admitted helper
artifact's proven dependency closure; this does not create standalone
captured-procedure entries or waive the no-procedure-captures admission.
Exact outputs distinguish runtime environments with the same code and ABI.

For all three SQL comment-edited workloads, runtime native procedure emission
is 754 → 727 and emitted bytes are 3,821,116 → 3,778,048. Shared native emission
is 1,077 → 1,049 procedures and 4,233,964 → 4,213,108 bytes. Ordinary early
specialization hits remain 503 in both masks; shared Monotype body contexts
remain approximately 68,140–68,178. These count changes do not show avoided
earlier analysis or a material elapsed-time improvement.

### Memory and persisted artifacts

Median maximum RSS values are bytes, measured by macOS `/usr/bin/time -l`.
Conversions use decimal GB = 1,000,000,000 bytes and binary GiB = 1,073,741,824
bytes; the table deliberately retains exact byte counts:

| Input | Cold build, off → on | Comment-edited build, off → on |
| --- | --- | --- |
| Original | 8,926,396,416 → 9,509,011,456 | 2,227,060,736 → 2,249,916,416 |
| Inline/refactored | 6,524,928,000 → 6,547,226,624 | 2,189,770,752 → 2,165,604,352 |
| Shared null | 6,411,845,632 → 6,455,574,528 | 2,158,477,312 → 2,142,781,440 |

Cold-build maximum-RSS ranges, also exact bytes:

| Input | Mask 127 minimum–maximum | Mask 639 minimum–maximum |
| --- | --- | --- |
| Original | 8,517,107,712–9,519,005,696 | 9,444,360,192–9,653,862,400 |
| Inline/refactored | 6,517,342,208–6,553,223,168 | 6,533,824,512–6,594,166,784 |
| Shared null | 6,376,308,736–6,432,030,720 | 6,406,307,840–6,462,472,192 |

The original cold RSS increase is retained as a measured limit; no general
memory-saving claim follows. Warm RSS medians are approximately 1.05–1.16
decimal GB (0.98–1.08 GiB).
Full per-sample RSS and phase/counter distributions are retained in the raw
results and `summary.json`.

Persisted `.rpk` bytes after the five comment-edit builds:

| Workload | Packs in either mask | Bytes, off → on |
| --- | ---: | --- |
| Imported helper | 7 | 25,912 → 24,857 |
| Captures / returned closures | 7 | 25,193 → 24,443 |
| Original SQL | 47 | 49,229,513 → 49,158,450 |
| Inline/refactored SQL | 47 | 46,076,460 → 46,005,397 |
| Shared-null SQL | 47 | 46,098,568 → 46,027,505 |

Complete cache bytes/file counts, including checked data and downloaded
dependencies, are retained separately in `artifact-bytes.json`.

### Evidence and disposition

Corrected evidence root:
`/Users/rtfeldman/code/roc/.delta/worktrees/25ny83kxzdxv/roc/.tmp/late-callable-rf-remeasure`.
It retains `source.sha256`, `source-change.json`, `builds.json`,
`recovered-inputs.json`, `summary.json`, `artifact-bytes.json`, every command,
complete stdout/stderr, runtime output and per-run result. Named hit identity
joins are retained in `runs/hof-diagnostic-*-on-2/named-late-hits.json`.
The rejected stage and its initial Debug evidence remain separate.

The implementation is correctness-validated for its admitted class and avoids
measured downstream code work. The paired SQL results do not justify enabling
it in the default seven-feature bundle. Retain the opt-in gate and the explicit
capture, ordered multi-target ABI, SpecConstr, relocation, literal-validation
and observation limits.
