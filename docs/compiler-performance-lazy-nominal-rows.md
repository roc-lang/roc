# Lazy closed nominal rows: implementation evidence

## Status

Persistent storage passed its narrow validation matrix. Production solver
admission and semantic consumers are not installed. There is no optimization
acceptance or performance claim. Boxy caching is excluded.

The inherited fourteen foundation cases passed serialized Debug/ReleaseFast
gate-off/on runs. They exercise the resumable opening API, not persistent Store
publication or ordinary solver admission.

## Persistent storage validation queue

The inherited four persistent tests cover rollback, failed capture cleanup,
typed serialization with demand after source destruction, and failed demand
invalidation/recovery. Additional source coverage now exercises:

- Nonidentity schema-root translations, including the closed tail.
- Nonempty legacy relocated Store serialization.
- Reconstructed persistent maps containing recursive dispatch/effect edges.
- Independent opening ownership after restoring a sparse Store.
- Allocation failure at schema translation publication, fragment append and
  latent rank-history writes, with paired savepoint rollback.
- Typed frozen-table byte equality despite spare capacity and destination poison.

All seven persistent cases passed with the eight foundation type cases and
two harness cases in Debug and ReleaseFast, with the gate off and on:
17/17 type tests per configuration. No storage implementation repair was needed.
The checked-artifact layout version remains 108; the exact version-hash golden
was regenerated from the initial intentional-layout assertion failure:
`5d6613482fce9c685f6798812bba6921d5c87bcb032fce1196faae97c4493f8e`.

Nine additional checker cases pin repeated visible payload equality,
omitted-payload conflict, constructor arity, non-tag backing, imported opacity,
intermediary import with aliases and independent openings, recursive occurs
through an omitted payload, escaped monomorphic payload sharing, and retention
of an open backing's original extension. All fifteen semantic cases passed
alongside the golden and one harness case in each of the same configurations:
17/17 checker tests per configuration.

Initial checker validation found two fixture errors, corrected before any
activation: the relay helpers needed an explicit `Relay` type namespace to
export, and the existing eager omitted-payload cycle diagnostic is
`Anonymous Recursion`, not `Infinite Type`. Initial failures and repaired runs
are retained; expected semantics were not changed to accept an optimized result.

The intermediary fixture tests ordinary transparent aliases, not contextual
`alias_source_mapping` replacement. The historical empty-complement negative
already constrains its tail to `None`; it alone cannot prove omitted `Message`
retention. Direct contextual-replacement coverage and paired no-nominal/nominal
complement-loss controls remain required before descriptor admission.

Raw logs and the preparation manifest are retained under
`.tmp/lazy-nominal-validation/` in this thread's Roc worktree. The serial commands
use `zig build -j2 run-test-zig-module-types` with filter `nominal opening`, and
`run-test-zig-module-check` with filters `late nominal` and
`SERIALIZED_VERSION_HASH golden value`, with `-Doptimize=Debug`/`ReleaseFast`,
`-Dperf-lazy-nominal-rows=false`/`true`, and custom per-configuration `.tmp`
cache/prefix paths. These are correctness results, not compiler benchmarks.

## Post-barrier source slice

The current working tree adds an explicit insertion/replacement binding delta
at the instantiator's map-write operation. Persistent demand publishes that
delta, rather than scanning and republishing every seeded map entry. New-key
release insertion uses producer proof; replacements keep the existing map rule.
The serialized layout is unchanged and remains version 108.

This slice passed Debug gate-off only: 19/19 type tests and 20/20 checker tests,
including two delta tests, the real contextual-alias replacement fixture and
paired complement-loss controls. The first contextual fixture run failed
19/20: raw `instantiateVar` did not produce the scheme reachability prewalk.
The fixture was corrected before activation to call `instantiateTypeScheme`;
the existing eager mixed-graph behavior then passed. The original failure log
is `context-check-debug-off.log`, the passing log is
`context-check-debug-off-repaired.log`, and the type log is
`delta-types-debug-off.log`. Gate-on and ReleaseFast reruns for this new slice
have not run.

The direct fixture exercises two explicit alias declaration identities mapping
to one constrained private rigid replacement, preservation of independently
copied identity children, non-reintroduction of discarded wrapper constraints,
independent opening demands in different orders and successive fresh-flex /
fresh-rigid scheme copies. It does not assert an implemented lazy contextual
producer: that producer is still absent.

`nominalCaptureHasSharedBoundary` is an unused producer-certificate prewalk
helper. It has not been validated against SQL schemas and cannot establish
useful admission counts. Placeholder declaration flags and a speculative
version-109 stamp were removed before this validation.

No immutable capture producer, delayed solver descriptor, production admission,
logical semantic-consumer activation or performance measurement is installed.
The original activation assignment remains incomplete. The current blocking
engineering work is complete logical-reference rank/generalization/copy/import
integration and its proof-complete producer, not the validated storage API.

## Production integration contract

The delayed fragment is an explicit descriptor, never an ordinary flex.
`Store.resolveVar` stays infallible and allocator-free. Admission applies to
ordinary structural rows against accessible valid closed nominal tag backing;
source provenance is irrelevant. Open or non-tag backing keeps existing rules.

Rank/generalization and occurs traversals require opening-scoped logical edges,
including omitted payloads. Mutable relations demand through one opening-owned
map. Independent openings isolate private cells; escaped monomorphic nodes
remain shared across scheme uses. Publication retains persisted orphan arrays
without materializing all complements.

Importer production must retain complete immutable schema correspondence before
ephemeral copy maps disappear. Immutable eligible declaration metadata is
separate from actual/owned solver mappings and per-copy opening ownership.
Canonical schema translations compose across intermediary imports; destination
backings cannot be zipped to recover correspondence.

An ordinary import's contextual alias substitution can discard a wrapper's
descriptor and edges. Canonical keys alone cannot recover that immutable shape.
The eligible-schema producer must retain its logical template before replacement
and retain replacement-owned references as explicit contextual substitutions,
including the existing importer's independently copied platform identity children.
It cannot recreate discarded constraints or derive a wrapper from a stripped
destination graph. The current storage-only implementation has no such producer.

Consumers requiring integration include type generalization/instantiation,
unification/occurs/effects/dispatch, checker solved-graph traversals,
diagnostics/exhaustiveness, canonical type keys/checked artifact traversal, type
views and import copying, and downstream solved-type/Monotype consumers.
Literal certification/ownership APIs and higher-order cache-key policy remain
outside this change. Boxy checked-type consumers still require representation
correctness when the gate is on, despite Boxy caching remaining excluded; a tiny
gate-off/on `--specialize=no` control belongs to activation acceptance.

## Algorithmic risk to measure

The approved capture trial uses the existing mapper for one reachable immutable
template graph per eligible declaration, not a whole-Store clone or scan.
`DenseMap` pages values but its outer chunk-pointer vector spans touched ID
ranges. This does not prove O(reachable roots) memory or work for widely
separated IDs. Record the inherited mapping span/allocation cost separately;
do not redesign the global mapper or hash ordinary Store-local IDs before
measuring whether this cost blocks acceptance.

Persistent demand currently rebuilds session substitution/name/history maps
from stored linked lists and republishes the session's complete binding map.
`putBinding` scans the opening's chain, so full demand can incur quadratic
duplicate-publication work. This is source analysis, not measured overhead.

Before adding runtime indexing and another rollback state, consider an explicit
producer delta that publishes only newly produced or changed bindings. Such a
delta must preserve updates and associated-name substitutions; insertion-only
behavior cannot be assumed. Scratch reconstruction still costs work proportional
to existing roots. Acceptance includes these costs, with deterministic work-count
tests rather than timing thresholds where a complexity contract is established.

## Measurement rule

After correctness, use paired same-source ReleaseFast gate-off/on whole-compiler
cold check, cache construction, warm reuse and genuine app-only edit workflows.
Use original/refactored/shared-null parser inputs plus ordinary regression
controls, fresh cache roots, two warmups and three measured repetitions.
Record timings, RSS and artifact bytes, including schema retention and
publication cost. No measurements have been run for this implementation.
