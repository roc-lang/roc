# Immutable Specialization Identity

## Problem

The identity of a Monotype specialization — the key that decides whether a
call site reuses an existing lowered function or creates a new one — has been
redefined four times in six weeks, and the current definition is still wrong
in one fundamental way: **the key is mutable**. A specialization's identity
includes its closed monomorphic function type, but that type is an *output*
of lowering the specialization's own body (body evidence refines the
requested type). So the compiler rekeys specialization records mid-flight,
maintains dual "request"/"solved" lookup entries, and synchronizes at least
four parallel lookup structures by hand inside
`src/postcheck/monotype/lower.zig`.

The four redefinitions:

1. **Pre-rework:** identity was an `Ast.FnTemplate` value (fn-def reference +
   SHA-256 digest of the source type + an un-interned mono `TypeId`). The
   Monotype builder compared these by struct equality, but the closure lifter
   *recomputed* digests and compared those — two inconsistent equality
   relations over the same conceptual key. Result: issue roc-lang/roc#9519,
   "Monotype function template was assigned two lifted function ids" (that
   invariant string still exists, reworded, in
   `src/postcheck/monotype_lifted/lift.zig`), flaky depending on file-split
   order.
2. **PR roc-lang/roc#9639:** an explicit `Ast.FnId` assigned once at lowering
   time, so downstream passes stop re-deriving identity.
3. **PR roc-lang/roc#9848** (+16,516 / −4,986 lines across 57 files,
   motivated by issue roc-lang/roc#9802: seconds spent specializing a small
   generic higher-order app — the pre-fix lookup scanned every
   `LoweredTemplate` in a family and recomputed a full recursive SHA-256
   `typeDigest` per candidate, roughly O(n³) in call sites): a new
   `SpecBuilder` in `src/postcheck/monotype/specialize.zig` keyed by
   `SpecLookupDigest`, with exact structural `typeEql` as the collision
   authority; `src/postcheck/monotype/type.zig` became a proper interner with
   per-node cached child-derived digests; draft graph-owned type cells sealed
   once (`DraftTypeCell`, defined in `lower.zig`); and
   `src/postcheck/monotype/serialize.zig` added an on-disk specialization
   cache (magic bytes `ROCSPEC`).
4. **Separately, PR roc-lang/roc#9593** added
   `src/postcheck/monotype_lifted/spec_constr.zig`, which keys *its*
   specializations by call-pattern shape (`CallPattern`) — a second
   specialization space with its own identity rules.

Each redefinition fixed real bugs and introduced new ones. The last two fixes
in this area produced fresh regressions: issue roc-lang/roc#9889 (roc-parser
`numbers.roc` crashes in monotype_lifted after PR #9848) and issue
roc-lang/roc#9890 (`letters.roc` crashes in monotype lowering after PR
roc-lang/roc#9873). The root cause is not any single bug; it is that identity
mutates, so every structure that indexes by identity needs a second entry and
a manual synchronization step, and every new consumer gets one of them wrong.

## Background

The compiler pipeline: parse → canonicalize → type-check (produces "checked
artifacts" per module) → postcheck: **Monotype IR**
(monomorphization/specialization, `src/postcheck/monotype/` — `lower.zig` is
~21k lines and owns type lowering, dispatch resolution, specialization, and
statement lowering) → **Monotype Lifted** (closure lifting; also
`spec_constr.zig` call-pattern specialization for optimized builds) →
**Lambda Solved** → **Lambda Mono** → **LIR** → **ARC** → backends.

`design.md` at the repo root is authoritative. Its "Monotype Specialization"
section documents the intended shapes: `SpecIdentity` (callable identity +
source-fn-type digest + closed mono-fn-type digest + mono `TypeId`),
`SpecStatus` (`reserved` → `lowering` → `ready`), and `SpecRecord`. These are
implemented in `src/postcheck/monotype/ast.zig` (`CallableIdentity`,
`SpecIdentity`, `SpecStatus`, `SpecRecord`).

Why the type is an output: Monotype lowering solves each specialization in an
instantiation graph (`src/postcheck/monotype/solve.zig` — "Monotypes are
materialized views of solved nodes, refilled in place when their node gains
evidence"). A template is *reserved* at the requested type, its body is
lowered, and body evidence (numeric defaults, row evidence, empty-tag-union
resolution) can produce a more specific *solved* type. Ready entries flow
their solved type back into requesters (see the comment above
`Builder.lowerTemplateWithMonoFor` in `lower.zig`). `design.md` also fixes
the one-way
snapshot rule: "A Monotype imported into another specialization's graph is a
finished snapshot, never a refreshable view" — a specialization that needs
more than its requested type is a unification conflict, not a silent rewrite.

## Evidence

All in `src/postcheck/monotype/` unless noted; symbols verified.

- `lower.zig` `Builder` fields — the manually-synced lookup structures:
  `spec_store: specialize.SpecBuilder`, `lowered_template_lookup:
  AutoHashMap(TemplateSpecLookup, ArrayList(u32))`, `lowered_nested_lookup:
  AutoHashMap(NestedSpecLookup, ArrayList(u32))`, the generated-helper caches
  `inspect_defs`/`equality_defs`/`hash_defs` (keyed by
  `GeneratedHelperDefAddress`), plus side indices `lowered_template_by_fn`,
  `lowered_template_by_def`, `lowered_nested_by_fn`.
- Dual lookup kinds: `SpecLookupKind = enum { request, solved }`;
  `TemplateSpecLookup`/`NestedSpecLookup` embed it; `findLoweredTemplate`
  probes `.request` first, then `.solved` (ready entries only).
- `lower.zig` `markTemplateReady`: mutates the `LoweredTemplate` in
  `lowered_templates`, rebuilds the record's `SpecIdentity` with the solved
  type, calls `spec_store.updateLocalIdentity` to REKEY the spec record
  mid-flight, appends a second `.solved` lookup entry via
  `appendTemplateLookup`, and `markReady` pokes the record. The nested-fn
  path repeats the same dance (two more `updateLocalIdentity` call sites).
- `specialize.zig` `SpecBuilder.updateLocalIdentity`: overwrites
  `record.identity` and appends the record under the new
  `SpecLookupDigest` — while the entry under the *old* digest is never
  removed, so one record is reachable from two keys.
- Direct status pokes bypassing the builder API:
  `self.program.specs.items[@intFromEnum(existing.spec)].status = .lowering;`
  appears twice in `lower.zig` (in the template reuse paths), alongside the
  official `SpecBuilder.markLowering`/`markReady`.
- `specialize.zig` `Queue.enqueue` linear-scans `entries.items` with
  `specEql` per insertion — O(n²) across a compilation.
- Cross-store equality: `Type.typeEqlAcrossStores` (in `type.zig`) and
  `SpecBuilder.entryMatches` compare a local requested type against a loaded
  cache record's type; these must agree with the one-way snapshot rule above.
- Regressions: issues roc-lang/roc#9889 and roc-lang/roc#9890 (see Problem).
- Historical: issue roc-lang/roc#9519; `Ast.FnTemplate`,
  `fnTemplateIdentityEql`, `fnTemplateDigest` still exist in `ast.zig` and
  feed `specialize.zig`'s queue `Spec.fn_def`.

## Solution design

**Make the key immutable.** `SpecIdentity` becomes `(CallableIdentity, digest
of the closed REQUESTED function type)` — nothing else. The solved/refined
type becomes *data* on the `SpecRecord` (a `solved_fn_ty: Type.TypeId` field,
plus its cached digest), never a rekey. Requesters read the solved type
through the record. If `source_fn_ty_digest` is still needed to separate
distinct checked source types that close to the same mono type, fold it into
`CallableIdentity` so the only type in the key is the requested type.

Steps, in migration order:

1. **Add `solved_fn_ty`/`solved_digest` to `SpecRecord`** (`ast.zig`),
   defaulting to the requested type at reservation. `SpecBuilder.markReady`
   takes the solved type and writes it here. Bump the `ROCSPEC` cache format
   in `serialize.zig` (records change layout; loaded records carry both).
2. **Replace rekeying with alias entries.** When a record becomes ready and
   its solved digest differs from its request digest, `SpecBuilder` adds an
   *additional* lookup entry `solved_digest → record` — an alias, not a
   rekey. `record.identity` is written once at `reserve` and never again.
   This makes the requested-type → record map explicitly many-to-one. The
   join rule lives in `SpecBuilder.find`: a request matches a record if the
   callable matches and the requested type is structurally equal
   (`typeEql`/`typeEqlAcrossStores`) to either the record's requested type or
   its solved type (solved only when status is `.ready`, mirroring today's
   `findLoweredTemplateInLookup` guard). A request whose type is LESS
   specific than the solved type therefore reuses the record through the
   request-digest alias; it never widens the record (one-way snapshot rule).
3. **Collapse the parallel tables into `SpecBuilder`.** Move the roles of
   `lowered_template_lookup` and `lowered_nested_lookup` into `SpecBuilder`'s
   single `lookup` map; `LoweredTemplate`/`LoweredNestedFn` keep only
   pass-local lowering state (def/fn ids, status mirror if needed), not
   digests. `lowered_template_by_fn`/`by_def`/`lowered_nested_by_fn` become
   derived indices owned by `SpecBuilder` or are replaced by fields on the
   record.
4. **Single mutation API.** `SpecBuilder` owns the status machine
   `reserved → lowering → ready` behind `reserve`/`markLowering`/
   `markReady(solved_ty)`. Delete the two direct
   `program.specs.items[...].status = .lowering` pokes in `lower.zig`.
5. **DELETE:** `SpecBuilder.updateLocalIdentity`; `SpecLookupKind` and the
   dual `.request`/`.solved` key kinds; `TemplateSpecLookup` and
   `NestedSpecLookup` plus the two maps; `appendTemplateLookup` /
   `findLoweredTemplateInLookup` (folded into `SpecBuilder`); the mutable
   `identity.mono_fn_ty`/`mono_fn_ty_digest` rewrite in `markTemplateReady`;
   the legacy `Ast.FnTemplate`-keyed queue `Spec.fn_def` once the queue
   carries `SpecId`s.
6. **Fix `Queue.enqueue`** with a hash set (`AutoHashMap` over the same
   digest key), O(1) membership; keep the `ArrayList` for FIFO order.
7. **Optional last:** route the generated-helper caches
   (`inspect_defs`/`equality_defs`/`hash_defs`) through
   `CallableIdentity.generated` so `SpecBuilder` is the only specialization
   index in the pass. `spec_constr.zig`'s call-pattern space stays separate
   (it specializes on value shape, not type), but document the boundary.
8. **Debug validator** (comptime-gated): after lowering, walk
   `SpecBuilder.lookup` and assert every record is reachable from exactly the
   keys that map to it (its request digest, plus its solved digest iff ready
   and different) and from no others; assert `record.identity` equals the
   identity captured at `reserve` (store a creation-time copy in debug
   builds, or make the field `const`-like via an opaque write-once wrapper).

## What success looks like

- The issue #9889 and #9890 repros compile and pass.
- No rekeying API exists: `updateLocalIdentity` is deleted and nothing
  mutates `SpecRecord.identity` after `reserve`.
- One lookup structure (`SpecBuilder.lookup`) answers every specialization
  reuse question in `lower.zig`; `lowered_template_lookup`,
  `lowered_nested_lookup`, and `SpecLookupKind` no longer exist.
- Status transitions happen only through `SpecBuilder`; grepping for
  `.status =` outside `specialize.zig` finds nothing in the pass.
- `Queue.enqueue` is O(1); the debug validator passes on the full test corpus.

## How to evaluate the result

### Correctness ideal

Identity is assigned once at record creation and is provably never used as a
map key after mutation — because mutation is impossible (write-once field,
alias entries instead of rekeys). The debug validator asserts every spec
record is reachable from exactly the keys that map to it, on every test-suite
run. Cross-store matching (`entryMatches`/`typeEqlAcrossStores`) and local
matching implement the same join rule, verified by unit tests that load a
`ROCSPEC` shard and issue requests at request-shaped and solved-shaped types.

### Performance ideal

O(1) expected lookup preserved (digest-keyed hash map with exact `typeEql`
only on digest collision); zero SHA-256 recomputation on the lookup path (the
per-node child-derived digest caching from PR #9848 stays); `enqueue` O(1).
Measure with issue #9802's benchmark (a `map2`-style generic higher-order app
with 63 call sites) and a nested structural accumulator variant; track wall
time and the `SpecializationCounters` statement/candidate counts
(`template_lookup_candidates`, `exact_type_checks`). Consider truncating
in-memory lookup digests to 128 bits (collision authority remains exact
`typeEql`), keeping full 256-bit digests only in the `ROCSPEC` file.

## Tests to add

- Issue #9889 / #9890 repros: roc-parser `numbers.roc` and `letters.roc` run
  via `roc test --no-cache`, added to the CLI test corpus.
- Issue #9519 flaky-identity repro: the same generic function reached via two
  file-split orders must produce one lifted function id (assert the
  `lift.zig` invariant never fires; run with both module orderings).
- Issue #9802 perf benchmark with an enforced bound on time and on the
  specialization counters (fail if candidate scans regress past a threshold).
- `SpecBuilder` unit tests (extend the existing ones in `specialize.zig`):
  alias-entry reuse at a less-specific requested type; no reuse across
  distinct callables with equal types; loaded-shard matching through
  `typeEqlAcrossStores`; validator catches a hand-corrupted identity.
- A debug-mode CI run of the existing corpus with the validator enabled.

## Related projects

- [Content-Based Nominal Identity](../big/content-based-nominal-identity.md)
  — cleaner `CallableIdentity` components.
- [Total Dispatch Plans](../big/total-dispatch-plans.md) — removes the other
  large identity-rederivation surface in `lower.zig`.
- [Debug Generation Counters on Growable Stores](../small/store-generation-counters.md)
  — guards `program.specs`/`program.fns` borrow hazards in the same pass.
