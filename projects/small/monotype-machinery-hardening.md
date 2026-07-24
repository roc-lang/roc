# Monotype Machinery Hardening: Verification Cost, Digest Depth, Graph Memos

## Problem

The 2026-07 comparative review of the Monotype stage against the cor `lss`
prototype confirmed the core machinery is sound (memoized instantiation
with placeholder-before-recurse, path-compressed union-find, structural
equality as the specialization-cache authority) and found no algorithmic
regression versus the prototype. What it did find is a cluster of bounded
hardening items — one design-policy violation, one measured-risk fallback,
and three could-not-disprove memos — none big enough for its own project,
all in the same few files:

**H1 — verification-only `sameType` checks run in release builds.**
`sameType` (`src/postcheck/monotype/lower.zig:16354`) structurally compares
two monotypes, computing SHA-256 digests via `typeDigestCached`
(`monotype/type.zig:589`) when the fast paths miss. Its call sites split
into two classes: *semantic* uses whose result feeds real control flow
(e.g. `lower.zig:6661`, `:7742`), and *verification* uses of the form
`if (!sameType(...)) Common.invariant(...)` (e.g. `lower.zig:8968`,
`:9017`, `:9290`, `:3863`, and the `lowerExprAtType` reconciliation guards
around `:16295-16348`). `Common.invariant` is `unreachable` in release
(`postcheck/common.zig:105-110`), but the *condition computation* still
executes (digest memoization mutates the cache, so the optimizer cannot
drop it). design.md is explicit: post-check stages "do not add
release-build runtime checks for compiler invariants" — these checks
predate that discipline being applied here, and cor does zero per-node
cross-checking at this stage. The digest cache is properly granular
(generation bumps only on reserved-slot refill, `type.zig:299`, with a
test pinning it), so the cost is bounded — but it is nonzero, on the
hottest lowering path, buying nothing in release.

**H2 — digest depth fallback degrades to identity hashing at depth 256.**
Both digest walks stop recursing at `digest_visiting_max = 256`
(`monotype/type.zig:633`) and hash the raw `@intFromEnum(ty)` instead
(`:756`, `:970-976`). Two structurally identical types deeper than 256
with different TypeIds then get different digests, land in different
specialization buckets, and `typeEql` (which has a cycle guard but no
depth cap) is never consulted — duplicate specialization of identical
functions. Correctness-preserving (identical duplicates, never a false
merge), but a silent-cap code-size/compile-time cost with no counter.

**H3 — `processed_relations` is a permanent cross-call unify memo that
could not be proven safe.** `unifyRoots` skips any node pair whose
version-stamped relation was ever processed
(`monotype/solve.zig:571-572`); the map lives for the whole graph
(`:226`). Re-unification is re-enabled only by a version bump, and
`union_` bumps only the winner's version (`:~506`). The review could not
construct a missed-constraint case — and could not prove one impossible.
An untested load-bearing memo in the unifier deserves a targeted stress
test more than trust.

**H4 — specialization-key refinement can split what cor shared.** The
spec cache keys on (callable, checked source digest, request digest)
(`monotype/specialize.zig` header, `SpecLookupAddress`); cor keyed on
(symbol, lowered monotype) (`specializations.ml:15`). The added source
component can only *split* buckets, never merge them — sound, but the
same monomorphic instantiation reached via two distinct checked source
types emits two byte-identical specializations. Unmeasured; if material
on real programs it is compile-time and code-size waste.

**H4b — two documented-trust hazards in the type store, guard with
comments and one audit.** (i) The *full* `typeDigest` hashes a plain
nominal's backing while `typeEql` compares plain nominals by
identity+args and ignores backing — the two authorities diverge for
equal-identity/different-backing pairs. The spec cache is safe (it keys
on the identity-only `specializationDigest`, consistent with `typeEql`),
and a false merge would need a SHA-256 collision; the hazard is any
*other* consumer using the full digest as a sole identity, which yields
avoidable duplicate identities. Audit full-digest consumers once, then
pin the invariant with a comment on both digest modes. (ii) `Store.add`
(`type.zig:253`) trusts callers to pre-sort record fields and tag rows;
digest and equality are positional, and only `Store.verify` (Debug)
catches an unsorted row. Sealing always routes through the sorting
adders today — say so on `add` itself so a future direct caller learns
the contract before shipping a type whose digest and equality disagree
with its sorted twin.

**H5 — three micro-costs.** `union_` (`solve.zig:499`) has no
union-by-rank/size (path compression in `find`, `:407`, keeps this
near-harmless, but adversarial merge orders exist); `layoutOfType`
(`src/postcheck/solved_lir_lower.zig:6714`) allocates an
O(total-type-count) scratch array per cache miss (`:6725`) instead of
O(reachable subgraph); `captureBindingForLocal`
(`solved_lir_lower.zig:833`) falls back to a linear scan of the captures
map on a direct-key miss (bounded by a function's capture count — note
and fix only if the direct key can be made total).

**H6 — the cross-store specialization reuse path is unverified.**
Loaded (cache-shard) specialization reuse compares a live request against
a deserialized record with `typeEqlAcrossStores`
(`monotype/specialize.zig:367`), and reserved records get a one-time
request-view refinement (`refineReservedRequestView`) — both
production-only surfaces with no cor analog and no targeted test. The
review flagged (without finding a bug) the question of whether a
reserved record's refinement can narrow a view another in-flight
requester already matched against. This wants a test, not trust.

## Background

The compiler pipeline: parse → canonicalize → type-check → postcheck:
**Monotype IR** (`src/postcheck/monotype/`: per-spec instantiation graphs
in `solve.zig`, interned monotypes and digests in `type.zig`,
specialization identity in `specialize.zig`, body lowering in `lower.zig`)
→ Monotype Lifted → Lambda Solved → Lambda Mono decisions → LIR → ARC →
backends. design.md is authoritative, particularly the invariant policy
(Debug assertion / release unreachable, zero release verification cost)
and the TypeDigest doctrine (digests identify closed structural content;
graph nodes are never cached by digest).

This is a measure-first project: H1 and H5 want profiles before surgery,
H2 and H4 want counters before redesign, H3 wants a test before any code
change. The deliverable is as much the measurements as the fixes.

## Evidence

All symbols verified in the current tree.

- `src/postcheck/monotype/lower.zig`: `sameType` (`:16354`), semantic uses
  (`:6661`, `:7742`), verification-only uses (`:3806`, `:3863`, `:3971`,
  `:8834`, `:8968`, `:9017`, `:9063`, `:9290`, `:9528`, `:9560`, and the
  at-type reconciliation cluster `:~16295-16348`).
- `src/postcheck/monotype/type.zig`: `typeDigestCached` (`:589`),
  generation bump only in `fillReservedSlot` (`:296-299`) with the
  invalidation test (`:2533`); `digest_visiting_max = 256` (`:633`), deep
  fallbacks (`:756`, `:970-976`).
- `src/postcheck/monotype/solve.zig`: `find` with path compression
  (`:407`), `union_` without rank (`:499`), `processed_relations`
  (`:226`, skip at `:571-572`).
- `src/postcheck/monotype/specialize.zig`: keying doctrine (header),
  `SpecLookupAddress` construction sites (`:242`, `:258`, `:320`).
- `src/postcheck/solved_lir_lower.zig`: `layoutOfType` (`:6714`),
  per-miss `local_nodes` allocation (`:6725-6727`).
- cor references: `monotype/specializations.ml:15` (structural key),
  `monotype/lower.ml` (no per-node cross-checking).

## Solution design

1. **H1 — classify, then gate.** Audit every `sameType` call site into
   semantic vs verification-only (the grep list above is the starting
   set; the classification is the reviewable artifact). Wrap
   verification-only sites in `if (builtin.mode == .Debug)` so the release
   branch is compile-time dead, per design.md's verifier rule. Semantic
   sites stay. Measure Monotype lowering time on the corpus before/after
   in ReleaseFast; whatever the number, the policy violation goes away.
2. **H2 — count, then decide.** Debug counter on the deep fallback; run
   the corpus and the deep-nesting stress test below. If it never fires
   outside the stress test, raise nothing and keep the counter as the
   tripwire (documented cap, no longer silent). If it fires on real code,
   switch the fallback from identity-hash to a truncated structural
   summary (hash the content one level deep) so equal types keep equal
   digests with bounded work.
3. **H3 — stress test the memo.** A solver-level test that unifies, then
   mutates row content through the sanctioned mutation paths, then
   re-unifies the same roots, asserting the second unification is not
   skipped while content differs (probe via sealed results, not
   internals). If the test finds a skip, the fix is bumping the loser's
   root version in `union_` alongside the winner's; if it cannot, the
   memo gets a comment stating the version-bump argument it relies on.
4. **H4 — measure duplicate emission.** Debug-build pass over
   `SpecBuilder` records at teardown: group by (callable, request digest)
   and count groups with >1 record whose solved monotypes are `typeEql`.
   Report the count and total duplicated body size on the corpus. If
   material (judgment: >1% of specs or any hot builtin duplicated), add a
   content-keyed reuse alias (the digest machinery already exists) —
   *as alias entries, never a rekey*, per the store's identity doctrine.
5. **H5 — smallest fixes last.** Union-by-size in `union_` is a few lines
   (store sizes alongside `versions`); take it only if a profile shows
   `find` chains at all. `layoutOfType`: replace the per-miss full-store
   array with a reusable scratch map (or per-Lowerer array allocated
   once), sized by what the graph actually visits.
6. **H6 — test the cross-store reuse contract.** Two tests: (a) a
   two-shard round trip where a loaded record is reused by a live request
   whose type is structurally equal across stores, and a near-miss
   variant (one leaf differs) asserting no reuse; (b) a concurrency-shaped
   sequential test of the reserved-refinement window — reserve, match a
   second requester against the pre-refinement view, refine, assert the
   second requester's reuse decision is still consistent with the
   refined record (or that the ordering is impossible by construction,
   asserted at the refinement site).

## What success looks like

- Zero verification-only `sameType` computation in ReleaseFast Monotype
  lowering (verified by inspecting one representative call site's release
  assembly, or by a `comptime` mode split making the dead branch
  structural).
- H2/H4 counters exist, ran on the corpus, and their numbers are recorded
  in the PR description; each either triggered its fix or pinned the
  do-nothing decision with data.
- The H3 stress test exists and passes, whichever way it resolved.
- Corpus compile time (ReleaseFast build of the compiler, timing the
  Monotype stage on roc-parser + examples) at parity or better; use CI
  benchmarks, not local timing.

## How to evaluate the result

### Correctness ideal

- No behavior change on checked programs, at all, in any item: H1 removes
  release-dead checks, H2/H4 deduplicate identical work, H3 is a test,
  H5 is allocation shape. Full snapshot corpus + cross-backend eval
  corpus bit-identical output is the acceptance bar (H4's dedup may
  change proc ordering in the LIR store; if so, assert semantic
  equivalence via the eval corpus and note the ordering delta).

### Performance ideal

- Monotype stage time strictly ≤ baseline on CI benchmarks; H1 is the
  expected win.
- `--opt=speed` binary size ≤ baseline (H4's dedup is the only item that
  can move it).

## Tests to add

- Deep-nesting stress: a generated 300-level nested record/tag type
  compiled twice via paths yielding distinct TypeIds; asserts one
  specialization (post-H2-fix) or documents two (pre-fix, counter test).
- The H3 re-unification stress test described above.
- H4 duplicate-emission counter test: two checked source types
  instantiating to the same monotype; asserts the counter sees them, and
  (if the fix lands) that one body is emitted.
- A `builtin.mode` compile test or assembly-grep guard pinning that the
  gated `sameType` sites contribute no release code (mirrors the
  GuardedList release-layout test pattern).

## Related projects

- [store-generation-counters.md](./store-generation-counters.md) — the
  landed guarded-store work whose release-zero-cost discipline H1 copies.
- [pin-deferred-spec-requests.md](./pin-deferred-spec-requests.md) — the
  live correctness project in the same solver; land that first if both
  are in flight, since H3's stress test will exercise the same
  deferred-request paths it changes.
