# ARC Certifier Lattice Join and Centralized Ownership-Transfer Keying

## Problem

Two related weaknesses in the ARC (automatic reference counting) stage
together mean the compiler's only safety net over hand-maintained refcount
bookkeeping has a hole exactly where the bookkeeping is most likely wrong.

First: the debug borrow certifier (`src/lir/arc_certify.zig`) verifies
each join point once per **distinct entry-state summary**, with no
widening or join operator. For a loop carrying K refcounted mutable locals
whose body merges and re-splits their alias groups, the number of distinct
(alias-partition × balance) summaries grows like the Bell number of K
(B(6) = 203). Issue roc-lang/roc#9658 was a **valid** program
(~6 refcounted mutable locals in nested loops) that exceeded the then-cap
of 64 distinct states; the certifier reported that the entry states of a
join diverge across jumps and aborted the build. PR roc-lang/roc#9746
raised the cap to 4096 and converted exceedance into
`error.CertifierCapacityExceeded`, which `certifyStore` catches per
procedure: the procedure is left **unverified** and the build continues.
The in-code comment above the cap check is candid that a real
per-iteration leak in such a procedure would be *skipped rather than
reported*, and names the sound fix — a converging dataflow fixpoint over a
finite-height lattice — as future work. `Diagnostic.skipped_proc_count`
counts skips but is surfaced nowhere (see
[Surface ARC Certifier Skips](../small/surface-arc-certifier-skips.md)).

Second: the RC inserter (`src/lir/arc.zig`) makes ownership-transfer
decisions open-coded per instruction kind, at roughly 13 sites spread
across two parallel abstract-interpretation walks over the LIR. Issue
roc-lang/roc#9703 was one flaw of exactly the kind this invites: the
solver (`src/lir/arc_solve.zig`) puts the ownership unit of a borrowed
pure alias on the alias's **source** local, while the inserter tested and
cleared its `OwnedSet` bitset by the **alias** local id — the bitset index
was not the semantic key. The fix introduced `Solution.unitLocalOf()` to
centralize alias-to-unit resolution, but the transfer *decision* logic
remains duplicated per instruction kind, so every new ownership-moving LIR
instruction is a fresh opportunity for the same class of bug.

The certifier is the only mechanism that catches inserter bookkeeping bugs
before they become silent memory unsafety — and state-complex procedures
are both the ones it skips and the ones most likely to be wrong.

## Background

The compiler pipeline: parse → canonicalize → type-check → postcheck IRs
(Monotype → Lifted → Lambda Solved → Lambda Mono) → LIR lowering → ARC
insertion → backends (LLVM / dev x86+aarch64 / wasm / interpreter).

ARC insertion works in three parts, all stage-local to `src/lir/`:

- `arc_solve.zig` computes a whole-program borrows-with-lifetimes solution
  over ownership-neutral LIR. The design is documented in `design.md`
  (repo root), section "ARC Borrow Inference", which is authoritative.
- `arc.zig` (`insert`) consumes the solution and rewrites each procedure
  body, emitting explicit `incref` / `decref` / `free` statements. It
  threads an `OwnedSet` (a bitset over `LIR.LocalId`) along every control
  path: a set bit means "this local currently carries an ownership unit
  that this path must release or transfer exactly once."
- `arc_certify.zig` is a debug-build-only certifier (`design.md` section
  "Debug Borrow Certifier", authoritative) that re-checks the emitted RC
  statements against the ownership rules using only the emitted LIR and
  the ARC-stage signature table. `arc.insert` invokes it under
  `builtin.mode == .Debug` via `certifyStoreOrPanic`; release builds
  compile it away entirely.

Borrow inference deliberately keeps refcounts at 1 across read-only uses:
that is what lets the `refcount == 1` fast paths in the runtime list/str
builtins (`src/builtins/list.zig`, `src/builtins/str.zig`) mutate in place
instead of copying. The flip side is that a misplaced RC statement is not a
theoretical concern — it directly produces leaks, double-frees, or wrongful
in-place mutation observable in program output.

Vocabulary: an **ownership unit** is one retained count on one runtime
value; the inserter's `OwnedSet` bit for a local means that local's *unit
local* (`Solution.unitLocalOf`) carries a unit. A **join point** is the LIR
merge construct (`.join` / `.jump` CF statements); the certifier summarizes
the state flowing into a join over the locals the join body reads before
rebinding (`computeJoinRelevant`) and today re-walks the body once per
distinct summary digest (`JoinRecord.scheduled`, `summaryDigest`).

## Evidence

All paths relative to the repo root; verified against the current tree.

- `src/lir/arc_certify.zig`
  - `CertifyError = error{ OutOfMemory, Certification,
    CertifierCapacityExceeded }`.
  - The `.jump` handler: on a new distinct summary,
    `if (record.scheduled.count() > 4096) return
    error.CertifierCapacityExceeded;`, preceded by a comment naming issue
    9658, the Bell-number growth, the leak-masking risk, and the lattice
    fixpoint as the ideal fix. `certifyStore` catches the error per
    procedure, increments `Diagnostic.skipped_proc_count`, and continues.
  - `LocalSummary` (fields `class`, `repr`, `balance`, `lender_repr`,
    `condition`, `condition_mask`) and `LocalClass` (`unbound`, `owned`,
    `conditional_owned`, `borrowed`) are the existing per-join state
    quotient. `JoinRecord.scheduled` / `pending` hold the enumeration.
  - The pre-9746 behavior (cap 64, hard "entry states of join diverge
    across jumps" error) is gone; only the capacity error remains.
- `src/lir/arc_solve.zig` — `Solution.unitLocalOf` walks `alias_source` to
  the unit-carrying local when `isBorrowed(local)`; introduced by the fix
  for issue roc-lang/roc#9703.
- `src/lir/arc.zig`
  - Two parallel walks duplicate per-instruction ownership-transfer logic:
    the rewrite walk (`processRewritePath`) and the analysis walk
    (`analyzeUntil`). Both have open-coded cases for `.set_local`,
    `.assign_call`, `.assign_call_erased`, aggregates
    (`spanTransferMask` / `preserveConsumedArgMask`), `.ret`, and
    `.expect_err`.
  - `ownsUnit` / `unsetOwnedUnit` route through `unitLocalOf`, but
    decision helpers remain scattered: `canMoveSetLocalValue`,
    `callArgOwnership`, `unsetArgs`, `retainArgs`, `retainMaskedArgs`,
    `releaseOldTargetIfNeeded` (which tests `owned.contains(target)` on the
    raw local id — audit whether rebind targets can ever be aliases).
  - PR roc-lang/roc#9474 fixed a >64-argument call crash by making
    `CallArgOwnership.retain_args` / `transfer_args` heap `ArrayList`s;
    the `argMaskBit` `u64` helper remains on bounded paths behind
    explicit `i >= 64` guards (e.g. `retainMaskedArgs`).
  - `test "RC alias into set_local moves the leader unit"` is the existing
    regression test for the 9703 keying class.
- `design.md` sections "ARC Borrow Inference" and "Debug Borrow Certifier".

## Solution design

### Part 1: certifier dataflow fixpoint

Replace enumerate-distinct-entry-states with a forward dataflow fixpoint:
one abstract state per join, a join operator, iterate until stable.

Abstract domain, per join, over the join's relevant dense locals:

1. **Must-alias partition**: locals x, y are in one class iff they are
   bound to the same value on *every* in-edge seen so far. Join = common
   refinement (intersection of the pair relations). Each join step can only
   refine, and a partition over n locals refines at most n−1 times, so this
   component has height n−1.
2. **May-alias relation**: union of the per-edge alias relations. Join =
   set union; height ≤ n².
3. **Per must-class ownership mode**, a lattice with ⊥ below the four
   concrete classes `unbound`, `borrowed(anchor)`, `owned(balance)`,
   `conditional_owned(cond, mask)`, and ⊤ above them all:
   ⊥ = no in-edge seen; equal concrete modes join to themselves;
   `owned(b) ⊔ owned(b) = owned(b)`; anything else joins to ⊤ (conflict).
   Valid inserter output keeps balances at 0 or 1 outside transient
   aggregate windows and expresses path-dependent ownership as conditional
   accounting (`decref_if_initialized`, maybe-uninitialized params), which
   `conditional_owned` represents — so ⊤ is rare and meaningful.

The join is pointwise over these components; every component only moves up
its finite-height lattice, so the joined state at each join point changes
at most O(n²) times and termination is structural, not capped. Monotonicity
must be argued in a doc comment: each per-statement transfer function
(already implemented as the certifier's concrete walk) is extended to
abstract states such that a higher input never yields a lower output —
⊤ propagates, must-alias facts only shrink through the same rebindings,
may-alias facts only grow.

A join body is verified under the current joined state, and re-verified
only when a new jump strictly increases that state. Checks are adapted to
the abstraction:

- Balance accounting is per must-class. A release through local L consumes
  a unit from L's must-class.
- Where a check's verdict depends on an *undecided* fact — a pair that is
  may- but not must-aliased, or a mode joined to ⊤ — the certifier
  refines: it splits the join's abstraction on that one fact and
  re-verifies each variant. Every variant is satisfied by a nonempty
  subset of the real in-edges, so refinement never manufactures false
  positives; in the worst case it degenerates to today's finite
  enumeration, but *without a cap or skip path*, because the widened walk
  absorbs the combinatorial bulk (the 9658 class — many alias partitions,
  same ownership behavior — converges in one widened walk).
- A residual ⊤ that a check depends on and that refinement traces to a
  genuine disagreement between in-edges about unit carriage is a
  certification **failure** (a positive finding), reported with the join
  id, the local, and the two conflicting predecessor summaries.

Deletions when this lands:

- the 4096 cap and its comment block;
- `error.CertifierCapacityExceeded` from `CertifyError` and both catch
  sites (`certifyStore`, `certifyStoreOrPanic`);
- `Diagnostic.skipped_proc_count` and the surfacing added by the stopgap
  project (its CI zero-skip assertion becomes vacuous and is removed);
- `JoinRecord.scheduled` / `pending` digest-enumeration machinery
  (`summaryDigest`-keyed scheduling; the `memo` for shared switch suffixes
  is a separate mechanism and stays).

Write the soundness argument in the module doc comment: the guaranteed
property is that every emitted schedule balances ownership on all paths
(each unit released or transferred exactly once, no use after death, no
release of a borrow) for **every** procedure, with no unverified residue.

### Part 2: centralized ownership-transfer keying in the inserter

Create one shared keying/transfer layer (a struct in `arc.zig` or a new
`src/lir/arc_transfer.zig`) through which *all* transfer sites route:

- `unitOf(local)` — delegates to `Solution.unitLocalOf`, the **only**
  alias-to-unit resolution in the inserter (already true for
  `ownsUnit`/`unsetOwnedUnit`; make it true everywhere, auditing
  `releaseOldTargetIfNeeded`'s raw `owned.contains(target)`).
- `owns(set, local)`, `take(set, local)` (test-and-clear by unit key,
  returning whether a unit moved), `place(set, local)`.
- Decision helpers shared by both walks: `moveForSetLocal(...)` (wrapping
  `canMoveSetLocalValue` plus the mode-specific target handling),
  `transferForCall(...)` (wrapping `callArgOwnership` + `unsetArgs` +
  `retainArgs`), `transferForAggregate(...)` (wrapping `spanTransferMask` /
  `preserveConsumedArgMask` keying), `consumeAtTerminal(...)` (ret,
  expect_err).

The rewrite walk (`processRewritePath`) and the analysis walk
(`analyzeUntil`) then differ only in whether they materialize RC
statements; each per-instruction case shrinks to a call into the shared
helper, computed once as a small decision struct and consumed by both.
Open-coded `owned.unset`/`owned.contains` at transfer sites is deleted.
Adding an ownership-moving LIR instruction then requires exactly one new
decision function, used identically by both walks, instead of two
hand-synchronized copies.

### Migration order

1. Land [Surface ARC Certifier Skips](../small/surface-arc-certifier-skips.md)
   so any regression during this work is visible.
2. Inserter keying refactor (Part 2), gated by a byte-identical-LIR check
   on the corpus (see below). No behavior change intended.
3. Certifier fixpoint (Part 1) behind a comptime or option flag; run both
   old and new certifiers over the full test corpus and diff findings.
4. Remove the enumeration path, the cap, the error, the counter, and the
   stopgap surfacing.

## What success looks like

- Zero skipped procedures on the full test corpus in debug builds; the
  skip path no longer exists in the code.
- The issue 9658 reproduction certifies successfully (no abort, no skip).
- The issue 9703 keying bug, if reintroduced (transfer by alias id instead
  of unit id), is caught by the certifier on the existing corpus.
- Every ownership-transfer site in `arc.zig` resolves its key through the
  shared layer; grepping for raw `owned.unset(` / `owned.contains(` at
  transfer sites finds only the shared helpers.
- The lattice, its join, monotonicity, and the certified property are in
  `arc_certify.zig`'s module doc comment and reflected in `design.md`'s
  "Debug Borrow Certifier" section.

## How to evaluate the result

### Correctness ideal

- Mutation testing: deliberately break `arc.zig` transfer logic in several
  distinct ways — drop a decref, transfer by alias id instead of
  `unitLocalOf`, skip a join's `entry_keep`, double-transfer a call
  argument — and verify the certifier flags each one on the corpus.
- The lattice join is proven monotone in a doc comment, with the height
  bound stated; the soundness argument (the certifier guarantees every
  emitted schedule balances ownership on all paths) is written down.
- Old and new certifiers agree on every finding across the corpus during
  the flag-gated transition (step 3 above).

### Performance ideal

The certifier is debug-only, so its budget is generous — but the fixpoint
should be near-linear in (joins × locals): measure certifier wall time on
the largest corpus tests before and after; the join-heavy stress tests
below must complete in seconds, not minutes. Release builds never run the
certifier, so release compile time and generated code are unaffected by
Part 1 by construction. The Part 2 refactor must not change emitted RC
statements: capture the post-ARC LIR for the corpus before and after and
require byte-identical output, except where a latent keying bug is fixed —
diff and account for every change explicitly.

## Tests to add

- A reproduction of issue 9658 (nested loops over ~6 refcounted mutable
  locals) asserting full certification with zero skips.
- A reproduction of issue 9703 (borrowed pure alias whose unit lives on
  the source local, moved through `set_local` and call arguments),
  extending `test "RC alias into set_local moves the leader unit"`.
- A join-heavy stress generator: N refcounted mutable locals rebound and
  re-aliased in nested loops, N up to ~12, asserting certification
  completes and time stays within budget.
- Injected-leak tests proving detection: build LIR with a known
  per-iteration leak / double-release / use-after-move inside a
  state-complex loop (the shape the old cap would have skipped) and
  assert `error.Certification` with the expected diagnostic; use the
  `ArcTest` harness in `arc.zig`, which already builds LIR directly.

## Related projects

- [Surface ARC Certifier Skips](../small/surface-arc-certifier-skips.md) —
  the stopgap that makes the current hole visible until this project
  removes it.
- [Centralize the Seamless-Slice Allocation-Reuse Predicate](../small/centralize-slice-reuse-predicate.md)
  — the runtime-side twin: one predicate definition instead of per-site
  open-coded uniqueness checks.
- [Store Generation Counters](../small/store-generation-counters.md) —
  `arc.zig` has a hoisted-pointer hazard site of the class that project
  addresses (`insert` re-fetches the proc-spec pointer after rewrites
  because the backing storage may have been reallocated).
