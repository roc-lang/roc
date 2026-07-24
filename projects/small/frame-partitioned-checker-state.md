# Frame-Partitioned Storage for Definitional State

## Problem

Two independent July 2026 bugs, in two different phases, had one root
shape: **a fact scoped to a definitional frame (a lambda being checked, a
declaration being defined) was held in shared, non-frame-partitioned
storage, so a consumer resolved it against the wrong frame.**

- Issue #9827 (checker): `return`/`?` constraints were appended to the
  checker's shared constraint list, and the drain at every lambda close
  processed *all* of them — a nested returned lambda's rank-3 constraint
  was unified while checking a top-level dependency in a rank-2 env,
  panicking the generalizer on a well-typed program. The first fix
  (PR #9929, tag-and-filter on the shared list) **failed** — other
  consumers still drained foreign frames' constraints — and was redone a
  day later (PR #10010) as dedicated per-frame storage:
  `return_constraint_frames` + `pushReturnConstraintFrame`
  (`src/check/Check.zig:129`, `:18182`), each lambda draining exactly
  `[frame.start..]` with a frame-identity assert.
- Issues #9961/#9912 (canonicalization): the declaration currently being
  defined could satisfy its own RHS lookups — `Thing : Thing` reported a
  false RECURSIVE ALIAS; `SelfRef := [].{ with_uri = with_uri }`
  manufactured a self-cycle that overflowed the stack downstream. Fixed
  (PR #10001) with explicitly scoped defining-state:
  `active_decl_scopes` (`src/canonicalize/Can.zig:209`) and the
  `scratch_defining_bound_vars` span stack (`Can.zig:310`).

Both cures are now in-tree and share a shape: dedicated storage, an
explicit frame stack, and asserts that frames balance. What does NOT
exist is any assurance that these were the only two members. Nothing
distinguishes, in either phase's code, "this list is settled-state by
design (drained once at end of check)" from "this list is frame-scoped
and every consumer must filter" — the distinction lives in each list's
drain discipline, which is exactly what #9827 got wrong. The failed
#9929 attempt shows the cost of guessing: tag-and-filter looked
equivalent and wasn't.

## Background

The checker (`src/check/Check.zig`) holds many deferred-work lists on
`Self`: the shared constraint list, `return_constraint_frames` (now
per-frame), `deferred_platform_required_unifications`,
`pending_tuple_accesses`, `open_numeral_literals`,
`ambiguity_candidates`/`ambiguity_verdicts`, `value_lookup_tracking`,
`hoist_invalidated_exprs`, and more. Some are *settled-state by design*:
they are intentionally judged once, at end of check, against final types
(the ambiguity worklist is the documented example). Others are
frame-scoped: their entries are only meaningful relative to the lambda /
decl / rank in which they were recorded. Canonicalization (`Can.zig`)
has the analogous split for scope state.

A separate live copy of the disease exists off-main: the
`iterative-checker-prologue-epilogue` branch received the weaker
tag-and-filter version of the #9827 fix (PR #9929) because its control
structure differs; if that branch merges, the shared-list entanglement
returns unless it adopts the frame-storage shape first.

## Evidence

- The #9929 → #10010 chain: same issue, same title, one day apart; the
  shared-list variant failed and the frame-partitioned variant is what
  held. The strongest single instability signal in the July analysis.
- `src/check/Check.zig:129` `return_constraint_frames`, `:18182`
  `pushReturnConstraintFrame` — the template cure.
- `src/canonicalize/Can.zig:209` `active_decl_scopes`, `:310`
  `scratch_defining_bound_vars` — the same template in canonicalization.
- No inventory exists: no comment on the checker's other deferred lists
  states whether they are settled-state or frame-scoped, and no assert
  enforces the distinction.

## Solution design

An audit-and-enforce project over both phases' deferred state.

1. **Inventory.** Every `Self`-level growable collection in `Check.zig`
   and every scope/defining collection in `Can.zig`. For each, answer
   from its consumers: is every read either (a) at end-of-check against
   settled state, or (b) filtered to the recording frame?
2. **Classify and mark.** Settled-state lists get a doc comment stating
   so and *why judging at the end is correct* (the ambiguity worklist's
   existing comment is the model). Frame-scoped lists that currently
   rely on consumer discipline get converted to the
   `return_constraint_frames` shape: dedicated storage or an explicit
   frame stack, drains take the frame identity and assert it matches.
3. **Enforce balance.** Every frame stack asserts push/pop balance at
   end of module check (debug), so a leaked frame is caught at the
   producer.
4. **Port before merge.** Record in the
   `iterative-checker-prologue-epilogue` branch's tracking notes that
   its #9929-shape fix must be upgraded to frame storage before that
   architecture lands; add the #9827 repro to whatever gate that merge
   runs.

## What success looks like

Every criterion below must hold; the project is not done until all do:

- The inventory exists (PR description or code comments), covering every
  deferred/growable collection on `Check.zig`'s `Self` and `Can.zig`'s
  scope state — none unclassified.
- Every frame-scoped collection is structurally frame-partitioned; no
  consumer filters a shared list by tag to find its frame's entries.
  `grep -n "return_lambda" src/check/Check.zig` (the #9929 tag) matches
  nothing on main.
- Every settled-state collection carries a doc comment justifying
  end-of-check judgment.
- Debug asserts: frame stacks balance at end of check; a drain with a
  mismatched frame identity panics naming both frames.
- The #9827 repro, the #9961/#9912 repros, and the full checker suite
  stay green.
- The off-main port obligation is recorded where the branch's merge
  checklist lives.

## How to evaluate the result

### Correctness ideal

"Drained the wrong frame's work" is unrepresentable for converted lists
(the storage is the frame) and loud for the rest (identity asserts). A
new deferred list must choose a classification to compile-review
cleanly — the inventory makes "unclassified" visible.

### Performance ideal

Neutral or better: frame-partitioned storage replaces whole-list scans
with span slices (that was #10010's effect). No new hashing; frame
stacks are push/pop on existing checkpoints. Verify checker wall-time
parity on the snapshot corpus.

## Tests to add

- A regression-style unit test per converted list: record work in a
  nested frame, close the outer frame first in the adversarial order its
  consumers allow, assert the nested work is not drained by the outer
  close.
- Frame-balance assert coverage: a test-only path that leaks a frame and
  expects the debug panic.

## Related projects

- [../small/audit-solver-mutating-rewrites.md](../small/audit-solver-mutating-rewrites.md)
  — the same audit-and-codify method applied to solver mutations.
