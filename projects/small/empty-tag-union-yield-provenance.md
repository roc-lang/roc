# Provenance for the Empty-Tag-Union Yield in Lambda Solved Unification

## Problem

Lambda Solved unification is exact-match by design — same counts, same
positional order, same names — because both sides of every unification were
lowered from the same closed Monotype. It has one escape hatch: when exactly
one side is an empty tag union, that side yields and links to the peer
unconditionally (`src/postcheck/lambda_solved/solve.zig:1264-1271`),
whatever the peer is — function, record, primitive. The comment justifies
this from design.md's doctrine: an empty tag union is the seal for a slot
no evidence reached (a variable defaulted at Monotype materialization), it
fixes no layout, so it yields to concrete local evidence.

The reasoning is sound **for defaulted slots**. But the hatch keys on the
*shape* `[]`, not on the *fact* "this slot was defaulted" — it re-derives
provenance from structure, which is the codebase's recurring disease shape.
Any upstream bug that leaves an empty tag union somewhere it should not be
(a genuinely wrong unification, a wrongly-sealed row, a future refactor of
Monotype defaulting) is silently absorbed here instead of tripping the
exact-match invariant: the wrong peer wins, the slot is silently retyped,
and the divergence surfaces — if at all — stages later as a layout or
dispatch anomaly with no trail back to this link.

The 2026-07 comparative review against the cor `lss` prototype rated this
the likeliest hiding place for a real semantic bug in the stage: cor's
open-row unifier (`lambdasolved/solve.ml`) has no analogous unconditional
yield, so this hatch is production-only surface with no reference
semantics.

## Background

The compiler pipeline: parse → canonicalize → type-check → postcheck:
Monotype IR → Monotype Lifted → **Lambda Solved** (lambda-set solving;
`src/postcheck/lambda_solved/`) → Lambda Mono decisions → LIR → ARC →
backends. `design.md` is authoritative; read the Monotype instantiation
section (an unconstrained checked variable "lowers to the empty tag union
in Monotype. This is not a default choice. It records the invariant that no
runtime value can be constructed at that type").

Monotype materialization is where the fact is born: an unresolved graph
node with no evidence seals as the empty tag union
(`src/postcheck/monotype/solve.zig:~2038`, `materializeUnresolved`). The
Monotype type store interns nodes, so the sealed `[]` is the *same
interned TypeId* as any legitimately-constructed empty tag union —
interning collapses provenance at the type level. Provenance therefore
cannot be a bit on the type; it must either travel per *slot* (per graph
node / per use), or be verified rather than carried.

## Evidence

All symbols verified in the current tree.

- `src/postcheck/lambda_solved/solve.zig:1252-1271`: the yield, with the
  design rationale comment (phantom argument types of a function value
  inspected but never called are the motivating case).
- `src/postcheck/monotype/solve.zig:~2038`: unresolved-node defaulting at
  materialization (the only sanctioned producer of "defaulted `[]`" slots).
- design.md: exact-match doctrine for post-Monotype unification; empty tag
  union as the uninhabited-slot seal; debug-assert/release-unreachable
  invariant policy ("Post-check stages ... do not add release-build runtime
  checks for compiler invariants") — so the fix must be structural or
  Debug-time, not a release check.
- cor reference: `lambdasolved/solve.ml` unifies rows openly and has no
  yield-to-anything case; the hatch exists only because production unifies
  exactly over already-closed monotypes.

## Solution design

Decide between two routes during implementation; route A is the
design-conformant destination, route B is the acceptable floor.

**Route A — carry the fact.** Record defaulted-slot provenance where it is
created and consume it where the hatch fires:

1. At Monotype materialization, when a node seals by default to the empty
   tag union, record it in a per-spec side set keyed by the position that
   will be visible downstream (the lifted type reference, not the interned
   TypeId — interning collapses TypeIds).
2. Thread the set through Monotype Lifted into Lambda Solved's input
   (`Lifted.Program`), the same way layout/schema requests already travel.
3. The hatch consults the carried set: yield only when the `[]` side's slot
   is recorded as defaulted; otherwise fall through to the exact-match
   invariant, which is the enforcement design.md prescribes for everything
   else.

The design wrinkle to resolve first (this is why the project needs a
half-day of investigation before coding): Lambda Solved clones types from
Monotype TypeIds via its `TypeCloner`, and a slot's identity must survive
that cloning. If per-slot keying proves too invasive, weaken the carried
fact to per-TypeId-occurrence-in-context (the lifted expression or local
whose type contains the defaulted node) — the hatch fires during
unifications rooted at typed expressions, so context is available.

**Route B — verify the fact (floor).** Keep the shape-keyed hatch, but make
drift observable:

1. Debug-build check at the yield: walk back to the checked type behind the
   `[]` side (Monotype AST retains checked source references) and assert
   the checked slot was genuinely unconstrained. If the back-reference is
   not retained today, add a Debug-only side map from mono type creation.
2. A Debug counter for hatch firings, asserted against expected sites on
   the snapshot corpus, so a new producer of `[]`-meets-non-`[]` shows up
   as a corpus diff rather than silence.

Either route keeps release builds at zero cost per design.md. Do not ship
both halves half-done: pick A or B explicitly, and if B, file A as
follow-up.

## What success looks like

- The yield at `solve.zig:1264` is conditioned on (route A) or verified
  against (route B) the *fact* "defaulted at materialization" — never on
  shape alone.
- An artificially wrongly-sealed `[]` (test-injected) trips the exact-match
  invariant in Debug instead of silently linking.
- The motivating phantom case (a function value rendered as `<function>`
  whose phantom argument type seals as `[]` at the reference site) still
  compiles and runs — the hatch's legitimate work is untouched.

## How to evaluate the result

### Correctness ideal

- One producer, one consumer, explicit data between them: the set of slots
  allowed to yield is exactly the set Monotype defaulted, by construction
  (A) or by Debug verification on every firing (B).
- The exact-match invariant regains full coverage for every non-defaulted
  slot; behavior on checked programs is unchanged (the hatch only ever
  fires on defaulted slots today, if the doctrine holds — this project
  proves it holds and keeps it proven).

### Performance ideal

Release: zero — provenance is either data already flowing through stage
handoff (a side slice, O(defaulted slots), typically tiny) or Debug-only.
Measure Lambda Solved stage time on the corpus; require parity within
noise. Zero effect on generated code.

## Tests to add

- The phantom-slot case from the hatch's own comment: an inspected-never-
  called polymorphic function value (`|x, y| x + y` bound and rendered),
  compiled and run — pins the legitimate yield.
- A generic-container case where an element type seals as `[]` on one path
  and concrete on another (e.g. an empty list literal flowing into a
  function that also receives a populated list) — the other known
  legitimate firing family.
- Debug tripwire: a unit test over the Lambda Solved solver constructing a
  non-defaulted `[]` (route A: absent from the carried set; route B:
  checked-concrete) unified against a function type; asserts the
  exact-match invariant fires.
- Corpus check: hatch-firing counter across the snapshot corpus and CLI
  suite recorded once and asserted stable (Debug builds), so new firings
  are reviewed rather than absorbed.

## Related projects

- [pin-lambda-solved-invariants.md](./pin-lambda-solved-invariants.md) —
  sibling: the same stage's other unstated assumptions (FnId granularity,
  capture order, member order, erasure triggers).
- [pin-deferred-spec-requests.md](./pin-deferred-spec-requests.md) — the
  same disease shape one stage earlier (a default papering over a fact the
  checker proved), and the template for "carry the checked fact to the
  consumer".
