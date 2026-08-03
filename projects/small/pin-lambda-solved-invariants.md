# Pin the Lambda Solved Stage's Load-Bearing Invariants

## Problem

The Lambda Solved stage (`src/postcheck/lambda_solved/`) is the semantic
heart of closure compilation: it decides which lambdas can flow to which
call sites. The cor `lss` prototype it was productionized from solves this
with a full Hindley-Milner-style pass — generalization, per-use
instantiation, and SCC-ordered solving (`lambdasolved/solve.ml`,
`defs_graph.ml`, `inst.ml`). Production deliberately dropped all three and
solves monomorphically over shared type nodes. The 2026-07 comparative
review confirmed this is sound — but only because of four upstream
invariants that are nowhere stated, asserted, or tested at this stage's
boundary. Each is a silent-wrong-code bug if it ever breaks, because the
stage has no mechanism left that would catch the violation:

**I1 — FnId granularity (the big one).** Sharing type nodes across a
function's call sites computes the *union* of lambda sets over those
sites. That is the correct set only because Monotype specialization keys
every `LiftedFnId` on the checked source function type digest
(`src/postcheck/monotype/specialize.zig` header, `source_digest` at
`:112-127`), which includes lambda-set structure — so two call sites
needing different lambda sets always land in different FnIds. If any path
ever produces one FnId for two checked-lambda-set-distinct instantiations,
Lambda Solved merges their sets silently and dispatch calls the wrong
function. No error, no panic — wrong behavior at runtime.

**I2 — Positional capture matching.** `unifyCaptures`
(`lambda_solved/solve.zig:1473`) matches capture lists positionally,
asserting `capture_id` equality per index. cor matches by symbol-set,
order-independent. Production's positional match is correct only because
every member's captures always come from the same FnId's capture span in
the same order — an invariant that holds today by construction and is
enforced only by `Common.invariant` (Debug-only per design.md policy).

**I3 — Lambda-set member order.** `mergeLambdaSets`
(`lambda_solved/solve.zig:1452`) appends new members in encounter order;
cor keeps members canonically sorted by lambda symbol. Order is
deterministic but traversal-dependent. It is safe today because every
consumer (variant construction and dispatch in Lambda Mono decisions /
direct LIR lowering) reads the *same* span — but nothing pins that a
consumer never materializes order-sensitive data (discriminants) from two
differently-ordered copies.

**I4 — Erasure trigger completeness.** cor erases closures at explicit
`~erase`/`~unerase` kernel calls (`lambdasolved/erased.ml`). Production
re-derives erasure from structure: `markErasedCallablesReachedByType`
(`lambda_solved/solve.zig:924`) fires on box ops (`:1080`, `:1085`) and on
every layout request (`:155`) and runtime schema request (`:166`). Nothing
argues these triggers cover every position where a callable's
specialization becomes unknowable; a missed trigger means a finite
lambda-set representation crosses a boundary that needed the erased
encoding.

## Background

The compiler pipeline: parse → canonicalize → type-check → postcheck:
Monotype IR (monomorphization; specialization identity in
`monotype/specialize.zig`) → Monotype Lifted → **Lambda Solved** → Lambda
Mono decisions → LIR → ARC → backends. `design.md` is authoritative; its
invariant policy (debug assertion / release unreachable, no release-build
invariant checks) governs the form every new check takes.

This project is deliberately test-and-assert-heavy rather than
code-change-heavy: the review found no violation of I1–I4 in the current
tree. The work is making each invariant *stated* (a comment at the
consuming site naming the producer), *asserted* (Debug checks at the stage
boundary), and *tested* (end-to-end programs that would fail today if the
invariant broke), so the next refactor of Monotype specialization or
lambda-set merging cannot break one silently.

## Evidence

All symbols verified in the current tree.

- `src/postcheck/lambda_solved/solve.zig`: global fn-type pre-registration
  before any body solves (`:131-147`), no SCC/generalization machinery
  anywhere in the stage; `unifyCaptures` positional loop (`:1473`);
  `mergeLambdaSets` append-in-encounter-order (`:1452-1471`);
  `markErasedCallablesReachedByType` (`:924`) and its three trigger sites
  (`:155`, `:166`, `:1080`, `:1085`); `closeUnfilledCallableSlots`
  (`:279`) as the end-of-stage sweep that finalizes callable slots.
- `src/postcheck/monotype/specialize.zig`: identity doctrine in the header
  ("callable, checked source function type digest, requested type —
  written once, never rewritten"), `source_digest` (`:112-127`).
- cor reference: `lambdasolved/solve.ml` (gen/inst), `defs_graph.ml`
  (SCC), `inst.ml` (per-use instantiation), `erased.ml` (explicit erasure
  propagation) — all deliberately absent from production.
- Review provenance: 2026-07 cor-vs-production comparative review, Lambda
  Solved stage report.

## Solution design

1. **I1 — test and boundary assert.** End-to-end tests (below) that fail
   with wrong output if two lambda sets merge. Plus a Debug check at
   Lambda Solved intake: for every pair of distinct FnIds sharing a source
   symbol, assert their checked source digests differ (the cheap
   contrapositive of "same digest ⇒ same FnId", catchable at the boundary
   without re-deriving lambda sets).
2. **I2 — state and assert at the producer.** Document the ascending-order
   contract on the capture span where members are built, and add a Debug
   assert at `unifyCaptures` entry that both spans are `capture_id`-sorted
   (today's per-index equality assert catches mismatches only after
   partially unifying).
3. **I3 — canonicalize instead of documenting.** Sort members by lambda
   `Symbol` at creation and in `mergeLambdaSets` (cor's behavior). This
   deletes the invariant rather than pinning it: member order becomes
   canonical, and any future consumer that materializes order-derived data
   is safe by construction. Confirm Lambda Mono decisions and the direct
   LIR lowerer assign variant indices from the (now canonical) span, and
   that the Debug Lambda Mono verifier still matches.
4. **I4 — erasure audit with a closing check.** Enumerate the positions
   where a callable type can reach a representation-committing boundary
   (layout roots, schema roots, box payloads, const-store materialization,
   host ABI surfaces), confirm each routes through
   `markErasedCallablesReachedByType`, and add the closing Debug sweep:
   after `closeUnfilledCallableSlots`, no type reachable from a layout or
   schema request may contain a *finite* lambda-set whose layout was
   committed as erased, and vice versa. The sweep turns "we think the
   triggers are complete" into a per-program proof.
5. **Def ordering (minor, document only).** cor SCC-orders output defs;
   production emits source order (`solve.zig:134-142`). Confirm no
   consumer assumes dependency order (LIR uses explicit references) and
   say so in the stage doc comment.
6. **`returnTargetTy` re-derivation (minor).** `returnTargetTy`
   (`solve.zig:900-907`) licenses reusing the enclosing function's solved
   return slot by structurally comparing monotypes (`sameMonoType`,
   `:909-912`, a full `typeEql` per `return_` statement) — a
   verification-flavored re-derivation where a direct id link would be
   authoritative. Carry the return target's solved type var explicitly
   from where the return context is entered, and demote the structural
   compare to a Debug assert. Same shape, smaller scale, as the other
   items: replace a re-derived fact with a carried one. (This also
   removes a per-return structural walk from release builds; while in the
   file, note the two stage-level perf micro-costs for the same audit:
   `solvedTypeDigest`'s SHA-256 walk per erased callable (`:1485+`) and
   the fresh `AutoHashMap` allocated per `markErasedCallablesReachedByType`
   call (`:925`) — pool the map if profiling ever surfaces it; neither
   needs action beyond a comment today.)

## What success looks like

- Each of I1–I4 is named in a comment at its consuming site, Debug-asserted
  at a boundary (not deep inside a unify loop), and covered by an
  end-to-end test that fails on violation.
- I3 no longer exists: member order is canonical by construction.
- The I4 closing sweep runs on the full snapshot corpus and CLI suite in
  Debug CI with zero firings.

## How to evaluate the result

### Correctness ideal

- The stage's soundness argument is written down and machine-checked: a
  reviewer can read the intake asserts and the closing sweep and conclude
  "monomorphic solving is sound for this input" without trusting folklore.
- Behavioral: full snapshot corpus and cross-backend eval corpus
  unchanged; canonical member ordering must not change any program's
  output (dispatch is by matched symbol, not position — if any output
  changes, that is a latent I3 consumer, which is exactly what the change
  exists to surface).

### Performance ideal

Sorting members costs O(set size log set size) at merge points on sets that
are almost always tiny (1–3 members); boundary asserts and the closing
sweep are Debug-only. Release stage time on the corpus: parity within
noise. Zero effect on generated code (verify: bit-identical `--opt=speed`
output on the corpus before/after, modulo the I3 caveat above).

## Tests to add

- **I1 dispatch matrix** (the review's highest-value missing test): one
  polymorphic higher-order function applied at the same value types with
  two different closures (different captures, different bodies), both
  paths' results asserted; variants with the closures defined in
  different modules, with a recursive closure, and with one closure
  reaching the call through a data structure. Today these pass; they must
  fail loudly if FnId granularity ever coarsens.
- **I2/I3 unit tests** over the solver: merge two lambda sets built in
  opposite encounter orders; assert canonical result order and correct
  capture unification.
- **I4 boundary matrix**: a function value (a) stored in a box, (b) in a
  record requested as a layout root, (c) in a value crossing the host ABI,
  (d) inside a const-materialized structure — each compiled and run;
  Debug closing sweep clean on all four.
- Debug-build corpus run asserting zero firings of every new assert.

## Related projects

- [empty-tag-union-yield-provenance.md](./empty-tag-union-yield-provenance.md)
  — the same stage's other shape-keyed assumption, split out because it
  needs a carry-vs-verify design decision.
- [cross-phase-coverage-parity-tests.md](./cross-phase-coverage-parity-tests.md)
  — the same producer/consumer parity discipline; its harness is the
  natural home for the I1 intake assert's corpus run.
