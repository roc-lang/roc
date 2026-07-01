# One Canonical CaptureId Through Every IR

## Problem

A closure capture has no single identity in this compiler. The same captured
binding is keyed four different ways across the post-check IRs — a lifted
`LocalId`, a `Common.Symbol`, a checked `PatternBinderId`, and a generated
`capture_id: u32` — and the stages are joined by fuzzy matchers whose rules
differ per file. When a matcher misses, one file silently substitutes a
different value, another silently drops the operand, and a third panics.
Capture operand spans are positional parallel arrays whose order comes from
fixpoint discovery, so any pass that clones or reorders functions must
re-derive the world and hope the matchers re-align it.

Four PRs in eight weeks patched this machinery (9677, 9852, 9853, 9874), each
making one more piece of capture data explicit, and the class is still open:
issue 9897 panics with "function reference capture count differs from its
target" on a simple nested-closure program.

## Background

The compiler pipeline is: parse → canonicalize → type-check (producing
"checked artifacts" per module) → postcheck: Monotype IR (monomorphization,
`src/postcheck/monotype/`) → Monotype Lifted (closure lifting,
`src/postcheck/monotype_lifted/`) → Lambda Solved → Lambda Mono → LIR
lowering → ARC insertion → backends. `design.md` at the repo root is the
authoritative design; its Core Principles forbid post-check stages from
recovering missing data by heuristics or fallbacks, and state "Identity
provenance follows meaning provenance."

Captures in this pipeline: type checking records which bindings a lambda
captures (`CheckedCapture` in `src/check/checked_artifact.zig`: a
`CheckedPatternId` plus `scope_depth`). Monotype lowering turns each into an
explicit operand (`FnDefCapture` in `src/postcheck/monotype/ast.zig`: a local
plus a value expr). Closure lifting (`src/postcheck/monotype_lifted/lift.zig`)
computes each lifted function's capture SET as a least fixpoint over the
function-reference graph and stores it as a `TypedLocal` span per `FnId`;
every `fn_ref`/`call_proc` carries a positional operand span that must align
with that set. Lambda Solved re-checks types over the lifted program
(`src/postcheck/lambda_solved/solve.zig`); its lambda-set members carry
`Capture` records (`src/postcheck/lambda_solved/type.zig`: `local`, `symbol`,
`binder`, `capture_id`, `ty`). Lambda Mono and LIR lowering then build the
runtime capture records. Compile-time evaluation (CTFE) has a parallel
representation: `ConstStore` closures use `CaptureId` (binder | generated)
plus `ConstTypeStore` type evidence (`src/check/const_store.zig`).

Caching constraint (this design must respect it): canonicalization output
must be serializable keyed on a pure function of (module name, module source
bytes) and NOTHING else; type-checking output is keyed on that hash combined
with the type-checked hashes of imports — a Merkle DAG. Any id serialized in
a stage's artifact must be a deterministic function of that stage's cache-key
inputs. Global uniqueness comes from pairing module-local ids with module
identity, never from global counters or coordinator state. Name-based
resolution is legitimate in exactly one place: resolving imports at the
canonicalize→check boundary. A capture id assigned during canonicalization in
deterministic traversal order satisfies this; `ConstStore` lives in checked
artifacts, so any generated id stored there must be deterministic from
checked inputs alone.

## Evidence

The fuzzy matchers, each with different rules:

- `fnDefCaptureLocalMatches`, `src/postcheck/monotype_lifted/lift.zig`:
  matches local == explicit ∨ same `symbol` ∨ same `binder` ∨ same
  `capture_id`, gated by `monotypeTypeEql` structural type equality.
- `captureExprForMemberCapture`, `src/postcheck/lambda_mono/lower.zig`:
  matches local ∨ binder only; on a miss the caller silently FALLS BACK to an
  implicit local read via `lowerLocalValue(capture.local, field.ty)` — the
  pre-explicit-captures mechanism surviving as a fallback.
- `captureExprForMemberCapture` → `captureLocalMatchesMember`,
  `src/postcheck/solved_lir_lower.zig`: matches local ∨ symbol ∨
  `capture_id` (three keys; unlike lift's matcher it does not consult the
  binder), and a miss is `Common.invariant("callable capture payload field
  had no explicit operand")`. One of the two `captureExprForMemberCapture`
  policies — silent fallback vs. hard invariant — must be wrong.
- `lowerClosureCaptureExprSpan`, `src/postcheck/monotype/lower.zig`:
  `self.binders.get(binder) orelse continue` — silently drops any checked
  capture whose binder it cannot resolve.

The positional-order machinery: operand spans are parallel arrays in
fixpoint-discovery order. PR 9874 (fix-capture-finalization-order) introduced
`CaptureIdentity` (`binder` | `generated` | `local`, plus a monotype type) in
`lift.zig`, but only to REPAIR order at the `recomputeCaptures` boundary —
`finalizeCaptureExprSpanFromOperands` re-sorts operands against the new slot
order using the `capture_operands_by_expr` side table, and
`captureOperandsFromPositionals` still derives identity from positional
alignment once per recompute. `Lift.recomputeCaptures` re-derives all capture
sets after mutation; it runs twice per pipeline (inside
`SpecConstr.run` via `src/postcheck/monotype_lifted/spec_constr.zig`, and
again from `src/lir/checked_pipeline.zig`). PR 9853 reverted an edge-driven
worklist back to a full fixpoint because explicit operands let capture sets
shrink.

Bug history: issue 9634 → PR 9677 made the capture SET a solved fixpoint
artifact computed before body rewriting (it was previously derived during
rewriting — both wrong and exponential); issue 9825 → PR 9852 made capture
VALUES explicit operand spans through four IRs with arity/type invariants at
Lambda Solved, Lambda Mono, and LIR; PR 9853 made capture TYPES explicit on
the CTFE path (`CaptureId` + `ConstTypeStore`); PR 9874 added identity-keyed
order repair. Open issue 9897: `Common.invariant("function reference capture
count differs from its target")` in `src/postcheck/lambda_solved/solve.zig`,
reproducible with a simple nested closure — the class is still open.

## Solution design

Separate the two concerns that are currently conflated:

- **Capture IDENTITY** is owned by canonicalization. Assign a `CaptureId` —
  module-local, dense, allocated in traversal order — to every captured
  binding. Being a pure function of (module name, source bytes), it is
  cache-safe to serialize in checked artifacts per the caching constraint.
- **Capture SET membership** stays owned by the lift-time fixpoint, and it
  must: devirtualized sibling-def recursion produces captures that no checked
  closure ever listed, so the set cannot come from canonicalization. But
  every member the fixpoint adds refers to a binding that already has a
  `CaptureId` (or to a compiler-synthesized local, below).

Data structures:

1. `CaptureId` becomes a single `enum(u32)` in `src/check/checked_ids.zig`
   with two deterministic ranges: canonical ids (dense, from
   canonicalization) and generated ids (high-bit range, allocated
   deterministically by the pass that synthesizes a capturable local — CTFE
   during checking, lifting/spec_constr after). Generated ids serialized in
   `ConstStore` are deterministic from checked inputs (CTFE evaluation order
   is deterministic), so the caching constraint holds; post-check generated
   ids never enter checked artifacts (design.md: post-check IRs are not
   cached).
2. `CheckedCapture` gains `capture_id: CaptureId` alongside its pattern.
3. Monotype `FnDefCapture`, lifted capture slots (`TypedLocal` spans — the
   id lives on the local, replacing today's `binder`/`capture_id` pair of
   optionals), operand spans, Lambda Solved `Capture`, and LIR capture
   records all carry the same `CaptureId`, immutably.
4. Every operand↔slot join becomes an exact keyed lookup on `CaptureId`.
   Spans are canonically sorted by `CaptureId` (or stored keyed), so order is
   never load-bearing; `recomputeCaptures` produces sorted spans directly and
   the order-repair machinery disappears.
5. `ConstStore` captures reference the same `CaptureId` type; the current
   binder|generated union in `src/check/const_store.zig` collapses into it.

What gets DELETED:

- `fnDefCaptureLocalMatches` and `captureIdentityForTypedLocal` /
  `captureIdentityEql` / `findCaptureOperand` fuzzy matching in `lift.zig`,
  plus `CaptureIdentity`, `captureOperandsFromPositionals`, and
  `finalizeCaptureExprSpanFromOperands` (spans are keyed; nothing to repair).
- Both `captureExprForMemberCapture` matchers, replaced by exact lookup.
- The lambda_mono silent fallback to `lowerLocalValue`: a missing operand is
  a hard invariant EVERYWHERE, adopting `solved_lir_lower.zig`'s policy
  (design.md Core Principles already forbids the fallback).
- The `orelse continue` silent drop in `lowerClosureCaptureExprSpan`
  (`src/postcheck/monotype/lower.zig`): unresolvable binder = invariant.

Add `verifyCaptureInvariants`, a debug-only pass over a Lifted program, run
after EVERY Lifted-IR mutation (lifting itself, spec_constr, inlining, any
future pass), so a pass that forgets capture maintenance fails
deterministically at its own boundary instead of five stages later. It
checks, per function and per `fn_ref`/`call_proc` site: operand span and
capture slot span contain the same `CaptureId` multiset, sorted; no duplicate
ids in a span; every operand's local exists with a type equal to its slot's;
every canonical id resolves to a live binder.

Migration order: (1) allocate `CaptureId` in canonicalization and thread it
into `CheckedCapture`; (2) carry it through monotype lowering (fixing the
`orelse continue` into an invariant); (3) switch lift slots and operands to
keyed/sorted form, delete the lift matchers and order repair; (4) switch
Lambda Solved / Lambda Mono / solved_lir_lower joins to exact lookups, delete
both matchers and the fallback; (5) collapse `ConstStore`'s `CaptureId`
union; (6) land `verifyCaptureInvariants` and wire it after every mutation.

Note for `../big/total-dispatch-plans.md`: where-clause dispatch evidence
threads through nested closures exactly like a compile-time capture and
should ride this same identity discipline rather than invent a fifth key.

## What success looks like

- One `CaptureId` type; `grep` finds no capture join on `symbol`, `binder`,
  or `LocalId` equality anywhere in `src/postcheck/`.
- Missing capture operands are impossible to paper over: every consumer
  panics in debug and is `unreachable` in release, per design.md.
- Capture spans have a defined canonical order; shuffling fixpoint discovery
  order changes no output.
- Issues 9634, 9825, and 9897 repros pass; the 9897 panic site becomes an
  exact-lookup invariant that no longer fires.
- `CaptureIdentity`, `captureOperandsFromPositionals`,
  `finalizeCaptureExprSpanFromOperands`, `fnDefCaptureLocalMatches`, and both
  `captureExprForMemberCapture` bodies are gone from the tree.

## How to evaluate the result

### Correctness ideal

Every capture join in the pipeline is an exact key lookup that either
succeeds or halts the compiler at the stage that broke the invariant — never
a heuristic that can pick the wrong value. A new Lifted-IR pass that clones,
reorders, or rewrites functions either maintains captures or fails
`verifyCaptureInvariants` on the first test that exercises it. Capture ids in
checked artifacts are byte-stable across runs, machines, and entry points,
because they are functions of (module name, source bytes) only.

### Performance ideal

Given perfect correctness: joins drop from O(slots) linear scans with
structural type equality per candidate (today's `fnDefCaptureLocalMatches`
calls `monotypeTypeEql` per comparison) to O(1) id lookups or a merged walk
of two sorted spans — measure lift + lambda-solve + LIR-lower time on
closure-heavy code (the recursive-parser corpus from PR 9853 is the
benchmark). `recomputeCaptures` keeps its fixpoint but stops rebuilding
identity tables; measure its share of pipeline time before/after.
`verifyCaptureInvariants` must be debug-only: release-mode cost is zero.

## Tests to add

- Repros for issues 9634 (capture set correctness), 9825 (explicit capture
  values), and 9897 (nested-closure fn_ref capture count) as pipeline tests.
- PR 9874's 5-deep curried closure chains (order-sensitivity regression).
- PR 9853's recursive parser const closures (CTFE `CaptureId` path).
- spec_constr stress test: a program whose specialization clones functions
  and reorders their capture sets; assert output equivalence and invariant
  pass.
- Mutation test for the debug pass: intentionally skip capture maintenance
  after a synthetic Lifted mutation in a unit test; assert
  `verifyCaptureInvariants` fails with the expected message.
- Determinism test: canonicalize the same source twice (and with shuffled
  allocator behavior); assert identical `CaptureId` assignment bytes in the
  serialized checked artifact.

## Related projects

- `../big/total-dispatch-plans.md` — where-clause evidence as compile-time
  captures rides this identity discipline.
- `../big/content-based-nominal-identity.md` — same disease (fuzzy identity
  joins at stage boundaries) applied to nominal types; shares the
  "assign identity once, carry it immutably" principle and the caching
  constraint.
