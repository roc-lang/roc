# Row Subsumption as a First-Class Judgment

## Problem

The type system has no row subtyping. A value whose tag row is CLOSED (for
example a callee's error union `[InnerErr]`) cannot unify with an OPEN but
RIGID annotated row (`[InnerErr, ..]` from a function's `Try` annotation),
even though using the narrower value at the wider type is semantically valid:
every tag the value can carry is one the annotation admits.

Users hit this with `?`, the early-return error-propagation operator. Issue
roc-lang/roc#9798: calling `inner({})?` inside a function annotated
`Try({}, [InnerErr, ..])` was rejected, while the equivalent explicit `match`
type-checked. The `match` works because re-tagging (`Err(e) => Err(e)`)
constructs a fresh value whose row variables are fresh and open, so they
unify with the annotation; the `?` condition's type is the callee's return
type itself, closed row and all.

PR roc-lang/roc#9834 fixed only that one construct, and did so by mutating
the union-find graph mid-inference (`dangerousSetVarRedirect`). The fix
applies exclusively to desugared `?` conditions. Any other construct with the
same closed-into-open shape — returning a stored closed `Try` value from a
function with an open-row annotation, pipeline combinators, or any future
syntax — needs its own copy of the special case, each with its own
hand-written row-inclusion probe and its own union-find rewrite.

## Background

The compiler pipeline: parse → canonicalize → type-check (src/check/Check.zig;
unification in src/check/unify.zig) → checked artifacts → postcheck:
Monotype IR → Lifted → Lambda Solved → Lambda Mono → LIR → ARC → backends.
design.md at the repo root is authoritative for pipeline design.

Rows are how records and tag unions are typed. A row is a set of labels
(fields or tags) plus an extension: a row can be OPEN (its extension is a
type variable, so more labels may exist) or CLOSED (empty extension; the
listed labels are all there are). An open row's extension variable can be
FLEX (inference may bind it freely) or RIGID (it came from a user annotation
and must not be bound to new structure — doing so would let inference invent
types the user did not write).

Unification is equational: it makes two types identical, it never proves one
is usable AT the other. Unifying closed `[InnerErr]` with rigid-open
`[InnerErr, ..a]` fails because success would require binding the rigid
extension `..a` to the empty row. Subsumption — "a closed row may be USED
where a wider row is expected" — is a different judgment, and today the
checker does not have it.

Related row handling that is already sound and NOT in scope here:

- Postcheck rows were fixed structurally: rows stay open inside the
  per-specialization instantiation graph and are only defaulted/closed at
  sealing (`GraphTypeFinals`, `sealNode`/`sealType` in
  src/postcheck/monotype/solve.zig).
- Record destructures are closed by default (PR roc-lang/roc#9687).
- Open rows are banned at host boundaries (PR roc-lang/roc#9870).

The remaining gap is checker-side: subsumption at annotated use positions.

## Evidence

- Issue roc-lang/roc#9798; fix PR roc-lang/roc#9834
  (merge commit 7199fe106d, branch `fix-9798/question-open-error-row`).
- src/check/Check.zig — PR 9834's legitimate plumbing: the `Expected` struct
  gained `return_result: ?Var` plus the `returnResult()` accessor, and the
  derived-expectation constructors (`forBranchBody`, `forStatement`, etc.)
  thread it through nested statements so `?` and `return` can unify against
  the ENCLOSING function's return type. This part should stay.
- src/check/Check.zig — the workaround: when checking a `match` with
  `match.is_try_suffix` set (`?` desugars to such a match), after unifying
  the condition with the builtin `Try` type, the checker calls
  `widenTryConditionForExpectedReturn(cond_var, expected_return, ...)`.
  That function:
  - `tryErrorRowNeedsUseSiteWidening` first tries an ordinary probe
    unification (`probeCanUseAs`, rolled back either way); only if that
    fails does it run the hand-written inclusion check.
  - `actualTagRowIsIncludedInExpected`, with helpers
    `expectedTagRowContainsTag`, `findVisibleTagInRow`, and
    `tagsCanUseSamePayloads`, is a bespoke one-shot structural walk proving
    every tag of the actual row appears in the expected row.
  - On success it mints a fresh `Try(ok, expectedErr)` var and REWRITES THE
    UNION-FIND GRAPH: `self.types.dangerousSetVarRedirect(cond_root,
    widened_try_var)`.
- src/types/store.zig — `dangerousSetVarRedirect` carries the codebase's own
  warning: "During type-checking, you probably don't want to use this
  function. ... it's possible to loose `rank` information! You should prefer
  to use regular `unify`". Check.zig has exactly two call sites: the
  widening path above, and `markErroneousBranchWithExpected` (error
  poisoning after a reported problem — out of scope for this project).
- src/postcheck/monotype/solve.zig — instantiation-graph sealing, showing
  the postcheck side already treats row openness structurally.

## Solution design

**A LANGUAGE-SEMANTICS DECISION IS REQUIRED FIRST.** Before any code is
written, decide: at which positions may a value of a smaller CLOSED tag
union be used at a BIGGER union type? This is a language design question,
not an implementation detail, and the answer determines which of the two
designs below is built. Both designs share the same machinery; they differ
only in where the judgment is invoked.

**Design A — subsumption at annotated-use positions.** One function,
`rowSubsumes(actual, expected)`, invoked at:

1. `?` conditions (the 9798 case),
2. return positions (explicit `return` and function-final expressions)
   checked against an annotated return type,
3. use sites of annotated bindings (a stored closed `Try` value used where
   an open-annotated type is expected).

**Design B — restrict to today's `?`-only behavior**, but re-implement it as
the same judgment plus explicit coercion, deleting the union-find rewrite.
Semantics stay exactly as after PR 9834; only the mechanism changes.

Shared machinery, step by step:

1. Implement `rowSubsumes(actual, expected)` next to unification: actual
   must be a closed (or error/empty) tag row; every tag of actual must be
   present in expected with payloads that themselves unify (payloads stay
   equational — subsumption is shallow at first; document this). The
   existing `actualTagRowIsIncludedInExpected` walk is a starting sketch of
   the logic, but the new judgment must be specified, named, and tested as
   THE row-subsumption judgment, not a `?`-local probe.
2. On success, do NOT touch the union-find graph. Instead emit an EXPLICIT
   coercion node in the checked output — a re-tag conversion carried in the
   checked expression data (`CheckedExprData` in
   src/check/checked_artifact.zig), wrapping the coerced expression. This
   matches design.md's principle that post-check consumes explicit checked
   data rather than re-deriving decisions. The coercion is exactly what an
   explicit `match` does implicitly today.
3. The coercion node's data structure carries BOTH types: the actual
   (source) row and the expected (target) row, as checked type references.
   Lowering must be total from the node alone — no re-inference.
4. Lowering (src/postcheck/monotype/lower.zig): for tag unions whose source
   and target representations are identical (same discriminants, same
   payload layouts), the coercion lowers to a no-op. Otherwise it lowers to
   a discriminant remap (and payload move if layouts differ).
5. Exhaustiveness must keep seeing the NARROWED source row: a `match` on a
   coerced value scrutinizes the coercion's source type, so users are not
   forced to write arms for tags the value can never carry. Specify this
   explicitly in the judgment's contract.

**What gets DELETED** (both designs):

- The `dangerousSetVarRedirect` call in the widening path, and
  `widenTryConditionForExpectedReturn` itself.
- `tryErrorRowNeedsUseSiteWidening`, `actualTagRowIsIncludedInExpected`,
  `expectedTagRowContainsTag`, `findVisibleTagInRow`, and
  `tagsCanUseSamePayloads` (fold anything still needed into the judgment).

Why the union-find mutation must go: a redirect rebinds an entire
equivalence class. Every other expression already unified with the condition
var — anything aliased through prior unification — silently changes type,
with no problem report and no record in the checked output. It can lose rank
information (the store's own comment), which corrupts generalization. The
`dangerous` prefix is the codebase's own warning that this is not a normal
inference operation; an explicit coercion node has none of these hazards
because it adds data instead of mutating shared state.

## What success looks like

- 9798-class code type-checks via the subsumption judgment: `inner({})?`
  under an annotation `Try({}, [InnerErr, ..])` compiles, and (under Design
  A) so does returning a stored closed `Try` from an open-annotated
  function.
- The judgment is the ONLY closed-into-open mechanism in the checker. Grep
  for `dangerousSetVarRedirect` finds no coercion-motivated call sites
  (error poisoning may remain).
- Every accepted subsumption is visible in the checked output as a coercion
  node; postcheck never infers a coercion.
- A new construct with the closed-into-open shape needs one new CALL to the
  judgment, not a new probe + rewrite.

## How to evaluate the result

### Correctness ideal

- Principal types are preserved: no inference regressions across the full
  snapshot corpus (src/snapshots/) — types printed for unrelated programs do
  not change.
- Coercion nodes carry both source and target types, so lowering is total:
  no lowering path needs to consult the type store to decide what a coercion
  means.
- The judgment is reflexive (`rowSubsumes(r, r)` for any closed r) and
  coherent under transitivity: if A subsumes into B and B into C, composing
  the two coercions equals the direct A-into-C coercion. Document this
  metatheory sketch alongside the implementation.
- Exhaustiveness checking still sees the narrowed source rows at coerced
  scrutinees.

### Performance ideal

- The judgment is O(row size) with interned tag-name comparisons; no
  quadratic re-walks of the expected row per actual tag beyond what interned
  lookup requires.
- Representation-identical coercions lower to NOTHING: assert via LIR
  statement-count tests that a `?` requiring subsumption emits the same LIR
  as one that does not.
- No new fixed-point passes anywhere in check or postcheck.

## Tests to add

- The 9798 reproduction: `inner({})?` under `Try({}, [InnerErr, ..])`,
  as a snapshot test and an end-to-end eval test.
- Stored-closed-Try-returned-at-open-annotation (Design A) — or, under
  Design B, a test asserting it still errors with a clear message.
- Combinator chains: a closed-error callee threaded through a pipeline into
  an open-annotated consumer.
- Negative tests: genuinely incompatible rows (missing tag; same tag with
  non-unifiable payload) still produce type errors, not coercions.
- Representation tests: no-op coercions emit no code (LIR statement-count
  parity); discriminant-remap coercions produce correct runtime values.
- Judgment unit tests: reflexivity, empty row into anything, closed into
  closed superset, closed into rigid-open, flex-open actual rejected.

## Related projects

- [../small/cross-phase-coverage-parity-tests.md](../small/cross-phase-coverage-parity-tests.md)
  — the coercion node is a new producer/consumer pair (checker emits,
  lowerer consumes) and should ship with a parity test in that suite's
  pattern.
- [../small/check-app-against-platform-requires.md](../small/check-app-against-platform-requires.md)
  — adjacent checker-side work on annotation boundaries.
