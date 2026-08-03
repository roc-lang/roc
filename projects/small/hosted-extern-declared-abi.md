# Pin Hosted Extern Specializations to the Declared ABI

## Problem

A hosted function's boundary type is an ABI contract: the host was
compiled against the platform header's declared signature, and design.md
(Host Symbol ABI) keys hosted identity by symbol string precisely so that
contract is stable. But nothing structural prevents the *compiler* from
specializing a hosted extern at some other type. If a hosted function's
closed `Try` error row is widened at a use site through ordinary
unification, the hosted function receives a specialization request at the
widened type, the extern boundary is emitted with a return layout the
host was never compiled against, and the host's `Ok` result is misread as
`Err` — silent wrong behavior at runtime, not a diagnostic and not a
panic.

PR roc-lang/roc#9966 fixed the one known path to this state — the `?`
desugar re-wrapping a hosted error into the caller's wider error union —
by adding a checker-side use-site rewrite:
`widenTryConditionForExpectedReturn` (`src/check/Check.zig:14675`) probes
row inclusion (`tryErrorRowNeedsUseSiteWidening` → `probeCanUseAs`) and
then mutates the solved graph (`dangerousSetVarRedirect`,
`Check.zig:14699`) so the hosted callee's own type stays at its declared
shape. That protects the ABI by intercepting one producer of widened
types, in the checker, with a probe-then-mutate rewrite — while the
actual invariant ("a hosted extern is only ever specialized at its
declared checked type") is stated nowhere and enforced nowhere. Any
*other* unification path that widens or otherwise perturbs a hosted
function's type will reproduce the silent ABI misread, and the failure
mode gives no signal until a host value is misinterpreted.

Notably, this rewrite re-landed within two days of PR #9921 removing the
near-identical `?`-widening machinery as a language-design mistake — the
same mechanism, now justified by the ABI instead of by type semantics.
Whether or not the checker rewrite stays, the ABI must not depend on it.

## Background

Hosted functions are declared in the platform header; their identities
are symbol-string-keyed by design (design.md, Host Symbol ABI). Postcheck
specialization (`src/postcheck/monotype/`) creates specializations from
requested types; the extern boundary for a hosted function is emitted
from the specialization's type. The checker's requirement surface
validation (#9911) proves app/platform compatibility at check time, so by
the time specialization runs, the declared checked type of every hosted
function is available in the platform's checked artifact.

The invariant this project makes structural: for a hosted extern, the
specialization request type must be exactly the declared checked type
(after the platform-app relation substitutions that publication applies —
the one sanctioned transformation). Anything else is a compiler bug that
must fail loudly at the producer, not flow to codegen.

## Evidence

- `src/check/Check.zig:14675` `widenTryConditionForExpectedReturn`,
  `:14703` `tryErrorRowNeedsUseSiteWidening`, `:14699` the
  `dangerousSetVarRedirect` — the checker-side interception (PR #9966,
  merged 2026-07-08).
- PR #9966's own description documents the failure mode: "the host's Ok
  result is misread as Err."
- PR #9921 (2026-07-06) deleted the same-shaped machinery for the
  language-semantics case; the test for issue #9798 asserts `?` into an
  open annotated row is a type error. #9966 must not have re-relaxed
  that: the two coexist only because #9966's probe is narrower.
- No assertion anywhere in `src/postcheck/monotype/lower.zig` or the
  extern-emission path compares a hosted specialization's type against
  the declared hosted type (grep for hosted checks around specialization
  request creation finds none).

## Solution design

1. **State the invariant.** design.md, Host Symbol ABI section: a hosted
   function has exactly one extern specialization per (symbol, target),
   at its declared checked type as substituted by the platform-app
   relation; the compiler never emits a hosted extern at any other type.
2. **Enforce at the producer.** In monotype lowering, where
   specialization requests are created: when the requested template is a
   hosted function, assert (debug) / reject (release, as a compiler-bug
   diagnostic, not `unreachable`) any request whose type differs from
   the declared one. The declared type is in the platform's checked
   artifact; the comparison is a checked-type equality, not a structural
   probe.
3. **Decide the checker rewrite's fate explicitly.** With the boundary
   enforced, `widenTryConditionForExpectedReturn` is either (a) still
   wanted as the *mechanism* that makes valid programs typecheck (the
   widening then happens on the caller's copy, never on the hosted
   type) — in which case document it as such and keep the #9966 repro; or
   (b) replaceable by having `?` on a hosted call unify against a fresh
   caller-side row from the start. Choose (a) unless (b) falls out
   simply; either way the ABI no longer depends on the choice.
4. **Cover the non-`?` paths.** Add tests that try to widen a hosted
   `Try` through other channels (annotation on the binding, passing the
   hosted result to a function expecting a wider row, storing it in a
   record field with a wider row) and assert each either typechecks with
   the hosted extern still at its declared type, or errors — never a
   misread.

## What success looks like

Every criterion below must hold; the project is not done until all do:

- design.md states the one-extern-per-declared-type invariant.
- The producer-side guard exists in monotype lowering; a hand-constructed
  wrong-type hosted request (unit test) is rejected with a message naming
  the hosted symbol, in both debug and release semantics.
- The #9966 repro stays green, and issue #9798's program still fails to
  typecheck (the #9921 decision is not re-relaxed).
- The non-`?` widening channels above are covered by tests; for each, the
  emitted extern layout equals the declared layout (assert via the
  host-boundary DCE/ABI test harness from PR #9621's family).
- An end-to-end host test where the host returns `Ok` through a boundary
  that a use-site tried to widen — asserting the app observes `Ok`
  (the #9966 failure mode, pinned permanently).
- The checker rewrite's role is documented at its definition (mechanism
  for caller-side typing, not ABI protection), or the rewrite is deleted
  — whichever option 3 chose.

## How to evaluate the result

### Correctness ideal

The ABI invariant holds by construction at the specialization boundary:
no path — present or future — from checker behavior to a wrongly typed hosted
extern exists, because the producer rejects it. Checker rewrites can then
be judged purely as type-semantics choices.

### Performance ideal

One checked-type equality per hosted specialization request — hosted
functions are few and requests per function fewer; unmeasurable. No
generated-code change for valid programs.

## Tests to add

- The producer-guard unit test (wrong-type hosted request rejected).
- The widening-channel matrix (annotation / argument / record-field).
- The end-to-end Ok-not-misread host test at both native and wasm
  targets.

## Related projects

- The solver-rewrite audit (landed): design.md's "Solver-Mutating Rewrites"
  section classifies the checker rewrite, which is now the declared Hosted
  Try Question Widening rule, scoped to direct hosted calls with both sides
  pinned by tests. This project's producer-side guard remains the structural
  half: the ABI must hold even if the checker rule changes.
- Carry the platform-app relation from checking — landed: the platform-root
  substitutions this project's invariant is stated relative to are now the
  requirement solutions recorded at check time (see design.md's
  "Platform/App Relation" section).
