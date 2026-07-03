# Check the App Against the Platform's `requires` Signature

## Problem

A Roc platform declares the types it demands from an app in its `requires`
clause (e.g. `requires { main! : List(Arg) => Try({}, [Exit(I32)]) }`). Today
the app module is type-checked **without** that signature. In an app like

```roc
main! = |args| ...
```

`args` stays an unconstrained flex var through the entire check of the app
module. The platform's declared type is grafted on *after* the app's checked
artifact has already been published: `applyPlatformRequiredSignatureSubstitutions`
in `src/check/checked_artifact.zig` structurally matches the platform's
declared scheme against the app def's checked root and rewrites the published
checked type roots via `cloneCheckedTypeRootSubstituting`. Then the build
coordinator (`src/compile/coordinator.zig`, `finalizeExecutableArtifactsInternal`,
called via `finalizeExecutableArtifacts`) re-validates everything the checker
would have caught had it known the types: it snapshots per-plan type keys
before republish with `PlatformRequiredValidationSnapshot`, diffs them after,
runs hand-rolled numeric fit checks, and hand-builds `Report` documents that
duplicate checker diagnostics.

This is a shadow type checker living outside the checker. It re-implements
type propagation, dispatch validation, and literal range checking on checked
artifacts, and it has produced a sustained bug cascade (see Evidence). Every
fix has grown the layer instead of shrinking the problem, because the root
cause is architectural: the checker never sees the platform types, so
everything downstream must guess at what the checker would have said.

## Background

The compiler pipeline is: parse → canonicalize → type-check (producing
"checked artifacts" per module — all user-facing diagnostics end here) →
postcheck: Monotype IR (monomorphization, `src/postcheck/monotype/`) →
Monotype Lifted (closure lifting) → Lambda Solved → Lambda Mono → LIR lowering
(`src/postcheck/solved_lir_lower.zig`) → ARC insertion (`src/lir/arc*.zig`) →
backends (LLVM/dev/wasm/interpreter). `design.md` at the repo root is the
authoritative post-check design. Its Core Principles state that every stage
after checking consumes explicit data produced by earlier stages, that all
user-facing failures are reported during checking at the latest, and that
post-check stages must not re-derive facts from names or structure.

Platform/app model: a platform module's header has a `requires` clause naming
the definitions the app must provide, with full type annotations. The clause
can also have a *for-clause* naming types the app supplies (e.g. `Model`) that
the requires signature itself refers to. So the relationship is bidirectional:
the platform's scheme is parameterized over app-supplied types. The checker
already has machinery for this: `processRequiresTypes` in `src/check/Check.zig`
processes requires-clause types, and for-clause aliases are tracked in
`for_clause_aliases` (see `isForClauseAliasStatement`), added for alias
handling in PR roc-lang/roc#9850.

The build coordinator (`src/compile/coordinator.zig`) orchestrates per-module
checking and artifact publication. Today the app is checked as if standalone,
its artifact is published, and only then does the platform signature get
substituted in — which is why validation had to be bolted on after the fact.

Error-recovery contract: PR roc-lang/roc#9819 added `allow_user_errors`
plumbing (coordinator + `src/cli/main.zig`) and `runtime_error_inserted`
markers in `src/check/Check.zig` so runs can proceed past user errors by
inserting runtime errors. That contract is orthogonal to this project and
stays.

## Evidence

All symbols below verified in the current tree.

- `applyPlatformRequiredSignatureSubstitutions`,
  `cloneCheckedTypeRootSubstituting` — `src/check/checked_artifact.zig`.
  Post-publication grafting of the platform scheme onto app checked roots.
- `PlatformRequiredValidationSnapshot`, `finalizeExecutableArtifactsInternal`,
  `appendPlatformRequiredUnresolvedDispatchReports`,
  `staticDispatchPlanDispatcherExpr`, `platformRequiredOkTagPayload` —
  `src/compile/coordinator.zig`. The snapshot records a `CanonicalTypeKey`
  per dispatch plan (dispatcher + callable), per iterator plan, and per
  checked pattern, then diffs after republish to find plans the substitution
  changed. `platformRequiredOkTagPayload` finds `Try`'s success type by
  string-comparing tag label text to `"Ok"`.
- `specializeResolvedStaticDispatchPlanCallables` —
  `src/check/checked_artifact.zig` (added by PR roc-lang/roc#9873).
- The numeric-literal sweep: `appendPlatformRequiredInvalidNumeralDispatchReportIfNeeded`
  in `src/compile/coordinator.zig` plus `numeralLiteralFitsBuiltin` /
  `builtinNominalAcceptsNumeralLiteral` in `src/check/checked_artifact.zig`,
  which re-render literal text and re-parse it (`parseIntFits`,
  `parseFloatFits`) to decide range fit — outside the checker.
- Bug cascade: issues roc-lang/roc#9540, #9541, #9542, #9559 (platform-typed
  values misbehaving because the app never saw the platform types) → PR #9762
  (~3k lines adding the snapshot/diff layer) → issue #9782 (nested dispatch
  missed by the snapshot diff) → PR #9835 (chases one call-hop via
  `staticDispatchPlanDispatcherExpr`) → issue #9857 → PR #9873 (+750 lines
  incl. `specializeResolvedStaticDispatchPlanCallables` and
  `platformRequiredOkTagPayload`) → regression issue #9890.
- Issue roc-lang/roc#9565: a literal silently wraps when the platform implies
  `I8`; fixed by the ~470-line coordinator/artifact literal sweep above,
  matching by canonical-type-key equality — all because the checker never saw
  the platform type at the literal's site.
- PR roc-lang/roc#9762 also made headerless `roc check` synthesize a temp-dir
  echo platform so that check sees platform types (see the default_app /
  "echo platform" path in `src/cli/main.zig`) — a tacit admission that
  checking against the real signature is the correct model.

## Solution design

Check the app *with* the platform's requires signature, the same way an
explicit type annotation constrains a def.

1. **Sequencing.** The coordinator already resolves the platform before the
   app (the app header names it). Ensure the platform's header declarations —
   the requires signature and any for-clause type parameters — are parsed,
   canonicalized, and checked before the app module's check begins. Only the
   platform *header/requires* surface is needed, not the platform's full body
   check, so this does not lengthen the critical path materially.
2. **For-clause instantiation.** For-clauses are bidirectional: the app
   supplies types (like `Model`) that the requires signature references.
   Ordering within the app's check: canonicalize the app far enough to know
   its for-clause type declarations, instantiate the platform's requires
   scheme with those types substituted for the for-clause parameters, then
   check app defs.
3. **Unification.** For each required definition (e.g. `main!`), unify the
   instantiated scheme into the app def's type variable during checking,
   exactly like `processRequiresTypes` handles annotation-shaped requires
   types today. From that point on, `args` in `main! = |args|` has the
   platform's type from the start; dispatch plans freeze against concrete
   types; literals default and range-check against the real expected type;
   pattern exhaustiveness sees the real unions.
4. **Diagnostics.** All dispatch/pattern/literal validation for
   platform-typed values becomes ordinary check-time diagnostics emitted by
   the checker with real regions — no hand-built Reports in the coordinator.
5. **Delete the shadow layer.** Once the checker owns this, delete (verify
   exact current names by reading the code first, as they may drift):
   - `PlatformRequiredValidationSnapshot` and its diff helpers
     (`dispatchPlanChanged` etc.) in `src/compile/coordinator.zig`
   - `appendPlatformRequiredUnresolvedDispatchReports`,
     `staticDispatchPlanDispatcherExpr`, `platformRequiredOkTagPayload`,
     `appendPlatformRequiredInvalidNumeralDispatchReportIfNeeded`, and the
     other `platformRequired*` report builders in the coordinator
   - the literal re-parse sweep support (`numeralLiteralFitsBuiltin` and
     friends in `src/check/checked_artifact.zig`) once no caller remains —
     the checker's own numeral machinery takes over
   - most of `applyPlatformRequiredSignatureSubstitutions` and
     `specializeResolvedStaticDispatchPlanCallables` in
     `src/check/checked_artifact.zig`; whatever survives should be a
     verification step (debug assertion that published roots already match),
     not a rewrite step
   - the temp-dir echo-platform synthesis for headerless `roc check` can
     remain (it is now just "the default platform"), but it no longer papers
     over a checking gap.
6. **Migration order.** (a) land platform-before-app sequencing with the
   unification behind a flag defaulting on; (b) port the repro suite from the
   Evidence issues to expect check-time diagnostics; (c) delete the shadow
   layer in one PR so no dead validation path lingers; (d) remove the flag.

Data structures: no new ones. The requires scheme already exists in checked
form (`requires_types` on the platform's ModuleEnv); the app-side change is a
unification call plus for-clause substitution using existing
instantiation machinery.

## What success looks like

- In an app compiled against a platform, `args` in `main! = |args|` has the
  platform's declared type immediately after the app's check — observable in
  the checked artifact without any coordinator post-processing.
- The repros from issues #9540, #9541, #9542, #9559, #9782, #9857, and #9565
  all produce ordinary check-time type errors with correct source regions.
- `roc check` and `roc run` report byte-identical diagnostics for these
  programs; nothing new appears only at build/finalize time.
- `PlatformRequiredValidationSnapshot` and the `platformRequired*` coordinator
  functions no longer exist in the tree.
- Net line count in coordinator + checked_artifact drops by thousands.

## How to evaluate the result

### Correctness ideal

The invariant: **after the app module's check completes, no published checked
type root changes.** Artifact publication is append/freeze, not rewrite.
Enforce it with a debug assertion at publication: recompute the canonical type
key of every root touched by platform-requirement finalization and assert
equality with the pre-finalization key (the inverse of today's snapshot diff —
kept only as a debug check, then removed once trusted). Secondary invariant:
the coordinator constructs zero `Report` documents describing type errors; all
type diagnostics originate in `src/check/`. A grep-level check ("no
`platformRequired` identifiers outside tests") should hold. The full existing
snapshot test corpus and the platform integration tests must pass with
diagnostics moved earlier, never lost.

### Performance ideal

Checking the app against real types does strictly less total work than
check-then-graft-then-revalidate: one unification during check replaces a
whole-artifact substitution pass plus a whole-artifact snapshot/diff pass.
Measure end-to-end `roc check` and `roc build` wall time on a platform-based
app corpus (examples/ plus a large app). `roc build` must get faster (the
snapshot alloc/diff over every plan and every pattern disappears). `roc check`
on an app may pay for checking the platform's header surface first; keep that
to header-only work so single-module check time regresses by no more than the
cost of parsing/canonicalizing the platform header. Peak memory in the
coordinator drops (no `CanonicalTypeKey` arrays sized by pattern count).

## Tests to add

- Repro tests (snapshot or integration) for #9540, #9541, #9542, #9559,
  #9782, #9857, #9565, each asserting a check-time diagnostic naming the
  app-side source region — and asserting `roc check` output equals `roc run`
  output.
- A for-clause test: platform requires signature referencing an app-supplied
  `Model`; app provides it; a deliberate mismatch inside `main!` reports at
  check time against the substituted scheme.
- A regression guard for #9890's shape (whatever the coordinator sweep last
  broke) now passing through the checker path.
- A literal-range matrix: each builtin numeric type implied by a platform
  signature × in-range/out-of-range literal → check-time fit error, no silent
  wrap (the #9565 family).
- A debug-build test asserting checked roots are identical before and after
  executable finalization (the publication-freeze invariant).

## Related projects

- [../big/total-dispatch-plans.md](../big/total-dispatch-plans.md) — depends
  on this project: dispatch plans cannot be total while platform-typed values
  are still flex vars at check time.
- [../big/exact-numeral-pipeline.md](../big/exact-numeral-pipeline.md) —
  depends on this project: it removes one of the two places literal range
  facts get dropped.
- [../big/unify-build-pipelines.md](../big/unify-build-pipelines.md) —
  independent, but benefits from the coordinator shedding its shadow
  validation layer first.
