# Total Dispatch Plans: Resolve Every Dispatch at Check Time

## Problem

Static dispatch — picking which concrete method a call like `value.method(..)`
invokes — is resolved or re-derived in roughly seven places across three
different type representations:

1. Parser qualification chains (PR roc-lang/roc#9838).
2. Canonicalization associated-item lookup via concatenated `"M.T.method"`
   strings (PR roc-lang/roc#9714).
3. Constraint solving in `src/check/Check.zig`.
4. End-of-check ambiguity sweeps (PRs roc-lang/roc#9504, #9660, #9819).
5. Artifact publication (PR roc-lang/roc#9718).
6. Coordinator platform finalization (PRs roc-lang/roc#9762, #9873).
7. Monotype lowering (`src/postcheck/monotype/lower.zig`).

PR #9718 added a `resolution` field to `StaticDispatchCallPlan`
(`src/check/static_dispatch_registry.zig`) with variants
`unresolved_checked_plan` and `resolved_target: MethodTarget` — but the plan
artifact is half-done. Only dispatchers that are concrete at check time get
`resolved_target`. Parametric plans fall through to `dispatchTarget` in
`src/postcheck/monotype/lower.zig`, which derives a `MethodOwner` from lowered
monotype content via `methodOwnerFromType` (20+ call sites) and looks it up in
a `method_lookup_index` built by re-interning module/type names **by text**
across module views (`initMethodLookupIndex`, `appendMethodLookupIndexFromView`,
`methodOwnerInProgramNames`; module identity is matched by `Ident.textEql`
against three name spellings in `moduleViewNameMatches`). When derivation
fails, lowering panics with
`Common.invariant("checked method registry is missing resolved dispatch target")`
— and this fires in the wild on ordinary programs: issues roc-lang/roc#9858
(trivial `args.get(0)? * 10`), #9892, #9875 (dispatch lost through nominal
aliases), #9864 (package-owned nominal platform methods).

There is also a runtime-crash fallback: lowering compiles unresolved
where-clause dispatch to
`runtimeCrashExpr(..., "unresolved \`where\`-clause method dispatch on a polymorphic value")`.
Issue roc-lang/roc#9815 hit it at runtime. `design.md`'s Core Principles say
post-check stages "do not emit fallback code", and its Forbidden Shapes ban
exactly this category.

Finally, derived structural equality/hash/encode dispatch is resolved nowhere:
lowering synthesizes it on the fly, gated by `planAllowsStructural` sniffing
the plan's result mode.

## Background

The compiler pipeline is: parse → canonicalize → type-check (producing
"checked artifacts" per module; all user-facing diagnostics end here) →
postcheck: Monotype IR (monomorphization/specialization,
`src/postcheck/monotype/`) → Monotype Lifted (closure lifting) → Lambda
Solved → Lambda Mono → LIR lowering (`src/postcheck/solved_lir_lower.zig`) →
ARC insertion (`src/lir/arc*.zig`) → backends (LLVM/dev/wasm/interpreter).

Roc's static dispatch: `x.method(args)` resolves against the nominal type of
`x`. Inside a generic function, the dispatcher's type may be a type variable
constrained by a `where` clause (e.g. `where a.method : ...`); each concrete
specialization created during monomorphization must pick the concrete method.
Monotype IR is produced per specialization: checked bodies are instantiated at
concrete types, so every dispatcher type is concrete *by the time a
specialization is built* — the question is only where the resolution decision
is made and what data carries it.

`design.md` at the repo root is the authoritative post-check design. Read its
sections "Static Dispatch At The Checked Boundary" and "Static Dispatch In
Monotype" before starting. Note: "Static Dispatch In Monotype" currently
*prescribes* deriving the `MethodOwner` from instantiated monotype type
content. This project revises that section — owner derivation from lowered
type content is exactly the re-derivation that keeps breaking. The Core
Principles ("consumers must not recover missing data from source syntax,
names, body scans, ... or incidental data structure shape") are the part that
stays; the monotype dispatch algorithm changes to consume explicit evidence.

Prerequisite context: the app module must see the platform's `requires` types
during checking (see Related projects), otherwise platform-typed dispatchers
are still flex vars when plans freeze and cannot be resolved at check time.

## Evidence

All symbols verified in the current tree.

- `src/check/static_dispatch_registry.zig`: `StaticDispatchCallPlan` with
  `resolution` (`unresolved_checked_plan` | `resolved_target: MethodTarget`);
  every plan construction site initializes `.resolution = .unresolved_checked_plan`
  and a separate pass upgrades some to `resolved_target`.
- `src/postcheck/monotype/lower.zig`: `dispatchTarget` (derives owner via
  `methodOwnerFromType`, then binary-searches `method_lookup_index` in
  `lookupMethodTargetInIndex`); `MethodLookupIndexEntry`;
  `methodOwnerInProgramNames` re-interning names by text;
  `moduleViewNameMatches` matching module identity by `Ident.textEql` against
  `module_name`, `display_module_name`, and `qualified_module_name`;
  `planAllowsStructural`; the `runtimeCrashExpr` where-clause fallback; the
  `Common.invariant("checked method registry is missing resolved dispatch target")`
  panic.
- Issue history: #9858, #9892, #9875, #9864 (missing-target panics), #9815
  (runtime crash fallback reached), #9782/#9857/#9890 (coordinator-side
  dispatch revalidation cascade), #9889/#9890 (regressions from
  specialization-heavy roc-parser package code).
- `src/check/checked_artifact.zig`: `specializeResolvedStaticDispatchPlanCallables`
  (PR #9873) — publication-time re-projection of resolved plan callables,
  part of place (5)/(6) above.

## Solution design

Make the plan artifact **total**: every dispatch site leaves checking with an
explicit resolution. This is dictionary passing evaluated entirely at compile
time — evidence is part of the specialization graph, never a runtime value,
and generated code is unchanged.

1. **New resolution representation.** Replace the current two-variant
   `resolution` with:

   ```zig
   resolution: union(enum) {
       direct: MethodTarget,      // dispatcher concrete at check time
       constraint: u32,           // k: index into the enclosing def's
                                  // where-clause constraint list
       structural: StructuralKind // derived eq/hash/encode chosen by checker
   },
   ```

   `direct` is produced when the checker can resolve the dispatcher through
   its own alias/import machinery (the checker already understands nominal
   aliases and imports — the alias-loss bug #9875 cannot happen there).
   `constraint(k)` is produced when the dispatcher's type is a where-clause
   type variable of the enclosing definition; `k` indexes that definition's
   where-clause list. `structural` replaces the implicit
   `planAllowsStructural` sniffing: the checker decides derived
   equality/hash/encode explicitly.

2. **Monomorphization resolves `constraint(k)` by substitution only.** At
   every specialization call edge, the caller knows what each callee
   where-clause variable was instantiated to: either a concrete type — in
   which case the caller resolves the evidence from *checked* data at the
   constraint-introduction site (the checker recorded which `MethodTarget`
   satisfies that constraint for that concrete type) — or one of the caller's
   own where-clause variables, in which case the caller forwards its own
   constraint index. Evidence flows along specialization edges exactly like
   type arguments do. No monotype-content inspection, no name lookup.

3. **Single type-finalization point in checking (prerequisite).** Literal
   defaulting and row closure must be frozen *before* plans freeze, so the
   #9858 shape (`x * 10` where `x`'s number type defaults) is resolvable at
   plan-freeze time. Concretely: order `checkFileInternal`'s numeric
   defaulting passes ahead of dispatch-plan finalization and assert no plan
   dispatcher root changes afterward.

4. **Evidence through nested closures.** A where-clause evidence index used
   inside a nested closure behaves exactly like a compile-time capture: it is
   introduced by the enclosing definition and threaded inward. Ride the same
   identity discipline as canonical capture IDs (see Related projects) rather
   than inventing a parallel mechanism.

5. **Deletions (not fallbacks).** After migration:
   - `methodOwnerFromType`-based dispatch resolution in
     `src/postcheck/monotype/lower.zig` (`dispatchTarget`'s derivation path);
     `methodOwnerFromType` itself survives only if non-dispatch callers
     remain, else it goes too
   - the text-keyed `method_lookup_index` (`MethodLookupIndexEntry`,
     `initMethodLookupIndex`, `appendMethodLookupIndexFromView`,
     `lookupMethodTargetInIndex`, `methodOwnerInProgramNames`,
     `moduleViewNameMatches`)
   - the `runtimeCrashExpr` where-clause fallback and `planAllowsStructural`
   - `unresolved_checked_plan` as a variant — its absence is the point.

6. **Migration order.** Migrate one plan class at a time, each landing with a
   debug boundary check "no unresolved plan of this class reaches lowering":
   (a) value dispatch; (b) iterator `for` plans (`.iter`/`.next`);
   (c) equality/hash; (d) `parser_for`/`encode_to`. Only after all classes
   migrate does the deletion PR land.

7. **Serialization/caching.** Checked artifacts are cached. `direct(target)`
   serializes as (dependency artifact reference, proc template id) — legal in
   check artifacts because dependency artifact hashes are already part of the
   cache key, so a stale target reference cannot survive a cache hit.
   `constraint(k)` is module-local data and serializes as the integer.

## What success looks like

- Every `StaticDispatchCallPlan` in every published artifact carries
  `direct`, `constraint`, or `structural`; the compiler contains no
  representation of an unresolved dispatch after checking.
- The repros from #9858, #9892, #9875, #9864 compile and run correctly; the
  "checked method registry is missing resolved dispatch target" panic string
  no longer exists in the tree.
- The #9815 program either compiles correctly or is rejected at check time;
  the "unresolved `where`-clause method dispatch" runtime crash string no
  longer exists in the tree.
- `grep -rn "method_lookup_index" src/` returns nothing.
- `design.md`'s "Static Dispatch In Monotype" section is rewritten to
  describe evidence consumption, and the tree matches it.

## How to evaluate the result

### Correctness ideal

Invariants, and where they are enforced:

- *Totality at the checked boundary*: asserted at artifact publication in
  `src/check/checked_artifact.zig` — a plan without a resolution is a
  compiler bug (debug assertion / release unreachable per design.md).
- *Substitution-only resolution in monotype*: `src/postcheck/monotype/` never
  consults names or type content to pick a target; enforceable by review plus
  a debug counter asserting zero name-interner lookups on the dispatch path.
- *Evidence/type agreement*: at each specialization edge, debug-assert that
  the resolved target's callable type instantiates to the plan's callable
  type (this check exists today post-lookup; it becomes the only check).
- Behavioral: the full snapshot corpus, all examples, and the roc-parser
  package suite produce identical output before/after; the Evidence issues'
  repros are added as permanent regression tests.

### Performance ideal

Resolution work is bounded: once per dispatch site at check time, plus once
per constraint per specialization edge during monomorphization — both O(1)
table operations. All registry/name-lookup work leaves the hot monotype
loop: no binary search, no text interning, no per-call-site owner derivation.
Measure monotype lowering time and total build time on specialization-heavy
code — the roc-parser package examples that produced regressions #9889/#9890
are a good corpus — plus `examples/`. Expect lowering to get measurably
faster (deleting per-site text interning and index construction); check-time
cost rises only by plan-resolution bookkeeping, which must stay within noise
on `roc check` benchmarks. Artifact size grows by one small union per plan;
cache serialization time must not measurably regress.

## Tests to add

- Regression repros for #9858, #9892, #9875, #9864, #9815 as end-to-end run
  tests with expected output.
- A where-clause forwarding chain: generic `f` calls generic `g` calls
  generic `h`, dispatch resolved three levels up; assert correct target per
  specialization (exercises `constraint(k)` forwarding).
- Nested-closure evidence: a closure inside a generic function dispatches on
  the outer where-clause variable.
- Alias/import matrix: dispatcher behind a nominal alias, a re-export, and a
  package-qualified import (the #9875/#9864 shapes) resolving to `direct`.
- Structural evidence: derived eq/hash/encode on records/tag unions chosen by
  the checker; assert the plan carries `structural`, not a lowering guess.
- Cache round-trip: serialize and reload an artifact with `direct` targets
  into a dependent build; assert identical resolution.
- Per-class boundary tests during migration: feed each plan class through
  lowering with the debug check active.

## Related projects

- [../small/check-app-against-platform-requires.md](../small/check-app-against-platform-requires.md)
  — prerequisite: plans cannot be total while platform-typed dispatchers are
  flex vars at check time.
- [../big/canonical-capture-id.md](../big/canonical-capture-id.md) — evidence
  threading through nested closures rides the same compile-time-capture
  discipline; do it before or together with step 4.
- [../big/generalization-time-ambiguity.md](../big/generalization-time-ambiguity.md)
  — shares the constraint-provenance foundation; `constraint(k)` gives each
  dispatch constraint a stable identity that ambiguity reporting can also
  use. Build provenance once, serve both.
