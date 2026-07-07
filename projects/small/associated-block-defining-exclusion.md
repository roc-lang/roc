# Currently-Defining Exclusion for Associated Blocks

## Problem

Associated blocks (`Type := ... .{ ... }`) are missing one scoping concept
that ordinary declarations already have: **the item currently being defined
must not satisfy name lookups from its own right-hand side**. The gap exists
independently on the type side and the value side, and each side ships an
open bug:

- roc-lang/roc#9961 (type side) — a type module that imports another type
  module and re-exports it under the same name inside its associated block
  (`Thing : Thing`, intending "associated alias `Thing` = the *imported*
  `Thing`") is wrongly reported as a RECURSIVE ALIAS. Verified mechanism:
  the imported type module lives in scope as a **module alias**, not a
  canonical-scope type binding, so the RHS lookup of `Thing` misses
  `scopeLookupTypeBindingInCanonicalScopes` and falls into
  `ensureParserTypeBinding` (`src/canonicalize/Can.zig:2615`), whose
  last-resort branch — `activeDeclScopeDeclaresType(ident)`
  (`Can.zig:2648`, implementation at `:1310`) — finds that the active
  associated decl scope declares `Thing`… which is **the very alias whose
  RHS is being canonicalized**. It prepares a placeholder binding for it and
  resolves the RHS to itself; checking then correctly reports the
  self-alias it was handed. Controls: `Other : Thing` in the same block
  resolves the import fine, and qualified `Thing.Thing` works — only the
  name collision breaks, and only for the item's *own* RHS.

- roc-lang/roc#9912, middle layer (value side) — associated-block value
  items skip the defining-bound-vars self-reference guard that ordinary
  declarations get. Top-level `x = x` reports INVALID ASSIGNMENT TO ITSELF
  via `beginDefiningBoundVars`/`isDefiningBoundVar`
  (`Can.zig:4723`/`:4759`, diagnostic emitted at the lookup consumer,
  `:7291`); the begin calls guard ordinary decl forms (`:6580`, `:8519`,
  `:8948`) but not associated value items. Minimal repro needing no
  imports, packages, or `--main`:
  `SelfRef := [].{ with_uri = with_uri }` — the self-reference resolves to
  the item being defined, flows downstream as a manufactured self-cycle,
  and `roc check` dies in a canonicalization dependency-graph stack
  overflow instead of reporting a diagnostic. (That overflow — the
  unguarded `DemandAnalyzer` recursion — is the crash *surface*, owned by
  [../big/dependency-ordered-def-checking.md](../big/dependency-ordered-def-checking.md)
  item 1. This project owns the *producer*: the self-cycle should never be
  manufactured, and the user should get the same diagnostic top-level
  `x = x` gets.)

In the #9912 original (a platform checked via `--main` with unresolvable
URL-package imports), error recovery degrades `Request.with_uri` to an
*unqualified* lookup of `with_uri`, which then resolves to the associated
item being defined — the same value-side gap reached through a different
door. The remaining layer of #9912 (platform roots never materializing URL
packages) belongs to
[../big/unify-build-pipelines.md](../big/unify-build-pipelines.md).

Both bugs are the fragile-identity leg of the backlog disease: name-string
scope resolution lets a definition shadow what its own RHS means, because
"currently being defined" is not part of the lookup's input.

## Background

The compiler pipeline: parse → canonicalize → type-check → postcheck.
Canonicalization owns scoping; a name-resolution error here must surface as
a canonicalization diagnostic, never as downstream misbehavior. `design.md`
(Canonicalization Policy Ownership) and AGENTS.md apply.

Type modules expose a main type plus associated items declared in the
`.{ ... }` block. During canonicalization of an associated block, an
"active decl scope" tracks the block's declarations so that items can
reference each other (forward references between *different* items in the
same block are legal and must keep working).
`ensureParserTypeBinding` (`Can.zig:2615-2654`) prepares type bindings
on demand from parser declarations when a type name is not yet in canonical
scope; its fallback chain ends at `activeDeclScopeDeclaresType`, which
answers purely by name — with no notion of *which* declaration is currently
being canonicalized. `activeDeclScopeDeclaresType` has a second consumer at
`Can.zig:1499` (qualifier-path resolution) that must be audited for the
same self-satisfaction hazard.

On the value side, the defining-bound-vars mechanism is exactly the missing
concept, already built: `beginDefiningBoundVars` (`Can.zig:4723`) records
the pattern(s) being defined before the RHS is canonicalized (restored
strictly LIFO, per the doc comment at `:4748`), `isDefiningBoundVar`
(`:4759`) answers "is this lookup target the thing being defined right
now", and the lookup consumer at `:7291` turns a hit into the INVALID
ASSIGNMENT diagnostic. Associated value items canonicalize their RHS
without a surrounding begin/end pair.

## Evidence

All symbols verified in the current tree.

- Type side: `activeDeclScopeDeclaresType` (`Can.zig:1310`), consumers at
  `:1499` and `:2648`; `ensureParserTypeBinding` (`:2615-2654`).
  Repro behavior at HEAD: the #9961 shape reports RECURSIVE ALIAS; the
  `Other : Thing` and `Thing.Thing` controls work.
- Value side: `beginDefiningBoundVars` (`:4723`, LIFO doc `:4748`),
  `isDefiningBoundVar` (`:4759`), diagnostic consumer (`:7291`), begin
  sites for ordinary decl forms (`:6580`, `:8519`, `:8948`) with no
  associated-item counterpart. Repro behavior at HEAD:
  `SelfRef := [].{ with_uri = with_uri }` stack-overflows `roc check`;
  top-level `x = x` reports INVALID ASSIGNMENT TO ITSELF.
- Downstream crash surface (owned elsewhere, listed for the failure chain):
  `DemandAnalyzer.lambdaFromDef` ↔ `lambdaFromExprWithLocals` unbounded
  mutual recursion, `src/canonicalize/DependencyGraph.zig:241-263`.
- The #9912 issue shape: `.platform` root via `--main` +
  URL-package imports → unresolved import → error recovery degrades a
  qualified access to an unqualified lookup → this gap → overflow.

## Solution design

One concept, applied to both sides: **the declaration whose RHS is being
canonicalized is excluded from satisfying lookups made by that RHS**, and a
self-reference that would have resolved to it gets an explicit diagnostic
(value side) or falls through to outer scopes (type side, where shadowing
an import is the legitimate meaning).

1. **Type side.** Track the associated-block declaration currently being
   canonicalized (a single index — associated items canonicalize one at a
   time; if nesting exists, a LIFO stack mirroring the defining-bound-vars
   discipline). `ensureParserTypeBinding`'s last-resort
   `activeDeclScopeDeclaresType` consult (`:2648`) skips the
   currently-defining declaration, so resolution proceeds outward — to the
   module-alias path for #9961, where `Thing` correctly means the import.
   Audit the `:1499` consumer for the same exclusion. Two behaviors must be
   preserved and pinned by tests: forward references from *other* items in
   the block to this one, and genuinely recursive type declarations (a
   nominal type whose definition references itself is legal and is not an
   associated-*alias* self-reference — the exclusion applies to the alias
   RHS resolution path, not to recursive nominal structure).

2. **Value side.** Wrap associated value items' RHS canonicalization in the
   existing `beginDefiningBoundVars`/restore discipline (same LIFO
   contract), so the lookup consumer at `:7291` produces the same INVALID
   ASSIGNMENT TO ITSELF diagnostic that top-level bindings get. No new
   mechanism: extend the begin-site coverage to the missing form.

3. **No downstream tolerance.** Do not add cycle-tolerance to
   `DemandAnalyzer` or the checker as part of this project — the producer
   fix means canonicalization output contains no manufactured self-cycles,
   and the downstream zero-recursion hardening lands independently in the
   dependency-ordered-def-checking project. A debug assertion in
   canonicalization output ("no def's dependency summary contains itself
   unless the def is a function") is cheap and catches recurrence.

## What success looks like

- The #9961 shape checks clean: the associated alias re-export resolves to
  the imported type module, and uses of it through the re-export work.
- `SelfRef := [].{ with_uri = with_uri }` reports INVALID ASSIGNMENT TO
  ITSELF (or the type-appropriate sibling diagnostic) from `roc check` —
  no stack overflow, no downstream involvement.
- Forward references between distinct associated items, recursive nominal
  types, and top-level self-reference diagnostics all behave exactly as
  before (pinned by the matrix below).
- Value-side and type-side "currently defining" use the same discipline
  (LIFO begin/restore), not two bespoke trackers.

## How to evaluate the result

### Correctness ideal

- *Lookup inputs are complete*: "is this the declaration being defined"
  is part of the lookup, not recovered later from graph shape or checker
  behavior. Diagnostics for self-reference are canonicalization
  diagnostics.
- *No semantic change beyond the two bugs*: the only programs whose
  behavior changes are (a) self-named re-exports (now legal and correct)
  and (b) direct self-references in associated values (now a clean
  diagnostic instead of a crash).
- Behavioral: full snapshot corpus unchanged except the two new
  diagnostics/acceptances; `roc docs` and `roc check` on the #9912 shape no
  longer overflow (its import-resolution errors still report — that layer
  is out of scope here).

### Performance ideal

The exclusion is an integer compare against the currently-defining index
during (rare) fallback lookups, and the value side reuses an existing
mechanism at its existing cost. No new allocations on the hot path, no new
traversals. Nothing to measure beyond confirming `roc check` timing is
unchanged on the stress corpus.

## Tests to add

Write the regression tests first and confirm each fails on the unmodified
tree (RECURSIVE ALIAS report for #9961; stack overflow for the value-side
repro):

- `issue_9961`: two-module CLI check test — type module `Thing`, importer
  re-exporting `Thing : Thing` in an associated block; asserts clean check
  and a use of the re-exported type checking end-to-end.
- Value-side self-reference: `SelfRef := [].{ with_uri = with_uri }` as a
  snapshot test asserting the INVALID ASSIGNMENT diagnostic (and
  `.exit = .not_panic` in a CLI test to pin "no overflow").
- Preservation matrix, all as snapshots:
  - forward reference from one associated item to a *later* sibling
    (legal today, stays legal);
  - associated alias referencing a *different* imported type of a
    non-colliding name (`Other : Thing`);
  - qualified self-module reference (`Thing.Thing`) — stays working;
  - genuinely recursive nominal type in an associated block — stays legal;
  - top-level `x = x` — diagnostic unchanged (guards against the exclusion
    logic drifting into ordinary decls);
  - an associated value shadowing a top-level function of the same name,
    where the RHS references that top-level function by that name — decide
    and pin the intended resolution (outer def, by the exclusion rule) so
    the semantics are explicit rather than accidental.
- The #9912-shaped harness (platform via `--main` with an unresolvable
  import) asserting `roc check` reports import errors without overflow —
  shared with the dependency-ordered-def-checking project's DemandAnalyzer
  test once both land.

## Related projects

- [../big/dependency-ordered-def-checking.md](../big/dependency-ordered-def-checking.md)
  — item 1 (DemandAnalyzer worklist rewrite) removes the crash surface this
  project's producer fix makes unreachable; land in either order, test the
  chain in both.
- [../big/unify-build-pipelines.md](../big/unify-build-pipelines.md) —
  owns the remaining #9912 layer (`.platform` roots via `--main` never
  materialize URL packages, `src/compile/compile_build.zig:~685`).
