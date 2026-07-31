# reunify.md — Eliminating Logical Type Re-Derivation After Checking

This document consolidates three independent drafts (reunify2/3/4) into the
authoritative plan. Its current-state claims have been verified against the
code; its remaining hypotheses are explicitly marked for Slice 0
measurement. The premise ledger (§3) records what was checked so settled
facts are not re-litigated as disagreement.

---

## 1. Decision

Roc stops reconstructing logical typing facts in postcheck.

Checking already proves the program's logical type relationships. The checked
artifact must publish those relationships explicitly — including polymorphic
scheme binders and the substitution chosen at every scheme-use edge — and
Monotype must instantiate that data by directed substitution into an
immutable, hash-consed type pool. Postcheck must not create fresh logical
type variables, solve rows, infer defaults, rediscover static-dispatch
targets, or constrain a callee's type by replaying argument and result
equalities.

This project does **not** claim that every postcheck equality-closure problem
is type inference. Two later responsibilities remain and must be named rather
than hidden inside value-type unification:

1. Monotype creates representation facts that do not exist during checking —
   minted and forced-dynamic iterator representations, generated evidence
   backings. When distinct representations of one logical type meet,
   postcheck applies a small explicit **representation-join algebra** (§10).
2. Lambda Solved computes callable flow for the first time. Its callable
   slots, finite lambda sets, erased-callable requirements, and recursive
   equality closure remain a solver-owned responsibility (§12) — the sole
   general postcheck unifier, kept exactly as it is.

The ownership model:

```text
checking
  owns logical types, schemes, use-site substitutions, defaults, and dispatch

Monotype instantiation
  substitutes checked schemes into immutable logical monotypes

Monotype representation closure
  joins only explicit postcheck-created representation facts

Lambda Solved
  computes callable flow and callable representation

LIR and backends
  consume the explicit results without recovering any of the above
```

The end state is not "rename the current instantiation graph." The current
graph mixes logical instantiation, row solving, defaulting, representation
selection, specialization stabilization, and mutable snapshots. The end state
separates those responsibilities and deletes the logical solver entirely.

Why this matters:

1. **Correctness.** Re-deriving means re-implementing type semantics, and the
   implementations drift. A large fraction of recent miscompile-class bugs
   trace to a divergence between what `check` concluded and what a postcheck
   solver re-concluded. (Numeric defaulting is already centralized behind
   `src/types/literal_defaulting.zig`; what still drifts is where and when
   rules are applied, plus row structure and representation identity. This
   project finishes the consolidation.)
2. **Performance.** Per-specialization union-find graphs, repeated constraint
   solving, and repeated structural digests re-pay costs the checker already
   paid. Substitution over frozen schemes is a memoizable, allocation-light
   copy; interned monotypes give O(1) interning equality (the other
   relations — logical, representation, specialization, cache — remain
   separately defined, §8.2).
3. **Simplicity.** The instantiation-graph machinery (evidence refill,
   cross-specialization snapshots, logical request refinement, broad
   deferred template sealing) exists to manage the consequences of
   re-solving. That machinery deletes. The only scheduling residue is the
   explicit pre-publication closure of representation dependency components;
   it never carries or revises a logical fact (§11).

The repository already proves the target pattern at scale: static dispatch is
decided during checking and consumed during lowering (§6.5), and layouts are
interned once and read by every backend. This project extends that
discipline to value-type structure itself.

---

## 2. Goals and non-goals

### 2.1 Goals

At completion:

1. Every postcheck-visible specialization source has an explicit frozen
   scheme with an owner and an ordered (possibly empty) binder list —
   monomorphic definitions, required values, and synthetic templates get
   zero-binder schemes, so no ownerless special paths exist.
2. Every ordinary checked scheme use publishes an explicit substitution
   vector and checked dispatch-evidence vector.
3. Monotype instantiates schemes by substitution. It never reconstructs
   binder assignments from argument, result, or row shapes in production.
4. Every Monotype type is immutable after publication and is created through
   a hash-consing API.
5. Interning equality, logical equality, representation compatibility,
   specialization equality, and cache identity are separately named and
   tested (§8.2).
6. Postcheck representation joins occur only through a small inventoried
   terminating algebra whose inputs already have equal eager logical types.
7. Logical recipes and represented templates have separate cache identities;
   an open occurrence always receives fresh representation slots, and a warm
   specialization-cache hit replays every stored representation output.
8. Lambda Solved callable-slot identity depends on type occurrences and
   explicit value flow, never on incidental Monotype interning.
9. Ordinary static dispatch consumes checked evidence. Only declared
   compiler-generated edges perform exact component lookup.
10. The Monotype logical instantiation graph, logical type variables, row
   solver, evidence refill, mutable Monotype views, and **logical-key**
   stabilization deferral are deleted. A much smaller pre-publication
   representation-dependency scheduler remains (§11): it stabilizes only
   explicit representation slots and can never revise a logical binding.
11. CI mechanically prevents their return.

### 2.2 Non-goals

This project does not:

- change Roc's source type system;
- change generalization or instantiation during checking;
- add polymorphic recursion;
- add an error monotype;
- replace Lambda Solved's callable equality closure with a dataflow
  approximation (§12.4 records why that rejected idea must stay rejected);
- change iterator tier policy, generated-evidence policy, or runtime
  layouts (one declared exception: Slice 1's occurrence-based lambda
  cloning deliberately permits finer lambda sets, hence finer closure
  layouts, where today's within-clone sharing was incidental);
- make process-local type IDs stable across compiler runs;
- make structural equality erase nominal or opaque identity;
- introduce a second production lowering route;
- preserve an old path as a selectable alternate route;
- accept output changes merely because the new architecture appears simpler.

---

## 3. Premise ledger: what verification established

Verification passes (2026-07-21/22) checked the current-state claims
against the code; a re-verification pass (2026-07-28) re-checked every one
of them after `origin/main` merged in, and rewrote the entries main had
overtaken. Entries carrying **[main]** are ones main resolved
independently of this branch. Implementers should treat this ledger as
authoritative.

**Confirmed true:**

- **[main, partly resolved]** The Monotype re-solver machinery was a
  coordinated defense against one root cause: types move during lowering
  because lowering re-solves them. Two of its four legs are gone. Refill-
  in-place is deleted: a solved node is read only as an immutable snapshot
  (`GraphTypeFinals`, `initActiveSnapshot`, `sealNode`, `freezeRelations`
  in `monotype/solve.zig`), and `structural_test.zig` pins the refill API's
  absence. Deferral survives under a new name and a new trigger:
  `DraftTemplateSpec` with `state: DraftSpecState` in
  `BodyDraftStore.template_specs`, resolved by
  `Builder.resolveDeferredTemplateSpecs` **after the caller's graph has
  frozen**, rather than once the requester's types stop moving.
  Snapshot-on-import and conflict-on-over-demand remain, and the second is
  now stronger (see the `importMono` entry below). `monotype/solve.zig`'s
  own comments and a comptime `assertNoNodeId` test still pin the intended
  end state ("Completed Monotype views must expose only `TypeId`s and
  durable AST ids, never these graph-local ids").
- The Monotype stage reads **no mutable `src/types` solver state** — only
  the frozen artifact, the dispatch registries, and canonical `ModuleEnv`
  structure for numeral payloads. Function kind/effectfulness is fully
  resolved at the checked boundary (`finalizedFunctionKind`; Monotype
  function nodes have no kind field). The disease is confined exactly as
  claimed.
- Per-node type coverage exists: every checked expression and pattern maps
  to a `CheckedTypeId` (`TypedCIR.Module`'s `exprType`/`patternType`;
  `CheckedTypePublication.rootForSourceVar` at finalization).
- The Lambda Solved architecture is a faithful port of cor's `lss`
  lambdasolved with one deliberate divergence (§12.3), and its exemption
  from this project is correct (§12.1).
- A hash-consing interner for Monotype types exists and now sits **at the
  production construction boundary**: `Store`'s own `intern*` constructors
  (`internFunc`, `internRecord`, `internTagUnion`, `internNamed`,
  `internRecursiveGroupRoot`, `internFilledNode`, …) are the entry points
  every Monotype construction goes through, and `enableInterning` /
  `internEnabled` is the switch that turns content dedup on for a store.
  Dedup is **off** on production stores, so today each call is a plain add;
  the canonicalization the constructors perform (record-field
  normalization, storage-transparent alias erasure in `internNamed`) is
  independent of the switch and is always in force. `excludeFromDedup`
  keeps a snapshot id that reads a live graph node out of the buckets.
  Two properties it does **not** yet have, stated here so they are built
  rather than assumed: its digest and equality paths unwrap **every backed
  alias**, with no `builtin_owner` exception, while `dispatchHeadContent`
  alone retains a builtin-owned alias as a dispatch head and checked
  canonical keys (`canonical_type_keys.zig`) preserve alias identity. Those
  are three different alias stories today; §8.2 chooses one target and the
  remaining Slice 3 work changes digest, equality, dispatch-head use, and
  verification together. Its recursive-group builder also registers only the
  selected group root in the interner bucket, so entry-order-independent
  identity for every cyclic node is a build task (§8.3), not an inherited
  property.
- The checker already computes the instantiation mapping this design
  needs: `src/types/instantiate.zig`'s `Instantiator.var_map` holds the
  resolved source-variable → fresh-variable map for each instantiation.
  `ModuleEnv.SchemeUseRecord` (slot kinds `value_use` /
  `nested_function_use` / `dispatch_target` / `shared_value_use`) still
  records only the constrained scheme vars, in nondeterministic map order.
  Alongside it, `ModuleEnv.SchemeUseSiteRecord` now records the
  **complete actual vector positionally**, keyed by
  `(use_node, slot_kind, slot_data, scheme_owner_node)` with a
  `defining_module_hash` for imported schemes; its written form is
  `CheckedInstantiationSite`, which carries `actuals_start`/`actuals_len`
  into `CheckedTypeStore.instantiation_site_actuals` (unreached positions
  marked by `checked_instantiation_actual_unreached`) and
  `evidence_start`/`evidence_len` into `StaticDispatchPlanTable.evidence_refs`.
  Nothing lowers from it yet — it is verified data, not authority.

**Failed verification — corrected in this document:**

- **"The scheme representation already exists."** It did not; it does now.
  When this ledger was first written, `CheckedTypeScheme.gv_start`/`gv_len`
  (`src/check/checked_artifact.zig`) defaulted to zero, no production site
  set them, `generalizedVars()` returned an empty slice for every real
  scheme, and nested let-generalized definitions got no scheme entry at
  all. Slice 2 closed that gap: `Check.captureSchemeSnapshot` selects the
  binders at `Rank.generalized` in `identityVarsFromVar` order and records
  them through `ModuleEnv.recordSchemeSnapshot`, and
  `publishSchemeSnapshot` writes them into `type_id_pool` from three
  production sites — required values, top-level defs, and
  `publishNestedSchemes` for every otherwise-unwritten
  `SchemeSnapshotRecord.owner_node` (annotation-owned schemes, inner
  lambdas, block-local bindings). Schemes also gained `snapshot_root` and
  `CheckedCapturedBinder` (`captured_start`/`captured_len`). Consumers read
  the binders: `spec_rehearsal.zig`, `reunify_shadow/shadow.zig`, and the
  `scheme.gv_len == site.actuals_len` boundary check.
- **"Poisoned `.err` types legitimately reach postcheck."** False.
  `problemAllowsLoweringWithUserErrors` (`src/compile/compile_package.zig`)
  returns `false` for `.type_mismatch` and every type-shaped problem; the
  only lowerable problems are unresolved dispatch with a canonicalization-
  inserted runtime-error node, and `effectful_function_name`. Monotype
  hard-rejects poison (`.err => Common.invariant("erroneous checked type
  reached Monotype lowering")`, `monotype/lower.zig`, two sites). This
  design adds **no error monotype**; the boundary verifier instead proves no
  executable Monotype input contains a reachable `.err` (§7.5).
- **"Dispatch can be resolved by registry lookup at ground types."** That
  would regress the checked boundary. Checking publishes per-site
  resolutions (`direct`, `constraint(depth, index)`, `structural`, checked
  error, unreachable); ordinary lowering consumes them, and exact registry
  lookup is reserved for compiler-generated edges with no checked
  instantiation record. Preserved verbatim (§9.7).
- **"Specialization requests are ground and final by construction."**
  Partly proven now. The counter-mechanisms have thinned: **[main]**
  `importMono` copies an unlinked Monotype in as **closed** structure
  (`.ext = newNode(.empty_tag_union)` for tag unions,
  `.ext = newNode(.empty_record)` for records), so a later attempt to widen
  it is a unification conflict rather than a silent rewrite of another
  specialization's final type — callee-row widening can no longer flow
  backward into the requester's node. What remains is the requested-vs-
  solved distinction in `monotype/specialize.zig` (`refineRequest`,
  `appendAliasEntry`) and the expected-return back-constraint. The corpus
  measurement (§13.3) shows the requested and solved digests differ on 12
  snapshot-corpus records and that every measured constraint-replay site
  outside the new binder gap is redundant, so the remaining mechanisms
  carry no logical information the checked data lacks.
- **"Only recursive specialization requests need representation
  stabilization."** False as a premise. Calls are discovered mid-body and
  today's builder defers every procedure-template request made inside the
  active specialization until that requester's types stabilize. A
  non-recursive argument can gain representation information later in the
  same caller draft. The replacement therefore operates over explicit
  representation dependency components and treats every open interface as
  provisional (§11), rather than using call-graph recursion as the boundary.
- **"Postcheck makes no decisions beyond matching/defaulting/dispatch."**
  False. Monotype mints iterator representations and applies an explicit
  tier relation (`IteratorRepresentation`: `none`/`minted`/`forced_dynamic`
  with `public_minted`/`forced_dynamic`/`minted_join` outcomes,
  `Type.iteratorRelation` in `monotype/type.zig`, shared with Lambda
  Solved); generated opaque evidence selects a backing. These are
  legitimate representation joins and get a named home (§10) instead of
  deletion or denial. Two things about them changed with main, and §10 must
  be read against the new shape:
  - **[main]** Backing selection is no longer by score.
    `generatedOpaqueEvidenceScore`, `generatedBackingScore` and
    `isScoreSelectedEvidenceOwner` are deleted; the producer-authored
    `BackingAuthority` (`generated_private` vs `checked_public`) decides,
    deterministically, in `unifyGeneratedOpaqueBacking` and
    `relateGeneratedPrivateEvidence`. Score selection survives **only** in
    `representation_policy.zig` (`chooseGeneratedEvidenceBacking`,
    `evidenceOwnerUsesScoreSelection`), which no production stage consults
    — its readers are the closure engine and the Debug rehearsal, and both
    real producers pass `.score = 0`.
  - **[main]** Tier classification became graph-aware. For two `.minted`
    operands, `InstGraph.iteratorRelation` decided the answer from in-graph
    provenance the shared descriptors did not carry:
    `InstNamed.generated_iterator` presence and `callable_evidence` digest
    equality, `def.iterator_kind`, and `sameNamedArgs`. Tier inputs are
    also no longer minted at node creation:
    `finalizeGeneratedIteratorRepresentations` computes
    `generatedIteratorDepth` and `iteratorRootRequiresForcedDynamic` after
    relation production and before any durable Monotype is sealed. §10.3
    already required generated identity to be an explicit input, so those
    provenance reads are now descriptor inputs of the shared policy: a
    representation whose producer has not sealed it states its **minting
    identity** (the callable evidence it is being minted under) and the
    caller states its own **component agreement** for the pair. The graph
    calls `iteratorTierRelation` with those inputs and holds no second copy
    of the rule; the depth and forced-dynamic pass stays a producer
    decision, taken by the closure engine as a declared input rather than
    derived by the relation (§10.1, §10.3).
- **"A production matching walk is the right way to compute bindings."**
  Rejected in this consolidation. A matching walk must re-implement type
  equality (alias transparency, head canonicalization, nominal backing
  rules, row closure) — exactly the drift-shaped surface this project
  exists to remove, reproduced in miniature. The checker already had the
  binder assignments in `var_map`; this design publishes them (§7.2) and
  demotes the matcher to a Debug boundary verifier (§7.6).
- **Interning silently coarsens lambda sets through the cloning boundary.**
  `lambda_solved`'s `TypeCloner` memo was keyed on Monotype `TypeId` within
  each clone, so hash-consing structurally equal function types would have
  merged callable slots without any value-flow edge (`{ f : I64 -> I64,
  g : I64 -> I64 }` sharing one slot). Slice 1 closed this: the cloner
  keeps an active-path map rather than a completed-graph memo, so a
  reservation is reused only by a genuine recursive back edge and a later
  non-recursive occurrence of one monotype id clones fresh with its own
  callable slot. Callable-free subgraphs may still share a completed clone,
  behind a `containsCallableOccurrence` proof (§12.5).
- **The lambda decision inventory was undercounted ~3× in early drafts.**
  The verified census (§12.4 item 5) includes the alias unwrap
  (`transparentAliasBacking`), the generated-private evidence relation
  (`unifyGeneratedOpaqueBacking` and its expression-side twin), and four
  distinct iterator joins (`unifyForcedDynamicIterator`,
  `unifyIteratorOwnerStampedPublic`, `unifyGeneratedIteratorJoin`,
  `unifyPublicGeneratedIterator`) — all verified present in
  `lambda_solved/solve.zig`. **[main]** Two of the items that drove the
  original undercount are now deleted rather than classified:
  `in_iter_backing` and `forced_dynamic_backings`, both of which lived in
  the erasure and cloning passes. The census list is pinned by line count
  in `ci/check_reunify_manifest.pl`, so it can no longer drift out of date
  silently.
- **The Lambda-Mono differential harness cannot detect mutations inside
  `lambda_solved`.** Both of its sides consume the same solved program, so
  a mutated set corrupts both identically; set-coarsening is usually
  behavior-preserving, so output tests miss it too. Lambda-set hardening
  uses direct expected-set and invariant tests (§12.6).
- **`design.md` (repo root) is the authoritative post-check design.** Its
  Forbidden Shapes ban alternate post-check lowering paths and comparing
  against another lowering path to decide compiler behavior. The migration
  therefore uses Debug-only shadow verification and a single authority flip
  by deletion, and `design.md` is amended at the start of the project, not
  the end (§13, Slice 0). Note: `design.md`'s Lambda Solving section still
  claims the solver generalizes and instantiates — stale; the code does
  not (§12.3). Slice 0 corrects it.

---

## 4. Background: the pipeline, for readers new to this codebase

The compiler (in `src/`, written in Zig) is one build module per directory
with an explicit dependency graph (`src/build/modules.zig`). The stages:

```
source text
  │  src/parse            tokenize + parse → AST
  ▼
  │  src/canonicalize     name resolution, desugaring → CIR ("canonical IR"),
  │                       stored in a ModuleEnv (one per module)
  ▼
  │  src/check            Hindley–Milner inference over the CIR, using
  │                       src/types (union-find store + unifier). Ends in
  │                       checking finalization, which publishes a frozen
  │                       CheckedModuleArtifact.
  ▼
  │  src/postcheck        post-check pipeline, driven from
  │                       src/lir/checked_pipeline.zig
  │                       (lowerCheckedModulesToLir):
  │                         Monotype        (monomorphization)
  │                         MonotypeLifted  (closure lifting; SpecConstr in
  │                                          inline mode; capture recompute)
  │                         LambdaSolved    (lambda-set solving)
  │                         SolvedInline    (inline analysis)
  │                         SolvedLirLower  → LIR (materializes the Debug-only
  │                                          LambdaMono oracle as it goes)
  ▼
  │  src/lir              LIR passes (TRMC, reachability, ARC refcount
  │                       insertion, etc.)
  ▼
  │  backends             interpreter (src/eval), dev/native, wasm, LLVM —
  │                       all four consume the same LIR and the test suite
  │                       requires byte-identical results across them.
```

Key vocabulary:

- **CIR**: the canonicalized IR. Every expression/pattern index has an
  associated type variable during checking, and a frozen `CheckedTypeId`
  after it.
- **Monomorphization / specialization**: polymorphic functions compile as
  separate copies per distinct concrete use type. A polymorphic body in the
  checked module is a **template**; postcheck instantiates templates on
  demand as calls are lowered.
- **Static dispatch**: method-style calls (including `where`-clause
  obligations) resolve at compile time — no vtables. Checking records
  per-site resolutions ("dispatch evidence"); lowering consumes them.
- **Lambda sets**: to compile first-class functions without universal
  boxing, the compiler computes, per function-typed value, the set of
  concrete lambdas that can flow into it. LambdaSolved produces this.

`design.md` at the repository root is the authoritative design for
everything after checking; this project amends it in Slice 0 and keeps it
amended slice by slice.

---

## 5. How types work during checking (`src/types` + `src/check`)

The checker's mutable representation is what must not leak past checking.

### 5.1 The union-find store

`src/types/store.zig`:

- A type variable is `Var = enum(u32)` (`src/types/types.zig`).
- Each `Var` indexes a `Slot`: either `root: DescStore.Idx` (representative,
  pointing at a `Descriptor { content: Content, rank: Rank }`) or
  `redirect: Var` (unified into another class).
- `Content` is a union: `flex`, `rigid`, `alias`, `structure: FlatType`
  (records, tag unions, functions, nominals, numbers…), and `err` (§5.4).
- `resolveVar` chases redirect chains, with path compression.

Equality between checker types is not integer comparison, and a `Var` is
meaningless outside its module's `ModuleEnv`.

### 5.2 Unification

`src/check/unify.zig` implements unification as an explicit work-list
machine (stack-safe on deep types). Merging writes one `Descriptor` and
redirects the other class (`Store.union_`). Speculative unification is
supported by savepoints plus `MismatchBehavior.write_no_report`, which
suppresses poisoning so the caller can roll back.

### 5.3 Generalization and instantiation

Rank-based generalization (`src/types/generalize.zig`) marks variables
unconstrained at a definition's binding level as **generalized**. Checking a
use instantiates (`src/types/instantiate.zig`): copy the type, replacing
generalized variables with fresh ones, memoized through `var_map` with
insert-before-recurse so shared/recursive structure is preserved.

A polymorphic type is a **scheme** — a body plus its generalized variables.
During checking, instantiation substitutes fresh unknowns. During postcheck,
it substitutes fully concrete types — which is why postcheck needs a copy
under an explicit binding, not a solver. And critically: **at each
checking-time instantiation, `var_map` momentarily holds the authoritative
answer to "which actual type did each binder receive at this use?"** This
design captures that answer instead of letting postcheck reconstruct it
(§7.2).

### 5.4 Poison, and what actually reaches postcheck

When checking finds a type error it records one diagnostic and unifies the
offending variables with `content = .err`; `.err` unifies with anything, so
one error does not cascade through diagnostics.

**Contract (current, and kept):** poisoned checked type payloads do not
reach Monotype. `problemAllowsLoweringWithUserErrors`
(`src/compile/compile_package.zig`) permits lowering-with-errors only for
the effectful-naming warning and for unresolved dispatch that
canonicalization already replaced with a runtime-error *node*; every
type-error-class problem blocks lowering. Both Monotype consumers of the
`.err` payload are hard invariants. Programs that "lower with user errors"
do so through canonicalization-inserted runtime-error nodes — ordinary
ground code, not poisoned types.

This design adds **no error monotype**. If a future project wants poisoned
types to lower, it must first change the checked boundary and every
downstream consumer deliberately.

---

## 6. Current state, honestly inventoried

### 6.1 `CheckedTypeStore`: frozen type payloads

Checking finalization produces `CheckedModuleArtifact`
(`src/check/checked_artifact.zig`): flat, relocatable plain-old-data
(POD shapes comptime-asserted), cached and consumed by postcheck. Payloads
are addressed by `CheckedTypeId = enum(u32)`. The payload inventory — listed
exhaustively because translation totality depends on it:

| payload | translation classification |
|---|---|
| `alias` (args + backing) | transparent — resolve through to backing |
| `record`, `record_unbound`, `tuple`, `function`, `tag_union`, `empty_record`, `empty_tag_union`, `nominal` | head constructor |
| `flex`, `rigid` (the `CheckedTypeVariable` forms) | variable — bound by a published substitution or explicitly disposed (§7.4) |
| `err` | blocked from postcheck by the §5.4 contract; invariant failure |
| `pending` | build-transient reservation; must never survive finalization (§7.5) |

Notes:

- **Head multiplicity is real.** A closed record can be represented as
  `record` with an `empty_record` extension or as `record_unbound`; likewise
  the tag-union forms. Interning must canonicalize these to one form (§8.4).
- **Nominals carry identity, not expansion.** The nominal payload holds
  name/origin/args; the backing lives on the separate nominal declaration.
  Builtins are represented inside the nominal payload, not as a separate
  variant. Backing access is a sanctioned operation at explicit
  construction/destruction/layout edges, never a generic mismatch path.
- `CheckedTypeVariable` carries: optional name, static-dispatch constraints
  (`CheckedStaticDispatchConstraint`), and defaulting evidence
  (`numeric_default_phase: ?NumericDefaultPhase`,
  `row_default: ?RowDefault`). The artifact re-exports the defaulting and
  numeral oracles (`literal_defaulting`, `exact_numeral`) for precisely
  this downstream consumer.

### 6.2 Schemes: the container and the data both exist now

`CheckedTypeScheme` (`id`, `key`, `root`, `snapshot_root`, `gv_start`,
`gv_len`, `captured_start`, `captured_len`) and its `generalizedVars()`
accessor exist, carry real data, and round-trip through serialization.
Slice 2 closed the gap this section originally described: binders are
selected at generalization (`Check.captureSchemeSnapshot`), carried on
`ModuleEnv.scheme_snapshot_binders`, and written by `publishSchemeSnapshot`
for required values, top-level defs, and — through `publishNestedSchemes` —
every nested owner (annotation-owned schemes, inner lambdas, block-local
bindings). `CheckedInstantiationSite` carries the positional actual vector
and the evidence range for each use site (§7.2).

What has **not** happened is the consumer change: Monotype still reads
`.root` and re-derives generalization in its own graph. The data is
verified and inert; the disease is now confined to the consumer.

### 6.3 Canonical type keys, and identity layering

`src/check/canonical_type_keys.zig` computes deterministic content digests
(`CanonicalTypeKey` / `TypeDigest`) with a defined first-encounter
enumeration order for identity variables (`identityVarsFromVar`). Its header
states the rule this project generalizes: *"Post-check stages consume the
resulting keys; they must not recompute them from source syntax or from
environment lookup."*

Layering that must never blur: in-memory interned ids are process-local and
never serialized as identities; canonical digests remain the cache,
serialization, and cross-module currency; and the store's existing
distinction between full type identity and specialization identity
(generated evidence and named backings deliberately affect them
differently) is preserved, not collapsed (§8.2).

### 6.4 Per-node type coverage

Every checked expression and pattern carries a frozen type root. The whole
body of every definition is type-annotated at `CheckedTypeId` granularity —
the property that makes substitution-based lowering possible once §7's
binder and site gaps are closed.

### 6.5 Dispatch evidence, and its ownership rule

Checking publishes per-site resolutions — `direct` (proved concrete
target), `constraint(depth, index)` (supplied per specialization edge),
`structural`, `checked_error`, `unreachable_dispatch` — and Monotype
lowering materializes and consumes each specialization's evidence vector.
Exact registry lookups by `(owner, method)` happen only for
compiler-generated edges with no checked instantiation record
(structural-derivation internals, inspect/parse/encode helpers, dispatcher
path synthesis). Lowering never derives an owner from type content and
never searches a registry by method name. `design.md` states this contract;
this project preserves it verbatim (§9.7).

### 6.6 The instantiation graph

`monotype/solve.zig`'s module doc describes today's design:

> "Checked types instantiate into union-find nodes with explicit row
> extension links; constraints unify nodes order-independently; Monotypes
> use immutable read-only snapshots of fully resolved nodes.
> Cross-specialization edges import finished Monotypes as snapshots, so a
> specialization that needs more than its requested type is a unification
> conflict rather than a silent rewrite of another specialization's final
> type."

Concretely, as the merged tree stands:

- A fresh union-find graph (`InstGraph`) per specialization, created in
  `lowerTemplateWithMono` and destroyed with it; `InstVariable` nodes
  carrying checked defaulting evidence. **[main]** Each specialization's
  graph is independent — cross-specialization edges carry finished
  Monotypes, never shared mutable state.
- **[main]** Mutable Monotype views are gone. `addMonoView`, `fillMono`,
  `registerNodeType`, `drainDirty`, `pointInTimeTypeForNode` and
  `unsolved_monos` are deleted, and `structural_test.zig` pins their
  absence. A node is read as an immutable snapshot through
  `GraphTypeFinals` (`initActiveSnapshot`, `sealNode`), gated by
  `freezeRelations`; `monoFor` survives only as the private snapshot
  helper behind `activeTypeViewForNode`/`finalTypeViewForNode`.
- **[main]** `importMono` imports **closed**: an unlinked Monotype copies
  in with `.ext = newNode(.empty_tag_union)` / `.empty_record`, so callee
  evidence can no longer widen a requester's imported row.
- **[main]** `DeferredTemplate` is deleted. The deferral it named survives
  as `DraftTemplateSpec` (`state: DraftSpecState` of
  `deferred`/`lowering`/`lowered`/`resolved`) held in
  `BodyDraftStore.template_specs`, resolved by
  `Builder.resolveDeferredTemplateSpecs` once the caller's graph has
  frozen and checked by `verifyDraftTemplateSpecsResolved`. Its
  `method_scope: checked.ModuleId` still carries the registry scope.
- The exact surface is pinned in `ci/check_reunify_manifest.pl`:
  `InstGraph.create` 33 in `solve.zig` + 28 in `lower.zig`, `.unify(` 19 +
  42, `importMono` 15 + 23, plus the named row/backing entry points
  (`unifyRoots`, `unifyConcrete`, `unifyThroughBacking`, `unifyTagRows`,
  `unifyRecordRows`, `unifyRowWithEmpty`, `writeOrQueueTagRest`,
  `writeOrQueueRecordRest`).

The specialization registry (`monotype/specialize.zig`) models the request
lifecycle explicitly: records are *reserved* (key registered) strictly
before lowering, a still-reserved record's request can be *refined* after a
requester's graph seals a deferred request, and completion records the
solved type — when the solved digest differs from the requested one, the
solved shape becomes an alias lookup entry pointing at the same record
(never a rekey). The requested/solved distinction is not hypothetical: the
snapshot corpus records 12 `solved_digest_differs_from_request` events.
Whether that difference carries information the frozen checked types lack
was the migration's most important empirical question, and §13.3's
measurement answers it — at every constraint-replay site the census
reaches, it does not.

### 6.7 What the graph actually decides

Every class of work the graph performs, mapped to its target home:

1. **Template-variable binding from ground context** — the dominant case;
   replaced by published substitutions (§7.2). Includes the
   expected-return back-constraint (`instantiateCallTypeFromCallerAtType`
   unifies the callee's return against the call site's expected type, so a
   variable appearing only in return position — `empty : List a` used where
   `List U64` is expected — is bound by context today). Published actuals
   cover this by construction, because checking saw the whole relation.
2. **Symmetric row solving** — `unifyTagRows`/`unifyRecordRows` mint fresh
   extensions and distribute disjoint remainders in both directions.
   **[main]** The import half of this is closed: `importMono` no longer
   keeps imported tag unions extensible, so callee evidence cannot widen a
   requester's row. Target: rows are settled at the checked boundary. The
   groundness measurement found no constraint-replay execution that carried
   row information (`row_width` is zero at every measured site); any future
   counterexample is fixed by recording fuller rows at finalization — never
   by keeping a row solver.
3. **Defaulting application** — `numeric_default_phase` → the shared
   `literal_defaulting` oracle; `row_default` → empty record/tag-union; a
   plain unconstrained checked variable with no evidence currently
   materializes as an **empty tag union** (an uninhabited slot, not a unit
   placeholder), and a surviving compiler-owned placeholder origin is an
   invariant failure. Target: directed application under explicit residual
   dispositions (§7.4).
4. **Dispatch-evidence consumption** — already lookup, not inference;
   carries over with its scoping unchanged (§9.7).
5. **Representation decisions** — postcheck-minted content joined by
   explicit policy: the iterator tier relation (`InstGraph.iteratorRelation`
   and `Type.iteratorRelation`, both adapters onto the shared
   `iteratorTierRelation`, plus the pre-seal
   `finalizeGeneratedIteratorRepresentations` pass), generated-evidence
   backing selection **[main]** by producer `BackingAuthority` rather than
   by score, and nominal-wraps-structural root selection
   (`unifyThroughBacking` keeps the nominal as the shared root). These are
   neither re-derivation nor substitution; they become the representation
   algebra (§10). The empty-tag-union-yields-to-concrete behavior is
   deliberately **not** in this category: an empty tag union acting as an
   unresolved slot is either checked bottom/residual data (§7.4) or import
   bookkeeping that deletes with the graph; §10.5 bans it from the algebra.
   **[main]** Lambda Solved's own named empty-tag-union tie-break
   (`isEmptyTagUnion`) is deleted; the behavior survives only inline in the
   same-constructor tag-union arm, and `seamResidualShapesAgree` now makes
   cross-constructor yielding impossible.
6. **Snapshot and logical-deferral bookkeeping** — consequence-management
   of re-solving; deletes with the graph. **[main]** The refill half is
   already gone (§6.6); what is left is snapshotting plus
   `DraftTemplateSpec` deferral. The real need to wait for a call's
   representation inputs holds even when the call is not recursive: a
   representation slot can gain information later in its caller's draft.
   That residue becomes §11's explicit pre-seal representation dependency
   scheduling. It carries no logical unknown, never revises a checked
   substitution, and is not the current draft-spec deferral mechanism under
   another name.

### 6.8 Lambda Solved and after

`lambda_solved/solve.zig` computes lambda sets — a fact that appears
nowhere in `CheckedModuleArtifact` — over its own store whose only
meaningful unknowns are the callable slots inside function types. It is the
first derivation of its domain, not a re-derivation, and it stays (§12).
Downstream, `solved_lir_lower.zig` and `lambda_mono/` contain **zero**
unification calls (verified); `SolvedLirLower` emits the final `LirStore`
plus one interned `layout.Store`, and from that point all four backends and
every LIR pass read stores by index. **Re-derivation of checked facts is
confined to the Monotype stage.**

### 6.9 Why this is the bug factory

The recurring hard-bug shape: checking concludes X; a postcheck solver,
re-deriving X from partially re-instantiated inputs, concludes X′ ≠ X; the
backends faithfully compile X′. Because re-derivation spans multiple
engines, a fix applied to one does not automatically apply to the others,
and digest-keyed specialization caching can additionally be poisoned by
drift. Every semantic feature added to the language currently costs
multiple implementations and as many chances to disagree.

---

## 7. Checked artifact changes

The checked boundary must make every later logical decision explicit.

```text
CheckedTypeScheme
  = owner + root + ordered generalized binders + ordered dispatch obligations

CheckedInstantiationSite
  = use-site identity + referenced scheme
  + one checked actual per binder
  + complete checked evidence vector
  + checked instantiated root

Concrete specialization request
  = translate each checked actual under the caller's binding environment
  + instantiate the referenced scheme with those logically ground BoundTypes
  + close their explicit postcheck representation occurrences (§10–11)
```

There is no production `matchSchemeAgainstGroundRequest` operation. A
matching walk exists only as a boundary verifier and migration oracle
(§7.6); using it to compute substitutions in production would reconstruct
information the checker already had — through a re-implementation of type
equality that can drift.

### 7.1 Published scheme ownership

Every postcheck-visible specialization source receives a
`CheckedTypeScheme` — generalized or not: monomorphic definitions,
required values, and synthetic templates get schemes with zero local
binders, so there are no ownerless special paths and every use-site
record references the same kind of owner:

```zig
const CheckedTypeScheme = struct {
    id: CheckedTypeSchemeId,
    key: CanonicalTypeSchemeKey,
    owner: CheckedSchemeOwner,
    root: CheckedTypeId,
    binders: CheckedTypeRange,
    captured: CheckedCapturedBinderRange, // ordered (outer scheme, binder idx)
    evidence_params: Span,
};

const CheckedSchemeOwner = union(enum) {
    top_level_def: CheckedDefId,
    nested_def: CheckedNestedDefId,
    required_type: RequiredTypeId,
    synthetic: CheckedSyntheticSchemeId,
};
```

The concrete field layout may reuse existing side pools; the semantics are
mandatory:

- root and binders describe the pristine scheme as it existed at the
  definition's generalization boundary — the checker records a **scheme
  snapshot when generalization completes**; publication serializes that
  snapshot and does not rebuild a supposedly pristine scheme from the final
  mutable solver root;
- ordering derives from **one canonical identity-slot traversal** of the
  scheme snapshot (`identityVarsFromVar`-style first-encounter order over
  the root): local binders and captured references interleave in that
  traversal, and the ordered `binders` and `captured` projections are
  both derived from it — so the order is identical in the defining
  artifact, imported projections, use-site substitutions,
  evidence-parameter enumeration, canonical scheme keys, and
  specialization bindings, and binding digests and canonical digests can
  never disagree;
- every binder appears once; nested schemes have independent owners and
  binder ranges; an outer scheme never lists an inner scheme's binders;
- **a nested scheme is a closure**: `captured` records each distinct free
  enclosing-scheme binder exactly once, in first-encounter order, as an
  explicit `(outer scheme, binder index)` pair. Instantiating a nested
  scheme depends on both its own binding *and* the values of its captured
  binders — an inner `∀b. b -> (a, b)` at identical local bindings under
  `a ↦ I64` versus `a ↦ Str` yields different types — so every memo and
  cache key for it includes the exact captured-environment projection
  (§9.4). Imported schemes likewise carry an ordered imported-binder
  projection mapping local copies back to the defining artifact's binder
  positions; consumer-side `var_map` keys refer to local variables and are
  mapped explicitly, never by shape;
- `CanonicalTypeSchemeKey` gets stated semantics: it encodes which
  identity variables are binders and which are free (today it hashes a
  root without that distinction), equivalent content keys are **not**
  owner identity, scheme descriptors are **not** deduplicated by content
  key, and every semantic reference migrates from `schemeForKey`-style
  content lookup to artifact-qualified scheme ids;
- `CheckedTypeSchemeId` is artifact-local; every cross-artifact reference
  and in-memory cache key uses an artifact-qualified
  `(CheckedArtifactKey, CheckedTypeSchemeId)` pair.

### 7.2 Published use-site substitutions

Every ordinary scheme instantiation edge publishes:

```zig
const CheckedInstantiationSite = struct {
    site: CheckedInstantiationSiteId,
    edge: CheckedInstantiationEdgeId, // stable per semantic CIR edge;
                                      // discriminates multiple
                                      // instantiations at one node
    source: CheckedUseSite,
    scheme: ArtifactCheckedTypeSchemeRef,
    actuals: CheckedTypeRange,
    instantiated_root: CheckedTypeId,
    evidence: CheckedEvidenceRange,
};
```

`actuals[i]` is the final checked type of the fresh variable created for
`scheme.binders[i]`. It is **not** inferred from call arguments later. The
checker projects these pairs from the complete `Instantiator.var_map` while
that map is available; the constrained-only `SchemeUseRecord` pairs are the
partial precedent, not a substitute. Publication resolves the recorded
fresh variables after checking settles and copies them into the checked
store without structural recovery. Two recording disciplines are part of
the contract:

- **Deterministic projection.** The actual vector is produced by walking
  `scheme.binders` in binder order and looking each binder up in the map —
  never by iterating the `AutoHashMap`, whose iteration order is not
  deterministic. (This matters doubly because today's recorded pairs are
  named, while the published vector is positional.)
- **Savepoint consistency and edge identity.** Checking instantiates
  schemes inside speculative attempts that can roll back (§5.2). Site
  recording must be savepoint-consistent — a rolled-back speculative
  instantiation leaves no record; the checker's `Probe` already snapshots
  and truncates the scheme-use records and their pair pool on rollback,
  and Slice 2 generalizes that mechanism. Re-checking is governed by
  identity, not ordering: writes are transactional against the stable
  `edge` id, and a duplicate write must be **exactly equivalent after
  resolution** — anything else is an invariant failure. There is no
  first-write-wins or last-write-wins selection (today's publication
  keeps the first re-check record by iteration order; that incidental
  rule does not survive). The Slice 2 boundary verifier has a named test
  for each: a rolled-back branch leaves no record; a re-checked edge
  leaves exactly one, equivalent record. Before that invariant becomes
  authoritative, Slice 0 measures every legitimate current re-check and
  reports whether its resolved records are exactly equivalent; a
  non-equivalent pair is a checking/publication bug to understand, not a
  case for choosing the first or last record.

**Coverage rule:** a site is published **iff the CIR edge is classified
as postcheck-visible by the checked-edge inventory** — a classification
publication can compute, unlike future demand (lowering is demand-driven,
so a dead-but-lowerable definition's edges legitimately carry sites that
are never reached). The contract splits in two checkable halves: the
boundary verifier proves exactly one record per eligible edge, and
Monotype asserts that every edge it actually consumes cites such a
record. Checker-internal instantiation kinds that lowering never consumes
(annotation subsumption, constraint-discharge internals) are outside the
inventory and publish nothing — "publish everything the checker
instantiates" would silently inflate the artifact. Within that rule the
table covers: direct calls; ordinary value uses; function values passed
without being called; binders constrained
only through an expected result; recursive and mutually recursive
references; nested generalized function construction and use; pattern-side
generalized uses; required/platform values; imported definitions; and
static-dispatch targets selected through checked evidence. In-group
recursive and mutually recursive uses publish a **dense explicit mapping**
like any other site — typically callee binder → current group binder,
since a function can be generalized externally while its in-group
references were monomorphic during checking; the `shared_definition_root`
form marks the sharing but never replaces the vector (a marker without a
vector cannot express mutual recursion).

**Cost checkpoint (Slice 2):** actuals are `gv_len` ids per site and the
instantiated root is already published as the use node's type, so expected
artifact growth is modest — but it is measured, not assumed. Slice 2
reports artifact-size, checking-time, and **checking-side peak-memory**
deltas on the corpus (per-instantiation pairs are retained until
publication) before downstream slices build on the table; if measurement
contradicts the expectation, the design returns to review rather than
silently thinning coverage.

### 7.3 Symbolic actuals and binding environments

A use-site actual need not be globally ground in the artifact: a nested use
may reference a binder owned by an enclosing scheme, becoming ground when
the enclosing specialization supplies its logical binding (its
representation may still be open). Monotype therefore carries a
lexical environment:

```zig
const BoundType = struct {
    logical: LogicalTypeIdentity, // fixed; keys logical substitution/recipes
    representation: TypeRef,      // may be a draft/slot until sealing (§9.1)
};

const BindingEnvironment = struct {
    scheme: ArtifactCheckedTypeSchemeRef,
    values: []const BoundType,
    parent: ?*const BindingEnvironment,
};
```

A binder's value is a `BoundType`, not a bare `MonoTypeId`: a `MonoTypeId`
carries representation identity (iterator tier/kind/depth, generated
owner), so it cannot simultaneously be the fixed logical binding and an
open representation-bearing occurrence — and an enclosing binder can
legitimately refer to a representation slot that has not sealed yet, which
no immutable id can express. The logical half is fixed and keys
substitution, logical recipes, and `LogicalSpecIdentity`; represented
templates and final specialization keys additionally include their declared
finalized representation inputs (§9.4, §11.1). The representation half
participates in drafts.

Translating `actuals[i]` consults this environment at enclosing-scheme
binders and never substitutes binders owned by an inner scheme; inner
schemes instantiate at their own use sites through their own binder lists.

### 7.4 Residual variable classification

After scheme ownership is known, every reachable checked variable has
exactly one disposition in a given body context:

```text
scheme binder
concrete checked structure
numeric default with recorded phase
row default with recorded row kind
explicit unreachable/bottom position
```

There is no generic "plain unresolved variable" disposition at the checked
boundary. The unreachable/bottom disposition is concrete artifact data
with two distinct encodings — `contextual(CheckedTypeId)`, where the
checker publishes the exact contextual type the position adopts (typically
the enclosing use edge's checked type), and `uninhabited`, an explicit
uninhabited leaf where no value can return — and **checking chooses one**;
postcheck never picks between them. The disposition is stored as scoped
artifact data — keyed by `(scheme owner, CheckedTypeId)`, so one checked
variable can carry different dispositions in different body contexts
without cloning roots — and `contextual` chains are banned: a
`contextual` target must itself be fully disposed, never another
`contextual`. The target must be visible from the same lexical scheme
environment, may not refer inward to an inner scheme, and is translated
under the current logical binding environment rather than copied as a raw
id. The boundary verifier rejects out-of-scope targets, chains, and cycles.
This lands in two phases: Slice 2
records the explicit disposition for every residual **without changing
materialization** — and Slice 0 first proves that every current
plain-flex-to-empty-tag-union case really is semantically bottom, so the
classification is measured rather than asserted; the direct instantiation
path (Slices 5–6) then consumes the dispositions, at which point an
undisposed residual is an invariant failure. Default application is
directed — the numeric phase selects through the shared
literal-defaulting authority, `empty_record`/`empty_tag_union` produce
closed empty rows, `contextual(id)` translates its published target under
the same lexical environment, and
`uninhabited` lowers to the uninhabited leaf. It never probes candidates,
merges rows, or asks what type would make a use work.

### 7.5 The boundary verifier

A checked-boundary verifier walks every published body and proves:

- every reachable `CheckedTypeId` is in range and not `pending`;
- no executable root reaches `.err`;
- every residual variable is a binder of exactly one visible scheme or has
  an explicit final disposition;
- every `contextual` disposition targets a fully disposed type visible in
  the same lexical scheme environment, with no inward reference, chain, or
  cycle;
- every scheme use has exactly one actual per binder, each visible from the
  site's lexical scheme environment;
- no inner binder is captured by an outer substitution;
- the published instantiated root equals the scheme root under the
  published substitution;
- every postcheck-visible edge (per the checked-edge inventory, §7.2) has
  exactly one site record, no record exists for an instantiation kind
  outside the inventory, and speculative rollback left no orphans —
  Monotype's half of the contract, that every consumed edge cites a
  record, is asserted at lowering;
- every checked dispatch plan and evidence reference is total.

It runs at publication and on cached-artifact load in Debug. Missing data
fails at the checked boundary, before Monotype begins.

### 7.6 The validation matcher

A directed symbolic matcher exists only for validation and migration:

```zig
verifyInstantiation(scheme, published_actuals,
                    lexical_captured_actuals, scoped_dispositions,
                    published_instantiated_root) !void
```

It applies the published substitution and compares the complete root —
function arguments **and** result — under §8.2's logical projection applied
identically to both sides. (Checked canonical keys preserve alias identity,
and stored interning identity may retain a builtin-owned alias, so the
matcher borrows neither normalization: it erases every backed source alias
as logical equality requires.) Nested schemes use the same captured
projection that production instantiation will use, and residuals use their
scheme-scoped dispositions; neither is recovered by shape. It preserves nominal identity,
normalizes the empty-row encodings, and ties recursive nodes with visited
pairs. It never supplies a missing actual, defaults an unbound binder,
resolves dispatch, or affects compiler output.

---

## 8. Immutable Monotype types

### 8.1 The production interner

The existing tested `Interner` scaffold in `monotype/type.zig` becomes the
only production construction API — promoted, not duplicated. Every Monotype
payload is immutable after its id becomes visible. Construction is
child-first for acyclic types; recursive groups reserve private slots, fill
each exactly once, and publish roots only after the complete group has a
digest and exact-equality bucket. Direct calls to the mutable store's
`add`, reserved-slot fills, span appenders, and payload mutation become
private to the interner and its recursive builder; generated types,
wrappers, tests, and deserialization all use the same public boundary.

**Sequencing constraint (Slice 3):** an immutable interner cannot coexist
with the graph's refill-in-place of already-published views (`fillMono` and
friends). Mutation is first isolated into graph-local cells — a graph
result commits to Monotype only when sealed, and the committed result is
immutable — and the mutable-view/refill API is deleted *before* interning
is switched on. Interning while published ids can still be refilled would
corrupt shared entries.

### 8.2 Equality has several names

Hash-consing does not collapse the compiler's equality relations into
`id == id`. The code retains explicit notions:

- **interning equality** — exact immutable content **after the pool's
  declared canonicalization**, including every identity and representation
  field that affects downstream meaning. The alias decision is made
  explicitly rather than inherited from today's inconsistent paths. A
  **storage-transparent alias** is a backed alias with no `builtin_owner`; it is
  erased *before insertion*, so no published pool id names that alias node.
  A backing-less alias is a retained marker. A builtin-owned alias is also
  retained because its explicit checked dispatch owner must survive; Slice
  0 measures where production creates the form, and Slice 3 changes digest
  and exact equality to treat it as nontransparent alongside storage. This
  is intentionally **not** the
  existing scaffold's complete behavior: today digest and equality unwrap
  every backed alias while only `dispatchHeadContent` has the builtin-owner
  exception. Promotion changes storage, digest, equality, dispatch-head
  behavior, validation, and deserialization together, and an interner
  verifier proves that no storage-transparent alias node was published.
  Checked canonical keys are different on purpose:
  they *preserve* alias identity for cache/serialization/diagnostic use on
  the checked side, and nothing may treat the two as interchangeable —
  consumers that need alias names for display read checked data, not the
  pool;
- **logical equality** — source-level type equality after the declared
  alias and representation-erasure normalization rules. This relation is
  *computed*, not abstract. Translation eagerly walks the frozen checked
  type under the current logical binding environment and residual
  dispositions:

  ```zig
  logicalIdentity(checked_type, logical_binding, dispositions)
      -> LogicalTypeIdentity
  ```

  The walk erases iterator tier/kind/depth and generated representation
  owner, and erases **every backed source alias**, including a stored
  builtin-owned alias: builtin dispatch ownership is not source-level type
  identity. A backing-less alias remains a marker because it has no type to
  project to. The walk preserves nominal declaration identity, ties checked
  cycles with an active map, and interns the resulting
  representation-free **logical skeleton** through the same canonical pool
  machinery. `LogicalTypeIdentity` is that skeleton's interned id — O(1)
  exact logical equality, with no digest-plus-witness authority. It is
  available before any representation draft or slot seals, including for a
  recursive draft cycle. Erasing a sealed represented id to a skeleton is a
  validation operation and optional memoized acceleration, never the
  bootstrap mechanism; sealing asserts that this projection equals the
  eager identity carried by the draft/slot;
- **representation compatibility** — equal logical types whose postcheck
  representation descriptors may join (§10);
- **specialization equality** — the existing callable/cache reuse
  authority;
- **canonical cache identity** — deterministic digest bytes valid across
  runs.

`MonoTypeId` equality is an optimization for interning equality within one
process. It is never serialized and never substitutes for specialization or
cache digests without an explicit proof the equalities coincide for that
key. Nominal and opaque definitions retain declaration identity even when
backings are structurally equal. Alias handling deliberately follows the
five-equalities split rather than one overloaded rule: stored interning
identity retains a builtin-owned alias; logical identity and the validation
matcher erase its backed source alias; generated component lookup reads its
explicit builtin owner before applying ordinary alias transparency. Each
path is named and tested, and no path inherits another's behavior by
accident.

### 8.3 Recursive identity is rooted

Recursive types are rooted graphs: **equivalent rooted graphs intern to the
same id regardless of construction or allocation order.** Different nodes
of one recursive component denote different rooted types and need not share
an id. (The layout store's `interned_recursive_graphs` — Tarjan SCC
discovery, visit-order back-references, per-entry-point keys — already
implements exactly this contract; reuse the technique.) Stated as a build
task, not an inherited property: the existing Monotype recursive-group
builder registers **only the selected group root** in its interner bucket,
so satisfying this contract requires registering every cyclic node's
rooted key. (The alternative — weakening the acceptance criterion — is
rejected; the registration is built.) Canonicalization
uses explicit visited maps and iterative worklists; valid deeply-nested
finite types are never rejected by an arbitrary depth cap — resource
limits, where necessary, are explicit compiler limits, and cycle detection
never depends on depth.

### 8.4 Canonical rows and heads

One canonical representation per logical record/tag shape: canonical ids
for empty record and empty tag union; `record_unbound`/`record`/empty-ext
forms translate to one canonical closed record after finalization;
tag-union extensions are closed before logical interning; fields and tags
use the existing deterministic label ordering; `pending` is never
internable. No two entry points may create distinct ids solely because one
caller used a special empty form and another used a zero-length general
form.

### 8.5 Type identity is not occurrence identity

Interning removes allocation identity on purpose. Any downstream analysis
needing expression, field, parameter, capture, row-position, or callable
**occurrence** identity must carry it explicitly. In particular, Lambda
Solved never interprets a repeated `MonoTypeId` as evidence that two
callable positions share one flow variable (§12.5). Slice 3 audits every
`TypeId`-keyed map in postcheck and classifies whether it means structural
identity, representation identity, or occurrence identity.

---

## 9. Directed scheme instantiation

### 9.1 Inputs, outputs, and the draft layer

```zig
const TypeRef = union(enum) {
    interned: MonoTypeId,                       // immutable, published
    draft: MonoDraftId,                         // compound under construction
    representation_slot: RepresentationSlotId,  // §10.2
};

instantiateScheme(
    scheme: ArtifactCheckedTypeSchemeRef,
    binding: []const BoundType,
    captured: []const BoundType,  // values for scheme.captured, in order
) -> TypeRef
```

The binding is dense and ordered exactly like `scheme.binders`, contains no
null entries and no inference variables, and is produced by translating a
published `CheckedInstantiationSite.actuals` vector under the caller's
`BindingEnvironment`; `captured` is the exact projection of the caller's
environment onto the scheme's captured binders (§7.1). The `logical`
halves key substitution and logical recipes; the `representation` halves
feed drafts and the represented-template input key — a bare `MonoTypeId`
cannot play both roles (§7.3).

Logical translation runs first and independently (§8.2). Every draft and
representation slot therefore carries its eager `LogicalTypeIdentity` from
creation; neither waits for a represented id to exist. Draft and slot
constructors are private to the instantiator/representation layer, which
checks that every child is a permitted representation of the corresponding
logical child. `TypeRef` is an implementation reference, not permission for
arbitrary represented content to masquerade under a logical key.

The draft layer exists because §10's representation slots can join *after*
a compound type containing them is built: if `List public_iter` were
interned immediately and its element later joined to a minted iterator,
the immutable list id could not follow — a stale parent. So substitution
builds **drafts** for any compound whose transitive children include a
representation slot; representation closure runs to fixpoint across a
representation dependency component (§11); only then are drafts interned
bottom-up into immutable ids and procedure bodies/cache records published
(§10.6). Types with no representation-bearing positions intern
immediately. Drafts contain no logical unknowns and perform no logical
solving — the central invariant is untouched; drafts only defer *identity
assignment* until representation sealing. Before that point body discovery
may create only §11's representation-neutral draft and provisional call
handles. Iterator/ABI-sensitive Monotype emission waits for the component
to seal; no draft is cached or consumed outside that component.

### 9.2 Translation

Walking the frozen scheme root under the already-computed logical skeleton:

- a binder owned by this scheme emits its `BoundType.representation`, while
  the parallel logical walk uses `BoundType.logical`;
- a visible enclosing binder does the same through the lexical environment;
- a concrete checked payload recursively translates its children, interning
  the result — or drafting it, when a transitive child is a representation
  slot (§9.1);
- an explicitly defaulted residual applies its recorded default (§7.4);
- a `contextual(id)` disposition translates its published target under the
  same lexical environment; an `uninhabited` disposition emits the
  uninhabited leaf;
- an inner scheme remains an inner scheme reference (no capture);
- aliases and nominals follow the declared identity/backing rules (§8.2);
- cycles use an insert-before-descend checked-node map; a recursive group
  with no representation-bearing positions goes through the interner's
  recursive-group builder directly, while one containing representation
  slots is built as a **draft cycle** (draft nodes referencing each
  other) and passes through the interner's recursive builder only at
  sealing (§10.6) — the final builder cannot run while slots are
  unsealed.

Encountering an unowned residual, `pending`, `.err`, an arity mismatch, or
a missing binding is an invariant failure. There is no recovery path.

### 9.3 Ground checked-node translation

Checked subgraphs with no visible binders may cache their logical recipes by
artifact-qualified checked address; binder-dependent recipes cache under
the exact logical binding-and-captured projection they use. Represented
templates use the stricter key in §9.4. **The occurrence-safety law governs
every structural cache in this pipeline:**

> Structural caches may contain representation-free logical
> skeletons/recipes, or fully sealed represented templates under their
> finalized representation-input key. They may never retain live draft or
> representation-slot occurrence identity. A sealed template entering an
> open occurrence is instantiated into fresh representation slots; a bare
> interned id may be reused directly only where a proof says that occurrence
> is representation-closed.

Without it, two structurally equal but independent occurrences would
share representation flow — forcing one occurrence dynamic would infect
another with no value-flow relation, the direct-path twin of the Lambda
Solved cloning bug (§12.5). Direct tests mirror the Lambda ones:
identical iterator-typed fields begin with distinct representation slots;
two instantiations at equal logical bindings but public versus minted
representation inputs do not collide; inserting one sealed template into
two open occurrences creates distinct slots; changing one does not affect
the other; an explicit value-flow relation joins them; genuine recursive
back-references reuse only the intended slot. These caches remain
optimizations only — removing them must not affect identity or behavior,
because the interner remains the structural equality authority.

### 9.4 Instantiation memoization, and two kinds of recursion

Logical and represented memoization are different tables because their
equalities are different:

```text
LogicalInstantiationMemo
  key   = artifact-qualified scheme
        + ordered bound logical ids
        + ordered captured logical ids
  value = representation-free logical skeleton / instantiation recipe

SealedRepresentationMemo
  key   = logical-instantiation key
        + ordered finalized representation-input digests
  value = sealed represented template
        + finalized effective interface / output summary
```

The captured projection is part of both identities because a nested scheme
is a closure (§7.1): identical local bindings under different outer
environments must not collide. The first table never contains a `TypeRef`,
draft, or slot. The second never receives an entry until all of its declared
representation inputs have sealed, and using its value in an open context
creates fresh representation occurrences as §9.3 requires. Evidence or
method-scope identity is included whenever the Slice 0 dependency audit
shows that it can affect the represented template; omission requires a
proof, not an assumption.

Within an unsealed representation dependency component (§11), provisional
sharing is keyed by the explicit `ProvisionalSpecId` and
`RepresentationInterfaceId` (or an exactly equivalent component-local
identity), never by the logical vector alone. Two open interfaces with equal
logical bindings are distinct until an explicit relation connects them.
Component-local draft/active maps are discarded at sealing; finalized
templates may then enter `SealedRepresentationMemo`. Serialized keys use
canonical scheme, logical-binding, captured-binding, evidence, scope, and
representation-input digests as applicable, never in-memory ids.

Type-graph recursion and procedure recursion are separate concerns:
recursive *type graphs* use the checked-node active map and the recursive
interner; recursive *procedure specialization* uses the specialization
registry's reserve-before-discover state machine (§11.3). The type memo never
publishes an unfinished immutable type to break a recursive procedure call.

### 9.5 No argument-only matching

The production path never computes a binding from parameter/argument pairs.
Doing so would miss result-only variables, non-function values, expected
lambda types, captures, and contextual row information — and would
re-implement type equality. The checked substitution vector is complete
because checking saw all those relations; the validation matcher (§7.6)
compares complete instantiated roots in Debug.

### 9.6 Compiler-generated instantiations

Compiler-generated edges with no source checked use site may not resort
to structural matching or registry search. Each uses a named
`GeneratedInstantiationRule` declared in `design.md`, with: a design
declaration; accepted and rejected tests; proof the edge has no ordinary
checked instantiation record; an exact, total binder mapping; and an exact
checked evidence or component-lookup source. There is no generic
best-effort generated instantiator. Every generated alternative in a
`RepresentationEmissionPlan` (§11.2) cites one of these rules and declares
its representation inputs and outputs before component closure.

### 9.7 Dispatch: evidence consumption, unchanged

- `direct(node)` lowers the exact checked target and its nested evidence;
- `constraint(depth, index)` reads the lexical specialization evidence
  chain;
- `structural(derivation)` follows the checker-selected plan;
- checked error and unreachable cases lower only through their explicit
  checked contracts.

Scheme instantiation carries the site's evidence vector alongside its
binding; the binding never triggers a registry query. Exact registry lookup
remains reserved for declared compiler-generated edges (§9.6), under the
same `method_scope` scoping the deferral machinery carries today.

---

## 10. Representation closure

### 10.1 Why it remains

Checking owns logical equality but does not create every runtime
representation. Monotype deliberately creates generated iterator chains,
forced-dynamic fixed points, and generated evidence backings. When values
with the same logical type but different explicit representations meet, a
decision must be made. Calling this "ordinary unification" obscures its
contract; deleting it would lose behavior; reusing a general solver would
preserve the architectural problem. The replacement is a restricted
representation relation.

### 10.2 Representation slots

While a Monotype body is being built, a position whose representation may
join uses a `RepresentationSlotId` (one arm of §9.1's `TypeRef`; compounds
containing slots stay drafts until sealing):

```zig
const RepresentationSlot = struct {
    logical: LogicalTypeIdentity, // fixed at slot creation (§8.2)
    represented: TypeRef,         // never a logical unknown; may itself
                                  // be a draft until sealing — recursive
                                  // backing joins nest
};

relateRepresentations(left: RepresentationSlotId,
                      right: RepresentationSlotId,
                      rule: RepresentationRule) !void
```

Slot construction and mutation are private to this module. Creation proves
that `represented` is an allowed representation of `logical`; every update
preserves that proof, and sealing rechecks
`logicalProjection(final_representation) == logical`. A nominal projection
never replaces a nominally typed value position with a bare backing: it
updates the explicit backing representation inside a wrapper that retains
the nominal logical identity.

Relating two slots first proves their logical keys equal —
`LogicalTypeIdentity` is an interned skeleton id (§8.2), so this is exact
id equality, not a digest match — then applies the algebra. Every call site
cites its `RepresentationRule`; the rule enum and call-site inventory live
in `design.md`. Slots also form a
specialization's **representation interface**: argument and result slots
are reserved before body discovery, so a body-produced iterator
representation flows to callers and through recursive or non-recursive open
dependencies without changing the specialization's logical type (§11.1).
Cross-specialization representation edges are explicit unpublished graph
edges, not late rewrites of cached immutable ids.

Minting representations is not part of the relation. §10.1's generated
iterator chains and forced-dynamic fixed points are producer decisions, so the
closure engine takes each finalized representation as a declared input at the
position the producer placed it, and the declared tier order (§10.4) refuses
any move back down.

The implementation may use a worklist or disjoint-set structure for its
equality closure, but its API makes "not a type solver" mechanically true:
it cannot create a logical unknown, bind a scheme variable, add or remove a
field or tag, open or close a row, default a literal, resolve dispatch,
change nominal identity, accept logically unequal inputs, or synthesize a
conversion after a mismatch.

The shared boundary with Lambda Solved is **policy, not storage**. A pure
module classifies and joins immutable representation descriptors and
returns explicit recursive obligations. Monotype alone owns
`RepresentationSlotId`, its dependency graph, and its closure engine;
Lambda Solved alone owns `TypeVarId` and invokes the same descriptor policy
inside its existing solver. Neither stage can observe or mutate the other's
slots.

### 10.3 The initial rule inventory

Each rule declares whether it is commutative, associative, and idempotent;
property tests enforce every claimed law; intentionally directional rules
name producer/consumer roles in the API rather than depending on traversal
order.

**Iterator representations** (the relation is already shared between
Monotype and LambdaSolved — keep it single-sourced):

- `public + minted → minted` (both directions);
- `forced_dynamic + public/minted → forced_dynamic` (both directions);
- equal minted identity → relate the explicitly shared item/backing
  components;
- distinct minted identities for one iterator declaration → preserve the
  declared generated owner, relate item types, and join backing information
  without dropping a step implementation (pin the issue-10170 recursive
  backing join as a test).

Compatibility requires the same public source declaration and equal logical
item type; generated identity, kind, depth, and tier are explicit inputs,
never inferred from backing shape or names. Generated identity is explicit at
every point in a representation's life: a finished representation states its
recorded producer digest, and one the producer has not sealed states its
**minting identity** — the callable evidence it is being minted under. "Equal
minted identity" for two still-minting operands means equal minting identity,
equal producer kind, and **component agreement**: the caller's own explicit
answer, over its own store, about whether the two operands' public item and
producer-minted component types already denote one representation. The policy
takes that answer as an input rather than reading a store, and a `minted_join`
keeps the still-minting side as representative, since only that side can still
be finalized to the dynamic fixed point.

**Generated opaque evidence** (`FieldNames`, `FieldName`,
`ParseTagUnionSpec`, and kin): one declared backing policy — the higher
declared score wins; an equal score must either mean exactly equivalent
backings or be covered by a separately declared deterministic semantic
tie-break; traversal/operand order is never the tie-break (Slice 0 records
current equal-score cases so the migration cannot silently change their
outcomes). Iterators are excluded from score selection because their
backings contain step-callable information that must join.

**Nominal backings**: ordinary nominal equality compares identity and
arguments; a backing is related only at an explicit construction,
destruction, inspection, or runtime-layout authority edge — and that
projection is a **distinct API**, not a peer-slot join, because
`relateRepresentations` requires equal logical identity and a nominal is
not logically equal to its backing. The nominal representation wrapper
retains the nominal's logical key and owns a separately typed backing
projection slot; sealing rebuilds the wrapper with the joined backing and
never publishes that backing as the value position itself. Alias
transparency and nominal backing access remain different operations with
different APIs.

### 10.4 Termination and convergence

"Runs to fixpoint" is a proved contract, not an implementation hope. For
each finite discovered draft component:

- every slot ranges over a declared finite-height representation domain
  built from the finite set of producer atoms discovered in that component;
  joins canonicalize to a flattened, deterministically ordered set of those
  atoms plus the declared tier. A rule can only move upward in that domain,
  never back to an earlier tier or manufacture a new atom;
- no join invents a new iterator operation, increases mint depth beyond
  producer-recorded input depth, or creates an unbounded chain of fresh
  generated owners. A derived representation has canonical identity from
  `(rule, logical identity, canonical producer-atom set)` rather than a
  nesting of pairwise join history;
- the derived-representation memo inserts that key before descending into
  backing obligations, so recursive backings terminate and revisiting the
  same join cannot mint another identity;
- every generated obligation is over a structurally smaller child pair or
  an already-registered recursive pair. An active-pair map closes cycles;
- the worklist progress measure is the finite tuple of unseen canonical
  derived keys, unprocessed relation edges, and remaining upward slot
  transitions. Every successful step strictly consumes or advances one
  member, so exhaustion is guaranteed.

The pure join policy is total on its declared compatible domain, and its
canonical result is independent of discovery and operand order. Property
tests cover claimed commutativity/associativity/idempotence and randomized
worklist order; direct termination fixtures cover self-recursive and
mutually recursive minted backings, including issue-10170. A rule that
cannot state this measure and canonical identity does not enter the
algebra.

### 10.5 What is not a representation rule

These current behaviors must not survive in the algebra: open-row merging;
empty-tag-union-as-unresolved-slot; parameter/result back-constraint
propagation; generalized-variable binding; numeric or row defaulting;
ordinary dispatch resolution; "try the nominal backing if heads differ" as
a generic mismatch path. Those are logical or checked-evidence
responsibilities, settled before a representation slot is created.

### 10.6 Sealing

After the relation reaches fixpoint across one representation dependency
component (§11), every draft is interned
bottom-up (children first, recursive groups through the interner's group
builder) and every body position receives its final immutable
`MonoTypeId`; no `RepresentationSlotId` or `MonoDraftId` survives past
this boundary. No unsealed identity crosses outside the current sealing
component or enters a published procedure body or cache; members of that
unpublished component may refer to one another through provisional handles
and slots until they seal together. Sealing asserts every final
representation's logical projection equals the eager identity on its slot
or draft. It cannot choose a default — every slot
already contains a complete represented type, and an unprocessed relation
is an invariant failure, not permission to keep an earlier approximation.

---

## 11. Specialization

### 11.1 Identity

Three deliberately separate identities:

1. **`LogicalSpecIdentity`** — fixed at reservation: artifact-qualified
   callable/scheme identity, dense **logical** binding (the `logical`
   halves of §7.3's `BoundType`s), method scope, checked evidence
   identity.
2. **`ProvisionalSpecId` / `RepresentationInterfaceId`** — process-local
   occurrence identities for one unpublished request and its explicit
   argument/result slots. They permit discovery and recursive references
   while representation inputs are open; they are neither reuse nor cache
   keys.
3. **`FinalSpecKey`** — the logical identity plus the canonical digests of
   the declared **representation inputs** that can affect body or ABI. It
   exists only when those inputs seal. Body-produced output facts are not
   retroactive key inputs (§11.5).

`RepresentationInterface` preserves provenance instead of trying to
recover it from a final joined slot:

```text
input projection
  facts supplied by the request context / caller / declared generated rule

effective slots
  input projection joined with facts produced while discovering the body

output summary
  the body-produced facts and relations a caller or cache hit must receive
```

Every interface relation records its producer/consumer roles even when the
underlying join policy is commutative. Joining effective slots never erases
that provenance. `FinalSpecKey` digests only the sealed input projection;
represented emission reads the effective slots; the cache value stores the
final effective interface and output summary. Thus a body output that flows
around a recursive component cannot accidentally become a new key input,
while an actual caller constraint on a result position remains an input.

Openness is not limited to recursive calls. A non-recursive call argument
can contain a slot that gains information later while its caller's body
draft is being discovered. The lifecycle therefore operates over explicit
**representation dependency components**, not source-call or procedure
recursion SCCs.

### 11.2 Discovery, closure, and sealing

Specialization is one pre-publication, stage-local computation with three
phases:

1. **Discover representation-neutral drafts.** Starting from explicit
   roots, reserve a provisional record and representation interface before
   inspecting its body. Walk the checked body once to build a neutral draft:
   expression structure, checked evidence, calls through provisional record
   handles, and explicit representation-rule sites are recorded, but no
   iterator/ABI-sensitive Monotype choice is emitted. Every sensitive site
   records a total `RepresentationEmissionPlan`: for each possible declared
   descriptor outcome it names the exact generated edges, binder mappings,
   and input/output dependencies that outcome would activate. Emission may
   select one recorded alternative later; it may not discover an unrecorded
   dependency after closure. Discovering a call fixes and reserves its
   logical identity immediately — checked substitution made that identity
   final; any provisional reuse obeys §11.3 — and adds the exact input/output
   representation dependencies with their provenance. Direct and mutual
   recursion terminate by citing the already-reserved handle.
2. **Close representation dependencies.** Nodes are provisional
   specializations, interface slots, and rule obligations; an edge says one
   node cannot seal until the other's representation output is known. The
   stage-local discovery/closure queue runs until it has found every
   reachable draft and relation. Dependency-ready components seal in
   condensation order; mutually dependent nodes seal together through
   §10's terminating algebra. Any request with an unsealed representation
   input remains provisional and participates in the relevant open
   component, recursive or not. A component is closed only when its
   discovery queue is empty and every outgoing dependency either targets a
   sealed component or is included in the component being solved.
3. **Finalize identity and emit.** Seal every interface and draft (§10.6),
   compute `FinalSpecKey`, resolve provisional call handles, assign final
   `FnId`s, then select each recorded emission-plan alternative and elaborate
   the neutral draft into representation-sensitive Monotype IR. A selected
   generated edge whose output can affect this component was already an
   explicit Phase-1 dependency; an edge with only sealed inputs may enqueue
   a later independent component, but the emitted call cites its finalized
   request contract. Only this phase publishes bodies or cache records. A
   body is never generated against a representation that can still move,
   and emission never reopens a sealed component.

This is not a post-demand repair list: no final key, final `FnId`, body, or
cache entry exists before closure, and nothing published is later patched.
It is the representation-shaped residue of today's broad
`DeferredTemplate` mechanism. The old mechanism and all logical-key
stabilization delete; the narrower scheduler can join only already-logically-
equal representation slots and cannot revise a checked substitution.

Lookup timing follows the same boundary. If a newly discovered request's
entire declared representation-input projection is already sealed, its
`FinalSpecKey` is known immediately and the registry may probe a ready local
record or warm cache before discovering the body. A hit replays the stored
output interface (§11.5) and adds those explicit relations to the caller; a
miss creates a draft component. If any declared input is open, persistent
lookup is forbidden until closure supplies the final key.

If two provisional records converge on one `FinalSpecKey`, finalization
chooses neither by discovery order. Their callable/logical identity,
evidence, finalized interface, and representation-neutral draft must be
exactly equivalent after canonical renumbering of draft-local ids and
provisional handles. Digest equality is only the probe; a structural witness
is authoritative. All draft call handles then resolve to one canonical
final record before any `FnId` or body is published; a non-equivalent
collision is an invariant failure. This pre-publication handle resolution
is not callable repointing.

The rewrite preserves existing specialization reuse semantics until a
dependency proof identifies exactly which representation inputs must split
specializations. A represented input that can change emitted body
structure belongs in `FinalSpecKey`; a body-produced result representation
is an output slot/cache value, not a retroactive key change. A result
position constrained by the caller contributes its caller-supplied
representation to the input projection; only facts first produced by the
body enter the output summary. The dense logical binding is available for
fast lookup, but `(scheme, binding)` does not replace the existing key until the
migration proves that equal
bindings + evidence + declared representation inputs cannot differ in
generated body behavior. Incorrect reuse is worse than a cold miss.

### 11.3 Reservation and recursion

The registry's states become
`reserved → discovering → representation_ready → ready`. Reservation
uses `LogicalSpecIdentity` only to select a component-local candidate bucket
and allocates a distinct interface; logical equality alone never reuses an
open request. Reuse before sealing requires the same semantic request
handle (including a genuine recursive back-edge) or an already-proved exact
interface relation. It does not pretend an open represented request is a
final reuse key. Recursive references cite the in-flight provisional handle
and add their interface relations. A recursive request at a different
logical binding is a different specialization. As today, programs generating unbounded
polymorphic-recursive specialization sequences are outside the supported
monomorphization contract and fail through an explicit compiler limit.

### 11.4 No logical request refinement

In the target architecture, a reserved `LogicalSpecIdentity` never changes.
Draft discovery may join reserved representation-interface slots through
the declared algebra only; it may not refine the request's logical type or
mutate a published `MonoTypeId`. Resolving a provisional representation
identity to its final key is not logical refinement: the logical identity
was fixed at reservation and the final key adds only the now-sealed declared
representation-input projection. The current request-refinement and
solved-shape-alias machinery is deleted only after Slice 0/6 instrumentation
proves every historical difference is accounted for by corrected checked
publication, an explicit representation-interface relation, or a
now-rejected compiler bug.

### 11.5 Persistent-cache identity and output replay

Process-local ids never enter persistent keys. The serialized contract
distinguishes what is known before lowering from what the body produces:

```text
cache key
  = canonical callable/scheme and logical-binding identity
  + canonical checked-evidence and method-scope identity
  + canonical finalized representation-INPUT digests
  + Monotype configuration that affects generated shape

cache value
  = sealed body
  + complete finalized effective representation interface
  + provenance-preserving body output summary
  + relocatable references to any other records in its sealing component
```

Outputs never enter lookup identity merely because they are finalized in
the stored record: a body-produced result upgraded to minted is learned
from the cache value, not known to the lookup. On a hit, the loader performs
an exact structural witness check after the digest probe, creates fresh
caller-side representation occurrences unless the destination is already
proved closed, initializes them from the stored interface, and applies the
declared call-edge relations before the caller's component seals. A hit must
therefore reproduce every output fact body discovery would have produced;
it cannot merely skip the body and return an input-key type.

Serialized interfaces name occurrences by canonical paths through the
logical skeleton and by canonical generated-rule site ids — argument index,
result, field/tag/payload position, backing projection, and declared
generated edge — never by process-local slot or draft ids. Loading validates
every path against the logical skeleton before allocating fresh slots.

A mutually dependent sealing component is serialized atomically, or each
member carries a complete relocation table whose validation proves that no
cross-member representation edge is missing. Partial loading that loses an
output relation is forbidden. Records are serializable only after their
whole component seals. Any serialized-shape or keyed-semantics change bumps
`CACHE_VERSION` (`src/compile/cache_config.zig`; the comptime layout hash
catches structural drift, the manual bump documents intent). Cold/warm
tests compare bodies, final interfaces, and caller-visible replay effects; a
cold miss is acceptable during a version transition, a false hit or missing
output is not.

---

## 12. Lambda sets: keep the cor-lineage solver (do not rewrite it)

An earlier draft of this project proposed replacing `lambda_solved`'s
unifier with a directed set-dataflow pass. That proposal was wrong, and
wrong in a way that is easy to re-discover: Roc's lambda sets produced a
long run of severe miscompiles before the current architecture — adopted
from the cor research compiler's `lss` experiment (the `experiments/lss/`
tree in the cor repository) — fixed them. Anyone working near this stage
must understand what the architecture is and why each part is load-bearing,
so this section records it in full. The rule: **the Lambda Solved solver's
architecture does not change.** The only permitted changes are the cloning
boundary contract (§12.5), the hardening (§12.6), and mechanical seam
adaptation to §8.2's finalized alias input form and §10's pure shared
descriptor policy. None changes callable solving, merge semantics, or
`FnSpec` identity.

### 12.1 Why lambda solving is not re-unification

`CheckedModuleArtifact` contains no lambda sets. Checking types a function
as `a -> b`; it never computes which concrete lambdas inhabit that arrow.
`lambda_solved` derives that fact for the first time. Deleting the Monotype
re-solver is safe because a frozen source of truth exists to consume
instead; there is no frozen source of truth for lambda sets. The "never
unify" rule is scoped to value-type structure, and this stage is exempt by
design, not by grandfathering.

### 12.2 The cor `lss` architecture

cor's pipeline mirrors ours: canonicalize → solve → monotype →
monotype_lifted → lambdasolved → lambdamono → ir.

**lambdasolved** (`lss/lambdasolved/`): the lifted, fully monomorphic
program is re-typed into a fresh mutable store (`inst.ml`) in which every
function type gets a *third slot* — `TFn (arg, lambda_set, ret)` in
`type.ml` — instantiated as a fresh unknown while all value structure stays
ground. `solve.ml` then runs genuine Hindley–Milner inference whose only
real unknowns are those slots:

- **Let-polymorphism over lambda sets.** Definitions are processed in SCC
  order (`defs_graph.ml`), generalized after solving (`gen` marks
  unconstrained set variables `ForA`), and — critically — **every use of a
  definition instantiates fresh copies of its generalized set variables**
  (`inst` at each `Var`). Two call sites of one function never pollute each
  other's sets.
- **Union merge with capture agreement.** Unifying two lambda sets unions
  their members, keyed by lambda symbol. The same lambda arriving from two
  paths must have identical capture keys — "incompatible captures" is a
  hard failure — and capture types unify pointwise.
- **Sets live structurally inside types.** Because the slot is part of the
  function type, a function buried in a record field, tag payload, or list
  element gets its set propagated by ordinary structural traversal;
  higher-order flow through data needs no special machinery.
- **Erasure as absorption.** `LSet ~ Erased → Erased`; erasure requests
  propagate to every function type they reach (`erased.ml`).

**lambdamono** (`lss/lambdamono/`): demand-driven re-specialization *keyed
on the solved lambda-set types* — (function name, lowered argument type,
lowered return type, captures-spec: toplevel / set captures / erased
captures) in `specializations.ml`. Lowering defunctionalizes: a lambda set
becomes a tag union with one tag per member lambda (payload = its capture
record); a call becomes a `when` dispatch over those tags; an erased
callable becomes a packed function-pointer-plus-captures value invoked
indirectly.

The fixture `lss/test/generic-higher-order-call.roc` shows why per-use
polymorphism matters. `id = \x -> x` gets **one** monotype specialization
at `(Int -> Int) -> Int -> Int`, but lambdasolved gives it
`Int -<'1092>-> Int -[id1]-> Int -<'1092>-> Int` — the set `'1092` is a
*generalized variable*. One call site passes a capturing closure, another a
non-capturing one; because each use instantiates `'1092` fresh, the sets
stay separate, and lambdamono emits **two** specializations of `id` with
different layouts: `id3(x: [Clos {n: Int}])` and `id2(x: [Clos1])`. One
monotype, multiple final types, distinguished only by lambda sets.

### 12.3 Roc's port, and its one deliberate divergence

`src/postcheck/lambda_solved/` is a close port:

- `type.zig`'s `Content` mirrors cor's store: `link`/`unbound`/`forall`
  correspond to `Link`/`Unbd`/`ForA`; `func` carries the third slot
  (`callable: TypeVarId`); `lambda_set` is a span of
  `FnMember { lambda, captures }`; `erased` mirrors cor's `Erased`.
- Set unification unions members keyed by lambda symbol
  (`mergeLambdaSets`), with hard invariants on capture count and identity
  (`unifyCaptures`). Erased absorbs sets in both directions (and
  erased × erased checks source-digest agreement).
- Erasure is applied by *unifying* a minted erased node into callable slots
  reachable as data (`markErasedCallablesReachedByType`), with a deliberate
  exemption for iterator-backing step closures; still-unbound slots —
  never called, never stored — seal to the empty set (`closeCallableSlot`).
- Recursion: every lifted function's type is registered before any body is
  solved; the `active_unifications` in-flight-pair guard and
  insert-before-recurse cloning handle cyclic structures.

Downstream, `solved_lir_lower.zig` plays lambdamono's role: procedures are
keyed by `FnSpec = (source FnId, rooted solved function type var, capture
ABI finite/erased, capture type)`; each set member becomes an `FnVariant`
of a generated callable tag union; the Lambda Mono type store has **no
function type at all** — finite function values *are* their callable tag
unions, erased ones use the erased callable layout.
(`src/postcheck/lambda_mono/` is the Debug-only differential oracle, not
the production consumer.)

The one deliberate divergence from cor: **roc does not generalize or
instantiate per use.** `Content.forall` is never constructed — it exists
only as an invariant trap ("generalized Lambda Solved type reached local
unification without instantiation"). Each lifted function gets exactly one
type (`fn_tys`, built in a first pass), and every use site unifies against
that same variable (`fn_ref`, `call_proc`). Where cor's `id` yields two
specializations, roc pools both closures into one merged set and one
procedure: coarser but self-consistent, because every connected position
shares one equivalence class and one layout. Do not "fix" this in either
direction as part of this project; lambda-set polymorphism is a separate
design project if ever wanted.

### 12.4 Why the rejected alternatives break it — and the real decision inventory

Invariants, each the negation of a plausible "simplification":

1. **Lambda solving must build types, not just read them.** Its entire
   output is enriched types; a side table of sets-per-position cannot
   express "list of functions whose element set is {F, G}", and two values
   with the same interned `MonoTypeId` routinely need different enriched
   types (the cor `id` fixture). Identical ids are the ambiguous case, not
   the solved one.
2. **Set agreement is equality closure, not directed subset flow.** A set
   determines the tag-union layout of its closures; producer and consumer
   of one runtime value must agree on the *same* set. One-way ⊆ propagation
   permits two layouts for one value; making it sound would require
   re-tagging coercions on every edge. Equality closure over merging slots
   is union-find — i.e., unification.
3. **Erasure infects in both directions.** Consumers erase producers'
   construction sites; bidirectional merge expresses this trivially,
   monotone forward dataflow does not.
4. **Downstream identity depends on merged roots.** `FnSpec` deduplicates
   on the rooted solved var — the equivalence class is literally the
   specialization identity.
5. **The structural walk makes real decisions beyond the callable slots.**
   The verified census in `lambda_solved/solve.zig` — load-bearing for
   §12.6, maintained next to the solver and in `design.md`, and pinned by
   line count in `ci/check_reunify_manifest.pl`'s `lambda-solved-census`
   category, so a new special relation must be classified in both before it
   can land. A Debug assertion may claim "all other structures are equal"
   only after this inventory is complete and tested. The current inventory:
   - backed-alias unwrapping (`transparentAliasBacking`, 4 sites). It still
     has no `builtin_owner` exception — every `named.kind == .alias`
     unwraps — and now counts the retained case it walks through
     (`lambda_alias_unwrap_builtin_owned`);
   - the generated-private evidence relation
     (`unifyGeneratedOpaqueBacking` on the pattern side and
     `relateGeneratedPrivateEvidence` on the expression side), **[main]**
     decided by the producer-authored `BackingAuthority` rather than by
     score, with the loser linked in;
   - four iterator nominal-identity joins (`unifyForcedDynamicIterator`,
     `unifyIteratorOwnerStampedPublic`, `unifyGeneratedIteratorJoin`,
     `unifyPublicGeneratedIterator`) under the shared iterator relation,
     which Lambda Solved still reads directly as `Type.iteratorRelation`;
   - erased-callable dominance and member accumulation
     (`markErasedCallablesReachedByType`, `closeCallableSlot`);
   - named backing authority and recursive backing traversal
     (`structuralBackingForNamed`);
   - `mergeLambdaSets`, `unifyCaptures`, `active_unifications`, and the
     `forall` invariant trap.

   **[main] Three items this list used to carry are deleted**, not
   reclassified: the named empty-tag-union tie-break (`isEmptyTagUnion`) —
   the behavior survives only inline in the same-constructor tag-union arm,
   and `seamResidualShapesAgree` restricts seam shape disagreement to
   `erased`↔`lambda_set`; the iterator-backing exemption
   (`in_iter_backing`); and forced-dynamic backing collection during
   cloning (`forced_dynamic_backings`). The score-selection census
   (`generatedBackingScore`, `generatedOpaqueEvidenceScore`,
   `isScoreSelectedEvidenceOwner`, `unifyIteratorBackings`) is deleted with
   them.

   Several of the survivors are the LambdaSolved face of the §10 algebra.
   The asymmetry main introduced is closed: `Type.iteratorRelation` is now
   the adapter that carries a finished named type's recorded identity into
   the shared `iteratorTierRelation`, and `InstGraph.iteratorRelation` is
   the adapter that carries the graph's still-minting identities and its own
   component answer into the same function. Both stages classify from one
   implementation again (§10.3).

### 12.5 The cloning boundary: occurrence identity, before interning

How monotypes enter the lambda store today: each program position is
lowered through its own `lowerTypeFresh` call, which creates a fresh
`TypeCloner` whose memo — keyed on monotype `TypeId`,
insert-before-recurse — lives for that one call. Within one cloned type,
two occurrences of the same `TypeId` share one solved var, hence one
callable slot, hence one lambda set. Because the production store is not
hash-consed, structurally equal function types inside one cloned type
usually have distinct ids today and get distinct slots. **Interning changes
that silently**: `{ f : I64 -> I64, g : I64 -> I64 }` would share one slot
and merge sets with no value-flow edge — coarser-but-consistent (a
superset layout, not a miscompile), but a representation change smuggled
inside a "no behavior change" refactor, invisible to digest-stability
checks, leaving set granularity a side effect of allocation patterns
forever.

The required invariant:

> Monotype structural identity never implies Lambda Solved callable-flow
> identity. Every non-recursive function-type occurrence receives a fresh
> callable slot. Callable slots become equal only through a recursive
> back-reference or an explicit Lambda Solved value-flow relation.

Implementation shape: an active-recursion map instead of a completed-DAG
memo — reserve on first entry along the current path, reuse only for
back-edges to active nodes, clone fresh on later non-recursive
occurrences; callable-free immutable subgraphs may be shared as an
optimization only behind a `containsCallableOccurrence` proof. This lands
as its own slice with its own snapshot review (finer sets where today's
within-clone sharing was incidental are this slice's diffs, and only
this slice's), **before** production interning. It touches the cloning
boundary only — solver architecture, merge semantics, and `FnSpec`
identity are untouched.

### 12.6 Hardening: what is actually effective, and when it lands

- **Seam assertions (Debug-only), landed early.** Non-callable structural
  unifications descend from the same ground monotypes on both sides;
  assert they see structurally equal content, with the exemption list
  being exactly the §12.4 item-5 census (re-verified against the code as
  the first task). These exist to catch Monotype-side drift at the seam
  *while the Monotype migration is in flight* — so they land in Slice 1,
  before the rewrite.
- **Direct set tests, not harness mutations.** The differential runner
  compares two consumers of the same solved program, so it structurally
  cannot see a bug inside `lambda_solved`, and set-coarsening is usually
  behavior-preserving so output tests miss it too. Lambda-set coverage
  comes from direct tests: fixtures with pinned expected sets/captures per
  position; invariant checks (capture agreement across a class, erased
  reachability closed, callable slots sealed); unit tests over
  `mergeLambdaSets`/`unifyCaptures`/the erasure pass; and the §12.5
  occurrence tests (equal function-typed fields keep distinct singleton
  sets; explicit flow merges them; recursion ties only genuine
  back-references; minted backings preserve all step members). Seeded
  mutations inside `lambda_solved` are added only with these direct
  detectors as the thing that must catch them; the existing five
  body-lowering mutations stay pointed at `solved_lir_lower.zig`, the seam
  the harness actually guards.
- **Documentation.** The exemption, the §12.4 invariants, and the cloning
  granularity contract get stated in `lambda_solved/`'s module docs and in
  `design.md`, so they are discoverable at the code.

---

## 13. Migration plan

Ordering principles:

- `design.md` is amended before or with the architecture it authorizes,
  never in a final cleanup slice.
- There is always exactly one production lowering route. Temporary
  comparisons run as Debug verifiers whose result cannot select compiler
  behavior; authority changes hands exactly once, by deletion. No
  selectable old/new build option, no release-cycle alternate path.
- Assumptions are measured before they are built on; guardrails go up
  before the risky work.

The per-slice verification battery: snapshot suite
(`zig build run-snapshot-tool`, `TYPES`/`MONO` diffs reviewed to zero or
explained); postcheck module tests; multi-backend eval differential
(`zig build run-test-eval`, plus LLVM on a supported platform —
byte-identical output); Lambda-Mono differential runner and its mutation
check; the direct lambda-set tests once Slice 1 lands; specialization-cache
cold/warm tests when keyed data changes; `zig build minici` locally; full
CI (including the `check-once` job) before merge; `CACHE_VERSION` bump on
any serialized-shape change; performance judged on CI benchmarks only. No
snapshot update is accepted merely to make a new implementation green —
every semantic difference is classified against the ownership model.

**Slice 0 — Declare and measure the current semantics.**
Amend `design.md`: state the ownership decomposition (§1), mark the
instantiation-graph sections as scheduled for replacement, declare §11's
discovery/representation-closure/sealing lifecycle (including the narrow
representation-scheduling residue), and fix the stale
lambda-generalization claim (§12.3). Add a mechanically checked
inventory of every Monotype graph creation, logical unification call,
request refinement, deferred request, mutable refill, compiler-generated
instantiation edge, exact registry-lookup site, and Lambda Solved special
relation — checked into the repository as an exact manifest that fails CI on
unclassified additions and can only shrink or move into a declared
replacement category (enforcing from its first landing; no warning mode).
Instrument Debug runs across the snapshot/eval/fuzz corpora to classify:
request-digest changes before lowering; request-vs-solved digest
differences; row widening and symmetric row merges; expected-result-only
constraints; empty-tag-union yielding; nominal/backing relations; iterator
representation relations; generated-evidence backing decisions (including
current equal-score cases); backed aliases with `builtin_owner`; and every
legitimate repeated scheme-use record after resolution, reporting whether
duplicates are exactly equivalent. Separately record whether a
non-recursive call request's representation inputs change after first
discovery and which finalized representation/evidence inputs actually
affect generated body shape. Disposition every finding as **publication
gap** / **algebra rule** / **deletable bookkeeping**. Add the
checked-boundary `.err`-reachability assertion and record whether any
lowerable corpus case violates it. Audit head-multiplicity
canonicalization totality, nominal construction/destruction rule totality,
`TypeId`-keyed consumer classification (§8.5), and re-verify the §12.4
census. A non-equivalent re-check record is fixed in checking/publication;
it never becomes a first/last-write policy.

**Slice 1 — Make Lambda callable identity occurrence-based, and guard the
seam.**
Implement §12.5 (occurrence-based cloning; its own snapshot review) and
§12.6 (census-based seam assertions; direct set-invariant and expected-set
tests; mutation coverage against those tests). Record the §12 invariants
in the module docs. Only after this slice may structurally equal Monotype
function types be interned in production.

**Slice 2 — Publish real schemes and use-site substitutions
(checking-side).**
Capture pristine scheme snapshots at generalization boundaries; populate
binder ranges for every production scheme; add nested-scheme ownership,
entries, and captured-binder closures plus the imported-binder projection
(§7.1); give `CanonicalTypeSchemeKey` its stated binder/free semantics and
migrate semantic references off content-key lookup (§7.1); publish
`CheckedInstantiationSite` actual and evidence vectors projected from
`var_map` in binder order, under the coverage rule and savepoint
discipline (§7.2, with the named rolled-back-branch and re-checked-node
verifier tests, informed by Slice 0's equivalence measurement); classify
every residual variable (§7.4 phase one —
dispositions recorded as `contextual`/`uninhabited`, materialization
unchanged); land the full boundary verifier (§7.5) and the validation
matcher (§7.6); round-trip everything through serialization and cache
loading. Report the §7.2 cost checkpoint (artifact size, checking time,
peak memory). `CACHE_VERSION` bump. Monotype still lowers through its
current path; the new data is verified, not yet authoritative.

**Slice 3 — Isolate graph mutation, then promote the interner.**
First establish the hard boundary: mutable evidence lives exclusively in
graph-local cells; a graph result commits to Monotype only when sealed and
is immutable thereafter; delete the mutable-view/refill API while
retaining the logical graph (done — §6.6). Then put the existing interner
behind the production construction boundary, route every construction
through it (sealed commits, generated types, wrappers, recursive groups —
done: `Store`'s `intern*` constructors are that boundary, with dedup off),
implement
head canonicalization (§8.4), preserve all five equality relations
separately (§8.2), implement the declared split between stored alias
identity, logical alias projection, and dispatch-head ownership across
storage, digest, exact equality, validation, deserialization, and lookup,
and add the verifier that no storage-transparent alias is published. Assert
specialization/cache digest stability —
explaining and versioning every intentional difference. Audit
`TypeId`-keyed maps per §8.5. Safe for lambda sets because Slice 1 already
made callable identity occurrence-based.

**Slice 4 — Extract representation policy and build the closure engine.**
Introduce the rule/call-site inventory and the finite convergence contract
(§10). Extract iterator tiers, generated-evidence selection, and authorized
nominal-backing decisions into a pure descriptor-policy module. Production
Monotype's existing graph uses it through a thin graph adapter; Lambda
Solved invokes it through its own `TypeVarId` adapter. Neither shares slot
storage. Build and directly test Monotype's separate
`RepresentationSlotId` closure engine, but do not feed its results into the
production graph. Add accepted/rejected tests for every pair, randomized
algebra-order properties, and recursive termination fixtures; prove the
layer rejects logically unequal inputs and cannot perform
row/default/dispatch work. The graph still owns production logical
instantiation and temporary representation storage, but no longer owns
representation *policy*. This adapter boundary is not a pool-to-graph
bridge; it passes immutable descriptors and explicit decisions only.

**Slice 5 — Direct instantiation for closed checked data, as Debug
shadow.**
Implement eager `logicalIdentity`, `BindingEnvironment`, checked-node
translation, the draft layer, both §9.4 memo classes, fresh occurrence
instantiation, and `instantiateScheme` (§9), exercised over concrete
non-template roots and schemes with fully concrete published bindings —
**shadow-only**: in
Debug builds the direct result is computed and digest-compared against the
graph's, and production output never routes through it. Because nothing
new feeds production, no pool-to-graph bridge exists at any point in the
migration; the graph remains the sole authority for everything until
Slice 7's single flip. Mismatches on this closed subset are the cheapest
early warnings the migration gets. The shadow is **state-isolated** —
"cannot select output" is necessary but not sufficient, since a shadow
sharing the production interner or symbol allocator would perturb
allocation order in the authoritative path. It runs against an immutable
snapshot of its inputs (or strictly after authoritative output is
sealed), owns its own interner/draft/representation/specialization/cache
state, performs no writes to authoritative registries or name stores,
compares only deterministic digests, and is destroyable with no
observable change except Debug time and memory. The same isolation
contract governs the Slice 6 expansion.

**Slice 6 — Expand the shadow to complete specialization.**
Extend the Slice 5 shadow to the full §11 lifecycle: translate each site's
actual vector under the caller binding (captured projection included),
reserve provisional handles, discover representation-neutral drafts and
call dependencies, close representation dependency components, seal and
finalize keys, resolve exact-equivalent collisions, then emit represented
IR. Carry evidence vectors without registry rediscovery, and cover nested
definitions, result-only bindings, non-function values, recursive and
non-recursive open requests, imports, required/platform edges, and the
declared generated rules (§9.6). Exercise a shadow cache-hit path that
replays finalized outputs into fresh caller slots, including an atomic or
relocatable recursive component. Build and round-trip §11.5's new serialized
component format inside the isolated shadow, but do not register it with the
authoritative cache yet.

The shadow verifier compares final logical and represented digests with
the graph; the old path remains sole output authority until every
difference is classified and burned to zero. The shadow never gains a
known-divergence suppression list — that would be the drift this project
exists to end, wearing a new name.

**Slice 7 — Delete logical Monotype solving.**
With the manifest at zero for logical solving, authority changes hands
here — once. Delete `InstGraph` logical variables, row nodes, logical
`unify`, logical graph sealing, `importMono`, `DraftTemplateSpec` deferral
and its logical-key stabilization, `refineRequest`, and solved-shape
logical aliases (the refill API and `unsolved_monos` are already
gone — §6.6). Retain only §11's new
pre-publication representation dependency scheduler and provisional
handles; their API cannot carry logical graph nodes. Delete the shadow
verifier; make direct substitution plus representation closure the sole
path; activate only the new §11.5 cache reader/writer proven by the shadow
(no old-format cache record crosses the flip); turn the architecture gate
from manifest enforcement into simple permanent prohibition.
`CACHE_VERSION` bump.

**Slice 8 — Cache, performance, and documentation closure.**
Run the full cold/warm cache identity and caller-visible output-replay
matrix over §11.5's now-authoritative atomic/relocatable component records;
compare CI benchmarks and investigate regressions; write `src/postcheck/`
module docs and the final `design.md` sections describing substitution +
representation closure as *the* architecture (new documents where none
exist — avoid `plan.md` as a tracked filename per CI rules); verify the
permanent gates carry no migration allowlist. This file is then superseded
by `design.md`.

Slices 0–5 each deliver standalone value (measured meaning, guarded
lambda seam, recorded schemes, isolated mutation, interned store, named
and terminating representation policy/closure, shadow-proven ground
translation) and are individually revertible; the project pays for itself
even if paused before cutover.

### 13.1 Slice status after the `origin/main` merge (2026-07-28)

`origin/main` merged in 370 commits that independently did part of this
plan. Status of each slice on the merged tree:

| slice | status |
|---|---|
| 0 — declare and measure | **done**; the manifest, the architecture gate, and the census are in the tree, the census is re-sited onto the merged tree's constraint surface, and both corpora are re-measured (§13.3). |
| 1 — occurrence-based lambda cloning | **done**; the cloner keeps an active-path map behind a `containsCallableOccurrence` proof. |
| 2 — schemes and use-site substitutions | **done**; binders, nested owners, captured binders, positional actuals, and evidence ranges are all written and verified, and nothing lowers from them. |
| 3 — isolate graph mutation, promote the interner | **mostly done.** **[main]** deleted the refill API and closed imported rows; the `intern*` constructors are the production construction boundary with dedup switched off. Still open: head canonicalization (§8.4), the alias identity split across digest/equality/dispatch-head/verification (§8.2), entry-order-independent recursive identity (§8.3), and the digest-stability assertions. |
| 4 — representation policy and closure engine | **re-landed against the graph-aware inputs.** **[main]** had made Monotype's own classification graph-aware (`InstGraph.iteratorRelation`, `finalizeGeneratedIteratorRepresentations`) and replaced score selection with producer backing authority. The provenance the graph read is now a declared descriptor input — the minting identity plus the caller's component agreement (§10.3) — and the producer's depth and forced-dynamic pass is a declared engine input (§10.2), so `InstGraph.iteratorRelation` and `Type.iteratorRelation` are both adapters onto one `iteratorTierRelation`. Under `ROC_REUNIFY_SHADOW` the mirror is at 0 mismatches on both corpora and the `stepIterator` assertion no longer fires (§13.3). Backing selection is still by producer authority in production and by score only in the policy's unused readers. |
| 5 — direct instantiation as Debug shadow | **done, with new drift.** The shadow runs; its scheme comparison, which was at zero mismatches pre-merge, now reports 3 own-module and 13 imported mismatches. |
| 6 — shadow expanded to full specialization | **done, with new drift.** The rehearsal reaches 254888 compared positions at 254179 match; 48 logical mismatches remain, all newly introduced (§13.3). |
| 7 — delete logical solving | **open.** This is the whole remaining project; §13.2 states the shortest path. |
| 8 — cache, performance, documentation | **open.** |

### 13.2 Shortest remaining path to deleting logical solving

The precondition Slice 7 waits on is that no constraint-replay execution
carries logical information the checked data lacks. The pre-merge run
established that over 53 measured sites; the merged tree's constraint
surface is differently shaped, so that result did not carry over and the
census was re-sited onto it before the argument could be redone.

1. **Re-site the constraint census.** *Done.* `UnifySite` is re-declared
   against the merged tree's constraint surface: every one of `lower.zig`'s
   42 `.unify(` calls has an identity, named by the relater that carries
   the relation plus the relation it states. Main funnelled most relations
   through shared relaters, so `relateRequestComponent`,
   `relateFunctionRequestInterface` and `checkedMonoRequestNode` each carry
   several distinct relations and each gets one member per relation
   (`request_component_*`, `function_request_interface_*`,
   `checked_mono_request_*`); the rest are stated as a bare `graph.unify`
   and keep a member named for the lowering step that states them. 43 of
   the 55 members are reached on eval, 36 on snapshots (§13.3). The hook
   counts are pinned in `ci/check_reunify_manifest.pl` alongside the
   `.unify(` counts, so hooks cannot silently disappear from a re-taken pin
   again. `solve.zig`'s 19 `.unify(` calls are not replay sites: 11 are unit
   tests and 8 are steps inside the unifier, reached only while executing a
   relation a `lower.zig` site already named.
2. **Close the remaining binder-naming gap.** 2754 informative executions
   on snapshots out of 892475 with a directed answer on both sides. 1077
   are §10 representation content and 4 are unclassified, so the logical
   part is 1485 `scheme_binder_unbound` plus 188 `unbound_residual`.
   §13.3 carries the full attribution; the short form is that nine
   readings of the 1485 were tested and eight were refuted by their own
   measurement — the targets' kind, the recorded edge identity, missing
   sites, the fan-out of the unresolved bindings, captured binders, the
   operand's callee attribution, the third scheme's owner kind, and an
   outer frame holding its value. What survives is that 1383 of these
   positions want a value the checked data already records under a key the
   operand cannot express, and that the 1383 are never recorded at the use
   expression being bound, so no key available at the position selects
   them. The rehearsal measures positions in isolation and these positions
   are not isolated; the measurement moves to the seam (step 2b) rather
   than the operand descriptor growing further.
2a. **The read surface, enumerated.** Coverage is only meaningful against a
   denominator, and the denominator is small. Every Monotype body lowering
   obtains from the graph leaves through exactly two exits:
   `graph.activeTypeViewForNode`, whose only production caller in
   `lower.zig` is the one line inside `activeTypeFromNode` (its other
   callers are `solve.zig`'s own internals and tests), and
   `GraphTypeFinals.sealNode` on the frozen-emission path. Every other
   helper — `resolvedTypeViewForNode`, `currentPhaseTypeForNode`,
   `activeTypeFromCell`, `typeForChecked`, `resolvedCheckedTypeView` —
   delegates into one of those two. So complete coverage does not mean
   routing dozens of call sites; it means measuring two functions.

   What blocks measuring them is not their number but their argument: they
   take a node, and a node does not name the checked position it stands
   for. `instNode` already records that provenance
   (`trace.noteProvenance`), but only for a node created in the
   specialization's own root context with no nested declaration scope
   open, because a nested scope binds the same checked id under a
   different binding. Extending that provenance to every node created from
   a checked position, and recording the binding context with it, is what
   turns the two chokepoints into total coverage. The immediate
   measurement is the denominator itself: count calls at both exits and
   compare against the 1568 currently measured.

2a-i. **What the two exits actually read.** Counting both exits gives
   75113 live reads and 553490 top-level seal entries — 628603, with the
   roughly 539000 further `sealNode` calls excluded as sealing recursing
   into children rather than a read a caller asked for. Against that, the
   1568 measured reads are **0.25%**, not the multiple the earlier framing
   implied.

   Asking which of those reads can name a checked position took three
   attempts, each of which would have supported a different conclusion.
   Marking only the placeholder `instNode` memoizes reported 38% nameable;
   also marking the content it unifies with reported 61%; asking whether
   ANY member of the node's union class carries the mark reports **593171
   of 628603, 94.4%**, with **35432 derived**. Unification makes
   nameability a property of the class, not of the id an exit receives, so
   the first two readings were artifacts of the instrument.

   The 35432 are not what a §9.6 carve-out would predict. They are
   composites — 13084 functions, 12493 named, 4416 tag unions, 3727
   tuples, 1712 records — with **zero** unresolved variables, primitives,
   lists, boxes or redirects. Nothing generated-looking dominates them.
   The shape that fits is structural composition, and the measurement
   confirms it: asking whether a derived read's whole structure bottoms out
   in nodes that name checked positions — terminating at primitives, empty
   rows and erased leaves — **all 35432 ground out, with none ungrounded**.
   No logical structure enters postcheck without checked backing, so these
   composites owe no `GeneratedInstantiationRule`; they are correct once
   their parts are, and the coverage identity is properly stated over reads
   that name a position with composites discharged by construction.

   That answer took three attempts and the first two pointed the opposite
   way. One level deep, only 24% had all components nameable. A recursive
   walk with a depth cap of 24 reported 25% grounded and 75% exceeding the
   cap, because Roc's types are routinely self-referential and the walk had
   no cycle handling — the exact failure §15.10 forbids. Rewritten
   iteratively with an insert-before-recurse visited set, treating a
   revisit as co-inductively grounded and traversing list and box elements
   the recursive form had omitted, the figure is 100%.

2a-ii. **The seam, measured at the live exit.** Recording each node
   `instNode` creates with its checked position AND the binding context it
   was created under lets a read decide whether the binding it holds is the
   one the node was built under. Wiring that into `activeTypeFromNode`
   takes snapshot coverage from 1568 to **64801** reads, with 8339 declined
   for a differing context, against 75113 live reads.

   At that coverage `seam_direct_diverged` is **19**, having been 0 at
   every earlier level — the earlier zeroes were a 0.25% slice, not a
   property. All 19 are one shape and it is the shape the constraint census
   already named: directed translation emits the empty tag union where the
   graph carries a concrete type — `fn([],u8)->u64` against
   `fn(str,u8)->u64`, `List<([],[])>` against `List<(str,dec)>`,
   `Dict<[],[]>` against `Dict<str,dec>`. Every one is in `Builtin`'s
   Dict/HashMap machinery under `binding=frame` or `binding=none`.

   So the seam fails for exactly the reason the constraint sites do: a
   binder whose value the checked data does not name at that position. The
   two measurements are of one defect, and the seam is where it counts,
   because the seam is what the flip keeps. Clause 1 and clause 2 of the
   acceptance condition are therefore coupled — raising coverage exposes
   more of the same gap rather than a different one.

2a-iii. **Both corpora.** The acceptance condition is stated over
   snapshots and eval, and eval is the larger and more varied corpus, so
   it is measured too (7498 census writers, suite green). Coverage there is
   **257870 of 1063281 nameable reads, 24.3%**, against 10.9% on
   snapshots. Every one of eval's **56154** derived reads grounds out in
   checked-named structure, none ungrounded, which confirms on the larger
   corpus what snapshots showed: composites owe no declared rule.

   `seam_direct_diverged` is **857** on eval against 19 on snapshots. The
   ratio tracks coverage rather than corpus size alone, which is the
   expected behaviour if divergence is a property of the surface being
   measured rather than of a few programs — raising coverage exposes more
   of one gap. Both counts are of the same shape §13.3 records: directed
   translation emitting the empty tag union where the graph carries a
   concrete type.

2a-iv. **The seam, fully measured, and what remains.** Wiring the seal
   exit alongside the live one takes coverage of the nameable reads from
   0.25% to **581471 of 593171 on snapshots and 1050147 of 1063281 on
   eval**, with `seam_direct_absent` 0 on both. Divergence rises with
   coverage as it has at every step — 19 to 13268 and 721 to 14631 — and
   it is one shape: on snapshots every one of the 13074 seal divergences
   is directed translation emitting the empty tag union where the graph
   carries content, with no head, row or identity disagreement at all.

   **The graph is not the wrong side.** For eval's 540 divergences where
   neither side is the empty tag union, asking whether the sealed type
   still agrees with the head checking recorded gives 218 agreeing, 322
   inconclusive and **0 contradicting**, once the lowerings §10 owns are
   conceded — an enum-like tag union to an integer, a one-field record or
   tuple unwrapped, a zero-sized value erased, a nominal taking its shape
   from its backing. A first pass that did not concede them reported 140
   contradictions; all were the check's own coarseness. So every
   divergence measured anywhere is directed translation lacking a value,
   never the graph deriving a wrong one.

   **Most of what remains has no recorded value to find.** 13154 of the
   13268 entered through no request edge, so the edge-sourced level cannot
   reach them, and splitting those by whether the checked data names them
   by any route: 5702 have a definition whose instantiation is recorded
   elsewhere in the module, so the value exists and only the selecting key
   is missing; **6426 have a free variable no scheme generalizes at all**;
   863 have an owning definition the module records no site for; 163 reach
   no free variable. Eval agrees in shape — 4215, 5783, 994 and 2692.

   So roughly 7300 of 13154 on snapshots, and 6800 of 13684 on eval, want
   a value nothing in the checked artifact records. That is not plumbing.
   Closing them requires checking to publish what it does not publish
   today, which is §15.2's contingency rather than a defect in this
   instrument or in the graph.

2a-v. **The residual, accounted for exhaustively.** Every one of the 12943
   divergences on snapshots falls in exactly one class, summing with
   nothing left over: **6540** whose free variable NO scheme generalizes,
   **5377** whose owning definition the module records a site for,
   **863** whose owning definition it records no site for anywhere, and
   **163** that reach no free variable at all.

   Only the 5377 looked like plumbing, and they are not. The site lookup
   refuses them at selection: 5628 of them name a definition the module
   instantiates SEVERAL times, and nothing at the position says which
   instantiation applies. Accepting the ambiguity where every candidate
   binds the same actual vector — so no key is needed — builds 480 more
   levels and fixes **zero** further divergences, so even a site that is
   unambiguous by construction does not state what these positions want.
   Building the level from the one site a module records, where it records
   exactly one, closes 325 on snapshots and 519 on eval and no more.

   So each class wants something the checked artifact does not record: a
   value for a variable no scheme generalizes, an instantiation never
   recorded, or a statement of WHICH recorded instantiation belongs to a
   position. None is an instrument gap, and none is the graph deriving a
   wrong type — where both sides carry content the sealed type never
   contradicts the head checking recorded. Closing them is §15.2's
   contingency: a finalization change that publishes more, budgeted for
   its artifact growth and digest churn, not a repair to this stage.

2b. **Re-measure the logical residual at the production seam.**
   `BodyContext.typeForChecked` compares directed translation against the
   graph at every read, holds the body-walk context the isolated operands
   lack, and is the seam the flip actually depends on — what the flip
   deletes is the constraint sites, what it keeps is this. It already
   reports `seam_direct_absent` 0 and `seam_direct_diverged` 0 on
   snapshots and 8 on eval. Its limit is coverage: 629 reads on snapshots
   against the constraint census's 116118 comparisons, because only 13
   call sites route through it and the rest of body lowering reads types
   from nodes rather than from checked ids. Widening it is the work, and
   it is known to be delicate — an earlier repointing produced 27 eval
   crashes — so each newly routed site is landed and verified on its own.
   Widening does not require rerouting a read, only knowing which checked
   position it answers for. The comparison is extracted into
   `measureSeamRead`, which returns nothing and feeds nothing back, so any
   read whose checked position is already in scope can be measured with no
   behavioural change at all. `lowerExprType` is the case in point: every
   branch answers for the same `expr.ty`, and its call-result,
   dispatch-result and lookup branches previously returned a Monotype the
   seam never saw. Measuring it took snapshot coverage from 629 to **1248
   reads with `seam_direct_diverged` and `seam_direct_absent` both still
   zero** — 619 production reads newly checked, all agreeing. The reads
   that genuinely resolve a checked type are separately exhausted
   (`resolvedCheckedTypeView`'s two call sites are const-restore paths the
   corpus does not reach), but that is a much smaller limit than it first
   appeared. What remains costly is only the reads whose checked position
   is NOT in scope, which is where naming one changes what flows through
   lowering.
   The precondition is properly a statement about this seam: that every
   position production lowering reads, directed translation already
   computes. Stating it over constraint executions measures the machinery
   being removed instead.
3. **Re-land Slice 4 against graph-aware inputs.** *Done.* The closure
   engine takes the provenance `InstGraph.iteratorRelation` read, as the
   minting identity and component-agreement inputs of §10.3, and the graph
   calls the shared policy instead of holding a second copy of the rule.
4. **Give the direct-translate probe a re-rooting class.** *Done.* The
   probe compares unfolded digests when stored digests differ, which is the
   same notion the rehearsal counts as
   `rehearsal_type_equal_under_rerooting`. All 585 of its former stored-form
   mismatches are §8.3 re-rooting: `direct_stored_equal_under_rerooting` is
   585 and `direct_stored_mismatch`, `direct_stored_mismatch_logical`,
   `direct_stored_mismatch_representation` and
   `direct_stored_skip_context_variant` are all zero on both corpora.
5. **Then flip.** Delete `InstGraph` logical variables and row nodes,
   `unify`/`unifyRoots`/`unifyConcrete`/`unifyThroughBacking`/
   `unifyTagRows`/`unifyRecordRows`/`unifyRowWithEmpty`/`writeOrQueue*`,
   logical graph sealing, `DraftTemplateSpec` deferral, `refineRequest`
   and the solved-shape logical aliases, and `importMono`.

Step 2 is the only one that can still change the answer; step 3 is the
remaining measurement debt.

### 13.2b Verdict of the seam investigation

The precondition Slice 7 waits on has been measured to completion, and the
answer is that it cannot be met from what checking publishes today.

**What was established.** Coverage of the reads production lowering makes
is 98.1% on snapshots and 98.8% on eval with no read left absent, up from
0.25% when the two graph exits were first enumerated. At that coverage the
comparison reports 12943 and 14112 divergences, every one of which falls
in exactly one class with nothing unexplained: a free variable no scheme
generalizes, an owning definition the module records several
instantiations of with nothing saying which applies, an owning definition
it records none of, or no free variable at all. `rehearsal_type_mismatch_logical`
decomposes the same way, 57 of 57 and 952 of 1040.

**What was ruled out.** Not the instrument: coverage is near total and
absence is zero. Not the graph: where both sides carry content the sealed
type never contradicts the head checking recorded, 0 contradictions
against 251 agreements once §10's lowerings are conceded. Not a missing
key: the one class holding a recorded site is refused at selection, and
accepting the ambiguity where every candidate binds identical actuals
builds 480 further levels and closes none.

**What follows — superseded by §13.2c.** The reading above, that every
class wants something the checked artifact does not contain and that the
flip is therefore blocked on a checking-side change under §15.2, does not
survive re-measurement. It was drawn from a comparison that asks for each
position standalone. §13.2c states what the same divergences say when the
question is asked the way production asks it.

### 13.2c What the divergences are, asked at the granularity production uses

The comparison resolves one checked position at a time, keyed by id. A
position carrying a free variable, entered with no frame that binds it,
can only come back unbound — so it diverges by construction, whatever the
artifact contains. That is what the counts were measuring.

**The shape is unanimous.** On snapshots every one of the 12943
divergences has the same shape: `seal_diverged_direct_unbound` = 12803,
the directed side unbound where the graph has structure, plus 140 measured
away from the seal exit and classified the same way. Zero divergences have
the graph unbound, and zero have both sides carrying content. Zero are at
a ground position.

**Where the binding comes from.** The question has to be asked in a form
that does not depend on which frame the graph happened to be in when it
sealed. The framing-independent form is whether the position lies *inside
the very root whose binder the walk binds* — a nominal declaration's
backing, or the root of the scheme that generalizes the variable. If it
does, every walk that enters that root supplies the position. Asked that
way the classes account for all of them:

| class | snapshots | eval |
|---|---|---|
| nominal formal, position inside the backing that binds it | 6536 | 5725 |
| scheme binder, position inside its own scheme's root | 6213 | 6656 |
| scheme binder, reached from the frame's scheme root | 32 | 267 |
| scheme binder, no entering read found | 0 | 23 |
| free variable no scheme lists | 4 | 40 |
| ground (waits on no binding) | 0 | 40 |
| **seal-exit total** | **12803** | **13460** |

The first class is supplied by `direct_translate.Walk.nominalBacking`,
which pushes a `BindingEnvironment` binding a declaration's `formal_args`
to the instance's `args` before it walks the backing. *Every* nominal
formal on both corpora lies inside the backing that binds it. The second
class is the same statement for schemes, which take their frame at an
instantiation site.

This also explains the earlier "no scheme generalizes it" reading: a
declaration's formal and a scheme's binder for the same parameter are
distinct `CheckedTypeId`s, related by the walk rather than by a table, so
scanning binder lists for the declaration's id finds nothing whether or
not the artifact is complete.

Asking instead whether some *other recorded read* reaches the position
understates coverage — it left 1583 + 112 unexplained on snapshots, all of
which the framing-independent question closes. `SealTrace.contexted`
records probed seal exits, not every position a walk traverses, so "no
entering read" only ever meant none was *recorded*.

**The same decomposition at the program level.** `compareSpecialization`
resolves positions the same way, and its logical mismatches split the same
way: 50 of 57 on snapshots and 936 of 952 unbound residuals on eval lie
inside the root of the scheme that generalizes them.

**What is left, named rather than counted.** 7 positions on snapshots and
16 on eval, all in the `Builtin` module, all on polymorphic function type
roots: `List(a)` with a rigid `a`, inside a record or a tuple. Their heads
agree with what the graph sealed — record to record, `List` to list, tuple
to tuple — and the difference is at the element. Nothing names that rigid:
not a local scheme's generalized list, not an imported scheme's projected
binders, not a nominal declaration's formals (through either its backing
or its declaration root), not a captured binder, and not a residual
disposition. The graph gives the element a value unification supplied; the
directed side gives the residual the rigid implies.

This is the only class that would need checking to record something, and
what it needs is coverage of an existing recorded, round-tripped mechanism
— a scheme carrying these roots' generalized vars, or a §7.4 module-body
disposition for the rigids — not a new structure. At 7 and 16 entries its
storage is immaterial.

**Consequence.** No checking-side change is required for the other classes,
so no `CACHE_VERSION` bump and no artifact growth on their account, and
§15.2's contingency is not reached by this evidence. Directed translation
has exactly one non-rehearsal caller today (`lower.zig` `translateGroundRoot`)
and it is the shadow probe itself; which roots production reads is decided
by the flip, per §9.2 — specialization roots entered under the site's
actuals, with interior positions reached compositionally and never as
entry points.

**The graph never contradicts checking, on either corpus.** The eval
corpus is not unanimous the way snapshots are: of its 14112 divergences,
529 carry content on both sides and 64 sit at ground positions. Those are
the only shapes that could be a disagreement rather than a missing frame,
so each is judged against the head checking recorded, with nominals judged
on identity rather than declined as inconclusive.

Judged on `CheckedTypeId` alone that leaves 30 apparent contradictions,
coinciding exactly with the 30 the digest walk flags as a named-identity
difference. They are not contradictions: two checked positions may denote
one nominal, and comparing the declared name
(`cursor.source_names.typeNameText`) resolves all 30 to the same nominal
reached by another position. With that comparison
`sealed_contradicts_checked_head` is **0 on both corpora** against 341
agreements, the rest being §10 lowering a nominal's backing (158) or a
position that is itself a variable (158).

So §15.1b's worry — that the census cannot say which side of a
disagreement is right — does not have to be resolved, because no
disagreement survives measurement.

### 13.3 Corpus measurement (2026-07-28, merged tree)

Run as `ROC_REUNIFY_SHADOW=1 ROC_REUNIFY_CENSUS=<file>` over the snapshot
corpus (`zig build run-snapshot-tool`, 3139 lowering runs in one process)
and the eval corpus (`eval-test-runner`, 1739 measured test processes;
`TMPDIR` must name a writable directory or 23 `dev_object_*` snapshots
fail silently). Each census counter is a per-process running total, so the
corpus total is the sum over processes of each process's last block; the
block now opens with `census_writer <pid>` so that sum is exact under the
eval runner's fork pool.

**Constraint replay.** The census is re-sited onto the merged tree's
constraint surface (§13.2 step 1): 55 declared `UnifySite` members over
every `.unify(` call `lower.zig` still states, 36 of them reached on
snapshots. Re-measured on the merged tree after §10 emission and the
deferred-scope hold landed; the figures below supersede the pre-emission
ones, which were taken when a callee root whose representation content the
checked data could not dictate did not translate at all.

| corpus | redundant | informative | representation_decision | unmeasurable | construction |
|---|---|---|---|---|---|
| snapshots | 889406 | 2754 | 315 | 314573 | 1863392 |

Of the executions with a directed answer on both sides, 99.66% are
redundant. Emission is what moved this: a callee root that used to skip
now translates, so its bindings establish and the relations that read them
become measurable rather than informative.

The informative remainder is 2754 executions at four sites, and it splits
into two populations that different sections own:

- **1673 logical.** 1485 `scheme_binder_unbound` plus 188
  `unbound_residual`, concentrated at
  `function_request_interface_target_to_plan` (1462, of which 1450 are
  `scheme_binder_unbound`) and `constrain_checked_to_cell` (209). The
  checked side reaches a generalized binder no active binding names, so
  directed translation emits the empty tag union where the graph takes the
  value from the other operand. §13.2 step 2 owns these.
- **1077 representation.** All at
  `checked_mono_request_call_ret_to_expected`, all classified
  `representation` with origin `graph_sealed`: the relation moves iterator
  or generated-evidence content that §10's closure engine owns, not a
  logical value. A further 315 executions at the same site classify as
  `representation_decision` outright.

4 `unclassified` executions remain, all at
`function_request_interface_target_to_plan`. `head_tag`, `row_width` and
`named_identity` are zero — the head and width disagreements the
pre-emission census reported were themselves consequences of the
untranslated callee roots.

**Where the 1450 come from.** `methodTargetContext` marks a dispatch
target's context as a callee context, so its operands read under the
innermost callee binding. `openDispatchTargetBinding` opens that binding
only for a `.procedure` target whose template carries a scheme id; a
`.local_proc` target, a `.structural` one, and a procedure template with
no scheme return without opening one, and the target's own binders are
then read under no binding at all. These are the §9.6 declared-rule cases:
the edge is real and the callee scheme is known, but no published site
names it because checking chose the callee per specialization edge. This
is a naming gap in the rehearsal's rule inventory, not missing checked
data.

**Dispatch-target publication is not the gap it was.** Checking records
the `dispatch_target` sites: 11 local and 5431 through imported-scheme
projection on the snapshot corpus, with `site_without_snapshot_dispatch`
at zero — every dispatch target whose scheme a snapshot or projection owns
gets a site. On the postcheck side only 392 uses find no recorded site
(`rehearsal_no_site_use_unrecorded`) and 402 specializations skip for want
of one. The 4342 siteless edges the pre-merge measurement reported were an
artifact of that tree, not a standing `src/check` gap.

**The callee binding, and why it does not resolve.** 62786 dispatch
targets reach the binding step on the snapshot corpus and every one of
them is a procedure target carrying a scheme id, so the binding is always
opened; local, structural, and scheme-less targets do not occur. Of those
bindings 58230 resolve from a recorded site and 1063 from a declared rule.
52038 name a scheme with no binders at all, which cannot leave a binder
unbound. The 4202 that find no site are 2384 whose use and whose callee
scheme owner no site in the requesting module names, 1778 whose scheme
owner other sites do name but not at this use, 36 present on both halves
but unpaired, and 4 whose use carries only sites owning other schemes.

**Almost none of the absent sites matter.** A callee scheme that
generalizes nothing cannot leave a binder unbound, so an edge naming one
needs no binding at all. Splitting the failures on that line collapses
them: of the 4202 bindings that find no site, **4126 name a scheme with no
binders and 76 name a scheme with binders**; of the 3325 declared-rule
failures, **21** name a scheme with binders, and no binder-bearing edge
reaches the no-rule case at all. The whole missing-site population is 97
bindings that can affect a binder value. Publishing sites for the other
4000-odd edges would be recording data nothing reads.

**The recorded use identity is not what blocks even those.** Checking
records a static-dispatch edge's use identity as the constraint's
introducing expression, or node 0 when the constraint names none, and 4554
of 6014 edges name none. That looked like the cause and is not.
`constraintSourceExpr` already recovers a real source expression for a
literal conversion through its literal dispatch plan; routing the recorded
identity through it raises the edges carrying a source expression from
1460 to 4690 and changes **nothing** downstream — the snapshot corpus stays
byte-identical and the site-absent, rule-failure and informative counts
stay at exactly 4202, 3325 and 2754. The experiment was run and reverted.

The counts say why directly: 11 local dispatch sites are recorded across
the whole corpus against 6014 dispatch edges, the remaining coverage being
5431 imported-scheme projections. `recordDenseInstantiationSite` fires
only from the ordinary instantiation entry point and only under
`fresh_flex`, and `dispatchTargetMethodVar` instantiates only when the
method var is generalized, so a dispatch target reached any other way
records nothing. For a callee with no binders that is correct and costs
nothing. For the 97 that have binders it is the gap, and it is small
enough to characterize edge by edge rather than by a coverage sweep.

**What the 97 actually account for.** An unresolved callee level makes
`innermostCallee` decline, so the callee's own checked positions translate
under the requesting frame's environment, which names none of its binders.
Attributing every informative `scheme_binder_unbound` execution by the
state of the innermost callee level splits the 1485 three ways: **1015**
under an unresolved binding, **435 under one that resolved**, and **35**
with no callee binding open at all — the last matching
`constrain_checked_to_cell`'s own 35 exactly.

So the 97 bindings fan out to roughly ten executions each and account for
1015 of the 1485, not all of it. The 435 are a separate defect: the
binding resolved and stated the callee scheme's binders, and a position
still reached a generalized binder no environment names.
`residualClass` reports `scheme_binder_unbound` when the free variable is
a binder of ANY scheme the view carries, not only the bound one, so the
likely reading is a position reaching an enclosing scheme's binder rather
than the callee's — which is the section 7.3 lexical-parent case. That is
a distinct investigation from the 97, and neither subsumes the other.

**Every unbound binder belongs to a scheme other than the bound one.**
Classifying the unnamed binder by which scheme owns it, against the scheme
the operand was translated under, gives **1485 owned by another scheme and
0 owned by the translating scheme**. No binding that resolved ever failed
to state one of its own binders. The binding mechanism is correct where it
applies, and what the remaining executions want is a binder the bound
scheme does not contain.

`resolveCalleeBinding` declines a scheme with captured binders outright,
citing exactly that case, so linking the caller's chain as the callee's
lexical parents was the obvious reading. It is wrong. Lifting the refusal
and projecting the caller's environment onto the captured binders in both
binding paths — the same `bindCaptured` the specialization frame uses —
changes nothing at all: informative stays 2754, `scheme_binder_unbound`
stays 1485, and the 1015/435/35 attribution is unchanged to the unit. The
reason is that the refusal never fires. `rehearsal_callee_captures_linked`
is zero in all 3139 blocks: **no callee scheme reaching the binding has
captured binders**, so there is nothing to link. `rehearsal_captured_binder`
is zero as well, and even the specialization frames report
`rehearsal_env_parent_absent` 22210 times with no linked parent, so
captured binders are effectively absent from this corpus. The change was
run and reverted.

**Which scheme owns the unnamed binder.** The classification is
well-defined before it is read: exactly one scheme in the view generalizes
each unnamed binder, 1485 of 1485, and none is generalized by two, so the
owner the walk names is a property of the variable rather than its
iteration order. Splitting that owner three ways gives **0** owned by the
scheme the operand was translated under, **0** owned by the scheme the
requesting frame specializes, **1408** owned by a third scheme, and **77**
where no frame is open at all.

So the binder belongs to neither end of the call. It is not the callee's,
which rules out a binding that failed to state its own binders; it is not
the caller's, which rules out a caller-side position described as the
callee's at the `callee_checked` seam. What remains is a position whose
checked type is written in terms of a definition neither end of the call
owns, which no call-site binding between those two ends can ever name. Classifying that third scheme by the kind
of definition owning it answers what the position is: **all 1408 are
`top_level_def`**, ordinary generalized top-level values, with none
synthetic, none a platform requirement, and none an inner generalization
boundary. **761** of them generalize more than one binder.

So the position mentions ANOTHER generalized definition, whose binders
take their values from that definition's own instantiation edge and not
from the dispatch edge being bound. That is why every repair aimed at the
call itself failed: one callee binding names one scheme's binders, a
position may reference several schemes, and no amount of identifying,
recording, or ruling the dispatch edge can supply a third definition's
values. The 761 multi-binder owners make the same point from the other
side, since no receiver projection or single positional mapping could
state them.

A level per referenced scheme is the obvious answer and it does not
survive contact either: a level needs a source for its values, and
**1404 of the 1408** name a definition that is specializing nowhere in the
active frame stack, with only **4** found in an outer frame. There is no
binding to read those values from, so there is nothing to add a level from.

What that leaves is a statement about this instrument rather than about
checking. The census resolves an operand from a checked type id alone,
while directed translation in production resolves a position in a body,
where the referencing expression is known — and §7.2 keys a recorded site
by exactly that use expression. A position mentioning another generalized
definition needs that definition's instantiation AT THAT USE, which the
production walk can name and an operand descriptor carrying only a type id
structurally cannot. On that reading some part of the 1404 is the cost of
comparing types outside the expression context that identifies them, not a
fact the checked data lacks, and the constraint census overstates the
logical residual by that amount.

That reading makes a checkable claim, and the check supports it. Asking
whether a recorded instantiation site in the position's own module names
the third definition at all, **1383 of the 1408 do**, and 25 do not. The
value these positions want is already in the checked data. What the
operand lacks is the key that selects it: a `CheckedAddress` is a module
and a type id, and a site is keyed by the use expression, so nothing in the
descriptor can pick which of that definition's recorded instantiations
applies here. Directed translation in production is driven by the body
walk, which holds that expression.

This is the first of nine readings of the residual that its own test did
not refute, so it is stated with the limit it has earned: it establishes
that the value EXISTS, not that consuming it yields a match.

The cheaper repair it suggests is already ruled out. If one use expression
carried a site for both the third definition and the scheme the operand
translates under, a binding built per site at that use would state the
missing binder from data in hand. **None does: 1383 separate uses and 0
co-located.** The third definition is instantiated at a different
expression in the body than the one being bound, so no key available at
the position — not a use expression, not a scheme owner — selects its
instantiation. An operand carrying a use expression is not enough; what
resolves these is the body-walk context that knows which definitions are
live at that point.

That is a statement about how far this instrument can be taken. The
rehearsal was built to measure positions in isolation, and positions that
mention another definition are outside what isolation can answer. The
route that does not fight that is the production seam,
`BodyContext.typeForChecked`, which already walks bodies and holds the
context these positions need; the remaining logical residual should be
re-measured there rather than by further enriching the operand
descriptor. Whether the
site that applies to a given position is selected correctly, and whether
translating under it agrees with the graph, is the next measurement, and
the 25 with no recorded site remain a genuine coverage question. But the
conclusion the earlier text drew — that this residual is a gap in the
checked data — is not what the corpus says. Most of it is the cost of
comparing a type outside the expression context that identifies it, and it
belongs to this instrument rather than to checking.

**Why the receiver rule cannot be widened to cover them.** The
`constraint_dispatch_receiver` rule reads binder values positionally from
the receiver's own type arguments and accepts only a nominal, list, or box
receiver; every one of its 3325 failures is a receiver that emits to a
head taking no arguments. Widening the accepted shapes would not make the
rule total, because the rule also requires the receiver's argument count to
equal the callee's binder count, and a method like `List.map : List(a),
(a -> b) -> List(b)` has a binder the receiver does not determine at all.
A rule whose binder mapping is neither exact nor total does not satisfy
§9.6, and recovering the missing binders by matching the call against the
signature is what §9.5 forbids. The edge identity is the fix; the rule is
not.

**The unmeasurable population is one cause.** All 314573 are
`operand_undescribed`: `nodeUnifyOperand` describes a graph node only when
the graph imported an immutable type at it, and any other node is one the
graph built, which the directed pipeline has no expression for at that
call. They concentrate at `constrain_checked_to_cell` (157726),
`request_component_dispatch_arg_formal_to_evidence` (61806),
`request_component_call_arg_formal_to_evidence` (32600),
`checked_mono_request_call_ret_to_expected` (15241),
`checked_to_produced_value` (14680), `expr_expected_to_lowered` (13597)
and `evidence_target_root_to_request` (11288). Every other blocker is
zero on the snapshot corpus, including `operand_engine_input_needed`,
which §10 emission drove to zero from 6624.

Describing these requires a node's *position identity* — the checked
address the node stands for — and not the graph's own solved value for it,
which would make the comparison a tautology. `instNode` already records
that provenance, but only for a node created in the specialization's own
root context with no nested declaration scope open, because a nested
instantiation scope or a per-call context binds the same checked id under
a different binding that the rehearsal's environment does not describe.
Extending the description therefore means recording the binding context
alongside the address and giving the genuinely context-dependent remainder
its own blocker name, so the residual is stated rather than pooled with
the describable majority.

**The lowering seam.** `seam_direct` 629 reads on snapshots and 757 on
eval, `seam_direct_absent` 0 on both — every checked position the seam
reads is one the checked data describes. `seam_direct_diverged` is 0 on
snapshots and 8 on eval, all from one program and all one shape:
`direct=fn(List<[]>,[])->List<[]>` against `graph=fn(List<i64>,i64)->List<i64>`
under `binding=frame`. That is the same unbound-binder residual as the
informative constraint executions, so §13.2 step 2 closes both.

**The rehearsal.** 115453 specializations attempted and 105615 compared on
eval (26615 / 14189 on snapshots); the unresolved remainder is entirely
`skip_root_edge` and `skip_generated_edge` — every other skip class is
zero on both corpora. 254888 positions compared, 254179 match, 661 equal
under re-rooting, **48 logical mismatches** and 0 representation
mismatches. The 48 split as 30 `unbound_residual` (all
`other_scheme_binder_off_chain` / `binder_scheme_unrelated`, i.e. the
position's free variable belongs to a scheme with no checked relation to
the frame at all), 16 `named_identity` where the two nominal heads differ
only in `kind`, and 2 `row_width` (a 1-field record against a 2-field
one). Snapshots are at zero logical mismatches.

**The direct-translate probe.** 90151 roots on eval (28230 on snapshots),
71026 stored-form matches (8771), **585 equal under re-rooting on eval and
zero mismatches of any class on either corpus**. The probe now compares
unfolded digests when stored digests differ, which is the same notion the
rehearsal counts as `rehearsal_type_equal_under_rerooting`: §8.3's
recursive re-rooting is a deliberate difference in the emitted stored form,
so it no longer lands in the required-zero `mismatch_logical` bucket.
`direct_stored_mismatch`, `direct_stored_mismatch_logical`,
`direct_stored_mismatch_representation` and
`direct_stored_skip_context_variant` are all zero.

**Representation.** The measurement that produced 24 sealed `minted_join`
disagreements and three aborted eval programs was taken before Slice 4 was
re-landed (§13.1). Two separate defects produced it. The tier gap: for two
representations the graph was still minting, the descriptors carried no
generated identity at all, because the durable digest is stamped only at
sealing — 5 join sites classified differently, 3 of them on a
producer-minted component the engine did not model. The seal gap: all 24
disagreements were one shape, a node the producer finalized to the dynamic
fixed point after the join, which the mirror never mirrored, so the engine
still carried the pre-finalization minted descriptor. With the minting
identity, component agreement, modelled components, and the producer-input
adoption in place, eval reports 35612 matches and **0 mismatches** (29 of
29 `minted_join` sites agree, up from 5), snapshots 28984 matches and 0
mismatches, `representation_mirror_adopt_rejected` is 0 on both, and the
full eval corpus is 1787/0/0 with the shadow on as well as off. Production
classification is unchanged: `iter_minted_join` (29) and
`iter_public_minted` (14) are identical to the pre-change run, and the
snapshot corpus regenerates with no tracked diff. The Slice 5 shadow's
scheme comparison, at zero pre-merge, still reports 3 own-module and 13
imported mismatches.

**What is not comparable to the pre-merge run.** The pre-merge figures
(943644 redundant across 53 sites; snapshot seam 94383 reads / 240
divergent; eval seam 3561 divergent; 23769 rehearsal specializations,
108430 positions, 4718 equal-under-rerooting; 9289 probe matches) were
taken against a `lower.zig` with 53 measured constraint sites and a
different graph. The absolute totals moved because the measured surface
moved, so the honest reading is per-class: zero informative became
165225 / 136139, all but 82 / 467 of them the one `scheme_binder_unbound`
shape §13.2 step 2 closes, and zero rehearsal-logical became 48.

---

## 14. Enforcement

The permanent architectural invariant:

> Outside checking, no stage creates or solves a logical type variable.
> Monotype instantiates explicit checked substitutions into immutable
> types. The only postcheck equality closures are the rule-bearing
> representation relation and Lambda Solved callable-slot solving.

CI enforces this structurally, not only with a regex over function names:

- postcheck modules outside the two declared closures cannot import
  logical-solver types or modules (module-dependency and forbidden-type
  checks are authoritative; renaming a wrapper does not bypass them);
- Lambda Solved may use reservation/link cells to clone recursive shapes
  and solve callable slots, but every non-callable logical payload in
  those cells is copied from an immutable Monotype and must remain
  structurally equal (modulo the §12.4 census); only callable-slot content
  begins unknown;
- only Monotype's representation module constructs `RepresentationSlotId`
  or calls `relateRepresentations`, and every call cites a declared rule;
  Lambda Solved may call the shared pure descriptor policy but cannot import
  Monotype's slot store or closure engine;
- structural caches obey the occurrence-safety law (§9.3): only
  representation-free logical skeletons/recipes or fully sealed templates
  under finalized representation-input keys, never live draft or
  representation-slot occurrence identity; importing a sealed template
  into an open occurrence creates fresh slots;
- ordinary Monotype modules cannot call registry search APIs;
- direct mutable store construction is private to the interner;
- no storage-transparent alias is published by the interner; stored
  interning identity, logical alias projection, validation, and
  dispatch-head ownership obey §8.2's explicit compatible rules rather than
  accidentally sharing one helper;
- provisional specialization handles, drafts, and representation slots are
  confined to the active unpublished dependency component; final bodies,
  `FnId`s, and cache records contain none of them;
- every representation-sensitive emission site consumes a total
  `RepresentationEmissionPlan`; emission cannot create an undeclared
  generated edge or reopen a sealed dependency component;
- deleted graph/refill/`DeferredTemplate`/logical-request-refinement types
  stay forbidden; §11's representation scheduler has distinct types whose
  APIs cannot carry logical graph nodes
  (the existing `ci/check_postcheck_architecture.pl` pattern — "deleted
  APIs stay gone" — is the template, wired into `minici` and the
  `check-once` CI job);
- during migration, the Slice 0 manifest uses exact call-site/file
  ownership and fails on both additions and stale zero-count entries;
  after deletion there is no migration allowlist.

Stated honestly: a symbol gate is a ratchet and a tripwire, not a proof —
the real enforcement is Slice 7's deletion of the solver implementation,
after which absence is the strongest gate. The `lambda_solved` carve-out is
a permanent, commented exemption pointing at §12, never an allowlist entry
that looks like migration debt someone should finish deleting.

---

## 15. Risks and required answers

**15.1 Checked publication may be missing more than binders.** Expected
result constraints, row widening, nested ownership, and defaults may
expose additional unpublished logical facts. Each finding is fixed at
checking or publication — never reconstructed downstream. The Slice 0
census sizes this tail before Slice 6 meets it.

**15.1b The census cannot say which side of a disagreement is right.**
Every measurement in §13.3 compares the graph against directed
translation, and that comparison is symmetric: it reports that the two
differ, never which one the program's meaning agrees with. The reading
applied throughout — that a disagreement is directed translation lacking
something — is argued only for the empty-tag-union population, where one
side materializes the shape a position no value reached takes, which is
never a plausible correct type. It is NOT argued for a disagreement where
both sides carry real content, and the pre-emission census had those:
differing heads, differing row widths, differing nominal identity. Those
are at zero now, which was attributed to emission completing the directed
side; it is equally consistent with emission removing a case where the
graph was wrong. Since §6.9's premise is that postcheck re-derivation is a
bug factory, a disagreement being a live defect in the current compiler is
the project's own thesis rather than a remote possibility, and this
project has already found such defects — the `reintern` call that dropped
a named backing's authority is one.

A first asymmetric reading now exists. `instantiateTargetFromPlanNode`
holds the target's and the plan's checked function types alongside the
graph nodes built for each, which is where the informative executions
live, so it can ask the one shape no representation choice may alter:
does the argument count the graph built match the one checking recorded.
**125554 agree and none contradict** on the snapshot corpus. That is
evidence against the graph fabricating structurally wrong function types
at these sites — the likeliest form the bug hypothesis would take there —
and it supports the empty-tag-union reading with something other than the
shape of the placeholder. It settles nothing about a disagreement arity
cannot see: a wrong argument type with the right count, a wrong nominal
identity, a wrong row. Those need per-shape comparisons that respect the
representation latitude §10 owns, which is the rest of this risk.

The comparison that resolves direction is different from the one being
run: checking is the authority on logical types, so a postcheck-derived
type that contradicts the checked data for its position is wrong
regardless of what directed translation says. Measuring graph against
CHECKED DATA, rather than graph against directed, is what would separate
"the recorded substitution is incomplete" from "the solver being deleted
is miscompiling". Note that a full corpus passing does not settle it: eval
is 1787/0/0 on the current compiler, so any such defect is either outside
the corpus or not observable in its output.

**15.2 The groundness verdict could be worse than expected.** If callee-row
widening routinely carries information the frozen types lack, the fix is a
finalization change (fuller rows), which may grow the artifact and shift
digests — budget the `CACHE_VERSION` bump and snapshot churn in whichever
slice absorbs it. Under no measured outcome does the answer become "keep a
row solver."

**15.3 Site-vector publication may cost more than expected.** The §7.2
checkpoint measures artifact growth, checking-time, and checking-side
peak-memory cost on the corpus.
Expected modest (actuals are `gv_len` ids per generalized use; roots are
already published); if measurement disagrees, the design returns to review
— coverage is never silently thinned.

**15.4 Representation closure may be larger than the initial inventory.**
Instrumentation may find more postcheck-created relations. A new relation
is accepted only if genuinely unavailable during checking, with a declared
total algebra, finite-height domain, canonical join identity, termination
measure, and inability to change logical types; otherwise its data moves
upstream.

**15.5 Hash-consing may expose more occurrence-identity coupling.** Lambda
callable slots are the known case; ARC, layout, specialization, snapshot
code, or generated-symbol naming may also depend accidentally on Monotype
allocation identity. Slice 3 audits every `TypeId`-keyed map and
classifies its meaning (§8.5).

**15.6 Specialization identity may not reduce to scheme bindings.**
Representation and evidence can affect generated bodies even when logical
bindings match. The existing identity remains until the new architecture
proves the exact finalized representation-input/evidence projection
complete (§11.1). A logical-only represented-result memo is categorically
invalid (§9.4).

**15.7 The cutover is deliberately wholesale.** Because Slices 5–6 are
shadow-only, no pool-to-graph bridge ever exists and production behavior
is frozen until Slice 7's single flip — but that concentrates risk at the
flip: the new path gets no production soak beforehand. The mitigations are
the breadth requirement on the shadow (full snapshot/eval/fuzz corpora at
zero unexplained mismatches) and the fact that the flip is one reviewable
change that deletes rather than toggles. Shadow slices do not count as
delivering the end-state invariant.

**15.8 Differential tests can share a bad upstream result.** The Monotype
digest shadow and the Lambda-Mono runner are not independent proof of
their shared inputs. Boundary verification, direct representation tests,
direct lambda-set tests, and backend agreement remain necessary (§12.6).

**15.9 Lambda-set regressions by adjacency.** This project does not change
the solver, so the residual risk is indirect: Monotype-side changes
altering the ground types the cloner consumes, or granularity shifting
through the cloning boundary. Slice 1's normalization, seam assertions,
and direct set tests are the countermeasures — live before Monotype work
begins. The secondary risk — a future "cleanup" re-attempting the dataflow
rewrite — is countered by §12's invariants living in the module docs and
the gate carve-out being explicitly permanent.

**15.10 Deep and recursive types.** Every recursive walk has explicit
cycle handling (insert-before-recurse visited maps, iterative worklists);
no valid checked type is rejected because a helper chose an arbitrary
depth cap. Resource limits are explicit compiler limits, separate from
cycle detection.

**15.11 Representation dependency discovery may create larger components
than the call graph suggests.** Non-recursive calls can participate when an
input remains open or a callee output flows back to the caller. Slice 0
measures this shape; Slice 6 stress-tests long acyclic chains, mutually
dependent calls, final-key collisions, and explicit specialization limits.
The answer is never to publish early and repair later.

**15.12 Cached outputs can be semantically required even when absent from
the key.** A warm hit that fails to replay one body-produced representation
fact can diverge from a cold build. Cache fixtures therefore pin final
interfaces and caller-visible replay, and recursive components load only
atomically or through a validated complete relocation table (§11.5).

---

## 16. Acceptance criteria

The project is complete only when all of the following hold:

1. **Checked completeness:** every scheme has real binders and owners;
   every ordinary use has an exact substitution/evidence vector; the
   boundary verifier accepts the full corpus and rejects deliberately
   malformed artifacts.
2. **No logical postcheck inference:** no postcheck code creates, unifies,
   defaults, widens, or seals a logical type variable.
3. **Immutable canonical Monotypes:** all production construction goes
   through the interner; equivalent rooted types share an id within a run
   (per-node rooted registration built, §8.3); the pool's alias
   canonicalization is exact across stored form, digest, equality, and
   deserialization; logical projection/validation and dispatch-head lookup
   obey their separately declared alias rules; no storage-transparent alias
   is published; the five equality relations remain distinct and tested.
4. **Explicit representations:** every remaining Monotype equality closure
   is a declared, terminating representation rule over already-equal
   logical types; eager logical identities match the projection of every
   sealed result; logical and represented memos use their respective keys,
   and open occurrences never inherit another occurrence's slots.
5. **Correct lambda granularity:** structural interning cannot merge
   callable slots; only recursion and explicit value flow can. The §12
   invariants are stated in the module docs.
6. **Checked dispatch ownership:** ordinary dispatch consumes checked
   evidence; only declared compiler-generated edges perform exact
   component lookup.
7. **Stable specialization:** reserved logical identities never refine;
   representation changes flow only through the declared algebra and
   interface slots. All open requests use unpublished provisional handles;
   representation dependency components close before final keys, `FnId`s,
   or bodies publish, converged keys deduplicate by exact equivalence, and
   represented emission selects only predeclared plan alternatives.
8. **Deleted machinery:** logical `InstGraph`, refill views, row solving,
   `DeferredTemplate` and logical-key stabilization, `unsolved_monos`,
   request refinement, solved-shape aliases, and the shadow verifier are
   gone; only the restricted pre-publication representation scheduler
   remains, and no transitional pool-to-graph bridge ever existed.
9. **Cache correctness:** versioned cold and warm caches pass exact
   identity checks with no false hits; keys contain finalized inputs but not
   body-produced outputs, interface provenance survives closure, values
   carry complete effective interfaces plus output summaries, and warm hits
   replay caller-visible output relations exactly.
10. **Behavioral equivalence:** snapshots, eval tests, all four backends,
    iterator cases, generated evidence, dispatch, and callable tests show
    no unexplained differences; the shadow reached zero before cutover
    while provably state-isolated (own interner/registry/cache state, no
    writes to authoritative stores, digest-only comparison).
11. **Performance:** CI benchmarks show no unexplained material
    regression, and stage counters confirm the intended allocation/solver
    work disappeared.
12. **Permanent enforcement:** the gates encode the final ownership model
    with no migration allowlist and no selectable alternate path; the
    `lambda_solved` carve-out is permanent and documented.
13. **Authoritative documentation:** `design.md` matches the delivered
    architecture, including the actual Lambda Solved generalization model.

---

## 17. Glossary

- **Logical type** — the source-level checked type relation (primitives,
  functions, rows, aliases, nominals and their arguments), independent of
  postcheck-created representation tiers.
- **Scheme** — an artifact-qualified checked root plus an ordered list of
  generalized binders and evidence parameters, with an owner.
- **Use-site substitution / `CheckedInstantiationSite`** — the
  checker-published ordered actual type per binder at one instantiation
  edge, with its evidence vector and instantiated root.
- **Binding environment** — the `BoundType` values for an active
  specialization's binders, linked lexically for nested schemes.
- **`BoundType` / `LogicalTypeIdentity`** — a binder's value splits into
  a fixed logical half and a representation half. `LogicalTypeIdentity`
  is the interned id of the representation-free logical skeleton, computed
  eagerly from checked data and the logical environment before drafts seal
  (§8.2); it keys substitution, logical recipes, and
  `LogicalSpecIdentity`. The representation half is a `TypeRef` and may be
  unsealed until §10.6.
- **Instantiation** — directed replacement of a scheme's binders by an
  explicit binding vector. Not matching, not unification.
- **Validation matcher** — the Debug-only directed walk that verifies a
  published substitution against the complete instantiated root; never a
  production mechanism.
- **Interning equality / logical equality / representation compatibility /
  specialization equality / canonical cache identity** — the five distinct
  equality relations (§8.2); hash-consing accelerates the first and
  replaces none of the others.
- **Representation slot / relation** — a temporary postcheck occurrence
  with fixed eager logical identity and a complete or draft represented
  value, created and joined only by private constructors and declared
  representation rules. Sealing proves its final logical projection equals
  that identity (§10).
- **Representation policy** — the pure descriptor join/classification rules
  shared by Monotype and Lambda Solved. It owns neither Monotype's
  `RepresentationSlotId` closure nor Lambda Solved's `TypeVarId` solver.
- **Draft type / `TypeRef`** — a compound type under construction whose
  transitive children include a representation slot; it contains no
  logical unknowns and is interned bottom-up into an immutable
  `MonoTypeId` only after representation sealing (§9.1, §10.6).
- **Occurrence identity** — identity of a position in a value/type-flow
  graph; unlike structural `MonoTypeId`, it may own a distinct Monotype
  representation slot or Lambda Solved callable slot.
- **Logical instantiation recipe / sealed represented template** — the two
  memo classes in §9.4. The former is keyed only by logical bindings and
  contains no represented state; the latter is additionally keyed by
  finalized representation inputs and creates fresh slots when entering an
  open occurrence.
- **Representation dependency component** — the unpublished set of
  specialization drafts, interfaces, slots, and rule obligations that must
  be discovered and closed together before any member receives final ids or
  emits represented IR (§11.2). It may connect non-recursive calls.
- **Representation emission plan** — the neutral-draft record of every
  representation-sensitive emission alternative and its exact generated
  edges/input-output dependencies. Sealed descriptors select an already
  declared alternative; emission cannot discover a new dependency.
- **Provisional specialization handle / final specialization key** — a
  provisional handle names one open request occurrence; the final key is
  computed only after declared representation inputs seal. Logical identity
  is fixed throughout; resolving the former to the latter is not logical
  request refinement.
- **Lambda set** — the exact lifted function members that may flow through
  one callable occurrence, including capture types; first derived by
  LambdaSolved, stored in the function type's callable slot; determines
  closure layout, so all connected positions must agree on one set.
- **Callable slot** — the extra variable inside every LambdaSolved
  function type (`func.callable`); the only kind of variable that solver
  solves for.
- **Cloning boundary** — where ground monotypes enter the lambda store
  (`TypeCloner` under `lowerTypeFresh`); its sharing granularity is a
  deliberate occurrence-based contract (§12.5), independent of interning.
- **Defunctionalization** — compiling a lambda set as a tag union with one
  tag per member (payload = captures record) and calls as dispatch over
  those tags.
- **Checked evidence** — the checker-published direct / constrained /
  structural / unreachable / checked-error resolution for static dispatch.
- **Compiler-generated edge** — a postcheck-created call edge with no
  checked instantiation record, governed by a declared
  `GeneratedInstantiationRule` and exact component lookup.
- **Bottom/unreachable position** — a checked position no value can reach;
  explicit data, never an unresolved variable postcheck may
  opportunistically replace.
- **Shadow oracle** — a Debug-only computation asserted equal to the
  authoritative path's result, never able to select compiler output; the
  migration's only permitted form of coexistence.
- **Finalized representation interface** — the complete sealed input/output
  representation summary stored with a specialization. It retains separate
  input provenance, effective joined slots, and body-produced output
  provenance. Persistent-cache lookup keys use the declared inputs; cache
  values replay the outputs into fresh caller occurrences (§11.5).
- **Poison / `.err`** — the error content checking substitutes at type
  errors so diagnostics don't cascade; blocked from postcheck by the
  lowering contract (§5.4).
- **`TypeDigest` / canonical key** — deterministic content hash of a
  checked type; the cross-module, cache, and serialization identity.

---

## 18. Final invariant

The project succeeds when postcheck can explain every type-related action
in one sentence:

```text
This logical type came from an explicit checked substitution.
This representation came from a declared representation rule.
This callable set came from Lambda Solved value flow.
```

If an action does not fit exactly one of those statements, it is in the
wrong stage or is missing explicit input.
