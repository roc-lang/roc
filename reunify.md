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
against the code; hypotheses that remain open are marked as Slice 0
measurements. Implementers should treat this ledger as authoritative.

**Confirmed true:**

- The Monotype re-solver machinery (refill-in-place, deferral-until-stable,
  snapshot-on-import, conflict-on-over-demand) exists exactly as described
  (§6.6) and is a coordinated defense against one root cause: types move
  during lowering because lowering re-solves them. `monotype/solve.zig`'s
  own comments and a comptime `assertNoNodeId` test already pin the intended
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
- A hash-consing interner for Monotype types **already exists** as tested
  scaffold (`src/postcheck/monotype/type.zig`, `Interner`/`InternerState`,
  digest probe + collision-bucket exact equality + recursive-group support).
  Production `Store.add` bypasses it today. Two properties it does **not**
  yet have, stated here so they are built rather than assumed: its digest
  and equality paths unwrap **every backed alias**, with no
  `builtin_owner` exception, while `dispatchHeadContent` alone retains a
  builtin-owned alias as a dispatch head and checked canonical keys
  (`canonical_type_keys.zig`) preserve alias identity. Those are three
  different alias stories today; §8.2 chooses one target and Slice 3 changes
  storage, digest, equality, dispatch-head use, and verification together.
  Its recursive-group builder also registers only the selected group root
  in the interner bucket, so entry-order-independent identity for every
  cyclic node is a build task (§8.3), not an inherited property.
- The checker already computes the instantiation mapping this design
  publishes: `src/types/instantiate.zig`'s `Instantiator.var_map` holds the
  resolved source-variable → fresh-variable map for each instantiation, and
  a narrower persistence mechanism already exists (`ModuleEnv.
  SchemeUseRecord`, with slot kinds like `value_use` / `nested_function_use`
  / `shared_value_use`, whose fresh vars are resolved at publication).
  Today it records only constrained variables for selected use kinds — the
  infrastructure is partial, not absent.

**Failed verification — corrected in this document:**

- **"The scheme representation already exists."** Only structurally.
  `CheckedTypeScheme.gv_start`/`gv_len` (`src/check/checked_artifact.zig`)
  default to zero and no production publication site sets them; the only
  writer of nonzero ranges is a serialization test. `generalizedVars()`
  returns an empty slice for every real scheme, and nested let-generalized
  definitions get no scheme entry at all. Publishing binders is real
  checking-side work (§7.1, Slice 2).
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
  Unproven; today's code contains counter-mechanisms: the requested-vs-
  solved distinction in `monotype/specialize.zig`, expected-return
  back-constraints (`instantiateCallTypeFromCallerAtType`), callee-row
  widening flowing backward into the requester's node, and `importMono`
  keeping imported tag unions extensible. Each occurrence is measured and
  classified in Slice 0 before anything is deleted (§6.7).
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
  `monotype/type.zig`, shared with Lambda Solved); generated opaque
  evidence has score-based backing selection. These are legitimate
  representation joins and get a named home (§10) instead of deletion or
  denial.
- **"A production matching walk is the right way to compute bindings."**
  Rejected in this consolidation. A matching walk must re-implement type
  equality (alias transparency, head canonicalization, nominal backing
  rules, row closure) — exactly the drift-shaped surface this project
  exists to remove, reproduced in miniature. The checker already had the
  binder assignments in `var_map`; this design publishes them (§7.2) and
  demotes the matcher to a Debug boundary verifier (§7.6).
- **Interning silently coarsens lambda sets through the cloning boundary.**
  `lambda_solved`'s `TypeCloner` memo is keyed on Monotype `TypeId` within
  each clone, so hash-consing structurally equal function types would merge
  callable slots without any value-flow edge (`{ f : I64 -> I64,
  g : I64 -> I64 }` sharing one slot). Occurrence-based cloning lands
  before interning (§12.5, Slice 1).
- **The lambda decision inventory was undercounted ~3× in early drafts.**
  The verified census (§12.4 item 5) includes the alias unwrap, the
  score-selected evidence backings (`unifyGeneratedOpaqueBacking` and its
  expression-side twin), and four distinct iterator joins
  (`unifyForcedDynamicIterator`, `unifyIteratorOwnerStampedPublic`,
  `unifyGeneratedIteratorJoin`, `unifyPublicGeneratedIterator`) — all
  verified present in `lambda_solved/solve.zig`.
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

### 6.2 Schemes: the container exists; the data must be published

`CheckedTypeScheme` (`id`, `key`, `root`, `gv_start`, `gv_len`) and its
`generalizedVars()` accessor exist and round-trip through serialization.
But per the ledger (§3): production leaves the ranges zero, nested schemes
have no entries, and today's consumer reads only `.root` and re-derives
generalization in its own graph — the project's disease in miniature.
`SchemeUseRecord` shows the checker can persist use-site instantiation data;
it currently records only dispatch-constrained variables for selected use
kinds. §7 turns both into complete, verified artifact data.

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
> are materialized views of solved nodes, refilled in place when their node
> gains evidence. Cross-specialization edges import finished Monotypes as
> snapshots, so a specialization that needs more than its requested type is
> a unification conflict rather than a silent rewrite of another
> specialization's final type."

Concretely: a fresh union-find graph (`InstGraph`) per specialization;
`InstVariable` nodes carrying checked defaulting evidence; dozens of
unification sites across `solve.zig` and `lower.zig`; mutable Monotype
views (`addMonoView`/`monoFor`/`fillMono`/`importMono`) refilled as
evidence arrives and sealed at the end; `unsolved_monos` tracking; template
requests deferred until the requester's types stop moving
(`DeferredTemplate`, whose `method_scope: checked.ModuleId` carries the
registry scope); finished specializations imported as one-way snapshots.

The specialization registry (`monotype/specialize.zig`) models the request
lifecycle explicitly: records are *reserved* (key registered) strictly
before lowering, a still-reserved record's request can be *refined* after a
requester's graph seals a deferred request, and completion records the
solved type — when the solved digest differs from the requested one, the
solved shape becomes an alias lookup entry pointing at the same record
(never a rekey). **The requested/solved distinction is not hypothetical;
whether the difference ever carries information the frozen checked types
lack is the migration's most important empirical question, and Slice 0
measures it rather than assuming the answer.**

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
   extensions and distribute disjoint remainders in both directions, and
   `importMono` keeps imported tag unions extensible so callee evidence can
   widen a requester's row. Target: rows are settled at the checked
   boundary. Whether today's two-sided flow ever adds information the
   frozen types lack is the Slice 0 groundness measurement; any
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
5. **Representation decisions** — postcheck-minted facts joined by explicit
   policy: the iterator tier relation (shared with LambdaSolved),
   generated-evidence backing selection, and nominal-wraps-structural root
   selection (`unifyThroughBacking` keeps the nominal as the shared root).
   These are neither re-derivation nor substitution; they become the
   representation algebra (§10). The empty-tag-union-yields-to-concrete
   behavior is deliberately **not** in this category: an empty tag union
   acting as an unresolved slot is either checked bottom/residual data
   (§7.4) or import bookkeeping that deletes with the graph — Slice 0's
   classification assigns each occurrence, and §10.5 bans it from the
   algebra. (Lambda Solved's own empty-tag-union tie-break stays where it
   is, in that solver's census — §12.4.)
6. **Snapshot/refill/logical-deferral bookkeeping** — consequence-management
   of re-solving; deletes with the graph. The *semantic* need to wait for a
   call's representation inputs is real even when the call is not recursive:
   a representation slot can gain information later in its caller's draft.
   That residue becomes §11's explicit pre-publication representation
   dependency scheduling. It carries no logical unknown, never revises a
   checked substitution, and is not the current `DeferredTemplate`
   mechanism under another name.

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
never inferred from backing shape or names.

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
   §12.6, maintained next to the solver and in `design.md`; a Debug
   assertion may claim "all other structures are equal" only after this
   inventory is complete and tested:
   - the empty-tag-union tie-break that yields to a concrete peer;
   - backed-alias unwrapping (today the Lambda helper has no
     `builtin_owner` exception; Slice 0 records whether that retained
     Monotype form can reach the solver and pins the intended behavior);
   - score-selected generated-evidence backings
     (`unifyGeneratedOpaqueBacking` on the pattern side and its
     expression-side twin; higher `generatedOpaqueEvidenceScore` wins,
     loser linked in);
   - four iterator nominal-identity joins (`unifyForcedDynamicIterator`,
     `unifyIteratorOwnerStampedPublic`, `unifyGeneratedIteratorJoin`,
     `unifyPublicGeneratedIterator`) under the shared iterator relation;
   - erased-callable dominance and member accumulation;
   - named backing authority and recursive backing traversal;
   - and, in the erasure pass rather than `unify`: the iterator-backing
     exemption (`in_iter_backing`) keeping a minted `Iter`/`Stream` step
     closure's lambda set, and forced-dynamic backing collection during
     cloning.
   Several of these are the LambdaSolved face of the §10 algebra; the
   Slice 0 classification records, for each, whether its policy home is
   the shared algebra module or the solver.

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
retaining the logical graph. Then put the existing interner behind the
production construction boundary, route every construction through it
(sealed commits, generated types, wrappers, recursive groups), implement
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
`unify`, logical graph sealing, `unsolved_monos`, `DeferredTemplate` and its
logical-key stabilization/refinement, request refinement, and solved-shape
logical aliases (the refill API died in Slice 3). Retain only §11's new
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

Slices 0–5 each deliver standalone value (measured semantics, guarded
lambda seam, published schemes, isolated mutation, interned store, named
and terminating representation policy/closure, shadow-proven ground
translation) and are individually revertible; the project pays for itself
even if paused before cutover.

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
