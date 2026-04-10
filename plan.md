## Typed CIR Transition Plan

Status: in_progress

This plan replaces the previous completed plan.

The goal is to move from the current state to the long-term ideal typed-CIR architecture:

- one published typed CIR boundary artifact
- one canonical solved type store owned by that boundary
- zero per-node type duplication
- typed access exposed only through typed-CIR API methods
- no downstream stage relying on raw `node idx == type idx` knowledge
- no duplicate MIR tree
- no cloned checker module stores used as general monotype workspace
- no remnants of the old boundary model left anywhere in the code base

This plan is explicitly about the long-term ideal implementation.
Do not choose shortcuts, temporary shims, fallback logic, or “good enough for tests” changes.
When design tradeoffs appear, prefer:

- explicit stage contracts
- immutable published artifacts
- one source of truth
- zero semantic recovery in later stages
- runtime/compiler performance that follows from clean architecture, not from hidden coupling

When in doubt, use `~/code/cor` as the architectural guide in spirit:

- publish an explicitly typed solved source program
- make later stages consume that published artifact only
- keep specialization state local to the specializing stage

The intentional divergence from `cor` is representational, not architectural:

- `cor` physically stores type handles on AST nodes
- Roc should keep the existing dense node-index-to-type-store-index invariant internally
- but that invariant must be fully encapsulated inside typed-CIR APIs so downstream stages consume a typed source boundary without knowing the storage trick

That divergence is the long-term ideal here because it preserves the same semantic contract as `cor` while avoiding per-node memory duplication.

## Goal State

At the end of this plan:

1. Published typed CIR is the only checker-to-monotype source boundary.
2. Typed CIR owns:
   - CIR nodes
   - solved type store
   - ident/import/module metadata needed by later stages
3. Typed CIR exposes typed accessors such as:
   - `exprType(expr_idx)`
   - `patType(pat_idx)`
   - `defType(def_idx)`
   - `annotationType(...)`
4. No code outside typed-CIR is allowed to:
   - call `ModuleEnv.varFrom(...)`
   - cast source node indices to type vars directly
   - rely on raw node-index/type-index coupling
5. Monotype consumes typed CIR only through typed-CIR APIs.
6. Monotype specialization state is entirely monotype-owned:
   - specialization table
   - per-specialization instantiation cache
   - per-specialization scratch
7. Monotype does not clone whole per-module checker stores as workspace.
8. Monotype does not mutate checker-style solved stores.
9. Static dispatch targets are resolved before monotype and published explicitly in typed CIR.
10. All old APIs, wrappers, side channels, and residue for the previous architecture are deleted.

## Non-Negotiable Invariants

1. There must be exactly one published typed source boundary artifact.
2. That artifact must be immutable after publication.
3. Later stages must consume explicit facts from typed CIR, not reconstruct or guess them.
4. The `node idx == type idx` relationship is an internal representation detail only.
5. No later stage may know that detail directly.
6. If a stage needs a source type, it asks typed CIR for it.
7. Specialization-specific monotype facts must not live in published typed CIR.
8. Published typed CIR contains source solved facts only; specialization results live on the monotype side.
9. No clone of the full typed source/type-store graph may remain in the final architecture.
10. No compatibility wrapper layer (`MIR`, borrowed views, duplicate typed trees, temporary side tables) may remain in the final architecture.

## Current Problems To Eliminate

### A. Published boundary still leaks storage conventions

Even after the typed-CIR work, the architecture still remembers too much of the old world:

- raw node ids still imply solved vars by convention
- downstream code can still reason in terms of checker storage mechanics instead of typed-CIR contract

The final system must expose typed source facts only through typed-CIR API.

### B. Monotype specialization still reuses checker-style store machinery

Today monotype still contains residue like:

- cloned module type stores / ident stores
- checker-style unification during specialization
- cross-module var copying

That is not the long-term ideal.
Monotype should own specialization state directly, in a `cor`-style specialization model.

### C. Static dispatch target resolution still happens too late

Today monotype still inspects solved type shape to resolve some dispatch targets.
That is semantic recovery after the checker should already know the answer.
Typed CIR should publish resolved dispatch targets explicitly.

## Highest-Quality Final Architecture

### Published Typed CIR

Typed CIR should be a published, owned, immutable source artifact with:

- source CIR nodes
- solved type store
- identifier store
- import resolution facts
- method resolution facts
- any other checker-known semantic source facts that later stages need

Typed CIR should expose typed/source-semantic APIs and hide storage details.

Example API shape:

- `expr(expr_idx) -> CIR.Expr`
- `exprType(expr_idx) -> types.Var`
- `pattern(pattern_idx) -> CIR.Pattern`
- `patternType(pattern_idx) -> types.Var`
- `def(def_idx) -> CIR.Def`
- `defType(def_idx) -> types.Var`
- `resolvedImportModule(...)`
- `resolvedStaticDispatchTarget(expr_idx) -> ResolvedTarget`

Important:

- typed CIR does not physically duplicate the type handle into each node
- typed CIR internally may continue using the dense index invariant
- but no consumer outside typed CIR knows about it

### Monotype

Monotype should consume typed CIR and own all specialization machinery itself.

Its storage should conceptually split into:

1. Immutable published typed source input
2. Monotype global outputs/caches:
   - monotype type interner
   - specialization table
   - emitted proc/value tables
3. Per-specialization scratch:
   - local instantiation cache for solved source vars reachable in that specialization
   - source-node to specialized monotype type scratch
   - local symbol/binding scratch

This means:

- no cloned checker module stores
- no checker unification inside monotype
- no mutation of published source solved state

### Static Dispatch

Static dispatch should be resolved by the checker and published in typed CIR.

Monotype should see either:

- a fully resolved target def/module on the source node, or
- a fully desugared direct-call source form

Monotype must not:

- chase aliases
- chase nominals
- inspect receiver type shape
- look methods up by module/type name

## Phase 1: Make Typed CIR The Only Typed Source API

### 1.1 Define the public typed-CIR contract

Create or tighten the typed-CIR module so it is the only legal API for later stages.

Requirements:

- all source-node/type access goes through typed-CIR methods
- raw `ModuleEnv`/checker storage details are private
- typed-CIR provides all source facts needed by monotype

Do not add convenience backdoors.
If later code needs a fact, typed CIR should publish it explicitly.

### 1.2 Move all raw type lookups behind typed-CIR methods

Audit every use in `src/` of:

- `ModuleEnv.varFrom(...)`
- direct node-id to type-var casting
- direct checker `types.Store` lookups that correspond to source node typing

Replace them with typed-CIR API calls.

Completion criteria:

- no code outside typed-CIR may access source solved types by raw index convention
- the only remaining knowledge of the index invariant lives inside typed-CIR implementation

### 1.3 Delete old naming and boundary residue

Delete all remaining residue of:

- `MIR`
- old borrowed boundary wrappers
- duplicated “published source tree” language if it no longer matches reality

The boundary should be described consistently as typed CIR.

## Phase 2: Publish Explicit Static Dispatch Facts In Typed CIR

### 2.1 Identify every static dispatch form at the checker boundary

Catalog all source forms whose target can be resolved by the checker.

For each form, define the exact published fact shape.

Minimum required fact content:

- resolved target module
- resolved target def or symbol
- any explicit receiver/type-evidence facts needed downstream

### 2.2 Publish resolved dispatch targets during checking

The checker should publish the resolved target directly into typed CIR.

This must happen before monotype.

No fallback path:

- if dispatch is unresolved, that is a checker problem
- monotype should never attempt semantic recovery

### 2.3 Convert monotype to consume explicit dispatch facts only

Delete monotype code that:

- resolves dispatch receiver vars from type shape
- chases aliases/nominals to infer target type/module
- looks up method symbols during lowering

Replace it with direct use of typed-CIR dispatch facts.

Completion criteria:

- no `resolveDispatchReceiver*`
- no `resolveMonomorphicDispatchTarget*`
- no type-shape-driven method lookup remains in monotype

## Phase 3: Replace Monotype’s Cloned Checker Stores With Monotype-Owned Specialization State

### 3.1 Define explicit monotype specialization storage

Design the monotype-owned specialization data structures.

They should include:

- specialization key
  - source function identity
  - specialization-driving monotype input shape
  - any enclosing specialization identity needed for local fns
- specialization table
  - pending/in-progress/completed state
- per-specialization local instantiation cache
- per-specialization scratch for lowered node types/bindings

This should mirror `cor`’s specialization-local cloning idea, not checker module cloning.

### 3.2 Restrict cloning to specialization-local instantiation only

If solved source vars still need cloning/instantiation, clone only the reachable solved vars for the current specialization.

Do not clone whole module stores.

The local instantiation cache should:

- start empty per specialization
- populate only for reachable source vars in that specialization
- be discarded when specialization lowering completes

### 3.3 Remove checker-style unification from monotype

Delete monotype usage of checker unification machinery as specialization workspace.

That includes:

- checker problem stores as ordinary monotype specialization infrastructure
- checker unify/occurs scratch as ordinary monotype specialization infrastructure
- mutation of cloned checker stores to drive specialization

Replace with explicit monotype-side specialization bookkeeping.

### 3.4 Remove cross-module checker-var copying from monotype

Delete `copyCheckerVarToModule(...)`-style logic.

Cross-module specialization should use:

- explicit published source identities from typed CIR
- monotype-owned specialization keys
- monotype-owned local instantiation caches

not copying mutable solved vars between checker store clones.

### 3.5 Delete full-module type/ident store cloning in monotype context init

Delete monotype startup logic that clones every module’s:

- `types.Store`
- `Ident.Store`

Monotype should keep only:

- published typed CIR input
- monotype-owned specialization/interner/output state

Completion criteria:

- no whole-module `types.Store.clone(...)` in monotype
- no whole-module `Ident.Store.clone(...)` in monotype
- no checker-store mutation APIs in ordinary monotype lowering

## Phase 4: Make Typed CIR The Only Source Of Source Typing Truth

### 4.1 Audit and remove parallel source typing side channels

Find and delete any remaining side channels that duplicate or bypass typed-CIR source typing facts.

Examples to audit:

- legacy side tables
- helper wrappers around the old boundary
- cached source-node typing maps outside typed CIR

The rule is:

- if it is source solved typing, it belongs in typed CIR
- if it is specialization-specific, it belongs in monotype

There should be nothing in between.

### 4.2 Enforce API boundaries in type signatures

Refactor stage entrypoints so that:

- checker outputs typed CIR
- monotype accepts typed CIR
- monotype does not accept raw checker module env/state as an alternative

Make the wrong thing impossible at the type/API level.

### 4.3 Delete obsolete implementation hooks

Delete any API that only exists to support the old architecture, including:

- mutable typed-CIR accessors not needed by the final design
- raw checker-source-type accessor helpers used by later stages
- back-compat wrappers around the old boundary contract

## Phase 5: Verification And Audit

### 5.1 Add targeted tests for typed-CIR API encapsulation

Add focused tests proving:

- typed CIR provides source node typing through its API
- later stages no longer depend on raw index-casting conventions
- typed CIR remains valid as the only source typing boundary

### 5.2 Add targeted tests for explicit static dispatch facts

Add tests covering:

- nominal receiver dispatch
- alias receiver dispatch
- imported-module dispatch
- dispatch through resolved type aliases/attached methods

These tests must verify monotype succeeds without type-shape rediscovery logic.

### 5.3 Add targeted tests for monotype specialization model

Add tests covering:

- repeated specializations of the same source fn at multiple types
- local functions specialized in multiple contexts
- cross-module specialization
- polymorphic higher-order cases that previously depended on cloned checker stores

### 5.4 Full-suite verification

The plan is complete only when all relevant suites pass, including at minimum:

- `zig build test-monotype`
- `zig build test-eval -- --threads 1`
- `zig build test-eval-host-effects -- --threads 1`
- `zig build test-cor-pipeline`

Also run any newly relevant checker/typed-CIR-focused tests introduced by the work.

### 5.5 Final architecture audit

Do a final fresh audit and confirm:

- no code outside typed CIR knows about raw node-index/type-index equivalence
- no monotype whole-module checker-store clones remain
- no checker-style unification remains in monotype specialization flow
- no static dispatch target recovery remains in monotype
- no duplicate boundary artifact remains
- no obsolete wrapper machinery remains anywhere in `src/`

## Explicit Things To Watch Out For

1. Do not accidentally reintroduce a duplicate “typed tree” while trying to clean up typed CIR.
2. Do not preserve old APIs “for convenience”.
3. Do not hide checker-store mutation behind a renamed helper and call it progress.
4. Do not let monotype specialization facts leak back into typed CIR.
5. Do not trade away memory by physically duplicating per-node type handles unless there is a truly compelling reason. The current plan assumes there is not.
6. Do not let static dispatch remain half-resolved. Either checker publishes the target or compilation fails earlier.
7. Do not stop at “tests pass” if old architectural residue still exists.

## Definition Of Done

This plan is complete only when all of the following are true:

- typed CIR is the only published typed source boundary
- source solved typing is accessed only through typed-CIR API
- raw node-index/type-index coupling is fully encapsulated
- monotype owns specialization state directly
- monotype no longer clones whole checker module stores
- monotype no longer mutates checker-style solved stores
- monotype no longer resolves static dispatch targets from type shape
- old boundary machinery is deleted with no residue left in `src/`
- targeted tests and full verification suites pass
