# MIR Architecture Cutover Plan

## Objective

Replace the old competing-facts architecture with a MIR-family lowering
pipeline.

The final pipeline is:

```text
checked CIR
  -> mono MIR
  -> lifted MIR
  -> lambda-solved MIR
  -> executable MIR
  -> IR
  -> LIR
```

This plan does not skip monomorphization, lambda lifting, lambda-set solving, or
executable representation planning. Those are real compiler responsibilities.
The cutover replaces the current implementations and contracts so that every
post-check stage has one explicit source of truth and no semantic side channels.

The current modules line up with those responsibilities:

```text
monotype          -> mono MIR
monotype_lifted   -> lifted MIR
lambdasolved      -> lambda-solved MIR
lambdamono        -> executable MIR
```

The final implementation will put these stages under one MIR architecture and
delete the old top-level architecture. The final namespace is:

```text
src/mir/mono
src/mir/lifted
src/mir/lambda_solved
src/mir/executable
```

The important final-state rule is not the directory name. The important rule is
that the old source/executable fact architecture is gone, and each MIR-family
stage has a precise, enforceable contract.

## Non-Negotiable Rules

No compiler stage after checking may recover, guess, reconstruct, approximate,
or best-effort semantic information.

Every post-check stage must consume explicit facts from the previous stage.

Backends must not think about reference counting. They lower explicit LIR
`incref` and `decref` statements only.

Static dispatch is eliminated in `mono MIR`, the first monomorphic stage. It
must not survive into lifted MIR, lambda-solved MIR, executable MIR, IR, or LIR.

The executable MIR stage replaces the current `lambdamono` fact planner. It must
not preserve `ValueFact`, `CallableFact`, expression fact maps, local constructor
facts, or source/executable duplicate truth.

## Hard Command Rule

Every Zig invocation must go through:

```sh
ci/guarded_zig.sh zig ...
```

Do not run `zig ...` directly.

If a wrapper precheck fails, fix the architectural or lint failure first. Do not
bypass, weaken, or locally skip the wrapper.

## Commit Discipline

Commit as work progresses.

Each commit must contain one coherent final-state step:

- one stage contract hardening
- one obsolete-family deletion
- one data-structure introduction
- one pipeline rewiring
- one audit strengthening
- one test rewrite

Do not mix unrelated work into a commit.

Temporary investigation prints, compatibility adapters, old-stage forwarding
modules, and architectural exceptions must not be committed.

## Current Problems To Remove

The current pipeline performs the right broad responsibilities, but its
contracts are wrong for the final architecture.

Current `monotype`:

- lowers checked CIR into monomorphic expression/type structure
- still emits `dispatch_call`, `type_dispatch_call`, and `method_eq`
- threads `attached_method_index` downstream

Current `monotype_lifted`:

- correctly owns lambda lifting and capture discovery
- still preserves unresolved dispatch nodes
- still threads method lookup data downstream

Current `lambdasolved`:

- correctly owns lambda-set inference, erasure propagation, and SCC ordering
- still preserves unresolved dispatch nodes
- still has to reason about dispatch argument constraints

Current `lambdamono`:

- correctly owns executable representation work
- incorrectly performs late static dispatch resolution
- carries `ValueFact`, `CallableFact`, `FactId`, and `expr_facts`
- reconstructs or refines source types from expression syntax
- resolves method owners from expressions instead of monomorphic types

The final plan keeps the valid responsibilities and deletes the invalid
contracts.

## Resolution Boundaries

Static dispatch target selection belongs in mono MIR.

It cannot happen before mono MIR because checked CIR can still contain generic
dispatch sites whose target depends on the concrete specialization.

It must not happen after mono MIR because mono MIR already has the required
facts:

- the checked dispatch operation
- the checked dispatch constraint function type
- the monomorphic receiver or type-dispatcher type
- the checked method registry
- the specialization table

Lambda lifting does not change the nominal owner of a receiver type.

Lambda-set solving does not change which attached method a monomorphic receiver
type names.

Executable representation lowering does not own source method semantics.

Therefore mono MIR is the exact resolution boundary. Static dispatch is checked
source structure before mono MIR and direct-call MIR after mono MIR.

## Final Stage Contracts

### Checked CIR

Checked CIR is the last source-level representation.

It may contain:

- `e_dispatch_call`
- `e_type_dispatch_call`
- `e_method_eq`
- `e_structural_eq`
- static dispatch constraints
- type-var alias dispatch information

It owns:

- source expression shape
- checked expression types
- static dispatch legality
- implicit structural equality rewriting
- nominal/custom equality marking

It must not own:

- final executable method targets for generic dispatch
- executable layout decisions
- executable callable packaging
- lambda-set representation

Checked CIR may keep source dispatch because it is checked source structure.
Every stage after `mono MIR` must be dispatch-free.

### Mono MIR

Mono MIR is the first post-check MIR stage.

It owns:

- monomorphic specialization of checked CIR
- expression construction with mandatory monomorphic types
- top-level specialization requests
- static dispatch target resolution
- nominal/custom equality target resolution
- direct-call emission for resolved static dispatch

Its input is checked CIR plus the checked type store, checked method registry,
and specialization roots.

Its output is monomorphic, typed MIR.

Mono MIR may still contain:

- local functions
- closures
- value calls through function values
- direct calls to known specialized procedures
- structural equality
- source tag names and full monomorphic tag-union types

Mono MIR must not contain:

- `dispatch_call`
- `type_dispatch_call`
- `method_eq`
- source type variables
- source-type refinement helpers
- syntax-derived singleton tag-union types
- method lookup tables for later stages

Every expression in mono MIR has a mandatory `MonoTypeId`.

The mono MIR store API must require the type at construction:

```zig
fn addExpr(store: *Store, ty: MonoTypeId, data: Expr.Data) Allocator.Error!ExprId
```

There must be no exported API that creates an untyped expression.

### Lifted MIR

Lifted MIR owns lambda lifting and capture discovery.

It consumes dispatch-free mono MIR.

It produces dispatch-free lifted MIR where:

- closures and local functions have been lifted to procedure definitions
- captures are explicit procedure metadata
- local-function rename environments are stage-private
- every expression still has a mandatory type

Lifted MIR must not:

- resolve static dispatch
- carry method registries
- carry attached method indexes
- reintroduce dispatch nodes
- infer semantic type facts from expression syntax

If lift changes procedure identities, it must rewrite direct-call targets through
an explicit procedure-id map. It must not recover targets from symbol names.

### Lambda-Solved MIR

Lambda-solved MIR owns callable/lambda-set solving.

It consumes dispatch-free lifted MIR.

It owns:

- lambda-set inference
- exact callable set representation
- capture type association with callable members
- erasure propagation
- recursive SCC ordering
- final callable representation facts needed by executable MIR

It must not:

- treat ordinary static dispatch as a lambda set
- preserve method names as unresolved executable operations
- reconstruct callable targets from source function types
- carry method registries or attached method indexes
- emit source/executable duplicate facts

Lambda-solved MIR output must make callable facts explicit in its type and
procedure metadata. The executable MIR stage must not have to rediscover them.

### Executable MIR

Executable MIR replaces the current `lambdamono` fact planner.

It consumes lambda-solved MIR.

It owns:

- executable representation of callables
- direct calls
- erased calls
- packed function values
- capture record construction
- explicit bridge insertion
- executable type publication
- explicit logical layout graph construction
- entrypoint wrapper generation

It must not own:

- static dispatch resolution
- method lookup
- expression-based owner resolution
- source type reconstruction
- source/executable fact side tables
- fallback executable signatures

Executable MIR is the final post-check executable representation consumed by IR.

Its AST may contain:

```text
call_direct
packed_fn
call_erased
bridge
low_level
structural_eq
bool_not
```

It must not contain:

```text
dispatch_call
type_dispatch_call
method_eq
ValueFact
CallableFact
FactId
expr_facts
```

### IR

IR consumes executable MIR only.

IR lowering may consume:

- executable MIR store
- executable MIR types
- executable MIR layouts
- executable MIR symbols
- executable MIR root defs
- strings

IR lowering must not import:

- checked CIR
- checker type stores
- method registries
- lambda-solved builder internals
- executable MIR builder internals

IR must continue to expose direct/erased call operations only:

```text
call_direct
call_erased
call_low_level
```

IR must not gain method or dispatch variants.

### LIR And Backends

LIR consumes IR.

Reference counting is inserted before backends, as explicit LIR statements.

Backends consume LIR only. They must not import MIR, IR builder internals,
checked CIR, method registries, or reference-counting analysis.

## Final Data Structures

### Procedure Identity

Each MIR-family stage will use stable `Symbol` values as public procedure
references for direct calls. A stage may use private dense procedure ids
internally, but exported call nodes must refer to stable symbols.

The required invariant is:

```text
direct call target identity is stored in the call node
```

If a stage rewrites procedure identities, it must carry an explicit map:

```text
old ProcId -> new ProcId
```

or:

```text
old Symbol -> new Symbol
```

It must not recover targets from names, expression shapes, or source lookup.

### Direct Calls And Value Calls

MIR must distinguish known direct calls from value calls.

Required distinction:

```zig
call_direct {
    proc: Symbol,
    args: Span(ExprId),
}

call_value {
    func: ExprId,
    args: Span(ExprId),
    call_ty: TypeId,
}
```

The exported MIR-family ASTs use these names. The semantic distinction cannot be
collapsed.

Static dispatch lowers to `call_direct` in mono MIR.

Calling a first-class function value remains `call_value` until callable solving
or executable lowering turns it into a direct or erased call.

### Method Registry

Build an explicit checked method registry before mono MIR lowering.

Conceptual shape:

```zig
const MethodKey = struct {
    owner_module_idx: u32,
    owner_type_ident: Ident.Idx,
    method_ident: Ident.Idx,
};

const MethodDefRef = struct {
    module_idx: u32,
    def_idx: CIR.Def.Idx,
};

const MethodTarget = union(enum) {
    proc: MethodDefRef,
    intrinsic: IntrinsicMethod,
};
```

The registry maps:

```text
MethodKey -> MethodTarget
```

The registry is an input to mono MIR only.

It is not part of lifted MIR, lambda-solved MIR, executable MIR, IR, or LIR.

The registry must be built from checked declaration facts. It must not rely on
late text lookup or module-name scanning during MIR lowering.

Checker validation and mono MIR lowering must agree through this registry.

The checker may keep `StaticDispatchConstraint` as the legality mechanism, but
the method definition identity used for validation must be the same explicit
definition identity mono MIR later consumes. There must not be one checker
lookup path and a second mono MIR lookup path that can disagree.

If ordinary method calls and type-var alias method calls need different origin
metadata, add that explicit origin to the checked constraint. Do not infer the
origin later from expression shape.

### Owner Resolution

Owner resolution takes a monomorphic type, never an expression:

```zig
fn ownerForDispatchType(types: *const MonoTypeStore, ty: MonoTypeId) MethodOwner
```

Allowed owner cases:

```text
nominal
primitive
list
box
```

Forbidden owner cases:

```text
record
tuple
anonymous tag union
function
callable value
erased function
unresolved builder type
```

Forbidden owner cases are compiler invariant failures. They are not fallback
paths.

Delete expression-based owner APIs, including the current family represented by:

```text
attachedMethodOwnerForExpr
resolveAttachedMethodTargetFromExpr
ownerForExpr
resolveTargetFromExpr
```

Chained dispatch works by lowering the receiver expression first and using the
receiver expression's MIR type.

### Mono Type Store

Mono MIR output types must be monomorphic.

Builder-private placeholders are allowed only inside mono MIR construction.

Exported mono MIR types must not contain:

```text
checker source vars
for_a
flex_for_a
unresolved placeholders
```

Nominal identity must be preserved. Transparent aliases resolve methods through
their nominal identity, not through the structural backing type.

### Lambda-Solved Types

Lambda-solved MIR may use a richer callable type representation than mono MIR.

It must explicitly encode:

- function argument types
- return type
- lambda/callable members
- captures for each callable member
- erased callable representation when required

It must not use ordinary source tag unions as a hidden carrier for unresolved
static dispatch.

If physical layout later uses a tag-union-like representation for callable
sets, that is a layout decision derived from explicit callable facts.

### Executable Types

Executable MIR types are representation types.

They may include:

```text
nominal
primitive
list
box
tuple
record
tag_union
erased_fn
callable representation
```

They must not include:

```text
source type variables
unresolved links
placeholder
unbd
fact handles
```

Executable type lowering consumes lambda-solved MIR types and metadata. It does
not inspect source CIR or reconstruct from expressions.

## Static Dispatch Lowering

### Ordinary Dispatch

For checked source:

```roc
x.foo(a, b)
```

mono MIR lowering does:

1. Lower the checked dispatch constraint function type into a mono function
   type.
2. Lower `x` with expected type `constraint.args[0]`.
3. Resolve the owner from `typeOf(x)`.
4. Look up `(owner, foo)` in the checked method registry.
5. Request or reserve the target mono specialization at the exact constraint
   function type.
6. Lower `a` and `b` with expected types `constraint.args[1..]`.
7. Emit `call_direct`.

No later stage sees the method name as an unresolved call.

### Type Dispatch

For checked source:

```roc
Thing.default()
```

mono MIR lowering does:

1. Lower the type-var alias target type into a mono type.
2. Resolve the owner from that mono type.
3. Lower the checked dispatch constraint function type.
4. Look up `(owner, default)` in the checked method registry.
5. Request or reserve the target mono specialization.
6. Lower explicit args.
7. Emit `call_direct`.

There is no receiver argument for type dispatch unless the checked constraint
itself has one.

### Nominal Equality

For nominal/custom equality:

```roc
x == y
x != y
```

mono MIR lowering resolves `is_eq` through the same owner and registry path.

`==` emits a direct call to the specialized `is_eq`.

`!=` emits the same direct call followed by `bool_not`.

Anonymous structural equality remains `structural_eq` and never goes through
method dispatch.

## Tags And Constructors

Tag names remain symbolic until layout lowering.

That is required because layout indices are not available in mono MIR.

The forbidden behavior is constructing source types from local tag syntax.

For:

```roc
Err("x")
```

mono MIR uses the checked expression's specialized type.

It must not synthesize:

```roc
[Err(Str)]
```

when the checked expression type is:

```roc
[Ok(I64), Err(Str)]
```

Discriminants and payload indexes may be computed from the full mono MIR
tag-union type. They must not be computed from a singleton type invented from
syntax.

## Callable And Capture Flow

Callable facts must flow as typed MIR data, not side tables.

Mono MIR:

- preserves function values and direct calls distinctly
- assigns monomorphic function types to expressions
- does not package erased callables

Lifted MIR:

- lifts local functions and closures
- computes captures
- stores captures in lifted procedure metadata

Lambda-solved MIR:

- determines exact callable sets
- associates capture types with callable members
- propagates erasure requirements

Executable MIR:

- builds capture records
- emits `packed_fn`
- emits `call_erased`
- emits direct calls where targets are exact
- inserts explicit bridges

The following are forbidden:

- callable truth in environment side fields
- callable truth in expression side tables
- capture truth in `FactId` arrays
- target recovery from source function types
- capture recovery from body scanning in executable MIR
- body-derived summaries as executable truth

## Deletions

Delete these families completely:

```text
ValueFact
CallableFact
FactId as semantic truth
expr_facts
refinedSourceTypeForExpr
exactTagSourceTypeForExpr
attachedMethodOwnerForExpr
resolveAttachedMethodTargetFromExpr
source/executable relation side tables
late source type refinement helpers
singleton tag source type construction
method lookup data threaded beyond mono MIR
```

Delete exported dispatch variants after checked CIR:

```text
dispatch_call
type_dispatch_call
method_eq
```

Allowed remaining locations for these checked dispatch names:

```text
src/canonicalize
src/check
src/lsp checked-CIR display/query code
src/mir/mono builder input pattern matching
tests that explicitly inspect checked CIR
```

Forbidden locations:

```text
src/mir/lifted
src/mir/lambda_solved
src/mir/executable
src/ir
src/lir
backends
semantic eval lowering
compile runner output stages
```

## Module Restructure

Move the valid responsibilities under `src/mir`.

Final intended layout:

```text
src/mir/mod.zig
src/mir/mono/ast.zig
src/mir/mono/type.zig
src/mir/mono/build.zig
src/mir/mono/method_registry.zig
src/mir/mono/verify.zig
src/mir/lifted/ast.zig
src/mir/lifted/lower.zig
src/mir/lifted/verify.zig
src/mir/lambda_solved/ast.zig
src/mir/lambda_solved/type.zig
src/mir/lambda_solved/lower.zig
src/mir/lambda_solved/verify.zig
src/mir/executable/ast.zig
src/mir/executable/type.zig
src/mir/executable/lower.zig
src/mir/executable/layouts.zig
src/mir/executable/verify.zig
```

The top-level old directories must not remain in the final state:

```text
src/monotype
src/monotype_lifted
src/lambdasolved
src/lambdamono
```

This is a rename and contract hardening of the valid responsibilities, not a
deletion of the responsibilities.

## Build Graph

Update `src/build/modules.zig`.

Final relevant dependency shape:

```text
mir -> base, types, can, check, symbol, layout
ir  -> base, types, symbol, mir, layout
lir -> base, layout, types, can, ir
```

Remove top-level build modules:

```text
monotype
monotype_lifted
lambdasolved
lambdamono
```

Do not provide old-name compatibility modules.

All public pipelines must call:

```text
checked CIR -> mir.mono -> mir.lifted -> mir.lambda_solved -> mir.executable -> ir -> lir
```

Required pipeline call-site updates include:

```text
src/eval/pipeline.zig
src/compile/runner.zig
src/cli/main.zig
src/dev_shim/main.zig
src/snapshot_tool/main.zig
src/eval/test/helpers.zig
```

## Implementation Order

### 1. Establish MIR Namespace

Create `src/mir` and move the current post-check stage code into MIR-family
submodules as implementation material.

This is not a compatibility layer. The old top-level modules must be removed
from the build graph in the same work sequence.

Commit when the build graph names the MIR-family modules and no public pipeline
imports the old top-level module names.

### 2. Build Checked Method Registry

Add `src/mir/mono/method_registry.zig`.

Build a registry from checked modules before mono MIR lowering.

The registry must map owner/type/method ids to method definition refs or
intrinsics.

Use the registry for mono MIR static dispatch lowering.

Then remove downstream `attached_method_index` threading.

### 3. Harden Mono MIR AST

Remove exported mono MIR variants:

```text
dispatch_call
type_dispatch_call
method_eq
```

Add or clarify:

```text
call_direct
call_value
structural_eq
bool_not
```

Mono MIR lowering from checked CIR must resolve:

- ordinary dispatch
- type dispatch
- nominal/custom equality

to direct calls immediately.

Commit when mono MIR verification proves no exported mono MIR dispatch nodes
exist.

### 4. Harden Lifted MIR

Update lifted MIR to consume dispatch-free mono MIR.

Delete all dispatch cases from lifted MIR AST and lowering.

If procedure ids or symbols change during lifting, add explicit rewrite maps for
direct-call targets.

Commit when lifted MIR verification proves:

- all direct call targets exist
- all captures are explicit
- no dispatch terms exist in exported lifted MIR

### 5. Harden Lambda-Solved MIR

Update lambda-solved MIR to consume dispatch-free lifted MIR.

Delete all dispatch cases from lambda-solved AST, inference, erasure
propagation, and verification.

Preserve and clean up the real responsibilities:

- instantiate lifted types
- infer callable sets
- propagate erasure
- order recursive SCCs

Commit when lambda-solved MIR verification proves:

- no dispatch terms exist
- callable members and captures are explicit
- erased callable requirements are explicit
- direct call targets remain explicit

### 6. Replace Executable Fact Planner

Rewrite executable MIR lowering so it consumes lambda-solved MIR and emits
executable MIR without fact side tables.

Delete:

```text
ValueFact
CallableFact
expr_facts
FactKey
semantic FactId usage
```

Specialization keys must be:

```text
target procedure + monomorphic call signature + representation mode
```

not:

```text
fact handles + expression ids + source/executable side channels
```

Commit when executable MIR verification proves:

- direct calls have explicit targets
- direct call args match target signatures
- erased calls have explicit erased function types
- packed functions have explicit captures
- bridge nodes connect known executable MIR types

### 7. Delete Source-Type Reconstruction

Delete the whole source-type reconstruction family:

```text
refinedSourceTypeForExpr
exactTagSourceTypeForExpr
freshContent(.{ .tag_union
singleton tag source helpers
late source result refinement
```

Do not keep compatibility wrappers.

Commit only after targeted searches prove there are no stragglers.

### 8. Rewire IR Lowering

Update `src/ir/lower.zig` to consume executable MIR.

IR lowering must be source-blind and method-blind.

It consumes executable MIR data only and emits existing IR direct/erased call
forms.

Commit when IR lowering has no imports of checked CIR or MIR builder internals.

### 9. Rewire Public Pipelines

Update eval, compile, CLI, dev shim, snapshot tool, and test helpers to call the
MIR-family pipeline.

Remove helper names that refer to old stages.

Commit when `rg` finds no old post-check imports or pipeline labels.

### 10. Strengthen Audits

Make audits allowlist-based.

Forbid old stage names outside historical docs if they remain at all:

```text
monotype
monotype_lifted
lambdasolved
lambdamono
```

Forbid old fact and reconstruction families everywhere outside tests that
intentionally check the audit:

```text
ValueFact
CallableFact
expr_facts
refinedSourceTypeForExpr
exactTagSourceTypeForExpr
attachedMethodOwnerForExpr
resolveAttachedMethodTargetFromExpr
ownerForExpr
resolve.*TargetFromExpr
```

Forbid dispatch variants outside checked CIR and mono MIR input lowering:

```text
dispatch_call
type_dispatch_call
method_eq
```

Commit when guarded semantic audits pass.

### 11. Rewrite Tests

Rewrite old intermediate-stage tests.

Obsolete expectations:

- dispatch survives into monotype
- dispatch survives into lambdasolved
- accumulator facts are visible before lambdamono
- executable fact tables contain expected facts
- old stage type stores contain old-stage shapes

Replacement expectations:

- checked CIR contains dispatch where appropriate
- mono MIR contains direct calls and no dispatch
- lifted MIR contains explicit captures and no dispatch
- lambda-solved MIR contains explicit callable sets and no dispatch
- executable MIR contains direct/erased calls, packed functions, bridges, and no
  fact side tables
- IR/LIR contain direct/erased calls only

## Required Structural Tests

Add MIR-family verification tests for each stage.

Mono MIR:

- every expression has a mono type
- no dispatch nodes exist
- static dispatch becomes direct call
- nominal/custom equality becomes direct `is_eq`
- structural equality remains structural
- transparent aliases preserve nominal identity

Lifted MIR:

- every lifted procedure target exists
- every capture is explicit
- direct-call targets are rewritten by explicit maps
- no dispatch nodes exist

Lambda-solved MIR:

- callable sets are explicit
- captures are attached to callable members
- erasure requirements are explicit
- no dispatch nodes exist

Executable MIR:

- every direct call target exists
- direct call arg/result types match signatures
- every packed function has explicit capture metadata
- every erased call has an explicit erased function type
- bridges connect known executable types
- no fact side tables exist

IR/LIR:

- direct calls lower to direct calls
- erased calls lower to erased calls
- no method/dispatch operation exists

## Required Behavioral Tests

Static dispatch:

- generic dispatch specializes to different nominal method targets
- generic target methods specialize at exact monomorphic function types
- chained dispatch resolves from the lowered receiver result type
- type-var alias dispatch resolves from the specialized alias target type
- primitive methods resolve through builtin primitive owners
- list methods resolve through builtin List owner
- box methods resolve through builtin Box owner
- custom equality lowers to direct `is_eq`
- inequality lowers to direct `is_eq` plus `bool_not`
- anonymous record equality remains structural equality
- transparent tag-union aliases resolve through nominal identity
- cross-module attached methods resolve through registry def refs
- recursive and mutually recursive methods use reserved specialized proc ids
- hosted/effect/platform methods use explicit method targets or explicit
  intrinsics
- tag construction never creates singleton source tag-union types

Callable/capture behavior:

- direct top-level function call
- generic top-level function specialization
- local closure with no captures
- local closure with captures
- recursive local function
- mutually recursive local functions
- closure returned from a function
- closure passed as an argument
- exact callable direct call without erased packaging
- erased boundary packaging with capture record
- boxed erased-call round trip
- non-boxed polymorphic closure does not erase
- hosted function flowing as first-class value
- deterministic capture record field ordering

End-to-end:

```sh
ci/guarded_zig.sh zig build test-cor-pipeline
ci/guarded_zig.sh zig build test-eval
ci/guarded_zig.sh zig build test-glue
```

If the test name `test-cor-pipeline` remains, its contents must be MIR-family
pipeline tests, not old-stage contract tests.

## Failure Handling

For every failure:

1. Identify which stage must have owned the missing fact.
2. Add that fact to that stage's explicit output.
3. Delete any old reconstruction or side-channel path exposed by the failure.
4. Strengthen audits if the failure reveals a family that could return.
5. Rerun the narrowest guarded test that exercises the failure.

Forbidden responses:

- restoring old module imports
- adding compatibility shims
- making owner resolution expression-based
- adding expression fact side tables
- storing callable truth in environment entries
- reconstructing source types from expression syntax
- using body-derived summaries as executable truth
- weakening audits
- changing tests to preserve obsolete intermediate invariants

## Final Verification Checklist

The cutover is complete only when all of these are true:

- public pipeline is `checked CIR -> mono MIR -> lifted MIR -> lambda-solved MIR
  -> executable MIR -> IR -> LIR`
- no public pipeline imports old top-level post-check modules
- static dispatch exists only in checked CIR and mono MIR input pattern matching
- mono MIR output has no dispatch nodes
- lifted MIR output has no dispatch nodes
- lambda-solved MIR output has no dispatch nodes
- executable MIR output has no dispatch nodes
- no source/executable fact side tables remain
- no expression-based method owner resolver remains
- no syntax-derived source type reconstruction remains
- method registry is consumed only by mono MIR construction
- IR lowering consumes executable MIR only
- LIR/backends do not know about source methods
- semantic audits forbid the deleted families
- guarded eval and glue gates pass
