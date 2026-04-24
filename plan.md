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

Therefore mono MIR is the exact source-method resolution boundary. Static
dispatch is checked source structure before mono MIR and an explicit
procedure-symbol MIR call after mono MIR.

Procedure-symbol MIR calls are not executable direct calls.

Executable direct calls begin only in executable MIR, after lambda-set solving,
erasure propagation, capture representation, and executable specialization have
all run.

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
- procedure-symbol call emission for resolved static dispatch

Its input is checked CIR plus the checked type store, checked method registry,
and specialization roots.

Its output is monomorphic, typed MIR.

Mono MIR may still contain:

- local functions
- closures
- value calls through function values
- `call_proc` calls to source/MIR procedures
- `proc_value` values
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
- executable direct calls
- executable call signatures

Every expression in mono MIR has a mandatory `MonoTypeId`.

The mono MIR store API must require the type at construction:

```zig
fn addExpr(store: *Store, ty: MonoTypeId, data: Expr.Data) Allocator.Error!ExprId
```

There must be no exported API that creates an untyped expression.

Mono MIR is built per monomorphic specialization. It is not one shared CIR-like
tree with specialization-indexed side tables.

The identity of a mono MIR expression is scoped to its specialization. If the
same checked CIR expression appears in two specializations, those are two mono
MIR expressions with separate mono types and separate resolved procedure-symbol
callees.

The specialization key is:

```text
source procedure identity + requested mono source function type
```

It is not:

```text
source procedure identity alone
executable procedure identity
executable representation mode
lambda-set shape
```

### Lifted MIR

Lifted MIR owns lambda lifting and capture discovery.

It consumes dispatch-free mono MIR.

It produces dispatch-free lifted MIR where:

- closures and local functions have been lifted to procedure definitions
- captures are explicit procedure metadata
- local-function rename environments are stage-private
- every expression still has a mandatory type
- `call_proc` and `proc_value` targets still refer to source/MIR procedure
  symbols, not executable procedures

Lifted MIR must not:

- resolve static dispatch
- carry method registries
- carry attached method indexes
- reintroduce dispatch nodes
- infer semantic type facts from expression syntax

If lift changes procedure identities, it must rewrite `call_proc` and
`proc_value` targets through an explicit procedure-id map. It must not
recover targets from symbol names.

If a pre-lift `call_proc` is allowed to target a local function, lift must rewrite
that target and supply the explicit capture path required by the lifted function.

The preferred invariant is simpler: mono MIR `call_proc` may target only
top-level mono-specialized procedures. Local functions and closures remain value
calls until after lifting has made captures explicit.

### Lambda-Solved MIR

Lambda-solved MIR owns callable/lambda-set solving.

It consumes dispatch-free lifted MIR.

It owns:

- lambda-set inference
- exact callable set representation
- capture type association with callable members
- erasure propagation
- recursive SCC ordering
- final callable representation metadata needed by executable MIR

It must not:

- treat ordinary static dispatch as a lambda set
- preserve method names as unresolved executable operations
- reconstruct callable targets from source function types
- carry method registries or attached method indexes
- emit source/executable duplicate facts
- decide static dispatch targets
- decide executable direct-call signatures

Lambda-solved MIR output must make callable metadata explicit in its type and
procedure metadata. The executable MIR stage must not have to rediscover it.

`call_proc` must participate in lambda-set inference exactly like an ordinary
call whose callee expression is `Var(proc)`.

`proc_value` must participate in lambda-set inference exactly like
`Var(proc)`.

For every `call_proc` and every `proc_value`, lambda-solved MIR must:

- add an SCC dependency edge to the procedure target
- instantiate the procedure target's callable type
- unify the procedure target type with the requested callable type
- fix capture types for the target when the target appears in a lambda set
- preserve the procedure target identity for executable MIR

For every `call_proc`, lambda-solved MIR must also unify the call arguments and
result with the procedure target type.

This rule is mandatory. A `call_proc` must not bypass lambda-set solving merely
because its source procedure target is already explicit.

This is also mandatory for `proc_value`. A `proc_value` may become
a callable-set value or packed erased function value later, but lambda-solved MIR
must own that decision.

Lambda-solved builder internals may use solver links, unbound variables, and
generalized variables while solving. Exported lambda-solved MIR must expose a
zonked view for every executable specialization input. Generalized variables may
remain only in specialization templates that are explicitly instantiated before
executable lowering consumes them.

Erasure is decided here. Executable MIR consumes explicit erased-callable
metadata from lambda-solved MIR and must not infer erasure from usage.

### Executable MIR

Executable MIR replaces the current `lambdamono` fact planner.

It consumes lambda-solved MIR.

It owns:

- executable representation of callables
- direct calls
- erased calls
- finite callable-set value construction
- finite callable-set call lowering to explicit `when`
- packed erased function values
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
- erasure decision
- lambda-set inference

Executable MIR is the final post-check executable representation consumed by IR.

Executable MIR converts lambda-solved `call_proc`, `proc_value`, and `call_value`
nodes into executable calls or executable function values. A source/MIR procedure
target may become:

- `call_direct` when the target executable specialization and argument
  representation are exact
- `call_erased` when lambda-solved MIR explicitly requires erased representation
- an explicit bridge plus one of the above when source and executable
  representations differ
- `callable_set_value` when the procedure is used as a non-erased value
- `packed_erased_fn` when lambda-solved MIR explicitly requires erased function
  representation

Captured non-erased callables are callable-set values with capture payloads.

Captured non-erased callables are not packed erased function pointers.

Capture presence alone must never cause erased packing.

This conversion must use only lambda-solved MIR metadata and executable MIR's own
specialization queue. It must not inspect checked CIR, method registries, source
syntax, or expression-derived facts.

Finite callable-set calls are mandatory lowering, not an optimization.

When lambda-solved MIR says a `call_value` callee has a finite non-erased
callable set, executable MIR must lower the call to an explicit `when` over the
callable-set representation:

1. Evaluate the callable value.
2. Branch on its callable member tag.
3. In each branch, destructure that member's capture payload if it has one.
4. Emit a `call_direct` to the executable specialization for that member.
5. Pass the original call arguments plus the explicit capture argument required
   by that member's executable specialization.
6. Insert explicit bridges when branch result representations differ from the
   call's required executable result type.

This finite callable-set `when` lowering is required for correctness. Executable
MIR must not replace it with erased calls, indirect calls, fallback dispatch, or
source-method lookup unless lambda-solved MIR explicitly says the callable is
erased.

Its AST may contain:

```text
call_direct
callable_set_value
packed_erased_fn
call_erased
when
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
references for `call_proc`, `proc_value`, and executable direct calls. A stage
may use private dense procedure ids internally, but exported call nodes must
refer to stable symbols.

The required invariant is:

```text
procedure call target identity is stored in `call_proc`
executable direct call target identity is stored in `call_direct`
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

### Proc Calls, Direct Calls, And Value Calls

MIR must distinguish procedure-symbol calls, executable direct calls, and
function-value calls.

`call_proc` and `proc_value` are MIR contract terms, not `cor` AST concepts.

In the `cor` prototype, the equivalent of `proc_value` is just a `Var(proc)`
whose symbol has already been specialized. The equivalent of `call_proc` is an
ordinary `Call(Var(proc), arg)`.

That cor call remains ordinary until lambda-set solving and executable lowering.

Production MIR names this case explicitly so later stages do not recover the
procedure target from environment lookup, expression shape, or source syntax.

Required pre-executable distinction:

```zig
proc_value {
    proc: Symbol,
    fn_ty: TypeId,
}

call_proc {
    proc: Symbol,
    args: Span(ExprId),
    requested_fn_ty: TypeId,
}

call_value {
    func: ExprId,
    args: Span(ExprId),
    requested_fn_ty: TypeId,
}
```

`proc_value` exists when a source/MIR procedure symbol is used as a value,
including a resolved method reference that is not immediately called.

`call_proc` exists in mono MIR, lifted MIR, and lambda-solved MIR.

`call_proc.proc` is a source/MIR procedure identity selected by earlier stages.
It is not an executable procedure identity and it does not imply an executable
argument representation.

`call_proc.requested_fn_ty` is the exact stage-local source/callable function
type used for this call. It is mandatory even though the call expression itself
also has a result type.

When mono MIR enters lambda-solved MIR, every mono `requested_fn_ty` is
transformed into the lambda-solved callable type shape by inserting the
lambda-set slot owned by lambda-solved MIR.

Debug verifiers must assert, as early as possible, that:

- the call expression type is the requested function return type
- each call argument expression type is the corresponding requested function
  argument type
- `call_value.func` has a callable type that unifies with `requested_fn_ty`
- `call_proc.proc` has a callable type that unifies with `requested_fn_ty`
- `proc_value.fn_ty` unifies with the procedure target's callable type

These are debug-only compiler assertions. They should panic loudly on failure in
debug builds and verifier builds, because failure means a compiler invariant was
violated. They must not generate runtime checks in user programs, and release
compiler builds must not pay for them when those checks are unnecessary assuming
the compiler is correct.

Static dispatch lowers to `call_proc` in mono MIR.

Resolved method references used as first-class values lower to
`proc_value`, not `call_proc` and not `call_direct`.

Calling a first-class function value remains `call_value` until callable solving
and executable lowering can decide whether it becomes direct, erased,
callable-set `when`, packed-erased, or bridged.

Required executable distinction:

```zig
call_direct {
    proc: Symbol,
    args: Span(ExprId),
}

call_erased {
    func: ExprId,
    args: Span(ExprId),
    erased_fn_ty: ExecTypeId,
}
```

`call_direct` exists only in executable MIR, IR, and LIR.

A `call_direct` target must be an executable specialization whose procedure
definition is present in executable MIR. The verifier must check that the call's
argument expressions and result type match that procedure definition exactly.

The names and stage boundaries are semantic. They must not be collapsed.

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

The registry returns source method definition identity, not a final executable
procedure.

Mono MIR lowering must pass that source method definition and the exact requested
mono source function type through the mono specialization queue. The queue
returns the mono-specialized source/MIR procedure symbol stored in `call_proc`.

Executable MIR later creates executable specializations from lambda-solved MIR.
It must not reuse raw method registry symbols as executable direct-call targets.

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

The receiver expression's MIR type must be the result of the resolved callee's
requested mono function type, after the dispatch constraint has been unified with
the lowered argument types. A later dispatch in a chain must never re-infer the
receiver owner from source expression shape.

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
sets, that is a layout decision derived from explicit callable metadata.

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
5. Request or reserve the target mono specialization at the exact mono source
   constraint function type.
6. Lower `a` and `b` with expected types `constraint.args[1..]`.
7. Emit `call_proc` with:
   - `proc` equal to the mono-specialized source/MIR procedure symbol
   - `args` equal to the lowered receiver followed by lowered explicit args
   - `requested_fn_ty` equal to the exact mono source constraint function type

No later stage sees the method name as an unresolved call.

No stage treats this as an executable direct call until executable MIR.

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
5. Request or reserve the target mono specialization at the exact mono source
   constraint function type.
6. Lower explicit args.
7. Emit `call_proc` with:
   - `proc` equal to the mono-specialized source/MIR procedure symbol
   - `args` equal to the lowered explicit args
   - `requested_fn_ty` equal to the exact mono source constraint function type

There is no receiver argument for type dispatch unless the checked constraint
itself has one.

### Nominal Equality

For nominal/custom equality:

```roc
x == y
x != y
```

mono MIR lowering resolves `is_eq` through the same owner and registry path.

`==` emits `call_proc` to the specialized `is_eq`.

`!=` emits the same `call_proc` followed by `bool_not`.

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

Callable and capture metadata must flow as typed MIR data, not side tables.

Mono MIR:

- preserves function values and procedure-symbol calls distinctly
- assigns monomorphic function types to expressions
- stores the exact requested mono source function type on every `call_proc` and
  `call_value`
- does not package erased callables

Lifted MIR:

- lifts local functions and closures
- computes captures
- stores captures in lifted procedure metadata
- rewrites `call_proc` and `proc_value` targets only through explicit procedure-id
  maps

Lambda-solved MIR:

- determines exact callable sets
- associates capture types with callable members
- propagates erasure requirements
- treats `call_proc` as calls to specialized `Var(proc)` targets for inference
  and SCCs

Executable MIR:

- builds capture records
- emits callable-set values for non-erased callable values
- emits finite callable-set `when` expressions for non-erased callable calls
- emits `packed_erased_fn` only for explicitly erased callable values
- emits `call_erased`
- emits `call_direct` where executable targets are exact
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
proc_value
call_proc
call_value
structural_eq
bool_not
```

Mono MIR lowering from checked CIR must resolve:

- ordinary dispatch
- type dispatch
- nominal/custom equality

to `call_proc` calls immediately.

Commit when mono MIR verification proves no exported mono MIR dispatch nodes
exist.

### 4. Harden Lifted MIR

Update lifted MIR to consume dispatch-free mono MIR.

Delete all dispatch cases from lifted MIR AST and lowering.

If procedure ids or symbols change during lifting, add explicit rewrite maps for
`call_proc` and `proc_value` targets.

Commit when lifted MIR verification proves:

- all `call_proc` and `proc_value` targets exist
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
- `call_proc` and `proc_value` targets remain explicit
- `call_proc` and `proc_value` participate in callable inference and SCC ordering

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

Executable specialization keys must be:

```text
source/MIR procedure target
+ lambda-solved argument and return types
+ exact callable-set capture payload shape or erased capture record shape
+ representation mode
```

not:

```text
fact handles + expression ids + source/executable side channels
```

Commit when executable MIR verification proves:

- direct calls have explicit targets
- direct call args match target signatures
- erased calls have explicit erased function types
- callable-set values have explicit member capture payloads
- finite callable-set calls lower to explicit `when`
- packed erased functions have explicit captures
- bridge nodes connect concrete executable MIR types

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
- mono MIR contains `call_proc` and no dispatch
- lifted MIR contains explicit captures and no dispatch
- lambda-solved MIR contains explicit callable sets and no dispatch
- executable MIR contains direct/erased calls, finite callable-set `when`
  lowering, packed erased functions, bridges, and no fact side tables
- IR/LIR contain direct/erased calls only

## Required Structural Tests

Add MIR-family verification tests for each stage.

Mono MIR:

- every expression has a mono type
- no dispatch nodes exist
- static dispatch becomes `call_proc`
- nominal/custom equality becomes `call_proc` to `is_eq`
- structural equality remains structural
- transparent aliases preserve nominal identity
- `proc_value` is distinct from `call_proc`
- `call_proc` carries exact requested mono source function types
- `call_proc` is not an executable direct call

Lifted MIR:

- every lifted procedure target exists
- every capture is explicit
- `call_proc` and `proc_value` targets are rewritten by explicit maps
- no dispatch nodes exist

Lambda-solved MIR:

- callable sets are explicit
- captures are attached to callable members
- erasure requirements are explicit
- no dispatch nodes exist
- `call_proc` and `proc_value` have SCC dependency edges
- `call_proc` is inferred as a call to its procedure target type
- `proc_value` is inferred as a value of its procedure target type

Executable MIR:

- every direct call target exists
- direct call arg/result types match signatures
- every finite callable-set call lowers to an explicit `when`
- every callable-set value has explicit member capture payload metadata
- every packed erased function has explicit capture metadata
- every erased call has an explicit erased function type
- bridges connect concrete executable types
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
- chained dispatch does not call any expression-based owner resolver
- type-var alias dispatch resolves from the specialized alias target type
- primitive methods resolve through builtin primitive owners
- list methods resolve through builtin List owner
- box methods resolve through builtin Box owner
- custom equality lowers to `call_proc` `is_eq` before executable MIR and direct
  executable `is_eq` after executable MIR
- inequality lowers to `call_proc` `is_eq` plus `bool_not` before executable MIR
  and direct executable `is_eq` plus `bool_not` after executable MIR
- anonymous record equality remains structural equality
- transparent tag-union aliases resolve through nominal identity
- cross-module attached methods resolve through registry def refs
- recursive and mutually recursive methods use reserved specialized proc ids
- hosted/effect/platform methods use explicit method targets or explicit
  intrinsics
- tag construction never creates singleton source tag-union types
- method references used as first-class values resolve to explicit `proc_value`
  values, not executable direct calls

Callable/capture behavior:

- direct top-level function call
- generic top-level function specialization
- local closure with no captures
- local closure with captures
- recursive local function
- mutually recursive local functions
- closure returned from a function
- closure passed as an argument
- exact callable becomes executable direct call without erased packaging
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
- mono MIR output uses `call_proc`, not `call_direct`, for resolved static
  dispatch
- lifted MIR output has no dispatch nodes
- lifted MIR rewrites `call_proc` and `proc_value` targets only through
  explicit maps
- lambda-solved MIR output has no dispatch nodes
- lambda-solved MIR has explicit callable/lambda-set/erasure metadata for every
  `proc_value`, `call_proc`, and `call_value` executable MIR consumes
- executable MIR output has no dispatch nodes
- executable MIR is the first stage that emits `call_direct`
- executable MIR lowers every finite non-erased callable-set call to explicit
  `when`
- executable MIR keeps callable-set values distinct from packed erased function
  values
- requested function type verifiers are debug-only compiler assertions and do not
  add runtime checks to user programs
- no source/executable fact side tables remain
- no expression-based method owner resolver remains
- no syntax-derived source type reconstruction remains
- method registry is consumed only by mono MIR construction
- IR lowering consumes executable MIR only
- LIR/backends do not know about source methods
- semantic audits forbid the deleted families
- guarded eval and glue gates pass
