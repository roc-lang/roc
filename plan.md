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

Roc functions have fixed arity. Roc functions are not automatically curried.

This is a hard semantic rule for every stage in this plan.

In Roc type syntax:

```roc
Str, Str -> Str
```

means one function that takes exactly two arguments. It does not mean:

```roc
Str -> (Str -> Str)
```

That is a different type: one argument, returning a function value.

A call to a fixed-arity function must provide exactly that function's full
argument count. The compiler must not synthesize partial-application closures,
curried call chains, or missing-argument wrappers unless Roc source syntax
explicitly constructs a function value that returns another function.

The cor prototype uses curried unary functions. This plan borrows cor's
lambda-set architecture, not cor's function arity model.

Every Roc source snippet in this plan must use Roc syntax, not Haskell syntax
and not cor prototype syntax.

Valid Roc examples:

```roc
id : a -> a
id = |x| x

plus_one : I64 -> I64
plus_one = |n| n + 1

same_plus_one = id(plus_one)
four = same_plus_one(3)
```

Function application is parenthesized and comma-separated:

```roc
foo(a, b)
id(foo(1, 2))
```

Do not use Haskell-style run declarations, backslash-arrow lambdas, or
whitespace function application in Roc examples.

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
- `proc_value` values for top-level procedure values with empty captures
- structural equality
- source tag names and full monomorphic tag-union types
- fixed-arity function types and calls

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
- captured procedure values
- automatically curried calls
- compiler-synthesized partial application

Every expression in mono MIR has a mandatory `MonoTypeId`.

Every mono MIR function type stores an explicit ordered parameter list and an
explicit result type. A function's arity is the length of that parameter list.
Function arity is not recovered from nested unary function types.

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
- captures are explicit `CaptureSlot` procedure metadata
- references to captured values are explicit `capture_ref` expressions
- procedure values carry explicit `CaptureArg` payloads in capture-slot order
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
- represent a procedure value as a bare `var_` expression

If lift changes procedure identities, it must rewrite `call_proc` and
`proc_value` targets through an explicit procedure-id map. It must not
recover targets from symbol names.

After lifted MIR, a symbol that names a procedure definition may appear in
expressions only as:

- `call_proc.proc`
- `proc_value.proc`

It must not appear as ordinary `var_` data. This prevents later stages from
recovering procedure identity from environment lookup or expression shape.

Mono MIR `call_proc` must target only top-level mono-specialized procedures.

Local functions and closures must remain value calls until after lifting has made
captures explicit.

Do not implement pre-lift `call_proc` to local functions. That path requires
target rewriting plus capture-path synthesis during lifting, and it creates an
avoidable second representation for the same callable flow.

After lifting, local functions and closures are still called through
`call_value` of explicit `proc_value` expressions. They do not become
`call_proc`.

This is true even for captureless local functions. Direct-call optimization is
not a representation invariant.

Recursive local-function groups are lifted by fixed point, not by one-pass
body scanning.

For each recursive local-function group, lifted MIR must:

1. Allocate stable procedure symbols for every group member before lowering any
   member body.
2. Assign a stable `ProcOrderKey` to every group member before lowering any
   member body.
3. Compute each member's `CaptureSlot`s as the least fixed point of:
   - external values directly referenced by that member body
   - external values needed to construct any referenced self or sibling
     `proc_value`
   - capture slots pulled through references to self or sibling procedures
4. Rewrite every body reference to a captured value to `capture_ref(slot)`.
5. Rewrite every self, sibling, local-function, and closure reference to an
   explicit `proc_value`.
6. Fill each `proc_value.captures` from values in the current scope or from the
   current procedure's own `CaptureSlot`s.
7. Iterate until every member's capture-slot set is stable.

Failure to converge is a compiler invariant failure. It is not a fallback path.

Capture slot ordering inside a recursive group must be deterministic. The base
order is lexical capture discovery. Fixed-point additions are appended in
`ProcOrderKey` order, then by lexical reference order inside that procedure.
Capture slot ordering must not depend on hash-map iteration, symbol allocation
order, pointer identity, or body traversal accidents.

After lifted MIR, no local alias variable may stand for a procedure value. An
alias of a local function or closure must be rewritten to an explicit
`proc_value`, or it must be an ordinary non-procedure value.

After lifted MIR, no captured source symbol may appear as an ordinary `var_`
inside the lifted procedure body. Captured values are read only through
`capture_ref(slot)`, where `slot` indexes the current procedure's
`CaptureSlot` metadata.

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

Lambda-solved MIR output must make callable metadata explicit in its types,
procedure metadata, and `proc_value` capture payloads. The executable MIR stage
must not have to rediscover it.

The lambda-solved type store is the single source of callable representation for
executable lowering.

Executable MIR must consume the zonked callable representation attached to:

```text
call_value.requested_fn_ty
call_proc.requested_fn_ty
proc_value.fn_ty
```

It must not derive a callable member set from `proc_value` syntax, environment
lookup, body scanning, source function types, or the local shape of a callee
expression.

`call_proc` participates in lambda-set inference as a direct procedure call. It
is not a first-class procedure value and it carries no captures.

`proc_value` participates in lambda-set inference as a first-class procedure
value. Its `proc` and `captures` fields are the only source of truth for the
callable member and capture payload.

For every `call_proc`, lambda-solved MIR must:

- add an SCC dependency edge to the procedure target
- instantiate the procedure target's callable type
- unify the procedure target type with the requested callable type
- verify the call supplies exactly the requested fixed-arity parameter list
- preserve the procedure target identity for executable MIR

For every `call_proc`, lambda-solved MIR must also unify the call arguments and
result with the procedure target type.

For every `proc_value`, lambda-solved MIR must:

- add an SCC dependency edge to the procedure target
- instantiate the procedure target's callable type
- unify `proc_value.fn_ty` with the instantiated procedure target type
- unify every `CaptureArg.expr` type with the corresponding target
  `CaptureSlot.ty`
- preserve the procedure target identity and capture slot order for executable
  MIR

These rules are mandatory. A `call_proc` must not bypass lambda-set solving
merely because its source procedure target is already explicit.

This is also mandatory for `proc_value`. A `proc_value` may become a
callable-set value or packed erased function value later, but lambda-solved MIR
must own that decision.

Lambda-solved callable sets are canonical finite maps:

```text
procedure symbol -> capture slots with capture types
```

Unification of two callable sets is exact:

1. Different procedure members union into one finite set.
2. The same procedure member must have the same capture slots.
3. The same capture slot unifies its capture type pointwise.
4. Mismatched capture slots for the same procedure member are compiler invariant
   failures.
5. A callable set unified with erased callable representation becomes erased.

This algebra is not an optimization. It is the exported lambda-solved contract
that executable MIR consumes.

For every `call_value`, lambda-solved MIR must export a complete zonked callable
representation through `call_value.requested_fn_ty`.

For every `call_value`, lambda-solved MIR must verify that the call supplies
exactly the fixed-arity parameter list of `call_value.requested_fn_ty`.

If that representation is a finite non-erased callable set, executable MIR must
treat every member as an executable specialization dependency of that call. If
that representation is erased, executable MIR must lower the call as an erased
call. Executable MIR must not inspect the callee expression to choose between
finite and erased representation.

Lambda-solved builder internals may use solver links, unbound variables, and
generalized variables while solving. Exported lambda-solved MIR must expose a
zonked view for every executable specialization input. Generalized variables may
remain only in specialization templates that are explicitly instantiated before
executable lowering consumes them.

Generalized variables may appear only in procedure specialization templates.
Before executable MIR consumes a procedure, call, or callable value, the
template must be clone-instantiated into a specialization-local lambda-solved
type store and zonked. No exported executable specialization key, executable MIR
type, callable-set member, capture type, bridge endpoint, or erased function
type may contain `for_a`, `flex_for_a`, `unbd`, unresolved links, or raw
checker variables.

Representation solving is also specialization-local.

Lambda-solved MIR must not use `TypeId` as representation identity.

Lambda-solved MIR owns an explicit `RepresentationStore`:

```zig
const RepRootId = union(enum) {
    expr: ExprId,
    binder: BinderId,
    pattern_binder: PatternBinderId,
    proc_param: struct { proc: Symbol, index: u32 },
    proc_return: Symbol,
    capture_slot: struct { proc: Symbol, slot: CaptureSlot.Index },
};

const RepVarId = enum(u32) { _ };
const RepEdgeId = enum(u32) { _ };
const RepClassId = enum(u32) { _ };

const RepresentationStore = struct {
    roots: Map(RepRootId, RepVarId),
    vars: Store(RepresentationVar),
    edges: Store(RepresentationEdge),
    classes: Store(SolvedRepresentationClass),
};
```

The exact Zig field names may differ, but the identity model must not.

Every expression result, binder, pattern binder, procedure parameter,
procedure return, and capture slot gets its own representation root before
representation requirements are solved. Structural children are also explicit
representation variables: record fields, tuple elements, tag payloads,
`List(T)` elements, `Box(T)` payloads, nominal backing slots, function
arguments, function returns, and callable-representation slots.

A representation variable may reference a zonked logical `TypeId` as a checked
type fact, but that `TypeId` is not the representation variable's identity. Two
different roots with equal logical types remain different representation
variables until an explicit value-flow edge unifies them. This is required so
two unrelated values with the same type do not accidentally share erased
representation. A shared value does share representation because `let`, use,
parameter, return, capture, and projection edges explicitly connect the same
value flow.

`BoxPayloadRepresentationPlan`, callable representation, capture shape keys,
erased function signature keys, erased adapter keys, and executable callable
member keys must be produced only from the specialization-local lambda-solved
store after clone-instantiation and zonking.

A boxed use in one specialization must not mutate:

- the generalized procedure template
- another specialization's lambda-solved type store
- another specialization's boxed payload representation plan
- another specialization's callable/capture keys

Generalized templates may contain unsolved representation variables while they
are still templates. Exported executable inputs may not. The verifier must panic
if any exported `BoxPayloadRepresentationPlan`, `CanonicalCallableSetKey`,
`CaptureShapeKey`, `ErasedFnSigKey`, `ErasedAdapterKey`, executable MIR type, or
layout-publication input references a template `TypeId`, a foreign
specialization's type store, `for_a`, `flex_for_a`, `unbd`, unresolved links, or
raw checker variables.

Executable specialization keys must be canonical structural keys after zonking.
They must not contain raw type-store ids from a transient clone. Procedure
members in those keys are ordered by `ProcOrderKey`. Capture components are
ordered by `CaptureSlot.index`.

Every type transform that contributes to executable MIR, executable
specialization keys, boxed payload representation, or layout publication must be
cycle safe. This includes:

```text
erased_box_payload_type(T)
canonical lambda-solved type keys
canonical callable-set keys
canonical capture-shape keys
canonical erased function signature keys
canonical erased adapter keys
canonical executable type keys
boxed payload representation plans
executable type lowering
layout graph construction
```

These transforms must be graph transforms, not recursive tree copies. They must
memoize by source type plus transform mode, allocate placeholders before
recursing, and fill those placeholders after children have been transformed.
Transform modes include at least natural representation, boxed-erased-payload
representation, and executable representation.

Canonical key serialization for recursive types and solved representation
classes must emit stable recursion
binders and backrefs derived from first encounter order in the explicit
type/representation graph being serialized. It must not serialize raw `TypeId`,
pointer identity, allocation order, or hash-map iteration order. Debug
verifiers must reject any exported key that contains a transient type-store id
or can recurse forever.

The same rule applies to recursive callable and capture graphs.
`CanonicalCallableSetKey`, `CaptureShapeKey`, `ErasedAdapterKey`, and any key
that references captures must serialize recursion with stable binders/backrefs.
They must not inline callable members or capture records recursively until the
process bottoms out. A recursive closure that captures a value containing itself
must produce a finite canonical key.

Specialization queues must reserve the semantic key and output symbol before
lowering the procedure body. Re-entering an in-progress specialization returns
the already-reserved symbol and records the dependency edge; it must not create a
duplicate specialization, derive a new key from the partially-lowered body, or
fall back to expression ids.

`ProcOrderKey` may define canonical ordering inside a specialization key, but
it must not be a semantic component of the specialization key itself.

Erasure is decided here. Erasure is permitted only for `Box(T)`.

This rule is absolute. A non-boxed value must not acquire erased callable
representation merely because it is a function, record, tuple, tag union,
nominal, or `List(T)`. A non-boxed container is never an erased boundary.

Lambda-solved MIR must preserve explicit boxed-boundary facts for every erased
`Box(T)` boundary. Erasure is not a callable-only operation, but it is a
`Box(T)`-only operation.

The exported lambda-solved type contract includes:

```text
erased_box_payload_type(T)
require_box_erased(payload_root: RepRootId)
```

`erased_box_payload_type(T)` may be called only for the payload type of an
explicit `Box(T)` boundary. It recursively walks that boxed payload type and
rewrites every reachable function slot to erased callable representation.

`require_box_erased(payload_root)` may be created only for the payload root of
an explicit `Box(T)` boundary. It is a constraint seed in the
`RepresentationStore`, not an executable conversion. Solving that seed walks
the payload root's representation graph and marks every reachable function
representation slot as erased. The walk follows only explicit representation
edges already present in the store.

Any recursion through records, tuples, tag unions, `List(T)`, nested `Box(T)`,
or nominal backing types happens only because those types are inside the payload
of an explicit `Box(T)`. Those types are not themselves erasure boundaries.
Non-callable data is preserved structurally.

Nominal recursion is allowed only when lambda-solved MIR has an explicit
representation fact for the nominal backing.

Inside the module that defines a transparent nominal, the defining module's
checked type facts provide that representation fact. Outside the defining
module, nominal traversal is controlled by module-interface capability
templates:

```zig
BoxPayloadCapabilityTemplate {
    nominal: NominalKey,
    params: Span(TypeParam),
    backing: NominalBackingRepresentationTemplate,
}

const NominalPayloadRepresentation = union(enum) {
    transparent_backing: struct {
        nominal: NominalKey,
        backing_plan: *BoxPayloadRepresentationPlan,
    },
    imported_capability: struct {
        capability_key: BoxPayloadCapabilityKey,
        instantiated_args: Span(CanonicalTypeKey),
        backing_plan: *BoxPayloadRepresentationPlan,
    },
    opaque_atomic: struct {
        nominal: NominalKey,
        proof: NoReachableCallableSlotsProof,
    },
    hosted_abi: HostedRepresentationCapabilityKey,
    recursive_ref: RepresentationRecursionBinder,
};
```

The defining module owns `BoxPayloadCapabilityTemplate` values for exported
nominals. An importing module may instantiate a capability template only with
the exact specialization-local zonked type arguments and boxed-payload
representation mode being compiled. The instantiated capability becomes an
ordinary `NominalPayloadRepresentation.imported_capability` node inside the
importer's specialization-local `BoxPayloadRepresentationPlan`.

Opaque nominals are atomic outside their defining module only when the interface
exports an explicit `opaque_atomic` capability with a compiler-produced
`NoReachableCallableSlotsProof`. Otherwise an opaque nominal that appears inside
a boxed erased payload must be traversed through an imported capability. If no
capability exists for that exact boundary, compilation must fail before
executable MIR.

The compiler must not use copied opaque backing details, source syntax, display
names, or layout inspection as a substitute for the interface capability. If a
boxed erased boundary would require traversing an imported, opaque, hosted, or
platform-owned value without an explicit representation fact, compilation must
fail before executable MIR. It must not emit a runtime conversion, generic
opaque coercion, fallback erased wrapper, or best-effort indirect call.

Hosted and platform procedures must declare their callable-containing argument
and return representations as part of their explicit ABI metadata. A hosted
procedure that accepts, returns, stores, or exposes a value containing function
slots must state whether each slot is finite callable-set representation or
erased function representation, including the erased fixed-arity signature and
ownership contract for erased calls. Executable MIR must consume that metadata.
It must not infer hosted representation behavior from host symbol names,
argument layouts, or the body of user code around the hosted call.

This transform produces types and representation requirements. It does not
describe runtime traversal, runtime container conversion, or executable shape
repair.

Lambda-solved MIR must solve representation requirements through aliases,
binders, captures, function parameters, function returns, and expression
occurrences. The boxed erased payload type is not merely assigned to the final
`Box.box(...)` call result; it must be propagated to the payload producer and all
uses that share that value.

The `RepresentationStore` must contain explicit edges for all constructs that
can move, bind, project, join, or return a boxed payload value:

- `let` and `var` binders connect the bound expression representation to every
  use of the binder.
- procedure parameters connect the caller's argument representation to the
  callee's instantiated parameter representation.
- procedure returns connect every returned expression to the instantiated
  procedure return representation.
- `capture_ref(slot)` connects to the instantiated `CaptureSlotInstance.ty`.
- `proc_value.captures[i]` connects to the instantiated target
  `CaptureSlotInstance[i].ty`.
- records connect each field expression to the checked logical field type.
- tuples connect each element expression to the checked logical element type.
- tag construction connects each payload expression to the checked logical tag
  payload type for that constructor.
- tag payload access connects the projected payload representation to the
  checked logical tag payload type and payload index.
- `List(T)` literals and builders connect every element expression to the list
  element representation.
- tuple and record access connect the projected value to the checked logical
  element or field representation.
- `if` and source `match` result requirements connect the whole expression
  representation to every branch result.
- source `match` condition requirements connect the matched value
  representation to every pattern.
- pattern tag payload requirements connect the matched tag payload
  representation to every nested payload pattern.
- pattern variable binders connect the pattern's representation to every use of
  that binder.
- `return` connects the returned expression to the current procedure return
  representation.
- `for` pattern binders connect to the iterable element representation.

These edges are lambda-solved facts. Executable MIR may verify them in debug
builds, but it must not add missing edges, rebuild containers, or reinterpret a
branch/pattern result to satisfy a boxed payload requirement.

`Box.box(payload)` creates a `boxed_erased_boundary` and a
`require_box_erased(payload_root)` seed. The produced box root's payload child
is linked to the solved boxed payload representation class. `Box.unbox(boxed)`
creates a `boxed_erased_boundary` whose box root is the boxed input and whose
payload root is the unboxed expression result. It links the unboxed payload root
to the explicit boxed payload representation. It does not recover or request
the original finite callable-set shape.

The required representation algebra is:

```text
finite callable set + finite callable set = canonical finite callable-set union
finite callable set + erased callable = erased callable
erased callable + erased callable = erased callable with exactly matching erased signature
```

If a shared value flows both to an ordinary use and to `Box(...)`, the explicit
`Box(T)` boundary is the only source of erasure, but lambda-solved MIR may solve
the shared callable slots to erased representation. The ordinary uses must then
consume that solved erased representation. Executable MIR must not create a
runtime `List(T)` map, record rebuild, tuple rebuild, tag rebuild, or nominal
rebuild to make a previously-produced non-erased container fit the box.

Every boxed erased boundary exports:

```zig
boxed_erased_boundary {
    direction: BoxErasureDirection,
    input: ExprId,
    box_root: RepRootId,
    payload_root: RepRootId,
    box_ty: TypeId,
    payload_source_ty: TypeId,
    payload_boundary_ty: TypeId,
    payload_plan: BoxPayloadRepresentationPlan,
}
```

For boxing, `input` is the payload expression. For unboxing, `input` is the boxed
expression. `box_root` is the representation root of the produced or consumed
box. `payload_root` is the representation root of the payload expression being
boxed or the unboxed expression result. `box_ty`, `payload_source_ty`, and
`payload_boundary_ty` are zonked lambda-solved types. `box_ty` must be exactly
`Box(payload_boundary_ty)`. `payload_boundary_ty` is the explicit erased
representation of the boxed payload. `direction` records whether this boundary
boxes or unboxes. `payload_plan` records the compile-time representation
requirement for the boxed payload.

`BoxPayloadRepresentationPlan` is explicit lambda-solved data:

```zig
const BoxPayloadRepresentationPlan = union(enum) {
    identity,
    record: Span(FieldPayloadRepresentation),
    tuple: Span(ElemPayloadRepresentation),
    tag_union: Span(TagPayloadRepresentation),
    list: *BoxPayloadRepresentationPlan,
    nested_box: *BoxPayloadRepresentationPlan,
    nominal: NominalPayloadRepresentation,
    callable_leaf: CallableBoxPlan,
};

const CallableBoxPlan = union(enum) {
    already_erased: ErasedFnSigKey,
    proc_value_to_erased: ProcValueErasePlan,
    finite_set_to_erased_adapter: ErasedAdapterKey,
};
```

The exact Zig shape may differ, but the semantics must not. Structural nodes in
`BoxPayloadRepresentationPlan` are expected-representation propagation facts.
They are not executable runtime conversions. Only a `callable_leaf` may cause
executable MIR to pack a callable or synthesize an erased adapter, and only while
lowering a value occurrence whose enclosing root is an explicit `Box(T)`
boundary.

For boxing, executable MIR lowers the payload expression under
`payload_boundary_ty` and consumes the payload plan as follows:

- `proc_value -> erased` packs the explicit procedure member and explicit
  capture payloads.
- `finite callable-set value -> erased` synthesizes an erased adapter procedure
  that captures the finite callable-set value. The adapter body dispatches with
  `callable_match`.
- `already-erased value -> erased` passes through after verifying the erased
  function type matches exactly.
- structural payload components are already constrained by lambda-solved MIR to
  use the boxed erased representation.

For unboxing, executable MIR performs the low-level unbox and gives the payload
the explicit `payload_boundary_ty` representation. There is no
`opaque_to_boundary` coercion and no recovery of the original finite callable-set
shape. Later field access or call lowering must consume the erased callable
representation already present in `payload_boundary_ty`.

Executable MIR must not synthesize generic runtime traversal or conversion for
`List(T)`, records, tuples, tag unions, nominals, or any other non-`Box(T)`
container. If such a value contains functions inside a boxed payload, the
required erased representation must have been propagated by lambda-solved MIR to
the producers and uses of that boxed payload.

Executable MIR must not infer erasure from usage.

Erased adapters are first-class synthetic procedures.

```zig
ErasedAdapterKey {
    source_callable_set_key: CanonicalCallableSetKey,
    erased_fn_sig_key: ErasedFnSigKey,
    capture_shape_key: CaptureShapeKey,
}
```

An erased adapter's signature is the fixed-arity erased function signature from
the boxed payload boundary. It captures exactly the finite callable-set value, or
an explicit capture record containing that value. Its body receives the erased
boundary arguments, dispatches with `callable_match`, and calls each finite
member specialization using the same erased-boundary requested function type.
The erased adapter procedure will later receive a `CallableCallOwnershipKey`
from executable ownership solving for its body-level `callable_match`.
`ErasedAdapterKey` itself is reserved before executable ownership solving and
must not contain that later key. It must not contain expression ids, fact ids,
raw type ids, symbol freshening suffixes, or allocation-order-dependent data.

`ErasedFnSigKey` must include the canonical erased argument types, canonical
erased return type, fixed Roc arity, and erased-call ownership contract. The
ownership contract states which erased-call arguments are borrowed, consumed, or
retained by the result, and how the erased result is materialized. A later LIR or
backend stage must not recover erased-call ownership from the adapter body,
capture layout, host symbol, or runtime function pointer.

Finite callable-set calls also have a single explicit call ownership key:

```zig
CallableCallOwnershipKey {
    callable_set_key: CanonicalCallableSetKey,
    requested_fn_key: CanonicalTypeKey,
    arg_modes: Span(ArgOwnershipMode),
    result_contract: ProcResultContract,
}
```

This key is produced by executable ownership solving, not by branch-local
lowering. A `callable_match` has exactly one `CallableCallOwnershipKey`.
Every member specialization and erased adapter branch in that `callable_match`
must expose the same argument modes and result contract at the branch boundary.
If one member consumes an argument, the whole callable call consumes that
argument. Members that only borrow it receive an explicit branch-local bridge
from the normalized ownership state. A result may preserve an alias or borrow
relation only when every branch returns the same compatible relation; otherwise
the branch boundary must materialize a fresh result. Later stages must not infer
these contracts from branch bodies.

### Executable MIR

Executable MIR replaces the current `lambdamono` fact planner.

It consumes lambda-solved MIR.

It owns:

- executable representation of callables
- direct calls
- erased calls
- finite callable-set value construction
- finite callable-set call lowering to explicit `callable_match`
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
- erased callable shape compatibility decisions
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

If executable MIR needs to cross a boxed erased boundary, it consumes the
lambda-solved `boxed_erased_boundary` node and its
`BoxPayloadRepresentationPlan`. It must not compare executable source and target
shapes to decide whether erasure repair, adapter synthesis, or pass-through is
semantically required. Such comparisons are allowed only as debug-only
verification of the explicit boxed payload representation plan.

Executable MIR must reject any erased-boundary request whose root is not
`Box(T)`. Non-boxed `List(T)`, records, tuples, tag unions, functions, and
nominals are not erased-boundary roots.

When executable MIR lowers `call_proc`, it must still reserve or create the
target executable specialization from the lambda-solved procedure target and the
zonked requested callable type. It must lower and bridge every argument
explicitly. A `call_proc` may return ordinary data, finite callable-set values,
or erased callable values; direct-call lowering must not assume the result is
non-callable.

Finite callable-set calls are mandatory lowering, not an optimization.

When lambda-solved MIR says a `call_value` callee has a finite non-erased
callable set, executable MIR must lower the call to an explicit
`callable_match` over the callable-set representation:

1. Evaluate the callable value exactly once.
2. Evaluate the original call arguments exactly once, in source call order.
3. Bind the callable value and arguments to executable MIR temporaries.
4. Branch on the callable member tag.
5. In each branch, destructure that member's capture payload if it has one.
6. Reserve or enqueue the executable specialization for that member.
7. Emit a `call_direct` to the reserved executable specialization for that
   member.
8. Pass the original argument temporaries plus the explicit capture argument
   required by that member's executable specialization.
9. Insert explicit bridges when branch result representations differ from the
   call's required executable result type.

The `callable_match` node must represent the whole call, not only the branch
switch. It owns the callable expression, the original argument expressions, the
callable-set branches, and the required executable result type. Branch bodies
consume the temporaries created by the node. They must not duplicate, reorder,
or rediscover the original arguments.

Every `callable_match` branch must store:

```zig
CallableBranch {
    member_proc: Symbol,
    member_tag_index: u32,
    capture_payload: ?TypedSymbol,
    direct_proc: Symbol,
    direct_args: Span(ExprId),
    body: ExprId,
}
```

`member_proc` is the lambda-solved callable-set member. `direct_proc` is the
reserved executable specialization used by the branch body. They are related by
the executable specialization queue, not by name lookup.

The executable calling convention for a callable-set member is:

```text
all fixed-arity source call arguments in source order
+ optional trailing capture-record argument when the member has captures
```

`direct_args` is the exact argument list supplied to the branch `call_direct`.
It must be the original argument temporaries, followed by the destructured
capture payload temporary when `capture_payload` is present. Branch `body` may
wrap that direct call in bridges, but it must not add, remove, duplicate, or
reorder direct-call arguments.

`callable_match` is not a curried-call loop. It dispatches one fixed-arity Roc
call. Every branch must call a member specialization whose source-argument arity
matches the original `call_value.requested_fn_ty`.

No branch may emit a `call_direct` to a procedure that has not been reserved or
created by executable MIR. By the end of executable MIR, every `direct_proc`
referenced by a `callable_match` branch must have a procedure definition in the
executable MIR program.

A singleton finite non-erased callable set still lowers to `callable_match`.
Only `call_proc` lowers directly to executable `call_direct`.

This finite callable-set `callable_match` lowering is required for correctness.
Executable MIR must not replace it with erased calls, indirect calls, fallback
dispatch, direct singleton calls, or source-method lookup unless lambda-solved
MIR explicitly says the callable is erased.

The cor prototype calls this construct `when`. Roc renamed that source keyword
from `when` to `match` after cor was built. Production user-facing terminology
and printed control flow must use `match`.

The executable MIR node is named `callable_match` so verifiers can distinguish
callable-set dispatch from ordinary source `match` expressions. An ordinary
source `match` does not satisfy the finite callable-set lowering requirement.

Callable-set member tag assignment must be deterministic.

The member ordering key is the canonical lambda-solved callable-set member
order. That order is by stable `ProcOrderKey`.

The ordering key must be explicit and stable across builds. It must not depend
on `Symbol.raw()`, hash-map iteration order, allocation order, pointer identity,
display names, fresh-symbol suffixes, or incidental lowering traversal order.

Capture payload field ordering must also be deterministic for every callable-set
member and every erased capture record.

Capture payload and erased capture record field order is the lifted MIR
`CaptureSlot.index` order. Executable MIR must consume that order. It must not
sort captures by name, scan procedure bodies, or inspect environments to rebuild
capture order.

Its AST may contain:

```text
call_direct
callable_set_value
packed_erased_fn
call_erased
match
callable_match
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

Executable MIR `match` and `callable_match` lower to IR branch/switch control
flow.

It must not lower to a call-like fallback operation, erased-call fallback,
indirect-call fallback, source dispatch operation, or method operation.

### Executable Ownership Solve

Executable ownership solving runs after executable MIR has reserved every user
procedure specialization, lifted-local specialization, erased adapter, intrinsic
wrapper, bridge procedure, entry wrapper, and hosted procedure target, and
before executable MIR lowers to IR.

This stage consumes:

- executable MIR
- explicit hosted/platform/intrinsic ABI metadata
- explicit low-level ownership metadata
- callable-set member metadata
- erased function signature keys
- bridge plans

It produces:

- `ProcOwnershipContract` for every executable procedure
- `CallableCallOwnershipKey` for every `callable_match`
- node-local ownership semantics for every value-producing executable MIR node
- bridge ownership semantics
- boxed-boundary ownership semantics

`ProcOwnershipContract` contains the owned/borrowed/consumed mode for every
parameter, the result contract, and any retained-borrow relation needed by
`RcInsert`. Recursive procedure groups are solved by fixed point over the
executable procedure dependency graph. Hosted, platform, intrinsic wrapper, and
low-level contracts are explicit inputs to the fixed point; they are not inferred
from names, layout shapes, runtime function pointers, or backend behavior.

Executable ownership solving is the only stage that may discover semantic
procedure ownership. IR lowering and LIR lowering preserve these facts. In the
final architecture, any LIR ownership pass may only verify, normalize storage,
or translate already-produced ownership facts for `RcInsert`; it must not infer
whether a user procedure owns a parameter or whether a call result borrows,
aliases, or owns data.

### LIR And Backends

LIR consumes IR.

Reference counting is inserted before backends, as explicit LIR statements.

Backends consume LIR only. They must not import MIR, IR builder internals,
checked CIR, method registries, or reference-counting analysis.

LIR ownership is a fact pipeline, not a backend behavior.

Executable ownership solving and IR lowering must preserve enough ownership
information for `RcInsert` to emit explicit `incref`, `decref`, and `free`
statements. `RcInsert` is the only non-builtin stage that may turn those
ownership facts into concrete reference-counting statements. Backends and the
ordinary interpreter path must execute those explicit LIR statements
mechanically.

Every executable MIR node introduced by this plan must have an explicit
ownership contract before LIR:

- `call_direct` consumes, borrows, or returns values according to the callee's
  explicit procedure ownership contract.
- `call_erased` consumes, borrows, or returns values according to the
  `ErasedFnSigKey` ownership contract. It must not infer ownership from the
  runtime function pointer or capture layout.
- `callable_match` evaluates the callable and source arguments once, then gives
  every branch the same ownership state for those temporaries. Branches may
  consume an argument only when the whole call consumes it; branch-local capture
  destructuring must not duplicate ownership.
- `callable_set_value` and callable-set member payload construction must state
  whether each captured value is stored as an owned child or retained borrow.
- `packed_erased_fn` must state the ownership of its function pointer and
  capture payload. If it stores a finite callable-set value or capture record in
  the packed function, that stored value must be owned by the packed function.
- erased adapter procedures must expose ordinary procedure ownership contracts
  for their erased arguments, capture record, and return value.
- capture record construction must consume owned child values or retain borrowed
  child values explicitly, following the same aggregate ownership rules as
  records/tuples.
- `Box.box` must state how the payload is materialized into the boxed storage.
  If the payload is copied from a borrowed source, LIR ownership must retain the
  produced boxed payload explicitly.
- `Box.unbox` always borrows the box and materializes the payload as a copied
  result from borrowed boxed storage. It is not a consuming move-out operation.
  A consuming unbox would require a separate explicit builtin, low-level op, and
  ownership contract.
- bridge nodes must state whether they are direct aliases, copied values,
  freshly materialized aggregates, or consuming transformations.

Debug verification must panic before backend lowering if any refcounted layout
value produced by executable MIR or IR lacks ownership semantics needed by LIR,
or if any backend/imported path branches on refcounted layout shape except while
executing explicit LIR RC statements.

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

Every procedure definition produced by mono MIR or any later MIR-family stage
must carry three distinct identities:

```zig
ProcBaseKey {
    module_idx: u32,
    source_def_idx: ?CIR.Def.Idx,
    lexical_proc_path: Span(u32),
    owner_mono_specialization: ?MonoSpecializationKey,
    synthetic_origin: SyntheticOrigin,
}

SourceProcKey {
    module_idx: u32,
    source_def_idx: CIR.Def.Idx,
    lexical_proc_path: Span(u32),
}

MonoSpecializationKey {
    source_proc: SourceProcKey,
    requested_mono_fn_ty: CanonicalTypeKey,
}

ExecutableSpecializationKey {
    base: ProcBaseKey,
    requested_fn_ty: CanonicalTypeKey,
    exec_arg_tys: Span(CanonicalExecTypeKey),
    exec_ret_ty: CanonicalExecTypeKey,
    callable_repr_mode: CallableReprMode,
    capture_shape: CaptureShapeKey,
}

ProcOrderKey {
    base: ProcBaseKey,
    specialization_order_component: ?CanonicalOrderComponent,
}
```

The exact Zig field names may differ, but the separation must not.

`ProcBaseKey` is the stable semantic origin of a source, lifted, or synthetic
procedure. `lexical_proc_path` records the stable path to a nested/lifted
procedure inside its source definition. `owner_mono_specialization` is null for
ordinary top-level source procedures and is set for lifted local procedures whose
body and capture slots are produced inside a particular monomorphic owner
specialization. Two lifted local procedures with the same `source_def_idx` and
`lexical_proc_path` but different owning mono specializations are different
procedures.

`source_def_idx`, `lexical_proc_path`, and `owner_mono_specialization` are the
single source of truth for lifted-local procedure identity. Do not duplicate
lifted-local owner identity in `synthetic_origin`.

`synthetic_origin` distinguishes ordinary source/lifted procedures from
compiler-created non-source procedures such as erased adapters, intrinsic
wrappers, entrypoint wrappers, and bridges. It must carry payload keys, not only
a kind tag:

```zig
const SyntheticOrigin = union(enum) {
    none,
    erased_adapter: struct {
        source_callable_set_key: CanonicalCallableSetKey,
        erased_fn_sig_key: ErasedFnSigKey,
        capture_shape_key: CaptureShapeKey,
    },
    bridge: struct {
        from_exec_ty: CanonicalExecTypeKey,
        to_exec_ty: CanonicalExecTypeKey,
        reason: BridgeReason,
    },
    intrinsic_wrapper: struct {
        intrinsic_id: IntrinsicId,
        requested_fn_ty: CanonicalTypeKey,
    },
    entry_wrapper: struct {
        root_name: Ident.Idx,
        target_proc: ProcBaseKeyRef,
        target_fn_ty: CanonicalTypeKey,
    },
};
```

No synthetic procedure identity may be keyed only by display name, generated
symbol, expression id, fact id, or a payload-free origin kind.

All key-like fields above are canonical keys, not handles into mutable stores.
`CanonicalCallableSetKey` is the canonical ordered finite member map plus capture
slot shape and capture types. `ErasedFnSigKey` is the canonical fixed-arity
erased function argument and return signature plus erased-call ownership
contract. `CaptureShapeKey` is the canonical `CaptureSlot.index` ordered capture
layout and capture types.
`ProcBaseKeyRef` is a canonical reference to an already-keyed procedure.
`BridgeReason`, `IntrinsicId`, and entry wrapper root names must be stable enum
or source identities, never generated symbol text.

`ExecutableSpecializationKey` is the semantic key for executable specialization
deduplication. It contains `ProcBaseKey` and canonical zonked structural type
keys. It must not contain `ProcOrderKey`, raw type-store ids, expression ids,
fact ids, or allocation-order-dependent data.

`ProcOrderKey` is for deterministic ordering and reproducibility only. A
base-only `ProcOrderKey` may exist before executable specialization so lifted
recursive groups can order members deterministically. When an executable
specialization exists, the specialization-specific order component is derived
after the semantic specialization key exists. `Symbol` remains the public
procedure identity used by call nodes.

Callable-set member ordering, erased adapter ordering, recursive capture
fixed-point ordering, generated procedure emission order, and stable printed
output must use `ProcOrderKey`.

Semantic equality, specialization deduplication, and executable call target
selection must use `ProcBaseKey` plus canonical type/representation keys, not
`ProcOrderKey`.

They must not use:

```text
Symbol.raw()
symbol display names
fresh-symbol suffixes
hash-map iteration order
allocation order
pointer identity
incidental traversal order
```

Procedure definitions also carry an explicit implementation target:

```zig
const ProcTarget = union(enum) {
    user_proc: UserProcTarget,
    hosted_proc: HostedProcTarget,
    intrinsic_wrapper: IntrinsicWrapperTarget,
};

const UserProcTarget = struct {
    body_symbol: Symbol,
};

const HostedProcTarget = struct {
    host_symbol: Ident.Idx,
    dispatch_index: u32,
    representation_abi: ProcRepresentationAbi,
    ownership_contract_template: ProcOwnershipContractTemplate,
};

const IntrinsicWrapperTarget = struct {
    intrinsic_id: IntrinsicId,
    representation_abi: ProcRepresentationAbi,
    ownership_contract_template: ProcOwnershipContractTemplate,
};
```

`ProcTarget` is procedure metadata. `call_proc`, `proc_value`, and `call_direct`
still refer to the procedure's `Symbol`. Later stages read the target metadata
from the procedure definition; they must not rediscover whether a procedure is
user code, hosted code, or an intrinsic wrapper from names or source syntax.
Hosted and intrinsic targets must carry their representation ABI and ownership
contract templates here before mono MIR output is exported. Lambda-solved MIR,
executable MIR, IR, LIR, and backends must consume this metadata; they must not
recover it from method names, host symbol names, layout shapes, runtime function
pointers, or surrounding user code.

### Proc Calls, Direct Calls, And Value Calls

MIR must distinguish procedure-symbol calls, executable direct calls, and
function-value calls.

`call_proc` and `proc_value` are MIR contract terms, not `cor` AST concepts.

In the `cor` prototype, the equivalent of `proc_value` is just a `Var(proc)`
whose symbol has already been specialized. Cor's AST uses unary
`Call(Var(proc), arg)` because cor's prototype language is curried.

Roc does not use that call model. The Roc MIR equivalent of a direct source/MIR
procedure call is one fixed-arity `call_proc` node with all source arguments in
`args: Span(ExprId)`.

That cor call remains ordinary until lambda-set solving and executable lowering.

Production MIR names this case explicitly so later stages do not recover the
procedure target from environment lookup, expression shape, or source syntax.

Lifted MIR owns capture slot assignment:

```zig
CaptureSlot {
    index: u32,
    symbol: Symbol,
    ty: TypeId,
}

CaptureArg {
    slot: u32,
    symbol: Symbol,
    expr: ExprId,
}

capture_ref {
    slot: u32,
    ty: TypeId,
}
```

`CaptureSlot.index` is assigned exactly once during lifting. It is the stable
field order for callable-set capture payloads and erased capture records.

`capture_ref.slot` indexes the current procedure's `CaptureSlot` metadata. A
captured value inside a lifted procedure body must be represented by
`capture_ref`, not by a `var_` expression that later stages reinterpret through
an environment. Executable MIR lowers `capture_ref(slot)` to a logical field
read from the current procedure's capture record.

Generalized procedure templates contain capture slot templates:

```zig
CaptureSlotTemplate {
    index: u32,
    symbol: Symbol,
    ty: TypeId,
}

CaptureSlotInstance {
    index: u32,
    symbol: Symbol,
    ty: TypeId,
}
```

When lambda-solved MIR clone-instantiates a generalized procedure template for
executable lowering, it must instantiate the procedure's capture slot table once
and then type every `capture_ref(slot)` from that instantiated slot table. Slot
indexes remain logical slot indexes; only the slot types are cloned into the
specialization-local type store.

Every `proc_value.captures[i]` for a lifted procedure must correspond to
instantiated target slot `i`. The capture argument expression type must equal the
instantiated `CaptureSlotInstance.ty`. Later stages must not clone or infer a
capture type independently from an environment lookup, a procedure body scan, or
the expression stored in `CaptureArg.expr`.

Required pre-executable distinction:

```zig
proc_value {
    proc: Symbol,
    captures: Span(CaptureArg),
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

`proc_value` exists when a source/MIR procedure symbol is used as a value.

For a top-level source procedure value, `proc_value.captures` is empty.

Mono MIR must reserve mono specializations for both direct procedure calls and
procedure values.

When mono MIR lowers a direct source procedure call, it must request or reserve
the target mono specialization at the exact requested mono source function type
and store the returned mono-specialized procedure symbol in `call_proc.proc`.

When mono MIR lowers a top-level procedure symbol used as a first-class value,
it must request or reserve the target mono specialization at the exact requested
mono source function type and store the returned mono-specialized procedure
symbol in `proc_value.proc` with empty captures.

This is mandatory for generic procedures. An exported `proc_value` must never
point at a generic source procedure and rely on lambda-solved MIR, executable
MIR, environment lookup, or call-site syntax to choose the specialization later.
By the time mono MIR is exported, every `call_proc.proc` and every top-level
`proc_value.proc` must name a mono-specialized procedure definition or a reserved
mono-specialization work item that will be drained before the next stage
consumes the program.

The mono specialization queue is therefore closed over both:

- `call_proc` dependencies
- `proc_value` dependencies

The queue key is exactly `MonoSpecializationKey`. The queue must reserve the
output symbol before lowering the specialization body, so recursive references
to the same procedure value reuse the same symbol instead of constructing a
second specialization.

For a lifted local function or closure, `proc_value.captures` must contain one
`CaptureArg` for every target `CaptureSlot`, in slot order. Each `CaptureArg`
stores both the slot index and symbol so verifiers can catch stale rewrite maps
or reordered payloads immediately.

`call_proc` exists in mono MIR, lifted MIR, and lambda-solved MIR.

`call_proc.proc` is a source/MIR procedure identity selected by earlier stages.
It is not an executable procedure identity and it does not imply an executable
argument representation.

Mono MIR emits `call_proc` only when the callee has already been resolved to a
specific source/MIR procedure and the procedure is being called directly. Static
dispatch, type dispatch, and nominal equality lower to this form. A procedure
symbol used as a value lowers to `proc_value`. A call whose callee is any
non-direct callable expression lowers to `call_value`.

`call_proc` means a direct source/MIR procedure call. It never carries captures.
It must not be used for local functions or closures after lifting, including
captureless local functions.

`call_proc.requested_fn_ty` is the exact stage-local source/callable function
type used for this call. It is mandatory even though the call expression itself
also has a result type.

`call_proc.args.len` and `call_value.args.len` must exactly equal the arity of
their `requested_fn_ty`. A missing argument is a compile-time source error before
MIR export, not a request to synthesize a partial application. Extra arguments
are likewise invalid unless the source explicitly calls the result of a function
that returns another function.

When mono MIR enters lambda-solved MIR, every mono `requested_fn_ty` is
transformed into the lambda-solved callable type shape by inserting the
lambda-set slot owned by lambda-solved MIR.

Debug verifiers must assert, as early as possible, that:

- the call expression type is the requested function return type
- each `call_proc` and `call_value` has exactly the requested function arity
- each call argument expression type is the corresponding requested function
  argument type
- `call_value.func` has a callable type that unifies with `requested_fn_ty`
- `call_proc.proc` has a callable type that unifies with `requested_fn_ty`
- `proc_value.fn_ty` unifies with the procedure target's callable type
- every `proc_value.captures` entry corresponds to the same-index target
  `CaptureSlot`
- every `CaptureArg.expr` type matches the corresponding `CaptureSlot.ty`
- every `capture_ref.slot` exists in the current procedure's `CaptureSlot`
  metadata
- no captured source symbol appears as ordinary `var_` inside a lifted,
  lambda-solved, or executable procedure body
- no post-lift expression represents a procedure value as bare `var_`

These are debug-only compiler assertions. They should panic loudly on failure in
debug builds and verifier builds, because failure means a compiler invariant was
violated. They must not generate runtime checks in user programs, and release
compiler builds must not pay for them when those checks are unnecessary assuming
the compiler is correct.

Verifier checks must not mutate production compiler state.

If a verifier needs unification-like logic, it must run on a cloned scratch type
store or compare already-zonked/canonicalized types. The real stage lowering or
inference pass performs the real unifications. A verifier must never repair,
complete, or mask an invalid type relation in the live store.

Static dispatch lowers to `call_proc` in mono MIR.

Current Roc checked CIR has no receiver-bound static method value. A dotted
expression without arguments, such as `x.foo`, is checked field access, not a
static method reference. Static dispatch inputs are checked `e_dispatch_call`,
`e_type_dispatch_call`, and `e_method_eq` nodes only.

If future syntax introduces an unbound source method symbol as a value, mono MIR
must lower that resolved symbol to `proc_value` with empty captures. If future
syntax introduces a receiver-bound method value, mono MIR must lower it to an
explicit closure that captures the receiver. It must not encode receiver-bound
method values as empty-capture `proc_value` nodes.

Calling a first-class function value remains `call_value` until callable solving
and executable lowering can decide whether it becomes direct, erased,
callable-set `callable_match`, packed-erased, or bridged.

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

callable_match {
    func: ExprId,
    args: Span(ExprId),
    branches: Span(CallableBranch),
    result_ty: ExecTypeId,
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
const MethodOwner = union(enum) {
    nominal: NominalOwnerKey,
    primitive: PrimitiveOwner,
    list,
    box,
};

const NominalOwnerKey = struct {
    module_idx: u32,
    nominal_ident: Ident.Idx,
};

const MethodKey = struct {
    owner: MethodOwner,
    method_ident: Ident.Idx,
};

const MethodDefRef = struct {
    module_idx: u32,
    def_idx: CIR.Def.Idx,
};

const HostedProcRef = struct {
    module_idx: u32,
    host_symbol: Ident.Idx,
    dispatch_index: u32,
    representation_abi: ProcRepresentationAbi,
    ownership_contract_template: ProcOwnershipContractTemplate,
};

const MethodTarget = union(enum) {
    user_proc: MethodDefRef,
    hosted_proc: HostedProcRef,
    intrinsic: IntrinsicMethod,
};
```

`MethodTarget` is a mono MIR input contract only. It must not appear in lifted
MIR, lambda-solved MIR, executable MIR, IR, or LIR.

The registry maps:

```text
MethodKey -> MethodTarget
```

`MethodKey.owner` is semantic owner identity, not a display name and not an
expression shape. Nominal owners use the defining module and nominal identifier.
Primitive, `List`, and `Box` owners are explicit builtin owner cases. Type-var
aliases and transparent aliases must resolve to a `MethodOwner` before registry
lookup.

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

The registry returns checked method target identity and ABI metadata needed to
create a `ProcTarget`, not a final executable procedure.

Mono MIR lowering must pass that checked method target and the exact requested
mono source function type through the mono specialization queue or wrapper
synthesis path. The queue returns the mono-specialized source/MIR procedure
symbol stored in `call_proc`.

Before mono MIR output is exported, every callable method target must be
normalized to a procedure symbol with `ProcTarget` metadata:

- ordinary source methods become `ProcTarget.user_proc`
- hosted/platform methods become `ProcTarget.hosted_proc`
- first-class intrinsic method references synthesize a wrapper procedure and
  become `ProcTarget.intrinsic_wrapper`

The `ProcTarget` metadata must include representation ABI facts and ownership
contract templates for hosted, platform, and intrinsic-wrapper procedures before
mono MIR is exported. Static dispatch lowering must not leave behind a method
name or owner key for later ABI discovery.

An intrinsic may lower directly to executable `low_level` only when it is
strictly call-only and never appears as a first-class value. If an intrinsic can
flow as a value, mono MIR must synthesize an intrinsic wrapper procedure, emit
`proc_value` for the value, and let lambda-solved/executable MIR handle it like
any other procedure value.

Hosted procedures are valid procedure targets. They are not a fallback for
intrinsics, and later stages must not infer hosted behavior from names.

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

- fixed-arity function parameter lists
- return type
- lambda/callable members
- captures for each callable member
- erased callable representation when required
- boxed erased-boundary payload type transforms for boxing and unboxing

It must not use ordinary source tag unions as a hidden carrier for unresolved
static dispatch.

If physical layout later uses a tag-union-like representation for callable
sets, that is a layout decision derived from explicit callable metadata.

Lambda-solved MIR owns `erased_box_payload_type(T)`.

This transform is structural only inside an explicit `Box(T)` payload. It
recursively rewrites reachable function slots to erased callable representation
inside the boxed payload, including through nested records, tuples, tags,
`List(T)`, nested `Box(T)`, and nominal backing types when they are part of that
payload. It is the same contract for boxing and unboxing boundaries. Executable
MIR consumes the already-computed boxed payload boundary type and
`BoxPayloadRepresentationPlan`.

This transform is not a runtime conversion plan. It is a representation
requirement propagated by lambda-solved MIR. Any structural node in the plan
exists only to route the boxed payload requirement to nested callable leaves.

Calling this transform on a non-`Box(T)` root is a compiler invariant failure.

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
curried-call markers
partial-application markers
```

Executable type lowering consumes lambda-solved MIR types and metadata. It does
not inspect source CIR or reconstruct from expressions.

### Logical Layout Indices

MIR and IR use logical field indexes. Physical layout lowering may reorder
fields for representation efficiency, but it must preserve an explicit mapping
from logical index to physical offset.

Logical indexes include:

- `CaptureSlot.index`
- callable-set member capture payload fields
- erased capture record fields
- checked type-store record field indexes
- tuple fields
- checked type-store tag constructor indexes
- checked type-store tag payload field indexes
- compiler-generated struct fields

Executable MIR layout graph nodes must store field identity explicitly:

```zig
LayoutField {
    logical_index: u32,
    ty: ExecTypeId,
}
```

IR `make_struct`, `get_struct_field`, capture-record construction, and
callable-set payload construction must refer to logical indexes. If an
implementation uses slice position as a temporary representation, debug
verification must prove that `slice_index == logical_index` before lowering
continues.

LIR access lowering is the first place that may resolve logical indexes to
physical offsets through the layout store.

No compiler stage may recover a capture field, record field, or tag payload
position by sorting names, scanning bodies, or relying on physical layout order.

If a source-level family has a canonical order, that order must be stored as an
explicit checked or MIR fact before layout lowering consumes it. A later stage
may use source/display names only to look up that explicit index in the owning
type metadata. It must not sort names to create a new order.

## Static Dispatch Lowering

Static dispatch lowering only consumes checked static-dispatch nodes:

```text
e_dispatch_call
e_type_dispatch_call
e_method_eq
```

A dotted expression without arguments is checked field access. It is not an
unresolved static method value and must not be treated as one later.

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

The full mono MIR tag-union type must carry or reference the checked canonical
constructor order and payload field indexes. Later stages may translate those
logical indexes to executable layout indexes, but they must not create a new
constructor order by sorting names or scanning expressions.

## Callable And Capture Flow

Callable and capture metadata must flow as typed MIR data, not side tables.

Mono MIR:

- preserves function values and procedure-symbol calls distinctly
- assigns monomorphic function types to expressions
- preserves Roc fixed arity on every function type and call
- stores the exact requested mono source function type on every `call_proc` and
  `call_value`
- requires `call_proc.args.len` and `call_value.args.len` to match the requested
  function arity exactly
- represents top-level procedure values as `proc_value` with empty captures
- reserves mono specializations for top-level `proc_value` targets at the exact
  requested mono source function type
- drains the mono specialization queue across both `call_proc` and `proc_value`
  dependencies before exporting mono MIR
- does not package erased callables
- does not synthesize curried or partial-application functions

Lifted MIR:

- lifts local functions and closures
- computes captures
- computes recursive local-function captures by least fixed point
- assigns `CaptureSlot.index` values and stores captures in lifted procedure
  metadata
- rewrites captured value references in lifted bodies to `capture_ref(slot)`
- rewrites every lifted local-function or closure value to `proc_value` with
  explicit `CaptureArg` payloads
- rewrites aliases of local functions and closures to explicit `proc_value`
  nodes
- rewrites every call through a lifted local function or closure to `call_value`
- rewrites `call_proc` and `proc_value` targets only through explicit procedure-id
  maps
- forbids bare procedure-symbol `var_` values

Lambda-solved MIR:

- determines exact callable sets
- associates capture types with callable members
- propagates erasure requirements
- emits explicit `boxed_erased_boundary` nodes with box type, payload source
  type, payload boundary type, direction, and `BoxPayloadRepresentationPlan`
- propagates boxed payload representation requirements through aliases, binders,
  captures, parameters, returns, and expression occurrences
- propagates boxed payload representation requirements through branch joins,
  source `match` condition/pattern edges, pattern binders, projections, and
  returned values
- solves boxed payload representation only in the specialization-local
  lambda-solved type store after clone-instantiation and zonking
- emits module-interface representation capability templates and instantiated
  capabilities for boxed payload traversal through imported or opaque nominals
- consumes hosted/platform callable representation metadata instead of inferring
  it from hosted symbol names or layouts
- exposes zonked callable representations for `call_value.requested_fn_ty`,
  `call_proc.requested_fn_ty`, and `proc_value.fn_ty`
- treats `call_proc` as direct procedure calls for inference and SCCs
- treats `proc_value` as first-class procedure values with explicit captures
- exports canonical callable-set algebra and ordering
- exports cycle-safe canonical callable-set, capture-shape, erased signature, and
  erased adapter keys

Executable MIR:

- builds capture records
- emits callable-set values for non-erased callable values
- synthesizes erased adapters when a finite callable-set value crosses an
  erased `Box(T)` boundary
- consumes `BoxPayloadRepresentationPlan` instead of deciding erased callable shape
  compatibility from executable types
- emits finite callable-set `callable_match` expressions for non-erased callable
  calls
- reserves executable specializations before emitting `call_direct` branches
- supplies `callable_match` branch `direct_args` as source args plus optional
  trailing capture record
- preserves fixed arity in every direct, erased, and callable-set call
- emits `packed_erased_fn` only for explicitly erased callable values
- emits `call_erased`
- emits `call_direct` where executable targets are exact
- inserts explicit bridges
- runs executable ownership solving after all executable targets are reserved
- preserves solved ownership facts for `call_direct`, `call_erased`,
  `callable_match`, callable-set values, `packed_erased_fn`, erased adapters,
  capture records, `Box.box`, `Box.unbox`, and bridges so IR/LIR can preserve
  them for `RcInsert`

The following are forbidden:

- callable truth in environment side fields
- callable truth in expression side tables
- capture truth in `FactId` arrays
- exact callable truth in alias side tables
- target recovery from source function types
- capture recovery from body scanning in executable MIR
- callable-set member ordering through `Symbol.raw()`
- body-derived summaries as executable truth
- bare procedure-symbol `var_` values after lifted MIR
- automatic currying
- compiler-synthesized partial application

## Deletions

Delete these families completely:

```text
ValueFact
CallableFact
FactId as semantic truth
expr_facts
exact_callable_aliases as semantic truth
callableTargetForExprFact
callableCapturesForExprFact
authoritativeCallableValue
refinedSourceTypeForExpr
exactTagSourceTypeForExpr
attachedMethodOwnerForExpr
resolveAttachedMethodTargetFromExpr
ownerForExpr
resolve.*TargetFromExpr
source/executable relation side tables
late source type refinement helpers
singleton tag source type construction
method lookup data threaded beyond mono MIR
bare procedure-symbol `var_` values after lifted MIR
non-`Box(T)` erased boundaries
executable erased-shape compatibility as semantic decision logic
LIR procedure ownership inference as semantic truth
backend or interpreter ownership inference
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
src/mir/lambda_solved/representation.zig
src/mir/lambda_solved/lower.zig
src/mir/lambda_solved/verify.zig
src/mir/executable/ast.zig
src/mir/executable/type.zig
src/mir/executable/lower.zig
src/mir/executable/ownership.zig
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

The registry must map `MethodKey { owner: MethodOwner, method_ident }` to method
definition refs or intrinsics. `MethodOwner` must be explicit semantic owner
identity, not expression shape and not display-name lookup.

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

Mono MIR `proc_value` is valid only for top-level procedure values and must have
empty captures.

Mono MIR lowering from checked CIR must resolve:

- ordinary dispatch
- type dispatch
- nominal/custom equality

to `call_proc` calls immediately.

Commit when mono MIR verification proves:

- no exported mono MIR dispatch nodes exist
- `call_proc` targets only top-level mono-specialized procedures
- mono `proc_value` captures are empty
- mono `proc_value` targets for top-level procedure values are mono-specialized
  at the exact requested mono source function type
- the mono specialization queue has no pending `call_proc` or `proc_value`
  dependencies
- every `call_proc` and `call_value` has exactly the arity of its
  `requested_fn_ty`
- no mono MIR node represents automatic currying or partial application

### 4. Harden Lifted MIR

Update lifted MIR to consume dispatch-free mono MIR.

Delete all dispatch cases from lifted MIR AST and lowering.

Add explicit `CaptureSlot` metadata to lifted procedure definitions.

For recursive local-function groups, allocate procedure symbols first, then
compute captures to a least fixed point across all members before exporting
lifted MIR.

Rewrite every captured value reference inside a lifted procedure body to
`capture_ref(slot)`.

Rewrite every local function or closure value to a `proc_value` with explicit
`CaptureArg` payloads.

Rewrite every self-reference, sibling-reference, and alias of a local function
or closure to an explicit `proc_value`.

Rewrite every call through a local function or closure to `call_value`.

If procedure ids or symbols change during lifting, add explicit rewrite maps for
`call_proc` and `proc_value` targets.

Commit when lifted MIR verification proves:

- all `call_proc` and `proc_value` targets exist
- all procedure captures are explicit `CaptureSlot`s
- all captured value references are explicit `capture_ref` nodes
- recursive local-function capture sets are fixed-point complete
- all `proc_value` captures are explicit `CaptureArg`s in slot order
- no captured source symbol remains as ordinary `var_` inside a lifted body
- no bare procedure-symbol `var_` values exist
- no aliases of local functions or closures remain as bare `var_`
- no local function or closure call is represented as `call_proc`
- no dispatch terms exist in exported lifted MIR

### 5. Harden Lambda-Solved MIR

Update lambda-solved MIR to consume dispatch-free lifted MIR.

Delete all dispatch cases from lambda-solved AST, inference, erasure
propagation, and verification.

Preserve and clean up the real responsibilities:

- instantiate lifted types
- infer callable sets
- propagate erasure
- compute `erased_box_payload_type(T)` facts for explicit `Box(T)` boundaries
- build the specialization-local `RepresentationStore`
- create distinct representation roots for every expression result, binder,
  pattern binder, procedure parameter, procedure return, and capture slot
- create `require_box_erased(payload_root)` seeds only from explicit `Box(T)`
  boundaries
- preserve explicit `boxed_erased_boundary` box type, payload source type,
  payload boundary type, direction, representation roots, and payload
  `BoxPayloadRepresentationPlan`
- solve boxed payload representation requirements through aliases, binders,
  captures, function parameters, function returns, and expression occurrences
- solve boxed payload representation requirements through branch joins, source
  `match` condition/pattern edges, pattern binders, projections, loops, and
  returned values
- solve boxed payload representation requirements only after
  specialization-local clone-instantiation and zonking
- publish and consume explicit module-interface representation capability
  templates and instantiated capabilities for imported and opaque nominal boxed
  payload traversal
- consume hosted/platform callable representation metadata explicitly
- order recursive SCCs
- enforce canonical callable-set unification algebra
- export zonked callable representations for every executable specialization
  input
- clone-instantiate generalized templates before executable lowering consumes
  them

Commit when lambda-solved MIR verification proves:

- no dispatch terms exist
- callable members and captures are explicit
- callable-set member order is canonical
- each repeated callable member has exactly the same capture slots
- erased callable requirements are explicit
- every `boxed_erased_boundary` stores box type, payload source type, payload
  boundary type, direction, and boxed payload representation plan
- every function slot reachable through an explicit `Box(T)` erased boundary is
  represented as erased in the exported boxed payload boundary type
- no structural boxed payload plan node implies runtime traversal, runtime
  container rebuilding, or non-`Box(T)` erasure
- no non-`Box(T)` root can introduce erased callable representation
- representation equality is occurrence/value-flow based, not type-id based
- two unrelated roots with equal logical `TypeId`s do not unify unless an
  explicit representation edge connects them
- every exported representation root has a solved representation class
- every `require_box_erased(payload_root)` seed is owned by an explicit
  `Box(T)` boundary
- no executable specialization input contains generalized or unresolved type
  variables
- canonical type keys and boxed payload transforms are cycle-safe graph
  transforms with stable recursion binders/backrefs
- canonical callable-set keys, capture-shape keys, erased function signature
  keys, erased adapter keys, and boxed payload representation plans are
  cycle-safe graph transforms with stable recursion binders/backrefs
- no exported boxed payload representation plan or callable/capture key refers
  to a template type store, another specialization's type store, raw type ids, or
  unresolved type variables
- imported, opaque, hosted, and platform-owned boxed payload traversal occurs
  only through explicit representation capabilities
- `call_proc` and `proc_value` targets remain explicit
- `call_proc` and `proc_value` participate in callable inference and SCC ordering
- every `proc_value` capture arg unifies with its target `CaptureSlot`

### 6. Replace Executable Fact Planner

Rewrite executable MIR lowering so it consumes lambda-solved MIR and emits
executable MIR without fact side tables.

Executable MIR must run executable ownership solving after all executable
specializations, erased adapters, intrinsic wrappers, bridge procedures, entry
wrappers, and hosted targets have been reserved, and before lowering to IR.
That solver produces `ProcOwnershipContract`, `CallableCallOwnershipKey`, node
ownership semantics, bridge ownership semantics, and boxed-boundary ownership
semantics.

Delete:

```text
ValueFact
CallableFact
expr_facts
FactKey
semantic FactId usage
exact_callable_aliases
callableTargetForExprFact
callableCapturesForExprFact
authoritativeCallableValue
executableTypesHaveErasedCallableShapeMismatch
executableTypeHasMoreSpecificErasedCallableShape
executableTypeHasWiderTagUnionShape as semantic repair logic
lowerBoxBoundaryExpr as executable shape recovery
```

Executable specialization keys must be:

```text
ProcBaseKey
+ owner mono specialization key for lifted locals
+ zonked lambda-solved argument and return structural type keys
+ finite callable-set member procedure identity when specializing a callable-set
  branch
+ erased adapter key when specializing an erased adapter
+ capture slot shape and capture types for callable-set or erased captures
+ executable argument and return types
+ representation mode
```

not:

```text
fact handles + expression ids + source/executable side channels + raw type ids
ProcOrderKey
```

Commit when executable MIR verification proves:

- direct calls have explicit targets
- direct call args match target signatures
- direct, erased, and callable-set calls preserve fixed Roc arity
- erased calls have explicit erased function types
- erased adapters are synthesized for finite callable-set values crossing
  erased `Box(T)` boundaries
- `callable_match` evaluates its callable expression and original arguments
  exactly once before branching
- callable-set values have explicit member capture payloads
- callable-set member tag assignment is deterministic
- callable-set capture payload field ordering is deterministic
- finite callable-set calls lower to explicit `callable_match`
- every `callable_match` branch has a reserved executable specialization
- every `callable_match` branch records exact `direct_args`
- every callable member direct-call signature is source args plus optional
  trailing capture record
- every `callable_match` has exactly one `CallableCallOwnershipKey`
- every branch in a `callable_match` conforms to the same normalized argument
  modes and result contract
- no executable MIR node represents automatic currying or partial application
- no ordinary source `match` satisfies callable-set lowering verification
- packed erased functions have explicit captures
- bridge nodes connect concrete executable MIR types
- every executable MIR node introduced for callable lowering, erased packaging,
  boxed payload boundaries, and bridges carries explicit ownership semantics for
  LIR
- every executable procedure has a `ProcOwnershipContract` before IR lowering
- LIR ownership logic does not infer procedure contracts as semantic truth

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
FactKey
semantic FactId usage
exact_callable_aliases
callableTargetForExprFact
callableCapturesForExprFact
authoritativeCallableValue
refinedSourceTypeForExpr
exactTagSourceTypeForExpr
attachedMethodOwnerForExpr
resolveAttachedMethodTargetFromExpr
ownerForExpr
resolve.*TargetFromExpr
```

Forbid `Symbol.raw()` as an ordering key for callable-set members, erased
adapter emission, capture fixed-point ordering, or generated procedure emission.

Forbid raw type-store ids in executable specialization keys.

Forbid any erased-boundary facts whose root is not `Box(T)`.

Forbid executable erased-shape compatibility helpers from making semantic
lowering decisions. Boxed erased-boundary decisions must come from lambda-solved
`boxed_erased_boundary` facts and `BoxPayloadRepresentationPlan` values. They
may be rechecked only by debug-only verifiers.

Forbid any MIR, executable MIR, IR, or LIR operation whose semantic purpose is
automatic currying or compiler-synthesized partial application.

Forbid invalid Roc syntax in plan examples and tests that are intended to be Roc
source. In particular, forbid Haskell-style run declarations, backslash-arrow
lambdas, and whitespace function application. Roc examples must use lambdas like
`|x| x` and calls like `f(x, y)`.

Forbid bare procedure-symbol `var_` values outside mono MIR and lifted MIR input
pattern matching.

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
- lifted MIR contains explicit `CaptureSlot`s, explicit `proc_value`
  `CaptureArg`s, and no dispatch
- lambda-solved MIR contains explicit callable sets and no dispatch
- executable MIR contains direct/erased calls, finite callable-set
  `callable_match`
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
- mono `proc_value` captures are empty
- `call_proc` carries exact requested mono source function types
- every `call_proc` and `call_value` carries all fixed-arity source arguments
- no call node encodes automatic currying or partial application
- `call_proc` is not an executable direct call
- `call_proc` does not target local functions or closures

Lifted MIR:

- every lifted procedure target exists
- every procedure capture is an explicit `CaptureSlot`
- every captured value reference is an explicit `capture_ref`
- recursive local-function groups compute captures to a fixed point
- every local function or closure value is an explicit `proc_value`
- aliases of local functions and closures become explicit `proc_value`
- every `proc_value` capture arg is explicit and in slot order
- every call through a local function or closure is `call_value`
- no captured source symbol remains as ordinary `var_`
- no bare procedure-symbol `var_` values exist
- `call_proc` and `proc_value` targets are rewritten by explicit maps
- no dispatch nodes exist

Lambda-solved MIR:

- callable sets are explicit
- captures are attached to callable members
- callable-set members are canonical ordered finite maps
- repeated callable members have identical capture slots
- mismatched capture slots panic in debug verification
- erasure requirements are explicit
- `boxed_erased_boundary` stores box type, payload source type, payload boundary
  type, direction, representation roots, and `BoxPayloadRepresentationPlan`
- boxed payload boundary types structurally rewrite every reachable function
  slot to erased callable representation
- `RepresentationStore` has distinct roots for every expression result, binder,
  pattern binder, procedure parameter, procedure return, and capture slot
- representation variables unify only through explicit value-flow edges, never
  merely through equal logical `TypeId`s
- every `require_box_erased(payload_root)` seed is attached to an explicit
  `Box(T)` boundary
- every exported representation root has a solved representation class
- boxed payload representation requirements propagate through aliases, binders,
  captures, parameters, returns, and expression occurrences
- boxed payload representation requirements propagate through source `match`
  branch joins, condition/pattern edges, pattern binders, projections, loops, and
  returned values
- structural boxed payload plan nodes never imply runtime container traversal or
  rebuilding
- no erased boundary exists for non-boxed `List(T)`, records, tuples, tag unions,
  functions, or nominals
- imported, opaque, hosted, and platform-owned boxed payload traversal requires
  explicit representation capabilities
- opaque nominal atomic traversal requires an explicit
  `NoReachableCallableSlotsProof`
- finite callable-set erasure preserves source member metadata for executable
  adapter synthesis
- erased adapter keys include finite callable-set identity, erased function
  signature, erased-call ownership contract, and capture shape
- generalized templates are clone-instantiated and zonked before executable
  lowering consumes them
- boxed payload representation plans, callable-set keys, capture-shape keys,
  erased signature keys, and erased adapter keys are computed from the
  specialization-local lambda-solved type store
- instantiated capture refs get their types from `CaptureSlotInstance`, not from
  environment lookup or body scanning
- canonical type keys and boxed payload transforms handle recursive types without
  raw type ids or infinite recursion
- canonical callable-set keys, capture-shape keys, erased signature keys, and
  erased adapter keys handle recursive callable/capture graphs without raw type
  ids or infinite recursion
- no dispatch nodes exist
- `call_proc` and `proc_value` have SCC dependency edges
- `call_proc` is inferred as a call to its procedure target type
- `proc_value` is inferred as a value of its procedure target type
- every `proc_value` capture arg type unifies with its target capture slot type

Executable MIR:

- every direct call target exists
- direct call arg/result types match signatures
- direct, erased, and callable-set calls preserve fixed Roc arity
- every finite callable-set call lowers to an explicit `callable_match`
- every `callable_match` binds the callable expression and original call
  arguments once, before member branching
- every `callable_match` branch corresponds to exactly one callable-set member
- every `callable_match` branch has a reserved executable specialization
- every `callable_match` branch stores exact `direct_args`
- every `callable_match` branch passes all fixed-arity source arguments exactly
  once, plus only the optional trailing capture record
- every `callable_match` has exactly one `CallableCallOwnershipKey`
- every branch conforms to that key's normalized argument modes and result
  contract
- ordinary source `match` does not satisfy callable-set lowering verification
- singleton finite callable-set calls still lower to `callable_match`
- every callable-set value has explicit member capture payload metadata
- every callable-set value has deterministic member tag ordering
- every callable-set capture payload has deterministic field ordering
- callable-set member tag ordering uses `ProcOrderKey`, not `Symbol.raw()`
- every packed erased function has explicit capture metadata
- finite callable-set values crossing erased `Box(T)` boundaries synthesize
  erased adapters
- executable MIR consumes `BoxPayloadRepresentationPlan` and does not make
  semantic erased-shape compatibility decisions
- executable MIR rejects erased-boundary roots other than `Box(T)`
- executable MIR rejects imported, opaque, hosted, and platform-owned boxed
  payload traversal without explicit representation capabilities
- every erased capture record has deterministic field ordering
- every erased call has an explicit erased function type
- every erased call has an explicit ownership contract from `ErasedFnSigKey`
- every executable procedure has a `ProcOwnershipContract` before IR lowering
- first-class intrinsic references use explicit wrapper procedures
- logical field indexes are preserved until LIR resolves physical offsets
- bridges connect concrete executable types
- callable lowering, erased packaging, boxed payload boundaries, and bridges
  preserve explicit ownership facts for LIR
- no fact side tables exist

IR/LIR:

- direct calls lower to direct calls
- erased calls lower to erased calls
- no method/dispatch operation exists
- every value-producing LIR statement has sufficient ownership semantics for
  `RcInsert`
- LIR ownership code verifies, normalizes, or translates explicit ownership
  facts only; it does not infer procedure ownership contracts as semantic truth
- `RcInsert` is the only non-builtin stage that emits explicit `incref`,
  `decref`, and `free`
- backends execute explicit LIR RC statements and perform no ordinary RC
  analysis

## Required Behavioral Tests

Static dispatch:

- generic dispatch specializes to different nominal method targets
- generic target methods specialize at exact monomorphic function types
- chained dispatch resolves from the lowered receiver result type
- chained dispatch does not call any expression-based owner resolver
- type-var alias dispatch resolves from the specialized alias target type
- primitive methods resolve through builtin primitive owners
- list methods resolve through the builtin `List` owner
- box methods resolve through the builtin `Box` owner
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
- call-only intrinsics lower directly only when they never flow as values
- first-class intrinsic method references synthesize wrapper procedures
- dotted expressions without arguments are checked as field access, not static
  method references
- tag construction never creates singleton source tag-union types
- any future unbound source method symbol used as a first-class value resolves to
  explicit `proc_value` with empty captures, not executable direct calls
- any future receiver-bound method value lowers to an explicit closure capturing
  the receiver, not an empty-capture `proc_value`

Callable/capture behavior:

- direct top-level function call
- fixed-arity multi-argument function call, for example a function with type
  `I64, I64 -> I64`
- missing-argument calls to fixed-arity functions are rejected before MIR export;
  they do not synthesize partial-application closures
- extra-argument calls to fixed-arity functions are rejected unless the source
  explicitly calls a returned function value
- generic top-level function specialization
- generic top-level function used as a first-class value specializes through
  `proc_value` before lambda-solved MIR
- local closure with no captures
- local closure with captures
- recursive local function
- mutually recursive local functions
- recursive local functions that reference sibling procedure values require
  fixed-point capture propagation
- closure returned from a function
- closure passed as an argument
- direct `call_proc` becomes executable direct call without erased packaging
- singleton `call_value(proc_value(...))` lowers to `callable_match`
- captured local function calls lower through explicit `proc_value` captures
- finite callable-set calls evaluate callable and arguments exactly once before
  branch dispatch
- finite callable-set branch direct calls receive source args plus optional
  trailing capture record
- boxed erased-boundary packaging with capture record
- finite callable-set value crossing an erased `Box(T)` boundary synthesizes an
  erased adapter whose body uses `callable_match`
- structural erased-boundary coercion through records, tuples, tags, `List(T)`,
  nested `Box(T)`, and nominal backing types only inside an explicit `Box(T)`
  payload
- two unrelated values with the same logical type do not share erased
  representation when only one flows into `Box(T)`
- a shared value used both normally and inside `Box(T)` has one representation
  class, and ordinary uses consume the solved erased representation
- representation propagation follows explicit `let`, parameter, return, capture,
  branch, pattern, projection, and loop edges without using equal `TypeId`s as a
  substitute for value flow
- unboxing to a payload containing function slots gives those slots erased
  callable representation
- non-boxed `List(T)`, records, tuples, tag unions, functions, and nominals do
  not erase
- already-erased value crossing a matching erased `Box(T)` boundary passes
  through after verification
- boxed erased-call round trip
- non-boxed polymorphic closure does not erase
- hosted function flowing as first-class value
- first-class intrinsic function flowing as a wrapper `proc_value`
- generalized procedure template instantiated at two concrete callable shapes
  without raw type ids leaking into executable keys
- logical capture field indexes survive physical layout reordering
- deterministic callable-set member tag ordering
- deterministic callable-set capture payload field ordering
- deterministic erased capture record field ordering
- recursive callable set whose capture graph refers back to the callable set
  produces finite canonical callable/capture keys
- boxed payload through source `match` branches propagates erased callable
  representation into every branch result and pattern binder
- imported opaque nominal boxed payload traversal succeeds only with an explicit
  exported representation capability
- imported opaque nominal boxed payload traversal without an exact capability
  fails before executable MIR
- hosted function with callable-containing args or returns consumes explicit ABI
  representation and erased-call ownership metadata
- finite callable-set call where one branch consumes an argument and another
  branch borrows it normalizes to one `CallableCallOwnershipKey` for the whole
  `callable_match`
- `Box.unbox` borrows the box and materializes a copied payload result; no test
  may rely on move-out semantics for `Box.unbox`
- `packed_erased_fn`, erased adapters, `callable_match`, `Box.box`,
  `Box.unbox`, and bridges produce LIR ownership facts consumed by `RcInsert`

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
- mono MIR `call_proc` targets only top-level mono-specialized procedures
- mono MIR top-level `proc_value` targets are mono-specialized at the exact
  requested mono source function type
- mono MIR exports no pending `call_proc` or `proc_value` specialization work
- mono MIR has no automatic currying or compiler-synthesized partial
  application
- every mono MIR call arity exactly matches its requested fixed-arity function
  type
- lifted MIR output has no dispatch nodes
- lifted MIR has explicit `CaptureSlot` metadata for every lifted procedure
- lifted MIR uses `capture_ref` for every captured value reference
- lifted MIR computes recursive local-function captures to a fixed point
- lifted MIR procedure values are explicit `proc_value` nodes with
  `CaptureArg`s
- lifted MIR has no captured source symbols represented as ordinary `var_`
- lifted MIR has no bare procedure-symbol `var_` values
- lifted MIR rewrites `call_proc` and `proc_value` targets only through
  explicit maps
- lambda-solved MIR output has no dispatch nodes
- lambda-solved MIR has explicit callable/lambda-set/erasure metadata for every
  `proc_value`, `call_proc`, and `call_value` executable MIR consumes
- lambda-solved MIR uses explicit `boxed_erased_boundary` nodes preserving box
  type, payload source type, payload boundary type, direction, representation
  roots, and `BoxPayloadRepresentationPlan`
- lambda-solved MIR exports a specialization-local `RepresentationStore` whose
  roots are expression/binder/parameter/return/capture occurrences, not logical
  type identities
- lambda-solved MIR creates `require_box_erased(payload_root)` only from
  explicit `Box(T)` boundaries
- lambda-solved MIR never unifies representation variables merely because their
  logical `TypeId`s are equal
- lambda-solved MIR structurally rewrites every reachable function slot inside
  explicit `Box(T)` payload boundaries
- lambda-solved MIR propagates boxed payload representation requirements through
  aliases, binders, captures, parameters, returns, and expression occurrences
- lambda-solved MIR propagates boxed payload representation requirements through
  source `match` branch joins, condition/pattern edges, pattern binders,
  projections, loops, and returned values
- lambda-solved MIR computes boxed payload representation plans and
  callable/capture keys only from specialization-local clone-instantiated and
  zonked type stores
- lambda-solved MIR requires explicit module-interface representation
  capabilities before traversing imported or opaque nominal boxed payloads
- opaque nominal atomic traversal requires an explicit
  `NoReachableCallableSlotsProof`
- lambda-solved MIR consumes hosted/platform callable representation metadata
  explicitly
- lambda-solved MIR exports only zonked executable specialization inputs
- lambda-solved MIR enforces canonical callable-set unification algebra
- canonical type keys and boxed payload transforms are cycle-safe graph
  transforms with stable recursion binders/backrefs
- canonical callable-set keys, capture-shape keys, erased function signature
  keys, erased adapter keys, and boxed payload representation plans are
  cycle-safe graph transforms with stable recursion binders/backrefs
- executable MIR output has no dispatch nodes
- executable MIR is the first stage that emits `call_direct`
- executable MIR lowers every finite non-erased callable-set call to explicit
  `callable_match`
- executable MIR synthesizes erased adapters for finite callable-set values
  crossing erased `Box(T)` boundaries
- executable MIR records exact branch `direct_args`
- executable MIR records one `CallableCallOwnershipKey` for every
  `callable_match`
- executable MIR direct, erased, and callable-set calls preserve fixed Roc arity
- executable MIR ordinary source `match` and callable-set `callable_match` are
  structurally distinguishable
- executable MIR keeps callable-set values distinct from packed erased function
  values
- callable-set member ordering uses `ProcOrderKey`, not `Symbol.raw()`
- executable specialization keys use semantic base/type/representation keys,
  not `ProcOrderKey`, raw type ids, expression ids, or fact ids
- executable MIR consumes `BoxPayloadRepresentationPlan` instead of making
  semantic erased-shape compatibility decisions
- executable MIR consumes erased-call ownership contracts from `ErasedFnSigKey`
  instead of recovering ownership from runtime function pointers or capture
  layouts
- executable ownership solving produces `ProcOwnershipContract` values before
  IR lowering
- executable MIR preserves ownership facts for callable lowering, erased
  packaging, boxed payload boundaries, and bridges
- `Box.unbox` is borrowed/copy materialization only; consuming move-out requires
  a separate explicit operation
- logical field indexes are resolved to physical offsets only through the layout
  store
- no exact callable alias side tables remain
- requested function type verifiers are debug-only compiler assertions and do not
  add runtime checks to user programs
- no source/executable fact side tables remain
- no expression-based method owner resolver remains
- no syntax-derived source type reconstruction remains
- method registry is consumed only by mono MIR construction
- IR lowering consumes executable MIR only
- IR lowering preserves executable ownership facts for LIR
- LIR ownership code verifies or translates ownership facts but does not infer
  procedure ownership contracts as semantic truth
- LIR `RcInsert` is the only non-builtin stage that emits explicit `incref`,
  `decref`, and `free`
- LIR/backends do not know about source methods
- backends do not perform ordinary reference-counting analysis and only execute
  explicit LIR RC statements
- semantic audits forbid the deleted families
- guarded eval and glue gates pass
