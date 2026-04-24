# MIR Architecture Cutover Plan

## Objective

Replace the old competing-source architecture with a MIR-family lowering
pipeline.

The final pipeline is:

```text
checked CIR
  -> mono MIR
  -> row-finalized mono MIR
  -> lifted MIR
  -> lambda-solved MIR
  -> executable MIR
  -> IR
  -> LIR
```

Compile-time constants use the same executable path, but they are evaluated as
part of checking finalization, before checked artifacts are published:

```text
checked CIR
  -> mono MIR
  -> row-finalized mono MIR
  -> lifted MIR
  -> lambda-solved MIR
  -> executable MIR
  -> IR
  -> LIR
  -> LIR interpreter
  -> compile-time value store
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
src/mir/mono/row_finalize
src/mir/lifted
src/mir/lambda_solved
src/mir/executable
```

The important final-state rule is not the directory name. The important rule is
that the old source/executable side-channel architecture is gone, and each MIR-family
stage has a precise, enforceable contract.

## Non-Negotiable Rules

No compiler stage after checking may recover, guess, reconstruct, approximate,
or best-effort semantic information.

Every post-check stage must consume explicit stage outputs from the previous stage.

All user-facing compiler errors must be reported during checking at the absolute
latest. After checking completes, every compiler stage must succeed. A violated
post-check assumption is a compiler bug, not a user-facing error and not a
recoverable compiler result.

Checking is not complete until compile-time constant evaluation has run and all
compile-time constant crashes, expect failures, numeric conversion failures, and
other user-facing compile-time evaluation problems have been reported. The LIR
interpreter may be used to perform that evaluation, but the whole operation is
inside checking finalization. After the checked artifact is published, compile-time
constant data is an input to later stages; it is not something later stages try
to produce, repair, or skip.

Compiler invariant violations have exactly one implementation shape:

```text
debug build: debug-only assertion
release build: unreachable
```

Post-check stages must not return recoverable semantic errors, emit fallback
code, silently repair missing data, or add release-build runtime checks for
compiler invariants.

Backends must not think about reference counting. They lower explicit LIR
`incref` and `decref` statements only.

When this plan says a type is fully resolved, it means all type-store links have
been chased, all placeholders that must be solved for the current stage are
solved, and the stage is consuming the actual current type rather than a stale
variable or unresolved link.

Static dispatch is eliminated in `mono MIR`, the first monomorphic stage. It
must not survive into lifted MIR, lambda-solved MIR, executable MIR, IR, or LIR.

Row finalization is a separate mono-MIR pass after static dispatch lowering and
before lifted MIR. It must not be implemented lazily inside representation
solving, executable lowering, IR lowering, or layout lowering.

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

The executable MIR stage replaces the current `lambdamono` side-table planner. It
must not preserve legacy value side-table records, legacy callable side-table
records, expression side tables, local constructor records, or
source/executable duplicate truth.

## Hard Command Rule

Every Zig invocation must go through:

```sh
ci/guarded_zig.sh zig ...
```

Do not run `zig ...` directly.

If a wrapper precheck reports an architectural or lint issue, fix that issue
first. Do not bypass, weaken, or locally skip the wrapper.

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
- carries legacy value/callable side-table records, semantic side-table ids, and
  expression-indexed side-table maps
- reconstructs or refines source types from expression syntax
- resolves method owners from expressions instead of monomorphic types

The final plan keeps the valid responsibilities and deletes the invalid
contracts.

## Resolution Boundaries

Static dispatch target selection belongs in mono MIR.

It cannot happen before mono MIR because checked CIR can still contain generic
dispatch sites whose target depends on the concrete specialization.

It must not happen after mono MIR because mono MIR already has the required
stage inputs:

- the checked dispatch operation
- the checked dispatch callable function type
- the checked dispatcher type variable selected for this dispatch site
- the checked method registry
- the specialization table

Lambda lifting does not change the semantic method owner of a dispatcher type.

Lambda-set solving does not change which method a monomorphic dispatcher type
names.

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
- source-name row operations before row finalization

Its input is checked CIR plus the checked type store, checked method registry,
and specialization roots.

Its output is monomorphic, typed, dispatch-free MIR that has not yet been row
finalized.

Mono MIR may still contain:

- local functions
- closures
- value calls through function values
- `call_proc` calls to source/MIR procedures
- `proc_value` values for top-level procedure values with empty captures
- structural equality
- source tag names and full monomorphic tag-union types
- source record field names on row operations
- fixed-arity function types and calls

Mono MIR must not contain:

- `dispatch_call`
- `type_dispatch_call`
- `method_eq`
- source type variables
- source-type refinement helpers
- syntax-derived singleton tag-union types
- method lookup tables for later stages
- checked `StaticDispatchCallPlan` values
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

### Row-Finalized Mono MIR

Row-finalized mono MIR is a separate type-state between mono MIR and lifted MIR.

It owns:

- converting source record field names to `RecordFieldId`
- converting source tag names to `TagId`
- converting tag payload positions to `TagPayloadId`
- interning canonical logical record and tag-union shapes
- deleting all name-based row operations before lifting

Its input is dispatch-free mono MIR plus the specialization-local mono type
store with all type links resolved for the current specialization.

Its output is dispatch-free, row-finalized mono MIR.

Row-finalized mono MIR may still contain:

- local functions
- closures
- value calls through function values
- `call_proc` calls to source/MIR procedures
- `proc_value` values for top-level procedure values with empty captures
- structural equality
- fixed-arity function types and calls

Row-finalized mono MIR must not contain:

- record construction keyed only by source field name
- record access keyed only by source field name
- record destructuring keyed only by source field name
- tag construction keyed only by source tag name
- tag pattern matching keyed only by source tag name
- tag payload projection keyed only by local payload position without an owning
  `TagPayloadId`
- any helper that computes logical row indexes by sorting names, scanning rows,
  scanning expressions, or inspecting physical layout order

Every row operation in row-finalized mono MIR stores compact finalized IDs:

```zig
record_construct: struct {
    shape: RecordShapeId,
    fields: Span(RecordFieldInit),
}

record_access: struct {
    record: ExprId,
    field: RecordFieldId,
}

tag_construct: struct {
    union_shape: TagUnionShapeId,
    tag: TagId,
    payloads: Span(TagPayloadArg),
}

tag_pattern: struct {
    union_shape: TagUnionShapeId,
    tag: TagId,
    payloads: Span(TagPayloadPattern),
}
```

The exact Zig field names may differ. The type-state boundary must not differ:
after row finalization, later MIR stages cannot represent name-only row lookup.

### Lifted MIR

Lifted MIR owns lambda lifting and capture discovery.

It consumes dispatch-free, row-finalized mono MIR.

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
- infer semantic type records from expression syntax
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

Recursive local-function groups are lifted through an explicit
`LiftedCaptureGraph`, not by one-pass body scanning and not by introducing local
alias variables that later stages reinterpret as procedure values.

Conceptual graph:

```zig
const LiftedCaptureGraph = struct {
    members: Span(LiftedGroupMember),
    value_edges: Span(CaptureValueEdge),
    proc_value_edges: Span(CaptureProcValueEdge),
};

const LiftedGroupMember = struct {
    source_symbol: Symbol,
    lifted_proc: Symbol,
    order_key: ProcOrderKey,
    args: Span(TypedSymbol),
    capture_slots: Span(CaptureSlot),
};

const CaptureValueEdge = struct {
    from_proc: Symbol,
    source_symbol: Symbol,
    source_ty: TypeId,
};

const CaptureProcValueEdge = struct {
    from_proc: Symbol,
    referenced_proc: Symbol,
};
```

The exact Zig field names may differ, but the graph responsibilities must not.
`CaptureValueEdge` records an ordinary external value needed by a member.
`CaptureProcValueEdge` records that one group member constructs or returns a
`proc_value` for another group member. The graph stores procedure identity
directly; it must not depend on generated alias names, environment lookup, or
source-name reconstruction.

For each recursive local-function group, lifted MIR must:

1. Allocate stable procedure symbols for every group member before lowering any
   member body.
2. Assign a stable `ProcOrderKey` to every group member before lowering any
   member body.
3. Scan member bodies only to build `LiftedCaptureGraph` edges.
4. Solve each member's `CaptureSlot` set to the least fixed point over:
   direct external value edges, values required to build referenced
   `proc_value` nodes, and capture slots required by referenced self or sibling
   procedures.
5. Assign deterministic `CaptureSlot.index` values after the fixed point is
   stable.
6. Lower each member body after capture slots are known.
7. Rewrite every body reference to a captured value to `capture_ref(slot)`.
8. Rewrite every self, sibling, local-function, and closure reference to an
   explicit `proc_value`.
9. Fill each `proc_value.captures` from values in the current scope or from the
   current procedure's own `CaptureSlot`s.

No local declaration may be inserted solely to stand for a lifted local
procedure value. A source alias like `g = f` may survive only if it becomes an
ordinary value binding whose body is a `proc_value`; it must not become a bare
`var_` that later stages interpret as a procedure.

Non-convergence is a compiler invariant violation. In debug builds, the
debug-only assertion must fire. In release builds, this path is `unreachable`.
It is not a fallback path.

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
- emit source/executable duplicate records
- decide static dispatch targets
- decide executable direct-call signatures

Lambda-solved MIR output must make callable metadata explicit in its types,
procedure metadata, and `proc_value` capture payloads. The executable MIR stage
must not have to rediscover it.

The lambda-solved type store is the single source of callable representation for
executable lowering.

Executable MIR must consume the fully resolved callable representation attached to:

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
   violations.
5. A callable set unified with erased callable representation becomes erased.

This algebra is not an optimization. It is the exported lambda-solved contract
that executable MIR consumes.

For every `call_value`, lambda-solved MIR must export a complete fully resolved callable
representation through `call_value.requested_fn_ty`.

For every `call_value`, lambda-solved MIR must also create explicit
representation-flow edges for the call relation. The callee expression root
merges with the whole function representation root for
`call_value.requested_fn_ty`. This is the representation root of the entire
fixed-arity Roc function value, not just its callable-set child. Each argument
expression root connects to the same-index requested function argument slot; the
requested function return slot connects to the call expression result root. The
callable-set slot is only the callable child of that whole function root.

For every `call_proc`, lambda-solved MIR must create the same representation-flow
edges against the whole representation root for `call_proc.requested_fn_ty` and
the instantiated target procedure signature. The call arguments connect to the
instantiated target parameter slots and the instantiated target return slot
connects to the call expression result root. The instantiated target procedure
function root and `call_proc.requested_fn_ty` must merge as whole fixed-arity
function representations before their argument, return, and callable child slots
are considered solved.

For every `proc_value`, lambda-solved MIR must connect the expression result root
to the whole function representation root for `proc_value.fn_ty`. A `proc_value`
is a function value, not just a callable-set child. Each `CaptureArg.expr` root
connects to the same-index instantiated `CaptureSlot` representation root.

These call/procedure/value representation edges are required even when the
logical type unifier has already unified the corresponding type variables. The
logical `TypeId` relation is not enough; boxed payload erasure walks
representation edges, and those edges must name the exact value-flow relation
from callee, arguments, returns, procedure values, and captures.

For every `call_value`, lambda-solved MIR must verify that the call supplies
exactly the fixed-arity parameter list of `call_value.requested_fn_ty`.

If that representation is a finite non-erased callable set, executable MIR must
treat every member as an executable specialization dependency of that call. If
that representation is erased, executable MIR must lower the call as an erased
call. Executable MIR must not inspect the callee expression to choose between
finite and erased representation.

Lambda-solved builder internals may use solver links, unbound variables, and
generalized variables while solving. Exported lambda-solved MIR must expose a
fully resolved view for every executable specialization input. Generalized variables may
remain only in specialization templates that are explicitly instantiated before
executable lowering consumes them.

Generalized variables may appear only in procedure specialization templates.
Before executable MIR consumes a procedure, call, or callable value, the
template must be clone-instantiated into a specialization-local lambda-solved
type store and fully resolved. No exported executable specialization key, executable MIR
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
    call_value_requested_fn: ExprId,
    call_proc_requested_fn: ExprId,
    proc_value_fn: ExprId,
    mutable_var_version: struct { symbol: Symbol, version: u32 },
    loop_phi: LoopPhiId,
};

const RepVarId = enum(u32) { _ };
const RepEdgeId = enum(u32) { _ };
const RepClassId = enum(u32) { _ };
const RepRequirementId = enum(u32) { _ };

const RepresentationStore = struct {
    roots: Map(RepRootId, RepVarId),
    vars: Store(RepresentationVar),
    edges: Store(RepresentationEdge),
    requirements: Store(RepresentationRequirement),
    classes: Store(SolvedRepresentationClass),
};

const RepresentationEdge = struct {
    from: RepVarId,
    to: RepVarId,
    kind: RepresentationEdgeKind,
};

const RepresentationEdgeKind = union(enum) {
    value_alias,
    value_move,
    function_arg: u32,
    function_return,
    function_callable,
    record_field: RecordFieldId,
    tuple_elem: u32,
    tag_payload: TagPayloadId,
    list_elem,
    box_payload,
    nominal_backing: NominalKey,
    branch_join,
    loop_phi,
    mutable_version,
};

const RepresentationRequirement = union(enum) {
    require_box_erased: BoxBoundaryId,
    require_shape: RepresentationShape,
};
```

The exact Zig field names may differ, but the identity model must not.

Every expression result, binder, pattern binder, procedure parameter, procedure
return, capture slot, callable requested-function occurrence, mutable variable
version, and loop phi gets its own representation root before representation
requirements are solved. Structural children are also explicit representation
variables: record fields, tuple elements, tag payloads, `List(T)` elements,
`Box(T)` payloads, nominal backing slots, function arguments, function returns,
and callable-representation slots.

Whole fixed-arity function values must have one representation root:

```zig
const FunctionRepShape = struct {
    args: Span(RepVarId),
    ret: RepVarId,
    callable: RepVarId,
};
```

`call_value`, `call_proc`, and `proc_value` each create or reference a
`requested_fn_root` whose shape is `FunctionRepShape`. There must be no exported
API that connects only the callable child while skipping the argument and return
children. The only legal helpers are whole-function helpers:

```zig
connect_call_value_whole_function(...)
connect_call_proc_whole_function(...)
connect_proc_value_whole_function(...)
```

The helper names may differ, but the contract must not. Each helper creates the
whole-function edge, each argument edge, the return edge, and the callable child
edge together.

Representation solving is a deterministic union-find plus worklist:

1. Allocate every `RepRootId` and structural child `RepVarId`.
2. Append all `RepresentationEdge` values.
3. Append all `RepresentationRequirement` values.
4. Union variables connected by value-flow edges.
5. Merge structural shapes inside each class.
6. Re-enqueue affected neighboring classes until no class changes.
7. Apply `require_box_erased` only by following the explicit representation graph
   reachable from the named `BoxBoundaryId.payload_root`.
8. Export one `SolvedRepresentationClass` for every class.

The solver may use path compression and dense indexes for compiler performance.
It must not use logical `TypeId` equality as a shortcut for unioning two roots.
Logical types are metadata on representation variables, not representation
identity.

Lambda-solved MIR must build representation edges through a dedicated
`ValueFlowGraphBuilder`. This builder owns the control-flow-sensitive mapping
from source symbols to current representation roots.

Conceptual builder state:

```zig
const ValueFlowGraphBuilder = struct {
    store: *RepresentationStore,
    current_proc: Symbol,
    current_return_root: RepVarId,
    locals: Map(Symbol, CurrentValueRoot),
    loop_stack: Stack(LoopValueFlowFrame),
};

const CurrentValueRoot = union(enum) {
    immutable: RepVarId,
    mutable_version: MutableVersionId,
};

const LoopValueFlowFrame = struct {
    header_phis: Map(Symbol, RepVarId),
    backedge_inputs: Map(Symbol, Span(RepVarId)),
    break_exit_inputs: Map(Symbol, Span(RepVarId)),
    exit_roots: Map(Symbol, RepVarId),
};
```

The builder walks lambda-solved MIR once per specialization and emits the full
set of roots, edges, loop phi records, branch joins, and boxed-boundary
requirements. It is the only place that interprets source control flow for
representation purposes. Executable MIR may verify the exported graph in debug
builds, but it must not add missing edges.

Semantic source mutation must be converted to SSA before representation solving.
The representation store must not model a source `var` as a physical mutable
storage cell whose representation can change over time. It models explicit SSA
values:

- a source `var` declaration creates version 0.
- every source reassignment creates a fresh version.
- every branch merge that can observe multiple incoming versions creates an
  explicit join version.
- every loop-carried mutable value creates explicit loop header phi, backedge,
  and loop-exit join roots.
- ordinary uses read the current SSA version for that control-flow point.

Any later IR or LIR `set`/`set_local`-style operation is a backend temporary
after executable layout has already been fixed. It is not semantic source
mutation. If such operations survive in the final architecture, debug
verification must prove the target and value layouts are exactly identical before
lowering continues. A stage after representation solving must not use assignment
to force two incompatible representations into the same storage slot.

A representation variable may reference a fully resolved logical `TypeId` as checked
type metadata, but that `TypeId` is not the representation variable's identity. Two
different roots with equal logical types remain different representation
variables until an explicit value-flow edge unifies them. This is required so
two unrelated values with the same type do not accidentally share erased
representation. A shared value does share representation because `let`, use,
parameter, return, capture, and projection edges explicitly connect the same
value flow.

`BoxPayloadRepresentationPlan`, callable representation, capture shape keys,
erased function signature keys, erased adapter keys, and executable callable
member keys must be produced only from the specialization-local lambda-solved
store after clone-instantiation and full type-link resolution.

A boxed use in one specialization must not mutate:

- the generalized procedure template
- another specialization's lambda-solved type store
- another specialization's boxed payload representation plan
- another specialization's callable/capture keys

Generalized templates may contain unsolved representation variables while they
are still templates. Exported executable inputs may not. Debug-only assertions
must fire if any exported `BoxPayloadRepresentationPlan`,
`CanonicalCallableSetKey`, `CaptureShapeKey`, `ErasedFnSigKey`,
`ErasedAdapterKey`, executable MIR type, or layout-publication input references
a template `TypeId`, a foreign specialization's type store, `for_a`,
`flex_for_a`, `unbd`, unresolved links, or raw checker variables. In release
builds, those paths are `unreachable`.

Executable specialization keys must be canonical structural keys after full
type-link resolution.
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
classes must emit stable recursion binders and backrefs derived from first
encounter order in the explicit type/representation graph being serialized. It
must not serialize raw `TypeId`, pointer identity, allocation order, or hash-map
iteration order. Debug-only assertions must fire if any exported key contains a
transient type-store id or can recurse forever. In release builds, those paths
are `unreachable`.

The same rule applies to recursive callable and capture graphs.
`CanonicalCallableSetKey`, `CaptureShapeKey`, `ErasedAdapterKey`, and any key
that references captures must serialize recursion with stable binders/backrefs.
They must not inline callable members or capture records recursively until the
process bottoms out. A recursive closure that captures a value containing itself
must produce a finite canonical key.

All exported semantic keys use one shared `CanonicalGraphKeyBuilder`:

```zig
const CanonicalGraphKeyBuilder = struct {
    arena: *BumpAllocator,
    seen_types: Map(CanonicalNodeRef, RecBinderId),
    seen_reps: Map(RepClassId, RecBinderId),
    out: ArrayList(u8),
    next_binder: u32,
};

const CanonicalNodeRef = union(enum) {
    lambda_solved_type: TypeId,
    executable_type: ExecTypeId,
    representation_class: RepClassId,
    proc_base: ProcBaseKeyRef,
    capture_shape: CaptureShapeKeyRef,
};
```

The exact Zig field names may differ, but there must be one builder contract
shared by:

- `MonoSpecializationKey`
- `ExecutableSpecializationKey`
- `CanonicalCallableSetKey`
- `CaptureShapeKey`
- `ErasedFnSigKey`
- `ErasedAdapterKey`
- `BoxPayloadCapabilityKey`
- `BoxPayloadRepresentationPlan` keys
- executable layout-publication keys

The builder must emit stable recursion binders on first encounter and backrefs
on repeated encounter. Procedure references are encoded as `ProcBaseKeyRef`,
not `Symbol.raw()`. Capture components are encoded in `CaptureSlot.index` order,
not capture-name order. Row components are encoded with finalized row IDs.
Children are visited in the canonical order defined by each structural shape.

Debug-only assertions in the builder must catch raw type-store IDs from
transient clones, generated symbol text, pointer identity, allocation order,
hash-map iteration order, and expression IDs. In release builds, those paths are
`unreachable`. There must not be parallel ad hoc serializers for individual key
families.

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

Lambda-solved MIR must preserve explicit boxed-boundary records for every erased
`Box(T)` boundary. Erasure is not a callable-only operation, but it is a
`Box(T)`-only operation.

The exported lambda-solved type contract includes:

```text
BoxBoundaryId
erased_box_payload_type(boundary: BoxBoundaryId)
require_box_erased(boundary: BoxBoundaryId)
```

The boxed-boundary table is the only source that can introduce erased callable
representation:

```zig
const BoxBoundaryId = enum(u32) { _ };

const BoxBoundary = struct {
    direction: BoxErasureDirection,
    input: ExprId,
    box_root: RepRootId,
    payload_root: RepRootId,
    box_ty: TypeId,
    payload_source_ty: TypeId,
    payload_boundary_ty: TypeId,
    payload_plan: BoxPayloadRepresentationPlan,
};
```

`erased_box_payload_type(boundary)` may be called only with a `BoxBoundaryId`
from this table. It recursively walks that boundary's boxed payload type and
rewrites every reachable function slot to erased callable representation.

`require_box_erased(boundary)` may be created only from a `BoxBoundaryId`. It is
a requirement in the `RepresentationStore`, not an executable conversion.
Solving that requirement walks the boundary's `payload_root` representation
graph and marks every reachable function representation slot as erased. The walk
follows only explicit representation edges already present in the store.

There must be no helper that accepts an arbitrary type, expression, or
`RepRootId` and makes it erased. Any API that introduces erasure must take a
`BoxBoundaryId`, and debug verification must prove that the boundary's `box_ty`
is exactly `Box(payload_boundary_ty)`.

Any recursion through records, tuples, tag unions, `List(T)`, nested `Box(T)`,
or nominal backing types happens only because those types are inside the payload
of an explicit `Box(T)`. Those types are not themselves erasure boundaries.
Non-callable data is preserved structurally.

Nominal recursion is allowed only when lambda-solved MIR has an explicit
representation record for the nominal backing.

Inside the module that defines a transparent nominal, the defining module's
checked type records provide that representation record. Outside the defining
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
the exact specialization-local fully resolved type arguments and boxed-payload
representation mode being compiled. The instantiated capability becomes an
ordinary `NominalPayloadRepresentation.imported_capability` node inside the
importer's specialization-local `BoxPayloadRepresentationPlan`.

Opaque nominals are atomic outside their defining module only when the interface
exports an explicit `opaque_atomic` capability with a compiler-produced
`NoReachableCallableSlotsProof`. Otherwise an opaque nominal that appears inside
a boxed erased payload must be traversed through an imported capability.
Checking must ensure the exact capability or exact `opaque_atomic` proof exists
before post-check lowering consumes the value. If post-check lowering reaches
this case without one, that is a compiler invariant violation: debug-only
assertion in debug builds, `unreachable` in release builds.

`NoReachableCallableSlotsProof` is instantiation-sensitive. It is valid only for
the exact nominal identity, exact fully resolved type arguments, and exact boxed-payload
representation mode it names.

Conceptual shape:

```zig
const NoReachableCallableSlotsProof = union(enum) {
    closed_backing_no_callable_paths: ClosedBackingProofKey,
    instantiated_args_no_callable_paths: struct {
        nominal: NominalKey,
        instantiated_args: Span(CanonicalTypeKey),
        proof_terms: Span<NoCallableProofTerm>,
    },
};
```

A generic opaque nominal may be `opaque_atomic` only if one of these is true:

1. Its hidden backing has no reachable function slot and no reachable path to any
   type parameter.
2. Every reachable type parameter is instantiated with an exact argument that has
   its own proof of no reachable function slot.

If a hidden backing reaches a type parameter and that instantiated argument may
contain a function slot, the nominal is not atomic for that instantiation. It
must be traversed through an explicit imported capability. The compiler must not
reuse a proof for `Opaque(I64)` when compiling `Opaque({ f : I64 -> I64 })`, and
it must not treat a proof for the generic nominal definition as a proof for all
instantiations unless the backing is closed over its type parameters.

The defining module produces these proofs from its checked backing records and
exports them as compiler-private interface records. Importing modules consume only
the proof and capability data. They must not inspect copied opaque backing
syntax, display names, or layout shapes to recreate the proof.

The compiler must not use copied opaque backing details, source syntax, display
names, or layout inspection as a substitute for the interface capability. If a
boxed erased boundary would require traversing an imported, opaque, hosted, or
platform-owned value without an explicit representation record, checking must
have reported that before post-check lowering. Reaching post-check lowering in
that state is a compiler invariant violation. It must not emit a runtime
conversion, generic opaque coercion, fallback erased wrapper, or best-effort
indirect call.

Nominal payload traversal uses one algorithm:

1. If the nominal is defined in the current module and is transparent, traverse
   the checked backing representation from the defining module.
2. If the nominal is imported and transparent through an exported capability,
   instantiate that capability with the exact canonical fully resolved type
   arguments and the exact boxed-payload representation mode for this
   specialization.
3. If the nominal is opaque and an `opaque_atomic` proof exists for the exact
   nominal identity, exact canonical type arguments, and exact boxed-payload
   representation mode, stop traversal at that nominal.
4. If the nominal is hosted or platform-owned, consume its explicit hosted or
   platform representation capability.
5. Otherwise trigger the post-check compiler-invariant path: debug-only
   assertion in debug builds, `unreachable` in release builds.

The algorithm returns a `NominalPayloadRepresentation` node. It must not return
source syntax, copied backing declarations, display names, layout shapes, or a
request for executable MIR to inspect the value later.

Hosted and platform procedures must declare their callable-containing argument
and return representations as part of their explicit ABI metadata. A hosted
procedure that accepts, returns, stores, or exposes a value containing function
slots must state whether each slot is finite callable-set representation or
erased function representation, including the erased fixed-arity signature and
explicit `ErasedCallAbiPolicyKey` for erased calls. Executable MIR must consume
that metadata.
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

- `let` binders connect the bound expression representation to the binder root,
  and the binder root connects to every use of the binder.
- `var_decl` creates mutable variable version 0. The initializer expression root
  connects to that version root.
- every `reassign` creates a new mutable variable version. The assigned
  expression root connects to the new version root, and later uses read from that
  current version root.
- every ordinary use of a mutable variable connects from the current mutable
  version root to the use expression root.
- procedure parameters connect the caller's argument representation to the
  callee's instantiated parameter representation.
- procedure returns connect every returned expression to the instantiated
  procedure return representation.
- `call_value.func` merges with the whole function representation root for
  `call_value.requested_fn_ty`; it does not connect directly to only the
  callable-set slot.
- `call_value.args[i]` connects to argument slot `i` of
  `call_value.requested_fn_ty`.
- the return slot of `call_value.requested_fn_ty` connects to the `call_value`
  expression result root.
- `call_proc.args[i]` connects to instantiated target procedure parameter slot
  `i`, and the instantiated target return slot connects to the `call_proc`
  expression result root.
- the instantiated target procedure function root merges with the whole function
  representation root for `call_proc.requested_fn_ty`.
- `proc_value` expression results merge with the whole function representation
  root for `proc_value.fn_ty`.
- `capture_ref(slot)` connects to the instantiated `CaptureSlotInstance.ty`.
- `proc_value.captures[i]` connects to the instantiated target
  `CaptureSlotInstance[i].ty`.
- records connect each field expression to the finalized `RecordFieldId` and its
  checked logical field type.
- tuples connect each element expression to the checked logical element type.
- tag construction connects each payload expression to the finalized
  `TagPayloadId` and its checked logical payload type for that constructor.
- tag payload access connects the projected payload representation to the
  finalized `TagPayloadId` and its checked logical tag payload type.
- `List(T)` literals and builders connect every element expression to the list
  element representation.
- tuple and record access connect the projected value to the checked tuple
  element or finalized `RecordFieldId` representation.
- `if` and source `match` result requirements connect the whole expression
  representation to every branch result.
- `if` and source `match` statement joins create explicit mutable variable join
  versions for variables assigned in only some branches or assigned to different
  versions in different branches. Each incoming branch version connects to the
  join version. Later uses read from the join version.
- source `match` condition requirements connect the matched value
  representation to every pattern.
- pattern tag payload requirements connect the matched tag payload
  representation to every nested payload pattern.
- pattern variable binders connect the pattern's representation to every use of
  that binder.
- `return` connects the returned expression to the current procedure return
  representation.
- `for` pattern binders connect to the iterable element representation.
- `for` and `while` loops create explicit loop phi roots for every mutable
  variable assigned in the loop body. The pre-loop version connects to the phi;
  every body reassignment that reaches the loop backedge connects to the same
  phi; after the loop, uses read from the loop exit version derived from that
  phi.
- `break` exits from a loop connect the current mutable versions to the loop exit
  join versions. A loop with no `break` still has exit join versions for
  variables assigned in the body.

These mutable-version, join, and loop-phi roots are SSA records. They are not
physical stack slots. Representation solving must finish before any later stage
chooses whether two SSA values can reuse storage. Storage reuse is allowed only
when it preserves the already-solved executable layout; it must not feed back
into representation solving.

These edges are lambda-solved records. Executable MIR may verify them in debug
builds, but it must not add missing edges, rebuild containers, or reinterpret a
branch/pattern result to satisfy a boxed payload requirement.

`Box.box(payload)` creates a `BoxBoundary` record and a
`require_box_erased(boundary)` requirement. The produced box root's payload child
is linked to the solved boxed payload representation class. `Box.unbox(boxed)`
creates a `BoxBoundary` record whose box root is the boxed input and whose
payload root is the unboxed expression result. It links the unboxed payload root
to the explicit boxed payload representation. It does not recover or request the
original finite callable-set shape.

The required callable representation algebra is:

```text
finite callable set + finite callable set = canonical finite callable-set union
finite callable set + erased callable = erased callable
erased callable + erased callable = erased callable with exactly matching ErasedFnSigKey
```

`ErasedFnSigKey` equality includes the erased-call ABI policy. Two erased
callables with the same erased argument and return representations but different
`ErasedCallAbiPolicyKey` values are different erased callable representations.
They must not silently merge.

The required structural representation algebra is separate and equally
mandatory. Each solved representation class has exactly one `RepresentationShape`
after solving:

```zig
const RepresentationShape = union(enum) {
    unknown,
    primitive: PrimitiveRep,
    record: Span(FieldRepSlot),
    tuple: Span(ElemRepSlot),
    tag_union: Span(TagRepSlot),
    list: RepVarId,
    box: RepVarId,
    nominal: NominalRepSlot,
    function: FunctionRepShape,
    callable: CallableRepShape,
};
```

Merge rules:

- `unknown` adopts the other shape.
- primitives must match exactly.
- records merge by finalized `RecordFieldId` from the owning `RecordShapeId`.
- tuples merge by tuple element index.
- tag unions merge by finalized `TagId` and `TagPayloadId` from the owning
  `TagUnionShapeId`.
- `List(T)` merges by merging the element representation.
- `Box(T)` merges by merging the payload representation. This does not create
  erasure; only an explicit `Box(T)` boundary may create
  `require_box_erased(boundary)`.
- transparent nominals merge through their explicit backing representation.
- imported nominals merge only through an instantiated
  `BoxPayloadCapabilityKey`.
- `opaque_atomic` nominals merge only with the same nominal identity and a valid
  instantiation-sensitive `NoReachableCallableSlotsProof`; they do not expose
  children to the merge.
- hosted/platform representations merge only through explicit hosted ABI
  capability keys.
- functions merge only when fixed Roc arity matches. Argument slots, return
  slots, and callable-representation slots merge pointwise.
- callable slots merge with the callable representation algebra above.

No structural merge rule may inspect source expression syntax, singleton tag
constructor syntax, display names, physical layout order, or source code body
shape. Tag construction must use the finalized full tag-union shape attached to
the expression, never a synthetic singleton constructor type. If the checked
tag-union type has constructors `[Ok(I64), Err(Str)]`, row finalization produces
one `TagUnionShapeId` for that full union, and constructing `Err("x")` creates
payload edges into the finalized `Err` slot of that full union.

Recursive shapes are solved with placeholders and stable recursion binders. A
representation solver must allocate the placeholder before merging children and
must serialize the solved class with stable backrefs. Infinite recursive copies,
raw pointer identity, allocation order, and hash-map iteration order are
compiler invariant violations handled only by debug-only assertion in debug
builds and `unreachable` in release builds.

If a shared value flows both to an ordinary use and to `Box(...)`, the explicit
`Box(T)` boundary is the only source of erasure, but lambda-solved MIR may solve
the shared callable slots to erased representation. The ordinary uses must then
consume that solved erased representation. Executable MIR must not create a
runtime `List(T)` map, record rebuild, tuple rebuild, tag rebuild, or nominal
rebuild to make a previously-produced non-erased container fit the box.

Every boxed erased boundary exports the corresponding `BoxBoundary` record:

```zig
BoxBoundary {
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
`payload_boundary_ty` are fully resolved lambda-solved types. `box_ty` must be exactly
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
`BoxPayloadRepresentationPlan` are expected-representation propagation plans.
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
must not contain that later key. It must not contain expression ids, side-table ids,
raw type ids, symbol freshening suffixes, or allocation-order-dependent data.

`ErasedFnSigKey` must include the canonical erased argument types, canonical
erased return type, fixed Roc arity, and an `ErasedCallAbiPolicyKey`.

`ErasedCallAbiPolicyKey` interns an explicit erased-call ABI policy:

```zig
ErasedCallAbiPolicy {
    kind: ErasedCallAbiKind,
    fixed_arity: u32,
    packed_function_mode: ErasedValueMode,
    arg_modes: Span(ErasedValueMode),
    result_mode: ErasedResultMode,
    retained_borrows: Span(ErasedBorrowRelation),
    hosted_owner: ?HostedAbiKey,
}
```

The policy describes the erased boundary's required calling convention. It says
how the packed erased function value is passed, how each fixed-arity Roc
argument is passed, how the result is materialized, and which retained borrow
relations the erased ABI promises. `arg_modes.len` must equal `fixed_arity`.
`hosted_owner` is set only for hosted, platform, or intrinsic ABI policies whose
ABI is defined outside ordinary Roc boxed-erased calling.

The policy must not contain a future `CallableCallOwnershipKey`, procedure body
ownership result, expression id, mutable type-store id, source syntax pointer,
runtime function pointer, capture-layout pointer, or backend-specific layout
handle. Those belong to later executable ownership records or lower-level ABI
lowering, not to erased callable identity.

That policy is part of key identity. Exact `ErasedFnSigKey` equality means:

```text
same fixed Roc arity
same canonical erased argument representations
same canonical erased return representation
same ErasedCallAbiPolicyKey
```

Same erased arguments and same erased return with a different
`ErasedCallAbiPolicyKey` is not the same erased callable representation. The
compiler must either emit an explicit adapter or bridge at a boundary that names
both policies. If no explicit adapter or bridge path exists after checking, that
is a compiler invariant violation. A later stage must not repair the mismatch by
inspecting an adapter body, capture layout, runtime function pointer, hosted
symbol, or ownership result.

`ErasedCallAbiPolicyKey` is produced before executable ownership solving. It is
the erased-call ABI contract for the boundary, not the ownership result of a
particular user procedure body.

For ordinary Roc boxed erased callables, the canonical policy is:

```text
borrow packed function value
borrow every erased call argument
fresh result materialization
```

If a concrete callable-set member or adapter body consumes an argument or returns
an alias/borrow relation that does not match this policy, executable ownership
solving inserts an explicit branch-local or adapter-local bridge to satisfy the
erased ABI. Hosted, platform, and intrinsic erased-call policies may use a
different `ErasedCallAbiPolicyKey`, but only when that policy is explicit hosted,
platform, or intrinsic ABI metadata.

This split is mandatory because `ErasedAdapterKey` and boxed payload plans are
reserved before executable ownership solving, while `CallableCallOwnershipKey`
is produced by executable ownership solving. `ErasedFnSigKey` must not contain a
future `CallableCallOwnershipKey`, procedure body ownership result, expression
id, side-table id, or runtime function pointer. A later LIR or backend stage must not
recover erased-call ownership from the adapter body, capture layout, host symbol,
or runtime function pointer.

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

Executable MIR replaces the current `lambdamono` side-table planner.

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
- source/executable side tables
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
syntax, or expression-derived records.

If executable MIR needs to cross a boxed erased boundary, it consumes the
lambda-solved `BoxBoundary` record and its
`BoxPayloadRepresentationPlan`. It must not compare executable source and target
shapes to decide whether erasure repair, adapter synthesis, or pass-through is
semantically required. Such comparisons are allowed only as debug-only
verification of the explicit boxed payload representation plan.

Executable MIR must only receive erased-boundary requests whose root is
`Box(T)`. If it receives a non-`Box(T)` root, that is a compiler invariant
violation handled by debug-only assertion in debug builds and `unreachable` in
release builds. Non-boxed `List(T)`, records, tuples, tag unions, functions, and
nominals are not erased-boundary roots.

When executable MIR lowers `call_proc`, it must still reserve or create the
target executable specialization from the lambda-solved procedure target and the
fully resolved requested callable type. It must lower and bridge every argument
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

Concrete shape:

```zig
CallableMatch {
    id: CallableMatchId,
    callable_set_key: CanonicalCallableSetKey,
    func_expr: ExprId,
    arg_exprs: Span(ExprId),
    func_tmp: TempId,
    arg_temps: Span(TempId),
    branches: Span(CallableBranch),
    result_ty: ExecTypeId,
}
```

`func_expr` and `arg_exprs` are the expressions evaluated exactly once by the
node. `func_tmp` and `arg_temps` are the temporaries produced by those
evaluations and consumed by every branch. `arg_temps.len` must equal the fixed
Roc arity of the requested callable type. No branch may re-evaluate `func_expr`
or any original argument expression.

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
legacy value side-table records
legacy callable side-table records
semantic side-table ids
expression-indexed side-table maps
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

Executable ownership solving is one fixed point over a single graph containing
both executable procedures and callable-match sites. It must not solve procedure
contracts first and callable-match contracts later as a separate sequential pass.

Conceptual node set:

```text
ProcOwnershipNode(proc_specialization)
CallableMatchOwnershipNode(callable_match_id)
ErasedAdapterOwnershipNode(erased_adapter_key)
BridgeOwnershipNode(bridge_id)
```

Concrete graph record:

```zig
OwnershipGraph {
    nodes: IndexSet(OwnershipNode),
    edges: Span(OwnershipEdge),
    proc_inputs: Map(ExecutableProcId, ProcOwnershipInput),
    callable_match_inputs: Map(CallableMatchId, CallableMatchOwnershipInput),
    bridge_inputs: Map(BridgeId, BridgeOwnershipInput),
    erased_adapter_inputs: Map(ErasedAdapterKey, ErasedAdapterOwnershipInput),
    explicit_abi_inputs: ExplicitAbiOwnershipInputs,
}

OwnershipEdge {
    from: OwnershipNodeId,
    to: OwnershipNodeId,
    reason: OwnershipEdgeReason,
}

CallableMatchOwnershipInput {
    callable_match_id: CallableMatchId,
    callable_set_key: CanonicalCallableSetKey,
    func_tmp: TempId,
    arg_temps: Span(TempId),
    branches: Span(CallableMatchBranchOwnershipInput),
    result_ty: ExecTypeId,
}

CallableMatchBranchOwnershipInput {
    member_proc: Symbol,
    direct_proc: Symbol,
    direct_args: Span(ExprId),
    bridge_ids: Span(BridgeId),
}
```

`OwnershipGraph` is built once from executable MIR plus explicit ABI metadata.
The graph owns the SCC inputs for the fixed point; no solver step may look up a
procedure body, callable branch, erased adapter body, bridge body, host symbol,
or layout shape through an independent path after graph construction. The node
ids above are graph-local ids. Semantic identity remains in `Symbol`,
`CallableMatchId`, `ErasedAdapterKey`, and `BridgeId`.

Required edges:

- procedure body to every direct callee procedure specialization
- procedure body to every `callable_match` it contains
- `callable_match` to every finite member procedure specialization branch
- erased adapter procedure to the body-level `callable_match` it emits
- procedure, adapter, and bridge nodes to explicit hosted/platform/intrinsic
  ownership metadata and `ErasedCallAbiPolicyKey` inputs

The solver computes strongly connected components and iterates each SCC until
all procedure contracts, callable-match ownership keys, and bridge obligations
are stable. Non-convergence is a compiler invariant violation handled by
debug-only assertion in debug builds and `unreachable` in release builds.

The ownership lattice is finite and deterministic:

- argument mode join is `borrow < consume`
- a callable-match argument is `consume` when any returning branch consumes it
- a callable-match argument is `borrow` only when every returning branch borrows
  it
- result alias/borrow relations are preserved only when every returning branch
  returns the same compatible relation to the same normalized input
- otherwise the callable-match result materializes as `fresh`
- `no_return` branches do not constrain returning branches
- an all-`no_return` callable match has a `no_return` result contract

Each callable-match branch receives explicit bridge obligations from the
normalized `CallableCallOwnershipKey` to that branch member's
`ProcOwnershipContract`. A branch may borrow a normalized consumed argument only
through an explicit bridge. A branch may return an alias when the normalized
result is fresh only by materializing the fresh result explicitly at the branch
boundary.

`CallableCallOwnershipKey` is assigned only after this fixed point. It is not a
component of `ErasedAdapterKey`, `ErasedFnSigKey`, executable specialization
keys, or layout keys.

`ProcOwnershipContract` contains the owned/borrowed/consumed mode for every
parameter, the result contract, and any retained-borrow relation needed by
`RcInsert`. Recursive procedure groups, callable-match groups, erased adapters,
and bridges are solved by fixed point over the ownership graph above. Hosted,
platform, intrinsic wrapper, erased-call ABI, and low-level contracts are
explicit inputs to the fixed point; they are not inferred from names, layout
shapes, runtime function pointers, or backend behavior.

Executable ownership solving is the only stage that may discover semantic
procedure ownership. IR lowering and LIR lowering preserve these records. In the
final architecture, any LIR ownership pass may only verify, normalize storage,
or translate already-produced ownership records for `RcInsert`; it must not infer
whether a user procedure owns a parameter or whether a call result borrows,
aliases, or owns data.

### LIR And Backends

LIR consumes IR.

Reference counting is inserted before backends, as explicit LIR statements.

Backends consume LIR only. They must not import MIR, IR builder internals,
checked CIR, method registries, or reference-counting analysis.

LIR ownership is a record pipeline, not a backend behavior.

Executable ownership solving and IR lowering must preserve enough ownership
information for `RcInsert` to emit explicit `incref`, `decref`, and `free`
statements. `RcInsert` is the only non-builtin stage that may turn those
ownership records into concrete reference-counting statements. Backends and the
ordinary interpreter path must execute those explicit LIR statements
mechanically.

Every executable MIR node introduced by this plan must have an explicit
ownership contract before LIR:

- `call_direct` consumes, borrows, or returns values according to the callee's
  explicit procedure ownership contract.
- `call_erased` consumes, borrows, or returns values according to the
  `ErasedCallAbiPolicyKey` inside `ErasedFnSigKey`. It must not infer ownership
  from the runtime function pointer or capture layout.
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

Before backend lowering, debug-only assertions must fire if any refcounted
layout value produced by executable MIR or IR lacks ownership semantics needed
by LIR, or if any backend/imported path branches on refcounted layout shape
except while executing explicit LIR RC statements. Release builds use
`unreachable` for the equivalent compiler-invariant path.

### Checking Finalization: Compile-Time Constants

Compile-time constants are evaluated by the LIR interpreter before checking is
considered complete.

This stage consumes:

- the checked module
- the MIR-family lowering pipeline
- LIR after executable ownership solving and reference-count insertion
- explicit compile-time root records
- explicit reification schemas built from the resolved source type and selected
  layout
- explicit callable-result records for roots whose source type is a function
  type

It produces:

- `CompileTimeValueStore`
- one binding entry for each evaluated top-level constant pattern
- promoted closed procedure symbols for top-level callable roots
- serialized constant data for cached modules and imported modules

It must not produce:

- runtime top-level thunks for constants
- runtime global initializer procedures for constants
- runtime zero-argument constant wrappers
- runtime top-level closure objects for top-level bindings
- runtime global callable-value objects for top-level bindings
- generated code that initializes module constants at program startup

Cor lowers top-level values through zero-argument thunks plus global
initializers. Roc must not adopt that model. Roc top-level constants are
compile-time evaluated and reified into compiler-owned constant data.

The compile-time evaluator may synthesize private LIR roots only as interpreter
entrypoints. These roots are `comptime_only` by construction. They are not
exported runtime roots, not module initializers, not user-callable procedures,
and not allowed to survive into backend input.

A private aggregate interpreter root may evaluate multiple constants in
dependency order and return an aggregate value for efficient reification. That
aggregate root is only an implementation detail of compile-time evaluation. The
observable result is the `CompileTimeValueStore`, not a callable procedure.

Compile-time root selection must be explicit. The final design must not use
late syntax filters such as "is this top-level expression shaped like a lambda?"
inside LIR evaluation. Checking finalization or mono MIR must emit a root table:

```zig
const ComptimeRoot = struct {
    module: ModuleId,
    pattern: PatternId,
    expr: ExprId,
    proc: Symbol,
    result: ComptimeRootResult,
    kind: enum {
        serializable_constant,
        callable_binding,
        expect_body,
    },
};

const ComptimeRootResult = union(enum) {
    serializable_schema: ComptimeSchemaId,
    callable_result: CallableResultId,
    expect_result: ExpectRootId,
};
```

The exact Zig names may differ, but the responsibility must not. A root records
which checked/MIR expression is being evaluated, which procedure symbol LIR must
execute, and which schema or callable-result record will reify or promote the
result. Later stages must not recreate this information from source syntax,
expression shape, naming conventions, or environment lookup.

#### Top-Level Callable Finalization

Checking finalization handles top-level callable bindings before publishing the
checked artifact. After publication, every top-level binding whose source type
is a function type is a `procedure_value: Symbol`. There is no post-check
top-level closure-value category.

There are three source-level cases:

1. A top-level function declaration or top-level lambda is already a procedure
   declaration. It does not need to be evaluated by the LIR interpreter just to
   discover that it is callable. Publication records the binding as
   `procedure_value` pointing at the procedure symbol created from the checked
   declaration. If its body references top-level constants, those references
   are resolved through `TopLevelValueTable` after checking finalization:
   serializable constants become `ConstRef` reads and function-valued bindings
   become procedure values.
2. A top-level binding with function source type whose expression is not already
   a function declaration or top-level lambda is a compile-time callable root.
   Checking finalization must evaluate the expression through the same
   MIR-family-to-LIR path used for serializable constants. The interpreter
   result is a compile-time callable value, not serialized constant data.
3. A top-level binding with non-function source type and a supported
   serializable schema is a serializable compile-time constant. It is evaluated
   and reified into `CompileTimeValueStore` as `serializable_constant`.

For example, this binding is case 1:

```roc
add1 : I64 -> I64
add1 = |x| x + 1
```

This binding is case 2:

```roc
makeAdder : I64 -> (I64 -> I64)
makeAdder = |n| |x| x + n

add5 : I64 -> I64
add5 = makeAdder(5)
```

After checking finalization, `add5` must be represented exactly like a
top-level function. Conceptually, later stages see the equivalent of:

```roc
add5 : I64 -> I64
add5 = |x| x + 5
```

The implementation must not depend on source rewriting, but the published
artifact must have the same semantic shape: `add5` is a closed procedure symbol
with the fixed arity from its resolved source function type.

Compile-time callable evaluation may call ordinary top-level functions. Those
functions are consumed as procedure symbols during interpretation, exactly as
they are consumed by later runtime lowering. The interpreter does not evaluate a
top-level function declaration merely because it exists; it only executes
listed compile-time roots and any procedures those roots call.

Compile-time callable roots produce `ComptimeCallable` values:

```zig
const ComptimeCallable = struct {
    source_type: Var,
    entry: CallableEntry,
    captures: CaptureValueSlice,
};

const CallableEntry = union(enum) {
    procedure: Symbol,
    lifted_lambda: Symbol,
    callable_set_member: CallableMemberId,
};

const CaptureValue = union(enum) {
    serializable_constant: ConstRef,
    callable: ComptimeCallableId,
};
```

The exact Zig names may differ, but the meaning must not. A compile-time
callable value is a compiler-owned description of the selected callable entry
and its evaluated captures. It is not a runtime function pointer, not a packed
erased function value, not a heap closure object, not a global initializer, and
not a thunk.

Callable promotion turns each `ComptimeCallable` that is the result of a
top-level binding into a closed top-level procedure symbol before artifact
publication. Promotion must:

- allocate a procedure symbol owned by the checked artifact; exported bindings
  export that promoted symbol, and private bindings keep it private
- give the promoted procedure the exact fixed arity and resolved source function
  type of the top-level binding
- rewrite every capture read in the callable body to an explicit
  `serializable_constant` or `procedure_value`
- recursively promote callable captures to private closed procedure symbols
  before the outer promoted procedure is published
- reject unsupported captured values during checking finalization, before
  publication, instead of leaving them for a post-check stage
- record debug provenance from the promoted symbol back to the original
  top-level binding and callable expression

Serializable captured values are reified into `CompileTimeValueStore` and then
referenced from the promoted procedure through `ConstRef`. Callable captured
values are promoted recursively and then referenced as procedure values.
Aggregate captured values are allowed only when their entire source type has a
supported serializable schema, or when a future explicit callable-containing
constant representation has been designed. The current design must not publish
an aggregate runtime closure environment for a top-level binding.

Promotion is part of checking finalization. If promotion cannot produce a closed
procedure from a top-level function-valued binding, checking has not completed.
After publication, any missing promoted procedure, remaining capture
environment, top-level closure object, runtime callable object, or top-level
callable initializer is a compiler invariant violation handled only by
debug-only assertion in debug builds and `unreachable` in release builds.

The current compile-time value store represents serializable data: ints,
fractions, strings, lists, boxes, tuples, records, tag unions, aliases, and
nominals. Function source types are not accepted as serializable constant
schemas. Callable top-level results use callable promotion and then publish as
`procedure_value`.

Reification must copy runtime interpreter results into compiler-owned constant
nodes. Raw runtime addresses, Roc heap pointers, interpreter arena pointers, and
refcount headers must not survive reification. Lists, strings, boxes, records,
tuples, tags, aliases, and nominals become logical constant nodes addressed by
`(schema_id, value_id)`.

The reification schema is built from the resolved source type and the selected
layout. Both are required:

- the source type gives logical names, wrappers, aliases, nominals, record
  fields, tag names, and payload arity
- the layout gives byte interpretation for the LIR interpreter result

Schema construction must not infer logical row structure from runtime bytes or
physical layout order. Physical layout order may be used only to read bytes after
the logical schema has identified the value being read.

Compile-time evaluation uses the same LIR interpreter ownership boundary as
ordinary interpreter execution. The interpreter executes explicit LIR
`incref`, `decref`, and `free` statements. Compile-time `RocOps` may allocate
temporary Roc heap data during evaluation, but reification must detach the
result from that temporary allocation domain.

Compile-time evaluation problems are checking problems because this stage is
part of checking finalization. A user-written compile-time crash, failed
`expect`, division by zero, numeric conversion failure, or unsupported constant
schema must be reported before `TypeCheckOutput` or equivalent checked artifact
data is returned. If any of those conditions reaches a post-check stage, that is
a compiler invariant violation handled only by debug-only assertion in debug
builds and `unreachable` in release builds.

Cached modules must store and load the serialized `CompileTimeValueStore` as
part of the complete checked module artifact. A downstream module consumes
imported constant bindings from that store. It must not re-run the imported
module's LIR constant roots unless the imported module is being rechecked and
re-finalized.

The checked artifact cache is target-independent. Its key must not include
target ABI/layout inputs. Target ABI/layout inputs belong only in later caches
that store target-shaped data, such as finalized layouts, executable MIR
specializations with target layout commitments, LIR, object code, or constant
materialization output.

The compile-time value store inside a checked artifact is a logical value graph,
not target-shaped storage. Reification copies interpreter results into logical
schema/value nodes. Any later target-specific bytes, statics, layout IDs,
alignment choices, or ownership materialization plans must be produced by a
target-specific post-check cache or by normal lowering for that target.

A checked artifact cache hit must restore checked artifact data and compile-time
values together or miss together. There must not be an independently accepted
compile-time-value sidecar. If the checked artifact says compile-time bindings
exist and the cache cannot restore their value store, the entire checked artifact
cache entry misses before publication.

After checking finalization, later stages may consume compile-time constants as
explicit constant handles or serialized constant data. They must not turn a
missing constant binding into a runtime initializer, attempt LIR interpretation
again as a post-check recovery path, or silently omit a constant.

### Checking Finalization: Artifact Publication

Checking finalization publishes exactly one immutable checked module artifact,
or it publishes nothing.

Publication is the boundary between "the compiler may still report user-facing
checking problems" and "all later stages must succeed unless the compiler has a
bug." A module is not checked merely because its source types solved. A module
is checked only after every checked-stage output required by importers and
post-check lowering has been built, verified, and bundled into the artifact.

Conceptual artifact shape:

```zig
const CheckedModuleArtifact = struct {
    key: CheckedModuleArtifactKey,
    module_identity: ModuleIdentity,
    checking_context_identity: CheckingContextIdentity,
    env: ModuleEnv,
    exports: ExportTable,
    provides_requires: ProvidesRequiresMetadata,
    method_registry: MethodRegistry,
    static_dispatch_plans: StaticDispatchPlanTable,
    root_requests: RootRequestTable,
    hosted_procs: HostedProcTable,
    platform_required_bindings: PlatformRequiredBindingTable,
    interface_capabilities: ModuleInterfaceCapabilities,
    top_level_values: TopLevelValueTable,
    promoted_procedures: PromotedProcedureTable,
    compile_time_roots: ?ComptimeRootTable,
    comptime_values: CompileTimeValueStore,
};
```

The exact Zig names may differ, but the completeness rule must not. The checked
artifact is the unit imported by downstream modules and consumed by public
post-check lowering APIs. Later stages may consume narrowed views of the
artifact, but those views must be derived from this artifact and must not scan
or mutate raw checked modules to rebuild missing semantic data.

Checking finalization order:

1. Finish type solving and collect all user-facing diagnostics.
2. Build the checked method registry and normalized static dispatch plans.
3. Build root requests for runtime, tools, tests, and compile-time evaluation.
4. Build hosted procedure and platform-required binding tables.
5. Build public exports, provides/requires metadata, and interface capability
   records.
6. Evaluate compile-time serializable constants and compile-time callable roots
   through the MIR-family path and LIR interpreter.
7. Reify serializable values into `CompileTimeValueStore`.
8. Promote compile-time callable results to closed procedure symbols.
9. Build `TopLevelValueTable` so every referenced top-level binding maps to
   either `serializable_constant` or `procedure_value`.
10. Store the complete `CompileTimeValueStore`, promoted procedure table, and
    top-level value table in the artifact.
11. Run debug-only artifact verification.
12. Publish the immutable checked artifact.

If any user-facing problem is found in steps 1 through 9, checking reports it
and no artifact is published for that module. Later compiler stages never see a
partial checked artifact.

After publication:

- `ModuleEnv` identity must not be patched.
- hosted indices must not be assigned by mutating checked CIR.
- platform-required lookup targets must not be populated by mutating checked
  modules.
- roots must not be discovered by scanning exports, declarations, or expression
  shapes.
- compile-time values must not be produced by re-running imported module LIR
  roots.
- interface capabilities must not be recreated from imported module bodies.

Missing artifact components after publication are compiler invariant
violations. Debug builds must use debug-only assertions that fail immediately.
Release builds use `unreachable` for the equivalent compiler-invariant path.

### Compile-Time Constant Consumption

The compile-time value store is not merely a cache payload. It is the
post-check input for imported and local serializable constants.

Checking finalization classifies each top-level binding that can be referenced
after checking as one of:

```zig
const TopLevelValueKind = union(enum) {
    serializable_constant: ConstRef,
    procedure_value: Symbol,
};

const TopLevelValueEntry = struct {
    module: ModuleId,
    pattern: PatternId,
    source_type: Var,
    value: TopLevelValueKind,
};

const PromotedProcedure = struct {
    symbol: Symbol,
    source_binding: PatternId,
    source_type: Var,
    provenance: PromotedProcedureProvenance,
};
```

`serializable_constant` is for top-level values whose checked source type has a
supported compile-time schema and whose value has been reified into
`CompileTimeValueStore`.

`procedure_value` is for function declarations and function-valued declarations.
Those do not become runtime zero-argument thunks and do not become serialized
constant data unless a future design adds an explicit serialized function-value
representation. A `procedure_value` symbol may be the original top-level
procedure symbol or a closed procedure symbol produced by compile-time callable
promotion. There is no post-check top-level closure-value category: any
top-level callable result must be promoted before artifact publication.

`TopLevelValueTable` is the only post-check lookup table for top-level values.
Mono MIR, executable MIR, eval, REPL, tests, glue, and CLI helpers must consume
this table. They must not classify top-level values by scanning source
declarations, checking whether an expression is syntactically a lambda, looking
for generated symbol-name patterns, or re-running constant evaluation.

`PromotedProcedureTable` records procedure symbols created from compile-time
callable roots. The promoted procedures live in the same procedure namespace as
ordinary top-level functions after publication. The separate table exists for
debug provenance, artifact verification, and deterministic serialization; it is
not a runtime closure environment and is not a second callable representation.

Conceptual constant handle:

```zig
const ConstRef = struct {
    artifact: CheckedModuleArtifactKey,
    module: ModuleId,
    pattern: PatternId,
    schema: ComptimeSchemaId,
    value: ComptimeValueId,
    source_type: Var,
};
```

The `artifact` field identifies the checked artifact that owns the value store.
The `schema` and `value` fields identify the logical constant node inside that
store. `source_type` records the resolved checked source type used to build the
schema. It is not a request for later stages to reconstruct a schema from bytes,
layout order, expression syntax, or display names.

Mono MIR lookup of a top-level serializable constant emits a constant reference
expression:

```zig
const_ref {
    ref: ConstRef,
    ty: MonoTypeId,
}
```

The exact expression name may differ, but the operation must carry the
`ConstRef`. It must not call a hidden top-level thunk, synthesize a runtime
initializer, or ask the LIR interpreter to run the imported module again.

Executable MIR and LIR materialize a constant reference through an explicit
target-specific materialization plan:

```zig
const ConstMaterializationPlan = struct {
    const_ref: ConstRef,
    target_type: CanonicalExecTypeKey,
    layout: LayoutId,
    ownership: ConstOwnership,
    storage: ConstStoragePlan,
};
```

The materialization plan is target-specific and belongs after checking. It may
choose target bytes, statics, alignment, and layout IDs. It must not change the
logical constant value. If target-specific materialization is cached, that cache
is keyed by the checked artifact key plus target/layout inputs, not by a
separate compile-time value sidecar.

Constant materialization must preserve the ordinary ownership contract:

- immutable static bytes may be referenced only through LIR operations whose
  ownership semantics are explicit
- heap values materialized from constants must have explicit LIR `incref`,
  `decref`, and `free` behavior where needed
- backends must only emit the requested static data or heap setup and follow
  explicit LIR reference-counting statements

Imported constants are consumed only through imported checked artifacts and
their `CompileTimeValueStore`. A downstream module never re-evaluates an
imported module's compile-time roots after that imported artifact has been
published.

### Checked Module Artifact Cache

The checked module cache stores one complete, target-independent checked module
artifact. It must not cache `ModuleEnv` separately from side stores that are
required to use it correctly.

The cached artifact must include every checked-stage output that later stages or
importing modules consume, including:

- `ModuleEnv`
- public exports
- provides/requires metadata after checking finalization
- method registry
- normalized static dispatch call plans
- root request table
- hosted procedure table
- platform-required binding table
- imported/interface representation capabilities
- compile-time root table, if retained for diagnostics or verification
- `CompileTimeValueStore`

The exact names may differ, but the cache hit unit must be the complete checked
artifact. A cache hit restores all of it or none of it.

The cache key for a checked artifact is:

```text
CheckedModuleArtifactKey =
    source_hash
  + compiler_artifact_hash
  + module_identity
  + checking_context_identity
  + direct_import_artifact_keys
```

This is a semantic cache key, not an object-code key and not a target-layout key.
It must not include target ABI, target pointer width, layout IDs, field offsets,
alignment decisions, backend choice, object format, or code-generation options.
Those inputs belong to later target-specific caches only.

#### `compiler_artifact_hash`

`compiler_artifact_hash` is one build-time hash produced by `zig build`.

It replaces separate cache-key fields for:

- compiler semantic version
- compiler implementation identity
- generated builtin module data
- builtin module source and interfaces
- semantic-affecting build options
- checked-artifact serialization format

Those inputs are all part of the compiler artifact. The checked module cache
must not store them as separately compared cache-key fields. If changing any of
those inputs can change checked output, the build-time compiler artifact hash
must change.

Unsupported cache format, invalid bytes, or a cache entry built by a different
compiler artifact is a cache miss before checked artifact data is published. It
is not a recoverable post-check compiler result.

#### `module_identity`

`module_identity` answers: which module is this?

It must include the semantic identity the compiler assigns to the module, for
example:

- package identity
- module name
- qualified module name
- module kind or role, such as app module, package module, platform module,
  platform sibling, hosted module, or builtin module

It must be strong enough that a loaded artifact never needs to patch its module
identity after a cache hit. In particular, a cache hit must not rewrite
`qualified_module_ident` or equivalent identity fields after deserialization.
If the same source text is compiled as two different modules, those are two
different checked artifact keys.

`module_identity` is not necessarily an absolute file path. Use a path only when
the compiler's semantic module identity is path-based. If package/module identity
is defined by package metadata plus module name, the key should use that
semantic identity instead of incidental filesystem spelling.

#### `checking_context_identity`

`checking_context_identity` answers: under which external checking context was
this module checked?

It includes name-resolution and role information that can affect checked output
but is not the source text itself and not the imported artifacts' contents.
Examples include:

- package shorthand mapping, such as `pf -> concrete package identity`
- source import name to resolved module identity mapping
- import alias information when the alias changes visible names or exported
  checked artifact data
- auto-import policy
- builtin import policy
- platform/app relationship for this check
- hosted/platform ABI context used during checking
- provides/requires configuration that affects checked module output

For example, this source is not enough to identify the checked result:

```roc
import pf.Stdout
```

The source text says `pf.Stdout`, but the meaning depends on which concrete
package identity `pf` resolves to in this build. Two builds with the same text
and different shorthand resolution must not share a checked artifact.

`checking_context_identity` records the resolution context. The imported
artifact keys record the checked artifacts that were resolved.

#### `direct_import_artifact_keys`

The key must include checked artifact keys for each direct import, not source
hashes of direct imports.

This gives transitive invalidation by construction:

```text
A imports B
B imports C
```

If `C` changes, then `C`'s checked artifact key changes. Because `B`'s key
contains `C`'s key, `B`'s checked artifact key changes even if `B.roc` text is
unchanged. Because `A`'s key contains `B`'s checked artifact key, `A` misses too.

Using only direct imported source hashes is insufficient. It misses changes
where an imported module's own source text is unchanged but its checked artifact
changes because one of its imports, context identities, or compiler artifact
inputs changed.

The direct import list must be deterministic. It should be keyed by the checked
module's resolved import records, not by hash-map iteration order. If the same
artifact is imported through two source import entries with different visible
names or roles, the key must preserve those distinct import records through
`checking_context_identity`.

#### Compile-Time Values In The Cache

Compile-time constants are part of checking finalization, so the checked artifact
is incomplete without its `CompileTimeValueStore`.

The cache must not accept an entry that restores checked declarations but omits
the compile-time value store required by those declarations. It also must not
accept a compile-time value store whose checked artifact is missing or whose key
does not match the checked artifact key.

The store is logical and target-independent:

```text
schema_id + value_id -> logical constant node
```

It is not:

```text
target bytes
layout IDs
field offsets
alignment-specific records
backend static-data symbols
```

Target-specific constant materialization happens after checking, in the
target-specific lowering pipeline. If that materialization is cached, that is a
different cache keyed by the checked artifact key plus target/layout inputs.

#### Cache Misses And Published Artifacts

Before publication, these conditions are cache misses:

- missing cache entry
- unsupported cache format
- invalid serialized bytes
- mismatched `compiler_artifact_hash`
- missing required artifact component
- missing `CompileTimeValueStore` for an artifact with compile-time bindings
- failed deserialization

After a checked artifact has been published to later compiler stages, missing
components are not recoverable. They are compiler invariant violations: debug-only
assertion in debug builds and `unreachable` in release builds.

## Final Data Structures

### Checked Artifact Boundary

Every post-check public pipeline consumes checked artifacts, not loose checked
modules plus optional side stores.

The compiler may expose restricted views for specific consumers:

```zig
const ImportedModuleView = struct {
    key: CheckedModuleArtifactKey,
    module_identity: ModuleIdentity,
    exports: ExportTableView,
    method_registry: MethodRegistryView,
    interface_capabilities: ModuleInterfaceCapabilitiesView,
    comptime_values: CompileTimeValueStoreView,
};

const LoweringModuleView = struct {
    artifact: *const CheckedModuleArtifact,
    roots: RootRequestSet,
};
```

Those views are read-only projections. They must not contain mutable pointers
that allow later stages to patch `ModuleEnv`, hosted indices,
platform-required bindings, compile-time values, or interface capability
records.

Downstream modules import `ImportedModuleView`. They do not inspect another
module's unchecked source, checked expression bodies, opaque backing syntax, or
private definitions to complete their own checked artifact.

The executable pipeline consumes `LoweringModuleView` values. It may lower only
roots named by `RootRequestSet`. It must not discover additional roots by
scanning exports, source declarations, checked CIR expression shapes, or
procedure order.

### Root Requests And Root Binding

Root selection happens during checking finalization or build planning before
post-check lowering begins.

A root request records that a particular checked artifact requires a runtime,
tool, test, REPL, development, or compile-time entrypoint. It is not a lowered
procedure yet.

Conceptual pre-MIR request shape:

```zig
const RootProcedureRequest = struct {
    id: RootRequestId,
    kind: RootKind,
    module: ModuleId,
    source: RootSource,
    checked_type: Var,
    abi: RootAbi,
    exposure: RootExposure,
    order: RootOrderKey,
};

const RootKind = enum {
    app_main,
    platform_required,
    provided_export,
    hosted_export,
    test_expect,
    repl_expr,
    dev_expr,
    comptime_constant,
    comptime_expect,
};

const RootSource = union(enum) {
    top_level_def: DefId,
    statement: StatementId,
    expression: ExprId,
    hosted_proc: HostedProcId,
};
```

`RootKind` is semantic. It says why this root exists. It must not be recovered
from display names such as `main`, from the last lowered procedure, from
whether an expression syntactically looks like a lambda, or from which tool is
currently calling the pipeline.

`RootSource` points at checked source ownership. It does not name an executable
procedure. Mono MIR binds the request to a mono procedure or constant by
lowering the source in the appropriate specialization.

Conceptual lowered binding shape:

```zig
const LoweredRoot = struct {
    request: RootRequestId,
    target: RootTarget,
};

const RootTarget = union(enum) {
    proc: Symbol,
    const_ref: ConstRef,
};
```

Runtime roots bind to procedures. Serializable compile-time constants bind to
`ConstRef` values after evaluation. Private aggregate interpreter roots may bind
to procedures while compile-time evaluation is running, but those roots are
`comptime_only` and must not be exported as runtime roots.

No MIR, IR, LIR, eval, REPL, snapshot, CLI, glue, or test helper may select a
root by:

- scanning `provides` entries
- scanning top-level declarations for a name
- filtering expressions by syntax
- comparing expression IDs for equality after lowering
- choosing the last procedure in a root list
- searching for a hosted lambda expression
- inferring root kind from a generated symbol name

If a command requires a root and checking/build planning cannot create one, that
is a user-facing checking or build-planning diagnostic before artifact
publication. If a missing root reaches mono MIR or later, it is a compiler
invariant violation handled by debug-only assertion in debug builds and
`unreachable` in release builds.

### Hosted And Platform Tables

Hosted procedure discovery and platform-required binding happen during checking
finalization. They produce artifact tables. They do not mutate CIR after the
artifact boundary.

Conceptual hosted table:

```zig
const HostedProcTable = struct {
    entries: Span(HostedProcEntry),
    by_source: Map(HostedProcSource, HostedProcId),
    by_abi_name: Map(HostedAbiName, HostedProcId),
};

const HostedProcEntry = struct {
    id: HostedProcId,
    module: ModuleId,
    def: DefId,
    expr: ExprId,
    symbol_name: Ident.Idx,
    abi_name: HostedAbiName,
    order: HostedOrderKey,
    representation: HostedRepresentationCapabilityKey,
    ownership_contract_template: ProcOwnershipContractTemplate,
};
```

`HostedOrderKey` gives deterministic ordering for hosted procedure tables and
generated ABI lists. It is not stored by writing an index into checked CIR.

The final architecture must delete or make non-authoritative any
`e_hosted_lambda.index`-style field. If a syntax node still carries a placeholder
for diagnostics while checking is running, post-check stages must ignore it and
consume `HostedProcTable`.

Conceptual platform-required binding table:

```zig
const PlatformRequiredBindingTable = struct {
    entries: Span(PlatformRequiredBinding),
    by_platform_required: Map(RequiredTypeId, PlatformRequiredBindingId),
};

const PlatformRequiredBinding = struct {
    id: PlatformRequiredBindingId,
    platform_required: RequiredTypeId,
    platform_ident: Ident.Idx,
    app_module: ModuleId,
    app_def: DefId,
    app_pattern: PatternId,
    checked_relation: PlatformRequirementRelationId,
};
```

This table replaces post-check lookup-target population. It records exactly
which app binding satisfies each platform requirement and which checked relation
proved it. Mono MIR consumes the table when producing roots and calls for
platform-required functions.

Hosted and platform methods that can be called through static dispatch must
also appear in the checked method registry as explicit procedure targets with
checked callable types. The hosted/platform tables provide ABI identity,
ordering, representation capability keys, and ownership templates. The method
registry provides method lookup identity. Later stages must consume both
explicit records as needed; they must not rediscover hosted/platform behavior
from names, expression shapes, or module scans.

### Interface Capability Publication

Representation capabilities are compiler-private interface records published in
the checked artifact.

Conceptual shape:

```zig
const ModuleInterfaceCapabilities = struct {
    boxed_payload_templates: BoxPayloadCapabilityTable,
    opaque_atomic_proofs: OpaqueAtomicProofTable,
    hosted_representations: HostedRepresentationCapabilityTable,
    platform_representations: PlatformRepresentationCapabilityTable,
    exported_nominal_representations: NominalRepresentationTable,
};
```

The defining module produces these records while it still has legal access to
its checked definitions and private nominal backing information. Importing
modules consume only the published capability records through
`ImportedModuleView`.

An importer must not:

- copy opaque backing bodies into its own semantic lowering path
- inspect imported private definitions
- infer transparent representation from display names
- use layout shapes as proof of source-level representation
- synthesize hosted or platform callable representation behavior from ABI names

Lambda-solved MIR and executable MIR consume capability records by key. Missing
capability records after artifact publication are compiler invariant
violations. Debug builds assert immediately. Release builds use `unreachable`.

If an imported opaque, hosted, platform, or cross-module transparent nominal
inside an explicit `Box(T)` erased payload requires traversal and no exact
capability or exact `opaque_atomic` proof exists, checking finalization reports
the problem before publishing the importing artifact. Later stages do not
recover by treating the value as erased, atomic, indirect, or runtime-converted.

### Public Pipeline API

There is one public semantic lowering entrance from checked artifacts to LIR.

Conceptual API:

```zig
pub const LowerResourceError = std.mem.Allocator.Error;

pub fn lowerArtifactsToLir(
    allocator: Allocator,
    artifacts: ArtifactSet,
    roots: RootRequestSet,
    target: TargetConfig,
) LowerResourceError!LoweredProgram;
```

The exact name may differ, but the contract must not. Public callers provide
published checked artifacts, explicit roots, and target configuration. They get
lowered output or resource failure. They do not get semantic failure variants.

The following clients must call this pipeline instead of hand-assembling old
stage chains:

- build runner
- CLI commands
- eval pipeline
- REPL
- dev shim
- interpreter shim
- snapshot tool
- glue
- test helpers

Tooling is not an exception to the architecture. A REPL expression or dev
expression becomes a temporary checked artifact with an explicit `.repl_expr` or
`.dev_expr` root. Tests may construct small artifacts directly, but they must
still call the same checked-artifact-to-LIR public pipeline when testing
semantic lowering.

Forbidden public APIs after the cutover:

```text
lowerTypedCIRToLir*
lowerTypedCIRToSemanticEval*
public monotype -> monotype_lifted -> lambdasolved -> lambdamono chains
public helpers that accept checked CIR plus an optional root name
public helpers that return NoRootProc or NoRootDefinition
```

Internal stage tests may call an individual MIR pass only when they construct
that pass's exact input type-state. Those helpers must not become compatibility
entrypoints for tools.

### Post-Check API Error Shape

Post-check semantic lowering APIs must not return semantic errors.

Forbidden public post-check API shapes:

```zig
pub fn lower(...) !T
pub fn lower(...) anyerror!T
pub fn lower(...) SemanticLowerError!T
```

unless the named error set contains only resource errors such as
`Allocator.Error`.

Allowed public post-check API shape:

```zig
pub const LowerResourceError = std.mem.Allocator.Error;

pub fn lowerExecutableMir(...) LowerResourceError!IrProgram;
```

Forbidden post-check semantic errors include:

```text
NoRootProc
NoRootDefinition
MissingRoot
MethodNotFound
DispatchOwnerNotFound
UnsupportedSourceType
UnsupportedLayout
SchemaLayoutMismatch
AbstractSchemaType
MissingCompileTimeValue
MissingInterfaceCapability
MissingHostedProc
MissingPlatformRequiredBinding
```

Those conditions must either be reported before checked artifact publication or
be treated as compiler invariant violations after publication.

The implementation shape for post-check invariants is:

```text
debug build: debug-only assertion
release build: unreachable
```

Cache invalidity, invalid serialized bytes, and unsupported cache format are
pre-publication cache misses. They are not semantic lowering errors. File I/O,
backend executable availability, and operating-system failures in command-line
wrappers may still be ordinary tool/resource errors, but they must not select a
different semantic lowering path.

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
symbol, expression id, side-table id, or a payload-free origin kind.

All key-like fields above are canonical keys, not handles into mutable stores.
`CanonicalCallableSetKey` is the canonical ordered finite member map plus capture
slot shape and capture types. `ErasedFnSigKey` is the canonical fixed-arity
erased function argument and return signature plus `ErasedCallAbiPolicyKey`.
`CaptureShapeKey` is the canonical `CaptureSlot.index` ordered capture layout
and capture types.
`ProcBaseKeyRef` is a canonical reference to an already-keyed procedure.
`BridgeReason`, `IntrinsicId`, and entry wrapper root names must be stable enum
or source identities, never generated symbol text.

`ExecutableSpecializationKey` is the semantic key for executable specialization
deduplication. It contains `ProcBaseKey` and canonical fully resolved structural type
keys. It must not contain `ProcOrderKey`, raw type-store ids, expression ids,
side-table ids, or allocation-order-dependent data.

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
their `requested_fn_ty`. Checking must report missing arguments before MIR
export; they are not requests to synthesize partial applications. Checking must
also report extra arguments unless the source explicitly calls the result of a
function that returns another function.

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

These are debug-only compiler assertions. They must fire immediately in debug
builds and verifier builds when a compiler invariant is violated. They must not
generate runtime checks in user programs, and release compiler builds must not
pay for them when those checks are unnecessary assuming the compiler is correct.

Verifier checks must not mutate production compiler state.

If a verifier needs unification-like logic, it must run on a cloned scratch type
store or compare already fully resolved/canonicalized types. The real stage lowering or
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
    id: CallableMatchId,
    callable_set_key: CanonicalCallableSetKey,
    func_expr: ExprId,
    arg_exprs: Span(ExprId),
    func_tmp: TempId,
    arg_temps: Span(TempId),
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

The registry must be built from checked declaration outputs. It must not rely on
late text lookup or module-name scanning during MIR lowering.

Checker validation and mono MIR lowering must agree through this registry.

The checker may keep `StaticDispatchConstraint` as the legality mechanism, but
checked CIR must export a normalized `StaticDispatchCallPlan` for every
expression classified as static dispatch. The plan stores the dispatcher type
variable, callable type variable, method identifier, ordered value arguments, and
equality behavior. It does not store a final target procedure.

Mono MIR resolves every static dispatch target from:

```text
the checked method registry
+ the plan's dispatcher type variable after mono instantiation and full type-link resolution
+ the plan's method identifier
```

That is the only target-selection path. There must not be one checker lookup
path and a second mono MIR lookup path that can disagree, and there must not be
separate post-check paths for ordinary method syntax, type-variable qualified
syntax, or equality syntax.

The registry returns checked method target identity and ABI metadata needed to
create a `ProcTarget`, not a final executable procedure.

Mono MIR lowering must pass the selected method target and the exact requested
mono source function type through the mono specialization queue or wrapper
synthesis path. The queue returns the mono-specialized source/MIR procedure
symbol stored in `call_proc`.

Before mono MIR output is exported, every callable method target must be
normalized to a procedure symbol with `ProcTarget` metadata:

- ordinary source methods become `ProcTarget.user_proc`
- hosted/platform methods become `ProcTarget.hosted_proc`
- first-class intrinsic method references synthesize a wrapper procedure and
  become `ProcTarget.intrinsic_wrapper`

The `ProcTarget` metadata must include representation ABI records and ownership
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

### Dispatch Type Resolution

The method owner is the semantic type identity used as the first component of a
method registry key. It is not runtime ownership, reference-count ownership, or
value ownership.

Dispatch type resolution takes a monomorphic type from `StaticDispatchCallPlan`,
never an expression:

```zig
fn methodOwnerForDispatcherType(types: *const MonoTypeStore, ty: MonoTypeId) MethodOwner
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

Forbidden owner cases are compiler invariant violations handled only by
debug-only assertion in debug builds and `unreachable` in release builds. They
are not fallback paths.

Delete expression-based owner APIs, including the current family represented by:

```text
attachedMethodOwnerForExpr
resolveAttachedMethodTargetFromExpr
ownerForExpr
resolveTargetFromExpr
```

Chained dispatch works by lowering the receiver expression first and using the
`StaticDispatchCallPlan.callable_var` result type for the already-lowered call.

The next dispatch in a chain has its own `StaticDispatchCallPlan`. That plan's
`dispatcher_var` must be the checked type variable selected by the checker for
that dispatch site. If that variable is the result of an earlier call, mono MIR
gets the value by instantiating the plan's type variable in the current mono
specialization, not by inspecting the earlier expression's syntax.

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

Calling this transform on a non-`Box(T)` root is a compiler invariant violation
handled only by debug-only assertion in debug builds and `unreachable` in
release builds.

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
side-table handles
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
- finalized `RecordFieldId.logical_index` values
- tuple fields
- finalized `TagId.logical_index` values
- finalized `TagPayloadId.payload_index` values
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
explicit checked or MIR row record before layout lowering consumes it. Row
finalization is the last stage that may use a source/display name to select a
logical row ID. Later stages must consume the finalized IDs directly and must
not use names to look them up again.

### Row Finalization Pass

Open record and tag-union rows must be finalized by a dedicated mono-MIR pass
before lifting and before representation solving.

This pass is required for correctness. It is not merely a debug verifier, and it
must not be replaced by lazy lookup inside a later lowering pass.

The pass consumes one complete mono specialization at a time:

- dispatch-free mono MIR
- the specialization-local mono type store
- checked declaration metadata
- checked canonical source-ordering rules

The pass requires every row type it consumes to be fully resolved in the
specialization-local mono type store. Checked types are inputs, but checked
types are not sufficient by themselves because open rows, aliases, and
specialized type variables must be resolved in the current mono specialization
before logical row identity can be assigned.

The pass produces row-finalized mono MIR. Later stages consume only this
row-finalized type-state.

The implementation must be compact. It must intern each unique logical row shape
once, and row-finalized MIR nodes must store small IDs into that interned shape
store. It must not duplicate the full row shape on every expression.

The pass may walk mono specializations one at a time while writing into a
module-wide or compilation-unit-wide interner, as long as the IDs remain stable
for all later stages. The shape interner must store only logical labels and
payload arity. It must not copy field types, payload types, expression IDs, or
per-use metadata into shape keys.

Conceptual shape store:

```zig
const RowShapeStore = struct {
    records: InternMap(RecordShapeKey, RecordShapeId),
    record_fields: Store(RecordFieldInfo),

    tag_unions: InternMap(TagUnionShapeKey, TagUnionShapeId),
    tags: Store(TagInfo),
    tag_payloads: Store(TagPayloadInfo),
};

const RecordShapeKey = struct {
    fields: Span(Ident.Idx),
};

const RecordFieldInfo = struct {
    owner: RecordShapeId,
    field_ident: Ident.Idx,
    logical_index: u32,
};

const TagUnionShapeKey = struct {
    tags: Span(TagShapeKey),
};

const TagShapeKey = struct {
    tag_ident: Ident.Idx,
    payload_count: u32,
};

const TagInfo = struct {
    owner: TagUnionShapeId,
    tag_ident: Ident.Idx,
    logical_index: u32,
};

const TagPayloadInfo = struct {
    tag: TagId,
    payload_index: u32,
};
```

The exact Zig field names may differ, but the identity model must not. Shape
keys describe logical row order and payload arity only. They do not include
payload slot types. Payload slot types remain in the mono type store and later
representation edges, because the same logical row shape can appear at multiple
type instantiations.

The row-finalization algorithm is:

1. Walk every expression and pattern in one mono specialization exactly once.
2. For each record construction, record access, record update, record
   destructuring pattern, tag construction, tag pattern, and tag payload
   projection, read the operation's mono result type or input type from the mono
   MIR node.
3. Fully resolve that type in the specialization-local mono type store.
4. Derive the full logical record or tag-union row from that resolved type.
5. Intern the full logical row shape in `RowShapeStore`.
6. Validate that the requested field or tag exists in that full row, and that
   the payload count in the operation matches the full row's constructor arity.
7. Rewrite the MIR node in place, or into a new row-finalized store, so it
   stores `RecordShapeId`, `RecordFieldId`, `TagUnionShapeId`, `TagId`, and
   `TagPayloadId` values instead of name-only row keys.
8. Attach or preserve the mono type slot for each field or payload edge so
   representation solving can connect value flow without looking names up again.
9. Export only row-finalized mono MIR.

Caching is allowed only inside this pass. A cache key must be the canonical
fully resolved row shape, not the first source expression that happened to use a
field or tag. The cache is an implementation detail of producing finalized IDs;
it must not be exposed as a later-stage lookup helper.

Name sorting is permitted only inside row finalization if the checked type
system defines that as the canonical source-level order. The output of that sort
is the interned shape and finalized ID set above. After row finalization, names
are diagnostic text and checked lookup keys only; they are not representation or
layout identity.

Representation merge consumes `RecordFieldId`, `TagId`, and `TagPayloadId`. It
must not use display names, sorted name order, source expression shape, or
physical layout position.

Debug verification after row finalization must assert if any row-finalized mono
MIR node still has a name-only row operation. Debug verification in later stages
must assert if any record, tag-union, tag constructor, tag payload, pattern, or
projection reaches representation solving without finalized row IDs. It must
also assert if a later stage attempts to compute a logical index by sorting
names, scanning a row, scanning expressions, or inspecting physical layout
order. The equivalent release-build compiler-invariant path is `unreachable`.

These verifications are debug-only assertions. They are not part of normal
release compiler runtime cost, and release builds must still be correct because
the row-finalized MIR type-state cannot represent name-only row lookup.

## Static Dispatch Lowering

Surface syntax does not determine whether an expression is static dispatch.
For example, this source form is only a qualified function call syntactically:

```roc
Fmt.decode_str(format, source)
```

It might refer to a module function, or it might be a static-dispatch call whose
dispatcher type variable is constrained by the checked `where` clause. Parser
and canonicalization must not decide that from the spelling. Name resolution and
type checking classify it.

Checked CIR may contain these source-level forms while type checking:

```text
e_dispatch_call
e_type_dispatch_call
e_method_eq
```

Before mono MIR lowering consumes checked CIR, every checked static-dispatch
expression must export exactly one normalized plan:

```zig
StaticDispatchCallPlan {
    expr: CIR.Expr.Idx,
    method_ident: Ident.Idx,
    dispatcher_var: Var,
    callable_var: Var,
    args: Span(CIR.Expr.Idx),
    result_mode: StaticDispatchResultMode,
}

const StaticDispatchResultMode = union(enum) {
    value,
    equality: EqualityDispatchMode,
};

const EqualityDispatchMode = struct {
    negated: bool,
    structural_allowed: bool,
};
```

The exact Zig field names may differ, but the semantic shape must not differ.
There is no post-check resolved-dispatch record with a preselected target
procedure, no separate owner-selection field, and no separate downstream
representation for ordinary method syntax, type-variable qualified syntax, or
equality syntax.

`dispatcher_var` is the checked type variable whose fully resolved monomorphic type determines
method lookup. It may come from any part of the checked constraint: the first
argument, a later argument, the return value, or a variable that appears only in
the `where` constraint. Mono MIR must consume this explicit variable. It must
not rediscover it from argument order, result position, receiver syntax, a
qualified-name prefix, a method name, a module environment lookup, or
display-name sorting.

`callable_var` is the checked fixed-arity function type for the operation. Roc
functions have fixed arity and are not automatically curried. Therefore the
arity of `callable_var` and the number of normalized `args` must match exactly.

`args` are the actual value arguments in final call order:

```roc
x.foo(a, b)
```

normalizes to:

```text
args = [x, a, b]
```

If checked name resolution proves that `Fmt` is a type-variable alias rather
than a module, then:

```roc
Fmt.decode_str(format, source)
```

normalizes to:

```text
args = [format, source]
```

Equality normalizes the same way:

```roc
x == y
x != y
```

normalizes to:

```text
method_ident = is_eq
args = [x, y]
result_mode = equality { negated = false, structural_allowed = ... } // for ==
result_mode = equality { negated = true,  structural_allowed = ... } // for !=
```

`result_mode.value` emits a method call. `result_mode.equality` emits a custom
`is_eq` call when mono finds one; if mono finds no custom method and
`structural_allowed` is true, it emits structural equality. `!=` is represented
only by `negated = true`, and mono emits the same equality operation followed by
`bool_not`.

A concrete equality expression that checking proves is always structural may be
rewritten directly to `structural_eq`. A generic equality expression must keep a
`StaticDispatchCallPlan` with `result_mode.equality.structural_allowed = true`
so mono decides per specialization after `dispatcher_var` has been instantiated
and fully resolved.

### Method Lookup In Mono

Mono MIR lowering uses one algorithm for every `StaticDispatchCallPlan`:

1. Instantiate `dispatcher_var` and `callable_var` into the same mono type store
   with the same clone-instantiation mapping as the expression.
2. Connect the instantiated callable return slot to this expression's
   instantiated mono result type. This must happen before method lookup because
   `dispatcher_var` may be selected from the return position.
3. Lower all `args` in the normalized order, using the instantiated callable
   argument slots as expected types.
4. Fully resolve the instantiated dispatcher type.
5. Resolve `MethodOwner` from the fully resolved dispatcher type.
6. Look up `(MethodOwner, method_ident)` in the checked method registry.
7. If a target exists, instantiate the target procedure type into the same mono
   type store.
8. Unify the instantiated target procedure type with the instantiated callable
   type. The unified function type is the exact requested mono source function
   type for this call.
9. Request or reserve the target mono specialization at that exact requested
   function type.
10. Emit `call_proc` with:
   - `proc` equal to the mono-specialized source/MIR procedure symbol
   - `args` equal to the lowered normalized args
   - `requested_fn_ty` equal to the unified requested mono source function type
11. If no target exists and `result_mode.equality.structural_allowed` is true,
    emit `structural_eq` using the lowered normalized args and the instantiated
    equality argument types.
12. If `result_mode.equality.negated` is true, emit `bool_not` after the custom
    call or structural equality operation.

If lookup is missing for `result_mode.value`, or if lookup is missing for
`result_mode.equality` while `structural_allowed` is false, that is a compiler
invariant violation. Checking must have reported invalid dispatch before mono
MIR begins. Mono debug verification must assert rather than inventing a target;
the equivalent release-build path is `unreachable`.

If `dispatcher_var` does not fully resolve to one allowed `MethodOwner` after the
callable arguments and return slot have been connected, that is also a compiler
invariant violation. A dispatch site whose controlling type cannot be
determined from the checked callable type and enclosing expression type is
ambiguous; checking must have reported it before mono MIR begins. The
post-check path is debug-only assertion in debug builds and `unreachable` in
release builds.

No later stage sees the method name as an unresolved call. No stage treats this
as an executable direct call until executable MIR.

For chained dispatch:

```roc
x.foo().bar()
```

the `bar` expression has its own `StaticDispatchCallPlan`. Mono lowers
`x.foo()` first, unifies the target procedure type with `foo`'s callable type,
and uses that unified return slot as the receiver expression type for the
surrounding expression. The `bar` plan still supplies its own `dispatcher_var`.
Mono must not use the pre-target constraint approximation from `foo` as the
dispatcher for `bar`.

### Method Registry

The checked method registry maps:

```zig
MethodKey {
    owner: MethodOwner,
    method_ident: Ident.Idx,
}
```

to procedure targets with checked callable types.

If the checked method registry contains a hosted, platform, or intrinsic method
entry, checking must normalize it to an explicit builtin procedure target with
a checked callable type before mono consumes the registry. Mono still emits
`call_proc` to a procedure symbol when a target exists. It must not special-case
a method name as an intrinsic after static-dispatch lookup.

A dotted expression without arguments is checked field access. It is not an
unresolved static method value and must not be treated as one later.

The registry is only a target table. It does not choose which type controls a
particular call. `StaticDispatchCallPlan.dispatcher_var` chooses that.

## Tags And Constructors

Tag names remain symbolic only until row finalization.

Logical `TagId` and `TagPayloadId` values are created by the row-finalization
pass. Physical layout indices are still not available in mono MIR; layout
lowering later translates finalized logical IDs through the explicit layout
store.

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

Logical discriminants and payload indexes may be computed only by row
finalization from the full mono MIR tag-union type. They must not be computed
from a singleton type invented from syntax, and they must not be computed lazily
inside representation solving, executable lowering, IR lowering, or layout
lowering.

Row-finalized mono MIR tag operations must carry finalized `TagUnionShapeId`,
`TagId`, and `TagPayloadId` records. Later stages may translate those logical
indexes to executable layout indexes, but they must not create a new constructor
order by sorting names, scanning rows, or scanning expressions.

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

Row-finalized mono MIR:

- interns logical record and tag-union row shapes once per unique shape
- rewrites record and tag operations to finalized row IDs
- preserves the mono type for every expression, field edge, and payload edge
- deletes name-only row lookup before lifting
- exports no API for later stages to compute logical row indexes from names

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
- emits explicit `BoxBoundary` records with box type, payload source type,
  payload boundary type, direction, representation roots, and
  `BoxPayloadRepresentationPlan`
- propagates boxed payload representation requirements through aliases, binders,
  captures, parameters, returns, and expression occurrences
- propagates boxed payload representation requirements through branch joins,
  source `match` condition/pattern edges, pattern binders, projections, and
  returned values
- solves boxed payload representation only in the specialization-local
  lambda-solved type store after clone-instantiation and full type-link
  resolution
- emits module-interface representation capability templates and instantiated
  capabilities for boxed payload traversal through imported or opaque nominals
- consumes hosted/platform callable representation metadata instead of inferring
  it from hosted symbol names or layouts
- exposes fully resolved callable representations for `call_value.requested_fn_ty`,
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
- preserves solved ownership records for `call_direct`, `call_erased`,
  `callable_match`, callable-set values, `packed_erased_fn`, erased adapters,
  capture records, `Box.box`, `Box.unbox`, and bridges so IR/LIR can preserve
  them for `RcInsert`

The following are forbidden:

- callable truth in environment side fields
- callable truth in expression side tables
- capture truth in side-table-id arrays
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
legacy value side-table records
legacy callable side-table records
semantic side-table-id usage
expression-indexed side-table maps
exact_callable_aliases as semantic truth
expression-to-callable-target lookup helper
expression-to-callable-captures lookup helper
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
Cor-style runtime top-level constant thunks
runtime global initializer procedures for compile-time constants
runtime zero-argument constant wrappers
runtime top-level closure objects for top-level bindings
runtime global callable-value objects for top-level bindings
public lowerTypedCIRToLir entrypoints
public lowerTypedCIRToSemanticEval entrypoints
NoRootProc and NoRootDefinition as post-check lowering results
post-check root discovery by export/name/expression/procedure-order scan
post-check hosted-index mutation in checked CIR
post-check platform-required lookup-target mutation
imported representation recovery from private bodies or opaque backing syntax
imported compile-time constant re-evaluation after artifact publication
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

All public executable pipelines must call:

```text
checked CIR -> mir.mono -> mir.mono.row_finalize -> mir.lifted -> mir.lambda_solved -> mir.executable -> ir -> lir
```

Compile-time constant evaluation must call the same MIR-family lowering path and
then run the LIR interpreter during checking finalization:

```text
checked CIR -> mir.mono -> mir.mono.row_finalize -> mir.lifted -> mir.lambda_solved -> mir.executable -> ir -> lir -> LIR interpreter -> compile-time value store
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

### 2. Build Checked Method Registry And Dispatch Plans

Add `src/check/static_dispatch_registry.zig`.
Add the checked representation for `StaticDispatchCallPlan`.

Build a registry from checked modules before checked dispatch plans are exported
to mono MIR.

The registry must map `MethodKey { owner: MethodOwner, method_ident }` to
procedure targets. `MethodOwner` must be explicit semantic type identity, not
expression shape and not display-name lookup. Hosted, platform, or intrinsic
method entries must be normalized to explicit builtin procedure targets with
checked callable types before mono consumes the registry.

Use the registry only as the target table for mono lookup. The registry must not
choose the dispatcher type for a call. The checked dispatch plan chooses that
with `dispatcher_var`.

Each checked dispatch expression that remains after type checking must store:

```text
StaticDispatchCallPlan.expr
StaticDispatchCallPlan.method_ident
StaticDispatchCallPlan.dispatcher_var
StaticDispatchCallPlan.callable_var
StaticDispatchCallPlan.args
StaticDispatchCallPlan.result_mode
```

`dispatcher_var` must be selected by checked name resolution and type checking
from the operation's semantic constraint. It must not be inferred later from
syntax. The same representation is used whether the dispatcher type appears in
the first argument, a later argument, the return value, or only in the `where`
constraint.

A concrete equality expression that checking proves is always structural may be
rewritten to `structural_eq`. A generic equality expression must keep
`StaticDispatchCallPlan.result_mode.equality.structural_allowed = true` so mono
can decide per specialization after instantiation and full type-link resolution of
`dispatcher_var`.

Then remove downstream `attached_method_index` threading.

### 3. Publish Checked Artifact Boundary Records

Introduce the checked artifact publication boundary before rewiring individual
lowering stages.

Add the checked artifact type and read-only views:

```text
CheckedModuleArtifact
ImportedModuleView
LoweringModuleView
```

The artifact must contain `ModuleEnv`, exports, provides/requires metadata,
method registry, static dispatch plans, root requests, hosted procedure table,
platform-required binding table, interface capabilities, compile-time roots if
retained, and `CompileTimeValueStore`.

This step must also introduce explicit table shapes for:

```text
RootRequestTable
HostedProcTable
PlatformRequiredBindingTable
ModuleInterfaceCapabilities
```

Do not migrate callers by adding compatibility adapters. Instead, create the new
records and make checking finalization populate them before any public
post-check pipeline can consume the module.

Root requests must be produced for app entrypoints, provided exports,
platform-required bindings, hosted exports, tests, REPL/dev expressions, and
compile-time constants. Root requests must record kind, source, checked type,
ABI, exposure, and deterministic order. They must not be inferred later from
export scans, declaration names, expression shapes, or procedure order.

Hosted procedures must be collected into `HostedProcTable`. Deterministic hosted
ordering belongs in that table. Do not write hosted indices into checked CIR as
authoritative post-check data.

Platform-required bindings must be collected into
`PlatformRequiredBindingTable`. Do not populate lookup targets by mutating
checked module environments after publication.

Interface capabilities must be published in `ModuleInterfaceCapabilities`.
Importers must consume these records through `ImportedModuleView`; they must not
inspect imported private definitions or recreate representation capability data.

Commit when checked artifacts can be constructed with these records and debug
verification proves:

- a published artifact has every required component
- artifact views are read-only
- no post-publication code path patches module identity
- no post-publication code path writes hosted indices into checked CIR
- no post-publication code path populates platform-required lookup targets by
  mutating checked modules
- root requests exist before MIR lowering starts
- missing root requests are reported before artifact publication or asserted as
  compiler bugs after publication

### 4. Harden Mono MIR AST

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

to `call_proc` calls or `structural_eq` immediately by consuming checked
`StaticDispatchCallPlan` values plus the checked method registry. It must not
choose the dispatcher variable from expression shape, receiver position, result
position, method name, or module environment lookup.

For every static-dispatch-produced `call_proc`, mono MIR must instantiate the
plan's `dispatcher_var` and `callable_var` into the current mono type store,
connect the callable return slot to the expression's instantiated mono result
type, lower normalized args through the callable arg slots, fully resolve the
dispatcher type, resolve `MethodOwner` from that type, look up `(MethodOwner,
method_ident)` in the checked method registry, instantiate the target procedure
type into the current mono type store, unify it with the instantiated callable
type, use the unified argument and return slots as the call's requested mono
source function type, and reserve the target mono specialization at exactly that
type before exporting mono MIR.

For static-dispatch equality without a custom target, mono may emit
`structural_eq` only when `StaticDispatchCallPlan.result_mode.equality`
explicitly allows structural equality. `!=` must emit `bool_not` after the
custom call or structural equality operation.

Commit when mono MIR verification proves:

- no exported mono MIR dispatch nodes exist
- every source dispatch or custom equality call consumed a checked
  `StaticDispatchCallPlan` or was already rewritten to `structural_eq`
- no mono MIR code path chooses the dispatcher variable from a receiver
  expression, result position, method name, or environment lookup
- no mono MIR code path has a separate ordinary-dispatch/type-dispatch/equality
  target model after it has consumed `StaticDispatchCallPlan`
- every dispatcher's mono type fully resolves to exactly one allowed `MethodOwner`
  after callable args and return slot have been connected
- every static-dispatch-produced `call_proc.requested_fn_ty` is the unified
  target-procedure type and `StaticDispatchCallPlan.callable_var` type in the
  mono type store
- `call_proc` targets only top-level mono-specialized procedures
- mono `proc_value` captures are empty
- mono `proc_value` targets for top-level procedure values are mono-specialized
  at the exact requested mono source function type
- the mono specialization queue has no pending `call_proc` or `proc_value`
  dependencies
- every `call_proc` and `call_value` has exactly the arity of its
  `requested_fn_ty`
- no mono MIR node represents automatic currying or partial application

### 5. Add Row-Finalized Mono MIR Pass

Add a dedicated row-finalization pass between mono MIR and lifted MIR.

This pass must consume dispatch-free mono MIR and produce a distinct
row-finalized mono MIR type-state. Do not implement row finalization as a lazy
helper inside lifted MIR, lambda-solved MIR, executable MIR, IR lowering, or
layout lowering.

Add a compact `RowShapeStore` or equivalent interner:

- unique record shapes keyed by canonical logical field order
- unique tag-union shapes keyed by canonical logical tag order and payload arity
- `RecordFieldId` records owned by `RecordShapeId`
- `TagId` records owned by `TagUnionShapeId`
- `TagPayloadId` records owned by `TagId`

Rewrite every row operation so it carries finalized IDs:

- record construction carries `RecordShapeId` and `RecordFieldId` entries
- record access carries `RecordFieldId`
- record update carries `RecordShapeId` and `RecordFieldId` entries
- record destructuring patterns carry `RecordFieldId`
- tag construction carries `TagUnionShapeId`, `TagId`, and `TagPayloadId`
  entries for payloads
- tag patterns carry `TagUnionShapeId`, `TagId`, and `TagPayloadId` entries for
  payload patterns
- tag payload projections carry `TagPayloadId`

For every row operation, the pass must resolve the operation's mono type in the
specialization-local mono type store, derive the full logical row from that
type, intern that full row shape, validate the requested field or tag against
the full row, validate constructor payload arity, and then rewrite the MIR node.

The pass may cache finalized IDs while it runs. The cache key must be the
canonical fully resolved row shape. The cache must not be exported as an API that
later stages can call to look up names.

Commit when row-finalized mono MIR verification proves:

- lifted MIR consumes row-finalized mono MIR, not name-bearing mono MIR
- no row-finalized mono MIR node stores a name-only row operation
- every record construction, access, update, and destructuring pattern stores
  finalized record IDs
- every tag construction, tag pattern, and tag payload projection stores
  finalized tag IDs
- row shape metadata is interned once per unique logical row shape, not copied
  onto every expression
- row shape keys exclude payload slot types; payload slot types remain in the
  mono type store and later representation edges
- every finalized row ID was derived from the full resolved mono type for that
  operation, never from singleton syntax such as `Err("x")`
- no later-stage helper exists for computing logical row indexes by sorting
  names, scanning rows, scanning expressions, or inspecting physical layout
  order

### 6. Harden Lifted MIR

Update lifted MIR to consume dispatch-free, row-finalized mono MIR.

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

### 7. Harden Lambda-Solved MIR

Update lambda-solved MIR to consume dispatch-free lifted MIR.

Delete all dispatch cases from lambda-solved AST, inference, erasure
propagation, and verification.

Preserve and clean up the real responsibilities:

- instantiate lifted types
- infer callable sets
- propagate erasure
- compute `erased_box_payload_type(boundary)` plans for explicit `Box(T)`
  boundaries
- build the specialization-local `RepresentationStore`
- create distinct representation roots for every expression result, binder,
  pattern binder, procedure parameter, procedure return, capture slot,
  callable requested-function occurrence, mutable variable version, and loop phi
- consume finalized `RecordShapeId`, `RecordFieldId`, `TagUnionShapeId`, `TagId`,
  and `TagPayloadId` records for records, tag unions, patterns, and projections
- create `require_box_erased(boundary)` requirements only from explicit
  `BoxBoundaryId` values
- create explicit representation edges that merge every `call_value` callee with
  the whole requested function representation root, plus every argument, return
  slot, and result
- create explicit representation edges for every `call_proc` argument and
  instantiated target return, and merge the target procedure function root with
  the whole `call_proc.requested_fn_ty` root
- create explicit representation edges from every `proc_value` result and
  capture argument to the whole `proc_value.fn_ty` function root and
  corresponding procedure capture slot
- preserve explicit `BoxBoundary` box type, payload source type, payload
  boundary type, direction, representation roots, and payload
  `BoxPayloadRepresentationPlan`
- solve boxed payload representation requirements through aliases, binders,
  captures, function parameters, function returns, and expression occurrences
- solve boxed payload representation requirements through branch joins, source
  `match` condition/pattern edges, pattern binders, projections, loops, and
  returned values
- solve mutable variable representation through explicit versions, branch joins,
  loop phis, and loop-exit joins
- treat those mutable versions, joins, and loop phis as SSA records rather than
  physical mutable storage cells
- solve structural representation classes with explicit merge rules for
  primitives, records, tuples, tag unions, `List(T)`, `Box(T)`, nominals,
  functions, and callable slots
- solve boxed payload representation requirements only after
  specialization-local clone-instantiation and full type-link resolution
- publish and consume explicit module-interface representation capability
  templates and instantiated capabilities for imported and opaque nominal boxed
  payload traversal
- publish and consume instantiation-sensitive `NoReachableCallableSlotsProof`
  records for `opaque_atomic` nominals
- consume hosted/platform callable representation metadata explicitly
- order recursive SCCs
- enforce canonical callable-set unification algebra
- export fully resolved callable representations for every executable specialization
  input
- clone-instantiate generalized templates before executable lowering consumes
  them

Commit when lambda-solved MIR verification proves:

- no dispatch terms exist
- callable members and captures are explicit
- callable-set member order is canonical
- each repeated callable member has exactly the same capture slots
- erased callable requirements are explicit
- every `BoxBoundary` stores box type, payload source type, payload boundary
  type, direction, representation roots, and boxed payload representation plan
- every function slot reachable through an explicit `Box(T)` erased boundary is
  represented as erased in the exported boxed payload boundary type
- no structural boxed payload plan node implies runtime traversal, runtime
  container rebuilding, or non-`Box(T)` erasure
- no non-`Box(T)` root can introduce erased callable representation
- representation equality is occurrence/value-flow based, not type-id based
- two unrelated roots with equal logical `TypeId`s do not unify unless an
  explicit representation edge connects them
- every `call_value` has representation edges that merge the callee with the
  whole requested function root, plus every argument, requested return slot, and
  result
- every `call_proc` has representation edges for every argument and instantiated
  target return, and merges the target procedure function root with the whole
  `call_proc.requested_fn_ty` root
- every `proc_value` has representation edges that merge the value result with
  the whole `proc_value.fn_ty` function root and connect every capture argument
  to the corresponding procedure capture slot
- every mutable use reads from a current mutable version root
- every `reassign` creates a new mutable version root
- every branch join and loop-carried mutable value has an explicit join or loop
  phi root
- mutable versions, branch joins, and loop phis are SSA representation records, not
  physical storage slots
- every exported representation root has a solved representation class
- every solved representation class has one structural `RepresentationShape`
- every record and tag-union representation slot refers to finalized row IDs, not
  display-name sorting or physical layout order
- tag construction edges target the full checked tag-union type, never a
  singleton constructor type reconstructed from syntax
- structural representation merge uses finalized row IDs, never display-name
  sorting or physical layout order
- every `require_box_erased(boundary)` requirement is owned by an explicit
  `BoxBoundaryId`
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
- every `opaque_atomic` proof is valid for the exact nominal identity and exact
  instantiated type arguments being compiled
- `call_proc` and `proc_value` targets remain explicit
- `call_proc` and `proc_value` participate in callable inference and SCC ordering
- every `proc_value` capture arg unifies with its target `CaptureSlot`

### 8. Replace Executable Side-Table Planner

Rewrite executable MIR lowering so it consumes lambda-solved MIR and emits
executable MIR without source-expression side tables.

Executable MIR must run executable ownership solving after all executable
specializations, erased adapters, intrinsic wrappers, bridge procedures, entry
wrappers, and hosted targets have been reserved, and before lowering to IR.
That solver is one SCC fixed point over procedure nodes, callable-match nodes,
erased adapter nodes, and bridge nodes. It produces `ProcOwnershipContract`,
`CallableCallOwnershipKey`, node ownership semantics, bridge ownership semantics,
and boxed-boundary ownership semantics.

Delete:

```text
legacy value side-table records
legacy callable side-table records
expression-indexed side-table maps
semantic side-table keys
semantic side-table-id usage
exact_callable_aliases
expression-to-callable-target lookup helper
expression-to-callable-captures lookup helper
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
+ fully resolved lambda-solved argument and return structural type keys
+ finite callable-set member procedure identity when specializing a callable-set
  branch
+ erased adapter key when specializing an erased adapter
+ capture slot shape and capture types for callable-set or erased captures
+ executable argument and return types
+ representation mode
```

not:

```text
side-table handles + expression ids + source/executable side channels + raw type ids
ProcOrderKey
```

Commit when executable MIR verification proves:

- direct calls have explicit targets
- direct call args match target signatures
- direct, erased, and callable-set calls preserve fixed Roc arity
- erased calls have explicit erased function types
- erased function signature keys contain an explicit `ErasedCallAbiPolicyKey`,
  not a future `CallableCallOwnershipKey`
- erased callable equality requires exact `ErasedFnSigKey` equality, including
  `ErasedCallAbiPolicyKey`
- same erased args/return with different `ErasedCallAbiPolicyKey` values requires
  an explicit adapter or bridge and must not silently merge
- ordinary Roc boxed erased callables use the canonical erased ABI policy:
  borrow packed function, borrow all erased arguments, fresh result
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
- `CallableCallOwnershipKey` values are produced by the executable ownership SCC
  fixed point and are not present in erased adapter keys
- callable-match ownership joins argument modes with `borrow < consume` and
  preserves result alias/borrow relations only when all returning branches agree
- no executable MIR node represents automatic currying or partial application
- no ordinary source `match` satisfies callable-set lowering verification
- packed erased functions have explicit captures
- bridge nodes connect concrete executable MIR types
- every executable MIR node introduced for callable lowering, erased packaging,
  boxed payload boundaries, and bridges carries explicit ownership semantics for
  LIR
- every executable procedure has a `ProcOwnershipContract` before IR lowering
- LIR ownership logic does not infer procedure contracts as semantic truth

### 9. Delete Source-Type Reconstruction

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

### 10. Rewire IR Lowering

Update `src/ir/lower.zig` to consume executable MIR.

IR lowering must be source-blind and method-blind.

It consumes executable MIR data only and emits existing IR direct/erased call
forms.

Commit when IR lowering has no imports of checked CIR or MIR builder internals.

### 11. Rewire Public Pipelines

Update eval, compile, CLI, dev shim, interpreter shim, snapshot tool, glue, REPL,
and test helpers to call the checked-artifact public pipeline.

The public semantic lowering API must accept:

```text
published checked artifacts
explicit RootRequestSet
target configuration
```

and return:

```text
LowerResourceError!LoweredProgram
```

where `LowerResourceError` contains only resource failures such as
`Allocator.Error`.

Delete public helpers that accept checked CIR plus optional roots or root names.
Delete public helpers that choose a root by scanning exports, selecting the last
root procedure, filtering expressions by syntax, or looking for hosted lambda
nodes.

REPL and development expressions must be checked as temporary modules or
temporary checked artifacts with explicit `.repl_expr` or `.dev_expr` roots.
Tests may build small artifacts directly, but semantic lowering tests must call
the same checked-artifact public pipeline as production tools.

Remove helper names that refer to old stages.

Commit when `rg` finds no old post-check imports or pipeline labels, no
`lowerTypedCIRToLir*` or `lowerTypedCIRToSemanticEval*` public entrypoints, and
no public semantic lowering result that can return `NoRootProc`,
`NoRootDefinition`, `MissingRoot`, `MethodNotFound`, `UnsupportedSourceType`,
`UnsupportedLayout`, `SchemaLayoutMismatch`, or `MissingInterfaceCapability`.

### 12. Rewire Compile-Time Constant Evaluation

Replace the current compile-time evaluation lowering path with the MIR-family
pipeline:

```text
checked CIR
  -> mir.mono
  -> mir.mono.row_finalize
  -> mir.lifted
  -> mir.lambda_solved
  -> mir.executable
  -> ir
  -> lir
  -> LIR interpreter
  -> CompileTimeValueStore + promoted procedure table
```

This work must preserve the current valid architecture:

- compile-time constants are evaluated by the LIR interpreter
- runtime bytes are reified into explicit schema/value nodes
- top-level constant bindings point at `(schema_id, value_id)`
- top-level callable bindings publish as `procedure_value` after compile-time
  callable promotion when promotion is needed
- serialized compile-time values travel inside cached checked artifacts
- imported modules expose compile-time constants through their serialized value
  store
- serializable top-level constants are consumed by `ConstRef`
- function declarations and function-valued declarations are consumed as
  procedure values; direct top-level functions use their original procedure
  symbols, and compile-time callable roots use promoted procedure symbols

This work must delete or replace the current invalid architecture:

- no runtime top-level constant thunks
- no runtime global initializer procedures for constants
- no runtime zero-argument wrappers for constants
- no runtime top-level closure objects for top-level bindings
- no runtime global callable-value objects for top-level bindings
- no late syntax filters for root selection
- no checked-CIR-to-LIR semantic eval path that bypasses MIR-family contracts
- no imported module LIR re-execution after checked artifact publication
- no target-shaped constant bytes stored in the checked artifact cache

Introduce an explicit compile-time root table before LIR interpretation. The
table must record at least the source module, top-level pattern, expression,
procedure symbol, root kind, and either the result schema for serializable
constants or the callable-result record for function-valued bindings. Root
selection must happen before the LIR interpreter runs. The interpreter must only
execute listed roots; it must not decide which top-level declarations are
constants or callable bindings.

Add compile-time callable promotion to checking finalization. Function-valued
top-level bindings whose expressions are not already top-level functions must
run as `callable_binding` roots. Their interpreter result must be reified as a
compiler-owned `ComptimeCallable`, promoted to a closed procedure symbol, and
published as `procedure_value`. The promoted procedure must have no runtime
capture environment. Serializable captures become `ConstRef` reads; callable
captures are promoted recursively to private procedure symbols. Unsupported
captures are checking problems before publication.

Private aggregate LIR roots are allowed only as `comptime_only` interpreter
entrypoints. They must be excluded from runtime root lists, backend input, and
generated program entry metadata. Debug verification must assert that no
`comptime_only` proc reaches runtime codegen; release builds use `unreachable`
if that invariant is violated.

Move the user-facing reporting boundary so compile-time evaluation is part of
checking finalization. `TypeCheckOutput` or equivalent checked artifact data
must not be returned until compile-time constant evaluation has either produced
a complete `CompileTimeValueStore` plus all required promoted procedures, or
appended all user-facing checking problems.

After checked artifact data is returned, missing compile-time constant data or a
missing promoted procedure is not a recoverable condition. Later stages must
consume the published artifact data or hit a compiler invariant violation.

Add `ConstRef` or the exact equivalent. Mono MIR lookup of a serializable
top-level constant must emit a constant-reference node that carries this handle.
Executable MIR/LIR must turn the handle into an explicit target-specific
`ConstMaterializationPlan` containing target executable type, layout, ownership,
and storage strategy. Backends must only emit requested static data or heap setup
and follow explicit LIR `incref` and `decref` statements.

Replace the current checked-module cache shape with the checked artifact cache
described above. The checked artifact key must use `source_hash`,
`compiler_artifact_hash`, `module_identity`, `checking_context_identity`, and
direct imported checked artifact keys. It must not include target/layout inputs.

A checked artifact cache hit and its compile-time value store must be accepted
together or rejected together. There must not be an independently accepted
compile-time-value sidecar. If target-specific constant materialization is ever
cached, that cache is separate from the checked artifact cache and is keyed by
the checked artifact key plus target/layout inputs.

Commit when compile-time evaluation uses the MIR-family pipeline, runtime
codegen cannot see `comptime_only` roots, cached/imported constants are loaded
only from the compile-time value store through `ConstRef`, and target-specific
constant materialization is outside the checked artifact cache.

### 13. Strengthen Audits

Make audits allowlist-based.

Deletion-protection audits are debug, verifier, guarded-test, and CI checks
only. They must compile out of release compiler builds. Release compiler builds
must not pay for string scans, allowlist walks, deleted-family checks, or audit
metadata that is unnecessary assuming the compiler is correct. If a deleted
family is nevertheless reached in a post-check compiler stage, that is a
compiler invariant violation handled only by debug-only assertion in debug
builds and `unreachable` in release builds.

Forbid old stage names outside historical docs if they remain at all:

```text
monotype
monotype_lifted
lambdasolved
lambdamono
```

Forbid old side-table and reconstruction families everywhere outside tests that
intentionally check the audit:

```text
legacy value side-table records
legacy callable side-table records
expression-indexed side-table maps
semantic side-table keys
semantic side-table-id usage
exact_callable_aliases
expression-to-callable-target lookup helper
expression-to-callable-captures lookup helper
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

Forbid any erased-boundary record whose root is not `Box(T)`.

Forbid executable erased-shape compatibility helpers from making semantic
lowering decisions. Boxed erased-boundary decisions must come from lambda-solved
`BoxBoundary` records and `BoxPayloadRepresentationPlan` values. They
may be rechecked only by debug-only verifiers.

Forbid any MIR, executable MIR, IR, or LIR operation whose semantic purpose is
automatic currying or compiler-synthesized partial application.

Forbid runtime top-level constant thunks, runtime global initializer procedures
for compile-time constants, runtime zero-argument constant wrappers, runtime
top-level closure objects for top-level bindings, and runtime global
callable-value objects for top-level bindings. Private `comptime_only` LIR
interpreter roots are allowed only in the compile-time evaluation module and
tests that explicitly verify they never reach runtime codegen.

Forbid compile-time root selection by late syntax filters in LIR evaluation.
Compile-time roots must come from the explicit compile-time root table.

Forbid post-check root selection by scanning exports, top-level declarations,
checked expressions, procedure order, hosted lambda expressions, or generated
symbol names. Root selection must consume `RootRequestTable`.

Forbid post-check mutation of hosted indices or platform-required lookup
targets inside checked CIR or `ModuleEnv`. Hosted and platform data must come
from `HostedProcTable` and `PlatformRequiredBindingTable`.

Forbid public semantic lowering APIs that return semantic errors after artifact
publication. Post-check semantic lowering may return resource errors only.

Forbid imported representation recovery from module bodies, opaque backing
syntax, display names, or layout shapes. Cross-module representation data must
come from `ModuleInterfaceCapabilities`.

Forbid runtime constant materialization from hidden top-level thunks or imported
module LIR re-execution. Serializable constants must be consumed through
`ConstRef` and `CompileTimeValueStore`.

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

### 14. Rewrite Tests

Rewrite old intermediate-stage tests.

Obsolete expectations:

- dispatch survives into monotype
- dispatch survives into lambdasolved
- accumulator side tables are visible before executable MIR
- executable side tables contain old source-level expression records
- old stage type stores contain old-stage shapes

Replacement expectations:

- checked CIR contains dispatch only with normalized `StaticDispatchCallPlan`
  values where appropriate
- mono MIR contains `call_proc` and no dispatch
- lifted MIR contains explicit `CaptureSlot`s, explicit `proc_value`
  `CaptureArg`s, and no dispatch
- lambda-solved MIR contains explicit callable sets and no dispatch
- executable MIR contains direct/erased calls, finite callable-set
  `callable_match`
  lowering, packed erased functions, bridges, and no source-expression side
  tables
- IR/LIR contain direct/erased calls only

## Required Structural Tests

Add MIR-family verification tests for each stage.

Checking finalization and compile-time constants:

- checking finalization publishes a complete `CheckedModuleArtifact` or no
  artifact
- published artifacts include method registry, static dispatch plans, root
  request table, hosted procedure table, platform-required binding table,
  interface capabilities, and compile-time value store
- artifact views exposed to importers and lowering are read-only
- a cache hit does not patch module identity after deserialization
- hosted procedure ordering is stored in `HostedProcTable`, not by mutating
  checked CIR
- platform-required bindings are stored in `PlatformRequiredBindingTable`, not
  by mutating checked module lookup targets after publication
- root requests exist before MIR lowering starts
- root requests cover app entrypoints, provided exports, platform-required
  bindings, hosted exports, tests, REPL/dev expressions, and compile-time roots
- no eval, REPL, snapshot, CLI, glue, build, or test helper selects roots by
  scanning exports, declaration names, expression syntax, hosted lambda nodes,
  or procedure order
- imported modules expose representation capabilities through
  `ModuleInterfaceCapabilities`
- importing modules do not inspect imported private definitions or opaque
  backing syntax to rebuild representation capability data
- compile-time constant evaluation runs before checked artifacts are published
- user-facing compile-time crashes, expect failures, numeric conversion
  failures, and evaluation errors are reported as checking problems
- every compile-time evaluation root appears in the explicit compile-time root
  table
- no LIR interpreter code path selects compile-time roots by inspecting source
  expression syntax
- private aggregate LIR roots used by compile-time evaluation are marked
  `comptime_only`
- no `comptime_only` root reaches runtime root lists, backend input, generated
  program entry metadata, or runtime codegen
- compile-time value reification stores `(schema_id, value_id)` bindings, not
  raw runtime addresses
- compile-time schemas are built from resolved source types plus selected
  layouts
- function source types are rejected as serializable constant schemas unless a
  future explicit serialized function-value representation exists
- function-valued top-level bindings that evaluate to closed callable values are
  promoted during checking finalization to closed procedure symbols
- cached checked artifacts include serialized compile-time values and hit or
  miss as one unit
- checked artifact keys use `source_hash`, `compiler_artifact_hash`,
  `module_identity`, `checking_context_identity`, and direct imported checked
  artifact keys
- checked artifact keys do not include target/layout inputs
- serializable top-level constants are consumed through `ConstRef`
- function declarations and function-valued declarations are consumed as
  procedure values after any required callable promotion, not serialized
  constants
- imported constants are not evaluated by re-running imported module LIR roots
  after the imported artifact is published
- target-specific constant materialization uses explicit layout, ownership, and
  storage plans outside the checked artifact cache

Mono MIR:

- every expression has a mono type
- no dispatch nodes exist
- static dispatch becomes `call_proc`
- static dispatch consumes checked `StaticDispatchCallPlan` values and the
  checked method registry, never syntax-derived method lookup in mono MIR
- static-dispatch `call_proc.requested_fn_ty` is the mono-store unification of
  `StaticDispatchCallPlan.callable_var` and the target procedure type
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

Row-finalized mono MIR:

- every row-finalized mono MIR expression still has a mono type
- no name-only record construction, access, update, or destructuring node exists
- no name-only tag construction, pattern, or payload projection node exists
- every record operation carries `RecordShapeId` and `RecordFieldId` values
- every tag operation carries `TagUnionShapeId`, `TagId`, and `TagPayloadId`
  values
- shape interning reuses one logical shape record across repeated uses of the
  same field or tag-union shape
- row shape keys do not include payload slot types
- `Err("x")` in a full `[Ok(I64), Err(Str)]` context uses the finalized `Err`
  ID from the full union, never a singleton `[Err(Str)]` shape
- no later-stage API can lazily compute logical row indexes by sorting names,
  scanning rows, scanning expressions, or inspecting physical layout order

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
- mismatched capture slots trigger debug-only assertions
- erasure requirements are explicit
- `BoxBoundary` stores box type, payload source type, payload boundary type,
  direction, representation roots, and `BoxPayloadRepresentationPlan`
- boxed payload boundary types structurally rewrite every reachable function
  slot to erased callable representation
- `RepresentationStore` has distinct roots for every expression result, binder,
  pattern binder, procedure parameter, procedure return, capture slot,
  callable requested-function occurrence, mutable variable version, and loop phi
- representation variables unify only through explicit value-flow edges, never
  merely through equal logical `TypeId`s
- every `call_value` exports representation edges that merge the callee with the
  whole requested function root, plus every argument, requested return slot, and
  result
- every `call_proc` exports representation edges for every argument and
  instantiated target return, and merges the target procedure function root with
  the whole `call_proc.requested_fn_ty` root
- every `proc_value` exports representation edges that merge the value result
  with the whole `proc_value.fn_ty` function root and connect every capture
  argument to the corresponding procedure capture slot
- every mutable use reads from a current mutable version root
- every reassignment, branch join, loop-carried value, and loop exit has an
  explicit representation edge through a mutable version, join, or loop phi
- mutable versions, branch joins, loop-carried values, and loop exits are SSA
  records, not physical mutable storage slots
- every `require_box_erased(boundary)` requirement is attached to an explicit
  `BoxBoundaryId`
- every exported representation root has a solved representation class
- every solved representation class has one structural `RepresentationShape`
- row finalization IDs are present before representation solving
- structural representation merge uses finalized row IDs and the full checked
  tag-union type
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
  `NoReachableCallableSlotsProof` for the exact nominal identity and
  instantiated type arguments
- finite callable-set erasure preserves source member metadata for executable
  adapter synthesis
- erased adapter keys include finite callable-set identity, erased function
  signature with `ErasedCallAbiPolicyKey`, and capture shape
- generalized templates are clone-instantiated and fully resolved before executable
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
- checking reports erased-boundary roots other than `Box(T)` before executable
  MIR; executable MIR only debug-verifies that none reached it
- checking reports imported, opaque, hosted, and platform-owned boxed payload
  traversal without explicit representation capabilities before executable MIR;
  executable MIR only debug-verifies that none reached it
- every erased capture record has deterministic field ordering
- every erased call has an explicit erased function type
- every erased call has an explicit ownership contract from the
  `ErasedCallAbiPolicyKey` inside `ErasedFnSigKey`
- every executable procedure has a `ProcOwnershipContract` before IR lowering
- executable ownership solving is one SCC fixed point over procedure,
  callable-match, erased-adapter, and bridge nodes
- `CallableCallOwnershipKey` values are produced by that fixed point and never
  stored in `ErasedAdapterKey`
- first-class intrinsic references use explicit wrapper procedures
- logical field indexes are preserved until LIR resolves physical offsets
- bridges connect concrete executable types
- callable lowering, erased packaging, boxed payload boundaries, and bridges
  preserve explicit ownership records for LIR
- no source-expression side tables exist

IR/LIR:

- public checked-artifact-to-LIR lowering APIs return only resource errors such
  as `Allocator.Error`
- public semantic lowering APIs do not return `NoRootProc`, `NoRootDefinition`,
  `MissingRoot`, `MethodNotFound`, `UnsupportedSourceType`,
  `UnsupportedLayout`, `SchemaLayoutMismatch`, or `MissingInterfaceCapability`
- direct calls lower to direct calls
- erased calls lower to erased calls
- no method/dispatch operation exists
- every value-producing LIR statement has sufficient ownership semantics for
  `RcInsert`
- LIR ownership code verifies, normalizes, or translates explicit ownership
  records only; it does not infer procedure ownership contracts as semantic truth
- `RcInsert` is the only non-builtin stage that emits explicit `incref`,
  `decref`, and `free`
- backends execute explicit LIR RC statements and perform no ordinary RC
  analysis

## Required Behavioral Tests

Static dispatch:

- generic dispatch specializes to different nominal method targets
- generic target methods specialize at exact monomorphic function types
- chained dispatch uses each dispatch site's own `StaticDispatchCallPlan` and
  the earlier call's unified return slot
- chained dispatch does not call any expression-based method resolver
- type-var alias dispatch resolves from the specialized dispatcher type selected
  by `StaticDispatchCallPlan.dispatcher_var`
- primitive methods resolve through builtin primitive owners
- list methods resolve through the builtin `List` owner
- box methods resolve through the builtin `Box` owner
- custom equality lowers to `call_proc` `is_eq` before executable MIR and direct
  executable `is_eq` after executable MIR
- inequality lowers to `call_proc` `is_eq` plus `bool_not` before executable MIR
  and direct executable `is_eq` plus `bool_not` after executable MIR
- anonymous record equality remains structural equality
- transparent tag-union aliases resolve through nominal identity
- cross-module methods resolve through registry target refs
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
- checking reports missing-argument calls to fixed-arity functions before MIR
  export; they do not synthesize partial-application closures
- checking reports extra-argument calls to fixed-arity functions unless the
  source explicitly calls a returned function value
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
- imported opaque nominal boxed payload traversal without an exact capability is
  reported during checking; if it reaches lambda-solved MIR, that path is a
  compiler invariant violation handled by debug-only assertion in debug builds
  and `unreachable` in release builds
- hosted function with callable-containing args or returns consumes explicit ABI
  representation and erased-call ownership metadata
- finite callable-set call where one branch consumes an argument and another
  branch borrows it normalizes to one `CallableCallOwnershipKey` for the whole
  `callable_match`
- `Box.unbox` borrows the box and materializes a copied payload result; no test
  may rely on move-out semantics for `Box.unbox`
- `packed_erased_fn`, erased adapters, `callable_match`, `Box.box`,
  `Box.unbox`, and bridges produce LIR ownership records consumed by `RcInsert`

Compile-time constants:

- simple top-level constants evaluate through the LIR interpreter and appear in
  the compile-time value store
- top-level constants that call helper functions evaluate through the same LIR
  interpreter path
- compile-time constants containing strings, lists, records, tuples, tag unions,
  boxes, aliases, and nominals reify to logical constant nodes
- top-level function declarations do not become runtime constant thunks
- top-level function-valued declarations do not become runtime closure objects,
  runtime global callable-value objects, runtime initializer procedures, or
  runtime thunks
- top-level function declarations are published directly as `procedure_value`
  without being evaluated just to prove they are callable
- top-level function-typed expressions such as `add5 = makeAdder(5)` evaluate
  as `callable_binding` roots during checking finalization
- top-level function-valued declarations are not reified as serializable
  constants unless an explicit serialized function-value representation has
  been added
- top-level function-valued declarations that evaluate to closed callable values
  are promoted before artifact publication to closed top-level procedures and
  then consumed as `procedure_value`
- promoted procedures have no runtime capture environment; serializable
  captures are consumed through `ConstRef`, and callable captures are promoted
  recursively to private procedure symbols
- `TopLevelValueTable` is the only post-check source for deciding whether a
  top-level binding is a `serializable_constant` or `procedure_value`
- user-written compile-time crashes and failed `expect` statements are reported
  before checked artifacts are published
- division by zero and numeric conversion failures during compile-time constant
  evaluation are reported before checked artifacts are published
- cross-module constants are consumed from the imported module's serialized
  compile-time value store
- cross-module constants are referenced by `ConstRef`, not by generated
  zero-argument procedures
- a cached checked artifact restores compile-time values with the rest of the
  checked artifact
- changing a direct or transitive import changes imported checked artifact keys
  and invalidates dependent checked artifacts
- changing target/layout-relevant inputs does not invalidate checked artifacts;
  those inputs invalidate only target-specific post-check caches
- no generated runtime code contains top-level constant initializer thunks,
  global initializer procedures for constants, or zero-argument constant wrappers
- private aggregate `comptime_only` roots never appear in backend input

Artifact and tooling behavior:

- app entrypoints lower only through explicit root requests
- platform-required roots lower only through `PlatformRequiredBindingTable`
- hosted exports lower only through `HostedProcTable`
- REPL expressions lower as temporary checked artifacts with `.repl_expr` roots
- dev expressions lower as temporary checked artifacts with `.dev_expr` roots
- tests that compile source to LIR call the same checked-artifact public
  pipeline as production tools
- missing roots are reported before artifact publication for commands that
  require them
- imported opaque representation succeeds only through published interface
  capabilities
- imported opaque representation without a capability is reported before the
  importing artifact is published
- public post-check lowering APIs never return semantic missing-data errors

End-to-end:

```sh
ci/guarded_zig.sh zig build test-cor-pipeline
ci/guarded_zig.sh zig build test-eval
ci/guarded_zig.sh zig build test-glue
```

If the test name `test-cor-pipeline` remains, its contents must be MIR-family
pipeline tests, not old-stage contract tests.

## Compiler Bug Handling

For every post-check compiler invariant violation found during development,
debug verification, or guarded tests:

1. Identify which stage must have owned the missing stage output.
2. Add that record, plan, table entry, or contract to that stage's explicit
   output.
3. Delete any old reconstruction or side-channel path exposed by the violation.
4. Strengthen audits if the violation reveals a family that could return.
5. Rerun the narrowest guarded test that exercises the violation.

Forbidden responses:

- restoring old module imports
- adding compatibility shims
- making owner resolution expression-based
- adding expression-derived side tables
- storing callable truth in environment entries
- reconstructing source types from expression syntax
- using body-derived summaries as executable truth
- weakening audits
- changing tests to preserve obsolete intermediate invariants

## Final Verification Checklist

The cutover is complete only when all of these are true:

- public executable pipeline is `checked CIR -> mono MIR -> row-finalized mono
  MIR -> lifted MIR -> lambda-solved MIR -> executable MIR -> IR -> LIR`
- every public semantic lowering client enters through the checked-artifact
  pipeline with explicit roots and target configuration
- checked finalization publishes complete immutable checked artifacts or no
  artifacts
- checked artifacts contain root requests, hosted procedure tables,
  platform-required binding tables, interface capabilities, method registries,
  static dispatch plans, and compile-time value stores
- published checked artifacts are consumed through read-only views and are not
  patched after cache load or publication
- no post-check stage mutates checked CIR or `ModuleEnv` to assign hosted
  indices, platform-required lookup targets, roots, or module identity
- root requests are built before MIR lowering and no later stage selects roots
  by scanning exports, declarations, expression syntax, hosted lambda nodes, or
  procedure order
- REPL and development expressions become temporary checked artifacts with
  explicit roots
- imported modules expose representation capabilities and compile-time constants
  only through their checked artifacts
- public post-check semantic lowering APIs return resource errors only; semantic
  missing-data conditions are checking diagnostics before publication or
  compiler invariant violations after publication
- compile-time constant evaluation runs during checking finalization through
  `checked CIR -> mono MIR -> row-finalized mono MIR -> lifted MIR ->
  lambda-solved MIR -> executable MIR -> IR -> LIR -> LIR interpreter ->
  compile-time value store`
- checked artifact data is not published until compile-time constant evaluation
  has either produced a complete compile-time value store or appended all
  user-facing checking problems
- no runtime top-level constant thunks, runtime global initializer procedures
  for constants, runtime zero-argument constant wrappers, runtime top-level
  closure objects, or runtime global callable-value objects exist for top-level
  bindings
- private `comptime_only` LIR roots are visible only to compile-time evaluation
  and never reach backend input
- imported constants are consumed through `ConstRef` from serialized
  compile-time value stores, not by re-running imported module LIR roots after
  checking
- target-specific constant materialization uses explicit layout, ownership, and
  storage plans outside the checked artifact cache
- no public pipeline imports old top-level post-check modules
- static dispatch exists only in checked CIR and mono MIR input pattern matching
- every checked static-dispatch node exported to mono MIR carries a
  `StaticDispatchCallPlan`, or has already been rewritten to `structural_eq`
- mono MIR consumes checked `StaticDispatchCallPlan` values plus the checked
  method registry and never chooses the dispatcher variable from receiver
  expressions, result positions, method names, or environment lookup
- mono MIR has one normalized static-dispatch lowering path after consuming
  `StaticDispatchCallPlan`; it has no separate ordinary-dispatch,
  type-dispatch, or equality target model
- mono MIR output has no dispatch nodes
- mono MIR output uses `call_proc`, not `call_direct`, for resolved static
  dispatch
- every static-dispatch-produced `call_proc.requested_fn_ty` is the mono-store
  unification of `StaticDispatchCallPlan.callable_var` and the instantiated
  target procedure type
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
- lambda-solved MIR exports explicit representation edges for every `call_value`
  callee merged with the whole requested function root, every argument, return
  slot, and result
- lambda-solved MIR exports explicit representation edges for every `call_proc`
  argument, instantiated target return, and whole target procedure function root
- lambda-solved MIR exports explicit representation edges for every `proc_value`
  result merged with the whole `proc_value.fn_ty` function root, and every
  capture argument
- lambda-solved MIR uses explicit `BoxBoundary` records preserving box type,
  payload source type, payload boundary type, direction, representation roots,
  and `BoxPayloadRepresentationPlan`
- lambda-solved MIR exports a specialization-local `RepresentationStore` whose
  roots are expression/binder/parameter/return/capture occurrences, not logical
  type identities
- lambda-solved MIR creates `require_box_erased(boundary)` only from explicit
  `BoxBoundaryId` values
- lambda-solved MIR never unifies representation variables merely because their
  logical `TypeId`s are equal
- lambda-solved MIR structurally rewrites every reachable function slot inside
  explicit `Box(T)` payload boundaries
- lambda-solved MIR propagates boxed payload representation requirements through
  aliases, binders, captures, parameters, returns, and expression occurrences
- lambda-solved MIR propagates boxed payload representation requirements through
  source `match` branch joins, condition/pattern edges, pattern binders,
  projections, loops, and returned values
- lambda-solved MIR represents mutable variables with explicit versions, branch
  joins, loop phis, and loop-exit joins
- lambda-solved MIR treats mutable versions, joins, and loop phis as SSA records,
  not physical mutable storage cells
- any later `set` or `set_local`-style assignment is layout-identical backend
  storage reuse after representation solving, verified in debug
- lambda-solved MIR solves structural representation classes with explicit merge
  rules for primitives, records, tuples, tag unions, `List(T)`, `Box(T)`,
  nominals, functions, and callable slots
- row finalization emits explicit `RecordShapeId`, `RecordFieldId`,
  `TagUnionShapeId`, `TagId`, and `TagPayloadId` records before representation
  solving
- representation merge consumes finalized row IDs and never sorts names, scans
  rows, or relies on physical layout order to compute logical indexes
- tag construction representation edges use the full checked tag-union type,
  never a singleton constructor type reconstructed from syntax
- lambda-solved MIR computes boxed payload representation plans and
  callable/capture keys only from specialization-local clone-instantiated and
  fully resolved type stores
- lambda-solved MIR requires explicit module-interface representation
  capabilities before traversing imported or opaque nominal boxed payloads
- opaque nominal atomic traversal requires an explicit
  `NoReachableCallableSlotsProof` for the exact nominal identity and
  instantiated type arguments
- lambda-solved MIR consumes hosted/platform callable representation metadata
  explicitly
- lambda-solved MIR exports only fully resolved executable specialization inputs
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
  not `ProcOrderKey`, raw type ids, expression ids, or side-table ids
- executable MIR consumes `BoxPayloadRepresentationPlan` instead of making
  semantic erased-shape compatibility decisions
- executable MIR consumes erased-call ownership contracts from `ErasedFnSigKey`
  through explicit `ErasedCallAbiPolicyKey` values instead of recovering
  ownership from runtime function pointers or capture layouts
- erased callable merge requires exact `ErasedFnSigKey` equality, including
  `ErasedCallAbiPolicyKey`; policy mismatches require explicit adapters or
  bridges
- executable ownership solving produces `ProcOwnershipContract` values before
  IR lowering
- executable ownership solving is one SCC fixed point over procedure,
  callable-match, erased-adapter, and bridge nodes
- `CallableCallOwnershipKey` values are produced by that fixed point and are not
  semantic components of erased adapter keys
- executable MIR preserves ownership records for callable lowering, erased
  packaging, boxed payload boundaries, and bridges
- `Box.unbox` is borrowed/copy materialization only; consuming move-out requires
  a separate explicit operation
- logical field indexes are resolved to physical offsets only through the layout
  store
- no exact callable alias side tables remain
- requested function type verifiers are debug-only compiler assertions and do not
  add runtime checks to user programs
- no source/executable side tables remain
- no expression-based method owner resolver remains
- no syntax-derived source type reconstruction remains
- checked static dispatch exports only normalized `StaticDispatchCallPlan`
  values; mono MIR consumes those plans and the checked method registry
- IR lowering consumes executable MIR only
- IR lowering preserves executable ownership records for LIR
- LIR ownership code verifies or translates ownership records but does not infer
  procedure ownership contracts as semantic truth
- LIR `RcInsert` is the only non-builtin stage that emits explicit `incref`,
  `decref`, and `free`
- LIR/backends do not know about source methods
- backends do not perform ordinary reference-counting analysis and only execute
  explicit LIR RC statements
- semantic audits forbid the deleted families
- guarded eval and glue gates pass
