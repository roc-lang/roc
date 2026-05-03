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

The final-state plan has no static alias-permission model, uniqueness inference,
parameter-mode procedure contracts, escape-summary contracts, or static
value-duration model. Those concepts are out of scope for this cutover. They
must not appear as MIR, IR, LIR, cache, ABI, or verifier contracts.

Reference counting remains automatic. LIR receives explicit `incref` and
`decref` statements from a mechanical ARC insertion pass over explicit LIR
values and control flow. Mutating operations rely only on a runtime uniqueness
check: `refcount == 1` permits in-place mutation; any other refcount must take
the copy path. The compiler must not statically prove uniqueness for this plan.

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

The cor prototype uses curried unary functions. This plan uses cor's
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
checked procedure templates, and concrete specialization requests.

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

Checked procedure bodies are registered before mono MIR as templates. A checked
procedure template is an artifact-owned checked body plus artifact-owned checked
type roots, checked static-dispatch plans, checked resolved value-reference
records, checked top-level use summaries, checked nested procedure-site table,
and checked procedure metadata. It is not mono MIR and it is not a
mono-specialized output procedure.

The published template must not depend on a raw `CIR.Expr.Idx`, `types.Var`,
`Ident.Idx`, unchecked syntax pointer, checked-module expression pointer, or
exporter-local `ModuleEnv` lookup to be lowered. Those handles are allowed only
inside checking finalization while constructing the checked artifact. Before the
artifact is published, checking finalization must copy the required checked
expression bodies and checked type graph roots into artifact-owned stores whose
ids are stable inside the artifact and usable through imported views.

The body source may be a user-written checked expression or a compiler-created
checked wrapper. Promoted compile-time callable results must become
compiler-created checked wrappers in this same table before the artifact is
published. A promoted callable procedure value without a corresponding checked
template and `PromotedProcedureTable` row is invalid; mono MIR must never receive
a bare generated symbol and then search for a body elsewhere.

Conceptual shape:

```zig
const CheckedProcedureTemplateId = enum(u32) { _ };
const NestedProcSiteId = enum(u32) { _ };
const CheckedBodyId = enum(u32) { _ };
const CheckedExprId = enum(u32) { _ };
const CheckedPatternId = enum(u32) { _ };
const CheckedTypeId = enum(u32) { _ };

const CheckedProcedureBody = union(enum) {
    checked_body: CheckedBodyId,
    promoted_callable_wrapper: PromotedCallableWrapperId,
    hosted_wrapper: HostedWrapperId,
    intrinsic_wrapper: IntrinsicWrapperId,
    entry_wrapper: EntryWrapperId,
};

const CheckedProcedureTemplate = struct {
    proc_base: ProcBaseKeyRef,
    template_id: CheckedProcedureTemplateId,
    body: CheckedProcedureBody,
    checked_fn_scheme: CanonicalTypeSchemeKey,
    checked_fn_root: CheckedTypeId,
    static_dispatch_plans: StaticDispatchPlanTableRef,
    resolved_value_refs: ResolvedValueRefTableRef,
    top_level_value_uses: TopLevelUseSummaryRef,
    nested_proc_sites: NestedProcSiteTableRef,
    target: ProcTarget,
};

const ProcedureTemplateRef = struct {
    proc_base: ProcBaseKeyRef,
    template: CheckedProcedureTemplateId,
};

const MonoSpecializationKey = struct {
    template: ProcedureTemplateRef,
    requested_mono_fn_ty: CanonicalTypeKey,
};

// `CanonicalTypeKey` and `CanonicalTypeSchemeKey` are identity keys, not type
// payloads. Every key consumed after publication must resolve through the owning
// artifact's checked type store to a real checked type root or scheme.

const ConcreteSourceTypeRef = enum(u32) { _ };

const ConcreteSourceTypeRoot = struct {
    key: CanonicalTypeKey,
    root: CheckedTypeId,
};

const ConcreteSourceTypeStore = struct {
    checked_types: CheckedTypeStore,
    roots: Store(ConcreteSourceTypeRoot),
    by_key: Map(CanonicalTypeKey, ConcreteSourceTypeRef),
};

const LiftedProcedureTemplateRef = struct {
    owner_mono_specialization: MonoSpecializationKey,
    site: NestedProcSiteId,
};

const SyntheticProcedureTemplateRef = struct {
    template: ProcedureTemplateRef,
};

const CallableProcedureTemplateRef = union(enum) {
    /// A procedure template that comes directly from the checked module artifact.
    /// This is the normal case for source-defined top-level functions and
    /// imported functions.
    ///
    /// ```roc
    /// inc : I64 -> I64
    /// inc = |x| x + 1
    /// ```
    checked: ProcedureTemplateRef,

    /// A procedure template created by lambda lifting a local function or
    /// closure after mono specialization has begun. It was written in source,
    /// but it was not a top-level checked procedure. Its identity includes the
    /// owning mono specialization plus the local function site, because the same
    /// source-local function can appear inside different mono specializations.
    ///
    /// ```roc
    /// make_adder : I64 -> (I64 -> I64)
    /// make_adder = |n|
    ///     add = |x| x + n
    ///     add
    /// ```
    lifted: LiftedProcedureTemplateRef,

    /// A procedure template generated by the compiler, not directly
    /// corresponding to a user-written function body. This is rare and explicit.
    /// One plain Roc example is a top-level callable constant produced by
    /// compile-time evaluation. Because top-level values never become runtime
    /// thunks or runtime top-level closure objects, the compiler promotes the
    /// compile-time callable result into a procedure-like template.
    ///
    /// ```roc
    /// make_adder : I64 -> (I64 -> I64)
    /// make_adder = |n|
    ///     |x| x + n
    ///
    /// inc : I64 -> I64
    /// inc = make_adder(1)
    ///
    /// answer = inc(41)
    /// ```
    synthetic: SyntheticProcedureTemplateRef,
};

const MonoSpecializedProcRef = struct {
    proc: ProcedureValueRef,
    specialization: MonoSpecializationKey,
};

const NestedProcSite = struct {
    owner_template: ProcedureTemplateRef,
    site: NestedProcSiteId,
    site_path: Span(NestedProcPathComponent),
    kind: NestedProcKind,
    checked_expr: ?CheckedExprId,
    checked_pattern: ?CheckedPatternId,
};

const NestedProcPathComponent = union(enum) {
    source_child: u32,
    branch: u32,
    pattern_child: u32,
    desugar: DesugarSiteKey,
};

const PromotedCallableWrapper = struct {
    promoted_proc: ProcedureValueRef,
    proc_base_key: ProcBaseKeyRef,
    callable_node: PromotedCallableNodeId,
    source_binding: PatternId,
    checked_fn_root: CheckedTypeId,
    body_plan: PromotedCallableBodyPlanId,
};

const PromotedCallableBodyPlan = union(enum) {
    finite: FinitePromotedWrapperBodyPlan,
    erased: ErasedPromotedWrapperBodyPlan,
};

const FinitePromotedWrapperBodyPlan = struct {
    source_fn_ty: CanonicalTypeKey,
    callable_set_key: CanonicalCallableSetKey,
    member: CallableSetMemberId,
    captures: Span(PrivateCaptureRef),
    params: Span(PromotedWrapperParam),
    call_args: Span(PromotedWrapperArg),
};

const ProcedureCallableRef = struct {
    template: CallableProcedureTemplateRef,
    source_fn_ty: CanonicalTypeKey,
};

const ProcedureValueRef = struct {
    proc_base: ProcBaseKeyRef,
};

const ErasedCallableCodeRef = union(enum) {
    direct_proc_value: ErasedDirectProcCodeRef,
    finite_set_adapter: ErasedFiniteSetAdapterRef,
};

const ErasedDirectProcCodeRef = struct {
    proc_value: ProcedureCallableRef,
    capture_shape_key: CaptureShapeKey,
};

const ErasedFiniteSetAdapterRef = struct {
    source_fn_ty: CanonicalTypeKey,
    callable_set_key: CanonicalCallableSetKey,
};

const ErasedPromotedWrapperBodyPlan = struct {
    source_fn_ty: CanonicalTypeKey,
    params: Span(PromotedWrapperParam),
    executable_signature: ErasedPromotedProcedureExecutableSignature,
    sig_key: ErasedFnSigKey,
    code: ErasedCallableCodeRef,
    capture: ErasedCaptureExecutableMaterializationPlan,
    arg_transforms: Span(ExecutablePayloadTransformPlanId),
    hidden_capture_arg: ?ErasedHiddenCaptureArgPlan,
    result_transform: ExecutablePayloadTransformPlanId,
    provenance: NonEmptySpan(BoxBoundaryId),
};

const ExecutablePayloadTransformPlanId = enum(u32) { _ };

const ExecutableValueEndpoint = struct {
    ty: ExecutableTypePayloadRef,
    key: CanonicalExecValueTypeKey,
};

const PayloadTransformRecordField = struct {
    field: RecordFieldLabelId,
    transform: ExecutablePayloadTransformPlanId,
};

const PayloadTransformTupleElem = struct {
    index: u32,
    transform: ExecutablePayloadTransformPlanId,
};

const PayloadTransformTagPayloadEdge = struct {
    source_payload_index: u32,
    target_payload_index: u32,
    transform: ExecutablePayloadTransformPlanId,
};

const PayloadTransformTagCase = struct {
    source_tag: TagLabelId,
    target_tag: TagLabelId,
    payloads: Span(PayloadTransformTagPayloadEdge),
};

const BoxPayloadTransformKind = enum {
    payload_to_box,
    box_to_payload,
    box_to_box,
};

const BoxPayloadTransformPlan = struct {
    boundary: BoxBoundaryId,
    kind: BoxPayloadTransformKind,
    payload: ExecutablePayloadTransformPlanId,
};

const ExecutablePayloadTransformOp = union(enum) {
    identity,
    structural_bridge: ExecutableStructuralBridgePlan,
    record: Span(PayloadTransformRecordField),
    tuple: Span(PayloadTransformTupleElem),
    tag_union: Span(PayloadTransformTagCase),
    nominal: struct {
        nominal: NominalTypeKey,
        backing: ExecutablePayloadTransformPlanId,
    },
    list: struct {
        elem: ExecutablePayloadTransformPlanId,
    },
    box_payload: BoxPayloadTransformPlan,
    callable_to_erased: CallableToErasedTransformPlan,
    already_erased_callable: AlreadyErasedCallableTransformPlan,
};

const ExecutableStructuralBridgePlan = union(enum) {
    direct,
    zst,
    list_reinterpret,
    nominal_reinterpret,
    box_unbox: ExecutablePayloadTransformPlanId,
    box_box: ExecutablePayloadTransformPlanId,
    singleton_to_tag_union: struct {
        source_tag: TagLabelId,
        target_tag: TagLabelId,
        payload_transform: ?ExecutablePayloadTransformPlanId,
    },
    tag_union_to_singleton: struct {
        source_tag: TagLabelId,
        target_tag: TagLabelId,
        payload_transform: ?ExecutablePayloadTransformPlanId,
    },
};

const CallableToErasedTransformPlan = union(enum) {
    finite_value: FiniteCallableValueToErasedPlan,
    proc_value: ProcValueToErasedPlan,
};

const FiniteCallableValueToErasedPlan = struct {
    source_fn_ty: CanonicalTypeKey,
    callable_set_key: CanonicalCallableSetKey,
    adapter_key: ErasedAdapterKey,
};

const ProcValueToErasedPlan = struct {
    proc_value: ProcedureCallableRef,
    erased_fn_sig_key: ErasedFnSigKey,
    capture_shape_key: CaptureShapeKey,
    executable_specialization_key: ExecutableSpecializationKey,
    capture: ErasedCaptureExecutableMaterializationPlan,
};

const AlreadyErasedCallableTransformPlan = struct {
    sig_key: ErasedFnSigKey,
};

const ExecutablePayloadTransformPlan = struct {
    from: ExecutableValueEndpoint,
    to: ExecutableValueEndpoint,
    provenance: PayloadTransformProvenance,
    op: ExecutablePayloadTransformOp,
};

const PayloadTransformProvenance = union(enum) {
    none,
    box_erasure: NonEmptySpan(BoxBoundaryId),
};

const ErasedPromotedProcedureExecutableSignature = struct {
    specialization_key: ExecutableSpecializationKey,
    source_fn_ty: CanonicalTypeKey,
    wrapper_params: Span(ExecutableProcedureParamPayload),
    wrapper_ret: ExecutableTypePayloadRef,
    wrapper_ret_key: CanonicalExecValueTypeKey,
    erased_call_args: Span(ExecutableTypePayloadRef),
    erased_call_arg_keys: Span(CanonicalExecValueTypeKey),
    erased_call_ret: ExecutableTypePayloadRef,
    erased_call_ret_key: CanonicalExecValueTypeKey,
    hidden_capture: ?ExecutableHiddenCapturePayload,
};

const ExecutableProcedureParamPayload = struct {
    param: PromotedWrapperParam,
    exec_ty: ExecutableTypePayloadRef,
    exec_ty_key: CanonicalExecValueTypeKey,
};

const ExecutableHiddenCapturePayload = struct {
    exec_ty: ExecutableTypePayloadRef,
    exec_ty_key: CanonicalExecValueTypeKey,
};

const ExecutableTypePayloadRef = struct {
    artifact: ArtifactRef,
    payload: ExecutableTypePayloadId,
};

const PromotedWrapperStageOwner = union(enum) {
    mono_finite,
    executable_erased,
};

const MonoSpecializationRequest = struct {
    template: ProcedureTemplateRef,
    requested_fn_ty: ConcreteSourceTypeRef,
    reason: MonoSpecializationReason,
};

const MonoSpecializationReason = union(enum) {
    root: RootRequestId,
    call_proc: ExprId,
    proc_value: ExprId,
    static_dispatch_target: StaticDispatchPlanId,
    comptime_dependency_summary: ComptimeSummaryRequestId,
};
```

`ProcedureTemplateRef` is the identity of a checked procedure-template table
entry. `CallableProcedureTemplateRef` classifies the origin of a callable
procedure template: `checked` for ordinary source-defined or imported checked
procedures, `lifted` for local functions/closures lifted out of an owning mono
specialization, and `synthetic` for compiler-created procedure identities such
as promoted compile-time callable results. A `lifted` callable procedure
template is not allowed to pretend to be a checked top-level procedure; a
`synthetic` callable procedure template must carry a real compiler-created
template and origin payload, not a generated name.

Promoted callable wrappers have two different body owners, and the distinction
is mandatory:

- finite promoted wrappers are source-level synthetic procedure bodies owned by
  mono MIR. They receive ordinary fixed-arity Roc parameters, materialize finite
  private captures as mono values, build a finite `proc_value`, and call it with
  `call_value`. They do not contain erased ABI information.
- erased promoted wrappers are executable-level synthetic procedure bodies owned
  by executable MIR. Checking finalization still publishes their stable
  `ProcedureValueRef`, `ProcBaseKeyRef`, source function type, and complete
  `ErasedPromotedWrapperBodyPlan`, but mono MIR must not lower their body and
  must not import `ErasedFnSigKey`, `CanonicalExecValueTypeKey`, executable
  payload transforms, hidden erased capture arguments, or any other executable
  call signature data.

This is not a runtime thunk or a runtime closure object. For example:

```roc
make_adder : I64 -> (I64 -> I64)
make_adder = |n| |x| x + n

add5 : I64 -> I64
add5 = make_adder(5)
```

`add5` is a finite promoted callable. Checking finalization evaluates the
function-valued root, reifies the selected finite callable member and private
captured `5`, reserves a promoted procedure identity, and seals a finite wrapper
body. Mono lowers that wrapper to ordinary source-level MIR. No runtime thunk,
runtime initializer procedure, runtime top-level closure object, or runtime
global callable-value object is created.

```roc
make_boxed : {} -> Box(I64 -> I64)
make_boxed = |_| Box.box(|x| x + 1)

add1 : I64 -> I64
add1 = Box.unbox(make_boxed({}))
```

`add1` is an erased promoted callable. The erased representation exists only
because the result flowed through the explicit `Box(I64 -> I64)` boundary.
Checking finalization evaluates the function-valued root, reifies the exact
erased code ref, `ErasedFnSigKey`, erased capture materialization plan,
executable payload transforms, the full
`ErasedPromotedProcedureExecutableSignature`, and non-empty `BoxBoundaryId`
provenance, then reserves a promoted procedure identity. Mono
may call or pass that procedure identity as an opaque `ProcedureValueRef` at the
source function type `I64 -> I64`, but mono does not lower the body. Executable
MIR later emits the synthetic procedure body from the published erased plan by
lowering the explicit executable type payloads in
`ErasedPromotedProcedureExecutableSignature`, materializing the explicit capture,
applying the argument payload transforms, issuing `call_erased`, and applying
the result payload transform. No runtime thunk, runtime initializer procedure,
runtime top-level erased callable object, runtime closure object,
source-shape recovery, or canonical-key-to-TypeId cache is allowed.

`MonoSpecializationKey` is keyed by a checked `ProcedureTemplateRef` and the
requested monomorphic source function type. `MonoSpecializedProcRef` is the
identity of the output procedure produced for that request. These identities are
not interchangeable: a mono-specialized output procedure must never be fed back
as the template identity for another mono request. Recursive and mutually
recursive requests reuse the reserved `MonoSpecializedProcRef` for the same
`MonoSpecializationKey`.

`MonoSpecializationRequest` carries the concrete requested function type payload
separately from the specialization key. `requested_fn_ty` is a
`ConcreteSourceTypeRef` into the current MIR-family lowering run's
`ConcreteSourceTypeStore`; `MonoSpecializationKey.requested_mono_fn_ty` is the
canonical identity key read from that payload. The queue may deduplicate by
`MonoSpecializationKey`, but it must retain one canonical payload ref for that
key. If a later request presents the same key with a non-equivalent payload, the
artifact is invalid: debug builds assert immediately and release builds use
`unreachable`.

`ConcreteSourceTypeStore` stores target-independent checked source type payloads
using the same checked type node shape as `CheckedTypeStore`. It is a
construction input for mono/lambda-solved/executable requests, not a public cache
key and not a layout decision. It may copy payload roots from the root artifact,
an imported template closure, a platform/app relation artifact, or the current
specialization-local checked source type graph. It must not contain raw
`types.Var`, raw `Ident.Idx`, `ModuleEnv` pointers, `MonoTypeId`, layout ids, or
source syntax. A canonical key never authorizes reconstruction; it only names
the already-registered payload.

Canonical type keys are stable identities, not serialized type payloads. A
published artifact must contain the checked type graph needed to resolve every
`CanonicalTypeKey` and `CanonicalTypeSchemeKey` it exports or lists in an
imported template closure. Mono specialization must clone-instantiate from that
artifact-owned checked type graph. It must not ask for the exporter
`ModuleEnv`, a checker `types.Store`, a raw `Var`, or a raw `Ident.Idx` in order
to reconstruct the type for an imported generic specialization.

The specialization algorithm mirrors the part of Cor/LSS that is correct:
clone-instantiating the checked type graph, unifying the cloned function root
with the concrete requested function type, and lowering from the cloned graph.
Cor/LSS stores the requested specialization type as the actual type graph
`t_new` and separately uses a lowered monomorphic type as the specialization
deduplication key. Roc must make that split explicit because checked artifacts,
imports, and cache hits cross module boundaries. The requested type graph is
`ConcreteSourceTypeRef`; the deduplication key is `CanonicalTypeKey`.
The difference is that Roc's source graph crosses module/cache boundaries as
explicit checked artifact data. Cor can keep the solved body and type graph in
one process; Roc must not depend on that in-process pointer model for imports or
cache hits.

`NestedProcSite` gives local functions, closures, and compiler-created nested
closure sites stable checked-template-local identity before mono lowering or
lifting. A lifted procedure identity is derived from the owner
`MonoSpecializationKey` plus the `NestedProcSiteId`, not from generated symbol
text, traversal order, expression shape, or the lowered body. Two closures with
the same body shape in different branches must still have different site ids.

The erased promoted wrapper plan must be complete enough to emit the wrapper
without inspecting the callable expression again. `params` are the ordinary
fixed-arity Roc parameters in source order. `arg_transforms` contains exactly
one `ExecutablePayloadTransformPlanId` per ordinary parameter, including
identity transforms. Each transform converts the wrapper parameter endpoint to
the erased-call argument endpoint described by `sig_key` and
`ErasedPromotedProcedureExecutableSignature`. `result_transform` converts the
erased-call result endpoint to the wrapper result endpoint, including the
identity case. `ErasedFnSigKey` already contains the exact `ErasedFnAbiKey`;
there must not be a second standalone ABI field that can diverge from it.
`capture` is a sealed post-evaluation
`ErasedCaptureExecutableMaterializationPlan`, not a pre-evaluation
`ErasedCaptureReificationPlan`, source-level private capture graph, or callable
leaf. `hidden_capture_arg` records whether that exact materialized capture value
is passed to the erased call. `provenance` is non-empty and contains only
explicit `BoxBoundaryId` values. A promoted erased wrapper must not rediscover
any of these decisions from the code ref, source syntax, runtime bytes, layout
shape, or shape comparison.

`ExecutablePayloadTransformPlanId` is an artifact-owned value-transform plan id.
It is not an executable-MIR `BridgeId`, an IR `BridgePlanId`, a layout id, a
row-finalized MIR field id, or a run-local type id. The checked artifact must
publish an `ExecutablePayloadTransformPlanStore` next to the promoted wrapper
body plans and executable type payload store. Every `arg_transforms` entry and
the mandatory `result_transform` point into that store. Executable MIR lowers a
payload transform by first lowering the transform endpoint
`ExecutableTypePayloadRef`s into the current executable type store, then
lowering the already-published transform operation. Executable MIR may use the
endpoint `key` fields only for debug-only verification that the endpoint
payloads match the expected canonical executable types. It must not derive a
transform by comparing source and target shapes.

Executable payload transforms are recursive value-conversion plans, not merely
layout bridges. They are the only artifact-published mechanism that may describe
finite-callable-to-erased-callable packing at an explicit `Box(T)` boundary.
When a transform reaches a callable leaf whose target endpoint is erased, the
operation must be `callable_to_erased` and its `provenance` must be
`box_erasure` with a non-empty `BoxBoundaryId` span. The `finite_value` case
packs the already-evaluated finite callable-set value as the hidden capture for
the erased adapter named by the full `ErasedAdapterKey`; the adapter body must
dispatch with `callable_match`, including singleton sets. The `proc_value` case
packs an already-resolved procedure value and its sealed executable capture
materialization for direct erased calls. The `already_erased_callable` case is
pass-through verification for an input value already represented by exactly the
published `ErasedFnSigKey`. No transform may introduce erased callable
representation without explicit `BoxBoundaryId` provenance.

Structural bridges still exist, but only as a sub-operation of executable
payload transforms. `ExecutableStructuralBridgePlan` covers representation
preserving or purely structural conversions such as `direct`, `zst`,
`nominal_reinterpret`, `box_unbox`, `box_box`, and singleton/tag reshaping.
It must not pack finite callable sets, synthesize erased adapters, materialize
captures, inspect source syntax, or recover callable representation from shape
comparison. Records, tuples, tag unions, nominals, lists, and boxes that contain
callable children must use recursive `ExecutablePayloadTransformPlanId` children
so that callable leaves are transformed by explicit callable operations.
`list_reinterpret` is allowed only when the element endpoint representation is
identical; if a list element transform changes callable representation, the plan
must be a recursive list transform that rebuilds or maps the list explicitly.

Executable payload transforms have two lowering modes, and the distinction is
mandatory:

- **construction lowering**: the value is being built by the current executable
  MIR node. The node must lower each child directly in the target executable
  representation chosen by lambda-solved MIR. It must not first construct the
  aggregate in a source representation and then repair it with an existing-value
  transform. This is the production Roc version of Cor/LSS's correct behavior:
  erasedness is solved before lowering, so constructors build the selected
  representation directly.
- **existing-value lowering**: the input value already exists at runtime or in a
  compile-time materialization store. Executable MIR must evaluate or read that
  input exactly once, then apply the published recursive transform by projection,
  switching, iteration, unboxing, recursive child transforms, and target
  construction. Existing-value transforms are required at real representation
  boundaries such as erased promoted wrapper arguments, erased promoted wrapper
  results, imported constants, private promoted captures, branch joins that have
  already produced a value, and explicit `Box(T)` boundary crossings.

An executable lowering implementation must never use an existing-value transform
as a workaround for missing construction lowering. If a constructor's target
representation is known, constructing directly in that representation is the
only correct implementation. If the target representation is not known by that
point, the previous stage failed to publish required data; debug builds assert
and release builds use `unreachable`.

Transform plans must carry stable semantic labels for structural children.
Record transforms name fields with `RecordFieldLabelId`; tuple transforms name
element indexes; tag-union transforms name source and target `TagLabelId`
pairs plus source and target payload indexes.
Executable MIR maps those labels to local lowered row ids and discriminants
after lowering the endpoint payloads. If a published label or payload index is
absent, duplicated, out of order for the canonical endpoint payload, or has a
child endpoint that does not match the enclosing source/target child types, that
is a compiler invariant violation: debug builds assert at the transform
publication or transform-lowering boundary and release builds use
`unreachable`.

A `tag_union` executable payload transform is an existing-value switch plan, not
a source `match` and not a compatible-shape repair. Each `PayloadTransformTagCase`
maps one source tag label to one target tag label. For ordinary full-union to
full-union transforms the labels are usually identical; singleton/full reshaping
with recursive non-bridge payload transforms must use explicit cases instead of
pretending to be a structural bridge. Each reachable source tag has exactly one
case. Inside a case, payload edges map source payload indexes to target payload
indexes and name the child transform for that payload. Executable MIR lowers
this by evaluating the source union once, switching on the finalized source
discriminant, extracting payloads by finalized source `TagPayloadId`, applying
child transforms in target logical payload order, and constructing the target
tag with the finalized target `TagId`. The generated default branch is compiler
`unreachable`; it is not a user-facing runtime error.

A `list` executable payload transform is an existing-value compiler-owned list
loop. It must not call Roc `List.map`, create a callable value, or depend on a
user-visible module import. If the element transform is identity and the list
endpoint representation is identical, the transform is identity or
`list_reinterpret` as appropriate. Otherwise executable MIR must lower the list
transform by evaluating the source list once, reading its length, allocating a
fresh target list with that length/capacity policy, iterating by index in source
order, reading each element with the checked low-level list access operation,
applying the child payload transform to the element, and writing/appending the
transformed element to the target list. The low-level operations used for the
loop must have explicit value-flow, ABI, and reference-counting metadata. ARC
insertion emits the required `incref`, `decref`, and `free`; backends still only
follow explicit LIR statements.

A `box_payload` executable payload transform is directional. `payload_to_box`
transforms an unboxed payload value and allocates a fresh `Box(T)` containing
the transformed payload. `box_to_payload` unboxes the source box exactly once,
applies the child payload transform, and returns the transformed payload.
`box_to_box` unboxes the source box exactly once, applies the child payload
transform, and allocates a fresh target box. Reusing or mutating an existing box
is only a future optimization through an explicit runtime uniqueness mutation
site where `refcount == 1`; the required baseline is fresh allocation. The
presence of a box transform does not itself authorize callable erasure. Callable
erasure is authorized only when the callable leaf transform has non-empty
`box_erasure` provenance naming the explicit `BoxBoundaryId`.

Checking finalization and lambda-solved representation solving are responsible
for publishing executable payload transform plans. They must publish a transform
for every erased promoted wrapper ordinary argument and for every erased
promoted wrapper result, including identity transforms. Finite promoted wrappers
do not publish or consume executable payload transforms for their wrapper bodies
because they are mono-MIR bodies, not executable-MIR bodies. If an endpoint's
keys differ and the transform is `identity`, or if a transform's endpoint keys
do not match the wrapper signature, the erased promoted wrapper is invalid;
later stages must not repair that by re-running representation solving,
comparing compatible shapes, reading the source expression, or synthesizing an
adapter from the erased code ref.

`ErasedCaptureReificationPlan`, source-level private capture graphs, and
`ErasedCaptureExecutableMaterializationPlan` are different type states.
`ErasedCallableResultPlan` may contain a reification plan while checking
finalization has not yet interpreted the compile-time root. That plan is a
recipe for reading the root's LIR interpreter result and converting it into
compiler-owned data. When `publishErasedCallableResult` seals the promoted
wrapper, it must consume the interpreter result and replace the recipe with an
explicit executable materialization plan. A sealed
`ErasedPromotedWrapperBodyPlan` must never contain
`CaptureSlotReificationPlanId`, `CallableResultPlanId`, `PrivateCaptureRef`,
`CallableLeafInstance`, or any other pre-executable record as the data
executable MIR is expected to lower.

`ErasedPromotedProcedureExecutableSignature` is mandatory because an erased
promoted procedure body skips mono, row-finalized mono, lifted MIR, and
lambda-solved body lowering. Executable MIR therefore cannot obtain parameter,
result, erased-call argument, erased-call result, or hidden-capture executable
`TypeId`s from lambda-solved value occurrences in a lowered body. The signature
must publish executable type payload refs as well as their canonical keys.
Executable MIR lowers those payload refs directly and uses the keys only for
debug-only verification. It must not maintain a map from
`CanonicalExecValueTypeKey` to executable `TypeId`, ask the checked source type
store to reconstruct executable representation, inspect the source expression,
or recover missing executable types from layouts or erased ABI shape.

`ExecutableTypePayloadRef` points into an artifact-owned
`ExecutableTypePayloadStore`. This store contains structural executable type
payloads using the same semantic cases as `CanonicalExecValueTypeKey`:
primitive, record, tuple, tag union, list, box, nominal, callable-set, and
erased-fn. Recursive payloads use store-local placeholders/backrefs, not raw
Zig pointers, raw lambda-solved type ids, raw checked type ids, layout ids,
expression ids, or allocation-order-dependent handles. The store is not a cache;
it is published semantic data produced by lambda-solved MIR while checking
finalization is still evaluating/promoting compile-time callable roots.

For an erased promoted procedure:

- `specialization_key` is the exact executable specialization key for the
  promoted procedure body. It uses the wrapper source function type, wrapper
  parameter executable keys, wrapper result executable key, callable mode
  `erased_callable`, and the published capture shape.
- `wrapper_params` are the ordinary fixed-arity Roc parameters. Each entry
  carries the source `PromotedWrapperParam`, the executable type payload ref for
  the parameter value, and the canonical executable key for debug verification.
- `wrapper_ret` and `wrapper_ret_key` describe the value returned by the
  promoted procedure after applying `result_transform`, not merely the erased-call
  ABI return.
- `erased_call_args` and `erased_call_arg_keys` describe the exact ABI argument
  values passed to `call_erased` after applying `arg_transforms`. Their arity
  must exactly match the fixed arity encoded by `sig_key`.
- `erased_call_ret` and `erased_call_ret_key` describe the raw erased-call result
  before applying `result_transform`. `erased_call_ret_key` must exactly match
  the return key encoded by `sig_key`.
- `hidden_capture` is present exactly when `sig_key.capture_ty` is non-null. Its
  `exec_ty_key` must equal `sig_key.capture_ty`. If `sig_key.capture_ty` is null,
  `hidden_capture` must be null. Runtime pointer nullness, runtime capture byte
  size, and backend ABI behavior must not decide this.

All mismatches are compiler invariant violations: debug builds assert at the
first boundary that observes the mismatch and release builds use `unreachable`.

An erased promoted wrapper plan is consumed by executable MIR, not by mono MIR.
The checked artifact publishes it early so that all later stages have explicit
procedure identity and dependency information, but executable MIR is the first
stage allowed to read `sig_key`, `hidden_capture_arg`,
`ErasedPromotedProcedureExecutableSignature`, erased payload transforms, or
erased capture materialization. Mono, row-finalized mono, lifted MIR, and
lambda-solved MIR may carry the erased promoted wrapper's opaque
procedure identity and source function type through `call_proc`, `proc_value`,
and `call_value` operands, but they must not lower or inspect its body. If mono
specialization attempts to lower an erased promoted wrapper body, that is a
compiler invariant violation: debug builds assert immediately and release builds
use `unreachable`.

The finite promoted wrapper plan is the selected finite callable value after
compile-time evaluation, not a bare function symbol. `source_fn_ty` is the exact
canonical fixed-arity function type of the promoted callable result.
`callable_set_key` is the canonical finite callable-set representation chosen by
lambda-solved MIR. `member` is the selected member inside that set. `captures`
are the already-reified private capture values in the member's canonical
capture-slot order. The member procedure and capture-slot schema are derived
from `callable_set_key + member`; if an implementation caches them in the plan,
debug builds must assert that they match and release builds must treat a mismatch
as `unreachable`. A finite promoted wrapper must not store only `entry_proc:
Symbol`, because that loses the callable-set member context and the capture
payload shape that Cor/LSS preserves when it builds callable-set tags.

A finite promoted wrapper plan is consumed by mono MIR. It is intentionally
source-level: it names the selected finite callable-set member, ordinary Roc
parameters, and ordinary private captures. It must not carry bridge ids. Because
it contains no erased ABI and no executable representation, mono can lower it to
ordinary `proc_value` and `call_value` MIR. If a finite promoted wrapper needs
to cross a later
explicit `Box(T)` erased boundary, lambda-solved/executable MIR handle that as
the normal finite-callable-to-erased adapter case at the boundary; mono must not
pre-package it as erased.

`ProcedureCallableRef` is the canonical identity for one resolved procedure
value occurrence before executable representation solving. `template` is the
stable `CallableProcedureTemplateRef`; `source_fn_ty` is the exact canonical
resolved fixed-arity function type at which that procedure value is used. It is
not a callable-set member, not an erased code ref, not a layout key, and not an
executable specialization by itself. Generic procedure values with the same
`template` but different `source_fn_ty` are different procedure value
occurrences.

`ErasedCallableCodeRef` has exactly two cases. `direct_proc_value` is an
already-resolved direct procedure code path and therefore carries
`ErasedDirectProcCodeRef`: the exact procedure value occurrence plus the
canonical hidden-capture shape needed by the erased specialization. It is not a
finite callable leaf instance; finite callable leaves are closed no-capture
constant graph leaves, while direct erased code may be a lifted, promoted,
imported, or source procedure whose erased specialization expects an explicit
hidden capture record. A bare procedure template is insufficient because the
erased signature does not uniquely recover the source function type, generic
instantiation, nominal wrappers, capability data, or capture shape that selected
the erased procedure specialization. `finite_set_adapter` identifies an adapter
that will call a finite callable-set value through `callable_match`. The
`ErasedFiniteSetAdapterRef` embedded in `ErasedCallableCodeRef` carries the
target-independent source function type and canonical callable-set key, but that
record is not the synthetic adapter identity. The exact adapter identity is
always the full executable key:

```zig
const ErasedAdapterKey = struct {
    source_fn_ty: CanonicalTypeKey,
    callable_set_key: CanonicalCallableSetKey,
    erased_fn_sig_key: ErasedFnSigKey,
    capture_shape_key: CaptureShapeKey,
};
```

The selected member and its capture payload are stored in the enclosing erased
value's capture materialization plan, not in the adapter code ref. The enclosing
erased value also carries the `ErasedFnSigKey`; the materialized capture node
carries the `capture_shape_key`. Executable MIR derives and reserves exactly one
`ErasedAdapterKey` from those four inputs before lowering the adapter body. No
adapter cache, adapter reservation table, executable specialization table,
snapshot expectation, or debug verifier may identify a finite-set erased adapter
by only the source function type and callable-set key. Omitting either
`erased_fn_sig_key` or `capture_shape_key` is a compiler invariant violation:
debug builds assert immediately and release builds use `unreachable`. No erased
callable code ref may store a finite callable leaf, executable specialization
key, layout id, generated symbol text, runtime function pointer, or target ABI
handle.

`CallableLeafInstance` is not one universal capture representation. It has two
allowed homes with different stage contracts:

- compile-time constant graphs may contain finite callable leaves and erased
  boxed callable leaves, because executable MIR materializes concrete
  `ConstInstanceRef` values from explicit target-specific materialization plans
  before IR
- source-level private promoted-capture graphs may contain only finite callable
  leaves, because they are consumed first by mono MIR and then pass through
  row-finalized mono, lifted MIR, lambda-solved MIR, and executable MIR in order

The finite leaf case remains the only callable leaf shape that source private
capture graphs may store:

```zig
const FiniteCallableLeafInstance = struct {
    proc_value: ProcedureCallableRef,
};
```

A finite callable leaf must not store `CanonicalCallableSetKey`,
`CallableSetMemberId`, `CaptureShapeKey`, executable specialization keys,
layout ids, generated symbol text, runtime function pointers, or runtime capture
pointers. Those are not known at this type state, and adding them here would
create a second source of truth competing with lambda-solved callable-set
descriptors.

An erased boxed callable leaf is executable-ready data, not source-level private
capture data. It is valid only inside compile-time constant graph instances and
executable erased capture materialization graphs. It must never appear in
`PrivateCaptureNode`, `PrivateCaptureRef`, mono MIR input, row-finalized mono
input, lifted MIR input, or lambda-solved MIR input. If source private capture
construction reaches a non-function value subtree whose concrete representation
contains an erased boxed callable payload, it must stop the source private graph
at that subtree and store a concrete private `ConstInstanceRef` leaf instead.
Executable MIR later materializes that const instance through the normal
constant materialization path. If source private capture construction reaches a
function-typed value, it must recursively promote that callable to a sealed
procedure value and store a finite `ProcedureCallableRef` leaf. It must not store
`ErasedFnSigKey`, erased callable code refs, executable hidden-capture plans, or
boxed-erased callable payloads in the source private capture graph.

Sealed erased promoted wrappers are different. An
`ErasedPromotedWrapperBodyPlan` is consumed first by executable MIR, because the
wrapper body intentionally skips mono, row-finalized mono, lifted MIR, and
lambda-solved body lowering. Therefore its capture field must not contain
source-level private capture refs or callable leaves that still require those
skipped stages. The sealed executable input is an
`ErasedCaptureExecutableMaterializationPlan`, not a generic private capture graph
and not a compile-time reification recipe.

Conceptually:

```zig
const ErasedCaptureExecutableMaterializationPlan = union(enum) {
    none,
    zero_sized_typed: CanonicalExecValueTypeKey,
    node: ErasedCaptureExecutableMaterializationNodeId,
};

const ErasedCaptureExecutableMaterializationNode = union(enum) {
    // General constant materialization. The referenced const instance may contain
    // callable leaves, including erased boxed callable leaves, so executable MIR
    // must use the full callable-aware constant materialization path.
    const_instance: ConstInstanceRef,
    pure_const: PureConstInstanceRef,
    pure_value: PureComptimeValueRef,
    finite_callable_set: MaterializedFiniteCallableSetValue,
    erased_callable: MaterializedErasedCallableValue,
    record: StableErasedCaptureRecordMaterialization,
    tuple: Span(ErasedCaptureExecutableMaterializationPlan),
    tag_union: StableErasedCaptureTagMaterialization,
    list: StableErasedCaptureListMaterialization,
    box: StableErasedCaptureBoxMaterialization,
    nominal: StableErasedCaptureNominalMaterialization,
    recursive_ref: ErasedCaptureExecutableMaterializationNodeId,
};

const PureConstInstanceRef = struct {
    const_instance: ConstInstanceRef,
    no_reachable_callable_slots: NoReachableCallableSlotsProof,
};

const PureComptimeValueRef = struct {
    schema: ComptimeSchemaId,
    value: ComptimeValueId,
    no_reachable_callable_slots: NoReachableCallableSlotsProof,
};

const MaterializedFiniteCallableSetValue = struct {
    source_fn_ty: CanonicalTypeKey,
    callable_set_key: CanonicalCallableSetKey,
    selected_member: CallableSetMemberId,
    captures: Span(ErasedCaptureExecutableMaterializationPlan),
};

const MaterializedErasedCallableValue = struct {
    source_fn_ty: CanonicalTypeKey,
    sig_key: ErasedFnSigKey,
    code: ErasedCallableCodeRef,
    capture: ErasedCaptureExecutableMaterializationPlan,
    provenance: NonEmptySpan(BoxBoundaryId),
};

const StableErasedCaptureRecordMaterialization = struct {
    fields: Span(StableErasedCaptureRecordField),
};

const StableErasedCaptureRecordField = struct {
    field: RecordFieldLabelId,
    value: ErasedCaptureExecutableMaterializationPlan,
};

const StableErasedCaptureTagMaterialization = struct {
    tag: TagLabelId,
    payloads: Span(StableErasedCaptureTagPayload),
};

const StableErasedCaptureTagPayload = struct {
    payload_index: u32,
    value: ErasedCaptureExecutableMaterializationPlan,
};

const StableErasedCaptureListMaterialization = struct {
    elems: Span(ErasedCaptureExecutableMaterializationPlan),
};

const StableErasedCaptureBoxMaterialization = struct {
    payload: ErasedCaptureExecutableMaterializationPlan,
};

const StableErasedCaptureNominalMaterialization = struct {
    nominal: NominalTypeKey,
    backing: ErasedCaptureExecutableMaterializationPlan,
};

const ErasedHiddenCaptureArgPlan = union(enum) {
    none,
    materialized_capture: ErasedCaptureExecutableMaterializationPlan,
};
```

The exact Zig names may differ, but the type-state boundary must not. A sealed
erased promoted wrapper may contain only executable-ready materialization
records. It must not contain `PrivateCaptureRef`, `CallableLeafInstance`,
`CaptureSlotReificationPlanId`, `CallableResultPlanId`, source expressions,
source symbols, interpreter memory addresses, runtime closure objects, backend
function pointers, raw pointer/nullness tests, runtime byte sizes, layout ids,
`ExecutableValueRef`, or syntax-derived lookup.

The `StableErasedCapture*Materialization` records are deliberately stable checked
artifact data. They must not store MIR-run-local `RecordShapeId`,
`RecordFieldId`, `TagUnionShapeId`, `TagId`, or `TagPayloadId` values. Those ids
are allocated by a particular row-finalized/executable lowering run and cannot be
persisted in an imported checked artifact. Instead, sealed erased capture
materialization stores canonical labels and canonical logical payload indexes
that are stable across lowerings:

- records store `RecordFieldLabelId` plus a child materialization plan per field
- tag values store `TagLabelId` plus canonical payload indexes and child plans
- tuples, lists, boxes, and nominals store child plans in logical order

Executable MIR lowers the expected executable type payload for the hidden capture
first. That creates the local `RecordShapeId`, `RecordFieldId`,
`TagUnionShapeId`, `TagId`, and `TagPayloadId` records for this lowering run.
Then executable MIR maps the stable materialization labels/indexes onto those
local ids by checking the already-lowered expected type. This is not source
recovery and not a heuristic: the expected executable type payload and the
stable materialization node are both explicit checked-artifact data. Debug builds
must assert immediately if a materialized record field, tag, or payload index is
not present in the expected executable type, if arities differ, or if canonical
ordering disagrees. Release builds use `unreachable`.

`PureConstInstanceRef` is allowed only when the referenced constant instance has
no reachable callable slots. If the exact constant instance contains a function
anywhere in a record, tuple, tag payload, list element, `Box(T)` payload,
transparent alias, or nominal backing value, checking finalization must expand
that portion into structural executable materialization nodes with explicit
`finite_callable_set` or `erased_callable` leaves. Executable MIR must never
open a generic compile-time constant graph to rediscover callables.

Finite callable-set adapter captures are the important selected-finite case.
`MaterializedFiniteCallableSetValue` stores exactly the source function type,
callable-set key, selected member, and executable-ready materialized capture
values in canonical descriptor capture-slot order. It must not cache member
procedure refs, source proc refs, capture shapes, capture-slot schemas,
executable proc ids, or layout data. Those records live in the published
callable-set descriptor store described below. Duplicating them here would create
parallel semantic inputs that could diverge.

Executable MIR materializes the `captures`, constructs the finite
`callable_set_value`, packs that value as the erased hidden capture, and reserves
the adapter under the full `ErasedAdapterKey`. If the materialized finite
callable-set value disagrees with the adapter key or descriptor, the artifact is
invalid: debug builds assert immediately and release builds use `unreachable`.

Checking finalization must publish an artifact-owned callable-set descriptor
store. This store is not a cache and not a lookup accelerator; it is semantic
input required after the lambda-solved session that created a descriptor is no
longer available.

Conceptually:

```zig
const CallableSetDescriptorRef = struct {
    artifact: ArtifactRef,
    key: CanonicalCallableSetKey,
};

const CallableSetDescriptorStore = struct {
    descriptors: Span(CanonicalCallableSetDescriptor),
};
```

Every `CanonicalCallableSetDescriptor` reachable from any of these records must
be copied into the owning checked artifact before publication:

- `CallableResultPlan.finite`
- `CallableValueEmissionPlan.finite`
- `CallableSetConstructionPlan`
- `ErasedAdapterKey`
- `ErasedCallableCodeRef.finite_set_adapter`
- `ExecutableTypePayload.callable_set`
- `MaterializedFiniteCallableSetValue`
- promoted wrapper bodies
- compile-time constants and private capture graphs that contain callable
  values
- imported template closures that expose any of the above

The descriptor record must include every member's `CallableSetMemberId`,
`ProcedureCallableRef`, `MirProcedureRef`, `CaptureShapeKey`, and dense
`CallableSetCaptureSlot` list. Executable MIR may build a transient hash table
from key to descriptor for performance during one lowering run, but the
published descriptor store remains the single semantic input. Debug builds must
assert that every duplicate key carries an identical descriptor, every selected
member exists, every capture list length equals the descriptor member's slot
list length, and every capture executable type key equals the descriptor slot's
`exec_value_ty`. Release builds must not retain verifier scanning cost.

The old ambiguous `values: Span(CaptureSlotReificationPlan)` shape is forbidden
in sealed erased promoted wrapper bodies. Before interpretation, the producer
must distinguish at least these recipe shapes:

- `whole_hidden_capture_value`: one source value whose executable
  representation is exactly `sig_key.capture_ty`
- `proc_capture_tuple`: ordered procedure capture slots that must be assembled
  into the erased hidden capture tuple for a direct erased proc-value code ref
- `finite_callable_set_value`: a callable-result plan whose interpreted value
  selects a finite callable-set member for a finite-set erased adapter

Checking finalization consumes those recipes against the interpreted LIR value
while the lambda-solved representation store and callable-set descriptors are
still available. It then publishes only
`ErasedCaptureExecutableMaterializationPlan` plus descriptor-store entries.
Executable MIR must never receive the recipe form and must never decide which of
these cases it has by looking at arity, type shape, capture byte size, code-ref
variant, or runtime data.

`ErasedHiddenCaptureArgPlan` must say exactly which materialized capture value
is passed to erased code, or state that no hidden capture argument is passed.
The hidden argument decision comes from `ErasedFnSigKey.capture_ty` plus the
materialization plan above. It must not be inferred from runtime byte size,
pointer nullness, source syntax, backend layout, or whether a particular
procedure happens to need a non-empty capture record after optimization. These
plans are executable inputs, not verifier hints.

#### Resolved Value References Before Mono MIR

Checking finalization must classify every value-like reference that can reach
mono MIR before the checked artifact is published. Mono MIR must consume that
classification. It must not decide whether a reference is local, top-level,
   imported, hosted, platform-required, promoted, or callable by scanning names,
source syntax, export tables, declaration order, or the later capture graph.

Conceptual shape:

```zig
const ResolvedValueRefId = enum(u32) { _ };

const ConstUseTemplate = struct {
    const_ref: ConstRef,
    requested_source_ty_template: CanonicalTypeTemplateKey,
};

const ProcedureBindingRef = union(enum) {
    top_level: TopLevelProcedureBindingRef,
    imported: ImportedProcedureBindingRef,
    hosted: HostedProcRef,
    platform_required: RequiredAppProcedureRef,
    promoted: PromotedProcedureRef,
};

const ProcedureUseTemplate = struct {
    binding: ProcedureBindingRef,
    source_fn_ty_template: CanonicalTypeTemplateKey,
};

const ResolvedValueRef = union(enum) {
    local_param: ParamId,
    local_value: LocalBindingId,
    local_mutable_version: MutableVersionId,
    pattern_binder: PatternBinderId,
    local_proc: LocalProcRef,

    top_level_const: ConstUseTemplate,
    imported_const: ConstUseTemplate,

    top_level_proc: ProcedureUseTemplate,
    imported_proc: ProcedureUseTemplate,
    hosted_proc: ProcedureUseTemplate,
    platform_required_declaration: PlatformRequiredDeclarationId,
    platform_required_const: ConstUseTemplate,
    platform_required_proc: ProcedureUseTemplate,
    promoted_top_level_proc: ProcedureUseTemplate,
};

const ResolvedValueRefRecord = struct {
    expr: CheckedExprId,
    ref: ResolvedValueRef,
    checked_ty: CheckedTypeId,
    scope_depth: u32,
};

const ResolvedValueRefTable = struct {
    records: Store(ResolvedValueRefRecord),
    by_expr: Map(CheckedExprId, ResolvedValueRefId),
};
```

The exact Zig names may differ, but the partition must not. The published table
is sealed. It must not contain pending top-level bindings, unresolved import
lookups, source names, or expression shapes that later stages have to classify.

Checking finalization may build the table in two internal phases:

```zig
const ResolvedValueRefBuilderCase = union(enum) {
    sealed: ResolvedValueRef,
    local_top_level_binding: TopLevelBindingRef,
    imported_top_level_binding: ImportedTopLevelValueRef,
};
```

The builder-only `local_top_level_binding` case exists only while checking
finalization is reserving top-level table rows. Local compile-time constants must
receive reserved `ConstRef` template identities before dependency analysis and
before compile-time evaluation; evaluation fills and seals those templates later.
Local function-valued roots may be promoted before publication, but the sealed
resolved reference must still name an explicit `ProcedureUseTemplate`, not a
generated symbol. The builder-only `imported_top_level_binding` case exists only
while the importer is reading the imported artifact view. Both builder-only
cases must be resolved to sealed `top_level_const`, `imported_const`,
`top_level_proc`, `imported_proc`, or `promoted_top_level_proc` entries before
the checked artifact is published.

A value reference that names a top-level binding must be represented by a
top-level or imported case in the sealed table. A value reference that names a
local binder shadowing a top-level binding must be represented by a local case.
This decision is based on resolved binding identity, not display text.

The table is part of the checked artifact because it is checked semantic data.
It is not a mono-MIR analysis cache, not a capture-discovery result, and not a
debug-only helper. Importing modules receive only the exported/importable pieces
of this classification through `ImportedModuleView`; they must not inspect
private checked CIR to recreate it.

Mono MIR lowering consumes `ResolvedValueRef` as follows:

- `local_param`, `local_value`, `local_mutable_version`, and `pattern_binder`
  become ordinary specialization-local value references.
- `local_proc` may appear in mono and row-finalized mono as a local callable
  reference only until lifting turns it into explicit `proc_value` data with
  captures. It must not become `call_proc` before lifting.
- `top_level_const` and `imported_const` carry a `ConstUseTemplate`. Mono MIR
  clone-instantiates `requested_source_ty_template` in the current specialization
  into the current run's `ConcreteSourceTypeStore`. The resulting
  `ConcreteSourceTypeRef` is the construction payload for the concrete const
  instance, and its canonical key is the identity portion of the
  `ConstInstantiationKey` used by runnable MIR. The checked artifact must not
  store a concrete `ConstInstantiationKey` for a generic procedure body or
  generic constant template.
- `top_level_proc`, `imported_proc`, `hosted_proc`,
  `platform_required_proc`, and `promoted_top_level_proc` carry
  `ProcedureUseTemplate`. In callee position,
  mono MIR clone-instantiates `source_fn_ty_template` into the current run's
  `ConcreteSourceTypeStore`, resolves any static dispatch required by that
  function type payload, resolves the named procedure binding at that concrete
  function type, and then creates the concrete `ProcedureCallableRef`/mono
  specialization request needed for `call_proc`. The resulting
  `ProcedureCallableRef.source_fn_ty` is the payload's canonical key; the mono
  specialization request also carries the `ConcreteSourceTypeRef` payload.
- `platform_required_const` carries `ConstUseTemplate`. Mono MIR treats it like
  an imported constant use owned by the app artifact and addressed through the
  platform/app relation; it must not synthesize a platform-local procedure.
- `platform_required_declaration` is allowed only in standalone platform
  artifacts that publish requirement declarations for checking, docs, or glue
  metadata without an app-specific relation. Executable platform artifacts must
  replace every required lookup with `platform_required_const` or
  `platform_required_proc` before publication. Mono MIR and later executable
  lowering must treat `platform_required_declaration` as a compiler invariant
  violation if it appears in any executable lowering input.
- The same procedure cases in value position become `proc_value` with empty
  captures after mono specialization has resolved the binding to the exact
  callable procedure template and monomorphic source function type.

`ConstUseTemplate.requested_source_ty_template` and
`ProcedureUseTemplate.source_fn_ty_template` are template identities. They are
not concrete payloads by themselves. In a generic checked procedure body, the
same template may instantiate to different concrete payloads in different mono
specializations. Mono MIR must store the concrete payload ref produced in the
current specialization and must not enqueue a const/procedure/callable request
from only the canonical key.

`ProcedureUseTemplate` deliberately does not store a
`CallableProcedureTemplateRef`. A function-valued top-level binding can be a
direct checked/imported/hosted/platform-required/promoted procedure binding, or it can be
an instantiable compile-time callable evaluation template. The concrete callable
procedure template is available only after the use's `source_fn_ty_template` has
been clone-instantiated in a concrete lowering context. Mono MIR must not
pretend that a generalized function-valued binding already has one concrete
callable template, and it must not recover that template from source syntax,
symbol names, export tables, or environment lookup.

For `promoted_top_level_proc`, the consumed procedure value must point at a
`ProcBaseKeyRef` that already has a `PromotedProcedureTable` row and a checked
template in the artifact being lowered or imported. Mono MIR is not allowed to
accept a generated symbol, display name, or placeholder procedure value as a
promise that a body will be produced later.

Mono MIR must not lower a top-level, imported, hosted, platform-required, or
promoted procedure as `call_value(var_(proc), args)`. It must not lower a
top-level or imported constant as an ordinary local `var_`. It must not leave a
raw source symbol for lifted MIR to classify later.

This is the production replacement for Cor/LSS's prototype-level
`ctx.toplevels` subtraction during capture discovery. Cor uses that set
subtraction because its prototype representation starts from raw symbols. Roc's
production compiler must prevent the mistaken capture earlier: by the time
capture discovery runs, top-level and imported values are already `const_ref`,
`call_proc`, or empty-capture `proc_value`, not ordinary capturable refs.

Debug-only verification after mono MIR must assert:

- every checked value reference that reached mono MIR consumed exactly one
  `ResolvedValueRefRecord`
- no top-level, imported, hosted, platform-required, or promoted value was
  emitted as an ordinary local value reference
- no `platform_required_declaration` reached an executable mono lowering input
- every `const_ref` came from `top_level_const`, `imported_const`, or
  `platform_required_const`
- every top-level/imported/hosted/platform-required/promoted procedure call was emitted
  as `call_proc`, not as `call_value(var_(proc), args)`
- every top-level/imported/hosted/platform-required/promoted procedure value was emitted
  as empty-capture `proc_value`
- local shadowing uses binding identity: a local binder with the same display
  name as a top-level binding is still local and may be captured later
- no mono MIR node requires lifted MIR, lambda-solved MIR, executable MIR, IR,
  or LIR to decide whether a value reference is local or global

Release builds use `unreachable` for the equivalent compiler-invariant paths.

The exact Zig names may differ, but the boundary must not. Mono MIR consumes
checked procedure templates only by clone-instantiating one concrete
`MonoSpecializationRequest`. A top-level checked function declaration, generic
export, imported function, hosted wrapper, or promoted procedure value must not
be lowered into mono MIR merely because it exists.

The mono specialization queue is the only way to turn a checked procedure
template into mono MIR. The queue must reserve the output `ProcedureValueRef` and
implementation-local procedure handle before lowering the body, then lower
exactly one specialization-local body with exactly one specialization-local mono
type store. Re-entering an in-progress specialization returns the reserved
procedure value and handle and records the dependency edge; it must not lower the
same checked body a second time with a partially resolved type store.

Mono MIR must not have a `lowerAllTopLevelFunctions`, "emit every function",
"lower every export", or equivalent eager path. Direct top-level functions are
checked procedure templates and may also be published as top-level
`procedure_binding` entries, but their mono bodies are still produced only when a
concrete root request, direct call, static-dispatch target, first-class
`proc_value`, or compile-time dependency summary requests an exact
`MonoSpecializationKey`.

This is mandatory for generic procedures and for any procedure whose body
contains static dispatch. Static dispatch can be eliminated only while lowering a
concrete mono specialization, after the enclosing procedure's requested function
type and the dispatch expression's `callable_ty`, arguments, and result slot
have been clone-instantiated into the same mono type store.

Exported checked artifacts may contain generic procedure templates. Exported
mono MIR may not contain generic procedure bodies, checked procedure templates,
unchecked static-dispatch nodes, or pending mono-specialization work.

Exported checked artifacts may also contain generic callable binding templates.
Those templates are `TopLevelProcedureBindingRef` entries whose body is
`callable_eval_template`. They are checked artifact data, not mono MIR. A
consumer turns one into a concrete callable value only by requesting a
`CallableBindingInstantiationRequest` at a fully resolved fixed-arity source
function type. The request carries both the `CallableBindingInstantiationKey`
for identity and a `ConcreteSourceTypeRef` for the requested function type
payload.

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
    eval_order: Span(RecordFieldEval),
    assembly_order: Span(RecordFieldAssembly),
}

record_access: struct {
    record: ExprId,
    field: RecordFieldId,
}

tag_construct: struct {
    union_shape: TagUnionShapeId,
    tag: TagId,
    eval_order: Span(TagPayloadEval),
    assembly_order: Span(TagPayloadAssembly),
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

`LiftedCaptureGraph` discovery must descend through every expression shape that
can carry a procedure value. This includes records, tuples, tags, lists,
`Box(T)`, transparent nominal wrappers, source `match` branch results, `if`
branch results, aliases, returns, captures, and SSA mutable/loop joins. A
reference to a self, sibling, local function, or closure anywhere inside those
structures creates a `CaptureProcValueEdge`. Any external value needed to build
that nested `proc_value` capture payload creates a `CaptureValueEdge`.

This nested scan is mandatory. A recursive local group is incorrect if it only
records direct body references like `loop(...)` and misses a procedure value
hidden inside `{ f: loop }`, `Ok(loop)`, `[loop]`, a branch result, or a value
captured by another local procedure. The fixed point is over the full nested edge
set, not over syntactically direct calls.

For each recursive local-function group, lifted MIR must:

1. Allocate stable procedure values and implementation-local procedure handles
   for every group member before lowering any member body.
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

Capture discovery must be symbol- and version-based, not name-based.

Lifted MIR must lower through an explicit lexical scope builder:

```zig
const LiftScopeFrame = struct {
    bindings: Map(Symbol, BindingLocation),
    mutable_versions: Map(Symbol, MutableVersionId),
};

const BindingLocation = union(enum) {
    local_value: ExprId,
    lambda_param: ParamId,
    pattern_binder: PatternBinderId,
    local_proc: Symbol,
    captured_slot: CaptureSlot.Index,
};
```

The exact Zig field names may differ, but the responsibility must not.
`LiftScopeFrame.bindings` is a lexical scope for values that may participate in
local capture analysis. It is not a global name environment and not an import
lookup table.

Lambda parameters, local function names, recursive local-function group
members, source `match` pattern binders, record destructuring binders, tuple
destructuring binders, `for` binders, block-local declarations, and shadowed
declarations all enter the lexical scope stack as resolved symbols. The builder
must not compare display names to decide whether a reference is local or
captured.

Top-level constants, imported constants, top-level procedures, imported
procedures, hosted procedures, platform-required procedure bindings, and
promoted top-level procedures must not enter `LiftScopeFrame.bindings`. They are
already explicit mono MIR operations:

- `const_ref` for top-level and imported compile-time constants
- `call_proc` for resolved direct procedure calls
- empty-capture `proc_value` for resolved
  top-level/imported/hosted/platform-required/promoted procedure values

Lifted MIR must not implement Cor/LSS-style free-variable capture by collecting
raw symbols and subtracting a `toplevels` set. A debug verifier may assert that
no top-level/imported/hosted/platform-required/promoted value appears in a capturable
binding set, but successful verification is not part of the lowering algorithm.
The lowering algorithm consumes the explicit value-reference partition produced
before mono MIR.

A local binding that shadows a top-level or imported name is different: it
enters the lexical scope stack with its own resolved local identity and may be
captured normally. Shadowing correctness must be tested by identity, not by
display name.

Mutable source variables must already be represented as explicit versions or
version records by the time capture discovery needs to reason about them. A
captured mutable value is a captured version or phi input, not a captured
physical mutable cell. Capturing a mutable value does not allow a later stage to
reopen source mutation or infer representation from assignments.

Recursive local-function groups must reserve every member symbol before capture
analysis begins. Capture discovery then computes the least fixed point over the
reserved member graph, including captures required to build `proc_value` values
for self and sibling members. Only after the fixed point is complete may lifted
MIR lower member bodies.

Every captured read must lower to `capture_ref(slot)`. A captured source symbol,
captured mutable version, captured pattern binder, or captured sibling procedure
must not remain as an ordinary `var_` inside the lifted body. Debug verification
must assert this immediately after lifting; release builds use `unreachable` for
the equivalent compiler-invariant path.

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

Lifted-to-lambda-solved type import is an explicit stage boundary.

Every lifted MIR function type imported into lambda-solved MIR becomes a
lambda-solved function type with a fresh callable slot:

```zig
const CallableVarId = enum(u32) { _ };

const LambdaSolvedFnType = struct {
    fixed_arity: u32,
    args: Span(TypeId),
    ret: TypeId,
    callable: CallableVarId,
};
```

The exact Zig names may differ, but the import rule must not. A lifted MIR
function type does not enter lambda-solved MIR as only an argument list and a
return type. It always gets an explicit callable variable that lambda-set
solving will resolve to either a finite callable set or erased callable
representation.

This rule applies everywhere a function type can appear:

- expression results
- `let` binders
- mutable variable versions
- pattern binders
- procedure parameters
- procedure returns
- capture slots
- `proc_value.fn_ty`
- `call_proc.requested_fn_ty`
- `call_value.requested_fn_ty`
- record fields
- tuple elements
- tag payloads
- `List(T)` elements
- `Box(T)` payloads
- nominal backing slots
- nested function argument and return positions

Freshness is per imported type occurrence. Two expression roots that reference
the same lifted MIR logical `TypeId` still receive distinct callable variables
unless an explicit value-flow edge connects those roots later. The lifted
logical type says the source type shape; it does not say that two runtime values
must share callable representation.

The import algorithm must still be cycle-safe. It uses a placeholder cache
inside one structural import traversal so recursive source types remain finite
and repeated references inside that one occurrence preserve recursion. That
cache is not a global `TypeId -> lambda-solved TypeId` memo table, and it must
not cause unrelated occurrences with equal source type to share callable slots.

For example, two unrelated local values of source type `I64 -> I64` start with
two callable variables. If one value later flows to the other through `let`,
branch join, parameter passing, capture, return, or another explicit value-flow
edge, representation solving unifies those callable variables. If no such edge
exists, the two callable variables may solve to different callable sets even
though their source function types are textually identical.

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
ProcedureCallableRef -> capture slots with capture types
```

Unification of two callable sets is exact:

1. Different procedure members union into one finite set.
2. The same procedure member must have the same capture slots.
3. The same capture slot unifies its capture type pointwise.
4. Mismatched capture slots for the same procedure member are compiler invariant
   violations.
5. A callable set unified with erased callable representation becomes erased.

The "same procedure member" comparison is specialization-local. It compares
callable member instances after mono specialization, lifting, clone-instantiation,
and capture-slot instantiation. The comparable identity is conceptually:

```zig
const CallableMemberInstanceId = struct {
    proc_base: ProcBaseKeyRef,
    mono_specialization: MonoSpecializationKey,
    lambda_solved_instance: ProcRepresentationInstanceId,
};
```

The exact Zig field names may differ, but the scope must not. A capture-slot
mismatch is an invariant violation only when both callable-set entries refer to
the same `CallableMemberInstanceId` in the same specialization-local
lambda-solved type store. The same source procedure may appear in different mono
or executable specializations with different instantiated capture types; those
are different callable member instances, not a global capture-slot conflict.

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

Generalization is environment-sensitive.

After solving a non-recursive procedure template, lambda-solved MIR computes the
set of type variables, callable variables, and representation variables reachable
from the current outer environment. The outer environment is the already-bound
procedure/template environment outside the procedure being generalized. It does
not include the current procedure's own parameters, capture slots, return slot,
or body-local values. Variables reachable from the current procedure template
but not reachable from the outer environment may be generalized.

After solving a recursive procedure SCC, lambda-solved MIR generalizes the SCC
as one template group. The outer environment is the already-bound environment
outside the SCC. SCC member procedure types, member capture slots, member body
locals, and member-to-member callable references are inside the group being
generalized. Generalization must preserve sharing among the SCC members before
marking variables generalized, so a callable variable shared by two members
remains one template variable after generalization.

This rule is the lambda-solved equivalent of Hindley-Milner let-generalization:
generalize only variables owned by the newly solved template, never variables
owned by already-bound outer entries. It applies to ordinary type variables,
callable variables, and representation variables together. A callable slot in a
captured function parameter may be generalized when that captured value is an
input to the current template; a callable slot already reachable from an outer
bound definition must not be generalized by the current template.

At every executable specialization point for `call_proc`, `proc_value`, or
`call_value`, lambda-solved MIR must:

1. Clone-instantiate the generalized procedure or callable template into a
   specialization-local lambda-solved type store.
2. Allocate fresh type, callable, and representation variables for every
   generalized template variable.
3. Instantiate the capture slot table exactly once for the specialization.
4. Unify the instantiated template with the requested fixed-arity function type
   and the explicit value-flow roots for that occurrence.
5. Solve all reachable callable and representation variables before publishing
   any executable MIR input.

No executable specialization may consume a generalized template directly. If a
debug verifier sees a generalized variable in an executable input, the compiler
must assert immediately in debug builds; release builds use `unreachable`.

Representation solving is also specialization-local.

Lambda-solved MIR must not use `TypeId` as representation identity.

Generalized procedures store representation templates, not executable
representation results.

Conceptual shape:

```zig
const ProcRepresentationTemplate = struct {
    template: ProcedureTemplateRef,
    type_template_store: TypeStoreRef,
    representation_template: RepresentationTemplateId,
    value_template: ValueInfoTemplateId,
    capture_slot_templates: Span(CaptureSlotTemplate),
};

const RepresentationSolveSessionId = enum(u32) { _ };
const ValueInfoStoreId = enum(u32) { _ };
const ProcRepresentationInstanceId = enum(u32) { _ };

const ProcPublicValueRoots = struct {
    params: Span(ValueInfoId),
    ret: ValueInfoId,
    captures: Span(ValueInfoId),
    function_root: RepRootId,
};

const RepresentationSolveSession = struct {
    members: Span(ProcRepresentationInstanceId),
    representation_store: RepresentationStore,
    state: RepresentationSolveState,
};

const RepresentationSolveState = enum {
    reserved,
    building,
    solving,
    sealed,
};

const ProcRepresentationInstance = struct {
    proc: MonoSpecializedProcRef,
    executable_specialization_key: ExecutableSpecializationKey,
    type_store: TypeStoreRef,
    solve_session: RepresentationSolveSessionId,
    value_store: ValueInfoStoreId,
    public_roots: ProcPublicValueRoots,
    capture_slot_instances: Span(CaptureSlotInstance),
};
```

The exact Zig names may differ, but the boundary must not. A generalized
procedure template may contain generalized type variables and template
representation variables plus template value metadata. It is not consumed
directly by executable MIR. Before executable MIR consumes a procedure,
callable value, `call_proc`, or `call_value`, lambda-solved MIR must
clone-instantiate the template into a specialization-local type store,
instantiate the capture slot table once, attach the instance to a
`RepresentationSolveSession`, build the instance's dense `ValueInfoStore`, and
fully resolve all links.

The `ProcRepresentationInstance` shape above is the sealed exported view.
Builder records may reserve an instance before `executable_specialization_key`
is available, but that key must be filled from sealed solved representation
before the instance can be exported or consumed by executable MIR.

The `RepresentationSolveSession` owns the `RepresentationStore`. A
`ProcRepresentationInstance` owns its dense `ValueInfoStore` and stores roots
into the session's representation store. A non-recursive specialization uses a
single-member solve session. A recursive specialization SCC uses one shared
solve session for all member procedures. The final exported data may be stored
physically however the implementation prefers, but the semantic model is one
shared representation graph per specialization SCC, not one isolated graph per
procedure body when the procedures can recursively reference each other.

#### SCC-Scoped Reservation, Fill, Solve, And Seal

Lambda-solved MIR must build recursive callable graphs by reservation, not by
finishing one body and hoping later recursive references can be repaired.

Every specialization instance has an internal lifecycle:

```zig
const ProcRepresentationBuildState = enum {
    reserved,
    building_body,
    body_built,
    solving_scc,
    sealed,
};

const ValueInfoBuildState = enum {
    reserved,
    structural_filled,
    solved,
    sealed,
};
```

These states are builder-only. Exported lambda-solved MIR contains only sealed
procedure representation instances, sealed solve sessions, sealed value stores,
and solved representation classes. If executable MIR sees any unsealed state,
debug verification must assert immediately; release builds use `unreachable`.

`reserve_specialization(key)` must allocate or return all public identity needed
to refer to the specialization before its body is lowered:

- output source/MIR `ProcedureValueRef` and procedure-instance id
- `ProcRepresentationInstanceId`
- `RepresentationSolveSessionId`, initially a provisional builder handle that
  is replaced or merged into the final SCC solve session after dependency SCC
  detection
- dense `ValueInfoStoreId`
- public parameter `ValueInfoId` roots
- public return `ValueInfoId` root
- public capture-slot `ValueInfoId` roots
- whole-function `RepRootId`
- capture slot instances
- canonical dependency-node identity for SCC construction

Re-entering a reserved or building specialization returns the already-reserved
instance and records a dependency edge. It must not allocate a second procedure
symbol, second value store, second capture-shape key, second adapter key, or
second executable specialization key from a partially solved body.

The body builder may allocate ordinary expression, binder, projection,
call-site, mutable-version, join, and loop-phi `ValueInfoId` values while walking
the body. For recursive references to another member whose body is not complete,
the builder may only reference that member's `ProcRepresentationInstanceId` and
its `ProcPublicValueRoots`. It must not inspect the target body, look up a raw
symbol in an environment, or wait for the target `ValueInfoStore` to be filled.

The lambda-solved construction algorithm is:

1. Reserve the root specialization instance and its public roots.
2. Build each newly reserved body far enough to discover its procedure-value,
   `call_proc`, `call_value`, capture, erased-adapter, and boxed-boundary
   dependencies.
3. Whenever a body references another specialization, call
   `reserve_specialization` for that dependency and record the dependency edge.
4. Continue until the reachable specialization dependency graph for the current
   root batch has no unbuilt reserved nodes.
5. Run SCC detection over the specialization dependency graph.
6. For each SCC, create exactly one final `RepresentationSolveSession`
   containing all member `ProcRepresentationInstanceId` values, replacing or
   merging any provisional builder handles. A one-member non-recursive SCC is
   still represented as a solve session.
7. Move or attach each member's representation roots, edges, and requirements to
   the SCC's shared `RepresentationStore`.
8. Solve representation and callable classes once for that session.
9. Fill every member `ValueInfoStore` from the solved session: each exported
   `ValueInfo.solved_class`, `CallableValueInfo.emission_plan`,
   `CallSiteInfo.dispatch`, boxed-boundary plan, projection result, capture-slot
   root, parameter root, and return root must be solved.
10. Seal the session and all member procedure representation instances together.
11. Publish executable specialization keys, callable-set keys, capture-shape
    keys, erased function signature keys, erased adapter keys, and
    layout-publication keys only from sealed data.

This mirrors the important Cor/LSS behavior without copying the prototype's
symbol-map representation. Cor reserves recursive type/procedure placeholders
before solving recursive bodies. Production Roc must do the same with typed
procedure-instance ids, public value roots, and dense sealed stores.

Cross-procedure value-flow inside a recursive SCC must use public roots:

```zig
const ProcPublicRootRef = struct {
    instance: ProcRepresentationInstanceId,
    value: ValueInfoId,
    rep_root: RepRootId,
};
```

For example, a recursive `call_proc` edge connects the caller's argument value
roots to the callee instance's public parameter roots, and connects the callee
public return root to the caller's call result root. A recursive `proc_value`
edge connects the occurrence's result root to the callee public whole-function
root and connects each explicit capture argument to the callee public capture
root. No edge may target a callee by source name or by scanning the callee body.

Edges that reference an imported procedure, hosted procedure, platform
procedure, or already sealed outer specialization must go through the explicit
imported/hosted/platform/sealed procedure capability records for that target.
They must not create a foreign edge into another solve session's private
builder state. If a value-flow edge cannot be represented through public roots
or an explicit capability, lambda-solved construction has violated a compiler
invariant.

Executable specialization keys, callable-set keys, capture-shape keys, erased
function signature keys, and layout-publication keys must be canonical
structural keys computed from sealed instantiated representation. They must
never contain raw type-store ids, generalized template ids, expression ids,
unsealed `ValueInfoId` values, or in-progress solver links.

Lambda-solved MIR owns an explicit `RepresentationStore`:

```zig
const RepRootId = union(enum) {
    expr: ExprId,
    binder: BinderId,
    pattern_binder: PatternBinderId,
    proc_param: struct { proc: ProcRepresentationInstanceId, index: u32 },
    proc_return: ProcRepresentationInstanceId,
    capture_slot: struct { proc: ProcRepresentationInstanceId, slot: CaptureSlot.Index },
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

`FunctionRepShape` is a compile-time representation-solving shape. It is not a
runtime object layout.

The argument and return children are required because calls, bridges, erased
function signatures, boxed payload transforms, specialization keys, and
higher-order value flow must know how function-typed values are used. They do
not become runtime fields of a function value. After representation solving, an
executable value whose source type is a function is represented by the solved
callable child of its `FunctionRepShape`:

```text
finite callable child -> callable-set executable value
erased callable child -> erased-fn executable value
```

There is no executable "function object" whose runtime payload contains
argument-type children, return-type children, and a callable child. Function
argument and return representation remains compile-time metadata attached to
the function representation root and to call/bridge plans. Runtime data for a
function-typed value is only the callable-set value or packed erased function
value selected by the solved callable child.

If executable type lowering sees an unresolved callable child for a function
value, that is a compiler invariant violation handled by debug-only assertion in
debug builds and `unreachable` in release builds.

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
    current_instance: ProcRepresentationInstanceId,
    solve_session: RepresentationSolveSessionId,
    representation_store: *RepresentationStore,
    value_store: *ValueInfoStore,
    current_proc: Symbol,
    current_return_root: RepVarId,
    scopes: LexicalScopeStack,
    loop_stack: Stack(LoopValueFlowFrame),
};

const LexicalScope = struct {
    bindings: Map(Symbol, BindingInfoId),
    mutable_current: Map(Symbol, MutableVersionId),
};

const LoopValueFlowFrame = struct {
    header_phis: Map(Symbol, RepVarId),
    backedge_inputs: Map(Symbol, Span(RepVarId)),
    break_exit_inputs: Map(Symbol, Span(RepVarId)),
    exit_roots: Map(Symbol, RepVarId),
};
```

The builder walks lambda-solved MIR once per specialization instance and emits
the full set of roots, edges, loop phi records, branch joins, and boxed-boundary
requirements into the current instance's solve session. It is the only place
that interprets source control flow for representation purposes. Executable MIR
may verify the exported graph in debug builds, but it must not add missing
edges.

The builder may allocate reserved `ValueInfoId` and `BindingInfoId` records
before their structural contents are complete when recursive SCC construction
requires a stable identity. It must fill those records before representation
solving and seal them before export. A reserved value metadata id is an internal
builder handle only; it is never an executable MIR input.

Lambda-solved MIR also owns an explicit value-metadata store. This store is the
long-term replacement for `exact_callable_aliases`, executable callable records,
expression-indexed semantic maps, and ad hoc lexical-environment fields such as
`EnvEntry.proc`.

This is the single value-semantic channel for later lowering stages. If a later
stage needs to know a value-level property such as callable identity, boxed
payload provenance, aggregate membership, projection slot, call-site dispatch,
capture payload, or compile-time constant origin, that property belongs in
lambda-solved value metadata and the MIR node that produced the value must carry
the corresponding ID. Do not add a parallel map, builder-environment field,
expression scan, source-name lookup, or procedure-name lookup for an individual
feature.

The implementation storage should be dense and ID-addressed: arena arrays for
metadata, compact IDs on MIR nodes, and temporary lexical maps only inside the
lambda-solved builder. Executable MIR should mostly follow IDs already present
on nodes. This gives one uniform mechanism for all value-sensitive lowering
without keeping hash maps keyed by source expressions in the hot executable
lowering path.

The value-metadata store is not a compatibility side table. Every lambda-solved
expression, binder, pattern binder, mutable version, capture slot, projection,
call result, and procedure-value occurrence must carry or reference its
`ValueInfoId` directly in the exported MIR. Later stages may follow those IDs;
they must not recover equivalent information from expression syntax, source
definitions, environment lookup, procedure names, or type shapes.

Conceptual shape:

```zig
const ValueInfoId = enum(u32) { _ };
const BindingInfoId = enum(u32) { _ };
const ProjectionInfoId = enum(u32) { _ };
const CallSiteInfoId = enum(u32) { _ };

const ValueInfoStore = struct {
    values: Store(ValueInfo),
    bindings: Store(BindingInfo),
    projections: Store(ProjectionInfo),
    call_sites: Store(CallSiteInfo),
};

const ValueInfoBuildRecord = struct {
    state: ValueInfoBuildState,
    value: ValueInfo,
};

const ValueInfo = struct {
    logical_ty: TypeId,
    rep_root: RepRootId,
    solved_class: RepClassId,
    origin: ValueOrigin,
    callable: ?CallableValueInfo,
    boxed: ?BoxedValueInfo,
    aggregate: ?AggregateValueInfo,
};

const BindingInfo = struct {
    symbol: Symbol,
    value: ValueInfoId,
    version: ?MutableVersionId,
    scope_depth: u32,
};

const ValueOrigin = union(enum) {
    expression: ExprId,
    binder: BinderId,
    pattern_binder: PatternBinderId,
    mutable_version: MutableVersionId,
    proc_param: struct { proc: ProcRepresentationInstanceId, index: u32 },
    proc_return: ProcRepresentationInstanceId,
    capture_slot: struct { proc: ProcRepresentationInstanceId, slot: CaptureSlot.Index },
    projection: ProjectionInfoId,
    call_result: CallSiteInfoId,
    compile_time_const: ConstRef,
    private_capture: PrivateCaptureRef,
};

const CallableValueInfo = struct {
    whole_function_root: RepVarId,
    callable_root: RepVarId,
    source: CallableValueSource,
    emission_plan: CallableValueEmissionPlanId,
    construction_plan: ?CallableSetConstructionPlanId,
};

const CallableValueSource = union(enum) {
    proc_value: struct {
        expr: ExprId,
        proc: ProcedureValueRef,
        captures: Span(ValueInfoId),
        fn_ty: TypeId,
    },
    finite_set: CanonicalCallableSetKey,
    already_erased: AlreadyErasedCallablePlan,
    erased_adapter: ErasedAdapterKey,
};

const AlreadyErasedCapturePlan = union(enum) {
    // There is no hidden capture value. The ErasedFnSigKey must also have
    // capture_ty = null.
    none,

    // The hidden capture exists in the erased signature, but its runtime value
    // is zero-sized. The type is still explicit because executable MIR must
    // lower a concrete executable TypeId; it must not try to reconstruct that
    // type from the canonical signature key.
    zero_sized_ty: TypeId,

    // The hidden capture is an explicit lambda-solved value occurrence whose
    // executable representation is the hidden capture type.
    value: ValueInfoId,
};

const AlreadyErasedCallablePlan = struct {
    sig_key: ErasedFnSigKey,
    capture_shape_key: CaptureShapeKey,
    capture: AlreadyErasedCapturePlan,
    provenance: Span(BoxBoundaryId),
};

const CallableSetConstructionPlanId = enum(u32) { _ };

const CallableSetConstructionPlan = struct {
    result: ValueInfoId,
    source_fn_ty: CanonicalTypeKey,
    callable_set_key: CanonicalCallableSetKey,
    selected_member: CallableSetMemberId,
    capture_values: Span(ValueInfoId),
};

const BoxedValueInfo = struct {
    box_root: RepVarId,
    payload_root: RepVarId,
    boundary: ?BoxBoundaryId,
};

const AggregateValueInfo = union(enum) {
    record: Span(FieldValueInfo),
    tuple: Span(ElemValueInfo),
    tag: struct {
        union_shape: TagUnionShapeId,
        tag: TagId,
        payloads: Span(TagPayloadValueInfo),
    },
    list: struct {
        elem_root: RepVarId,
        elems: Span(ValueInfoId),
    },
};
```

The exact Zig names may differ, but the ownership rule must not. Callable
identity, capture payload identity, boxed-erased provenance, aggregate member
identity, projection identity, and call-result identity are value metadata
attached to MIR values. They are not separate lookup channels.

`ValueOrigin.private_capture` is allowed only for values inside
compiler-created promoted callable wrapper bodies. It names an artifact-private
capture graph node created during checking finalization. It is not a top-level
value, not an import, not an exported constant, and not a general replacement
for ordinary source binders. Lambda-solved MIR must treat it as an explicit
value input with known source type and representation roots, so callable leaves,
aggregate children, projections, branch joins, and bridge inputs use the same
`ValueInfo` machinery as ordinary values.

`ValueInfo.logical_ty` is the fully resolved lambda-solved logical type for the
value occurrence. `ValueInfo.rep_root` is that occurrence's representation root.
`ValueInfo.solved_class` is the solved representation class after representation
solving. Logical type equality never substitutes for `rep_root` identity.

`ValueInfo.callable` exists when the value's solved representation contains a
function callable child. It records the whole function root and the callable
child root so executable MIR can lower the occurrence without inspecting the
callee expression or the surrounding source shape. It also records the
occurrence-specific `CallableValueEmissionPlanId`; an erased plan is valid only
when its underlying plan carries non-empty `BoxBoundaryId` provenance.

`CallableValueInfo.construction_plan` is present only for a value occurrence that
constructs one selected finite callable-set member. It is not present for
branch joins, parameters, returns, variables, projections, captures, constants,
or aggregate fields that merely carry an already-constructed callable value.
Those carried values still have `CallableValueInfo` and an emission plan, but
they do not invent a selected member.

`CallableValueInfo.construction_plan` and `CallableValueInfo.emission_plan` must
agree for the same value occurrence. If `construction_plan` is present, then:

```text
construction.result == this ValueInfoId
emission_plan == finite_callable_set(construction.callable_set_key)
construction.selected_member exists in descriptor(construction.callable_set_key)
```

The construction plan is owned by that one `CallableValueInfo`. It may not be
shared with another occurrence, retargeted to another `ValueInfoId`, or used as a
generic recipe for rebuilding callable-set values later. This is the explicit
replacement for Cor/LSS's local "lower `Var fn` into a callable-set tag now"
behavior: the lambda-solved record says exactly which occurrence constructs
which selected member, and executable MIR consumes that record directly.

If a procedure-value occurrence solves to a non-erased finite callable-set
representation and executable MIR must emit that occurrence as a value, the
occurrence must have a `CallableSetConstructionPlan`. If a value merely carries
an already-constructed finite callable-set value from a parameter, variable,
projection, branch join, call result, return, capture, aggregate field, bridge,
or constant, `construction_plan` must be absent; executable MIR must use the
existing value handle. If an occurrence solves to erased callable representation,
`construction_plan` must be absent unless the occurrence is first explicitly
emitted as a finite callable-set value and then consumed by a separate erased
adapter plan at an explicit `Box(T)` boundary.

`CallableSetConstructionPlan` is the lambda-solved record consumed by executable
MIR when it must emit a finite callable-set value. `result` is the value
occurrence being constructed. `source_fn_ty` is the exact canonical fixed-arity
function type at which the procedure value occurs. `callable_set_key` is the
canonical finite callable-set representation selected by representation solving.
`selected_member` is the member inside that set. `capture_values` are the
already-solved value occurrences captured by that selected member, in canonical
`CaptureSlot.index` order. The plan must not contain executable procedure ids,
layout ids, generated symbol text, runtime capture pointers, runtime function
pointers, or backend ABI handles.

The selected descriptor member and the construction plan must agree exactly:

```text
descriptor(construction.callable_set_key)
    .member(construction.selected_member)
    .proc_value.source_fn_ty
    == construction.source_fn_ty

descriptor(construction.callable_set_key)
    .member(construction.selected_member)
    .capture_slots.len
    == construction.capture_values.len

for every i:
    descriptor(...).capture_slots[i].slot.index == i
    canonical(value_info(construction.capture_values[i]).logical_ty)
        == descriptor(...).capture_slots[i].source_ty
    executable_value_type(construction.capture_values[i])
        == descriptor(...).capture_slots[i].exec_value_ty
```

The `source_fn_ty` equality is canonical fixed-arity Roc function type equality,
not display-name equality, raw type-store-id equality, arity inference from
syntax, or executable-layout equality. This is what prevents a generic procedure
template from sharing one callable-set member instance across distinct concrete
uses such as `I64 -> I64` and `Str -> Str`.

Constructing a callable-set value is separate from reserving executable code.
Executable MIR may lower a `CallableSetConstructionPlan` to a
`callable_set_value` without immediately creating every member body. Member
executable specializations are reserved only when code emission requires them:
`callable_match`, erased adapter generation, constant materialization, promoted
wrapper emission, backend/root boundary emission, or another explicit
executable consumer. This preserves Cor/LSS's correctness for finite callable
sets without adopting its eager "specialize as soon as a procedure value is
seen" prototype behavior.

`CallableValueSource.proc_value` is occurrence-local. It names the exact
procedure value occurrence and the `ValueInfoId` values for the explicit
captures on that occurrence. It is not a global procedure summary and not an
alias map. If the same procedure value is mentioned twice, those two mentions
have distinct `ValueInfoId` records, even when they ultimately solve to the same
canonical callable-set member.

`BindingInfo` is the only thing a lexical environment may carry for an ordinary
source symbol. The environment maps `Symbol -> BindingInfoId`. It may not grow
special-purpose fields such as `proc`, `boxed`, `record_fields`, `tag_payloads`,
or `callable_target`. A variable occurrence resolves its `BindingInfoId`, emits
a representation edge from the binding's current value root to the occurrence's
own value root, and assigns the occurrence a `ValueInfoId`. The occurrence then
gets all callable, boxed, aggregate, projection, and emission information from
the value-metadata store and solved representation class.

This rule is what makes aliases correct by construction. For example:

```roc
inc : I64 -> I64
inc = |n| n + 1

main =
    f = inc
    f(41)
```

Mono MIR lowers the use of `inc` as a value to `proc_value(proc=inc, captures=[])`.
Lambda-solved MIR assigns that `proc_value` a `ValueInfoId`, connects its result
root to the whole `proc_value.fn_ty` representation root, and records a
`CallableValueInfo` whose source is that occurrence. The binder `f` gets its own
`BindingInfoId` and `ValueInfoId`, plus a value-flow edge from the
`proc_value` occurrence to the binder. The later variable occurrence `f` gets a
new expression `ValueInfoId` by following the lexical `BindingInfoId` and adding
a value-flow edge from the current binding root to the use root. Executable MIR
does not ask whether `f` aliases `inc`; it consumes the solved callable
representation and emission plan attached to the `f` occurrence.

The same rule applies through records, tuples, tags, lists, captures, branch
joins, mutable versions, procedure parameters, procedure returns, and
compile-time values. A callable value stored in `{ f: inc }`, `Ok(inc)`,
`[inc]`, a captured local, or a top-level compile-time constant is still lowered
from explicit `ValueInfoId` and representation roots. Later stages must not
recover the callable member by walking the aggregate expression or by following
source aliases.

Call expressions also have explicit value metadata:

```zig
const CallSiteInfo = struct {
    expr: ExprId,
    result: ValueInfoId,
    callee: ?ValueInfoId,
    args: Span(ValueInfoId),
    requested_fn_root: RepVarId,
    dispatch: CallDispatchInfo,
};

const CallDispatchInfo = union(enum) {
    call_proc: CallProcExecutablePlanId,
    call_value_finite: CallableMatchPlanId,
    call_value_erased: ErasedCallPlanId,
};
```

For `call_proc`, `callee` is null because the callee is a procedure target in
the MIR node, not a first-class value. For `call_value`, `callee` names the
function value occurrence. `dispatch` is computed from the solved whole-function
representation root and the solved callable child; executable MIR consumes it
directly. A singleton finite callable set still produces
`call_value_finite`/`callable_match`. The only source-level direct-call case is
`call_proc`.

Intrinsic and low-level value-flow behavior is also represented through this
same value-metadata path. A call to `Box.box` or `Box.unbox` may appear as:

- direct `call_proc` to an intrinsic wrapper procedure
- first-class `proc_value` for the intrinsic wrapper, followed by `call_value`
- a finite callable-set branch inside `callable_match`

All three cases must create `BoxBoundaryId` records from checked procedure
metadata and the solved call-site metadata, not from syntax. `ProcTarget` for
the intrinsic wrapper records the intrinsic role and checked
`LowLevelValueFlowSignature`; `CallSiteInfo.dispatch` records which branch or
direct target is being applied. If a finite callable-set call has a branch whose
member is `Box.box`, that branch's callable-match plan owns the corresponding
`BoxBoundaryId` and branch-local payload/result value metadata. If another
branch returns an existing boxed value, the ordinary branch join connects that
branch result to the same call result root. The representation solver then
merges those roots according to the explicit edges. No stage may infer a boxed
boundary merely because a result type is `Box(T)`.

Projection metadata is mandatory for aggregates:

```zig
const ProjectionInfo = struct {
    source: ValueInfoId,
    result: ValueInfoId,
    path: ValueProjectionPath,
    finalized_slot: ProjectionSlot,
};

const ProjectionSlot = union(enum) {
    record_field: RecordFieldId,
    tuple_elem: u32,
    tag_payload: TagPayloadId,
    list_elem,
    box_payload,
    nominal_backing: NominalKey,
};
```

Record field access, tuple access, tag payload access, pattern binders, and
low-level operations that project values must emit `ProjectionInfo` records.
The projection slot must use row-finalized IDs where rows are involved. It must
not use field display names, tag display names, physical layout indexes, or a
synthetic singleton tag shape.

The value-metadata store must be built at the same time as the
`RepresentationStore`, by the same `ValueFlowGraphBuilder`, in one traversal per
specialization. This is important for correctness and performance:

1. Each visited expression allocates its `ValueInfoId` and representation root
   together.
2. Each binder allocates its `BindingInfoId`, `ValueInfoId`, and representation
   root together.
3. Each variable occurrence resolves only to a `BindingInfoId` in the lexical
   scope stack, then emits a value-flow edge from the binding value root to the
   occurrence value root.
4. Each aggregate construction allocates aggregate member metadata and structural
   representation edges at the same time.
5. Each projection allocates its `ProjectionInfoId` and representation edge at
   the same time.
6. Each call allocates `CallSiteInfoId`, argument/result value metadata, whole
   requested-function representation edges, and dispatch metadata at the same
   time.
7. Representation solving fills each exported `ValueInfo.solved_class` and
   occurrence-specific callable emission plan.

For recursive specialization SCCs, "one traversal per specialization" means one
body traversal per reserved member instance before the shared SCC solve. Public
roots are allocated before body traversal; ordinary body-local metadata is
allocated during traversal; solved classes and emission plans are filled only
after the SCC solve. A member body may reference another member's public roots
before that other body is structurally filled, but it may not read the other
member's private body-local metadata.

There must not be a second pass that scans expressions to reconstruct missing
callable identity or aggregate member metadata. A debug verifier may walk the
finished MIR and recompute cheap consistency checks, but verifier success is not
an input to executable lowering.

Current Cor/LSS uses a lexical type environment during `lambdamono` lowering to
decide how `Var(proc)` becomes a callable-set tag, packed erased function, or
ordinary variable. That works in the prototype because Cor's AST keeps the
language small and curried. Production Roc must preserve the same semantic idea
but move the decision earlier: lambda-solved MIR exports the value metadata and
solved representation for the occurrence, and executable MIR consumes those
records. Executable MIR must not reimplement Cor's `Var` inspection path.

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

Specialization queues must reserve the semantic key and output procedure handle
before lowering the procedure body. Re-entering an in-progress specialization
returns the already-reserved handle and records the dependency edge; it must not
create a duplicate specialization, derive a new key from the partially-lowered
body, or fall back to expression ids. The handle may be a module-local `Symbol`
inside the live procedure store, but every exported dependency, callable-set key,
executable specialization key, and cache key must name the procedure through
`ProcedureValueRef`, `ProcBaseKeyRef`, or `ExecutableProcId`.

`ProcOrderKey` may define canonical ordering inside a specialization key, but
it must not be a semantic component of the specialization key itself.

Erasure is decided here. Erasure is permitted only for `Box(T)`.

This rule is absolute. A non-boxed value must not acquire erased callable
representation merely because it is a function, record, tuple, tag union,
nominal, or `List(T)`. A non-boxed container is never an erased boundary.

Lambda-solved MIR must preserve explicit boxed-boundary records for every erased
`Box(T)` boundary. Erasure is not a callable-only operation, but it is a
`Box(T)`-only operation.

Hosted, platform, and intrinsic ABI metadata may describe how to pass or call an
erased callable that already exists because an explicit `Box(T)` boundary
requires it. That metadata must not introduce erasure for a non-`Box(T)` source
slot. If a hosted, platform, or intrinsic API requires an erased callback, the
checked source-facing type must expose that callback through an explicit
`Box(T)` slot. There is no separate hosted erased-boundary root.

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

Every solved representation class that contains an erased callable slot must
record exactly which boxed boundaries introduced that erasure:

```zig
const ErasedCallableProvenance = struct {
    boundaries: NonEmptySpan(BoxBoundaryId),
};
```

The exact Zig shape may differ, but the semantics must not. A non-empty
provenance set proves that erasure came from one or more explicit `Box(T)`
boundaries. There is no `unknown`, `intrinsic`, `hosted`, `layout`, or
`compatibility` provenance case. If a solved erased callable slot has an empty
provenance set, or if any boundary in the set is not a checked `Box(T)`
boundary, lambda-solved MIR must hit the compiler-invariant path: debug-only
assertion in debug builds, `unreachable` in release builds.

`erased_box_payload_type(boundary)` may be called only with a `BoxBoundaryId`
from this table. It recursively walks that boundary's boxed payload type and
rewrites every reachable function slot to erased callable representation.

`require_box_erased(boundary)` may be created only from a `BoxBoundaryId`. It is
a requirement in the `RepresentationStore`, not an executable conversion.
Solving that requirement walks the boundary's `payload_root` representation
graph, marks every reachable function representation slot as erased, and attaches
that `BoxBoundaryId` to the erased callable provenance set for the solved
representation class. The walk follows only explicit representation edges already
present in the store.

There must be no helper that accepts an arbitrary type, expression, or
`RepRootId` and makes it erased. Any API that introduces erasure must take a
`BoxBoundaryId`, and debug verification must prove that the boundary's `box_ty`
is exactly `Box(payload_boundary_ty)`.

Any recursion through records, tuples, tag unions, `List(T)`, nested `Box(T)`,
function argument and return positions, or nominal backing types happens only
because those types are inside the payload of an explicit `Box(T)`. Those types
are not themselves erasure boundaries. Non-callable data is preserved
structurally.

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
        proof_terms: Span(NoCallableProofTerm),
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
and return representations as part of their explicit ABI metadata, but that
metadata is not allowed to introduce erasure. For every callable-containing slot
whose source-facing type is not inside an explicit `Box(T)` boundary, the hosted
or platform metadata must describe a finite callable-set representation. It may
describe an erased function representation only for a callable slot that checking
has already proven to be inside an explicit `Box(T)` boundary, and the metadata
must name that boundary, the erased fixed-arity signature, and the explicit
`ErasedFnAbiKey` for erased calls. If hosted or platform metadata requests erased
representation for a non-`Box(T)` source slot, checking rejects the program before
artifact publication. If executable MIR sees such a request after checking, that
is a compiler invariant violation: debug-only assertion in debug builds,
`unreachable` in release builds. Executable MIR must consume the checked
metadata; it must not infer hosted representation behavior from host symbol
names, argument layouts, or the body of user code around the hosted call.

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
finite callable set + erased callable = erased callable with the erased side's BoxBoundaryId provenance
erased callable + erased callable = erased callable with exactly matching ErasedFnSigKey and unioned BoxBoundaryId provenance
```

The finite-callable plus erased-callable case can occur only in a representation
class reached by `require_box_erased(boundary)` for an explicit `Box(T)`
boundary. If that merge appears in a class that is not reached from such a
boundary, lambda-solved MIR has introduced non-`Box(T)` erasure and must hit the
compiler-invariant path.

This merge is representation solving, not executable repair. Once the class is
solved, every function-typed value occurrence assigned to that class receives an
explicit callable emission plan:

```zig
const CallableValueEmissionPlan = union(enum) {
    finite_callable_set: CanonicalCallableSetKey,
    already_erased: AlreadyErasedCallablePlan,
    proc_value_to_erased: ProcValueErasePlan,
    finite_set_to_erased_adapter: ErasedAdapterKey,
};
```

The exact Zig shape may differ, but the contract must not. Function-typed
procedure returns, branch results, capture values, record fields, tuple elements,
tag payloads, list elements, mutable versions, and ordinary expression results
all consume a `CallableValueEmissionPlan` when their solved representation class
contains a callable child. An erased plan is valid only when it carries non-empty
`BoxBoundaryId` provenance. Executable MIR must consume this plan directly; it
must not ask whether the current syntax is inside `Box.box(...)`, compare source
and target executable shapes, recover erasedness from a physical layout, or
re-run representation solving.

`ErasedFnSigKey` equality includes the erased-call ABI shape and hidden capture
type. Two erased callables with the same erased argument and return
representations but different hidden capture types or different `ErasedFnAbiKey`
values are different erased callable representations. They must not silently
merge.

`ErasedFnSigKey.capture_ty` is a canonical executable-value type key, not an
executable `TypeId`. It is correct for equality, interning, and debug
verification, but it is not sufficient by itself to emit executable MIR. For an
`already_erased` callable, lambda-solved MIR must also publish an
`AlreadyErasedCapturePlan`. If the signature has no hidden capture type, the
capture plan must be `none`. If the signature has a hidden capture type, the
capture plan must be either `zero_sized_ty` with the exact lambda-solved type to
lower, or `value` with the exact lambda-solved value occurrence whose executable
representation is the hidden capture. Executable MIR consumes that capture plan
directly to obtain the hidden capture `TypeId` and uses `sig_key.capture_ty` only
for debug-only consistency assertions. It must not maintain a cache from
canonical type keys to executable type ids, and it must not recover a hidden
capture type by inspecting source syntax, layouts, or callable shapes.

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
    function: FunctionPayloadRepresentation,
};

const FunctionPayloadRepresentation = struct {
    args: Span(BoxPayloadRepresentationPlan),
    ret: *BoxPayloadRepresentationPlan,
    callable: CallableBoxPlan,
    erased_fn_sig_key: ErasedFnSigKey,
};

const CallableBoxPlan = union(enum) {
    already_erased: AlreadyErasedCallablePlan,
    proc_value_to_erased: ProcValueErasePlan,
    finite_set_to_erased_adapter: ErasedAdapterKey,
};

const ProcValueErasePlan = struct {
    source: ProcedureCallableRef,
    erased_fn_sig_key: ErasedFnSigKey,
    capture_shape_key: CaptureShapeKey,
    executable_specialization_key: ExecutableSpecializationKey,
    capture_slots: Span(CaptureSlot.Index),
};
```

The exact Zig shape may differ, but the semantics must not. Structural nodes in
`BoxPayloadRepresentationPlan` are expected-representation propagation plans.
They are not executable runtime conversions. Only the `callable` child of a
`function` node may cause executable MIR to pack a callable or synthesize an
erased adapter. That packing decision is carried by the solved
`CallableValueEmissionPlan` for the value occurrence being emitted. The plan must
carry non-empty `BoxBoundaryId` provenance, but the value occurrence itself does
not have to be syntactically enclosed by a `Box(T)` expression.

For example:

```roc
choose : Bool, Box(I64 -> I64) -> (I64 -> I64)
choose = |use_box, boxed|
    if use_box {
        Box.unbox(boxed)
    } else {
        |x| x + 1
    }
```

The `else` branch closure is not inside `Box.box(...)`, but the branch result
representation is erased because the `then` branch came from `Box.unbox(boxed)`.
Lambda-solved MIR must therefore attach a `finite_set_to_erased_adapter` or
`proc_value_to_erased` emission plan to the `else` branch result with provenance
pointing back to the `Box.unbox` boundary. Executable MIR must consume that
emission plan. It must not reject the branch merely because the branch syntax is
not a `Box(T)` boundary, and it must not introduce erasure unless the plan names
the boundary.

A `function` node is not a leaf. For a function payload inside an explicit
`Box(T)` boundary, lambda-solved MIR recursively transforms every fixed-arity
argument representation and the return representation, and it also rewrites the
callable representation to erased. This is the only correct shape for higher-order
boxed payloads. A value of type `Box(A -> B)` therefore carries an erased payload
function whose erased argument representation is the boxed-boundary transform of
`A`, whose erased return representation is the boxed-boundary transform of `B`,
and whose callable child is described by `CallableBoxPlan`.

`ProcValueErasePlan` is a boundary-local lowering obligation for an explicit
`proc_value` occurrence. It is not a global procedure summary and not a runtime
conversion. `source` identifies the lambda-solved procedure value occurrence
being packed: the resolved procedure handle plus the exact canonical source
function type at which that value occurs. `erased_fn_sig_key` identifies the
erased callable ABI and hidden capture type required by the boxed boundary.
`capture_shape_key` is the canonical hidden capture record shape for this erased
procedure value occurrence.
`executable_specialization_key` is the erased executable specialization that
must be reserved before the packed value is emitted. `capture_slots` is the
logical slot order that executable MIR must use to read the occurrence's
`proc_value.captures` and build the hidden capture value.

`ProcValueErasePlan` must not contain expression ids, generated symbol text,
layout ids, LIR temporaries, runtime function pointers, runtime capture pointers,
ARC placement data, backend-specific ABI handles, or a bare `Symbol` standing in
for the whole procedure value occurrence. The actual capture operands come from
the `proc_value.captures` of the value occurrence being lowered. The plan names
the required executable specialization and capture ordering; it does not
rediscover them from the target body or the surrounding expression.

For boxing, executable MIR lowers the payload expression under
`payload_boundary_ty` and consumes callable emission plans as follows:

- `proc_value -> erased` packs the explicit procedure member and explicit
  capture payloads by consuming `ProcValueErasePlan`, reserving its
  `executable_specialization_key`, and emitting an `ErasedFnValue` whose
  `sig_key` is exactly `erased_fn_sig_key`.
- `finite callable-set value -> erased` synthesizes an erased adapter procedure
  by consuming `CallableBoxPlan.finite_set_to_erased_adapter`. That plan names
  the full `ErasedAdapterKey { source_fn_ty, callable_set_key,
  erased_fn_sig_key, capture_shape_key }`, not just the callable-set key. The
  adapter captures the already-emitted finite callable-set value through an
  explicit `ExecutableValueRef` handle whose executable type is the finite
  callable-set executable type and whose `capture_shape_key` matches the adapter
  key. The adapter body dispatches with `callable_match`, including singleton
  finite sets.
- `already-erased value -> erased` passes through after verifying the erased
  function type matches exactly.
- structural payload components are already constrained by lambda-solved MIR to
  use the boxed erased representation.

The same three callable cases apply at any solved value occurrence whose
representation class is erased with `BoxBoundaryId` provenance: branch joins,
mutable join versions, returns from functions that unbox and re-expose a boxed
callable, aggregate construction, and captures. These are not additional erasure
sources. They are uses of the solved erased representation that was introduced
by the named `Box(T)` boundary.

When executable MIR consumes `ProcValueErasePlan`, the hidden capture argument is
present exactly when `erased_fn_sig_key.capture_ty` is non-null. A no-capture
procedure value has no hidden capture argument and packs `ErasedCapture.none`. A
zero-sized capture still has a non-null hidden capture type and packs
`ErasedCapture.zero_sized_typed`. A runtime capture record packs
`ErasedCapture.boxed` with the exact `capture_shape_key`. Runtime byte size,
pointer nullness, and backend behavior must not decide whether a hidden capture
argument exists.

For unboxing, executable MIR performs the low-level unbox and gives the payload
the explicit `payload_boundary_ty` representation. There is no
`opaque_to_boundary` coercion and no recovery of the original finite callable-set
shape. Later field access or call lowering must consume the erased callable
representation already present in `payload_boundary_ty`.

Executable MIR must not synthesize generic runtime traversal or conversion for
`List(T)`, records, tuples, tag unions, function argument/return slots, nominals,
or any other non-`Box(T)` container. If such a value contains functions inside a
boxed payload, the required erased representation must have been propagated by
lambda-solved MIR to the producers and uses of that boxed payload.

Executable MIR must not infer erasure from usage.

Erased adapters are first-class synthetic procedures.

```zig
ErasedAdapterKey {
    source_fn_ty: CanonicalTypeKey,
    callable_set_key: CanonicalCallableSetKey,
    erased_fn_sig_key: ErasedFnSigKey,
    capture_shape_key: CaptureShapeKey,
}
```

Executable procedure definitions must carry an explicit origin instead of a
mandatory source procedure:

```zig
const ExecutableProcOrigin = union(enum) {
    source: MirProcedureRef,
    erased_adapter: ErasedAdapterKey,
};
```

Source procedure lookup may only inspect `ExecutableProcOrigin.source`.
Erased-adapter lookup may only inspect `ExecutableProcOrigin.erased_adapter`
and must compare the full `ErasedAdapterKey`. An erased adapter must never fake
its origin by borrowing the first member procedure's `MirProcedureRef`; doing so
would create two executable procedures with the same apparent source identity
and would reintroduce an implicit side channel for adapter identity.

An erased adapter's signature is the fixed-arity erased function signature from
the boxed payload boundary. It captures exactly the finite callable-set value, or
an explicit capture record containing that value. Its body receives the erased
boundary arguments, dispatches with `callable_match`, and calls each finite
member specialization using the same erased-boundary requested function type.
`ErasedAdapterKey` is reserved before executable MIR lowers adapter bodies. It
must include the canonical source function type because the same finite
callable-set representation can be requested at different fixed-arity source
function types with different nominal/capability data. It must include the
`erased_fn_sig_key` because the same source function type and callable-set key
can be requested at different boxed erased ABIs or hidden-capture types. It must
include the `capture_shape_key` because the adapter receives a hidden capture
value whose executable shape is part of the adapter's calling convention.
Adapter interning, deduplication, and synthetic procedure identity must use all
four fields. It must not contain expression ids, side-table ids, raw type ids,
symbol freshening suffixes, LIR temporaries, ARC placement records, or
allocation-order-dependent data.

`ErasedFnSigKey` must include the canonical erased argument types, canonical
erased return type, fixed Roc arity, canonical hidden capture type, and an
`ErasedFnAbiKey`.

Executable MIR values that have already been evaluated are referenced through
explicit value handles:

```zig
const ExecutableValueRef = union(enum) {
    temp: TempId,
    local: LocalValueId,
    const_instance: ConstInstanceRef,
    private_capture: PrivateCaptureRef,
    proc_value: ProcedureCallableRef,
    capture_ref: CaptureSlot.Index,
};
```

The exact Zig names may differ, but the boundary must not. An
`ExecutableValueRef` names a value that is already available to the executable
MIR node consuming it. It is not an arbitrary source expression and it does not
authorize re-evaluation. Later executable MIR sections use this type
conceptually for call arguments, bridge inputs, aggregate assembly operands,
branch join inputs, and packed erased capture payloads.

A `private_capture` executable value is a compiler-owned value handle that can
appear only while lowering a promoted callable wrapper that still passed through
the normal MIR-family stages. It is valid for finite promoted wrappers and other
source-level promoted bodies whose private capture graph is consumed by mono,
row-finalized mono, lifted MIR, lambda-solved MIR, and executable MIR in order.
It is not valid inside a sealed erased promoted wrapper body, because that body
skips those earlier stages.

For finite/source-level promoted bodies, executable MIR materializes
`private_capture` by following already-published value metadata from earlier MIR
stages: serializable leaves become concrete `const_instance` reads, callable
leaves become already-solved finite callable-set or erased callable values, and
structural nodes become ordinary aggregate constructions or projections with
finalized row/tag ids. Executable MIR must not ask the compile-time interpreter
to run again, read a runtime closure environment, recover private captures from
source syntax, infer row/tag positions from names, or convert a source-level
`PrivateCaptureRef` into an executable erased wrapper capture.

For sealed erased promoted wrappers, checking finalization must have already
converted the private capture graph into
`ErasedCaptureExecutableMaterializationPlan`. Because this plan is stored in a
checked artifact and can be imported into later lowering runs, its structural
nodes store stable canonical labels and logical payload indexes, not
row-finalized ids from the run that produced it. Executable MIR consumes the
materialization together with the expected executable type payload for the hidden
capture, lowers that expected type to this run's row-finalized ids, and then
assembles ordinary executable aggregate nodes with those ids. Any mismatch is a
compiler bug: debug assertion, release `unreachable`.

Packed erased function values have one ABI shape:

```zig
const ErasedFnValue = struct {
    code_ptr: ErasedCodePtr,
    sig_key: ErasedFnSigKey,
    capture: ErasedCapture,
};

const ErasedCapture = union(enum) {
    none,
    zero_sized_typed: ErasedCaptureTypeKey,
    boxed: ErasedCaptureBoxRef,
};

const ErasedCaptureBoxRef = struct {
    value: ExecutableValueRef,
    capture_type: ErasedCaptureTypeKey,
    layout: LayoutId,
    size: u32,
};
```

The exact Zig names may differ, but the representation responsibilities must
not. `code_ptr` names code whose erased ABI is exactly `sig_key`. `capture`
records whether the erased function has no hidden capture argument, a typed
zero-sized hidden capture argument, or a runtime boxed capture payload. Runtime
boxed capture payloads carry the value handle, erased capture type key, physical
layout id, and byte size explicitly.

The erased code signature is keyed by `ErasedFnSigKey`:

```zig
const ErasedFnSigKey = struct {
    fixed_arity: u32,
    args: Span(CanonicalExecValueTypeKey),
    ret: CanonicalExecValueTypeKey,
    capture_ty: ?ErasedCaptureTypeKey,
    abi: ErasedFnAbiKey,
};
```

- `args` are the fixed-arity Roc source arguments after erased-boundary
  representation rewriting.
- `ret` is the erased-boundary return representation.
- `capture_ty` is null when the callable has no capture argument.
- `capture_ty` is non-null when erased code expects an explicit hidden capture
  argument.
- the hidden capture argument, when present, is appended after all fixed-arity
  Roc source arguments.

No-capture erased functions have `capture_ty = null`, no hidden capture
argument, and `capture = none`.

Zero-sized captures still have explicit capture typing when the erased code
expects a capture argument. Their `ErasedFnSigKey` records the non-null capture
type, and `ErasedFnValue.capture` must be `zero_sized_typed` when no runtime
boxed payload is needed. A zero-sized capture is not the same ABI as no capture.
If the concrete ABI carries zero-sized arguments as no bytes, hidden capture
argument emission is still driven by `ErasedFnSigKey.capture_ty`, not by runtime
pointer nullness, runtime byte size, or backend behavior.

`call_erased` consumes the `ErasedFnSigKey` attached to the erased function
value. It must not infer hidden capture arguments, capture layout, or erased
argument shape from the runtime packed value, the target procedure body, the
physical layout store, pointer nullness, or runtime capture size.

`ErasedFnSigKey.capture_ty == null` requires `ErasedFnValue.capture = none`.
`ErasedFnSigKey.capture_ty != null` requires `ErasedFnValue.capture` to be
either `zero_sized_typed` with the exact capture type key or `boxed` with the
same capture type key. A mismatch is a compiler invariant violation handled by
debug-only assertion in debug builds and `unreachable` in release builds.

`ErasedFnAbiKey` interns an explicit erased-call ABI shape:

```zig
ErasedFnAbi {
    kind: ErasedFnAbiKind,
    fixed_arity: u32,
    packed_function_arg: ErasedPackedFunctionArgAbi,
    arg_abis: Span(ErasedValueAbi),
    result_abi: ErasedResultAbi,
    capture_arg: ?ErasedCaptureArgAbi,
    hosted_owner: ?HostedAbiKey,
}
```

The ABI shape describes the erased boundary's required calling convention. It
says how the packed erased function value is passed, how each fixed-arity Roc
argument is represented at the boundary, how the result is materialized, and
whether an explicit hidden capture argument exists. `arg_abis.len` must equal
`fixed_arity`. `capture_arg` must agree exactly with
`ErasedFnSigKey.capture_ty`.

`hosted_owner` is set only for hosted, platform, or intrinsic ABI shapes whose
ABI is defined outside ordinary Roc boxed-erased calling.

The ABI shape must not contain expression ids, mutable type-store ids, source
syntax pointers, runtime function pointers, capture-layout pointers, LIR
temporaries, ARC placement records, or backend-specific layout handles. Those
belong to executable MIR, LIR, or backend lowering, not to erased callable
identity.

That ABI shape is part of key identity. Exact `ErasedFnSigKey` equality means:

```text
same fixed Roc arity
same canonical erased argument representations
same canonical erased return representation
same canonical hidden capture type, including null vs non-null
same ErasedFnAbiKey
```

Same erased arguments and same erased return with a different hidden capture
type or a different `ErasedFnAbiKey` is not the same erased callable
representation. The compiler must either emit an explicit adapter or bridge at a
boundary that names both shapes. If no explicit adapter or bridge path exists
after checking, that is a compiler invariant violation. A later stage must not
repair the mismatch by inspecting an adapter body, capture layout, runtime
function pointer, hosted symbol, or ARC placement result.

`ErasedFnAbiKey` is produced before executable MIR. It is the erased-call ABI
shape for the boundary, not a body-derived summary for a particular procedure.

For ordinary Roc boxed erased callables, the canonical ABI shape is:

```text
packed function value is passed as an ordinary refcounted value
each fixed-arity erased argument is passed as an ordinary Roc value
result is returned as an ordinary Roc value
optional hidden capture argument is present exactly when `capture_ty` is non-null
```

Hosted, platform, and intrinsic erased-call ABI shapes may use a different
`ErasedFnAbiKey`, but only when that shape is explicit hosted, platform, or
intrinsic ABI metadata.

`ErasedAdapterKey` and boxed payload plans are reserved before executable MIR
emits adapter bodies. `ErasedFnSigKey` must not contain procedure body summaries,
expression ids, side-table ids, LIR temporaries, or runtime function pointers. A
later LIR or backend stage must not recover erased-call ABI shape from the
adapter body, capture layout, host symbol, runtime function pointer, or ARC
placement.

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

Finite callable-set value construction is explicit executable MIR, not an
environment lookup and not a delayed syntax interpretation.

Concrete shape:

```zig
const CallableSetValue = struct {
    id: CallableSetValueId,
    callable_set_key: CanonicalCallableSetKey,
    member: CallableSetMemberRef,
    capture_record: ?CallableCaptureRecord,
    result_ty: ExecTypeId,
    result_tmp: TempId,
};

const CallableSetMemberRef = struct {
    callable_set_key: CanonicalCallableSetKey,
    member_index: u32,
};

const CallableCaptureRecord = struct {
    capture_shape_key: CaptureShapeKey,
    values: Span(CaptureValueRef),
    record_tmp: TempId,
};

const CaptureValueRef = struct {
    slot: CaptureSlot.Index,
    value: ExecutableValueRef,
    exec_ty: ExecTypeId,
};
```

The exact Zig names may differ, but the representation responsibilities must
not. A non-erased value occurrence with a
`CallableSetConstructionPlan` lowers to `CallableSetValue`. The member is
identified by `callable_set_key + member_index`; that pair derives the
`ProcedureCallableRef`, member tag/discriminant order, capture-slot schema, and
capture shape. An implementation may cache those derived values beside the
member, but only as debug-verified accelerators.

Executable MIR must validate the lambda-solved consistency unit before emitting
the value. For a value occurrence `v`, the executable builder must see:

```text
value_info(v).callable.construction_plan == construction_id
construction.result == v
value_info(v).callable.emission_plan
    == finite_callable_set(construction.callable_set_key)
descriptor(construction.callable_set_key)
    contains construction.selected_member
descriptor member proc_value.source_fn_ty == construction.source_fn_ty
```

These checks are compiler invariants. Debug builds assert immediately; release
builds use `unreachable`. Executable MIR must not repair a missing or mismatched
construction plan by inspecting the expression, consulting checked CIR, looking
up the current lexical symbol, or comparing executable shapes.

Executable MIR constructs a `CallableSetValue` by consuming the lambda-solved
`CallableSetConstructionPlan` for that exact occurrence:

1. Resolve each `construction_plan.capture_values[i]` to an
   `ExecutableValueRef` exactly once.
2. Resolve captures in canonical `CaptureSlot.index` order.
3. Build at most one `CallableCaptureRecord` for the selected member.
4. Store that capture record as the member payload.
5. Use the resulting callable-set value handle for later storage, return,
   aggregate construction, bridge input, `callable_match`, or erased packing.

A member with no captures has no payload. A member with captures assembles one
capture record in `CaptureSlot.index` order and stores that record as the member
payload. Runtime byte size is not the source of truth for payload presence; a
zero-sized capture still follows the explicit capture-slot metadata.

The capture operands must match the selected descriptor member exactly. The
number of operands must equal the number of `CallableSetCaptureSlot` entries,
the slot indexes must be dense and canonical, and each operand's executable
value type must equal the slot's `exec_value_ty` after canonical executable type
lowering. A mismatch is a compiler invariant violation, not a cue to reorder
captures, look up names, or synthesize a different capture record.

Capture operands are evaluated when the callable-set value is constructed, not
when it is later called. `callable_match` destructures the stored member payload;
it must not rebuild captures, re-read source variables, replay field accesses, or
re-evaluate expressions that produced the captured values.

Callable-set values produced by `match`, `if`, blocks, loops, constants, or
bridges join through the same executable result-join rules as any other value.
They must not become packed erased functions unless lambda-solved MIR explicitly
requires an erased `Box(T)` boundary.

This conversion must use only lambda-solved MIR metadata and executable MIR's own
specialization queue. It must not inspect checked CIR, method registries, source
syntax, or expression-derived records.

Executable MIR consumes `ValueInfoId`, `BindingInfoId`, `ProjectionInfoId`, and
`CallSiteInfoId` values from lambda-solved MIR. It may maintain a lexical
runtime environment while lowering, but that environment maps source symbols to
already-exported `BindingInfoId`/executable value handles. It must not contain
special-purpose semantic fields such as `proc`, `callable_target`,
`boxed_payload`, `record_fields`, or `tag_payloads`.

When executable MIR lowers a variable occurrence, it follows the occurrence's
exported `ValueInfoId`. If the value is callable, it consumes
`ValueInfo.callable` and the solved `CallableValueEmissionPlan`. If the value is
boxed, aggregate-shaped, or a projection, it consumes the corresponding
`BoxedValueInfo`, `AggregateValueInfo`, or `ProjectionInfo`. The variable name
itself is only a lexical handle; it is never evidence that the value is a
procedure, a boxed payload, a record field, or a tag payload.

When executable MIR lowers `call_value`, it consumes the lambda-solved
`CallSiteInfo.dispatch`:

- `call_value_finite` lowers to `callable_match`, including singleton finite
  callable sets.
- `call_value_erased` lowers to `call_erased` using the exact
  `ErasedFnSigKey` and callable emission plan.
- direct procedure calls are represented only by `call_proc`; executable MIR
  must not turn a `call_value` into direct singleton dispatch by inspecting the
  callee expression.

When an intrinsic wrapper such as `Box.box` or `Box.unbox` is used as a
first-class function, executable MIR still receives the intrinsic role through
the selected callable member's `ProcTarget` metadata and the call site's
dispatch plan. Member-specific `BoxBoundaryId` records are explicit
lambda-solved data. Executable MIR must not decide that a first-class call is a
box boundary from the callee's name, from a result type of `Box(T)`, or from the
source expression shape.

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

Executable MIR must represent `call_proc` lowering with an explicit executable
plan before it emits `call_direct`:

```zig
const CallProcExecutablePlan = struct {
    source: ProcedureCallableRef,
    representation_root: RepClassId,
    executable_specialization_key: ExecutableSpecializationKey,
    executable_proc: ExecutableProcId,
    arg_transforms: Span(ExecutableValueTransformId),
    result_transform: ExecutableValueTransformId,
    result_ty: ExecTypeId,
};
```

The exact Zig names may differ, but the plan must record the same decisions.
`source` is the source/MIR procedure target from lambda-solved MIR plus the exact
canonical fixed-arity function type requested by this call.
`representation_root` is the solved whole-function representation class, not
only the callable child. `executable_specialization_key` is the canonical key
used to reserve `executable_proc`. `executable_proc` is an executable-MIR
procedure id allocated by that reservation, not a generated symbol used as a
semantic key. `arg_transforms` and `result_transform` are explicit value
transform obligations between the caller's executable values and the target
specialization signature. They use the same operation family as artifact-owned
`ExecutablePayloadTransformPlan` records, but may be stored in the current
executable lowering run when the call is not a promoted-wrapper artifact
boundary.

A `call_proc` target identity never authorizes executable MIR to skip erased
callable representation inside an explicit `Box(T)` payload. Direct target
identity answers which procedure is called. It does not answer which
representation every argument, result, capture, or boxed payload must use. Those
come from the specialization-local lambda-solved representation store and the
explicit value-transform plan above.

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
    requested_source_fn_ty: CanonicalTypeKey,
    func_expr: ExprId,
    arg_exprs: Span(ExprId),
    func_tmp: TempId,
    arg_temps: Span(TempId),
    branches: Span(CallableBranch),
    result_ty: ExecTypeId,
    result_tmp: TempId,
}
```

`requested_source_fn_ty` is the exact canonical fixed-arity source function type
from the original `call_value.requested_fn_ty`. `func_expr` and `arg_exprs` are
the expressions evaluated exactly once by the node. `func_tmp` and `arg_temps`
are the temporaries produced by those evaluations and consumed by every branch.
`arg_temps.len` must equal the fixed Roc arity of
`requested_source_fn_ty`. No branch may re-evaluate `func_expr` or any original
argument expression. `result_ty` is the executable result type required by the
original call expression. `result_tmp` is the single join value produced by the
whole `callable_match`.

Every `callable_match` branch must store:

```zig
CallableBranch {
    member: CallableSetMemberRef,
    capture_payload: ?TempId,
    executable_specialization_key: ExecutableSpecializationKey,
    executable_proc: ExecutableProcId,
    direct_args: Span(ExecutableValueRef),
    direct_call_result: TempId,
    bridge_to_result: ?BridgeId,
}
```

`member` identifies the lambda-solved callable-set member through the canonical
callable-set key and member index. The member's procedure value occurrence,
capture-slot schema, tag/discriminant order, and capture shape are derived from
that pair. `executable_specialization_key` is the canonical key for the reserved
member executable specialization. `executable_proc` is the executable procedure
allocated for that key and used by the branch body. These values are related by
the executable specialization queue, not by name lookup, generated symbol text,
or environment lookup.

For every branch, the descriptor member's concrete procedure value type must
match the call's requested function type exactly:

```text
descriptor(callable_match.callable_set_key)
    .member(branch.member)
    .proc_value.source_fn_ty
    == callable_match.requested_source_fn_ty

branch.executable_specialization_key.requested_fn_ty
    == callable_match.requested_source_fn_ty
```

This equality is checked after canonical type normalization. It is not enough
for the member to come from the same source procedure template, have the same
display name, have the same argument count, or lower to an executable procedure
whose layout happens to be compatible. This is the finite-callable-set analogue
of Cor/LSS's specialization key `(source function, requested concrete function
type)`, expressed without raw symbols.

The executable calling convention for a callable-set member is:

```text
all fixed-arity source call arguments in source order
+ optional trailing capture-record argument when the member has captures
```

`direct_args` is the exact argument list supplied to the branch `call_direct`.
It must be `ExecutableValueRef.temp` handles for the original argument
temporaries, in source call order, followed by the destructured capture payload
temporary when `capture_payload` is present. It must not contain source
expressions, nested calls, field projections, box operations, or anything that
could evaluate again inside the branch.
`direct_call_result` is branch-local. If its executable type is not exactly
`result_ty`, `bridge_to_result` names the explicit branch-local bridge into the
single `result_tmp`. No branch-local layout or branch-local return type may
escape the `callable_match` node. `no_return` branches do not constrain
returning branches.

Every returning branch must assign exactly one value to the shared
`result_tmp`: either the `direct_call_result` itself when its type is already
`result_ty`, or the result of `bridge_to_result`. Branch lowering may not add,
remove, duplicate, or reorder direct-call arguments.

`callable_match` is not a curried-call loop. It dispatches one fixed-arity Roc
call. Every branch must call a member specialization whose source-argument arity
matches the original `call_value.requested_fn_ty`.

No branch may emit a `call_direct` to a procedure that has not been reserved or
created by executable MIR. By the end of executable MIR, every `executable_proc`
referenced by a `callable_match` branch must have a procedure definition in the
executable MIR program, and its definition key must equal the branch's
`executable_specialization_key`.

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

Ordinary source `match` expressions must also have a concrete executable MIR
node. They are not callable-set dispatch nodes, and callable-set
`callable_match` nodes are not ordinary source `match` nodes.

Ordinary source `match` lowering uses a checked pattern-decision plan. This is
the production equivalent of the solid part of the old Rust compiler's
`crates/compiler/mono/src/ir/decision_tree.rs` and
`crates/compiler/mono/src/ir/pattern.rs`: checked patterns are flattened into
path-specific tests, tests are compiled into an ordered decision tree, guards
preserve fallback behavior, and branch bodies are joined through one result.

The old Rust compiler is only a reference for the decision model. Production
MIR must not copy its post-check error shape, curried-call assumptions, runtime
global thunking, or late layout recovery.

Conceptual shape:

```zig
const SourceMatch = struct {
    id: SourceMatchId,
    scrutinees: Span(MatchScrutinee),
    decision_plan: PatternDecisionPlanId,
    branches: Span(SourceMatchBranch),
    result_ty: ExecTypeId,
    result_tmp: TempId,
};

const MatchScrutinee = struct {
    expr: ExprId,
    tmp: TempId,
    exec_ty: ExecTypeId,
};

const SourceMatchBranch = struct {
    source_branch: CheckedBranchId,
    alternatives: Span(SourceMatchAlternative),
    materialized_paths: Span(MaterializedPatternPathValue),
    bindings: Span(PatternBinding),
    guard: ?GuardPlanId,
    body: ExprId,
    branch_result: TempId,
    bridge_to_result: ?BridgeId,
};

const SourceMatchAlternative = struct {
    source_branch: CheckedBranchId,
    source_branch_pattern: CheckedMatchBranchPatternId,
    root_pattern: PatId,
    degenerate: bool,
    binder_remaps: Span(AlternativeBinderRemap),
};

const AlternativeBinderRemap = struct {
    candidate_binder: PatternBinderId,
    representative_binder: PatternBinderId,
};

const PatternPathValuePlanId = distinct u32;
const RecordRestProjectionId = distinct u32;

const PatternPathValuePlan = struct {
    id: PatternPathValuePlanId,
    path: PatternPath,
    source: PatternPathValueSource,
    exec_ty: ExecTypeId,
};

const MaterializedPatternPathValue = struct {
    plan: PatternPathValuePlanId,
    temp: TempId,
};

const PatternPathValueSource = union(enum) {
    scrutinee: MatchScrutineeId,
    tag_payload_record: struct {
        parent: PatternPathValuePlanId,
        tag: TagId,
    },
    tag_payload_field: struct {
        parent_payload_record: PatternPathValuePlanId,
        payload: TagPayloadId,
    },
    record_field: struct {
        parent: PatternPathValuePlanId,
        field: RecordFieldId,
    },
    record_rest: RecordRestProjectionId,
    tuple_field: struct {
        parent: PatternPathValuePlanId,
        field: TupleFieldId,
    },
    list_element: struct {
        parent: PatternPathValuePlanId,
        probe: ListElementProbeId,
    },
    list_rest: struct {
        parent: PatternPathValuePlanId,
        probe: ListRestProbeId,
    },
    opaque_payload: struct {
        parent: PatternPathValuePlanId,
        payload: OpaquePayloadId,
    },
    newtype_payload: struct {
        parent: PatternPathValuePlanId,
        payload: NewtypePayloadId,
    },
};

const RecordRestProjection = struct {
    id: RecordRestProjectionId,
    parent: PatternPathValuePlanId,
    source_shape: RecordShapeId,
    result_shape: RecordShapeId,
    projected_fields: Span(RecordRestProjectedField),
};

const RecordRestProjectedField = struct {
    source_field: RecordFieldId,
    result_field: RecordFieldId,
    exec_ty: ExecTypeId,
    result_logical_index: u32,
};

const PatternBinding = struct {
    binder: LocalValueId,
    source: PatternPathValuePlanId,
    exec_ty: ExecTypeId,
    temp: TempId,
};
```

Every scrutinee expression is evaluated exactly once, in source order, into its
`MatchScrutinee.tmp`. The decision plan reads only those temporaries and
explicit `PatternPathValuePlan` records derived from them. A path plan is a
recipe keyed by finalized path ids and explicit parent path plans; it is not a
global temporary. Decision nodes and selected branches materialize the path plans
they need into control-flow-local `MaterializedPatternPathValue` temps. Branch
bodies consume pattern-binding temporaries produced by the selected branch; they
must not re-evaluate scrutinees or rediscover payloads from source syntax.

The decision plan is explicit data:

```zig
const PatternDecisionPlan = struct {
    id: PatternDecisionPlanId,
    scrutinees: Span(PatternScrutineeId),
    path_value_plans: Span(PatternPathValuePlan),
    root: DecisionNodeId,
    leaves: Span(DecisionLeaf),
};

const DecisionNode = union(enum) {
    leaf: DecisionLeafId,
    test: DecisionTestNode,
};

const DecisionTestNode = struct {
    path_value: PatternPathValuePlanId,
    edges: Span(DecisionEdge),
    default: ?DecisionNodeId,
};

const DecisionEdge = struct {
    test: PatternTest,
    next: DecisionNodeId,
};

const PatternPath = struct {
    scrutinee: PatternScrutineeId,
    steps: Span(PatternPathStep),
};

const PatternPathStep = union(enum) {
    tag_payload: TagPayloadId,
    record_field: RecordFieldId,
    record_rest: RecordRestProjectionId,
    tuple_field: TupleFieldId,
    list_index: ListElementProbeId,
    list_rest: ListRestProbeId,
    opaque_payload: OpaquePayloadId,
    newtype_payload: NewtypePayloadId,
};

const PatternTest = union(enum) {
    tag: TagId,
    bool_literal: bool,
    byte_union_tag: TagId,
    int_literal: IntPatternLiteralId,
    float_literal: FloatPatternLiteralId,
    decimal_literal: DecimalPatternLiteralId,
    str_literal: ProgramLiteralId,
    list_len_exact: u32,
    list_len_at_least: u32,
    guard: GuardPlanId,
};

const DecisionLeaf = struct {
    source_branch: CheckedBranchId,
    source_branch_pattern: CheckedMatchBranchPatternId,
    degenerate: bool,
    guard: ?GuardPlanId,
    fallback_after_guard: ?DecisionNodeId,
    body: ExprId,
};
```

The exact Zig names may differ, but the contract must not. The plan supports
all checked source-pattern forms: identifiers, `_`, `as`, records, tuples,
single-field newtypes, ordinary tag unions, opaque unwraps, list patterns with
head/tail/rest probes, bool/byte union tests, numeric literals, string
literals, and guards. Pattern tests are keyed by finalized row and literal ids,
not by names, source text, physical layout indexes, or syntax-derived singleton
tag shapes.

String literal tests use `ProgramLiteralId` from the lowered program literal
pool. The decision-plan builder must intern checked string-pattern bytes into
that pool while lowering the owning checked artifact. It must not compare raw
`base.StringLiteral.Idx` values, raw `CheckedStringLiteralId` values, or string
text from a `ModuleEnv`.

Decision construction follows these rules:

- The input is the checked pattern matrix plus row-finalized ids, fully resolved
  pattern types, and guard plans. User-facing exhaustiveness, redundancy, and
  invalid-pattern diagnostics have already happened before checked artifact
  publication.
- Current checked Roc `CheckedMatchBranch.patterns` are branch alternatives
  produced by source `|` patterns. They are not multiple scrutinees. Each
  alternative is an independent row in the pattern matrix with the same source
  branch body and guard. The decision plan must carry the selected
  `CheckedMatchBranchPatternId` all the way to the leaf so later stages know
  which alternative matched without inspecting source syntax. If future Roc
  syntax adds true multi-scrutinee `match`, checked artifact publication must
  expose explicit `MatchScrutinee` rows separately; it must not overload branch
  alternatives as scrutinees.
- A non-degenerate alternative whose binders have the same source names as the
  representative alternative still has distinct pattern binder ids in checked
  CIR. The branch guard and branch body refer to the representative binders that
  canonicalization introduced into the branch scope. Therefore checked artifact
  publication must store an explicit `AlternativeBinderRemap` for every
  candidate binder in every non-degenerate alternative. The remap says:

  ```text
  when this candidate alternative matches, bind this candidate pattern path to
  this representative branch binder
  ```

  The first representative alternative stores identity remaps. Later
  non-degenerate alternatives store candidate-to-representative remaps. A
  degenerate alternative stores no usable remaps because reaching it lowers to
  runtime error before guard/body evaluation.

  This remap must be produced while checked artifact publication still has the
  canonical pattern graph and source-name equality result from canonicalization
  or checking. Mono MIR, row-finalized mono MIR, lifted MIR, lambda-solved MIR,
  executable MIR, IR, and LIR must not compare identifier text, inspect pattern
  names, or try to match binders by arity to recover this mapping.
- Flatten nested patterns into `(PatternPath, PatternTest)` pairs. A path starts
  at a scrutinee and then steps through finalized payload, field, tuple,
  list-probe, opaque, or newtype ids.
- Deduplicate identical tests at the same path while preserving the first source
  order at which each test can matter.
- A default edge exists only when the tests at a path are incomplete for the
  already-checked pattern matrix.
- Guards are ordered decision tests, but their representation must make binder
  scope explicit. A guard may appear either as `PatternTest.guard` after the
  selected alternative's pattern bindings have been materialized, or as the
  equivalent `DecisionLeaf.guard` plus `fallback_after_guard` continuation. In
  both encodings, a branch whose structural tests pass but whose guard fails
  continues to the next source-compatible branch through an explicit
  `DecisionNodeId`. The guard must run with only the selected alternative's
  remapped representative binders in scope, and the fallback continuation must
  run after those branch-local bindings have been removed. This is the same
  semantic requirement that old Rust handled with `PlaceholderWithGuard`,
  `GuardedNoTest`, and `break_out_guard`, but production MIR carries it as
  explicit decision-plan data.
- A degenerate branch alternative is explicit runtime semantics, not a compiler
  invariant and not a post-check user-facing error. Canonicalization marks an
  alternative as degenerate when that alternative does not bind every symbol the
  source branch guard or body may use. For example:

  ```roc
  value =
      match input {
          A(x) | B(_) => x
      }
  ```

  Reaching `A(x)` evaluates the branch normally. Reaching `B(_)` must lower to
  the checked degenerate-alternative runtime error before evaluating the guard
  or branch body, because the branch lexical environment cannot be completed.
  The decision leaf therefore carries `degenerate = true` for that alternative
  and IR/LIR lower it to the explicit runtime-error path selected by checking.
  Later stages must not infer degeneracy by comparing binder names or scanning
  the body.
- List-length tests must preserve specificity ordering. More specific
  `list_len_at_least` tests run before less-specific ones, and exact length
  tests at the same length run before the at-least test for that length. This
  preserves cases like `[x, y, ..]` before `[x, ..]`.
- Single-tag non-nullable unions, single-field records, and newtypes may erase
  a runtime tag/projection only when row-finalized representation metadata says
  there is no runtime tag or no runtime wrapper. They must not make that choice
  from source syntax.
- A branch reached from multiple decision paths may use an explicit join point
  or equivalent branch-sharing node. Pattern bindings required by the branch are
  passed through that join explicitly.

Path extraction is once per selected path value, not once per binder and not
once per source branch. When a selected decision path needs a tag payload record,
executable MIR first materializes the `PatternPathValuePlan` for that tag payload
record from the already-materialized parent path value. Individual payload
binders are then projected from that payload record through `tag_payload_field`
path plans keyed by `TagPayloadId.payload_index`. It must not emit a separate
union-payload extraction for every binder. A zero-payload tag has no
payload-record path plan.

Record rest bindings are explicit record-assembly path values. For a pattern
like:

```roc
strip_name = |{ name: _, ..rest }| rest
```

the selected branch does not ask IR to infer "all fields except `name`." The
decision plan contains a `record_rest` path value that points at an explicit
`RecordRestProjectionId`. The projection's `parent` is the original record path,
its `source_shape` is the row-finalized input record shape, its `result_shape` is
the row-finalized rest record shape, and its `projected_fields` list gives the
exact source field id, exact result field id, field executable type, and result
logical assembly index for every field in the rest record. IR materializes the
rest value by loading that projection id, projecting those source fields from the
parent record, and assembling the result record in finalized logical order. Empty
rest records materialize as the unit/empty-record representation selected by
executable layout metadata. Post-check lowering must not compute a record-rest
complement by scanning source field names, row labels, source patterns, or type
syntax.

Branch binder extraction uses `AlternativeBinderRemap`. When an alternative is
selected, executable MIR walks the selected alternative's pattern paths and
creates `PatternBinding` records for representative binders only. The source path
for each binding is the path of the candidate binder inside the selected
alternative's root pattern; the target binder is
`AlternativeBinderRemap.representative_binder`. Candidate binder ids are never
visible to the branch guard or body after this remapping step. A missing remap
for a candidate binder in a non-degenerate alternative is a compiler invariant
violation.

This path-indexed model is required for nested patterns and multi-scrutinee
matches. One reached source branch can require several payload records at
different `PatternPath`s, for example one payload from the first scrutinee and
another payload from a nested tag inside the second scrutinee. Those values are
distinct `PatternPathValuePlan` records, each with its own `PatternPath`,
`source`, and `exec_ty`, and distinct `MaterializedPatternPathValue` temps at
the control-flow points where they are needed. A single branch-level
payload-record temporary is not expressive enough and must not exist in the final
design.

`PatternPathValuePlan` records are reusable path recipes. Materialized path
values are control-flow-local temps created only when a decision node or selected
branch needs them. Guards and pattern binders consume these already-materialized
path values. They must not ask executable MIR to reconstruct a path by scanning
source patterns, sorting row names, or re-running decision tests.

Record rest path values use the same `PatternPathValuePlan` table as record
fields, tag payloads, tuples, lists, opaque payloads, and newtype payloads. The
record-rest projection id is allocated by row-finalized mono MIR or executable
decision-plan construction while finalized source and result shapes are both
available. Later stages may verify the projection in debug builds, but they must
not derive it.

List element and rest bindings are also explicit probes. Head indexes, tail
indexes, rest start, and rest length come from the checked list-pattern arity,
not from ad hoc source parsing. If a tail index needs the list length, the
decision plan names the length probe once for that branch path. List element and
rest path values use the same `PatternPathValuePlan` table as tag payloads,
records, tuples, opaque payloads, and newtype payloads.

Optional record-field defaults are checking-time semantics. If a checked pattern
requires a default expression for a missing optional field, the decision plan
contains an explicit binding/default plan for that branch. Post-check lowering
must not rediscover optional-field behavior by scanning record names.

After type checking, branch coverage and pattern reachability are settled for
the user. Executable MIR debug verification must assert that the decision plan
and branch list exactly match the checked pattern matrix: no missing reachable
branch, no impossible branch, no payload arity mismatch, no path id from the
wrong finalized row shape, and no guard plan attached to the wrong branch.
Release builds use `unreachable` for the equivalent compiler-invariant path.

Executable MIR may canonicalize emitted IR switch arms by finalized `TagId`,
literal value, or list length where that is a representation detail. It must
not reorder scrutinee evaluation, guard evaluation, branch body evaluation, or
branch-local payload/list/field extractions relative to the selected branch.

Every returning source `match` branch must assign exactly one value to the
shared `result_tmp`: either the `branch_result` itself when its executable type
is already `result_ty`, or the result of `bridge_to_result`. Branch-local
layouts and branch-local return types must not escape the `SourceMatch` node.
`no_return` branches do not constrain returning branch result representation.

IR lowering of source `match` is mechanical lowering of `PatternDecisionPlan`.
The IR builder must consume `PatternDecisionPlan.root`, `DecisionNode`,
`DecisionEdge`, `DecisionLeaf`, `PatternPathValuePlan`,
`MaterializedPatternPathValue`, and `PatternBinding` records directly. It must
not inspect `Branch.pat`, recurse over executable pattern syntax, infer a switch
subject from branch patterns, rebuild tag or literal tests, or special-case the
single-scrutinee case. Executable MIR may keep lowered `Pat` values only for
debug printing, verifier diagnostics, or pre-IR structural tests; those pattern
syntax nodes are not semantic input to IR lowering.

IR emission must evaluate every `SourceMatch.scrutinees[i].expr` once in source
order, bind the corresponding `MatchScrutinee.tmp`, and then emit the decision
tree from `PatternDecisionPlan.root`. A `DecisionTestNode` first materializes
its named `PatternPathValuePlan` in the current control-flow block if that path
value has not already been materialized on that path. It then emits the concrete
test named by each `DecisionEdge.test`:

- tag and byte-union tests read the finalized union id for the already
  materialized path value
- bool and integer tests switch on the already materialized scalar value
- decimal and float literal tests call the checked, explicit equality operation
  selected for that literal kind before the branch test
- string literal tests compare the already interned `ProgramLiteralId` payload
  through the explicit string-equality low-level operation selected before IR
- list-length tests read the list length once for the named list path/probe and
  branch on the exact or at-least relation named by the plan
- guard tests lower the published guard expression in the guard's lexical
  environment after all structural path values it can reference have been
  materialized

When a decision leaf is reached, IR lowering must materialize exactly the
`SourceMatchBranch.materialized_paths` required by that branch, bind every
`PatternBinding.temp` from its source path value, lower the branch body, bridge
the branch result if `bridge_to_result` is present, and assign the shared match
result variable exactly once. A leaf that is reachable from several decision
paths must receive all needed materialized path values through explicit
control-flow-local temporaries or an explicit join block; it must not recompute
path extractions from the original scrutinees.

If the reached `DecisionLeaf.degenerate` flag is true, IR lowering must emit the
checked degenerate-alternative runtime-error path immediately. It must not
materialize branch binders, evaluate the branch guard, lower the branch body, or
try to synthesize missing binder values. This runtime-error path is part of the
checked artifact's published semantics.

The temporary cache used while lowering one decision path is control-flow-local.
It may be threaded through recursive IR emission as implementation state, but it
is not a side channel and is not observable after the `SourceMatch` has been
lowered. It stores only already materialized `PatternPathValuePlanId -> IR Var`
bindings for the current path. It must not store source pattern ids, source
names, row labels, branch indexes as semantic targets, or any recovered type
information.

The old one-scrutinee ordered-cascade lowering is forbidden in the final
architecture. Any helper that decides whether a pattern "needs a discriminant",
whether patterns can share a tag subject, or what switch value a source pattern
has is a temporary deletion target. Final IR lowering reads only the decision
plan and finalized row/literal ids produced earlier.

The old Rust compiler's generated mono tests are useful regression inspiration
for this area, especially nested patterns, guards that appear more than once in
the compiled decision tree, matches over multiple values, record and tuple
patterns, list exact/spread patterns, list rest bindings, and literal matches.
Those tests must be ported or replaced with MIR-family tests using Roc's current
`match` terminology.

Payload and field bindings use explicit local ids:

```zig
const LocalValueId = distinct u32;

const LetValue = struct {
    local: LocalValueId,
    expr: ExprId,
    value: ExecutableValueRef,
    exec_ty: ExecTypeId,
};

const Block = struct {
    statements: Span(StmtId),
    result: ExecutableValueRef,
};

const Stmt = union(enum) {
    let_value: LetValue,
    eval_boundary: EvalBoundary,
    branch: BranchStmt,
    runtime_uniqueness_mutation: RuntimeUniquenessMutation,
};
```

`LocalValueId` is allocated by executable MIR's lexical-scope builder. Shadowed
source names become distinct ids. A `let` evaluates its RHS exactly once before
the body that uses it. Debug verification asserts no use-before-definition, no
out-of-scope local use, no escaped branch-local binding, and no direct source
name lookup after local ids have been assigned. Release builds use
`unreachable` for the equivalent compiler-invariant path.

Low-level operations are explicit nodes:

```zig
const LowLevelCall = struct {
    op: LowLevelOpId,
    arg_exprs: Span(ExprId),
    arg_values: Span(ExecutableValueRef),
    result_ty: ExecTypeId,
    result_tmp: TempId,
    abi: LowLevelAbiKey,
    rc_effect: LowLevelRcEffect,
    value_flow: LowLevelValueFlowSignature,
};

const LowLevelRcEffect = struct {
    may_allocate: bool,
    may_retain_or_release: bool,
    may_runtime_uniqueness_check_args: BitSet,
};

const LowLevelValueFlowSignature = union(enum) {
    no_value_flow,
    flows: struct {
        op: LowLevelOpId,
        arg_tys: Span(TypeId),
        result_ty: TypeId,
        edges: Span(LowLevelValueFlowEdge),
        box_boundary: ?BoxBoundaryIntrinsic,
    },
};

const LowLevelValueFlowEdge = union(enum) {
    arg_to_result: struct {
        arg: u32,
        projection: ValueProjectionPath,
    },
    arg_to_result_projection: struct {
        arg: u32,
        arg_projection: ValueProjectionPath,
        result_projection: ValueProjectionPath,
    },
    produced_from_args: struct {
        args: Span(u32),
        result_projection: ValueProjectionPath,
    },
};

const BoxBoundaryIntrinsic = struct {
    boundary: BoxBoundaryId,
    direction: BoxErasureDirection,
};
```

`arg_exprs` evaluate exactly once in source order into `arg_values`. The ABI and
RC-effect metadata are explicit inputs; later stages must not infer them from
operation names. The value-flow signature is a separate explicit input. It
records how representation edges for this low-level operation were created before
executable MIR. Later stages must not derive those edges from the low-level
operation name, argument layouts, result layout, or builtin implementation body.

Every call-only intrinsic or low-level operation that can touch non-primitive
values must publish a checked `LowLevelValueFlowSignature`:

- pure numeric, boolean, and comparison operations use `no_value_flow`
- `List.get_unsafe` links the selected element of argument 0 to the result
- `List.set` links argument 0's element representation and argument 2's value
  representation into the result list's element representation
- `List.append`, `List.prepend`, `List.concat`, `List.split_first`, and
  `List.split_last` explicitly describe element/container result flow
- `Str` operations that return strings explicitly describe whether the result is
  fresh, argument-derived, or structurally independent for representation
  solving; reference-count behavior remains in `LowLevelRcEffect`
- `Box.box` creates a `BoxBoundaryId`, links the payload argument to the boxed
  payload representation, and creates `require_box_erased(boundary)`
- `Box.unbox` creates a `BoxBoundaryId`, links the boxed payload representation
  to the result, and gives any function slots in the result erased callable
  provenance from that boundary

Call-only intrinsics may lower directly to `LowLevelCall` only when they never
flow as first-class values and when they have complete ABI, RC-effect, and
value-flow signatures. First-class intrinsics still lower through wrapper
procedures and `proc_value`. Missing value-flow signatures for non-primitive
low-level operations are compiler invariant violations handled only by
debug-only assertion in debug builds and `unreachable` in release builds.

Executable MIR is not required to be in administrative normal form. It may keep
nested expression structure where that structure does not cross a semantic
boundary. The required invariant is narrower: nodes that branch, bridge, call,
construct aggregates, pack erased functions, or join results must evaluate
their semantically significant operands exactly once and expose those operands
as `ExecutableValueRef` handles before the boundary consumes them.

IR lowering and LIR lowering are responsible for introducing the mechanical
temporaries needed by their own representation. LIR must be in administrative
normal form before reference counting and backend consumption. MIR must not
pretend to be ANF merely to paper over missing boundary value handles.

Executable MIR bridge insertion must preserve single evaluation.

Every source operand with evaluation, allocation, reference-counting, or
control-flow significance
must lower exactly once, in source order, to an executable MIR value handle
before any bridge consumes it. Bridge nodes consume `ExecutableValueRef` handles
only. They do not own source expressions and they must not re-run source
expressions.

This rule applies to:

- `call_proc`
- `call_value`
- `callable_match`
- `Box.box`
- `Box.unbox`
- record construction and update
- tuple construction
- tag construction
- constant materialization
- erased adapters
- erased function packing
- source `match` result joins
- callable-match result joins

Conceptual bridge input shape:

```zig
const BridgeInput = struct {
    value: ExecutableValueRef,
    from_ty: ExecTypeId,
    to_ty: ExecTypeId,
};
```

The exact Zig names may differ, but the restriction must not. A bridge source
must already be an evaluated value handle. Debug verification must assert if a
bridge directly contains an arbitrary source expression, a source `match`, a
call, a box operation, a field projection, a tag payload extraction, or any
expression with effects or reference-counting behavior. In release builds, the
equivalent path is `unreachable`.

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
- the executable MIR program literal pool

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

IR literal, crash, and string-pattern payloads must use `ProgramLiteralId` into
the IR program literal pool. IR must not store `base.StringLiteral.Idx`,
`CheckedStringLiteralId`, raw string bytes in each literal node, or a pointer to
any checked artifact string table.

Executable MIR `match` and `callable_match` lower to IR branch/switch control
flow.

It must not lower to a call-like fallback operation, erased-call fallback,
indirect-call fallback, source dispatch operation, or method operation.

### Reference Counting And Mutation

There is no executable semantic parameter-mode pass and no procedure contract
solve.
Executable MIR lowers directly to IR after it has emitted explicit calls,
bridges, matches, boxed-boundary operations, low-level operations, and value
construction nodes.

Reference counting is handled by a mechanical LIR ARC insertion pass after LIR
is in administrative normal form. That pass consumes:

- LIR values and control flow
- the committed layout graph, including which layouts are refcounted
- explicit call ABI shapes
- explicit low-level RC-effect metadata
- explicit runtime-uniqueness mutation sites

It produces only explicit LIR reference-counting statements:

```text
incref
decref
free
```

It must not produce or consume semantic parameter modes, alias contracts,
procedure result contracts, escape summaries, or interprocedural uniqueness
summaries.

The uniform Roc call boundary for this plan is:

- Procedure parameters are ordinary live value references available during the
  call.
- Procedure results are ordinary live value references returned to the caller.
- The callee must not release the caller's argument references merely because
  they were passed as arguments.
- If a callee returns a parameter value or stores a parameter into a returned
  aggregate, capture, box, or other escaping value, ordinary local ARC insertion
  emits the required `incref` before that escape.
- The caller releases its argument references at their LIR last-use points.
- Calls are mutation barriers. For any refcounted argument whose value is used
  after a call that may perform a runtime uniqueness mutation, ARC insertion
  emits a temporary `incref` before the call and a matching `decref` after the
  call. This makes a callee's `refcount == 1` check fail while the caller still
  needs the old value.

This is a fixed ARC convention, not a heuristic and not a semantic
parameter-mode system. It may retain more than a future alias-permission system
would retain, but it preserves Roc value semantics without static uniqueness
reasoning.

Mutation is represented explicitly:

```zig
const RuntimeUniquenessMutation = struct {
    id: MutationSiteId,
    value: ExecutableValueRef,
    value_ty: ExecTypeId,
    unique_path: MutationUniquePathId,
    shared_path: MutationSharedPathId,
    result_tmp: TempId,
};
```

The exact Zig names may differ, but the operation must have the same shape.
The unique path runs only after the runtime check proves `refcount == 1`. The
shared path allocates or copies as required by the operation and then produces
the same logical result. No MIR or LIR stage may statically mark a value unique
for this plan.

Low-level, hosted, platform, and intrinsic operations that can allocate, retain,
release, or attempt runtime uniqueness mutation must expose explicit
`LowLevelRcEffect` or equivalent metadata before LIR ARC insertion. Later stages
must not infer those effects from names, layout shapes, host symbols, or runtime
function pointers.

`Box.box` and `Box.unbox` are ordinary value operations for ARC purposes.
`Box.unbox` does not mean a consuming move-out and does not have a special
payload-reference contract. If the unboxed payload is refcounted and escapes or
is duplicated, ARC insertion emits the required retains and releases from LIR
use sites. Any future consuming unbox must be a separate explicit operation.

Before IR lowering, executable MIR debug verification must assert:

- there is no semantic parameter-mode pass configured in the pipeline
- no executable MIR node contains parameter-mode or escape-summary contracts
- every runtime mutation site has an explicit `RuntimeUniquenessMutation`
  record or equivalent
- every low-level, hosted, platform, or intrinsic operation with RC behavior has
  explicit RC-effect metadata
- bridges consume evaluated value handles only
- callable-set calls, source matches, erased packing, boxed-boundary operations,
  and aggregate assembly expose all values that ARC insertion will later see

Release builds use `unreachable` for equivalent compiler-invariant paths.

### LIR And Backends

LIR consumes IR.

Reference counting is inserted before backends, as explicit LIR statements.

IR-to-LIR lowering interns every used `ProgramLiteralId` into `LirStore.strings`
and rewrites LIR literal and crash payloads to the resulting
`base.StringLiteral.Idx`. This is the only post-check transition that may
produce `base.StringLiteral.Idx` for lowered program literals. Backends and the
interpreter read literal bytes only from `LirStore.getString`.

Backends consume LIR only. They must not import MIR, IR builder internals,
checked CIR, method registries, or reference-counting analysis.

LIR ARC insertion is a value/control-flow pass, not a backend behavior and not a
semantic procedure-summary pass. It computes where explicit `incref`, `decref`,
and `free` statements belong from LIR uses, branch joins, call boundaries,
runtime mutation sites, and refcounted layout metadata.

Every value-producing LIR statement must expose enough explicit operands for ARC
insertion to see what values are produced and consumed mechanically:

- `call_direct` and `call_erased` expose callee, argument values, result value,
  fixed arity, and call ABI shape.
- `callable_match` has already lowered to ordinary LIR control flow with
  explicit branch inputs and result joins.
- source `match` has already lowered to ordinary LIR control flow with explicit
  scrutinee temporaries, test nodes, pattern bindings, and result joins.
- `callable_set_value`, capture records, records, tuples, tags, boxes, and
  packed erased functions expose their child values as explicit operands.
- bridges expose input value and output value.
- runtime mutation sites expose the checked value, unique path, shared path, and
  joined result.

`RcInsert` is the only non-builtin stage that emits explicit `incref`, `decref`,
and `free`. Backends and the ordinary interpreter path execute those statements
mechanically. They do not branch on layout shape to decide reference-counting
behavior except while executing the explicit LIR RC statements already present.

Before backend lowering, debug-only assertions must fire if any refcounted
layout value produced by executable MIR, IR, or LIR lacks the metadata needed by
ARC insertion, if any mutation site lacks a runtime uniqueness check, or if any
backend/imported path branches on refcounted layout shape except while executing
explicit LIR RC statements. Release builds use `unreachable` for the equivalent
compiler-invariant path.

### Checking Finalization: Compile-Time Constants

Compile-time constants are evaluated by the LIR interpreter before checking is
considered complete.

This stage consumes:

- the checked module
- the MIR-family lowering pipeline
- LIR after reference-count insertion
- explicit compile-time root records
- explicit reification schemas built from the resolved source type and selected
  layout
- explicit callable-result records for roots whose source type is a function
  type

It produces:

- `CompileTimeValueStore`
- one binding entry for each evaluated top-level constant pattern
- private promoted-capture data for promoted callable captures, including
  serializable leaves and private structural graphs with callable leaves
- promoted closed procedure values for top-level callable roots
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
    lir_root: ExecutableProcId,
    result: ComptimeRootResult,
    kind: enum {
        compile_time_constant,
        callable_binding,
        expect_body,
    },
};

const ComptimeRootResult = union(enum) {
    constant_graph: ConstGraphReificationPlanId,
    callable_result: CallableResultId,
    expect_result: ExpectRootId,
};

const ConstGraphReificationPlanId = enum(u32) { _ };

const ConstGraphReificationPlan = union(enum) {
    scalar: ScalarConstPlan,
    string: StringConstPlan,
    list: ListConstPlan,
    box: BoxConstPlan,
    tuple: Span(ConstGraphReificationPlanId),
    record: Span(ConstRecordFieldPlan),
    tag_union: ConstTagPlan,
    transparent_alias: TransparentAliasConstPlan,
    nominal: NominalConstPlan,
    callable_leaf: CallableLeafReificationPlan,
    recursive_ref: ConstGraphReificationPlanId,
};

const CallableLeafReificationPlan = union(enum) {
    finite: CallableResultPlanId,
    erased_boxed: CallableResultPlanId,
    already_resolved: CallableLeafInstance,
};
```

The exact Zig names may differ, but the responsibility must not. A root records
which checked/MIR expression is being evaluated, which executable procedure the
LIR interpreter must execute, and which constant-graph or callable-result record
will reify or promote the result. `constant_graph` is a structural reification
plan, not only a schema id. It says exactly how to copy interpreter results into
`CompileTimeValueStore`, including nested callable leaves. Later stages must not
recreate this information from source syntax, expression shape, naming
conventions, runtime bytes, or environment lookup.

`ConstGraphReificationPlan` is produced before interpretation from resolved
checked source types plus sealed lambda-solved/executable representation data.
It is a graph, not a tree: recursive constants and recursive callable captures
use reserved plan ids plus `recursive_ref` edges. The interpreter may inspect
runtime values only according to this plan. For a `callable_leaf`, the plan
either names the callable-result plan that will be promoted, the erased boxed
callable result whose provenance is an explicit `BoxBoundaryId`, or an already
resolved callable leaf such as an existing top-level procedure. Reification must
not infer callable identity by inspecting source syntax, runtime closure memory,
field names, tag names, generated procedure names, or allocation order.

Compile-time roots must be evaluated through an explicit dependency graph.

Conceptual shape:

```zig
const CompileTimeRootDependencyGraph = struct {
    nodes: Span(CompileTimeRootNode),
    edges: Span(CompileTimeRootEdge),
};

const CompileTimeRootNode = union(enum) {
    compile_time_constant_root: ComptimeRootId,
    callable_binding_root: ComptimeRootId,
    expect_root: ComptimeRootId,
};

const CompileTimeRootEdge = struct {
    from: ComptimeRootId,
    to: CompileTimeRootPrerequisite,
    reason: CompileTimeRootDependencyReason,
};

const CompileTimeRootPrerequisite = union(enum) {
    local_root: ComptimeRootId,
    imported_value: ImportedTopLevelValueRef,
};

const CompileTimeRootDependencyReason = union(enum) {
    top_level_compile_time_constant: PatternId,
    top_level_callable_binding: PatternId,
    imported_checked_artifact: CheckedModuleArtifactKey,
    reachable_procedure_body: ExecutableSpecializationKey,
    callable_capture: CaptureSlot.Index,
    erased_callable_promotion: ErasedFnSigKey,
};
```

The exact Zig names may differ, but the dependency model must not. Direct
top-level function declarations publish `TopLevelProcedureBindingRef` entries
containing `ProcedureValueRef` values; they are not compile-time evaluation
roots. A root edge exists only when evaluating one compile-time root requires a
compile-time constant root, callable binding root, expect root, or imported
checked artifact value to be available first.

`CompileTimeRootEdge.from` is the dependent root. `CompileTimeRootEdge.to` is
the prerequisite local root or imported top-level value that must be available
first.

The dependency graph must be built from sealed callable-aware lowering records,
not from monomorphic MIR alone. Root expression scanning is not enough, and
mono MIR after static dispatch is still too early for first-class calls. A
compile-time root can call a function value whose finite callable members are
known only after lambda-solved representation solving. It can also call an
erased callable whose code, ABI, and capture dependencies are known only through
explicit `BoxBoundaryId` provenance. Checking finalization must therefore run a
summary-only MIR-family lowering path far enough to produce sealed
`CallSiteInfo`, callable emission plans, constant-graph reification plans, and
callable-result plans before it finalizes `CompileTimeRootDependencyGraph`.

This summary path is not runnable MIR. It may record unfilled local roots as
dependency edges, but it must not emit executable MIR, IR, LIR, runtime roots,
or backend input. Runnable lowering happens later, after the graph has been
ordered and every prerequisite root has filled its `ConstRef` template or
published its `procedure_binding`.

Checking finalization must compute a dependency summary for every procedure body
that can execute while evaluating a compile-time root:

```zig
const ComptimeProcDependencySummary = struct {
    proc: ExecutableSpecializationKey,
    availability_values: Span(AvailabilityUse),
    concrete_values: Span(ConcreteValueUse),
    call_deps: Span(ComptimeCallDependency),
    const_graph_deps: Span(ConstGraphDependency),
    callable_result_deps: Span(CallableResultDependency),
};

const ErasedCallableCodeDependency = union(enum) {
    direct_proc_value: ErasedDirectProcCodeDependency,
    finite_set_adapter: ErasedFiniteAdapterDependency,
};

const ErasedDirectProcCodeDependency = struct {
    erase_plan: ProcValueErasePlan,
};

const ErasedFiniteAdapterDependency = struct {
    adapter_key: ErasedAdapterKey,
    member_targets: Span(ExecutableSpecializationKey),
};

const ComptimeCallDependency = union(enum) {
    call_proc: ExecutableSpecializationKey,
    call_value_finite: struct {
        call_site: CallSiteInfoId,
        callable_set: CanonicalCallableSetKey,
        members: Span(ExecutableSpecializationKey),
    },
    call_value_erased: struct {
        call_site: CallSiteInfoId,
        code: ErasedCallableCodeDependency,
        capture_availability: Span(AvailabilityUse),
        capture_concrete_values: Span(ConcreteValueUse),
        provenance: NonEmptySpan(BoxBoundaryId),
    },
};

const ConstGraphDependency = struct {
    plan: ConstGraphReificationPlanId,
    availability_values: Span(AvailabilityUse),
    concrete_values: Span(ConcreteValueUse),
    callable_leaves: Span(CallableLeafDependency),
};

const CallableResultDependency = struct {
    plan: CallableResultPlanId,
    members: Span(ExecutableSpecializationKey),
    capture_availability: Span(AvailabilityUse),
    capture_concrete_values: Span(ConcreteValueUse),
    erased: ?ErasedCallableDependency,
};

const CallableLeafDependency = union(enum) {
    resolved_finite: FiniteCallableLeafInstance,
    promoted_callable: CallableResultPlanId,
    erased_boxed_callable: ErasedCallableDependency,
};

const ErasedCallableDependency = struct {
    code: ErasedCallableCodeDependency,
    capture_availability: Span(AvailabilityUse),
    capture_concrete_values: Span(ConcreteValueUse),
    provenance: NonEmptySpan(BoxBoundaryId),
};

const ComptimeDependencySummary = struct {
    availability_values: Span(AvailabilityUse),
    concrete_values: Span(ConcreteValueUse),
};

const AvailabilityUse = union(enum) {
    local_root: ComptimeRootId,
    imported_value: ImportedTopLevelValueRef,
    const_template: ConstRef,
    procedure_binding: TopLevelProcedureBindingRef,
};

const ConcreteValueUse = union(enum) {
    const_instance: ConstInstantiationKey,
    callable_binding_instance: CallableBindingInstantiationKey,
    procedure_callable: ProcedureCallableRef,
};

const ComptimeDependencySummaryTemplate = struct {
    availability_values: Span(AvailabilityUseTemplate),
    concrete_value_templates: Span(ConcreteValueUseTemplate),
};

const AvailabilityUseTemplate = union(enum) {
    local_root: ComptimeRootId,
    imported_value: ImportedTopLevelValueRef,
    const_template: ConstUseTemplate,
    procedure_binding: ProcedureUseTemplate,
};

const ConcreteValueUseTemplate = union(enum) {
    const_use: ConstUseTemplate,
    callable_binding_use: ProcedureUseTemplate,
    procedure_callable: ProcedureUseTemplate,
};
```

The exact Zig names may differ, but the staging must not. A procedure summary is
computed from sealed lambda-solved/executable call-site records, constant-graph
plans, and callable-result plans. It is not computed from source text, display
names, import strings, unchecked CIR, or mono MIR syntax. It records top-level
values referenced by the body and the concrete executable specializations that
the body may call through `call_proc`, finite `call_value`, erased `call_value`,
callable leaves in constants, callable leaves in promoted captures, and erased
boxed callable values. The compiler then computes the fixed point over
procedure-call SCCs. The summary for a compile-time root is the union of:

Availability and concrete uses are separate. `AvailabilityUse` is for dependency
ordering: it says which local root, imported artifact value, constant template,
or procedure binding must exist before the current root can be evaluated or
published. `ConcreteValueUse` is for runnable lowering: it says which concrete
`ConstInstantiationKey`, `CallableBindingInstantiationKey`, or
`ProcedureCallableRef` is consumed after a generic use has been
clone-instantiated. Concrete summaries may store keys because the concrete
instances have already been reserved and sealed. Code that constructs a new
concrete instance must use `ConstInstantiationRequest` or
`CallableBindingInstantiationRequest`, not the key alone.

Generic templates do not store concrete summaries. A `ConstEvalTemplate` or
`CallableEvalTemplate` stores a `ComptimeDependencySummaryTemplate` containing
only availability uses and use templates. A concrete
`ConstInstantiationRequest` or `CallableBindingInstantiationRequest`
instantiates that template in the requesting artifact's concrete mono context,
resolves static dispatch, finite callable members, erased callable code,
callable leaves, and promoted captures, and then stores a concrete
`ComptimeDependencySummary` in the concrete instance. The template must not
manufacture a concrete `ConstInstantiationKey`,
`CallableBindingInstantiationKey`, `ProcedureCallableRef`, executable
specialization, or concrete source type payload before the requested source type
is fully resolved. This is required for generic constants such as
`table = { f: id }`, where different consumers of the same exported constant
instantiate callable leaves at different function types, and for generic
callable roots such as `also_id = choose(True, id, id)`, where different
consumers instantiate the callable binding at different function types.

- top-level values named directly by the root expression
- imported top-level values named directly by the root expression
- the fixed-point summaries of every `call_proc` target reachable while
  evaluating that root
- every member target of every finite `call_value` reachable while evaluating
  that root, including singleton finite callable sets
- every erased-call code dependency and capture dependency named by explicit
  erased call plans with non-empty `BoxBoundaryId` provenance. A direct erased
  code dependency consumes the exact `ProcValueErasePlan` that will be used to
  pack or call the procedure value. A finite-set erased code dependency names the
  `ErasedAdapterKey` and every member executable specialization reachable
  through the adapter's mandatory `callable_match`, including singleton sets.
- every callable leaf reachable through the root's constant-graph reification
  plan or callable-result plan
- top-level values captured by any callable value that can be returned from that
  root and then promoted

Compile-time root dependencies are availability dependencies under Roc's
checking-time top-level constant rule. They are not a dynamic demand trace of
the exact interpreter branch taken by one execution. If a sealed lowered record
for a root or for a reachable procedure body contains a resolved reference to a
non-procedure top-level value, that referenced root is a prerequisite even when
the interpreter would not evaluate that expression path for a particular input.
This conservative edge is allowed only when it is the same edge checking would
use for the top-level constant cycle relation and it comes from sealed lowered
records. The collector must not invent edges by scanning names, source syntax,
display strings, unchecked CIR, or environment tables after lowering. Missing
edges are compiler bugs.

`ProcSpecializationKey` is not precise enough for this dependency summary. The
summary is built after callable representation has been solved and after the
target compile-time interpreter configuration is known, so it must name
`ExecutableSpecializationKey` for runnable procedure bodies. For erased
callables it must name the erased code dependency shape above instead of
collapsing the dependency to a source procedure specialization, because erased
code identity includes `ProcValueErasePlan` or `ErasedAdapterKey`. This is the
Cor/LSS distinction between a function name and an erased specialization whose
hidden capture record type is part of the key.

For example:

```roc
a : I64
a = if Bool.true { 1 } else { b }

b : I64
b = a + 1
```

Under this plan, the reference to `b` inside `a` is an availability edge, even
though that branch is not dynamically taken. The cycle is therefore reported
during checking finalization. If Roc later chooses demand-driven semantics for
top-level constants, this dependency graph must be redesigned explicitly; later
stages must not silently switch to demand tracing by dropping sealed
availability edges.

Dependency summaries have one special checking-finalization-only mode for local
roots that have not been evaluated yet. While building
`CompileTimeRootDependencyGraph`, the compiler may encounter a top-level binding
whose reserved `ConstRef` is not filled or whose function-valued root is still
`pending_callable_root`. In that mode, the summary collector records
`AvailabilityUse.local_root` and continues collecting dependencies, but it does
not produce runnable mono MIR, executable MIR, IR, LIR, or backend input. It is a
dependency-analysis product, not a lowering result.

The summary collector must still run inside a concrete mono specialization
context. Static dispatch inside the summarized body must be resolved to concrete
`call_proc` targets using the same `StaticDispatchCallPlan` algorithm as normal
mono lowering. Direct procedure calls and `proc_value` dependencies reserve
ordinary mono specializations and participate in the fixed-point summary graph.
Only top-level value lookup may produce `AvailabilityUse.local_root` for an
unfilled local root, and only before the checked artifact is published.

After dependency ordering has evaluated a prerequisite root, real MIR lowering
for any dependent root may turn `ConstUseTemplate` records into explicit
`ConstInstantiationRequest` values and `ProcedureUseTemplate` records into
explicit `CallableBindingInstantiationRequest` or `ProcedureCallableRef` values.
The sealed concrete summaries may later refer to the corresponding keys. If a
runnable post-check lowering path sees an unfilled reserved constant template, a
`pending_callable_root`, or a generic use template where a concrete use is
required, that is a compiler invariant violation handled by debug-only assertion
in debug builds and `unreachable` in release builds.

The pending collection mode must not insert placeholder constants, placeholder
procedure values, placeholder local symbols, runtime thunks, runtime initializer
procedures, or closure objects. It must not suppress a dependency to make
lowering proceed. Its only observable output is a dependency edge to a real local
compile-time root.

For example:

```roc
make_adder : I64 -> (I64 -> I64)
make_adder = |n| |x| x + n

add5 : I64 -> I64
add5 = make_adder(5)

use_add5 : I64 -> I64
use_add5 = |x| add5(x)

answer : I64
answer = use_add5(37)
```

`add5` is a `callable_binding` root. `answer` is a
`compile_time_constant_root`. The expression for `answer` names only `use_add5`,
but the dependency graph still contains an edge from `answer` to `add5` because
the monomorphic body summary for `use_add5` names `add5`. `answer` cannot lower
through `TopLevelValueTable` until `add5` has been promoted to
`procedure_binding`.

Checking finalization must topologically evaluate compile-time roots according
to the availability graph described above. When a root depends on another local
root, the dependency must be evaluated, reified or promoted, and published into
the in-progress `TopLevelValueTable` before the dependent root is lowered.
Serializable dependencies are consumed as `ConstRef`. Callable dependencies are
consumed as promoted `ProcedureValueRef` values. Imported dependencies are
consumed from imported checked artifacts; they are not evaluated again. The
topological order is intentionally stricter than dynamic interpreter demand; it
matches the checking-time rule that all non-procedure top-level references must
be available before post-check lowering.

`TopLevelValueTable` exists before compile-time root evaluation starts. Checking
finalization first reserves top-level value identities and seeds the in-progress
table. Every non-function top-level constant receives a reserved `ConstRef`
before dependency analysis and before compile-time evaluation. Every direct
top-level function declaration and every top-level lambda that is already a
procedure declaration receives a `TopLevelProcedureBindingRef`. A function-valued
root that still requires compile-time callable evaluation may be temporarily
represented as `pending_callable_root`, but only inside checking finalization:

```zig
const TopLevelProcedureBindingRef = enum(u32) { _ };
const CallableEvalTemplateId = enum(u32) { _ };
const CallableBindingInstanceId = enum(u32) { _ };
const ComptimeDependencySummaryTemplateRef = enum(u32) { _ };
const ComptimeDependencySummaryId = enum(u32) { _ };
const ComptimeOnlyExecutableRootId = enum(u32) { _ };

const TopLevelProcedureBinding = struct {
    source_scheme: CanonicalTypeSchemeKey,
    body: ProcedureBindingBody,
};

const ProcedureBindingBody = union(enum) {
    direct_template: DirectProcedureBinding,
    callable_eval_template: CallableEvalTemplateId,
};

const DirectProcedureBinding = struct {
    proc_value: ProcedureValueRef,
    template: CallableProcedureTemplateRef,
};

const CallableEvalTemplate = struct {
    body: CheckedCallableBodyRef,
    source_scheme: CanonicalTypeSchemeKey,
    resolved_value_refs: ResolvedValueRefTableRef,
    static_dispatch_plans: StaticDispatchPlanTableRef,
    nested_proc_sites: NestedProcSiteTableRef,
    dependency_template: ComptimeDependencySummaryTemplateRef,
};

const CallableBindingInstantiationKey = struct {
    binding: ProcedureBindingRef,
    requested_source_fn_ty: CanonicalTypeKey,
};

const CallableBindingInstantiationRequest = struct {
    key: CallableBindingInstantiationKey,
    requested_source_fn_ty_payload: ConcreteSourceTypeRef,
};

const CallableBindingInstantiationStoreRef = struct {
    owner: CheckedModuleArtifactKey,
};

const CallableBindingInstanceRef = struct {
    store: CallableBindingInstantiationStoreRef,
    key: CallableBindingInstantiationKey,
    instance: CallableBindingInstanceId,
};

const CallableBindingInstance = struct {
    key: CallableBindingInstantiationKey,
    dependency_summary: ComptimeDependencySummaryId,
    executable_root: ComptimeOnlyExecutableRootId,
    result_plan: CallableResultPlanId,
    promotion_plan: ?CallablePromotionPlanId,
    promotion_output: CallablePromotionOutput,
    proc_value: ProcedureCallableRef,
};

const CallablePromotionOutput = union(enum) {
    existing_procedure: ProcedureCallableRef,
    promoted_procedure: PromotedProcedureRef,
};

const CallableBindingInstantiationStore = struct {
    owner: CheckedModuleArtifactKey,
    instances: Map(CallableBindingInstantiationKey, CallableBindingInstantiationState),
};

const CallableBindingInstantiationState = union(enum) {
    reserved,
    evaluating,
    evaluated: CallableBindingInstance,
};

const TopLevelValue = union(enum) {
    const_template: ConstRef,
    procedure_binding: TopLevelProcedureBindingRef,
    pending_callable_root: ComptimeRootId,
};
```

Semantic instantiation ownership is artifact-only. `ConstInstantiationStore` and
`CallableBindingInstantiationStore` are owned by checked artifacts, never by a
post-check lowering stage. A post-check lowering stage may create target
layouts, executable specializations, IR, LIR, target-specific constant
materialization, and object-code artifacts, but it must not create or evaluate a
new semantic constant instance, callable-binding instance, promoted procedure,
private capture graph, or synthetic checked procedure template.

For imported generic values, the exported template remains owned by the exporting
artifact, and the concrete instance is owned by the consuming artifact:

```text
const_ref.artifact = exporting artifact
const_instance.store.owner = consuming artifact

procedure_binding.binding = exported procedure binding
callable_binding_instance.store.owner = consuming artifact
```

The consuming artifact requests and seals those concrete instances during its own
checking finalization before it is published. It must not mutate the exporting
artifact, inspect exporting source, or re-run the exporting module's
compile-time roots. Once any artifact has been published, missing semantic
instances are compiler invariant violations: debug builds assert immediately and
release builds use `unreachable`.

Only checking finalization may create `pending_callable_root` entries, and no
published artifact may contain one. During root evaluation, each completed
non-function compile-time constant fills and seals the already-reserved
`ConstRef` template. It does not allocate the `ConstRef` for the first time after
evaluation. Each completed callable binding replaces its temporary
`pending_callable_root` with a sealed `TopLevelProcedureBindingRef`. If the
callable binding is fully concrete, that binding may contain a
`direct_template`. If the callable binding is generalized and cannot be evaluated
without a concrete requested function type, the sealed binding contains a
`callable_eval_template`. The published `TopLevelValueTable` still contains a
`procedure_binding` entry, never `pending_callable_root`.

Later lowering consumes the published table. It must not build the table for the
first time after evaluating roots, allocate a new top-level `ConstRef` while
lowering a reference, or interpret a missing entry as permission to synthesize a
runtime initializer.

Cycles among non-procedure compile-time roots are checking diagnostics before
artifact publication. A cycle that reaches a later post-check stage is a
compiler invariant violation handled by debug-only assertion in debug builds
and `unreachable` in release builds.

Private aggregate interpreter roots may group only roots whose dependencies are
already satisfied or roots in a dependency-ordered batch with no internal
cycle. Aggregate roots must not hide root dependency ordering or cause a later
root to observe a missing `TopLevelValueTable` entry.

#### Top-Level Callable Finalization

Checking finalization handles top-level callable bindings before publishing the
checked artifact. After publication, every top-level binding whose source type
is a function must be a `procedure_binding: TopLevelProcedureBindingRef`. The
binding owns the source type scheme at which it may be requested and either a
direct sealed procedure template or an instantiable compile-time callable
evaluation template. There is no post-check top-level closure-value category.

There are three source-level cases:

1. A top-level function declaration or top-level lambda is already a procedure
   declaration. It does not need to be evaluated by the LIR interpreter just to
   discover that it is callable. Publication records the binding as
   `procedure_binding` pointing at the procedure value and checked callable
   template created from the declaration. If its body references top-level
   constants, those references are resolved through `TopLevelValueTable` after
   checking finalization: compile-time constants become `ConstUseTemplate` reads
   that instantiate through concrete `ConstInstantiationRequest` values only
   inside a concrete lowering context, and function-valued bindings become
   `ProcedureUseTemplate` reads.
2. A top-level binding with function source type whose expression is not already
   a function declaration or top-level lambda is a compile-time callable root.
   If the binding's source function type is fully concrete, checking finalization
   evaluates the expression through the same MIR-family-to-LIR path used for
   compile-time constants. The interpreter result is a compile-time callable
   value, not serialized constant data. If the binding's source function type is
   generalized and the expression cannot be represented as one already-sealed
   direct callable template, checking finalization publishes a
   `CallableEvalTemplate` instead. A consumer that needs a concrete use evaluates
   that template with a concrete `CallableBindingInstantiationRequest` during
   the consuming artifact's checking finalization. The request's key supplies
   stable identity; the request's `ConcreteSourceTypeRef` supplies the source
   function type payload used to lower and promote the callable. A published
   artifact never contains a `ProcedureUseTemplate` that requires post-check
   lowering to evaluate a `CallableEvalTemplate`.
3. A top-level binding with non-function source type is a compile-time
   constant. It is evaluated and reified into `CompileTimeValueStore` as a
   `ConstRef` template graph. That graph may contain callable leaves nested inside
   records, tuples, tags, `List(T)`, `Box(T)`, transparent aliases, or
   nominals. Callable leaves are explicit procedure-value references or
   recursively promoted closed procedure values; they are not runtime thunks,
   runtime global initializer procedures, runtime top-level closure objects, or
   interpreter pointers.

For example, this binding is case 1:

```roc
add1 : I64 -> I64
add1 = |x| x + 1
```

This binding is case 2:

```roc
make_adder : I64 -> (I64 -> I64)
make_adder = |n| |x| x + n

add5 : I64 -> I64
add5 = make_adder(5)
```

An alias to an existing top-level procedure is also case 2:

```roc
inc : I64 -> I64
inc = |x| x + 1

also_inc : I64 -> I64
also_inc = inc
```

`also_inc` must not introduce a separate callable-alias semantic category. It is
a compile-time callable root like any other function-valued expression. Checking
finalization evaluates the expression `inc` through the normal compile-time
callable path. Reification produces the same compiler-owned callable result it
would produce for any other root:

```zig
finite {
    source_fn_ty = canonical("I64 -> I64"),
    callable_set_key = canonical_callable_set([inc]),
    member = member(inc),
    captures = [],
}
```

Publication may then map `also_inc` directly to the existing procedure binding
for `inc`. If a distinct procedure value is required for export metadata, debug
provenance, or other artifact bookkeeping, that value must be produced as a
normal promoted checked wrapper from the evaluated `ComptimeCallable`; it is not
a separate alias representation and it must not be recovered by recognizing
source syntax.

A generic function-valued root follows the same rule, but it publishes an
instantiable procedure binding rather than one concrete mono procedure:

```roc
id : a -> a
id = |x| x

also_id = id

use_int = also_id(1)
use_str = also_id("x")
```

`also_id` is not a runtime closure object and not a runtime thunk. Checking
finalization processes the root through the compile-time callable path. Because
the expression reifies to an existing captureless checked procedure template, the
published `TopLevelProcedureBinding` may use `ProcedureBindingBody.direct_template`
with `CallableProcedureTemplateRef.checked(template_ref_of_source_proc(id))`, or a
sealed `CallableProcedureTemplateRef.synthetic` wrapper if a distinct exported
procedure identity is required. This direct publication is an outcome of
callable-template reification, not a syntax shortcut and not a callable-alias
category. `use_int` and `use_str` create separate concrete procedure uses by
clone-instantiating the binding's source scheme at `I64 -> I64` and
`Str -> Str`; they do not force `also_id` to become an alias table entry or a
single concrete specialization.

A generic function-valued root whose value must be computed at the concrete
function type publishes `ProcedureBindingBody.callable_eval_template`:

```roc
id : a -> a
id = |x| x

choose : Bool, (a -> a), (a -> a) -> (a -> a)
choose = |b, f, g| if b { f } else { g }

also_id = choose(True, id, id)

use_int = also_id(1)
use_str = also_id("x")
```

The published `also_id` binding has one generalized `source_scheme` and one
`CallableEvalTemplate`. `use_int` creates a
`CallableBindingInstantiationRequest` whose key is
`CallableBindingInstantiationKey(binding = also_id, requested_source_fn_ty =
I64 -> I64)` and whose payload is a `ConcreteSourceTypeRef` for the concrete
`I64 -> I64` function type. `use_str` creates a second request at `Str -> Str`
with a different payload. Each request reserves a `CallableBindingInstance` in
the artifact that requested the use. During that artifact's checking
finalization, the compiler lowers the checked callable body through mono MIR,
row-finalized mono MIR, lifted MIR, lambda-solved MIR, executable MIR, IR, and
LIR at that concrete function type payload, computes a concrete dependency
summary, builds the concrete `CallableResultPlan`, runs the LIR interpreter,
reifies the `ComptimeCallable`, promotes it if needed, and seals the instance as
a concrete `ProcedureCallableRef`. No generalized executable MIR, generalized
`ComptimeCallable`, runtime top-level closure object, runtime thunk, or callable
alias table is produced.

After checking finalization, `add5` must be represented exactly like a
top-level function. Conceptually, later stages see the equivalent of:

```roc
add5 : I64 -> I64
add5 = |x| x + 5
```

The implementation must not depend on source rewriting, but the published
artifact must have the same semantic shape: `add5` is a closed
`TopLevelProcedureBindingRef` whose `ProcedureBindingBody.direct_template`
contains a `ProcedureValueRef` with the fixed arity from its resolved source
function type.

Compile-time callable evaluation may call ordinary top-level functions. Those
functions are consumed as procedure values during interpretation, exactly as
they are consumed by later runtime lowering. The interpreter does not evaluate a
top-level function declaration merely because it exists; it only executes
listed compile-time roots and any procedures those roots call.

This uniform compile-time evaluation rule is intentionally broader than
procedure aliases. These all use the same root/reify/publish path:

```roc
also_inc = inc
add5 = make_adder(5)
choose = if use_inc { inc } else { dec }
table = { f: inc }
```

The result shape determines publication: a root whose source type is a function
publishes as `procedure_binding`, and a non-function root publishes as `ConstRef`
whose graph may contain callable leaves. No compiler stage needs a callable
alias side table to handle any of these examples.

`CallableBindingInstantiationStore` has the same reserve/fill/seal discipline as
procedure specialization and constant instantiation. When a concrete use of a
function-valued top-level binding is requested, the compiler receives a
`CallableBindingInstantiationRequest`. It first clone-instantiates the binding's
`source_scheme` and the request's `requested_source_fn_ty_payload` into one
checking-finalization source type graph, validates that the request is an
instance of that scheme, and debug-asserts that the payload's canonical key
equals `key.requested_source_fn_ty`. A `direct_template` binding then produces a
concrete `ProcedureCallableRef` from the direct callable template and the
requested source function type payload. A `callable_eval_template` binding
reserves a `CallableBindingInstanceRef`, lowers the checked callable body at the
requested source function type payload, evaluates the private compile-time root
through the LIR interpreter, reifies the `ComptimeCallable`, promotes captured
callable results if needed, and fills the instance with a complete
`CallableBindingInstance`.

A `CallableBindingInstance` is the concrete evaluation product, not just a
procedure pointer. It records the concrete instantiation key, concrete dependency
summary, private compile-time executable root used by the interpreter, concrete
`CallableResultPlan`, optional `CallablePromotionPlan`, promotion output, and
final `ProcedureCallableRef`. These records are sealed in the owning checked
artifact. Later executable, IR, LIR, materialization, backend, REPL, glue, and
test paths consume the sealed `ProcedureCallableRef` and related records; they
must not run the callable-eval root or create promoted procedures after artifact
publication.

The store owner is always the checked artifact that requested the concrete
callable binding instance. This matters for imported generic callable bindings:
an importing module may instantiate an exported callable eval template at a
concrete type, but it does so while building the importing checked artifact. It
must not mutate the exporting artifact, inspect exporting source, or rerun an
already-published imported module's roots. Missing callable binding instances
after an artifact has been published are compiler invariant violations: debug
builds assert immediately and release builds use `unreachable`.

#### Generic Constant Templates

A non-function top-level constant may be generic and may contain callable slots.
This is valid Roc code and must not be rejected, lowered through a runtime
top-level thunk, or represented as a runtime top-level closure object:

```roc
id : a -> a
id = |x| x

table = { f: id }

use_int = (table.f)(1)
use_str = (table.f)("x")
```

`table` publishes as one reserved and sealed `ConstRef` whose source type is a
generalized record type scheme. The constant template behind that `ConstRef` may
be a structural value-graph template or an evaluation template. For this simple
example, the `f` field can be represented as a callable leaf template that names
the checked procedure template for `id`, not a concrete mono output procedure.
Each concrete use creates an explicit constant instantiation request only after
the surrounding generic use has been clone-instantiated:

```zig
const ConstInstantiationKey = struct {
    const_ref: ConstRef,
    requested_source_ty: CanonicalTypeKey,
};
```

For the example above, `use_int` requests the `table` constant at a record type
whose `f` field has type `I64 -> I64`; `use_str` requests the same `ConstRef` at a
record type whose `f` field has type `Str -> Str`. During the requesting
artifact's checking finalization, those requests materialize two concrete
constant instances from the same target-independent template. They do not create
two source-visible top-level constants, they do not turn `table` into a runtime
initializer, and they do not leave any semantic instantiation work for post-check
lowering.

A generalized `ConstValueGraphTemplate` may contain a finite callable leaf
template only when that leaf's callable procedure template identity is already
sealed without a future owner mono specialization. This is true for an existing
checked/imported/hosted/platform-required/promoted procedure template such as
`id` above.
It is not true for an inline lambda or local function whose lifted identity
requires an owning mono specialization that does not exist until the constant is
instantiated at a concrete type:

```roc
table = { f: |x| x }

use_int = (table.f)(1)
use_str = (table.f)("x")
```

This `table` binding is valid Roc code, but it must publish a `ConstEvalTemplate`
unless the compiler has already created a sealed synthetic checked template for a
semantically equivalent closed procedure without relying on a future lifted owner.
At `{ f : I64 -> I64 }`, constant instantiation lowers the expression at the
concrete requested record type during the requesting artifact's checking
finalization, creates the concrete lifted or promoted callable template, reifies
the record, and stores a concrete callable leaf instance for the `I64` function
field. At `{ f : Str -> Str }`, it performs a separate concrete instantiation.
If those instantiations create synthetic checked procedure templates or promoted
procedures, the requesting artifact owns and seals them before publication. The
plan must not add a pre-mono "lifted" callable template variant and must not
invent a fake `owner_mono_specialization` for a generic value graph.

Not every generic constant can publish a complete value graph before it is
instantiated. A generic constant expression may need compile-time evaluation at
the concrete requested type:

```roc
id : a -> a
id = |x| x

choose : Bool, (a -> a), (a -> a) -> (a -> a)
choose = |b, f, g| if b { f } else { g }

table = { f: choose(True, id, id) }

use_int = (table.f)(1)
use_str = (table.f)("x")
```

The exported `table` binding still publishes one `ConstRef`, but that `ConstRef`
must point at a `ConstEvalTemplate`, not a pre-evaluated concrete value. The
`I64` use and the `Str` use each create a `ConstInstantiationRequest`, reserve a
concrete `ConstInstanceRef` under the request's key, evaluate the template
through the MIR-family path and LIR interpreter for the request's concrete source
type payload, and then reify the result into the requesting artifact's
`ConstInstantiationStore` during that artifact's checking finalization. This is
generic constant instantiation. It is not runtime thunking, not runtime global
initialization, not post-check semantic work, and not imported-module LIR
re-execution after checking.

The template/instance split is mandatory:

- `ConstRef` identifies the checked, target-independent constant template and
  its source type scheme.
- `ConstInstantiationKey` identifies a concrete use of that template at a fully
  resolved source type.
- `ConstInstantiationRequest` carries the `ConstInstantiationKey` plus the
  `ConcreteSourceTypeRef` payload required to evaluate or reify that use.
- `ConstEvalTemplate` identifies a checked expression template plus
  reification-template data that must be evaluated separately for each requested
  concrete source type.
- `ConstValueGraphTemplate` identifies a structural logical value graph that can
  be instantiated without re-running an expression body, although callable leaf
  templates inside it still instantiate at the concrete source function type.
- executable MIR and LIR materialization consume only concrete constant
  instances, never generalized constant templates.
- finite callable leaf instances use the instantiated source function type to
  request the correct `MonoSpecializationKey`.
- imported modules consume serialized exported constant templates and instantiate
  them from explicit imported `ConstRef` values while building the consuming
  checked artifact. The consuming artifact owns the concrete instances it
  requests. It must not inspect exporter source, rerun exporter roots, create
  semantic instances during post-check lowering, or recover callable leaves from
  syntax.

If a constant instance reaches executable MIR with unresolved generalized
variables, or if a callable leaf template is materialized without a concrete
source function type, that is a compiler invariant violation. Debug builds assert
at the first boundary that observes it; release builds use `unreachable`.

Compile-time callable roots produce `ComptimeCallable` values:

```zig
const ComptimeCallable = union(enum) {
    finite: FiniteComptimeCallable,
    erased: ComptimeErasedCallable,
};

const CaptureValueId = enum(u32) { _ };

const FiniteComptimeCallable = struct {
    source_fn_ty: CanonicalTypeKey,
    callable_set_key: CanonicalCallableSetKey,
    member: CallableSetMemberId,
    captures: Span(CaptureValueId),
};

const ComptimeErasedCallable = struct {
    source_fn_ty: CanonicalTypeKey,
    sig_key: ErasedFnSigKey,
    code: ErasedCallableCodeRef,
    capture: ErasedComptimeCapture,
    provenance: NonEmptySpan(BoxBoundaryId),
};

const ErasedComptimeCapture = union(enum) {
    none,
    zero_sized_typed: ErasedCaptureTypeKey,
    values: Span(CaptureValue),
};

const CaptureValue = union(enum) {
    serializable_leaf: ConstRef,
    callable_leaf: ComptimeCallableId,
    record: Span(PrivateCaptureFieldValue),
    tuple: Span(CaptureValueId),
    tag_union: PrivateCaptureTagValue,
    list: Span(CaptureValueId),
    box: PrivateCaptureBoxValue,
    nominal: PrivateCaptureNominalValue,
};
```

The exact Zig names may differ, but the meaning must not. A compile-time
callable value is a compiler-owned description of either a selected finite
callable-set member with evaluated captures or an erased callable with an
explicit `ErasedFnSigKey`, erased callable code ref, compiler-owned capture
value, and non-empty `BoxBoundaryId` provenance. It is not a runtime function
pointer, not a heap closure object, not a global initializer, and not a thunk.
The erased code ref is either a direct erased procedure code ref with capture
shape or a finite-set adapter ref. It must not be a bare symbol and must not be a
finite callable leaf.

`FiniteComptimeCallable` must store exactly the selected finite callable value:
`source_fn_ty`, `callable_set_key`, `member`, and evaluated `captures` in the
member's canonical capture-slot order. These are the semantic fields required to
reconstruct the same finite callable value that Cor/LSS represents as a
callable-set tag plus optional capture payload. The member procedure, tag index,
capture slot schema, and capture shape are derived from `callable_set_key +
member`; implementations may cache derived values only as debug-verified
accelerators. A finite compile-time callable must not store `CallableEntry`, a
raw procedure `Symbol`, a raw lifted-lambda `Symbol`, `FiniteCallableLeafInstance`,
layout id, generated symbol text, runtime function pointer, or executable
specialization as its primary identity.

If a compile-time callable has no captures and its callable set contains a
single member, it is still represented as the selected finite callable value
above until publication or materialization explicitly chooses to collapse it into
a closed `FiniteCallableLeafInstance`. That collapse is allowed only when the
callable-set key proves the selected member has an empty capture schema and the
source function type is fully instantiated. If the selected member has captures,
publication must first promote the callable to a closed promoted procedure or
keep it as a selected finite callable value for materialization; it must not
pretend that it is an empty-capture finite leaf.

Capture values are structural compiler-owned graphs. A `serializable_leaf`
points at a source-visible top-level constant or at a serializable private
capture leaf created while promoting a compile-time callable. A `callable_leaf`
inside a compile-time callable graph points at another compiler-owned callable
value that must be promoted recursively. Records, tuples, tags, lists, boxes,
transparent aliases, and nominals preserve the exact source-level container
shape. Public constant graphs and source private capture graphs share the same
logical container discipline, but not the same callable leaf contract: public
constant graphs may contain finite callable leaves and erased boxed callable
leaves, while source private capture graphs may contain only finite callable
leaves and concrete const-instance leaves. No graph stores raw interpreter
addresses, Roc heap pointers, runtime closure objects, or thunk entrypoints. The
`ConstRef.owner` identifies source-visible top-level data versus private capture
data.

Private promoted-capture data has two forms. Serializable leaves are ordinary
compiler-owned constant templates addressed by `ConstRef` with
`ConstOwner.promoted_capture`; concrete uses carry `ConstInstantiationKey`.
Mixed structural capture graphs are addressed by
`PrivateCaptureRef` and may contain concrete const-instance leaves, finite
callable leaves, and source-level containers. They must not contain erased boxed
callable leaves; any non-function subtree that needs erased boxed callable
materialization is represented by a concrete private `ConstInstanceRef` leaf.
Neither form corresponds to a source top-level binding, is exported or imported
by name, appears as a `TopLevelValueTable` entry, or can be looked up by later
stages except through explicit promoted-procedure body operands or debug
provenance.

Erased compile-time callable results are allowed only when their
`ErasedFnSigKey` and callable provenance came from explicit `Box(T)` boundaries.
Promotion of an erased compile-time callable creates a closed ordinary procedure
symbol whose body performs the known erased call using the known erased capture.
It must not publish a top-level packed erased callable object, runtime closure
object, global callable object, or runtime thunk.

For example:

```roc
make_boxed : {} -> Box(I64 -> I64)
make_boxed = |_| Box.box(|x| x + 1)

add1 : I64 -> I64
add1 = Box.unbox(make_boxed({}))
```

`add1` is a function-valued top-level binding. Checking finalization must
promote it to a closed procedure that calls the known erased callable. The
erased callable is valid only because the erased representation came from the
explicit `Box(I64 -> I64)` boundary. The implementation must not leave a
runtime top-level erased callable object behind.

Callable promotion turns each `ComptimeCallable` that is the result of a
top-level binding into a closed top-level procedure value before artifact
publication. Promotion must:

- allocate a `ProcedureValueRef` owned by the checked artifact; exported bindings
  export that promoted procedure value, and private bindings keep it private
- give the promoted procedure the exact fixed arity and resolved source function
  type of the top-level binding
- rewrite every capture read in the callable body to an explicit private
  capture path whose serializable leaves are `ConstRef` reads with concrete
  `ConstInstanceRef` values selected from sealed `ConstInstantiationRequest`
  instances and whose callable leaves are `procedure_value` references
- resolve callable leaves to sealed procedure values before the outer promoted
  procedure is published. Existing source/imported/hosted/platform-required
  procedure bindings remain existing procedure values; local closure leaves and
  evaluated callable leaves are recursively promoted to private closed procedure
  values.
- require every captured value to have a complete `CaptureSlotReificationPlan`.
  Missing capture data after type checking is a compiler invariant violation:
  debug-only assertion in debug builds and `unreachable` in release builds.
  It must not be left for a post-check stage to repair.
- record debug provenance from the promoted procedure value back to the original
  top-level binding and callable expression

Callable result reification consumes a precomputed result plan. The interpreter
may read the runtime callable tag and capture payload only according to this
plan, in the same way constant-graph reification reads runtime bytes and
callable leaves only according to `ConstGraphReificationPlan`. It must not
inspect runtime memory to discover callable members, capture fields, source
names, dependencies, or callable identity.

Conceptual shape:

```zig
const CallableResultPlan = union(enum) {
    finite: FiniteCallableResultPlan,
    erased: ErasedCallableResultPlan,
};

const CallableResultPlanId = enum(u32) { _ };

const FiniteCallableResultPlan = struct {
    source_fn_ty: CanonicalTypeKey,
    callable_set_key: CanonicalCallableSetKey,
    members: Span(CallableResultMemberPlan),
};

const ErasedCallableResultPlan = struct {
    source_fn_ty: CanonicalTypeKey,
    sig_key: ErasedFnSigKey,
    provenance: NonEmptySpan(BoxBoundaryId),
    code: ErasedCallableCodeRef,
    capture: ErasedCaptureReificationPlan,
};

const ErasedCaptureReificationPlan = union(enum) {
    none,
    zero_sized_typed: ErasedCaptureTypeKey,
    whole_hidden_capture_value: ErasedCaptureSlotReificationRef,
    proc_capture_tuple: Span(ErasedCaptureSlotReificationRef),
    finite_callable_set_value: CallableResultPlanId,
};

const ErasedCaptureSlotReificationRef = struct {
    source_ty: CanonicalTypeKey,
    plan: CaptureSlotReificationPlanId,
};

const CallableResultMemberPlan = struct {
    member: CallableMemberId,
    capture_slots: Span(CaptureSlotReificationPlan),
};

const CaptureSlotReificationPlan = union(enum) {
    serializable_leaf: SerializableCaptureLeafPlan,
    callable_leaf: CallableResultPlanId,
    record: Span(PrivateCaptureFieldPlan),
    tuple: Span(PrivateCaptureElemPlan),
    tag_union: PrivateCaptureTagUnionPlan,
    list: PrivateCaptureListPlan,
    box: PrivateCaptureBoxPlan,
    nominal: PrivateCaptureNominalPlan,
    recursive_ref: CaptureSlotReificationPlanId,
};

const CaptureSlotReificationPlanId = enum(u32) { _ };
```

The exact Zig names may differ, but the contract must not. The result plan is
produced before interpretation from the resolved lambda-solved/executable
callable representation. `CallableResultPlan` and
`CaptureSlotReificationPlan` records form a reserved graph, not a tree.
Recursive callable captures and recursive aggregate captures must use reserved
ids plus `recursive_ref`; they must not duplicate nodes or keep extending a path
forever. Capture-slot reification is structural, not a single-level
serializable-or-callable test. A serializable leaf gives the resolved source
type and schema used to copy the interpreter result into
`CompileTimeValueStore`. A callable leaf names another callable-result plan that
will be promoted recursively or resolved to an already sealed procedure value.
For a finite callable result, `source_fn_ty + callable_set_key + member` is the
complete selected-member identity. `CallableResultMemberPlan` therefore stores
only the `member` and its capture-slot reification plans; the member procedure,
tag index, capture slot schema, and capture shape are derived from
`callable_set_key + member`. If a later implementation caches those derived
values, debug builds must assert that the cache matches the canonical callable
set and release builds must use `unreachable` for a mismatch.
Records, tuples, tags, lists, boxes, transparent aliases, and nominals describe
exactly where those leaves live inside a private promoted-capture value. A
transparent alias records source/debug provenance only and reifies the
underlying payload; it is not a runtime wrapper. A nominal records the nominal
representation capability used to materialize the payload. For an erased
callable result, the plan carries the exact `ErasedFnSigKey`, erased code
ref, capture reification plan, and non-empty `BoxBoundaryId` provenance. The
code ref has the same exact two-case shape everywhere: direct erased procedure
code with procedure value occurrence plus capture shape, or finite-set adapter
ref with the full `ErasedAdapterKey { source_fn_ty, callable_set_key,
erased_fn_sig_key, capture_shape_key }`. No erased callable result may be
reified or promoted without explicit `Box(T)` provenance, and no erased callable
result may record only a code symbol, only an erased signature, only a finite
callable leaf, only `source_fn_ty + callable_set_key`, or only an executable
specialization key.

Promotion consumes a precomputed promotion plan:

```zig
const CallablePromotionPlan = struct {
    root: ComptimeRootId,
    source_fn_ty: CanonicalTypeKey,
    dependency_summary: ComptimeDependencySummaryId,
    result_plan: CallableResultPlanId,
};

const PromotedCallableNodeId = enum(u32) { _ };

const PromotedCallableNodeKey = struct {
    root: ComptimeRootId,
    graph_node: PromotedCallableGraphNodeId,
    path_provenance: PromotedCallablePathKey,
    source_fn_ty: CanonicalTypeKey,
};

const PromotedCallableGraphNodeId = enum(u32) { _ };

const PromotedCallableGraphNode = struct {
    id: PromotedCallableGraphNodeId,
    source_fn_ty: CanonicalTypeKey,
    result_plan: CallableResultPlanId,
    private_captures: Span(PrivateCaptureNodeId),
    recursive_edges: Span(PromotedCallableGraphNodeId),
    debug_path: PromotedCallablePathKey,
};

const PromotedCallablePathKey = union(enum) {
    root_result,
    finite_member_capture: struct {
        parent: PromotedCallableNodeId,
        member: CallableMemberId,
        capture_slot: u32,
        path: CapturePathKey,
    },
    erased_capture_callable: struct {
        parent: PromotedCallableNodeId,
        path: CapturePathKey,
    },
};
```

Every captured top-level constant must already have a published `ConstRef`, and
each capture occurrence must carry the concrete `ConstInstantiationKey` for its
resolved source type.
Every captured local value must be reified according to the precomputed
`CaptureSlotReificationPlan`. Serializable leaves become private capture
constants in `CompileTimeValueStore`. Callable leaves either reference existing
sealed procedure values or are recursively promoted to private procedure values
reserved in the same recursive promotion group. Structural containers become
private promoted-capture nodes that contain those leaves; they are not
source-visible top-level constants and they are not runtime closure
environments.

Every reachable callable node has a stable `PromotedCallableGraphNodeId` and
`PromotedCallableNodeKey` before any wrapper body is filled. The key is derived
from the compile-time root, the reserved graph-node identity, and the canonical
source function type. The capture path is debug provenance only; it is not the
canonical identity of the node. This distinction is mandatory because recursive
callable captures can revisit the same semantic callable node through a capture
edge. A path-only key can duplicate the node or attempt to build an infinite
path. It is not derived from generated symbol text, interpreter allocation
order, capture object memory addresses, store insertion order, LIR temporary
ids, or source syntax.

Callable-result plans and private capture reification plans are graph stores,
not trees. Checking finalization reserves every reachable
`PromotedCallableGraphNodeId`, `PromotedCallableNodeId`,
`PrivateCaptureNodeId`, `ProcedureValueRef`, `ProcBaseKeyRef`, and checked template
slot before descending into the captures of that node. If traversal reaches an
already reserved node, it records a `recursive_ref` edge instead of allocating a
second node. Recursive callable-capture groups must reserve the complete graph
before any member body is filled or sealed.

Promotion must not discover new top-level dependencies by looking through a
runtime capture object, by scanning source syntax, by reading interpreter memory
without a result plan, or by lowering an additional root after dependency
ordering has finished. If a required captured value is not already available,
the dependency graph or callable result plan is wrong and checking finalization
must hit the compiler invariant path: debug-only assertion in debug builds,
`unreachable` in release builds.

Callable promotion uses a reserve/fill/seal lifecycle:

1. Reify the callable root into compiler-owned `ComptimeCallable` graph nodes
   using `CallableResultPlan`.
2. Walk every reachable callable capture named by those nodes, reserving
   `PromotedCallableGraphNodeId` and `PrivateCaptureNodeId` values before
   descending into child captures.
3. Reserve one promoted `ProcedureValueRef`, one `ProcBaseKeyRef`, one
   `PromotedCallableNodeId`, and one checked template slot for every reachable
   callable node before lowering any promoted body. If traversal reaches an
   already reserved callable or private capture node, record `recursive_ref`
   instead of allocating a duplicate.
4. Reify every local capture graph into private promoted-capture data by
   following `CaptureSlotReificationPlan`; allocate private `ConstRef` template
   leaves only for serializable leaves, and carry concrete instantiation keys at
   every use.
5. Connect callable leaves to already-reserved checked procedure templates.
   Existing source/imported/hosted/platform-required procedure bindings remain
   those procedure templates; only genuinely promoted callable leaves point at
   reserved promoted procedure templates.
6. Preserve structural containers as compiler-owned private capture nodes; when
   a promoted body reads a field, element, tag payload, list element, boxed
   payload, or nominal payload, it reads through the explicit private capture
   node and obtains either a serializable `ConstRef` leaf plus concrete
   instantiation key or an explicit callable leaf instance.
7. Fill a `PromotedCallableWrapper` body for each reserved template slot. The
   wrapper body receives the ordinary source-level parameters of the promoted
   procedure and uses explicit private-capture operands for all captured data.
8. Seal each promoted procedure template by storing the filled wrapper in
   `CheckedProcedureTemplateTable`.
9. Publish the root binding's `procedure_binding` entry only after its promoted
   procedure value has a sealed checked procedure template.

Private capture graph materialization is deterministic:

- a `const_instance_leaf` materializes as the exact `ConstInstanceRef`; in pure
  mode the artifact must prove the referenced const instance has no reachable
  callable slots, and in general mode executable MIR must materialize the const
  instance through the full callable-aware constant materialization path before
  IR
- a finite `callable_leaf` materializes as
  `proc_value { template = leaf.proc_value.template, captures = [], fn_ty = leaf.proc_value.source_fn_ty }`
- an erased boxed callable leaf is forbidden in source private capture graphs;
  erased boxed callable leaves may materialize only from compile-time constant
  materialization graphs or from sealed
  `ErasedCaptureExecutableMaterializationPlan` graphs consumed by executable MIR
- a record materializes by assembling finalized field ids in logical field
  order while preserving the captured evaluation order recorded by the
  reification plan
- a tuple materializes by assembling elements in tuple index order
- a tag union materializes by constructing the exact finalized tag id and
  payload ids recorded for the node
- a list materializes by assembling elements in source element order; list
  element type does not introduce erasure
- a `Box(T)` materializes according to its explicit `BoxBoundaryId`; erasure is
  allowed only for the boxed payload slot named by that boundary
- a transparent alias materializes exactly as its underlying payload and emits
  no runtime wrapper
- a nominal/newtype materializes according to its recorded nominal
  representation capability around the already-materialized payload

If a promoted wrapper needs a whole private capture aggregate, the wrapper must
bind that aggregate as an explicit local value inside the promoted procedure
before passing or returning it. If the wrapper needs only leaves or projections,
the wrapper lowers those reads directly from the private capture graph. Both
cases consume `PrivateCaptureRef` handles; neither case creates a runtime
top-level global, runtime thunk, or runtime closure object.

The source private capture graph deliberately stops at a concrete
`ConstInstanceRef` leaf when the captured non-function value subtree already
requires executable-owned callable materialization. For example:

```roc
make_adder : I64 -> Box(I64 -> I64)
make_adder = |n| Box.box(|x| x + n)

make_runner : { f : Box(I64 -> I64), bonus : I64 } -> (I64 -> I64)
make_runner = |table| |x| Box.unbox(table.f)(x) + table.bonus

run : I64 -> I64
run = make_runner({ f: make_adder(41), bonus: 1 })
```

`run` is a finite promoted callable wrapper, so its wrapper body still lowers
through mono MIR. The captured record is source-level private capture data, but
the `f` field's boxed payload is erased only because of the explicit
`Box(I64 -> I64)` boundary. The source private capture graph therefore stores
the concrete `Box(I64 -> I64)` subtree as a private `ConstInstanceRef` leaf whose
constant materialization graph contains the erased boxed callable leaf and the
executable-ready capture materialization for `41`. Mono sees a const-instance
read for the field value, not an `ErasedCallableLeafInstance`. Executable MIR
resolves and materializes that const instance before IR. No runtime thunk,
runtime global initializer, runtime closure object, source-shape recovery, or
post-check erasure inference is allowed.

For example:

```roc
make_caller : { f : I64 -> I64 } -> (I64 -> I64)
make_caller = |r| |x| (r.f)(x)

add1 : I64 -> I64
add1 = make_caller({ f: |x| x + 1 })
```

`add1` is a callable-binding root. Its result is promoted to a closed ordinary
procedure. The captured record is a private promoted-capture record node. The
`f` field is a callable leaf that is recursively promoted to a private
procedure value. The sealed `add1` procedure reads the private record field and
gets an explicit `procedure_value`; it does not materialize a runtime top-level
closure object and does not publish `{ f: ... }` as an importable constant.

The same rule applies to tuples, tags, lists, boxes, transparent aliases, and
nominals that are captured by a promoted callable. A captured
`List(I64 -> I64)` is a private capture list whose elements are callable leaves;
`List(T)` is not an erasure
boundary. A captured `Box(I64 -> I64)` may introduce erased callable
representation only for the boxed payload slot named by an explicit
`BoxBoundaryId`; a captured `Box(I64)` does not introduce callable erasure, and
non-`Box(T)` containers never introduce erasure.

Public top-level constants use the same explicit constant graph representation
when their non-function source type contains callable leaves. For example:

```roc
table = { f: |x| x + 1 }
```

`table` is a normal compile-time constant whose `ConstRef` points at a record
node. The `f` field is a callable leaf that points at a sealed procedure value,
either an already-existing procedure value or a promoted closed procedure
value. Importing modules consume `table` through the exported `ConstRef`; they
must not re-run the defining module's LIR interpreter, call a hidden
zero-argument initializer, or inspect the defining module's source expression.

Callable-containing public constants may also contain captured data:

```roc
make : I64 -> { f : I64 -> I64 }
make = |n| { f: |x| x + n }

table = make(41)
```

Checking finalization evaluates `table`, reifies the record into
`CompileTimeValueStore`, promotes the callable leaf in `f` to a closed
procedure value, and stores `41` as compiler-owned constant capture data for
that promoted procedure. The record remains a normal constant record. The
implementation must not publish a runtime top-level closure object, runtime
global callable-value object, runtime initializer procedure, runtime thunk, or
partially serialized interpreter pointer for `table`.

Promotion is part of checking finalization. If promotion cannot produce a closed
procedure from a top-level function-valued binding, checking has not completed.
After publication, any missing promoted procedure, remaining capture
environment, top-level closure object, runtime callable object, or top-level
callable initializer is a compiler invariant violation handled only by
debug-only assertion in debug builds and `unreachable` in release builds.

The compile-time value store represents logical constant graphs: ints,
fractions, strings, lists, boxes, tuples, records, tag unions, transparent
aliases, nominals, and callable leaves. A root source type that is itself a
function still publishes as `procedure_binding`, not as a `ConstRef`; callable
top-level results use callable promotion and then publish as `procedure_binding`.
Callable leaves nested under a non-function root stay inside that root's
constant graph and are represented by explicit procedure-value references.

Reification must copy runtime interpreter results into compiler-owned constant
nodes. Raw runtime addresses, Roc heap pointers, interpreter arena pointers, and
refcount headers must not survive reification. Lists, strings, boxes, records,
tuples, tags, transparent aliases, nominals, and callable leaves become logical
constant nodes addressed by `(schema_id, value_id)`.

The reification plan is built from the resolved source type, the selected
layout, and sealed callable representation data. All are required:

- the source type gives logical names, wrappers, aliases, nominals, record
  fields, tag names, and payload arity
- the layout gives byte interpretation for the LIR interpreter result
- callable representation data gives callable leaf identity, callable-set
  members, erased ABI keys, and explicit `BoxBoundaryId` provenance

Reification-plan construction must not infer logical row structure or callable
identity from runtime bytes or physical layout order. Physical layout order may
be used only to read bytes after the logical plan has identified the value being
read.

Compile-time evaluation uses the same LIR interpreter reference-counting path as
ordinary interpreter execution. The interpreter executes explicit LIR `incref`,
`decref`, and `free` statements. Compile-time `RocOps` may allocate temporary
Roc heap data during evaluation, but reification must detach the result from
that temporary allocation domain.

Compile-time evaluation problems are checking problems because this stage is
part of checking finalization. A user-written compile-time crash, failed
`expect`, division by zero, numeric conversion failure, or invalid constant
schema must be reported before `TypeCheckOutput` or equivalent checked artifact
data is returned. If any of those conditions reaches a post-check stage, that
is a compiler invariant violation handled only by debug-only assertion in debug
builds and `unreachable` in release builds.

Cached modules must store and load the serialized `CompileTimeValueStore` as
part of the complete checked module artifact, along with any sealed
`ConstInstantiationStore`, `CallableBindingInstantiationStore`, and
`SemanticInstantiationProcedureTable` entries owned by that artifact. A
downstream module consumes imported constant bindings and exported callable
binding templates from that artifact data. It must not re-run the imported
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
alignment choices, or reference-counting materialization plans must be produced
by a target-specific post-check cache or by normal lowering for that target.

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
    checked_types: CheckedTypeStore,
    checked_bodies: CheckedBodyStore,
    provides_requires: ProvidesRequiresMetadata,
    method_registry: MethodRegistry,
    static_dispatch_plans: StaticDispatchPlanTable,
    resolved_value_refs: ResolvedValueRefTable,
    procedure_templates: CheckedProcedureTemplateTable,
    root_requests: RootRequestTable,
    hosted_procs: HostedProcTable,
    platform_required_declarations: PlatformRequiredDeclarationTable,
    platform_required_bindings: PlatformRequiredBindingTable,
    interface_capabilities: ModuleInterfaceCapabilities,
    top_level_values: TopLevelValueTable,
    promoted_procedures: PromotedProcedureTable,
    compile_time_roots: ?ComptimeRootTable,
    comptime_values: CompileTimeValueStore,
    const_instances: ConstInstantiationStore,
    callable_binding_instances: CallableBindingInstantiationStore,
    semantic_instantiation_procedures: SemanticInstantiationProcedureTable,
};
```

The exact Zig names may differ, but the completeness rule must not. The checked
artifact is the unit imported by downstream modules and consumed by public
post-check lowering APIs. Later stages may consume narrowed views of the
artifact, but those views must be derived from this artifact and must not scan
or mutate raw checked modules to rebuild missing semantic data.

Checking finalization order:

1. Finish type solving and collect all user-facing diagnostics.
2. Copy every lowering-visible checked type root and checked body into
   artifact-owned `CheckedTypeStore` and `CheckedBodyStore` records. This copy is
   where raw `types.Var`, raw `CIR.Expr.Idx`, raw `CIR.Pattern.Idx`, and
   `Ident.Idx` handles are converted to checked type ids, checked body ids, and
   canonical names. The copied stores must include direct source bodies, hosted
   wrappers, intrinsic wrappers, entry wrappers, compile-time root bodies,
   callable-eval template bodies, constant-eval template bodies, nested
   procedure-site bodies, static-dispatch type roots, resolved-value-reference
   type roots, and all public/exported type schemes.
3. Build the checked method registry, normalized static dispatch plans, and the
   builder-form resolved value-reference table. These records must point at the
   artifact-owned checked type/body stores, not raw checker vars or raw checked
   expression ids.
4. Build checked procedure templates for direct source procedures, hosted
   wrappers, intrinsic wrappers, entry wrappers, and any other compiler-created
   checked procedure bodies whose bodies do not depend on compile-time callable
   results. Promoted callable templates are not built here because their bodies
   depend on evaluated callable roots. Each template stores a `CheckedBodyId`,
   `CheckedTypeId`, and `CanonicalTypeSchemeKey`, not a raw source expression or
   checker var.
5. Build root requests for concrete runtime, tool, test, REPL, development, and
   compile-time entrypoints. Generic exports are not root requests merely because
   they are exported.
6. Build hosted procedure tables and platform-required declaration tables.
   Platform-required binding tables are built only for an executable
   app/platform co-finalization group, after the app's top-level values and
   callable promotions have been sealed. Standalone platform checking and glue
   contexts may publish platform-required declarations without app-specific
   bindings, but executable lowering may not consume such an artifact.
7. Build public exports, provides/requires metadata, and interface capability
   records.
8. Reserve `ConstRef` identities for every non-function top-level constant,
   allocate direct procedure values, create `TopLevelProcedureBindingRef` rows for
   every direct top-level function declaration and already-procedure top-level
   lambda, and initialize the in-progress `TopLevelValueTable` with
   `const_template`, `procedure_binding`, or `pending_callable_root` entries.
9. Build the callable-aware summary-only lowering records and the
   `CompileTimeRootDependencyGraph` for compile-time constants, callable
   binding roots, and expect roots.
10. Evaluate concrete compile-time constants and concrete compile-time callable
   roots through the MIR-family path and LIR interpreter in dependency order.
   For generalized compile-time callable roots that require a concrete requested
   function type before they can be evaluated, build and seal
   `CallableEvalTemplate` records instead of attempting to produce generalized
   executable MIR or a generalized interpreter value.
11. Fill each evaluated non-function compile-time constant into the
   already-reserved `ConstRef`. The filled template is either a
   `ConstValueGraphTemplate` or a `ConstEvalTemplate`. The template may contain
   callable leaf templates; those leaves must reference sealed checked, lifted,
   or synthetic callable procedure templates before the artifact is published. If
   the constant source type is generic, including a generic non-function value
   that contains function-typed fields, the `ConstRef` stores the canonical
   source type scheme and the target-independent template. Concrete instances are
   created through `ConstInstantiationRequest` in the requesting artifact's
   `ConstInstantiationStore` during that artifact's checking finalization; the
   request key identifies the instance and the request payload provides the
   concrete source type graph. Checking finalization must not reject or thunk the
   constant merely because it contains generalized callable leaves.
12. Promote concrete compile-time callable results with a reserve/fill/seal
    lifecycle:
    reserve every promoted `ProcedureValueRef`, `ProcBaseKeyRef`,
    `PromotedCallableNodeId`, and checked template slot; build private capture
    graphs; fill every `PromotedCallableWrapper`; append the sealed promoted
    templates to `CheckedProcedureTemplateTable`; then insert each
    `PromotedProcedure` row. Generalized callable eval templates are promoted
    only when a concrete `CallableBindingInstantiationRequest` is requested while
    building the artifact that owns that concrete instance.
13. Replace promoted root `pending_callable_root` entries in the in-progress
    `TopLevelValueTable` with `procedure_binding` entries only after their
    promoted procedure values have sealed checked procedure templates.
14. If this module is the app root of an executable app/platform group, use the
    sealed app `TopLevelValueTable`, `CompileTimeValueStore`,
    `ConstInstantiationStore`, `CallableBindingInstantiationStore`,
    `SemanticInstantiationProcedureTable`, and promoted procedure table to build
    the app-specific `PlatformRequiredBindingTable` for the platform root
    artifact. This happens before either root artifact is published. A
    platform-required binding for a non-function value stores a
    `PlatformRequiredValueUse.const_value` that points at the app `ConstRef` and
    exact requested source type. A binding for a function value stores a
    `PlatformRequiredValueUse.procedure_value` that points at the sealed app
    `TopLevelProcedureBindingRef` or concrete callable-binding instance required
    by the platform's requested function type. No binding may be created from a
    raw app name, pattern lookup, generated wrapper name, or unsealed
    `pending_callable_root`.
15. Verify that `TopLevelValueTable` has no `pending_callable_root` entries and
    that every referenced top-level binding maps to either a `ConstRef`-backed
    constant template or `procedure_binding`.
16. Seal the resolved value-reference table by replacing every builder-only
    top-level binding reference with `ConstUseTemplate` or `ProcedureUseTemplate`
    data from the completed `TopLevelValueTable` and imported artifact views.
17. Build exported procedure-template, procedure-binding, and const-template
    views. For every exported generic template, build the deterministic
    `ImportedTemplateClosureView` containing the checked bodies, checked type
    roots/schemes, private checked procedure templates, resolved value-reference
    spans, static-dispatch plan spans, nested procedure-site spans, method
    registry entries, private capture nodes, and compile-time template records
    required to instantiate that export without inspecting source or exporter
    `ModuleEnv`.
18. Store the complete checked type store, checked body store, checked procedure
    template table, sealed resolved value-reference table,
    `CompileTimeValueStore`, `ConstInstantiationStore`,
    `CallableBindingInstantiationStore`, `SemanticInstantiationProcedureTable`,
    promoted procedure table, exported views, imported template closures, and
    top-level value table in the artifact.
19. Run debug-only artifact verification. This verification must assert that
    every exported or imported `CanonicalTypeKey` and `CanonicalTypeSchemeKey`
    has a checked type payload in the artifact or imported closure; every checked
    procedure template body points at a sealed checked body; no checked procedure
    template body is a raw `CIR.Expr.Idx`; every checked body node points at a
    sealed checked type id; every imported closure lists all private checked
    bodies and checked type roots reachable from the exported template;
    every published `ConstRef` has a complete constant template and source type
    scheme; every callable leaf template points at a sealed
    `CallableProcedureTemplateRef`; every
    `MonoSpecializationKey` names a checked template, not a mono output
    procedure; no checked artifact stores a concrete `ConstInstantiationKey`
    where a `ConstUseTemplate` is required; no checked artifact stores a concrete
    `CallableBindingInstantiationKey` where a `ProcedureUseTemplate` is required;
    every semantic-instantiation procedure points at a sealed checked procedure
    template owned by the same artifact; every concrete const/callable instance
    that lists generated procedures has those procedures in the artifact's
    `SemanticInstantiationProcedureTable`;
    and no generalized type or callable variable appears in an executable-stage
    input without an explicit concrete instantiation key.
20. Publish the immutable checked artifact.

If any user-facing problem is found in steps 1 through 15, checking reports it
and no artifact is published for that module. Later compiler stages never see a
partial checked artifact.

After publication:

- `ModuleEnv` identity must not be patched.
- imported generic specialization must not inspect another artifact's
  `ModuleEnv`, checker type store, raw checked expression ids, or private
  definitions to recover a missing checked body or checked type payload.
- hosted indices must not be assigned by mutating checked CIR.
- platform-required lookup targets must not be populated by mutating checked
  modules.
- roots must not be discovered by scanning exports, declarations, or expression
  shapes.
- compile-time values must not be produced by re-running imported module LIR
  roots.
- concrete constant instances, callable binding instances, semantic
  instantiation procedures, promoted procedures, and private capture graphs must
  not be created by post-check lowering.
- interface capabilities must not be recreated from imported module bodies.

Missing artifact components after publication are compiler invariant
violations. Debug builds must use debug-only assertions that fail immediately.
Release builds use `unreachable` for the equivalent compiler-invariant path.

### Compile-Time Constant Consumption

The compile-time value store is not merely a cache payload. It is the
post-check input for imported and local compile-time constants, including
constant graphs with callable leaves, and it owns the artifact-private capture
graphs needed by promoted procedures.

Checking finalization classifies each top-level binding that can be referenced
after checking as one of:

```zig
const TopLevelValueKind = union(enum) {
    const_template: ConstRef,
    procedure_binding: TopLevelProcedureBindingRef,
};

const TopLevelValueEntry = struct {
    module: ModuleId,
    pattern: PatternId,
    source_scheme: CanonicalTypeSchemeKey,
    value: TopLevelValueKind,
};

const PromotedProcedure = struct {
    proc: ProcedureValueRef,
    proc_base_key: ProcBaseKeyRef,
    template: CheckedProcedureTemplateId,
    callable_node: PromotedCallableNodeId,
    source_binding: PatternId,
    source_fn_ty: CanonicalTypeKey,
    provenance: PromotedProcedureProvenance,
};
```

`const_template` is for a top-level compile-time constant whose non-function
source type has a reserved and sealed `ConstRef`. The referenced `ConstRef` may
identify a value graph template or an eval template. Either form may contain
callable leaf templates, as long as every callable leaf template points at a
sealed checked, lifted, or synthetic callable procedure template.

`procedure_binding` is for function declarations and function-valued
declarations. Those do not become runtime zero-argument thunks and do not become
`ConstRef` graphs. A root function value is always published as a
`TopLevelProcedureBindingRef`. The binding may point at the original top-level
procedure value, an existing imported/source procedure binding, or a closed
procedure value produced by compile-time callable promotion. If the binding is
generic and its callable result must be computed at a concrete requested function
type, the binding may instead point at a sealed `CallableEvalTemplate`. Concrete
uses instantiate it through `CallableBindingInstantiationRequest`, which carries
both the stable `CallableBindingInstantiationKey` and the concrete source
function type payload. There is no post-check top-level closure-value category:
any concrete top-level callable result must be promoted before it can be
consumed as a concrete procedure value.

`TopLevelValueTable` is the only post-check lookup table for top-level values.
Mono MIR, executable MIR, eval, REPL, tests, glue, and CLI helpers must consume
this table. They must not classify top-level values by scanning source
declarations, checking whether an expression is syntactically a lambda, looking
for generated symbol-name patterns, or re-running constant evaluation.

`PromotedProcedureTable` records procedure values created from compile-time
callable roots. The promoted procedures live in the same procedure namespace as
ordinary top-level functions after publication. The separate table exists for
debug provenance, artifact verification, and deterministic serialization; it is
not a runtime closure environment and is not a second callable representation.
Every row must point at a sealed checked procedure template in
`CheckedProcedureTemplateTable` and must carry the stable `ProcBaseKey` used for
later specialization, ordering, cache keys, and debug provenance. A promoted
procedure value may not appear in `TopLevelValueTable`, `ResolvedValueRef`, a
private capture `callable_leaf`, or an imported artifact view unless its
`PromotedProcedure` row and checked template are already present in the same
published artifact.

Concrete instantiation can create procedure templates. For example, instantiating
`table = { f: |x| x }` at `{ f : I64 -> I64 }` may need a sealed synthetic
checked procedure template for the concrete `I64` callable leaf. Instantiating
the same source constant at `{ f : Str -> Str }` may need a different concrete
template. These procedure templates are semantic checked-artifact data, not
post-check lowering products.

Every checked artifact therefore owns a semantic-instantiation procedure table:

```zig
const SemanticInstantiationProcedureRef = struct {
    artifact: CheckedModuleArtifactKey,
    key: SemanticInstantiationProcedureKey,
};

const SemanticInstantiationProcedureKey = union(enum) {
    const_instance_callable_leaf: struct {
        instance: ConstInstantiationKey,
        value_path: ComptimeValuePathKey,
        source_fn_ty: CanonicalTypeKey,
    },
    callable_binding_promoted_leaf: struct {
        instance: CallableBindingInstantiationKey,
        callable_path: PromotedCallablePathKey,
        source_fn_ty: CanonicalTypeKey,
    },
    private_capture_callable_leaf: struct {
        promoted_proc: PromotedProcedureRef,
        capture_path: PrivateCapturePathKey,
        source_fn_ty: CanonicalTypeKey,
    },
};

const SemanticInstantiationProcedure = struct {
    template: CallableProcedureTemplateRef,
    proc_value: ProcedureValueRef,
    promoted: ?PromotedProcedureRef,
};

const SemanticInstantiationProcedureTable = struct {
    owner: CheckedModuleArtifactKey,
    procedures: Map(SemanticInstantiationProcedureKey, SemanticInstantiationProcedure),
};
```

The exact Zig names may differ, but the ownership rule must not. Any procedure
template created while instantiating a `ConstEvalTemplate`,
`ConstValueGraphTemplate`, or `CallableEvalTemplate` is inserted into the
requesting artifact's `SemanticInstantiationProcedureTable` before that artifact
is published. If the procedure is promoted, its `PromotedProcedureTable` row and
checked template must also be sealed before publication. Later stages consume the
sealed `CallableProcedureTemplateRef` or `ProcedureValueRef`; they must not
allocate a new checked procedure template while lowering, materializing, or
running backend code.

The `ConstInstantiationKey` and `CallableBindingInstantiationKey` fields inside
`SemanticInstantiationProcedureKey` refer only to already-reserved and sealed
instances. Creating those instances required the corresponding
`ConstInstantiationRequest` or `CallableBindingInstantiationRequest` with a
`ConcreteSourceTypeRef` payload. The semantic-instantiation procedure table must
not be used as a backdoor to construct a new instance from a key alone.

Conceptual constant handle:

```zig
const ConstOwner = union(enum) {
    top_level_binding: struct {
        module: ModuleId,
        pattern: PatternId,
    },
    promoted_capture: PromotedCaptureId,
};

const PromotedCaptureId = struct {
    promoted_proc: ProcedureValueRef,
    capture_index: u32,
};

const ConstRef = struct {
    artifact: CheckedModuleArtifactKey,
    owner: ConstOwner,
    template: ConstTemplateId,
    source_scheme: CanonicalTypeSchemeKey,
};

const ConstTemplate = union(enum) {
    eval_template: ConstEvalTemplateId,
    value_graph_template: ConstValueGraphTemplateId,
};

const ConstEvalTemplate = struct {
    body: CheckedConstBodyRef,
    source_scheme: CanonicalTypeSchemeKey,
    resolved_value_refs: ResolvedValueRefTableRef,
    static_dispatch_plans: StaticDispatchPlanTableRef,
    reification_template: ConstGraphReificationTemplateId,
    dependency_template: ComptimeDependencySummaryTemplateRef,
};

const ConstValueGraphTemplate = struct {
    source_scheme: CanonicalTypeSchemeKey,
    root: ComptimeValueTemplateId,
};

const ConstInstantiationKey = struct {
    const_ref: ConstRef,
    requested_source_ty: CanonicalTypeKey,
};

const ConstInstantiationRequest = struct {
    key: ConstInstantiationKey,
    requested_source_ty_payload: ConcreteSourceTypeRef,
};

const ConstInstantiationStoreRef = struct {
    owner: CheckedModuleArtifactKey,
};

const ConstInstanceRef = struct {
    store: ConstInstantiationStoreRef,
    key: ConstInstantiationKey,
    instance: ConstInstanceId,
};

const ConstInstantiationStore = struct {
    owner: CheckedModuleArtifactKey,
    instances: Map(ConstInstantiationKey, ConstInstantiationState),
};

const ConstInstantiationState = union(enum) {
    reserved,
    evaluating,
    evaluated: ConstInstance,
};

const ConstInstance = struct {
    schema: ComptimeSchemaId,
    value: ComptimeValueId,
    dependency_summary: ?ComptimeDependencySummaryId,
    reification_plan: ?ConstGraphReificationPlanId,
    generated_procedures: Span(SemanticInstantiationProcedureRef),
};
```

The `artifact` field identifies the checked artifact that owns the value store.
The `owner` field identifies whether the constant is the value of a source
top-level binding or private constant data captured by a promoted callable.
`template` identifies the target-independent constant template inside that store.
`source_scheme` records the canonical checked source type scheme for that
template and may contain generalized variables.

`ConstTemplate.eval_template` is for a constant whose concrete value must be
computed by evaluating a checked expression template at the requested source
type. `ConstTemplate.value_graph_template` is for a constant whose logical value
graph has already been structurally reified in target-independent form. Both
template forms are checked-artifact data. Neither is executable MIR, target
bytes, a runtime initializer, or a request for later stages to inspect source
syntax.

`ConstInstantiationKey` selects the identity of one concrete use of the template
at a fully resolved source type. `ConstInstantiationRequest` is the construction
input for that use and must carry `requested_source_ty_payload`, a
`ConcreteSourceTypeRef` whose canonical key exactly equals
`key.requested_source_ty`. A generic procedure body, generic constant template,
or imported module view may refer to a `ConstUseTemplate`; it may not require a
`ConstInstantiationRequest` until the use has been clone-instantiated in a
concrete lowering context.

`ConstInstanceRef` identifies a concrete instantiated logical node owned by a
`ConstInstantiationStore`. The store owner is the checked artifact that requested
the concrete instance, not necessarily the artifact that owns the `ConstRef`.
This distinction is mandatory for imported generic constants: an importing
module may instantiate an exported `ConstRef` at a concrete type while building
the importing checked artifact, but it must not mutate the exporting artifact's
value store. Later stages must not reconstruct schemas from bytes, layout order,
expression syntax, or display names.

`ConstInstantiationStore` has a reserve/fill/seal lifecycle like procedure
specialization. When a concrete `ConstInstantiationRequest` is requested,
checking finalization reserves the `ConstInstanceRef` before evaluating any
dependencies. If the template is a value graph template, instantiation clones the
graph through `requested_source_ty_payload`, instantiates callable leaves, and
fills the instance. If the template is an eval template, instantiation lowers the
checked expression template through the MIR-family path at
`requested_source_ty_payload`, runs the LIR interpreter, and reifies the result
through the recorded reification template.
For an eval template, the concrete instance stores the concrete dependency
summary and concrete reification plan used for that requested type. If
instantiating the constant creates synthetic checked procedure templates,
promoted procedures, or private capture data for callable leaves, those records
are sealed in the requesting artifact and listed from the concrete
`ConstInstance`. The concrete instance is therefore the complete semantic product
of the request, not a promise for post-check lowering to finish.
Recursive instance requests reuse the reserved instance. Cycles that are invalid
under Roc's top-level constant rules are reported before artifact publication;
cycles that reach post-check lowering are compiler invariant violations handled
by debug-only assertion in debug builds and `unreachable` in release builds.

A published checked artifact may contain sealed local instances in its own
`ConstInstantiationStore`. A consuming module may create additional concrete
instances for imported generic `ConstRef` values only while building and
publishing the consuming checked artifact. No stage may write those instances
into the imported artifact, re-run the imported module's root evaluation, create
them during post-check lowering, or use a target-specific materialization cache
as the logical instance store.

`top_level_binding` owners are source-visible compile-time constants and may
appear in `TopLevelValueTable`. Their template graph may contain callable leaf
templates. A callable leaf template is part of the explicit constant graph, not a
request for later stages to recover a function value from syntax, runtime
closure memory, or interpreter state. `promoted_capture` owners are
artifact-private leaves used only by promoted procedures. They are stored in the
same `CompileTimeValueStore` as other constants, but they are not exported,
imported by name, or classified through `TopLevelValueTable`.

The compile-time value store must have an explicit node kind for callable
leaves. Conceptually:

```zig
const ComptimeValueNode = union(enum) {
    scalar: ComptimeScalarValue,
    string: ComptimeStringValue,
    list: Span(ComptimeValueId),
    box: ComptimeBoxValue,
    tuple: Span(ComptimeValueId),
    record: Span(ComptimeRecordFieldValue),
    tag_union: ComptimeTagValue,
    transparent_alias: ComptimeAliasValue,
    nominal: ComptimeNominalValue,
    callable_leaf_template: CallableLeafTemplate,
};

const CallableLeafTemplate = union(enum) {
    finite: FiniteCallableLeafTemplate,
    erased_boxed_callable: ErasedCallableTemplate,
};

const CallableLeafInstance = union(enum) {
    finite: FiniteCallableLeafInstance,
    erased_boxed_callable: ErasedCallableLeafInstance,
};

const FiniteCallableLeafTemplate = struct {
    proc_template: CallableProcedureTemplateRef,
    source_fn_ty_template: CanonicalTypeTemplateKey,
};

const ProcedureCallableRef = struct {
    template: CallableProcedureTemplateRef,
    source_fn_ty: CanonicalTypeKey,
};

const FiniteCallableLeafInstance = struct {
    proc_value: ProcedureCallableRef,
};

const ProcedureValueRef = struct {
    proc_base: ProcBaseKeyRef,
};

const ErasedCallableTemplate = struct {
    sig_template: ErasedFnSigTemplateKey,
    code_template: ErasedCallableCodeTemplateRef,
    capture_template: ErasedCaptureTemplateRef,
    provenance: NonEmptySpan(BoxBoundaryId),
};

const ErasedCallableLeafInstance = struct {
    source_fn_ty: CanonicalTypeKey,
    sig_key: ErasedFnSigKey,
    code: ErasedCallableCodeRef,
    capture: ErasedCaptureExecutableMaterializationPlan,
    provenance: NonEmptySpan(BoxBoundaryId),
};
```

An `ErasedCallableTemplate` is a pre-interpretation recipe.
`ErasedCallableLeafInstance` is post-interpretation, post-reification data and
must be executable-ready. Its `capture` field is therefore an
`ErasedCaptureExecutableMaterializationPlan`, not an
`ErasedCaptureReificationPlan`, not a `CaptureSlotReificationPlanId`, not a
`CallableResultPlanId`, and not a pointer into the interpreter's returned
closure bytes. The physical hidden capture exists only while reifying the LIR
interpreter result. Reification must consume that physical hidden capture
immediately and store the complete executable materialization graph in the
checked artifact. Later MIR, IR, LIR, ARC, backend, interpreter, and imported
artifact consumers must not rerun the interpreter or recover erased captures
from runtime packed-function bytes.

The exact Zig names may differ, but the contract must not. A finite callable
leaf template is exactly a target-independent `proc_value` template with an
empty capture list. Its `proc_template` field stores the callable procedure
template identity, including whether the procedure is checked, lifted, or
synthetic. Its `source_fn_ty_template` field stores the source function type
template for this callable occurrence; in a generic constant this may contain
generalized variables from the constant's `source_scheme`.

A generalized value graph template may store `CallableProcedureTemplateRef.lifted`
only if the `LiftedProcedureTemplateRef.owner_mono_specialization` already names
a real concrete owner mono specialization. It must not store a placeholder lifted
template for a lambda or local function that will be lifted only after the
generic constant is instantiated. If a callable leaf's identity depends on a
future owner mono specialization, the enclosing constant must be a
`ConstEvalTemplate`, or the callable must first be promoted into a sealed
synthetic checked template whose identity is independent of a future lifted owner.
This rule preserves Cor/LSS's sequencing: concrete monotype specialization first,
then lifting inside that specialization, then callable representation solving.

A concrete finite callable leaf instance stores exactly the instantiated
procedure template and the exact canonical resolved fixed-arity source/callable
function type for that occurrence. This is the same semantic payload carried by
`proc_value.fn_ty`: it selects the requested procedure instantiation and records
the argument types, return type, and any resolved constraint/capability data that
belongs to the callable function type. It is not an executable representation
key, callable-set key, erased code key, or layout key.

A concrete finite callable leaf instance must store exactly those two semantic
fields:

```text
finite callable leaf instance = proc_value(template = leaf.proc_value.template, captures = [], fn_ty = leaf.proc_value.source_fn_ty)
```

It must not store `CanonicalCallableSetKey`, `CallableMemberId`,
`MonoSpecializationKey`, `ExecutableSpecializationKey`, `CaptureShapeKey`,
layout id, generated symbol text, expression id, runtime function pointer,
runtime capture pointer, or backend ABI handle. `CanonicalCallableSetKey` and
`CallableMemberId` are produced later when lambda-solved/executable MIR
materializes the leaf as an ordinary `proc_value` occurrence and solves its
callable representation. `MonoSpecializationKey` is derived from the leaf
instance's callable procedure template and `source_fn_ty` when the appropriate
procedure body has been reserved. For checked templates this is an ordinary mono
specialization request. For lifted templates the owning mono specialization and
nested procedure site already identify the lifted body. For synthetic templates
the compiler-created origin payload identifies the generated body.
`ExecutableSpecializationKey` is
target/representation-specific and must be produced only after callable
representation and executable value types are known.

If the evaluated callable had any local or serializable captures, checking
finalization must first promote it to a closed promoted procedure, seal that
procedure's compiler-created template, allocate a `PromotedProcedure` row, and
then store a finite callable leaf template whose `proc_template` is
`CallableProcedureTemplateRef.synthetic` for the promoted procedure and whose
instantiated `source_fn_ty` is the promoted callable's exact source function
type. A finite callable leaf itself never stores captures. If lowering a finite
callable leaf would require a capture payload, the promotion/reification record
is wrong and checking finalization must hit the compiler invariant path.

For example:

```roc
id : a -> a
id = |x| x

int_table : { f : I64 -> I64 }
int_table = { f: id }

str_table : { f : Str -> Str }
str_table = { f: id }

table = { f: id }
```

`int_table` and `str_table` are concrete constants, so their concrete constant
instances contain distinct finite callable leaf instances:

```zig
.{
    .proc_value = .{
        .template = CallableProcedureTemplateRef.checked(template_ref_of_source_proc(id)),
        .source_fn_ty = canonical("I64 -> I64"),
    },
}
.{
    .proc_value = .{
        .template = CallableProcedureTemplateRef.checked(template_ref_of_source_proc(id)),
        .source_fn_ty = canonical("Str -> Str"),
    },
}
```

The unannotated `table` constant publishes one `ConstRef` template whose callable
leaf template stores `CallableProcedureTemplateRef.checked` for `id` plus a
source function type template. Each use of `table` must supply a
`ConstInstantiationRequest` with a fully resolved requested record type payload
before executable MIR or constant materialization can consume it. The key inside
that request is the stable identity of the use; the payload is what instantiates
the callable leaf template.

A bare `ProcedureValueRef { .proc_base = ... }` is insufficient as a finite
callable leaf and is forbidden because it loses the occurrence-specific function
type that chooses the correct instantiation. If the target procedure is generic,
the instantiated `source_fn_ty` is what makes the leaf unambiguous. If the
instantiated `source_fn_ty` is not a function type, if its arity does not match
the procedure target, or if the procedure target cannot be instantiated at that
type, checking finalization or constant instantiation has emitted an invalid
artifact; debug builds assert immediately and release builds use `unreachable`.

If the leaf names a promoted procedure, that promoted procedure's checked
template and `PromotedProcedure` row must already exist in the same published
artifact. If the leaf names an imported procedure, the imported artifact view
must expose the procedure and its checked template/capability record. An erased
boxed callable leaf stores the exact erased function signature, erased code ref,
capture materialization, and non-empty `BoxBoundaryId` provenance. The exact
erased ABI is inside `ErasedFnSigKey`; it must not be duplicated as a second
field. Later stages instantiate callable leaf templates into ordinary
`proc_value` or explicit erased callable values and must not rediscover their
targets from source syntax, layout shape, runtime bytes, generated symbol text,
or field/tag names.

`PrivateCaptureRef` is still needed for source-level promoted-procedure capture
data because private capture graphs are not source-visible top-level constants:

```zig
const PrivateCaptureRef = struct {
    artifact: CheckedModuleArtifactKey,
    owner: PromotedCaptureId,
    node: PrivateCaptureNodeId,
    source_scheme: CanonicalTypeSchemeKey,
};

const PrivateCaptureInstantiationKey = struct {
    capture_ref: PrivateCaptureRef,
    requested_source_ty: CanonicalTypeKey,
};

const PrivateCaptureNode = union(enum) {
    const_instance_leaf: PrivateCaptureConstLeaf,
    finite_callable_leaf: FiniteCallableLeafInstance,
    record: Span(PrivateCaptureField),
    tuple: Span(PrivateCaptureNodeId),
    tag_union: PrivateCaptureTagNode,
    list: Span(PrivateCaptureNodeId),
    box: PrivateCaptureBoxNode,
    nominal: PrivateCaptureNominalNode,
    recursive_ref: PrivateCaptureNodeId,
};

const PrivateCaptureConstLeaf = struct {
    const_ref: ConstRef,
    const_instance: ConstInstanceRef,
    requested_source_ty: CanonicalTypeKey,
    schema: ComptimeSchemaId,
    mode: PrivateCaptureConstMode,
};

const PrivateCaptureConstMode = enum {
    pure_no_callable_slots,
    general_may_contain_callable_slots,
};
```

Top-level constants and source-level private capture graphs have different
callable contracts. A `top_level_binding` constant is source-visible and
importable through its `ConstRef`; its constant graph may contain finite callable
leaves and erased boxed callable leaves because executable MIR materializes
concrete const instances through target-specific materialization plans before
IR. A `PrivateCaptureRef` is reachable only from promoted procedure bodies that
still pass through the normal MIR-family stages; its source private graph may
contain only finite callable leaf templates and concrete const-instance leaves.
Later stages must consume the explicit constant nodes or private capture nodes
and must not rediscover callable leaves from source syntax, runtime values,
field names, layout order, or expression shape.

Sealed erased promoted wrappers are not consumers of `PrivateCaptureRef`.
Checking finalization must recursively convert any private capture graph needed
by such a wrapper into `ErasedCaptureExecutableMaterializationPlan` before the
wrapper is published. After publication, executable MIR consumes only the stable
materialization records, the expected executable type payload for the capture,
and the callable-set descriptor store.

Private captures follow the same template/instance rule as public constants.
The promoted procedure's `MonoSpecializationKey` supplies the concrete source
types needed to instantiate any generic private capture template. No generalized
private capture node may reach executable MIR. If a promoted procedure body needs
a private capture at a concrete type, it must reference the corresponding
`PrivateCaptureInstantiationKey` or a concrete instance derived from it; it must
not inspect the promoted callable expression again.

Each finite callable leaf instance must reference a sealed checked procedure
template and the exact canonical function type for that occurrence. When the
template names a promoted procedure, the artifact must contain a
`PromotedProcedure` row for that procedure value, and that row must point at a
sealed `CheckedProcedureTemplate`. Existing source procedures, imported
procedures, hosted procedures, and platform-required procedure bindings are valid
callable leaves when their procedure templates or capability records are already
published. A private capture graph cannot contain a callable leaf that is only a symbol
reservation, and it cannot contain a bare procedure value without
`source_fn_ty`. This is what allows promoted wrapper lowering to materialize
callable leaves as ordinary `proc_value` values without consulting the
compile-time callable graph, interpreter results, or source syntax again.

If private capture construction reaches a function-typed capture, it must
recursively promote that callable and store a finite callable leaf template. If
it reaches a non-function subtree whose concrete materialization contains erased
boxed callable slots, it must store a `const_instance_leaf` for that subtree
instead of embedding the erased callable in the private capture graph.
`pure_no_callable_slots` may be used only when debug verification can prove the
referenced const instance contains no reachable callable slots. Otherwise the
leaf must use `general_may_contain_callable_slots`, and executable MIR must use
the full callable-aware const materialization path. A private capture graph must
never contain `ErasedCallableLeafInstance`, `ErasedFnSigKey`,
`ErasedCallableCodeRef`, `ErasedCaptureExecutableMaterializationPlan`,
`CallableResultPlanId`, interpreter addresses, runtime packed-function bytes, or
any request to re-run compile-time evaluation.

Private capture structural nodes for source-level promoted bodies are owned by
the checked artifact, but their row and tag identities must become explicit
before those bodies reach executable MIR. The row-finalized mono MIR pass assigns
`RecordShapeId`, `RecordFieldId`, `TagUnionShapeId`, `TagId`, and `TagPayloadId`
to those private capture materializers using the same canonical row-finalization
tables used for source aggregates. After that point, source-level private
capture projections and aggregate materializers carry finalized ids and no later
stage may look up fields or tags by source names.

Sealed erased promoted wrapper captures do not pass through that row-finalized
body pipeline. Their persisted `ErasedCaptureExecutableMaterializationPlan`
therefore must not contain row-finalized ids. It stores stable canonical
`RecordFieldLabelId`, `TagLabelId`, and logical payload indexes instead.
Executable MIR maps those stable structural entries to local row-finalized ids
from the expected executable capture type payload in the current lowering run,
with debug-only assertions that the stable entries and expected type are exactly
consistent.

Before executable MIR assigns procedure ids, it must reserve every synthetic
erased finite-set adapter procedure reachable from the executable input. This is
not a cache and not a second semantic analysis pass; it is a deterministic
reservation walk over already-published executable inputs. The walk must include:

- every `CallableValueEmissionPlan` in every lambda-solved representation store
- every `ErasedPromotedWrapperBodyPlan.code`
- every `ErasedPromotedWrapperBodyPlan.capture`
- every `ErasedPromotedWrapperBodyPlan.hidden_capture_arg`
- every `const_instance` expression in lambda-solved MIR
- every `const_instance` node inside an
  `ErasedCaptureExecutableMaterializationPlan`

When the walk reaches a `ConstInstanceRef`, it resolves that reference through
the same zero-copy checked-artifact views used for executable materialization:
root artifact, relation artifacts, and imported artifacts. It then traverses the
sealed `CompileTimeValueStore` graph for that concrete instance. Pure scalar,
string, list, record, tuple, tag, box, alias, and nominal nodes only recurse into
their children. A finite callable leaf reserves no erased adapter by itself. An
erased boxed callable leaf reserves the adapter named by its explicit
`ErasedCallableCodeRef` when that code ref is `finite_set_adapter`, and then
recurses into its explicit erased capture materialization plan.

The same rule applies inside executable erased-capture materialization graphs. A
`finite_callable_set` materialization node must recurse into every materialized
capture, because a capture can itself contain an erased boxed callable. A
`const_instance` materialization node must recurse through the referenced
constant instance. `pure_const` and `pure_value` are the only constant-like nodes
that may be skipped, and only because their
`NoReachableCallableSlotsProof` proves there are no reachable callable leaves.

This reservation walk must not inspect source syntax, infer erasedness from
types, scan all constants in every imported artifact, build a runtime closure
object, allocate a runtime thunk, deserialize another copy of an imported
constant, or use a target-layout handle as adapter identity. Its key for adapter
deduplication is exactly `ErasedAdapterKey`: source function type, callable-set
key, erased function signature key, and capture shape key. Omitting any part is a
compiler bug: debug assertion in debug builds and `unreachable` in release
builds.

Mono MIR lookup of a top-level compile-time constant emits a constant reference
expression:

```zig
const_ref {
    ref: ConstRef,
    instantiation: ConstInstantiationKey,
    ty: MonoTypeId,
}
```

The exact expression name may differ, but the operation must carry the
`ConstRef` and the concrete `ConstInstantiationKey` produced by
clone-instantiating the checked `ConstUseTemplate` in this mono specialization.
It must not store a generalized source type, call a hidden top-level thunk,
synthesize a runtime initializer, or ask the LIR interpreter to run the imported
module again.

Executable MIR and LIR materialize a constant reference through an explicit
target-specific materialization plan:

```zig
const ConstMaterializationPlan = struct {
    const_instance: ConstInstanceRef,
    target_type: CanonicalExecValueTypeKey,
    root: ConstMaterializationNodeId,
    layout: LayoutId,
    rc_plan: ConstRcPlan,
};

const ConstMaterializationNodeId = enum(u32) { _ };

const ConstMaterializationNode = union(enum) {
    scalar: ConstScalarMaterialization,
    string: ConstStringMaterialization,
    list: ConstListMaterialization,
    record: ConstRecordMaterialization,
    tuple: ConstTupleMaterialization,
    tag_union: ConstTagUnionMaterialization,
    box: ConstBoxMaterialization,
    transparent_alias: ConstMaterializationNodeId,
    nominal: ConstNominalMaterialization,
    finite_callable_leaf: FiniteCallableMaterialization,
    erased_callable_leaf: ErasedCallableMaterialization,
    recursive_ref: ConstMaterializationNodeId,
};

const FiniteCallableMaterialization = union(enum) {
    finite_value: struct {
        leaf: FiniteCallableLeafInstance,
        callable_set_key: CanonicalCallableSetKey,
        member: CallableSetMemberId,
        executable_specialization_key: ExecutableSpecializationKey,
    },
    boxed_erased_value: ProcValueErasePlan,
};

const ErasedCallableMaterialization = struct {
    leaf: ErasedCallableLeafInstance,
    sig_key: ErasedFnSigKey,
    code: ErasedCallableCodeMaterializationPlan,
    capture: ErasedCaptureExecutableMaterializationPlan,
};

const ErasedCallableCodeMaterializationPlan = union(enum) {
    direct_proc_value: ProcValueErasePlan,
    finite_set_adapter: ErasedAdapterKey,
};
```

The materialization plan is target-specific and belongs after checking. It is a
recursive graph rooted at `root`, not a flat `ConstStoragePlan`. The logical
constant template in `CompileTimeValueStore` stays target-independent;
`ConstInstanceRef` points into the `ConstInstantiationStore` entry that supplies
the concrete source type and instantiated logical schema for this use.
Materialization chooses target bytes, statics, alignment, layout IDs,
reference-counting strategy, callable-set values, erased packed callable values,
and recursive backrefs for one target. It must not change the logical constant
value. If target-specific materialization is cached, that cache is keyed by the
checked artifact key, `ConstInstantiationKey`, and target/layout inputs, not by a
separate compile-time value sidecar.

`ConstMaterializationPlan` and `ErasedCaptureExecutableMaterializationPlan`
must use one shared materialization node discipline, even if the implementation
keeps separate Zig type names for clarity. Both graph families must be able to
represent scalar, string, list, record, tuple, tag, box, nominal, finite
callable, erased callable, and recursive-ref nodes directly. A node that points
at a sealed `ConstInstanceRef` is an optional sharing representation for a
serializable subtree whose checked artifact proves it has no reachable callable
slots; it is not the only legal representation for serializable data.

This is required for constant-owned erased captures. In a public constant such
as:

```roc
make_adder : I64 -> Box(I64 -> I64)
make_adder = |n| Box.box(|x| x + n)

table : { f : Box(I64 -> I64) }
table = { f: make_adder(41) }
```

the erased callable leaf for `table.f` captures `41`, but that `41` is not a
source-visible top-level constant and is not private-to-a-promoted-procedure
capture data. Reification must therefore be able to store the hidden capture as
an executable-ready materialization graph directly. It must not invent a
runtime thunk, allocate a runtime top-level closure object, create a fake
`ConstOwner.promoted_capture`, or require a promoted-procedure owner merely to
store serializable bytes. If the implementation chooses to factor out the `41`
as a sealed `ConstInstanceRef`, it must use an explicit constant-owner form for
constant-materialization private leaves, with a parent `ConstInstanceRef` plus a
stable `ComptimeValuePathKey`; it must not overload `promoted_capture`.

`const_instance` is a MIR-family handle, not an IR/LIR value form. The
`Executable MIR -> IR` boundary must eliminate every `const_instance` by building
and lowering a `ConstMaterializationPlan` from:

- the `ConstInstanceRef`
- the expected executable result type for this occurrence
- the target layout graph produced while lowering executable types
- read-only checked artifact views for the owning artifact and all imported or
  relation artifacts that may own the referenced constant

The executable program handed to IR lowering must therefore carry zero-copy,
read-only artifact views for `CompileTimeValueStore`,
`ConstInstantiationStoreView`, `CompileTimePlanStore`, callable-set
descriptors, and procedure identity data needed by callable leaves. IR lowering
must not deserialize an imported module, rerun compile-time evaluation, inspect
source declarations, or ask the LIR interpreter for imported-module data.

After IR lowering, there must be no `const_instance` expression left. LIR,
ARC, backends, and interpreters only see ordinary literal, aggregate, callable,
box, list, tag, and RC statements. A `const_instance` reaching LIR is a compiler
bug: debug assertion in debug builds and `unreachable` in release builds.

The executable builder may accept `const_instance` only as an input handle from
lambda-solved MIR. It must resolve that handle immediately while building the
executable program, using the read-only artifact view for the owning checked
artifact. Resolution returns both the sealed `ConstInstance` row and the owning
materialization stores: `CompileTimeValueStore` for logical values and
`CompileTimePlanStore` for callable/erased-capture materialization nodes. This
pair is required because constants imported through zero-copy artifact views can
contain callable leaves whose erased captures point at plan-store nodes owned by
the exporting artifact. Executable MIR must not copy the imported artifact into a
private runtime payload, deserialize a second constant representation, or ask the
exporting module to run again.

Materialization has two modes. Pure materialization is used for serializable
constant leaves and must reject callable schemas. General constant materialization
is used only for concrete `ConstInstanceRef` occurrences and recursively permits
callable leaves. In the general mode, a finite callable leaf materializes to a
finite `callable_set_value` only when the expected executable type is the matching
callable-set type and the selected member has an empty capture schema. If the
callable value has captured data, checking finalization or constant
instantiation must already have promoted it to a closed procedure value or
recorded the exact erased callable materialization required by a `Box(T)`
boundary. Executable materialization must not invent captures, allocate a runtime
closure object, or treat a captured finite callable as a no-capture leaf.

This boundary is intentionally before IR lowering. IR lowering may keep a loud
debug-only guard for `const_instance` as a deletion audit, but it must not contain
the implementation of constant materialization. The complete materialization has
to happen while executable MIR still has access to executable type metadata,
callable-set descriptors, erased callable plans, and the checked-artifact
zero-copy views needed to interpret `ConstInstanceRef` correctly.

Every materialization node is built from three inputs: the logical constant
node, the requested `CanonicalExecValueTypeKey`, and the finalized layout graph
for that target. Records use `RecordShapeId` and `RecordFieldId`; tag unions
use `TagUnionShapeId`, `TagId`, and `TagPayloadId`; tuples use tuple index;
lists preserve source element order; transparent aliases emit no runtime
wrapper; nominals materialize through their recorded representation capability.
The node graph must preserve source evaluation order separately from logical
assembly order when those differ. Later stages must not recover field order,
tag selection, callable identity, erasedness, or nominal wrapping from runtime
bytes or physical layout order.

The logical `CompileTimeValueStore` is deliberately insufficient by itself.
Lowering must not try to reverse-engineer callable materialization from only a
stored `ComptimeValue.callable` plus the requested executable type. For example,
a finite callable leaf requested at an erased `Box(T)` boundary requires the
exact solved `ProcValueErasePlan` or finite-set adapter selected for that
boundary. That selection is produced by representation solving and must be
recorded in `ConstMaterializationPlan`; it must not be rediscovered from the
erased function ABI, source syntax, descriptor scans, or a fallback search.

Callable leaves are ordinary constant contents. A non-function top-level
constant such as `{ f: |x| x + 1 }` materializes as a record whose field is a
callable value; it does not become a runtime top-level thunk, runtime global
callable object, runtime initializer procedure, or interpreter pointer. If the
requested target type keeps the field finite, `finite_callable_leaf` emits a
finite callable-set value using the exact `FiniteCallableLeafInstance`, canonical
callable-set key, member id, and executable specialization selected for that
occurrence. If the requested target type is erased because of an explicit
`Box(T)` boundary, the same finite leaf materializes through
`boxed_erased_value` and consumes the exact `ProcValueErasePlan` for that
boundary. Non-`Box(T)` containers never introduce erased representation.

The `finite_callable_leaf` materialization fields have distinct meanings.
`leaf.proc_value` is the closed no-capture procedure value occurrence stored in
the concrete constant instance. `callable_set_key` and `member` are the finite
executable representation selected for the requested target type. They must be
derived from the same solved callable representation that would lower an
ordinary `proc_value` occurrence. `executable_specialization_key` is the direct
body specialization needed by that selected member. Materialization must not use
`leaf.proc_value.template` alone as the member identity, because the same checked
procedure template can appear at multiple canonical function types and inside
different callable-set representations.

An `erased_callable_leaf` materializes only as an erased packed callable with
matching `ErasedFnSigKey` and non-empty `BoxBoundaryId` provenance. Its code is
either a direct proc-value erase plan or a finite-set adapter key. It must not
materialize back into a finite callable value, and it must not recover its code
or capture from source syntax, symbol spelling, callable-set member order, or
runtime packed-function bytes.

Constant materialization must preserve the ordinary ARC contract:

- immutable static bytes may be referenced only through LIR operations whose
  reference-counting behavior is explicit
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
- sealed resolved value-reference table
- artifact-owned checked type store, including canonical key-to-root and
  key-to-scheme maps
- artifact-owned checked body store, including every checked expression,
  pattern, and statement body reachable from exported templates, compile-time
  templates, callable-eval templates, nested procedure sites, and platform
  relation roots
- checked procedure template table
- promoted procedure table
- root request table
- hosted procedure table
- platform-required binding table
- imported/interface representation capabilities
- compile-time root table, if retained for diagnostics or verification
- `CompileTimeValueStore`, including public constant graphs with callable
  leaves and serializable private capture leaves
- `ConstInstantiationStore` entries owned by the artifact
- `CallableBindingInstantiationStore` entries owned by the artifact
- `SemanticInstantiationProcedureTable` entries owned by the artifact
- private promoted-capture graph nodes addressed by `PrivateCaptureRef`
- callable result plans, callable promotion plans, constant reification plans,
  and dependency-summary templates referenced by concrete instances or imported
  template closures

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

For executable app/platform builds, app and platform root artifacts are
published as one co-finalization group. The app root artifact is checked in a
context that includes the platform's requirement interface, because platform
requirements can constrain app numeric defaults, static dispatch, where-clause
resolution, and exported type aliases. The platform root artifact used for
executable lowering is checked in a context that includes the sealed app artifact
key or an equivalent `PlatformAppRelationKey`, because required lookups in
platform code are app values, not platform-local declarations. Therefore:

- the executable platform root artifact is app-specific
- the app root artifact key changes when the platform requirement interface
  changes in a way that can affect app checking
- the executable platform root artifact key changes when the app artifact key or
  platform/app relation key changes
- `PlatformRequirementContextKey` is derived from the platform module identity
  and a hash of the platform-required declaration table. App artifacts checked
  against a platform include this key in `checking_context_identity`; this is
  what makes the app artifact key change when the platform requirement interface
  changes, without depending on the app artifact key itself.
- A published platform declaration artifact must expose a read-only
  `platformRequirementContextKey()` view so the app checker can consume the exact
  checked requirement interface instead of reconstructing it from source text.
- `PlatformAppRelationKey` is derived from the sealed app artifact key and
  `PlatformRequirementContextKey`. It must not be a name-only or
  requirement-count-only key.
- standalone platform checking and glue generation may publish requirement
  declarations without app-specific bindings, but those artifacts are not valid
  inputs for executable lowering
- no artifact in the co-finalization group is published until platform
  requirement checking, numeric default finalization, static-dispatch
  finalization, compile-time constant evaluation, callable promotion, and
  platform-required binding construction have all completed

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

Compile-time constants and concrete callable binding instantiations are part of
checking finalization, so the checked artifact is incomplete without its
`CompileTimeValueStore`, the sealed `ConstInstantiationStore` entries required
by that artifact, and the sealed `CallableBindingInstantiationStore` entries
required by that artifact. It is also incomplete without the
`SemanticInstantiationProcedureTable` entries, promoted procedure rows, private
capture graph nodes, and private capture constant templates referenced by those
instances.

The cache must not accept an entry that restores checked declarations but omits
the compile-time value store or constant instantiation store entries required by
those declarations. It also must not omit callable binding instantiation entries
required by those declarations, semantic-instantiation procedure entries they
reference, or private promoted-capture data they need. It must not accept a
compile-time value store, constant instantiation store, callable binding
instantiation store, or semantic-instantiation procedure table whose checked
artifact is missing or whose owner key does not match the checked artifact key.

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
different cache keyed by the checked artifact key, `ConstInstantiationKey`, and
target/layout inputs.
The target-specific materialization cache is not the logical
`ConstInstantiationStore`; it may consume `ConstInstanceRef` values, but it must
not create or repair them.

#### Cache Misses And Published Artifacts

Before publication, these conditions are cache misses:

- missing cache entry
- unsupported cache format
- invalid serialized bytes
- mismatched `compiler_artifact_hash`
- missing required artifact component
- missing checked type store payload for an exported/imported
  `CanonicalTypeKey` or `CanonicalTypeSchemeKey`
- missing checked body store payload for an exported/imported checked procedure,
  constant eval template, callable eval template, nested procedure site, or
  promoted callable wrapper
- missing `CompileTimeValueStore` for an artifact with compile-time bindings
- missing required `ConstInstantiationStore` entries for concrete constant
  instances recorded by the artifact
- missing required `CallableBindingInstantiationStore` entries for concrete
  callable binding instances recorded by the artifact
- missing required `SemanticInstantiationProcedureTable` entries for procedures
  created by concrete constant or callable-binding instantiation
- missing private promoted-capture graph nodes, private capture constants,
  callable result plans, callable promotion plans, or dependency-summary
  templates required by exported/imported templates
- failed deserialization

After a checked artifact has been published to later compiler stages, missing
components are not recoverable. They are compiler invariant violations: debug-only
assertion in debug builds and `unreachable` in release builds.

## Final Data Structures

### Checked Artifact Boundary

Every post-check public pipeline consumes checked artifacts, not loose checked
modules plus optional side stores.

#### Artifact-Owned Checked Bodies And Types

Published checked artifacts must contain the complete checked body and checked
type payloads needed by downstream checking finalization, mono MIR
specialization, compile-time constant instantiation, callable binding
instantiation, and platform/app relation lowering. `ModuleEnv` may still be
stored in the artifact for diagnostics, type printing, source locations, cache
round-tripping, or checking-only services, but it is not the payload used to
lower imported generic procedure templates. Imported specialization must work
from artifact-owned checked stores and imported template closures alone.

This is mandatory because `CanonicalTypeKey` and `CanonicalTypeSchemeKey` are
stable identities, not type payloads. A hash key can say "this is the requested
type," but it cannot by itself provide the argument list, return type, row
labels, static-dispatch constraints, nominal backing type, recursive edges, or
generalized variable structure needed to clone-instantiate and lower a generic
body. The payload lives in `CheckedTypeStore`.

Conceptual checked type payload:

```zig
const CheckedTypeId = enum(u32) { _ };
const CheckedTypeSchemeId = enum(u32) { _ };
const CheckedBodyId = enum(u32) { _ };
const CheckedExprId = enum(u32) { _ };
const CheckedPatternId = enum(u32) { _ };
const CheckedStatementId = enum(u32) { _ };
const CheckedStringLiteralId = enum(u32) { _ };
const StringBytes = []const u8;

const CheckedTypeStore = struct {
    nodes: Store(CheckedTypeNode),
    concrete_roots: Map(CanonicalTypeKey, CheckedTypeId),
    schemes: Map(CanonicalTypeSchemeKey, CheckedTypeScheme),
};

const CheckedTypeScheme = struct {
    root: CheckedTypeId,
    generalized_vars: Span(CheckedTypeId),
};

const CheckedTypeNode = union(enum) {
    generalized_var: CheckedGeneralizedVar,
    flex_var: CheckedOpenVar,
    rigid_var: CheckedOpenVar,
    alias: CheckedAliasType,
    nominal: CheckedNominalType,
    func: CheckedFuncType,
    record: CheckedRecordType,
    record_empty,
    tag_union: CheckedTagUnionType,
    tag_union_empty,
    tuple: Span(CheckedTypeId),
    primitive: PrimitiveType,
    recursive_ref: CheckedTypeId,
};

const CheckedOpenVar = struct {
    name: ?CanonicalTypeVarName,
    constraints: Span(StaticDispatchConstraintTemplate),
};

const CheckedFuncType = struct {
    args: Span(CheckedTypeId),
    ret: CheckedTypeId,
    effect: CheckedFunctionEffect,
    needs_instantiation: bool,
};

const CheckedRecordType = struct {
    fields: Span(CheckedRecordField),
    ext: CheckedTypeId,
};

const CheckedRecordField = struct {
    label: RecordFieldLabelId,
    ty: CheckedTypeId,
};

const CheckedTagUnionType = struct {
    tags: Span(CheckedTag),
    ext: CheckedTypeId,
};

const CheckedTag = struct {
    label: TagLabelId,
    payloads: Span(CheckedTypeId),
};

const CheckedAliasType = struct {
    alias: NominalOrAliasTypeKey,
    args: Span(CheckedTypeId),
    backing: CheckedTypeId,
};

const CheckedNominalType = struct {
    nominal: NominalTypeKey,
    is_opaque: bool,
    args: Span(CheckedTypeId),
    backing: CheckedTypeId,
};
```

The exact Zig names may differ, but these invariants must not:

- every `CanonicalTypeKey` exported from or consumed inside a checked artifact
  resolves to a `CheckedTypeId` in that artifact's `CheckedTypeStore`
- every `CanonicalTypeSchemeKey` resolves to a `CheckedTypeScheme` whose root
  and generalized variables are in that same store
- checked type nodes use canonical type, field, tag, method, module, and export
  identities; they do not contain `Ident.Idx`
- recursive type graphs are represented by explicit graph edges, not by pointer
  identity or stack recursion
- generalized variables are explicit nodes in the checked type graph, not raw
  checker vars that later stages reinterpret
- static-dispatch constraints attached to open variables use canonical method
  names and checked type roots for their function types
- if a key is present without its checked type payload, that artifact is
  incomplete; debug builds assert immediately and release builds use
  `unreachable`

#### Concrete Source Type Payloads For Requests

Every construction request that names a concrete source type must carry both:

- a canonical key used for identity, deduplication, cache lookup, snapshot
  comparison, and stable procedure naming
- a concrete source type payload ref used for clone-instantiation, unification,
  static-dispatch resolution, callable-leaf instantiation, constant
  reification, and executable wrapper construction

The key and payload are deliberately separate. A `CanonicalTypeKey` is a stable
name for a type graph. It is not the graph. It cannot provide argument order,
return type, row extension structure, nominal backing type, static-dispatch
constraints, generalized variable identity, recursive edges, or alias structure.
Any code path that has only a key may compare identity, but it may not construct
or lower a type from that key.

The shared request payload store is:

```zig
const ConcreteSourceTypeRef = enum(u32) { _ };

const ConcreteSourceTypeRoot = struct {
    key: CanonicalTypeKey,
    root: CheckedTypeId,
};

const ConcreteSourceTypeStore = struct {
    checked_types: CheckedTypeStore,
    roots: Store(ConcreteSourceTypeRoot),
    by_key: Map(CanonicalTypeKey, ConcreteSourceTypeRef),
};

const ConcreteSourceTypeArg = struct {
    key: CanonicalTypeKey,
    payload: ConcreteSourceTypeRef,
};
```

The exact names may differ, but the split must not. `ConcreteSourceTypeStore` is
owned by the current MIR-family lowering or checking-finalization instantiation
run. It stores checked source type payloads in artifact-owned checked type node
form. It is not `ModuleEnv`, not a checker `types.Store`, not mono MIR types,
not lambda-solved types, not executable layouts, and not runtime data.

Concrete source type payloads enter the store only from explicit earlier-stage
data:

- root requests register the artifact-owned checked type root named by
  `RootRequest.checked_type`
- resolved procedure uses register the cloned source function type produced from
  `ProcedureUseTemplate.source_fn_ty_template` in the current specialization
- resolved const uses register the cloned source value type produced from
  `ConstUseTemplate.requested_source_ty_template` in the current specialization
- static-dispatch targets register the unified callable/target source function
  type that will be requested from the target procedure after the enclosing
  template has been clone-instantiated; verifier-only dispatcher payloads may be
  registered separately, but the mono specialization request payload is the
  unified target function type
- const and callable binding instantiation during checking finalization register
  the requested concrete source type before evaluating or reifying the instance
- imported artifacts and platform/app relation artifacts may contribute payloads
  only through their published checked type stores and imported closure views

Registration computes the canonical key from the explicit payload graph and
stores `key -> payload`. If the same key is registered twice with structurally
equivalent checked payloads, the existing `ConcreteSourceTypeRef` may be reused.
If the same key is registered with a non-equivalent payload, that is a compiler
bug: debug builds assert immediately and release builds use `unreachable`.
Payload refs are construction-run-local handles. They must not be serialized as
cache keys, written into stable checked artifacts, exposed as public semantic ids,
or passed to a later independent compiler run. Sealed artifacts may retain the
canonical key and the artifact-owned checked type graph; any later construction
run creates its own `ConcreteSourceTypeRef` from that explicit payload.

No post-check stage may recover a concrete source type payload from a key by
hashing source text, resolving names, inspecting a foreign `ModuleEnv`, reading
a raw checker `Var`, asking a type printer, converting a `MonoTypeId` back to a
source type, or comparing expression syntax. The payload ref is the construction
input. The key is only the stable identity of that input.

Conceptual checked body payload:

```zig
const CheckedBodyStore = struct {
    bodies: Store(CheckedBody),
    exprs: Store(CheckedExpr),
    patterns: Store(CheckedPattern),
    statements: Store(CheckedStatement),
    string_literals: Span(StringBytes),
};

const CheckedBody = struct {
    root_expr: CheckedExprId,
    owner_template: ProcedureTemplateRef,
};

const CheckedExpr = struct {
    ty: CheckedTypeId,
    source_region: SourceRegion,
    data: CheckedExprData,
};

const CheckedPattern = struct {
    ty: CheckedTypeId,
    source_region: SourceRegion,
    data: CheckedPatternData,
};
```

`CheckedExprData`, `CheckedPatternData`, and `CheckedStatement` may remain
CIR-like in shape, but they are artifact-owned checked records. They must store
canonical labels, checked body ids, checked expression ids, checked pattern ids,
checked type ids, and ids into sealed artifact tables such as
`ResolvedValueRefTable`, `StaticDispatchPlanTable`, and `NestedProcSiteTable`.
They must not store raw `CIR.Expr.Idx`, raw `CIR.Pattern.Idx`, raw `types.Var`,
raw `Ident.Idx`, source-name lookup handles, or pointers into another module's
checked store as semantic payload. Source regions may be retained for
diagnostics and debugging, but source regions are never lookup keys for lowering.

Checked body string literal ids are also artifact-local payload ids, not final
program ids. `CheckedStringLiteralId` values index
`CheckedBodyStore.string_literals` in the checked artifact that owns the
checked body. They may be used only inside checked artifacts and inside the
artifact-local lowering
context that is currently reading that checked body. They must never be exported
as MIR, IR, LIR, cache-key, backend, or interpreter payloads without first being
resolved to bytes from the owning artifact.

Local templates inside the root artifact may be built by copying from checked
CIR into `CheckedBodyStore` and `CheckedTypeStore` during checking
finalization. Imported templates must be consumed from the exporter artifact's
read-only checked body/type stores through exported views and
`ImportedTemplateClosureView`. Adding `module_env: *const ModuleEnv` or
`types_store: *const types.Store` to `ImportedModuleView` is forbidden as a
solution to imported generic specialization, because it would make importers
reconstruct semantic payload from exporter-local state instead of consuming the
published artifact data.

#### Artifact-Owned Strings And The Program Literal Pool

String literals have three distinct lifetimes, and the implementation must not
collapse them:

1. Source/parser lifetime: `base.StringLiteral.Idx` is valid only in the
   `ModuleEnv` or `CommonEnv` string store that created it.
2. Checked-artifact lifetime: `CheckedStringLiteralId` is valid only in one
   `CheckedModuleArtifact` and indexes that artifact's checked string bytes.
3. Lowered-program lifetime: `ProgramLiteralId` is valid only in one lowered
   MIR/IR program and indexes the program-owned literal pool carried by that
   program.

The final architecture must introduce an explicit lowered-program literal pool:

```zig
const ProgramLiteralId = enum(u32) { _ };

const ProgramLiteral = struct {
    bytes: []const u8,
};

const ProgramLiteralPool = struct {
    literals: Store(ProgramLiteral),
    by_bytes: Map(BytesHash, ProgramLiteralId),
};
```

The exact storage representation may differ, but the ownership boundary must
not. The pool owns or references bytes that remain valid for the whole lowered
program. It deduplicates by exact byte contents within the lowered program and
returns stable dense `ProgramLiteralId` values. It must not use
`base.StringLiteral.Idx`, `CheckedStringLiteralId`, source regions, expression
ids, artifact-local dense ids, or `ModuleEnv` pointers as cross-stage literal
identity.

Mono MIR creates the initial `ProgramLiteralPool`. When mono MIR lowers a
checked expression, checked pattern, or checked statement that contains a
`CheckedStringLiteralId`, it must resolve that id through the checked body store
of the artifact that owns the currently lowered template, copy or retain the
literal bytes in the program literal pool, and store only `ProgramLiteralId` in
mono MIR. Imported template lowering therefore reads imported literal bytes from
the imported `CheckedBodyStore.string_literals` view. It must not read the
exporter's `ModuleEnv`, the importer's `ModuleEnv`, or any `base.StringLiteral`
store.

The program literal pool covers every source literal byte payload that survives
checking:

- string expression literals and string interpolation segments
- bytes literal payloads
- string literal patterns in source `match`
- user-written `crash` expression and statement messages
- compile-time constant reification of `Str` and byte-list values into runtime
  values
- debug verifier payloads that need to validate lowered source literal bytes

`runtime_error` is not a source literal. It carries no `ProgramLiteralId`.
Backends may synthesize backend-owned diagnostic text for runtime errors, but
that text is not source-literal payload and must not be represented as a checked
artifact literal or MIR program literal.

Row-finalized mono MIR, lifted MIR, lambda-solved MIR, executable MIR, and IR
must move the same literal pool forward with the program and must store
`ProgramLiteralId` for all literal-bearing nodes. They may copy, compact, or
remap the pool only as an explicit whole-program transform that rewrites every
referencing `ProgramLiteralId` in the same type-state transition. They must not
re-resolve checked string ids, inspect `ModuleEnv`, or carry artifact-local
literal ids after mono MIR lowering.

IR-to-LIR lowering is the only place where program literal ids become
`base.StringLiteral.Idx` again. It resolves `ProgramLiteralId` to bytes from the
IR program literal pool, interns those bytes into `LirStore.strings`, and stores
the resulting `base.StringLiteral.Idx` in LIR. Backends and the interpreter
continue to use only `LirStore.getString`; they must never reach back into MIR,
IR, checked artifacts, or source module string stores.

#### Imported Canonical Name Remapping

Canonical name ids stored inside a checked artifact are dense ids owned by that
artifact's `CanonicalNameStore`. They are canonical because they are derived from
canonical bytes while the owning `Ident.Store` was known, but the dense id value
itself is not globally meaningful across artifacts unless the implementation
uses one global content-addressed canonical-name store for all artifacts in the
process. The final architecture must not rely on that. It must treat a pair
like:

```text
artifact A: RecordFieldLabelId(0) == "x"
artifact B: RecordFieldLabelId(0) == "y"
```

as two different artifact-local ids until both have been remapped into the same
lowering-run canonical-name store. Without this remapping, imported checked type
payloads can be cloned into mono MIR with foreign row labels, and row
finalization can incorrectly treat unrelated fields or tags as equal merely
because their artifact-local dense ids match.

For example, suppose an imported module exports a generic procedure whose
instantiated body mentions `{ x: I64 }`, and the root module independently
interned `y` as its first record field label. If mono specialization copies the
imported `RecordFieldLabelId(0)` directly into the root lowering run, the
imported `x` field can be indistinguishable from the root `y` field. That would
produce wrong row-finalized MIR. This is a compiler design error, not a verifier
preference.

Every `CheckedModuleArtifact` must therefore publish a read-only canonical-name
view. The exact Zig names may differ, but the view must expose canonical bytes
for every lowering-visible name kind:

```zig
const CanonicalNameView = struct {
    module_names: Span([]const u8),
    type_names: Span([]const u8),
    method_names: Span([]const u8),
    record_field_labels: Span([]const u8),
    tag_labels: Span([]const u8),
    export_names: Span([]const u8),
    external_symbol_names: Span([]const u8),
};
```

This view is artifact data. It is not source lookup, not an `Ident.Store`, and
not a fallback. It is the published canonical byte table that justifies the
artifact-local ids already stored in checked payloads.

Every MIR-family lowering run must own one lowering-run canonical-name store.
The artifact resolver must remap all imported artifact-local canonical name ids
into that lowering-run store before any imported checked payload reaches mono
MIR, row-finalized mono MIR, lifted MIR, lambda-solved MIR, executable MIR, IR,
LIR, cache keys produced by lowering, or backend semantic inputs.

Conceptual resolver support:

```zig
const ArtifactNameResolver = struct {
    lowering_names: *CanonicalNameStore,
    artifacts: Span(ImportedModuleView),

    fn moduleName(artifact: CheckedModuleArtifactKey, id: ModuleNameId) ModuleNameId;
    fn typeName(artifact: CheckedModuleArtifactKey, id: TypeNameId) TypeNameId;
    fn methodName(artifact: CheckedModuleArtifactKey, id: MethodNameId) MethodNameId;
    fn recordFieldLabel(artifact: CheckedModuleArtifactKey, id: RecordFieldLabelId) RecordFieldLabelId;
    fn tagLabel(artifact: CheckedModuleArtifactKey, id: TagLabelId) TagLabelId;
    fn exportName(artifact: CheckedModuleArtifactKey, id: ExportNameId) ExportNameId;
    fn externalSymbolName(artifact: CheckedModuleArtifactKey, id: ExternalSymbolNameId) ExternalSymbolNameId;
};
```

The resolver implementation reads canonical bytes from the source artifact's
`CanonicalNameView` and interns those bytes into the lowering-run
`CanonicalNameStore`. It may cache remap tables keyed by
`CheckedModuleArtifactKey` and source name id. Cache misses in these remap
tables are ordinary construction work, not heuristic lookup. A requested source
name id outside the artifact's published name table is a compiler invariant
violation: debug assertion immediately, release `unreachable`.

The artifact resolver must return remapped clones for imported checked payloads.
It must not hand out borrowed imported payloads that still contain
artifact-local canonical ids unless the consumer is explicitly artifact-local and
will not compare those ids with ids from any other artifact. Mono MIR lowering is
not such a consumer; it always works in a lowering-run identity space.

Name remapping is required for every lowering-visible payload that stores a
canonical name id, including:

- `CheckedTypePayload.alias.name` and `.origin_module`
- `CheckedTypePayload.nominal.name` and `.origin_module`
- record type fields and record expressions
- tag-union type tags, tag expressions, tag patterns, and tag-payload metadata
- method names in checked expressions, static dispatch plans, constraints, and
  method registry keys
- type names in typed literals or nominal metadata
- export names, module names, hosted ABI names, platform requirement names, and
  external symbol names
- any future checked body, compile-time value graph, callable template,
  promoted-procedure, or bridge metadata that carries a canonical name id

The clone-instantiated checked source type graph used by mono specialization must
therefore contain only lowering-run canonical name ids. `ConcreteSourceTypeRef`
payloads that point at imported artifacts must be cloned through
`ArtifactNameResolver` before they are registered as local concrete payloads.
`ConcreteSourceTypeStore` may keep artifact refs as visibility handles, but a
payload that is lowered into mono types must first be converted into the
lowering-run name space.

Canonical type keys remain byte-derived stable identities. Remapping dense
canonical-name ids must not change the canonical type key of a payload. After an
imported type payload is cloned and remapped into the lowering-run checked type
graph, debug builds must recompute or compare its canonical key by canonical
bytes and assert that it equals the source artifact key. Release builds use
`unreachable` for mismatch. Verifiers must not repair mismatches.

Debug-only verifiers must assert:

- no MIR row shape, method key, static-dispatch plan, hosted/platform record,
  compile-time value graph, callable-set key, erased adapter key, or executable
  specialization key contains a foreign artifact-local canonical name id
- every imported checked type/body/static-dispatch/method-registry payload
  consumed by mono lowering was remapped through the artifact resolver
- row finalization compares only lowering-run `RecordFieldLabelId`,
  `TagLabelId`, and `MethodNameId` values
- remapping tables are not consulted by release builds except as required to
  construct the real lowering-run payloads; verifier-only foreign-origin
  metadata is debug-only

This remapping is not optional and not an optimization. It is required for
correctness whenever a lowering run consumes more than one checked artifact.

Mono specialization uses an `ArtifactTemplateResolver`-style service:

```zig
const ArtifactTemplateResolver = struct {
    root: LoweringModuleView,
    imports: Span(ImportedModuleView),
    relations: Span(RelationModuleView),
    names: ArtifactNameResolver,

    fn procedureTemplate(ref: ProcedureTemplateRef) CheckedProcedureTemplateView;
    fn checkedBody(ref: ArtifactCheckedBodyRef) RemappedCheckedBodyView;
    fn checkedType(ref: ArtifactCheckedTypeRef) RemappedCheckedTypeView;
    fn typeScheme(ref: ArtifactCheckedTypeSchemeRef) CheckedTypeSchemeView;
};
```

The resolver is not a lookup heuristic. It enforces artifact visibility:

- root artifact refs may resolve to the root artifact's own checked stores
- ordinary imported refs may resolve only exported entries and private entries
  listed in the exported entry's `ImportedTemplateClosureView`
- platform/app relation refs may resolve only explicitly named
  platform-required values and relation artifacts
- missing entries are compiler invariant violations, not triggers to inspect
  source declarations, scan exports, or lower every imported template

Clone-instantiation from checked type payloads must be deterministic and
cycle-safe:

1. Reserve a fresh destination type node for each source `CheckedTypeId` before
   cloning that node's children.
2. Map every generalized source variable in the selected
   `CheckedTypeScheme` to one fresh specialization-local variable.
3. Clone aliases, nominals, records, tag unions, tuples, functions, static
   dispatch constraints, and recursive refs through that map, remapping every
   canonical name id through `ArtifactNameResolver` as the clone is written.
4. Clone the concrete requested source type payload named by
   `ConcreteSourceTypeRef` into the same specialization-local checked type
   store, again remapping imported canonical names before the payload becomes
   local to the lowering run.
5. Debug-assert that the cloned requested payload's canonical key equals the
   `MonoSpecializationKey.requested_mono_fn_ty` for this request; release builds
   use `unreachable` for mismatch.
6. Unify the cloned template function root with the cloned concrete requested
   function root in the same specialization-local type store.
7. Lower mono MIR expressions using the cloned types attached to checked body
   nodes.

No post-check stage may recreate a missing checked type payload by hashing
source text, resolving an import name, looking at a display name, inspecting a
foreign `ModuleEnv`, reading a raw checker `Var`, or comparing source syntax.

The compiler may expose restricted views for specific consumers:

```zig
const ArtifactRef = struct {
    artifact: CheckedModuleArtifactKey,
    local_id: u32,
};

const ArtifactCheckedBodyRef = ArtifactRef;
const ArtifactCheckedTypeRef = ArtifactRef;
const ArtifactCheckedTypeSchemeRef = ArtifactRef;
const ArtifactCheckedCallableBodyRef = ArtifactRef;
const ArtifactCheckedConstBodyRef = ArtifactRef;
const ArtifactProcedureTemplateRef = ArtifactRef;
const ArtifactCallableEvalTemplateRef = ArtifactRef;
const ArtifactResolvedValueRefTableRef = ArtifactRef;
const ArtifactStaticDispatchPlanTableRef = ArtifactRef;
const ArtifactNestedProcSiteTableRef = ArtifactRef;
const ArtifactCallableResultPlanRef = ArtifactRef;
const ArtifactCallablePromotionPlanRef = ArtifactRef;
const ArtifactConstGraphReificationPlanRef = ArtifactRef;
const ArtifactComptimeDependencySummaryTemplateRef = ArtifactRef;
const ArtifactPrivateCaptureNodeRef = ArtifactRef;

const ImportedModuleView = struct {
    key: CheckedModuleArtifactKey,
    module_identity: ModuleIdentity,
    canonical_names: CanonicalNameView,
    exports: ExportTableView,
    checked_types: CheckedTypeStoreView,
    checked_bodies: CheckedBodyStoreView,
    exported_procedure_templates: ExportedProcedureTemplateView,
    exported_procedure_bindings: ExportedProcedureBindingView,
    exported_const_templates: ExportedConstTemplateView,
    method_registry: MethodRegistryView,
    interface_capabilities: ModuleInterfaceCapabilitiesView,
    comptime_values: CompileTimeValueStoreView,
    const_instances: ConstInstantiationStoreView,
    callable_binding_instances: CallableBindingInstantiationStoreView,
    semantic_instantiation_procedures: SemanticInstantiationProcedureTableView,
};

const ImportedProcedureBindingView = struct {
    binding: ImportedProcedureBindingRef,
    source_scheme: CanonicalTypeSchemeKey,
    body: ImportedProcedureBindingBody,
    template_closure: ImportedTemplateClosureView,
};

const ImportedProcedureBindingBody = union(enum) {
    direct_template: DirectProcedureBinding,
    callable_eval_template: CallableEvalTemplateId,
};

const ImportedConstTemplateView = struct {
    const_ref: ConstRef,
    source_scheme: CanonicalTypeSchemeKey,
    template: ConstTemplate,
    template_closure: ImportedTemplateClosureView,
};

const ImportedTemplateClosureView = struct {
    checked_bodies: Span(ArtifactCheckedBodyRef),
    checked_type_roots: Span(ArtifactCheckedTypeRef),
    checked_type_schemes: Span(ArtifactCheckedTypeSchemeRef),
    checked_callable_bodies: Span(ArtifactCheckedCallableBodyRef),
    checked_const_bodies: Span(ArtifactCheckedConstBodyRef),
    checked_procedure_templates: Span(ArtifactProcedureTemplateRef),
    callable_eval_templates: Span(ArtifactCallableEvalTemplateRef),
    const_templates: Span(ConstRef),
    promoted_procedures: Span(PromotedProcedureRef),
    semantic_instantiation_procedures: Span(SemanticInstantiationProcedureRef),
    private_capture_roots: Span(PrivateCaptureRef),
    private_capture_nodes: Span(ArtifactPrivateCaptureNodeRef),
    private_capture_const_templates: Span(ConstRef),
    callable_result_plans: Span(ArtifactCallableResultPlanRef),
    callable_promotion_plans: Span(ArtifactCallablePromotionPlanRef),
    const_reification_plans: Span(ArtifactConstGraphReificationPlanRef),
    dependency_summary_templates: Span(ArtifactComptimeDependencySummaryTemplateRef),
    nested_proc_sites: Span(ArtifactNestedProcSiteTableRef),
    resolved_value_refs: Span(ArtifactResolvedValueRefTableRef),
    static_dispatch_plans: Span(ArtifactStaticDispatchPlanTableRef),
    method_registry_entries: Span(MethodRegistryEntryRef),
    interface_capabilities: ModuleInterfaceCapabilitiesView,
};

const LoweringModuleView = struct {
    artifact: *const CheckedModuleArtifact,
    roots: RootRequestSet,
    relation_artifacts: Span(ImportedModuleView),
};
```

Those views are read-only projections. They must not contain mutable pointers
that allow later stages to patch `ModuleEnv`, hosted indices,
platform-required bindings, compile-time values, or interface capability
records.

Downstream modules import `ImportedModuleView`. They do not inspect another
module's unchecked source, checked expression bodies, opaque backing syntax, or
private definitions to complete their own checked artifact.

If an import uses an exported generic procedure at a concrete type, it consumes
only the exported checked procedure template named by the imported module's
export table. It must not scan imported private definitions or lower every
exported template in the imported module. Private templates are invisible to
importers except through concrete procedure dependencies explicitly exposed by
the imported artifact.

If an import uses an exported function-valued binding at a concrete type, it
consumes the exported `ImportedProcedureBindingView`. A direct binding behaves
like an exported checked procedure template plus an exact requested function
type. A callable-eval binding behaves like an exported `CallableEvalTemplate`
plus its `ImportedTemplateClosureView`; the importing artifact owns the concrete
`CallableBindingInstanceRef` it requests and seals it during its own checking
finalization. The importer must not inspect the exporting module's source,
private checked bodies outside the imported closure, runtime interpreter output,
or any post-check lowering stage to complete the binding.

If an import uses an exported generic constant at a concrete type, it consumes
only the exported `ImportedConstTemplateView` named by the imported module's
export table. The view must include the exported `ConstRef`, its source scheme,
the exported `ConstTemplate`, and the private template closure needed to
instantiate that constant without inspecting source. The importing artifact owns
the resulting `ConstInstanceRef` in its own `ConstInstantiationStore` and
seals it during its own checking finalization; it must not mutate the imported
artifact's `CompileTimeValueStore`, rerun imported compile-time roots, create the
instance during post-check lowering, or discover private dependencies by looking
through source declarations.

`ImportedTemplateClosureView` is a serialized semantic closure, not a source
module. It contains only the checked bodies, checked type roots, checked type
schemes, checked callable bodies, checked constant bodies, checked procedure
templates, callable eval templates, constant templates, promoted procedures,
semantic-instantiation procedures, private promoted-capture roots and nodes,
private capture constant templates, callable-result plans, callable-promotion
plans, constant reification plans, dependency-summary templates, nested
procedure-site tables, resolved value references, static dispatch plans,
method-registry entries, and interface capabilities required to instantiate the
exported binding or constant at a concrete requested type. The closure must be
deterministic and complete. If an imported callable or constant template needs an
entry that is absent from the closure after the imported artifact has been
accepted, that is a compiler invariant violation: debug builds assert
immediately and release builds use `unreachable`.

For example, if an exported generic function `id` calls a private helper `go`,
the exported `id` template closure must include `go`'s checked procedure
template, checked body, checked type roots and schemes, resolved value-reference
records, static-dispatch plans, nested procedure sites, and method-registry
entries needed by `go`. The importer may specialize `go` only because `id`'s
closure explicitly exposes that private dependency as artifact data. It must not
scan the exporting module's private definitions, ask the exporter `ModuleEnv`
for `go`, or lower every exported/private template to find the dependency.

Every private reference in an imported closure is either artifact-qualified by
`CheckedModuleArtifactKey` or explicitly remapped into an importer-owned closure
namespace before use. A raw local id from an exporting artifact must never be
used as a semantic key inside an importing artifact. Debug verification must
assert that every artifact-qualified private ref reachable from an imported
template is either public/exported or listed in that template's
`ImportedTemplateClosureView`; release builds use `unreachable` for violations.

Every imported closure must also verify that each `CanonicalTypeKey` and
`CanonicalTypeSchemeKey` reachable from the closure has a corresponding checked
type root or scheme in the closure or in the public checked type view of the
imported artifact. Missing type payload is the same class of invariant violation
as a missing body or missing resolved value-reference table.

The executable pipeline consumes `LoweringModuleView` values. It may lower only
roots named by `RootRequestSet`. It must not discover additional roots by
scanning exports, source declarations, checked CIR expression shapes, or
procedure order.

For executable platform lowering, `relation_artifacts` must include the sealed
app artifact view named by `PlatformAppRelationKey`. This is not a source import
and must not be discovered through import scanning. It exists solely because
`PlatformRequiredBindingTable` entries can point at app `ConstRef` and app
procedure bindings, and mono MIR needs the app checked artifact view to resolve
those explicit references.

### Root Requests And Root Binding

Root selection happens during checking finalization or build planning before
post-check lowering begins.

A root request records that a particular checked artifact requires a runtime,
tool, test, REPL, development, or compile-time entrypoint. It is not a lowered
procedure yet.

Root requests are concrete entrypoint obligations. They are not the same thing
as public exports. A public generic function export is represented in
`ExportTable` as an exported checked procedure template plus its checked source
type. It does not force mono MIR to lower the generic body, and it does not
become a `RootProcedureRequest` until some consumer requests a concrete
monomorphic function type for a concrete runtime, tool, test, REPL,
development, or compile-time entrypoint.

Provided exports split into two cases:

- A concrete required export whose ABI, source type, and exposure require a
  runtime entrypoint becomes a root request with one concrete requested function
  type.
- A generic package export, generic module export, or first-class function export
  with no concrete runtime entrypoint remains a checked procedure template
  reachable through the export table. Importers specialize it later by adding
  concrete `MonoSpecializationRequest` values. The exporting module must not
  lower every exported checked procedure just because it is public.

This distinction is mandatory for static dispatch. A generic exported procedure
may contain checked `StaticDispatchCallPlan` records. Those records are resolved
only when an importer or root requests a concrete mono specialization. Exporting
the template does not require and must not attempt method-owner lookup.

Conceptual pre-MIR request shape:

```zig
const RootProcedureRequest = struct {
    id: RootRequestId,
    kind: RootKind,
    module: ModuleId,
    source: RootSource,
    checked_type: CheckedTypeId,
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
    comptime_callable_binding,
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

`RootSource` points at checked source origin. It does not name an executable
procedure. Mono MIR binds the request to a mono procedure or constant by
lowering the source in the appropriate specialization.

For a procedure root, `checked_type` is the concrete requested checked function
type for that root. If a source definition is still generic at this point, root
binding first instantiates it to the requested concrete function type and then
creates one `MonoSpecializationRequest`. A root request must never mean "lower
all specializations of this source procedure" or "lower the generic template as
mono MIR."

Conceptual lowered binding shape:

```zig
const LoweredRoot = struct {
    request: RootRequestId,
    target: RootTarget,
};

const RootTarget = union(enum) {
    proc: MonoSpecializedProcRef,
    const_instance: ConstInstanceRef,
    callable_binding_instance: CallableBindingInstanceRef,
    procedure_binding: TopLevelProcedureBindingRef,
};
```

Runtime roots bind to procedures. Serializable compile-time constants bind to
concrete `ConstInstanceRef` values after evaluation. Compile-time callable
binding roots bind to concrete `CallableBindingInstanceRef` values when the
binding is concrete, or to sealed `TopLevelProcedureBindingRef` entries whose
body is `callable_eval_template` when the binding is generalized and requires a
future concrete function type. Private aggregate interpreter roots may bind to
procedures while compile-time evaluation is running, but those roots are
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
    by_abi_name: Map(ExternalSymbolNameId, HostedProcId),
};

const HostedProcEntry = struct {
    id: HostedProcId,
    module: ModuleId,
    def: DefId,
    expr: ExprId,
    source_name: ExportNameId,
    abi_name: ExternalSymbolNameId,
    order: HostedOrderKey,
    representation: HostedRepresentationCapabilityKey,
    call_boundary_rc_template: CallBoundaryRcTemplate,
};
```

`CallBoundaryRcTemplate` is explicit external ABI metadata for hosted,
platform, and intrinsic wrapper procedures. It records only the reference-count
actions required at the external boundary and whether the external call may
attempt runtime-uniqueness mutation of any refcounted argument. Ordinary Roc
user procedures do not get per-procedure semantic parameter-mode summaries in
this plan.

`HostedOrderKey` gives deterministic ordering for hosted procedure tables and
generated ABI lists. It is not stored by writing an index into checked CIR.

The final architecture must delete or make non-authoritative any
`e_hosted_lambda.index`-style field. If a syntax node still carries a placeholder
for diagnostics while checking is running, post-check stages must ignore it and
consume `HostedProcTable`.

Conceptual platform-required declaration and binding tables:

```zig
const PlatformAppRelationKey = struct {
    bytes: [32]u8,
};

const PlatformRequirementContextKey = struct {
    bytes: [32]u8,
};

const PlatformRequiredDeclarationTable = struct {
    entries: Span(PlatformRequiredDeclaration),
    by_platform_required: Map(RequiredTypeId, PlatformRequiredDeclarationId),
};

const PlatformRequiredDeclaration = struct {
    id: PlatformRequiredDeclarationId,
    platform_required: RequiredTypeId,
    platform_name: ExportNameId,
    declared_source_ty: CanonicalTypeSchemeKey,
    for_clause_aliases: Span(PlatformRequiredAlias),
};

const PlatformRequiredBindingTable = struct {
    entries: Span(PlatformRequiredBinding),
    by_declaration: Map(PlatformRequiredDeclarationId, PlatformRequiredBindingId),
};

const PlatformRequiredBinding = struct {
    id: PlatformRequiredBindingId,
    relation: PlatformAppRelationKey,
    declaration: PlatformRequiredDeclarationId,
    app_value: TopLevelValueRef,
    requested_source_ty: CanonicalTypeKey,
    checked_relation: PlatformRequirementRelationId,
    value_use: PlatformRequiredValueUse,
};

const TopLevelValueRef = struct {
    artifact: CheckedModuleArtifactKey,
    pattern: PatternId,
};

const PlatformRequiredValueUse = union(enum) {
    const_value: ConstUseTemplate,
    procedure_value: ProcedureUseTemplate,
};
```

`PlatformAppRelationKey.bytes` is the hash of the sealed app
`CheckedModuleArtifactKey` and `PlatformRequirementContextKey`.
`PlatformRequirementContextKey.bytes` is the hash of the platform
`ModuleIdentity` plus the identity hash of `PlatformRequiredDeclarationTable`,
including each requirement's canonical platform name and declared source type
scheme. The app root artifact includes `PlatformRequirementContextKey` in its
`checking_context_identity`; the executable platform root artifact includes
`PlatformAppRelationKey` in its `checking_context_identity`. These keys
deliberately do not hash target/layout ABI inputs. Platform-required bindings
are checked-artifact data, and checked artifacts are target-independent. They
also deliberately do not use only a requirement count; changing
`requires {} { main : Str }` to `requires {} { main : I64 }` with the same app
artifact must produce a different platform/app relation key.

These tables use canonical published names. `source_name`, `abi_name`, and
`platform_name` are not raw `Ident.Idx` handles; they have already crossed the
canonical identity boundary while the owning `Ident.Store` was still known.
`abi_name` is the external symbol spelling that hosted/platform ABI codegen must
use. `source_name` and `platform_name` are the checked Roc names used for
diagnostics, ordering, and artifact identity.

The declaration table is platform-owned source data. It records what the
platform requires, using canonical names and canonical checked source type
schemes, without guessing which app will satisfy the requirement.

The binding table must never copy artifact-local canonical-name ids from the app
artifact into the platform artifact. A required app value is identified by the
app `CheckedModuleArtifactKey` plus a source binding id such as `PatternId` or a
sealed app `TopLevelProcedureBindingRef`. Any app display names needed for
diagnostics are used while checking is still running; post-check platform
artifacts do not store foreign `ModuleIdentity` ids from another artifact's
`CanonicalNameStore`.

The binding table is app-specific executable data. It replaces post-check
lookup-target population. It records exactly which sealed app top-level value
satisfies each platform requirement and which checked relation proved it. It is
built only after the app root artifact has sealed its `TopLevelValueTable`,
`CompileTimeValueStore`, `ConstInstantiationStore`,
`CallableBindingInstantiationStore`, `SemanticInstantiationProcedureTable`, and
promoted procedure table. A required value can be a non-function constant or a
procedure value:

- non-function required values use `PlatformRequiredValueUse.const_value` and
  point at an app `ConstRef` plus the exact requested source type
- function required values use `PlatformRequiredValueUse.procedure_value` and
  point at a sealed app procedure binding or concrete callable-binding instance
  at the platform-requested function type

Required values are not always procedures. For example, this is valid Roc:

```roc
platform "echo-in-zig"
    requires {} { main : Str }
    exposes []
    packages {}
    provides [main_for_host]

main_for_host : Str
main_for_host = main
```

Here `main` is a required constant. The platform root artifact must classify the
`main` lookup as `platform_required_const`, not synthesize a procedure wrapper.

Mono MIR consumes the binding table when producing roots and when lowering
required lookups in platform code. It must not lower `e_lookup_required` by
looking up a platform-local procedure, by constructing a generated
`platform_required_wrapper`, or by re-searching the app exports.

Debug-only artifact verification must assert that every `e_lookup_required`
reachable from an executable platform artifact has a binding and that the
binding kind matches the final required source type. If the required type is not
a function, later stages must see a constant use. If the required type is a
function, later stages must see a procedure use. In release builds, the
equivalent invariant path is `unreachable`.

Hosted and platform methods that can be called through static dispatch must
also appear in the checked method registry as explicit procedure targets with
checked callable types. The hosted/platform tables provide ABI identity,
ordering, representation capability keys, and call-boundary RC templates. The method
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

### Interpreter Shim Runtime Image Boundary

`roc run` in all optimization modes, `roc build --opt=interpreter`, and any
interpreter-shim host path must split compilation from execution at
ARC-inserted LIR, not at `ModuleEnv`, CIR, checked artifacts, MIR, or IR.

The parent compiler process owns every semantic stage:

```text
parse -> canonicalize -> check -> checked artifact publication
-> mono MIR -> row-finalized mono MIR -> lifted MIR -> lambda-solved MIR
-> executable MIR -> IR -> LIR -> ARC insertion
```

After ARC insertion, the parent publishes a target-specific `LirRuntimeImage`
or exact equivalent into the existing shared-memory infrastructure. It is an
offset-addressed shared-memory object graph containing the exact
LIR/runtime-layout arrays that the interpreter reads. The child interpreter
process maps the same shared-memory object and only does runtime work:

```text
map shared memory -> validate LIR runtime image header -> create zero-copy LIR views
-> initialize LIR interpreter -> call explicit roots
```

The child interpreter process must never:

- map or view `ModuleEnv`
- inspect CIR
- inspect checked artifacts
- run MIR, IR, LIR lowering, ARC insertion, static dispatch resolution,
  root selection, hosted/platform binding lookup, or compile-time evaluation
- scan source, exports, declarations, expressions, `ModuleEnv` definitions, or
  platform module definitions to find entrypoints
- recover missing semantic data
- create anything other than zero-copy views over the mapped LIR/runtime-layout
  arrays

For IPC paths such as `roc run`, the transport mechanism must use the existing
`SharedMemoryAllocator`/shared-memory coordination infrastructure. The payload
is a viewable LIR runtime image, not a live or cached `ModuleEnv` and not a
checked artifact. Shared memory is the allocator for the runtime image at this
boundary. The child process turns offsets in the mapped region into read-only
views.

IPC execution must not serialize or deserialize the LIR runtime image between
the parent and child processes. The parent allocates and fills the
offset-addressed image in the existing shared-memory region, then hands the child
the shared-memory mapping information. The child validates the header and builds
zero-copy views over the mapped bytes. Any serialization format used for checked
artifact caches, deterministic debug output, or a future file-backed embedded
runtime image is separate from the `roc run` shared-memory IPC path and must not
be reused as a parent-child transport.

For embedded interpreter builds, any file-backed runtime image must preserve the
same view-oriented contract: the embedded payload is made viewable as the LIR
runtime image before interpretation, and the child/interpreter side still does
not run semantic compiler stages, root discovery, or reconstruction from CIR.
The embedded path must not dictate or slow down the shared-memory IPC path.

Conceptual shape:

```zig
const LirRuntimeImageHeader = extern struct {
    magic: u32,
    format_version: u32,
    image_size: u64,
    target_usize: u8,
    root_procs: ArrayRef,
    platform_entrypoints: ArrayRef,
    store: LirStoreImage,
    layouts: LayoutStoreImage,
    literal_pool: ProgramLiteralPoolImage,
    hosted_table: HostedProcTableImage,
};

const ArrayRef = extern struct {
    offset: u64,
    len: u64,
    capacity: u64,
};

const LirRuntimeImageView = struct {
    header: LirRuntimeImageHeader,
    root_procs: []LirProcSpecId,
    platform_entrypoints: []PlatformEntrypointRoot,
    store: LirStoreView,
    layouts: CommittedLayoutStoreView,
    literal_pool: ProgramLiteralPoolView,
    hosted_table: HostedProcTableView,
};
```

The exact Zig names may differ, but the ownership rule must not. The image
contains target-shaped runtime data that the interpreter needs to execute
already-lowered LIR, allocated in the shared-memory runtime image for IPC. It
may contain LIR proc ids, LIR locals, LIR statements, committed layouts,
explicit RC statements, literal bytes, hosted procedure descriptors, root proc
ids, and platform entrypoint-to-root mappings. It must not contain `ModuleEnv`,
CIR ids, checked expression ids, checked pattern ids, checker type variables,
checked artifact views, MIR ids, IR vars, raw `Ident.Idx`, raw source text as
semantic data, or post-check lookup keys that require semantic compiler stages
to run in the child. Any string or literal ids present in the LIR runtime image
must refer only to runtime-image-local LIR literal/string stores, not to
`ModuleEnv` or checked-artifact stores.

Platform entrypoints must be resolved in the parent from published checked
artifacts and platform-required binding tables before runtime-image
publication. The runtime image stores direct LIR root proc ids for those
entrypoints. It must not store
platform def indices, app def indices, `CIR.Def.Idx`, exported-name lookup
requests, or offsets to `ModuleEnv` values.

The child may validate the runtime image header, compiler/runtime image version,
target pointer width, byte order, and structural bounds before interpretation.
These checks are invariant checks over compiler-produced shared memory, not a
semantic fallback path. Header, bounds, missing-root, missing-layout,
missing-proc, missing-hosted-binding, or malformed-RC violations are compiler
or runtime-image publication bugs: debug builds assert at the first invalid
read, and release builds use `unreachable`.

This boundary is target-specific and intentionally outside the target-independent
checked artifact cache. Checked artifact cache keys must not include layout or
target ABI inputs. Any future cache for viewable runtime images must use
target/layout/compiler-runtime inputs and must preserve the same zero-copy view
contract. The `roc run` and dev-shim IPC paths continue to use the existing
shared-memory allocator handoff.

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

### Canonical Identity Boundary

Checking finalization must convert store-local names and mutable lowering handles
into canonical identities before any data crosses the checked-artifact boundary or
a MIR-family type-state boundary that can be cached, imported, compared across
modules, or consumed by a later stage without the original store.

`Ident.Idx` is a handle into one exact `Ident.Store`. It is valid only while the
owning store is known and available. It is not a canonical name and must not be
stored in row-shape keys, method keys, static-dispatch plans consumed by mono
MIR, checked-artifact cache keys, compile-time value graphs, executable
specialization keys, callable-set keys, capture-shape keys, erased adapter keys,
or LIR/backend semantic inputs.

`Symbol` is a dense in-memory binding handle. It is useful for local variables,
temporary generated names, debug printing, and backend naming after lowering has
selected a concrete procedure. It is not sufficient procedure identity for
semantic comparison, cache keys, imported artifacts, callable leaves, executable
specialization selection, or compile-time value serialization.

Canonical names are interned by exact canonical bytes:

```zig
const CanonicalNameId = enum(u32) { _ };

const CanonicalNameStore = struct {
    names: InternMap([]const u8, CanonicalNameId),
};

const RecordFieldLabelId = distinct CanonicalNameId;
const TagLabelId = distinct CanonicalNameId;
const MethodNameId = distinct CanonicalNameId;
const ExportNameId = distinct CanonicalNameId;
const ExternalSymbolNameId = distinct CanonicalNameId;
```

The exact Zig names may differ, but the boundary must not. The byte sequence is
the canonical spelling that the checker uses for the semantic lookup being keyed:
record field labels use the field label bytes, tag labels use the constructor
label bytes, method names use the method name bytes, export names use the
source-visible export name bytes, and external symbol names use the ABI symbol
bytes. Distinct wrappers prevent accidentally using a method name as a row label
or an external ABI name as a source export name.

Conversion from `Ident.Idx` to a canonical name may happen only at a point where
the implementation has the exact owning `Ident.Store`. If a value can be imported,
serialized, cached, or compared after the owning store is gone, it must already be
a canonical name id or a higher-level canonical key. A later stage must not
recover canonical identity by guessing which `Ident.Store` an `Ident.Idx` came
from, by comparing display text in the wrong store, or by reinserting names into a
fresh local store and treating the new local indexes as equivalent.

Dense canonical name ids are local to the `CanonicalNameStore` that produced
them unless the implementation has explicitly chosen a single global
content-addressed canonical-name store for all artifacts. The required final
architecture does not depend on such a global store. It requires imported
artifact-local canonical name ids to be remapped through the published
`CanonicalNameView` and the lowering-run `CanonicalNameStore` before they enter
MIR. Later stages must never compare `RecordFieldLabelId`, `TagLabelId`,
`MethodNameId`, `TypeNameId`, `ModuleNameId`, `ExportNameId`, or
`ExternalSymbolNameId` values that came from different artifact-local stores.

Remapping from published canonical bytes is not source reconstruction. The bytes
are checked artifact data produced during checking finalization. Re-reading an
exporter's `ModuleEnv`, `Ident.Store`, source text, export table, or declaration
environment to recover these names is forbidden.

Debug-only artifact and MIR verifiers must reject:

- any exported, cached, or imported key that contains raw `Ident.Idx`
- any MIR payload that contains an imported artifact-local canonical name id
  instead of a lowering-run canonical name id
- any row-shape, method, static-dispatch, callable-set, capture-shape, erased
  adapter, executable-specialization, or compile-time value key that contains raw
  `Symbol`
- any cached accelerator field whose canonical derivation disagrees with the
  canonical key it is supposed to accelerate

Release builds must not retain verifier metadata or deletion-scan metadata for
these checks. If such an invariant violation is reached in release code, the
release path is `unreachable`.

### Procedure Identity

Each MIR-family stage must distinguish semantic procedure identity from local
procedure handles. `call_proc`, `proc_value`, callable leaves, erased code refs,
and executable specialization keys use `ProcedureValueRef`,
`ProcedureCallableRef`, `ProcBaseKeyRef`, and `ExecutableSpecializationKey`.
Implementation-local `Symbol` or dense ids may cache the selected procedure for a
store that is currently alive, but exported semantic nodes must not use raw
symbols as the identity being compared.

The required invariant is:

```text
procedure value identity is stored in `ProcedureValueRef`
procedure callable occurrence identity is stored in `ProcedureCallableRef`
source/MIR procedure call target identity is stored in `call_proc`
executable direct call target identity is stored in `call_direct`
```

If a stage rewrites procedure identities, it must carry an explicit map:

```text
old ProcBaseKeyRef -> new ProcBaseKeyRef
```

or an implementation-local handle map paired with the exact live store that owns
those handles.

It must not recover targets from names, expression shapes, or source lookup.

Every procedure definition produced by mono MIR or any later MIR-family stage
must carry distinct identities for the checked template being instantiated, the
mono-specialized output procedure, and any nested/lifted procedure site:

```zig
const NestedProcSiteKey = struct {
    owner_template: ProcedureTemplateRef,
    site: NestedProcSiteId,
};

ProcBaseKey {
    module_idx: u32,
    source_def_idx: ?CIR.Def.Idx,
    nested_proc_site: ?NestedProcSiteKey,
    owner_mono_specialization: ?MonoSpecializationKey,
    synthetic_origin: SyntheticOrigin,
}

SourceProcKey {
    module_idx: u32,
    source_def_idx: CIR.Def.Idx,
    nested_proc_site: ?NestedProcSiteKey,
}

ProcedureTemplateRef {
    proc_base: ProcBaseKeyRef,
    template: CheckedProcedureTemplateId,
}

LiftedProcedureTemplateRef {
    owner_mono_specialization: MonoSpecializationKey,
    site: NestedProcSiteId,
}

SyntheticProcedureTemplateRef {
    template: ProcedureTemplateRef,
}

CallableProcedureTemplateRef {
    checked: ProcedureTemplateRef,
    lifted: LiftedProcedureTemplateRef,
    synthetic: SyntheticProcedureTemplateRef,
}

MonoSpecializationKey {
    template: ProcedureTemplateRef,
    requested_mono_fn_ty: CanonicalTypeKey,
}

ConcreteSourceTypeRef {
    local_id: u32,
}

MonoSpecializedProcRef {
    proc: ProcedureValueRef,
    specialization: MonoSpecializationKey,
}

ExecutableSpecializationKey {
    base: ProcBaseKey,
    requested_fn_ty: CanonicalTypeKey,
    exec_arg_tys: Span(CanonicalExecValueTypeKey),
    exec_ret_ty: CanonicalExecValueTypeKey,
    callable_repr_mode: CallableReprMode,
    capture_shape: CaptureShapeKey,
}

ProcOrderKey {
    base: ProcBaseKey,
    specialization_order_component: ?CanonicalOrderComponent,
}
```

The exact Zig field names may differ, but the separation must not.

`ProcedureTemplateRef` names a checked procedure-template table entry.
`CallableProcedureTemplateRef` is the procedure-template identity used by
first-class callable values and callable leaves, because not every callable
procedure template is an ordinary checked top-level source procedure. The
`checked` case names source-defined or imported checked procedures, the `lifted`
case names local functions/closures by owner mono specialization plus nested
procedure site, and the `synthetic` case names compiler-created procedure
templates such as promoted compile-time callable results. `MonoSpecializationKey`
uses the checked `ProcedureTemplateRef` for ordinary checked-template
instantiation; lifted and synthetic callable templates must enter through their
explicit callable-template case instead of pretending to be top-level source
procedures.

`ProcedureTemplateRef` is not the identity of the mono-specialized output
procedure. `MonoSpecializedProcRef` names the output of a concrete mono request
and pairs the reserved `ProcedureValueRef` with the `MonoSpecializationKey` that
produced it. Any API that requests ordinary checked-template mono lowering must
accept a `ProcedureTemplateRef`; any API that emits a direct mono call must use
the corresponding `MonoSpecializedProcRef` or its `ProcedureValueRef`. Passing a
mono output procedure back as a template key is invalid by construction and must
fail debug verification if it is ever represented.

This separation is required for generic procedures and generic callable leaves.
For example, `id` is one checked procedure template. The two requested function
types `I64 -> I64` and `Str -> Str` produce two distinct mono-specialized output
procedures. Both outputs point back to the same `ProcedureTemplateRef`; neither
output becomes a new template. Cor/LSS models this by using the original symbol
plus requested type as the specialization key and then allocating a fresh output
symbol for the specialized body. Roc must preserve that separation with explicit
semantic ids instead of raw symbols.

`MonoSpecializationKey.requested_mono_fn_ty` is the identity of the requested
source function type, not the payload used to lower the body. The matching
`MonoSpecializationRequest` and queue entry must retain a `ConcreteSourceTypeRef`
whose canonical key is `requested_mono_fn_ty`. `ConcreteSourceTypeRef` is scoped
to the current MIR-family lowering/checking-finalization construction run and is
never serialized as a stable semantic id. Any API that can enqueue a mono
specialization from only `ProcedureTemplateRef + CanonicalTypeKey` is incomplete
and must be rejected during the plan audit.

`ProcBaseKey` is the stable semantic origin of a source, lifted, or synthetic
procedure. `nested_proc_site` is null for ordinary top-level source procedures.
It is set for a nested source procedure, local function, closure, or desugared
closure site recorded in the owning checked template. `owner_mono_specialization`
is null for ordinary top-level checked templates and is set for lifted local
procedures whose body and capture slots are produced inside a particular
monomorphic owner specialization. Two lifted local procedures with the same
source definition and same nested site but different owning mono specializations
are different procedures.

`source_def_idx`, `nested_proc_site`, and `owner_mono_specialization` are the
single source of truth for lifted-local procedure identity. Do not duplicate
lifted-local owner identity in `synthetic_origin`.

Nested procedure sites are precomputed during checking finalization and stored in
the owning checked template:

```zig
const NestedProcSiteTable = struct {
    owner_template: ProcedureTemplateRef,
    sites: Span(NestedProcSite),
};

const NestedProcSite = struct {
    site: NestedProcSiteId,
    site_path: Span(NestedProcPathComponent),
    kind: NestedProcKind,
    checked_expr: ?CheckedExprId,
    checked_pattern: ?CheckedPatternId,
};

const NestedProcKind = enum {
    local_function,
    closure,
    desugared_closure,
};
```

The `site_path` is a stable structural path through the checked template, but it
is not a late lookup mechanism. It is used while building the table, for debug
verification, deterministic serialization, and artifact diffs. Later stages
refer to the reserved `NestedProcSiteId`. The table must reserve a site for
every local function, closure expression, and compiler-created desugared closure
that can become a lifted procedure. Identity must not depend on generated names,
body equality, traversal order in a lowering pass, or whether the closure later
captures values. Compiler-created non-source top-level procedures that are not
nested inside a checked template use `SyntheticOrigin`; they must not invent fake
nested site paths.

`synthetic_origin` distinguishes ordinary source/lifted procedures from
compiler-created non-source procedures such as erased adapters, intrinsic
wrappers, entrypoint wrappers, and bridges. It must carry payload keys, not only
a kind tag:

```zig
const SyntheticOrigin = union(enum) {
    none,
    erased_adapter: struct {
        source_fn_ty: CanonicalTypeKey,
        callable_set_key: CanonicalCallableSetKey,
        erased_fn_sig_key: ErasedFnSigKey,
        capture_shape_key: CaptureShapeKey,
    },
    bridge: struct {
        from_exec_ty: CanonicalExecValueTypeKey,
        to_exec_ty: CanonicalExecValueTypeKey,
        reason: BridgeReason,
    },
    intrinsic_wrapper: struct {
        intrinsic_id: IntrinsicId,
        requested_fn_ty: CanonicalTypeKey,
    },
    entry_wrapper: struct {
        root_name: ExportNameId,
        target_proc: ProcBaseKeyRef,
        target_fn_ty: CanonicalTypeKey,
    },
    promoted_callable: struct {
        artifact: CheckedModuleArtifactKey,
        source_binding: PatternId,
        callable_node: PromotedCallableNodeId,
        source_fn_ty: CanonicalTypeKey,
    },
};
```

No synthetic procedure identity may be keyed only by display name, generated
symbol, expression id, side-table id, or a payload-free origin kind.
`promoted_callable` identity is keyed by the checked artifact that owns the
promotion, the source binding that produced the callable result, the stable
promoted callable node, and the canonical source function type. This is the
only synthetic-origin path for compile-time callable promotion; later stages
must not recover promoted-procedure identity from `TopLevelValueTable`, symbol
spelling, private capture graph shape, or callable body syntax.

All key-like fields above are canonical keys, not handles into mutable stores.
`CanonicalCallableSetKey` is the key for an interned
`CanonicalCallableSetDescriptor`:

```zig
const CanonicalCallableSetDescriptor = struct {
    members: Span(CanonicalCallableSetMember),
};

const CanonicalCallableSetMember = struct {
    member: CallableSetMemberId,
    proc_value: ProcedureCallableRef,
    capture_slots: Span(CallableSetCaptureSlot),
    capture_shape_key: CaptureShapeKey,
};

const CallableSetCaptureSlot = struct {
    slot: CaptureSlot.Index,
    source_ty: CanonicalTypeKey,
    exec_value_ty: CanonicalExecValueTypeKey,
};
```

The exact Zig names may differ, but this descriptor is the canonical ordered
finite member map plus capture-slot shape and capture types. Each member entry
identifies the selected procedure value occurrence and the member's capture-slot
schema. Therefore `callable_set_key + member` is enough to derive the member
procedure, tag/discriminant order key, capture shape, and capture slot types.
Later records may cache those derived values only as debug-verified
accelerators; they must not treat them as separate semantic sources.

`CanonicalCallableSetMember.proc_value.source_fn_ty` is part of member identity.
Two entries with the same callable procedure template but different canonical
source function types are different finite callable-set members. This is
required for generic procedures: a single checked template can produce a finite
callable leaf at `I64 -> I64` and another at `Str -> Str`, and those leaves must
reserve different mono/executable specializations even though their source
template is the same. Descriptor interning must therefore include both the
procedure-template identity and the canonical `source_fn_ty`.

Any occurrence-local construction plan and any `callable_match` branch that
names a descriptor member must carry the same canonical `source_fn_ty` as the
member's `ProcedureCallableRef`. This equality is debug-verified at the boundary
where the construction plan or branch consumes the descriptor. Later stages must
not infer the requested function type from the member body, from the callee
syntax, from argument count, from generated procedure names, or from executable
layout compatibility.

The descriptor must not contain `ExecutableSpecializationKey`, executable
procedure ids, layout ids, generated symbol text, expression ids, side-table ids,
LIR temporaries, runtime function pointers, runtime capture pointers, ARC
placement data, or backend ABI handles. Those belong to later executable, IR,
LIR, ARC, or backend stages. If a later stage needs executable code for a member,
it derives or reserves that code from the descriptor plus the requested
executable call/adapter/materialization context.

`ErasedFnSigKey` is the canonical fixed-arity erased function argument and return
signature plus canonical hidden capture type and `ErasedFnAbiKey`.
`CaptureShapeKey` is the canonical `CaptureSlot.index` ordered capture layout
and capture representation. Each capture slot in the key stores a
`CanonicalExecValueTypeKey` for that captured value, or the canonical erased
capture type key when the slot is part of an erased hidden capture record. It
must not store a checked source type id, lambda-solved `TypeId`, executable
`TypeId`, layout id, source name, generated symbol text, or expression id.
`ProcBaseKeyRef` is a canonical reference to an already-keyed procedure.
`BridgeReason`, `IntrinsicId`, and entry wrapper root names must be stable enum
or canonical source identities, never generated symbol text and never raw
`Ident.Idx`.

`ExecutableSpecializationKey` is the semantic key for executable specialization
deduplication. It contains `ProcBaseKey` and canonical fully resolved structural type
keys. It must not contain `ProcOrderKey`, raw type-store ids, expression ids,
side-table ids, or allocation-order-dependent data.

`CanonicalExecValueTypeKey` is the canonical runtime value representation key.
It is not the checked source type and not merely the logical executable type
before callable lowering. For a function-typed value, it recursively encodes the
solved callable child of that function representation:

```text
function value with finite callable child -> callable_set(CanonicalCallableSetKey)
function value with erased callable child -> erased_fn(ErasedFnSigKey)
```

The key recursively applies this rule through records, tuples, tag payloads,
`List(T)`, `Box(T)`, nominal backing slots when visible through an explicit
capability, function argument slots, and function return slots. Therefore two
values with the same source function type can have different executable value
keys when their solved callable representations differ.

For example, these two executable specializations must not share one key:

```text
id : (I64 -> I64) -> (I64 -> I64)

id called with a closure whose callable child is [AddN { n : I64 }]
id called with a closure whose callable child is [Identity]
```

The source argument type is `I64 -> I64` in both calls, but the executable
argument value keys are different:

```text
callable_set([AddN { n : I64 }])
callable_set([Identity])
```

A single `callable_repr_mode` field cannot stand in for this recursive
representation. It may describe the procedure's own top-level callable packaging
mode if the implementation keeps such a field, but nested callable
representations in arguments, returns, captures, records, tags, lists, boxes, and
nominals must be encoded inside `CanonicalExecValueTypeKey`.

`ProcOrderKey` is for deterministic ordering and reproducibility only. A
base-only `ProcOrderKey` may exist before executable specialization so lifted
recursive groups can order members deterministically. When an executable
specialization exists, the specialization-specific order component is derived
after the semantic specialization key exists. A generated `Symbol` may be the
local printed/backend name for an already-selected procedure, but it is not the
public semantic procedure identity used by call nodes.

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
    template: CheckedProcedureTemplateId,
};

const HostedProcTarget = struct {
    host_symbol: ExternalSymbolNameId,
    dispatch_index: u32,
    representation_abi: ProcRepresentationAbi,
    call_boundary_rc_template: CallBoundaryRcTemplate,
};

const IntrinsicWrapperTarget = struct {
    intrinsic_id: IntrinsicId,
    representation_abi: ProcRepresentationAbi,
    call_boundary_rc_template: CallBoundaryRcTemplate,
};
```

`ProcTarget` is procedure metadata attached to the `ProcBaseKeyRef` selected by a
procedure value or executable specialization. Later stages read the target
metadata from the selected procedure definition; they must not rediscover whether
a procedure is user code, hosted code, or an intrinsic wrapper from names or
source syntax. Hosted and intrinsic targets must carry their representation ABI
and call-boundary RC templates here before mono MIR output is exported.
Lambda-solved MIR, executable MIR, IR, LIR, and backends must consume this
metadata; they must not recover it from method names, host symbol names, layout
shapes, runtime function pointers, generated `Symbol` values, or surrounding user
code.

### Proc Calls, Direct Calls, And Value Calls

MIR must distinguish procedure-value calls, executable direct calls, and
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
    proc: ProcedureValueRef,
    captures: Span(CaptureArg),
    fn_ty: TypeId,
}

call_proc {
    proc: ProcedureValueRef,
    args: Span(ExprId),
    requested_fn_ty: TypeId,
}

call_value {
    func: ExprId,
    args: Span(ExprId),
    requested_fn_ty: TypeId,
}
```

`proc_value` exists when a source/MIR procedure value is used as a value.

For a top-level source procedure value, `proc_value.captures` is empty.

Mono MIR must reserve mono specializations for both direct procedure calls and
procedure values.

When mono MIR lowers a direct source procedure call, it must request or reserve
the target mono specialization at the exact requested mono source function type
and store the returned mono-specialized procedure value in `call_proc.proc`.

When mono MIR lowers a top-level procedure value used as a first-class value,
it must request or reserve the target mono specialization at the exact requested
mono source function type and store the returned mono-specialized procedure
value in `proc_value.proc` with empty captures.

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
output `ProcBaseKeyRef` and any implementation-local output handle before
lowering the specialization body, so recursive references to the same procedure
value reuse the same reserved procedure instead of constructing a second
specialization.

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
    proc: ExecutableProcId,
    args: Span(ExecutableValueRef),
}

call_erased {
    func: ExecutableValueRef,
    args: Span(ExecutableValueRef),
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
    result_tmp: TempId,
}

source_match {
    id: SourceMatchId,
    union_shape: TagUnionShapeId,
    matched_expr: ExprId,
    matched_tmp: TempId,
    discr_tmp: TempId,
    branches: Span(SourceMatchBranch),
    result_ty: ExecTypeId,
    result_tmp: TempId,
}
```

`call_direct` exists only in executable MIR, IR, and LIR.

A `call_direct` target must be an executable specialization whose procedure
definition is present in executable MIR. The verifier must check that the call's
argument value refs and result type match that procedure definition exactly.

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
    nominal_type: NominalTypeKey,
};

const MethodKey = struct {
    owner: MethodOwner,
    method: MethodNameId,
};

const MethodDefRef = struct {
    artifact: CheckedModuleArtifactKey,
    def_idx: CIR.Def.Idx,
};

const HostedProcRef = struct {
    artifact: CheckedModuleArtifactKey,
    host_symbol: ExternalSymbolNameId,
    dispatch_index: u32,
    representation_abi: ProcRepresentationAbi,
    call_boundary_rc_template: CallBoundaryRcTemplate,
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
expression shape. Nominal owners use the canonical nominal type key of the
defining checked artifact and type declaration, not a raw module-local
identifier. Primitive, `List`, and `Box` owners are explicit builtin owner cases.
Type-var aliases and transparent aliases must resolve to a `MethodOwner` before
registry lookup. `MethodKey.method` is a canonical method name id, not an
`Ident.Idx`.

The registry is an input to mono MIR only.

It is not part of lifted MIR, lambda-solved MIR, executable MIR, IR, or LIR.

The registry must be built from checked declaration outputs. It must not rely on
late text lookup or module-name scanning during MIR lowering.

Checker validation and mono MIR lowering must agree through this registry.

The checker may keep `StaticDispatchConstraint` as the legality mechanism, but
checked CIR must export a normalized `StaticDispatchCallPlan` for every
expression classified as static dispatch. The plan stores the dispatcher type
variable, callable type variable, canonical method name, ordered value arguments,
and equality behavior. It does not store a final target procedure.

Mono MIR resolves every static dispatch target from:

```text
the checked method registry
+ the plan's dispatcher type variable after mono instantiation and full type-link resolution
+ the plan's canonical method name
```

That is the only target-selection path. There must not be one checker lookup
path and a second mono MIR lookup path that can disagree, and there must not be
separate post-check paths for ordinary method syntax, type-variable qualified
syntax, or equality syntax.

The registry returns checked method target identity and ABI metadata needed to
create a `ProcTarget`, not a final executable procedure.

Mono MIR lowering must pass the selected method target and the exact requested
mono source function type through the mono specialization queue or wrapper
synthesis path. The queue returns the mono-specialized `ProcedureValueRef` stored
in `call_proc`.

Before mono MIR output is exported, every callable method target must be
normalized to a procedure value with `ProcTarget` metadata:

- ordinary source methods become `ProcTarget.user_proc`
- hosted/platform methods become `ProcTarget.hosted_proc`
- first-class intrinsic method references synthesize a wrapper procedure and
  become `ProcTarget.intrinsic_wrapper`

The `ProcTarget` metadata must include representation ABI records and
call-boundary RC templates for hosted, platform, and intrinsic-wrapper
procedures before mono MIR is exported. Static dispatch lowering must not leave
behind a method name or owner key for later ABI discovery.

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
method registry key. It is not a runtime value concept and is unrelated to
reference counting.

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
`StaticDispatchCallPlan.callable_ty` result type for the already-lowered call.

The next dispatch in a chain has its own `StaticDispatchCallPlan`. That plan's
`dispatcher_ty` must be the checked type root selected by the checker for that
dispatch site. If that type root is the result of an earlier call, mono MIR gets
the value by instantiating the plan's checked type root in the current mono
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
- a fresh callable variable for every imported function type occurrence
- lambda/callable members
- captures for each callable member
- erased callable representation when required
- boxed erased-boundary payload type transforms for boxing and unboxing

The lifted-to-lambda-solved import pass is the only place where lifted function
types become lambda-solved function types. It must allocate callable variables
for function types found in parameters, returns, captures, records, tuples,
tags, `List(T)`, `Box(T)`, nominals, and nested function argument and return
positions. It must not wait for executable MIR to add callable slots, and it
must not use equal source `TypeId`s as a reason to reuse callable variables
between unrelated value occurrences.

It must not use ordinary source tag unions as a hidden carrier for unresolved
static dispatch.

If physical layout later uses a tag-union-like representation for callable
sets, that is a layout decision derived from explicit callable metadata.

Lambda-solved MIR owns `erased_box_payload_type(T)`.

This transform is structural only inside an explicit `Box(T)` payload. It
recursively rewrites reachable function slots to erased callable representation
inside the boxed payload, including through nested records, tuples, tags,
`List(T)`, nested `Box(T)`, function argument and return positions, and nominal
backing types when they are part of that payload. It is the same contract for
boxing and unboxing boundaries. Executable MIR consumes the already-computed
boxed payload boundary type and `BoxPayloadRepresentationPlan`.

The function case is explicit. For a boxed payload function type, lambda-solved
MIR recursively transforms every fixed-arity argument slot and the return slot,
then rewrites the callable child to erased callable representation. A function
inside an explicit `Box(T)` payload is never treated as an opaque terminal leaf
whose argument and return slots remain in natural representation.

This transform is not a runtime conversion plan. It is a representation
requirement propagated by lambda-solved MIR. Any structural node in the plan
exists only to route the boxed payload requirement to nested callable leaves.

Calling this transform on a non-`Box(T)` root is a compiler invariant violation
handled only by debug-only assertion in debug builds and `unreachable` in
release builds.

### Executable Types

Executable MIR types are representation types.

Executable value type keys use this conceptual shape:

```zig
const CanonicalExecValueTypeKey = union(enum) {
    primitive: PrimitiveExecKey,
    record: RecordExecKey,
    tuple: TupleExecKey,
    tag_union: TagUnionExecKey,
    list: *CanonicalExecValueTypeKey,
    box: *CanonicalExecValueTypeKey,
    nominal: NominalExecKey,
    callable_set: CanonicalCallableSetKey,
    erased_fn: ErasedFnSigKey,
};
```

The exact Zig layout may differ, but the semantic shape must not. Function-typed
source values do not appear as a `function` case in executable value keys.
Executable type lowering consumes the solved `FunctionRepShape.callable` child
and produces either `callable_set` or `erased_fn`.

Function argument and return executable value keys still exist as metadata for
calls, bridges, erased signatures, boxed payload transforms, and specialization
keys. They are reached from the lambda-solved `FunctionRepShape`, not from a
runtime function-object field. A nested function argument such as:

```text
(I64 -> I64) -> I64
```

has an executable parameter key that is `callable_set(...)` or `erased_fn(...)`
for the argument value, plus separate call metadata describing the outer
function's fixed arity and return representation.

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

Executable type lowering must debug-assert that every function-typed value has a
fully solved callable child before it publishes a `CanonicalExecValueTypeKey`.
In release builds, the equivalent compiler-invariant path is `unreachable`.

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

### Recursive Physical Layout Indirection

MIR and IR carry executable types and logical layout graph references. They do
not carry final physical layout indexes or physical offsets.

Physical layout commitment happens after executable MIR has built the logical
layout graph and before LIR needs physical storage operations. That commit must
be graph-based:

1. Reserve a logical layout graph node before lowering that node's children.
2. Record every field, tag payload, tuple element, capture slot, callable-set
   payload field, and erased capture field as an explicit slot edge.
3. Run SCC detection over the logical layout graph.
4. For every by-value slot edge whose source and target are in the same
   recursive SCC, commit that slot edge as physical recursive indirection.
5. Preserve the logical slot identity so constructors, pattern payload
   extraction, field access, and reference-count insertion
   all agree about which slot became indirect.

This is physical recursive layout indirection. It is not source `Box(T)`, not a
`BoxBoundary`, not erased callable representation, and not a semantic type
change. The only source-level erased boundary in this plan remains explicit
`Box(T)`.

Recursive tag unions need careful edge identity. A tag-union-to-payload-struct
edge is not automatically the slot that becomes indirect. The by-value payload
struct field that points back to the recursive union is the slot edge that
becomes indirect. For example, a recursive list-like union stores the payload
fields logically, and the recursive tail field is the edge that becomes
physically indirect.

Layout commitment must publish one recursive-slot mapping consumed by all later
layout users:

```zig
const RecursiveSlotCommit = struct {
    owner_layout: LogicalLayoutId,
    logical_slot: LogicalSlotId,
    physical_indirection: RecursiveIndirectionId,
};
```

The exact Zig names may differ, but the responsibility must not. Constructors,
source `match` payload extraction, field access, capture access, callable-set
payload access, erased capture access, and RC plans must consume the same
committed recursive-slot mapping. A later stage must not independently decide
that a recursive field is direct or indirect by inspecting type syntax, layout
names, or physical offsets.

Debug verification after layout commit must assert that every recursive
by-value SCC edge has exactly one committed physical indirection and that no
non-recursive edge was made indirect by this rule. Release builds use
`unreachable` for the equivalent compiler-invariant path.

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
    fields: Span(RecordFieldLabelId),
};

const RecordFieldInfo = struct {
    owner: RecordShapeId,
    label: RecordFieldLabelId,
    logical_index: u32,
};

const TagUnionShapeKey = struct {
    tags: Span(TagShapeKey),
};

const TagShapeKey = struct {
    tag: TagLabelId,
    payload_count: u32,
};

const TagInfo = struct {
    owner: TagUnionShapeId,
    label: TagLabelId,
    logical_index: u32,
};

const TagPayloadInfo = struct {
    tag: TagId,
    payload_index: u32,
};
```

The exact Zig field names may differ, but the identity model must not. Shape
keys describe logical row order and payload arity only. The labels in shape keys
are canonical label ids, not `Ident.Idx` values. Row finalization may read
module-local `Ident.Idx` values from the mono type store, but it must immediately
convert them to `RecordFieldLabelId` or `TagLabelId` using the exact owning
`Ident.Store` before interning a shape or exporting a row-finalized node. They do
not include payload slot types. Payload slot types remain in the mono type store
and later representation edges, because the same logical row shape can appear at
multiple type instantiations.

The row-finalization algorithm is:

1. Walk every expression and pattern in one mono specialization exactly once.
2. For each record construction, record access, record update, record
   destructuring pattern, tag construction, tag pattern, and tag payload
   projection, read the operation's mono result type or input type from the mono
   MIR node.
3. Fully resolve that type in the specialization-local mono type store.
4. Derive the full logical record or tag-union row from that resolved type.
5. Convert every field/tag label in that row from its store-local `Ident.Idx` to
   the canonical label id for its exact semantic spelling.
6. Intern the full logical row shape in `RowShapeStore`.
7. Validate that the requested field or tag exists in that full row, and that
   the payload count in the operation matches the full row's constructor arity.
8. Rewrite the MIR node in place, or into a new row-finalized store, so it
   stores `RecordShapeId`, `RecordFieldId`, `TagUnionShapeId`, `TagId`, and
   `TagPayloadId` values instead of name-only row keys.
9. Attach or preserve the mono type slot for each field or payload edge so
   representation solving can connect value flow without looking names up again.
10. Export only row-finalized mono MIR.

Row-finalized construction nodes must make construction order explicit.
Construction evaluation order and construction assembly order are different
things, and both must survive this pass.

Conceptual shapes:

```zig
const RecordInit = struct {
    shape: RecordShapeId,
    eval_order: Span(RecordFieldEval),
    assembly_order: Span(RecordFieldAssembly),
};

const RecordFieldEval = struct {
    field: RecordFieldId,
    expr: ExprId,
};

const RecordFieldAssembly = struct {
    field: RecordFieldId,
    eval_index: u32,
};

const TagInit = struct {
    union_shape: TagUnionShapeId,
    tag: TagId,
    eval_order: Span(TagPayloadEval),
    assembly_order: Span(TagPayloadAssembly),
};

const TagPayloadEval = struct {
    payload: TagPayloadId,
    expr: ExprId,
};

const TagPayloadAssembly = struct {
    payload: TagPayloadId,
    eval_index: u32,
};
```

The exact Zig names may differ, but source-order ambiguity must not survive
this pass. `eval_order` records the source evaluation order for field or
payload expressions. `assembly_order` records how the already-evaluated
temporaries are placed into finalized logical slots. Executable MIR must
evaluate `eval_order` exactly once in source order and then assemble records,
tuples, or tag payload records from the resulting temporaries according to
`assembly_order`.

`assembly_order` is a deterministic mechanical conversion from row-finalized
IDs to logical slot order. It is not name lookup and it is not a layout
decision. Later stages must not reorder evaluation to match finalized logical
order, name sorting order, or physical layout order.

Record update uses the same split. The base record expression is evaluated once
before update field expressions when source semantics require that ordering.
Update field expressions are evaluated in source update order. Assembly then
uses finalized `RecordFieldId` values to construct the updated logical record.

Tag construction stores the full `TagUnionShapeId`, the selected `TagId`, and
payload entries keyed by `TagPayloadId`. Payload expressions evaluate in source
payload order and assemble by finalized payload id. Later stages must not
depend on source payload order beyond the preserved evaluation order and the
explicit finalized payload ids.

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
    expr: CheckedExprId,
    method: MethodNameId,
    dispatcher_ty: CheckedTypeId,
    callable_ty: CheckedTypeId,
    args: Span(CheckedExprId),
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

`dispatcher_ty` is the artifact-owned checked type root whose instantiated and
fully resolved monomorphic type determines method lookup. It may come from any
part of the checked constraint: the first argument, a later argument, the return
value, or a type root that appears only in the `where` constraint. Mono MIR must
consume this explicit checked type root. It must not rediscover it from argument
order, result position, receiver syntax, a qualified-name prefix, a method name,
a module environment lookup, or display-name sorting.

"Appears only in the `where` constraint" is a source-syntax statement, not
permission for an unconstrained post-check owner. A valid checked dispatch plan
must be determinate for every concrete mono specialization that can reach it.
After the enclosing checked procedure template is clone-instantiated at its
requested mono function type, and after the dispatch plan's `callable_ty`, all
normalized arguments, and the dispatch expression result slot are connected in
that same mono type store, `dispatcher_ty` must fully resolve to exactly one
allowed method owner.

If two different method owners can satisfy the same
`MonoSpecializationKey { template, requested_mono_fn_ty }`, then either the
specialization key is missing required semantic input or checking accepted an
ambiguous dispatch. Both are forbidden. The design chooses the simpler invariant:
checking may export a `StaticDispatchCallPlan` only when the selected
`dispatcher_ty` is functionally determined by the checked callable type, the
enclosing expression type, and the concrete mono specialization request. If that
cannot be proven during checking finalization, checking reports the dispatch as
ambiguous before publishing the artifact.

For example, a dispatcher selected from a return type is valid when the call
result is constrained by the enclosing expression or by the requested procedure
return type. A dispatcher selected from a checked type root that is mentioned
only in a `where` clause and not connected to any argument, return, result,
annotation, or enclosing requested function type is not a post-check problem;
checking must reject it before artifact publication.

`callable_ty` is the checked fixed-arity function type for the operation. Roc
functions have fixed arity and are not automatically curried. Therefore the
arity of `callable_ty` and the number of normalized `args` must match exactly.

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
method = canonical_method_name("is_eq")
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
so mono decides per specialization after `dispatcher_ty` has been instantiated
and fully resolved.

### Method Lookup In Mono

Mono MIR lowering uses one algorithm for every `StaticDispatchCallPlan` while
lowering one concrete mono specialization:

1. Instantiate `dispatcher_ty` and `callable_ty` into the same mono type store
   with the same clone-instantiation mapping as the expression.
2. Connect the instantiated callable return slot to this expression's
   instantiated mono result type. This must happen before method lookup because
   `dispatcher_ty` may be selected from the return position.
3. Lower all `args` in the normalized order, using the instantiated callable
   argument slots as expected types.
4. Fully resolve the instantiated dispatcher type.
5. Resolve `MethodOwner` from the fully resolved dispatcher type.
6. Look up `(MethodOwner, method)` in the checked method registry.
7. If a target exists, instantiate the target procedure type into the same mono
   type store.
8. Unify the instantiated target procedure type with the instantiated callable
   type. The unified function type is the exact requested mono source function
   type for this call.
9. Register that unified function type in the current
   `ConcreteSourceTypeStore`. The returned `ConcreteSourceTypeRef` is the
   request payload; its canonical key is the `requested_mono_fn_ty` portion of
   the target `MonoSpecializationKey`.
10. Request or reserve the target mono specialization with that exact payload.
11. Emit `call_proc` with:
   - `proc` equal to the mono-specialized `ProcedureValueRef`
   - `args` equal to the lowered normalized args
   - `requested_fn_ty` equal to the stage-local `TypeId` for the same unified
     requested mono source function type whose `ConcreteSourceTypeRef` payload
     was used to request the target specialization
12. If no target exists and `result_mode.equality.structural_allowed` is true,
    emit `structural_eq` using the lowered normalized args and the instantiated
    equality argument types.
13. If `result_mode.equality.negated` is true, emit `bool_not` after the custom
    call or structural equality operation.

If lookup is missing for `result_mode.value`, or if lookup is missing for
`result_mode.equality` while `structural_allowed` is false, that is a compiler
invariant violation. Checking must have reported invalid dispatch before mono
MIR begins. Mono debug verification must assert rather than inventing a target;
the equivalent release-build path is `unreachable`.

If `dispatcher_ty` does not fully resolve to one allowed `MethodOwner` after the
callable arguments and return slot have been connected, that is also a compiler
invariant violation. A dispatch site whose controlling type cannot be
determined from the checked callable type and enclosing expression type is
ambiguous; checking must have reported it before mono MIR begins. The
post-check path is debug-only assertion in debug builds and `unreachable` in
release builds.

This invariant is checked only at concrete specialization time. A checked
procedure template may still contain `StaticDispatchCallPlan` records whose
`dispatcher_ty` is generic. That template is not exported mono MIR. Exported
mono MIR must contain only the resolved `call_proc`, `structural_eq`, and
`bool_not` results produced after clone-instantiating the template for one exact
`MonoSpecializationKey`.

No later stage sees the method name as an unresolved call. No stage treats this
as an executable direct call until executable MIR.

For chained dispatch:

```roc
x.foo().bar()
```

the `bar` expression has its own `StaticDispatchCallPlan`. Mono lowers
`x.foo()` first, unifies the target procedure type with `foo`'s callable type,
and uses that unified return slot as the receiver expression type for the
surrounding expression. The `bar` plan still supplies its own `dispatcher_ty`.
Mono must not use the pre-target constraint approximation from `foo` as the
dispatcher for `bar`.

### Method Registry

The checked method registry maps:

```zig
MethodKey {
    owner: MethodOwner,
    method: MethodNameId,
}
```

to procedure targets with checked callable types.

If the checked method registry contains a hosted, platform, or intrinsic method
entry, checking must normalize it to an explicit builtin procedure target with
a checked callable type before mono consumes the registry. Mono still emits
`call_proc` to a `ProcedureValueRef` when a target exists. It must not
special-case a method name as an intrinsic after static-dispatch lookup.

A dotted expression without arguments is checked field access. It is not an
unresolved static method value and must not be treated as one later.

The registry is only a target table. It does not choose which type controls a
particular call. `StaticDispatchCallPlan.dispatcher_ty` chooses that.

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
- consumes checked `ResolvedValueRef` records for every value-like reference
- resolves artifact-local `CheckedStringLiteralId` values to bytes while
  lowering the owning checked artifact and interns those bytes into the
  program-owned `ProgramLiteralPool`
- stores `ProgramLiteralId`, never `base.StringLiteral.Idx` or bare
  `CheckedStringLiteralId`, for string literals, string interpolation segments,
  bytes literal payloads, string pattern tests, and user-written crash messages
- lowers top-level/imported constants only as `const_ref`
- lowers top-level/imported/hosted/platform-required/promoted procedure calls only as
  `call_proc`
- lowers top-level/imported/hosted/platform-required/promoted procedure values only as
  empty-capture `proc_value`
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
- preserves the program literal pool and rewrites no literal id unless it is
  doing an explicit whole-program literal-pool compaction/remap
- preserves the mono type for every expression, field edge, and payload edge
- deletes name-only row lookup before lifting
- exports no API for later stages to compute logical row indexes from names

Lifted MIR:

- lifts local functions and closures
- preserves the program literal pool without re-reading checked artifacts or
  source module string stores
- computes captures only over local runtime references and local procedure refs
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
- forbids top-level/imported/hosted/platform-required/promoted values as capture sources
- does not subtract a global/top-level symbol set as part of capture discovery

Lambda-solved MIR:

- determines exact callable sets
- preserves the program literal pool and stores `ProgramLiteralId` on every
  literal-bearing node
- associates capture types with callable members
- propagates erasure requirements
- reserves procedure instances, public value roots, value stores, and solve
  sessions before recursive specialization body solving
- solves one shared `RepresentationSolveSession` per recursive specialization
  SCC
- seals every representation instance and value metadata store before executable
  MIR consumes it
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
- preserves the program literal pool and emits executable string literal,
  string-pattern, bytes-literal, and crash-message references only as
  `ProgramLiteralId`
- emits callable-set values for non-erased callable values
- synthesizes erased adapters when a finite callable-set value crosses an
  erased `Box(T)` boundary
- consumes `BoxPayloadRepresentationPlan` instead of deciding erased callable shape
  compatibility from executable types
- emits finite callable-set `callable_match` expressions for non-erased callable
  calls
- reserves executable specializations before emitting `call_direct` branches
- supplies `callable_match` branch `direct_args` as `ExecutableValueRef` handles
  for source argument temps plus optional trailing capture record temp
- preserves fixed arity in every direct, erased, and callable-set call
- emits `packed_erased_fn` only for explicitly erased callable values
- emits `call_erased`
- emits `call_direct` where executable targets are exact
- inserts explicit bridges
- emits explicit runtime-uniqueness mutation sites
- exposes enough explicit values, call ABI shapes, and low-level RC-effect
  metadata for LIR ARC insertion

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
semantic parameter-mode inference after checking
backend or interpreter reference-counting inference
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
raw base.StringLiteral.Idx in MIR or IR AST nodes
bare CheckedStringLiteralId outside checked artifact bodies and artifact-local
  mono lowering context
raw ModuleEnv/CommonEnv string-store lookup while lowering imported checked
  templates
```

Allowed remaining locations for `base.StringLiteral.Idx` are source/parser
storage, checked-artifact construction internals that are copying from source
stores, and final LIR storage after IR-to-LIR lowering has interned program
literal bytes into `LirStore`. Allowed remaining locations for
`CheckedStringLiteralId` are checked artifact records, checked-artifact
verification, and the mono lowering code path that resolves the current owning
artifact's checked string bytes into `ProgramLiteralPool`. All MIR type-states
and IR must expose only `ProgramLiteralId` for lowered source literal payloads.

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
src/mir/executable/rc_effects.zig
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

The registry must map `MethodKey { owner: MethodOwner, method }` to procedure
targets. `MethodOwner` must be explicit semantic type identity, not expression
shape and not display-name lookup. `method` must be a canonical `MethodNameId`,
not a raw `Ident.Idx`. Hosted, platform, or intrinsic method entries must be
normalized to explicit builtin procedure targets with checked callable types
before mono consumes the registry.

Use the registry only as the target table for mono lookup. The registry must not
choose the dispatcher type for a call. The checked dispatch plan chooses that
with `dispatcher_ty`.

Each checked dispatch expression that remains after type checking must store:

```text
StaticDispatchCallPlan.expr
StaticDispatchCallPlan.method
StaticDispatchCallPlan.dispatcher_ty
StaticDispatchCallPlan.callable_ty
StaticDispatchCallPlan.args
StaticDispatchCallPlan.result_mode
```

`dispatcher_ty` must be selected by checked name resolution and type checking
from the operation's semantic constraint. It must not be inferred later from
syntax. The same representation is used whether the dispatcher type appears in
the first argument, a later argument, the return value, or only in the `where`
constraint.

A concrete equality expression that checking proves is always structural may be
rewritten to `structural_eq`. A generic equality expression must keep
`StaticDispatchCallPlan.result_mode.equality.structural_allowed = true` so mono
can decide per specialization after instantiation and full type-link resolution of
`dispatcher_ty`.

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
checked type store, checked body store, method registry, static dispatch plans,
checked procedure templates, root requests, hosted procedure table,
platform-required binding table, interface capabilities, compile-time roots if
retained, and `CompileTimeValueStore`.

This step must also introduce explicit table shapes for:

```text
RootRequestTable
CheckedTypeStore
CheckedBodyStore
CheckedProcedureTemplateTable
HostedProcTable
PlatformRequiredBindingTable
ModuleInterfaceCapabilities
```

Do not migrate callers by adding compatibility adapters. Instead, create the new
records and make checking finalization populate them before any public
post-check pipeline can consume the module.

Root requests must be produced for app entrypoints, concrete provided exports
that require runtime entrypoints, platform-required bindings, hosted exports,
tests, REPL/dev expressions, and compile-time constants. Generic exports that do
not require one concrete runtime entrypoint stay in the export table as checked
procedure templates. Root requests must record kind, source, checked type, ABI,
exposure, and deterministic order. They must not be inferred later from export
scans, declaration names, expression shapes, or procedure order.

Hosted procedures must be collected into `HostedProcTable`. Deterministic hosted
ordering belongs in that table. Do not write hosted indices into checked CIR as
authoritative post-check data.

Platform-required bindings must be collected into
`PlatformRequiredBindingTable`. Do not populate lookup targets by mutating
checked module environments after publication.

Interface capabilities must be published in `ModuleInterfaceCapabilities`.
Importers must consume these records through `ImportedModuleView`; they must not
inspect imported private definitions or recreate representation capability data.

Checked type and body stores must be published before checked procedure
templates are visible to mono MIR or importers. `CheckedProcedureTemplate` rows,
`StaticDispatchCallPlan` rows, `ResolvedValueRefRecord` rows,
`CallableEvalTemplate` rows, and `ConstEvalTemplate` rows must point at checked
type/body store ids instead of raw `types.Var`, raw `CIR.Expr.Idx`, raw
`CIR.Pattern.Idx`, or exporter-local `ModuleEnv` data.

Commit when checked artifacts can be constructed with these records and debug
verification proves:

- a published artifact has every required component
- every exported source procedure has either a checked procedure template or an
  explicit non-procedure top-level value entry
- every exported promoted procedure has a `PromotedProcedureTable` row whose
  template points at either a sealed finite promoted callable wrapper body owned
  by mono MIR or a sealed erased promoted callable wrapper plan owned by
  executable MIR
- every checked procedure template has checked type identity, checked type
  payload, checked body identity, checked body payload, static-dispatch plan
  coverage, and top-level-use summaries
- artifact views are read-only
- no post-publication code path patches module identity
- no post-publication code path writes hosted indices into checked CIR
- no post-publication code path populates platform-required lookup targets by
  mutating checked modules
- no imported generic specialization path reads the exporter `ModuleEnv`, raw
  checked expression ids, or checker type store to recover checked type/body
  payload
- root requests exist before MIR lowering starts
- missing root requests are reported before artifact publication or asserted as
  compiler bugs after publication

### 4. Replace Eager Mono Lowering With Specialization Queue

Before removing dispatch nodes from exported mono MIR, replace the current eager
top-level lowering model with the final specialization-driven model.

Add the checked procedure template table and mono specialization queue:

```zig
const CheckedProcedureTemplateTable = struct {
    templates: Span(CheckedProcedureTemplate),
};

const MonoSpecializationQueue = struct {
    requested: Map(MonoSpecializationKey, ReservedMonoProc),
    pending: WorkQueue(MonoSpecializationKey),
};

const ReservedMonoProc = struct {
    proc: MonoSpecializedProcRef,
    local_handle: MonoProcHandle,
    state: enum { reserved, lowering, lowered },
};
```

The exact Zig names may differ, but the lifecycle must not:

1. Checking finalization registers checked procedure templates for source,
   hosted, intrinsic, entry-wrapper, and promoted callable procedures. Finite
   promoted callable templates are appended only after compile-time callable
   promotion has sealed their mono-owned wrapper bodies. Erased promoted
   callable templates are appended only after compile-time callable promotion
   has sealed their executable-owned erased wrapper plans. The two cases share
   stable procedure identity and source function type metadata, but they do not
   share a body-lowering stage.
2. Root binding, direct calls, static-dispatch targets, and first-class
   `proc_value` uses create concrete `MonoSpecializationRequest` values whose
   `template` field is a checked `ProcedureTemplateRef` and whose
   `requested_fn_ty` field is a `ConcreteSourceTypeRef`. The corresponding
   `MonoSpecializationKey.requested_mono_fn_ty` is derived from that payload's
   canonical key; it is not accepted as a substitute for the payload. First-class
   callable values may also carry `CallableProcedureTemplateRef.lifted` or
   `CallableProcedureTemplateRef.synthetic`, but those are not ordinary
   checked-template mono requests; they follow the lifted or synthetic procedure
   identity path described above. A request whose template is an erased promoted
   callable wrapper reserves the opaque procedure identity for call sites, but
   mono must not lower its body; executable MIR emits that body from the sealed
   `ErasedPromotedWrapperBodyPlan`.
3. The queue reserves the output `MonoSpecializedProcRef`, including its output
   `ProcedureValueRef` and implementation-local `MonoProcHandle`, for each
   `MonoSpecializationKey` before lowering the body. The queue entry retains the
   canonical `ConcreteSourceTypeRef` payload for that key.
4. The body is clone-instantiated from artifact-owned checked type/body stores
   into a specialization-local checked source type graph and then mono type
   store. The requested source function type payload is cloned into that same
   source graph and unified with the cloned template root before mono type
   lowering.
5. Every checked string literal, bytes literal, string-pattern literal, and
   user-written crash message reached while lowering that checked body is
   resolved from the owning artifact's `CheckedBodyStore.string_literals` and
   interned into the specialization program's `ProgramLiteralPool`. The lowered
   MIR body stores only `ProgramLiteralId`.
6. Static dispatch is resolved inside that specialization-local lowering.
7. Any new `call_proc` or top-level `proc_value` dependencies enqueue additional
   concrete mono specializations.
8. The queue drains before row-finalized mono MIR consumes the output.

Delete `lowerAllTopLevelFunctions` and every equivalent eager lowering path
before static dispatch nodes are removed. There must be no code path whose
contract is "visit every top-level function and lower it." There must also be no
code path whose contract is "visit every exported function and lower it." A
public generic function export remains a checked procedure template until a
concrete consumer requests a concrete mono function type.

This deletion is not cosmetic. Eager lowering can force static dispatch lookup
inside a generic template before `dispatcher_ty` has a concrete method owner.
That produces either an invariant violation or a temptation to reintroduce
owner reconstruction. The queue-based model is the correctness mechanism.

Implement `call_proc` and `proc_value` before resolving dispatch. A resolved
direct source procedure call, resolved static dispatch call, resolved custom
equality call, or top-level procedure value must store the target mono-specialized
`ProcedureValueRef` in `call_proc.proc` or `proc_value.proc`. It must not lower
to a generic `call` whose callee expression is `var_(target_symbol)`. Cor can use
`Var(proc)` because Cor's symbol is already specialized and its prototype call
syntax is unary; Roc MIR must use explicit fixed-arity `call_proc` whose semantic
target is not a raw `Symbol`.

Commit when mono verification and deletion audits prove:

- checked procedure templates are registered without lowering bodies
- no eager top-level function lowering path exists
- no eager exported-function lowering path exists
- every concrete root request creates at most one initial
  `MonoSpecializationRequest`
- generic exports remain checked procedure templates until a concrete importer or
  root requests a concrete mono type
- every reserved mono specialization has exactly one output `ProcedureValueRef`
  and exactly one implementation-local procedure handle
- every `MonoSpecializationKey.template` names a checked procedure template,
  never a mono-specialized output procedure
- recursive or mutually recursive specializations reuse reserved procedure values
  and handles
- every exported mono procedure was produced by the specialization queue
- every literal-bearing mono MIR node uses `ProgramLiteralId`; no mono MIR node
  uses raw `base.StringLiteral.Idx` or bare `CheckedStringLiteralId`
- imported generic specialization tests prove that string literals are read from
  the imported checked artifact's literal table, not from any `ModuleEnv`
- static dispatch target lookup runs only while lowering one concrete
  `MonoSpecializationKey`
- no exported mono MIR `call` node uses a bare `var_` target to stand in for a
  resolved direct procedure call

### 5. Harden Mono MIR AST

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

Mono MIR lowering from one concrete checked procedure template specialization
must resolve:

- ordinary dispatch
- type dispatch
- nominal/custom equality

to `call_proc` calls or `structural_eq` immediately by consuming checked
`StaticDispatchCallPlan` values plus the checked method registry. It must not
choose the dispatcher variable from expression shape, receiver position, result
position, method name, or module environment lookup.

For every static-dispatch-produced `call_proc`, mono MIR must instantiate the
plan's `dispatcher_ty` and `callable_ty` into the current mono type store,
connect the callable return slot to the expression's instantiated mono result
type, lower normalized args through the callable arg slots, fully resolve the
dispatcher type, resolve `MethodOwner` from that type, look up `(MethodOwner,
method)` in the checked method registry, instantiate the target procedure
type into the current mono type store, unify it with the instantiated callable
type, use the unified argument and return slots as the call's requested mono
source function type, register that exact source function type in the current
`ConcreteSourceTypeStore`, and reserve the target mono specialization with the
resulting `ConcreteSourceTypeRef` before exporting mono MIR.

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
  target-procedure type and `StaticDispatchCallPlan.callable_ty` type in the
  mono type store
- every static-dispatch-produced mono specialization request carries a
  `ConcreteSourceTypeRef` whose canonical key equals the
  `call_proc.requested_fn_ty` key
- `call_proc` targets only top-level mono-specialized procedures
- mono `proc_value` captures are empty
- mono `proc_value` targets for top-level procedure values are mono-specialized
  at the exact requested mono source function type
- the mono specialization queue has no pending `call_proc` or `proc_value`
  dependencies
- no direct source procedure call, static-dispatch call, or custom equality call
  is represented as `call(var_(proc), args)` instead of `call_proc`
- every `call_proc` and `call_value` has exactly the arity of its
  `requested_fn_ty`
- no mono MIR node represents automatic currying or partial application

### 6. Add Row-Finalized Mono MIR Pass

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

- record construction carries `RecordShapeId`, source evaluation order, and
  finalized `RecordFieldId` assembly entries
- record access carries `RecordFieldId`
- record update carries `RecordShapeId`, source evaluation order, and finalized
  `RecordFieldId` assembly entries
- record destructuring patterns carry `RecordFieldId`
- tag construction carries `TagUnionShapeId`, `TagId`, source payload
  evaluation order, and finalized `TagPayloadId` assembly entries
- tag patterns carry `TagUnionShapeId`, `TagId`, and `TagPayloadId` entries for
  payload patterns
- tag payload projections carry `TagPayloadId`

For every row operation, the pass must resolve the operation's mono type in the
specialization-local mono type store, derive the full logical row from that
type, intern that full row shape, validate the requested field or tag against
the full row, validate constructor payload arity, and then rewrite the MIR node.

Record, record-update, and tag construction must preserve source evaluation
order separately from finalized logical assembly order. A later stage that needs
positional construction may assemble already-evaluated temporaries by finalized
logical IDs only. It must not evaluate fields in finalized logical order, name
sorting order, or physical layout order.

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
- record, record-update, and tag construction preserve source evaluation order
  separately from finalized logical assembly order
- every finalized row ID was derived from the full resolved mono type for that
  operation, never from singleton syntax such as `Err("x")`
- no later-stage helper exists for computing logical row indexes by sorting
  names, scanning rows, scanning expressions, or inspecting physical layout
  order

### 7. Harden Lifted MIR

Update lifted MIR to consume dispatch-free, row-finalized mono MIR.

Delete all dispatch cases from lifted MIR AST and lowering.

Add explicit `CaptureSlot` metadata to lifted procedure definitions.

For recursive local-function groups, allocate procedure values and
implementation-local procedure handles first, then compute captures to a least
fixed point across all members before exporting lifted MIR.

Implement capture discovery with a lexical scope builder keyed by resolved
symbols and mutable-version records. The builder must account for lambda
parameters, local function names, recursive local-function group members,
source `match` binders, record and tuple destructuring binders, `for` binders,
block-local declarations, and shadowing. It must not decide capture status by
comparing display names.

Captured mutable values must be captured as explicit mutable versions, branch
join versions, or loop phi values. They must not be represented as physical
mutable cells.

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
- capture discovery uses resolved symbols and mutable-version records, not
  display-name comparisons
- pattern, destructuring, `for`, block-local, and shadowed binders participate
  in the lexical scope stack
- captured mutable source values are explicit version or phi records, not
  physical mutable cells
- all `proc_value` captures are explicit `CaptureArg`s in slot order
- no captured source symbol remains as ordinary `var_` inside a lifted body
- no bare procedure-symbol `var_` values exist
- no aliases of local functions or closures remain as bare `var_`
- no local function or closure call is represented as `call_proc`
- no dispatch terms exist in exported lifted MIR

### 8. Harden Lambda-Solved MIR

Update lambda-solved MIR to consume dispatch-free lifted MIR.

Delete all dispatch cases from lambda-solved AST, inference, erasure
propagation, and verification.

Preserve and clean up the real responsibilities:

- instantiate lifted types
- import every lifted function type occurrence as a lambda-solved fixed-arity
  function type with a fresh callable variable
- infer callable sets
- propagate erasure
- compute `erased_box_payload_type(boundary)` plans for explicit `Box(T)`
  boundaries
- build the specialization-local `RepresentationStore`
- build the specialization-local `ValueInfoStore` in the same traversal as the
  `RepresentationStore`
- reserve `ProcRepresentationInstanceId`, public parameter roots, public return
  roots, public capture roots, whole-function roots, and dense value stores
  before lowering recursive specialization bodies
- group recursive specialization dependencies into SCC-scoped
  `RepresentationSolveSession` records and solve one shared representation
  store per SCC
- seal every procedure representation instance, solve session, and value store
  before executable MIR consumes it
- attach `ValueInfoId` or `BindingInfoId` directly to every exported expression,
  binder, pattern binder, mutable version, capture slot, projection, call result,
  and procedure-value occurrence
- ensure lexical scope maps source symbols only to `BindingInfoId` values; it
  must not carry ad hoc semantic fields such as procedure target, boxed payload,
  record-field, tag-payload, or callable-target data
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
- solve callable aliases through `ValueInfoId`/`BindingInfoId` value flow rather
  than `exact_callable_aliases` or any replacement global alias map
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
- generalize procedure and recursive-SCC templates only over type, callable, and
  representation variables not reachable from the already-bound outer
  environment
- clone-instantiate generalized templates before executable lowering consumes
  them
- clone-instantiate callable variables and representation variables together
  with ordinary type variables
- store generalized procedure representation templates separately from
  executable representation instances
- reserve all procedure values, callable representation nodes, capture-shape
  nodes, erased-adapter keys, value stores, public value roots, and solve-session
  membership for a recursive specialization SCC before solving any member body
  to completion
- publish executable specialization keys only after specialization-local
  representation solving has completed for the SCC

Commit when lambda-solved MIR verification proves:

- no dispatch terms exist
- every imported function type occurrence has an explicit callable variable
- every exported expression, binder, pattern binder, mutable version, capture
  slot, projection, call result, and procedure-value occurrence has explicit
  value metadata
- every recursive specialization instance has reserved public parameter, return,
  capture, and whole-function roots before any member body refers to it
- every recursive specialization SCC has exactly one sealed
  `RepresentationSolveSession`
- every member of a recursive specialization SCC points to that SCC's solve
  session and no other solve session
- no exported value metadata record is still reserved, building, structurally
  filled without solved class, or otherwise unsealed
- every cross-procedure value-flow edge inside a recursive SCC targets a
  `ProcPublicValueRoots` entry, not a source symbol, expression id, body-local
  value id, or environment lookup result
- no value-flow edge points into another solve session's private builder state;
  references to imported, hosted, platform, or already sealed outer procedures
  go through explicit capability/public-root records
- every `BindingInfo` points to a `ValueInfoId` and a representation root
- lexical environments used during lambda-solved construction map source symbols
  only to `BindingInfoId` records
- no lambda-solved builder environment contains ad hoc procedure-target,
  boxed-payload, aggregate-member, or callable-target fields
- unrelated equal source function `TypeId`s do not share callable variables unless
  an explicit value-flow edge connects them
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
- every `proc_value` occurrence has `CallableValueInfo` that names the occurrence
  procedure, occurrence capture values, whole function root, callable child root,
  and emission plan
- every callable alias has value-flow edges from producer occurrence to binder
  and from binder to every use; no exported callable alias map exists
- every callable value inside a record, tuple, tag payload, list, capture,
  compile-time value, branch join, mutable version, parameter, or return is
  reachable through explicit `ValueInfoId` metadata
- every `call_value` has `CallSiteInfo` that names callee value, argument values,
  result value, requested whole-function root, and finite/erased dispatch plan
- `Box.box` and `Box.unbox` called through first-class procedure values create
  `BoxBoundaryId` records from checked procedure metadata and `CallSiteInfo`,
  never from callee syntax
- every aggregate access and pattern projection has `ProjectionInfo` with a
  finalized row slot where rows are involved
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
- no executable specialization input contains generalized or unresolved callable
  variables or representation variables
- generalized procedure and SCC templates generalize only variables not reachable
  from the already-bound outer environment
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

### 9. Replace Executable Side-Table Planner

Rewrite executable MIR lowering so it consumes lambda-solved MIR and emits
executable MIR without source-expression side tables.

Lower every `call_proc` through an explicit `CallProcExecutablePlan` that names
the source/MIR procedure value occurrence, solved whole-function representation
root, executable specialization key, reserved `ExecutableProcId`, argument
value transforms, result value transform, and executable result type. Do not treat direct
procedure target identity as permission to skip boxed payload representation or
argument/result value-transform planning.

Define packed erased function values with explicit `ErasedFnValue` fields:
code pointer, `ErasedFnSigKey`, and typed capture metadata. The capture metadata
must distinguish no capture, typed zero-sized capture, and boxed runtime capture
payload. Boxed runtime capture payloads must carry value handle, capture type
key, layout, and size explicitly. `ErasedFnSigKey` must distinguish
no capture from a zero-sized typed capture, and must define whether a hidden
capture argument exists. When present, the hidden capture argument is appended
after all fixed-arity Roc source arguments.

Make `callable_match` a whole-call result-join node. It must own one
`result_ty` and one `result_tmp`. Every returning branch must produce a
branch-local direct-call result and then either assign it directly to the shared
result temp or bridge it through an explicit branch-local bridge. Branch-local
layout choices must not escape the node.

Make ordinary source `match` a concrete executable MIR result-join node distinct
from callable-set `callable_match`. It must evaluate every scrutinee once,
consume an explicit `PatternDecisionPlan`, materialize `PatternPathValuePlan`
records once per selected path at the control-flow point where they are needed
before projecting individual binders by finalized path ids, handle records,
tuples, lists, literals, opaque unwraps, newtypes, and guards through explicit
decision-path records, and join returning branches into one `result_tmp` through
explicit bridges when needed.

Enforce single-evaluation boundary discipline: source operands lower once to
`ExecutableValueRef` handles in source order, and bridges, calls, aggregate
assembly, erased packing, mutation sites, and branch joins consume only those
value handles. Executable MIR may remain expression-based where no semantic
boundary is crossed. LIR must be in administrative normal form before reference
counting and backend consumption.

Executable MIR must own explicit existing-value payload-transform nodes for
aggregate conversions that cannot be represented as ordinary structural bridges:

```zig
payload_transform_tag_union
payload_transform_list
payload_transform_box
```

The exact Zig names may differ, but the semantics must not. These nodes consume
an already-bound `ExecutableValueRef`, a lowered executable source type, a
lowered executable target type, and a checked-artifact or run-local
`ExecutablePayloadTransformPlanId`. They are executable-MIR operations, not
source syntax and not IR side tables. IR lowering consumes executable MIR only;
it must not reopen checked artifacts to discover transform semantics. Record,
tuple, nominal, identity, structural-bridge, callable-to-erased, and
already-erased callable transforms may still expand directly to ordinary
executable MIR nodes when doing so preserves single evaluation.

`payload_transform_tag_union` lowers to an executable discriminant switch over
the already-bound source union value. Each branch extracts the selected source
payloads, applies child transforms, constructs the target tag, and assigns the
shared result value. This is not source `match`, does not consume
`PatternDecisionPlan`, and does not run source pattern exhaustiveness logic.

`payload_transform_list` lowers to a compiler-owned list loop. The checked
artifact `ExecutablePayloadTransformOp.list` stores only the element child
transform, but executable MIR must expand that child transform before IR
lowering. The executable node must carry exactly:

```zig
payload_transform_list: struct {
    source: ExecutableValueRef,
    source_elem: ExecutableValueRef,
    source_elem_ty: TypeId,
    target_elem_ty: TypeId,
    body: ExecutableExprId,
}
```

`source` is the already-bound source list value. `source_elem` is the executable
value handle bound to each source element inside the compiler-owned loop.
`source_elem_ty` is the executable type of elements read from `source`.
`target_elem_ty` is the executable type produced by the child transform. `body`
is the already-lowered child transform expression that consumes `source_elem`
and returns one transformed target element. The result list type is the type of
the `payload_transform_list` expression itself, so the node does not duplicate
that type.

IR lowering for `payload_transform_list` must be mechanical and must not reopen
checked artifacts:

1. bind the source list once before the node is lowered
2. compute its length with the compiler low-level `list_len`
3. allocate an empty target list with `list_with_capacity(length)`
4. emit an IR/LIR `for_list` over the source list
5. inside the loop, bind `source_elem` to the loop element, lower `body`, append
   the transformed element with `list_append_unsafe`, and update the target-list
   accumulator with `set`
6. return the accumulator as the transformed list

This loop is an internal compiler lowering of a representation transform. It is
not a Roc source `for`, does not introduce a Roc callable value, does not call
`List.map`, and does not participate in static dispatch. The only possible
child behavior is the explicit executable child expression already produced
from the payload-transform plan.

`payload_transform_box` lowers to explicit unbox/box low-level operations plus
the child transform according to `BoxPayloadTransformKind`. It must allocate a
fresh target box for `payload_to_box` and `box_to_box` in the required baseline.
Any future reuse optimization must be represented by an explicit runtime
uniqueness mutation site and must still preserve ordinary ARC semantics.
Executable lowering must use the payload type from the already-lowered source or
target `Box(T)` endpoint for the actual unbox/box node. It must not compare
executable `TypeId` identity between separately lowered endpoint graphs; the
checked artifact verifier owns endpoint-alignment validation before executable
lowering begins.

Delete executable semantic parameter-mode solving entirely. Executable MIR must
not compute per-procedure parameter modes, escape relations, result
alias contracts, callable-call mode keys, or source-match mode joins. It must
instead emit explicit value nodes, call ABI shapes, low-level RC-effect records,
and runtime-uniqueness mutation sites for LIR ARC insertion.

Delete:

```text
legacy value side-table records
legacy callable side-table records
expression-indexed side-table maps
semantic side-table keys
semantic side-table-id usage
exact_callable_aliases
any replacement exact-callable alias map
ad hoc EnvEntry.proc or equivalent procedure-target fields
ad hoc EnvEntry.boxed, EnvEntry.record_fields, EnvEntry.tag_payloads, or
EnvEntry.callable_target fields
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
+ executable argument and return value type keys
+ representation mode
```

Executable argument and return value type keys are `CanonicalExecValueTypeKey`
values. They recursively collapse function-typed value slots to their solved
callable child, so nested higher-order arguments and returns participate in
specialization identity. The key for a procedure that accepts a record
containing `{ f : I64 -> I64 }` must distinguish whether `f` is represented as
`callable_set([AddN { n : I64 }])`, `callable_set([Identity])`, or
`erased_fn(sig)`.

not:

```text
side-table handles + expression ids + source/executable side channels + raw type ids
ProcOrderKey
```

Commit when executable MIR verification proves:

- direct calls have explicit targets
- every `call_proc` was lowered through an explicit executable call plan
- direct call args match target signatures
- executable value types collapse function-typed runtime values to
  `callable_set` or `erased_fn`, never to runtime function objects with
  argument and return fields
- executable specialization keys recursively encode nested function-valued
  argument, return, capture, record, tag, list, box, and nominal slots through
  `CanonicalExecValueTypeKey`
- direct, erased, and callable-set calls preserve fixed Roc arity
- erased calls have explicit erased function types
- packed erased function values carry code pointer, `ErasedFnSigKey`, and typed
  capture metadata
- packed erased capture metadata distinguishes no capture, typed zero-sized
  capture, and boxed runtime capture payload
- erased function signatures distinguish no capture from typed zero-sized
  captures
- hidden erased capture arguments appear only when `ErasedFnSigKey` records a
  non-null capture type, and then appear after all fixed-arity source arguments
- erased function signature keys contain an explicit `ErasedFnAbiKey`
- erased callable equality requires exact `ErasedFnSigKey` equality, including
  hidden capture type and `ErasedFnAbiKey`
- same erased args/return with different hidden capture type or different
  `ErasedFnAbiKey` values requires an explicit adapter or bridge and must not
  silently merge
- ordinary Roc boxed erased callables use the canonical erased ABI shape:
  ordinary packed function value, ordinary erased arguments, ordinary result
- erased adapters are synthesized for finite callable-set values crossing
  erased `Box(T)` boundaries
- `callable_match` evaluates its callable expression and original arguments
  exactly once before branching
- callable-set values have explicit member capture payloads
- callable-set member tag assignment is deterministic
- callable-set capture payload field ordering is deterministic
- finite callable-set calls lower to explicit `callable_match`
- every `callable_match` branch has a reserved executable specialization
- every `callable_match` branch records exact `direct_args` as
  `ExecutableValueRef` handles
- every `callable_match` has one result type and one result temp
- every returning `callable_match` branch bridges its branch-local result into
  the shared result temp when needed
- no branch-local result layout escapes a `callable_match`
- every callable member direct-call signature is source args plus optional
  trailing capture record
- no executable MIR node represents automatic currying or partial application
- no ordinary source `match` satisfies callable-set lowering verification
- every ordinary source `match` lowers to a concrete `SourceMatch` node distinct
  from callable-set `callable_match`
- every `SourceMatch` evaluates its scrutinees once, consumes an explicit
  `PatternDecisionPlan`, and extracts selected tag payload records once before
  projecting payload binders by finalized `TagPayloadId`
- source matches cover records, tuples, lists, literals, opaque unwraps,
  newtypes, tags, and guards through explicit decision-path records
- packed erased functions have explicit captures
- bridge nodes connect concrete executable MIR types
- bridge nodes consume only `ExecutableValueRef` handles and never own arbitrary
  source expressions
- operands with evaluation, allocation, reference-counting, or control-flow
  significance are lowered exactly once before any bridge consumes them
- every executable MIR node introduced for callable lowering, erased packaging,
  boxed payload boundaries, mutation, and bridges exposes explicit operands and
  RC-effect metadata needed by LIR ARC insertion
- no executable semantic parameter-mode solver exists
- LIR ARC insertion emits explicit `incref`, `decref`, and `free` from LIR
  values and control flow, not from procedure contracts

### 10. Delete Source-Type Reconstruction

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

### 11. Rewire IR Lowering

Update `src/ir/lower.zig` to consume executable MIR.

IR lowering must be source-blind and method-blind.

It consumes executable MIR data only and emits existing IR direct/erased call
forms.

Wire logical layout graph commitment before LIR consumes physical storage
operations. Layout commitment must reserve graph nodes before children, run SCC
detection, commit recursive by-value slot edges to physical indirection, and
publish one recursive-slot mapping consumed by constructors, field access,
source `match` payload extraction, capture access, callable-set payload access,
erased capture access, and RC plans.

Commit when IR lowering has no imports of checked CIR or MIR builder internals,
and layout verification proves every recursive by-value SCC edge has exactly
one committed physical indirection while no stage treats that indirection as
source `Box(T)` or erased callable representation.

### 12. Rewire Public Pipelines

Update eval, compile, CLI, interpreter shim, snapshot tool, glue, REPL, and
test helpers to call the checked-artifact public pipeline.

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

For `roc run`, `roc build --opt=interpreter`, and the interpreter shim, move
the semantic pipeline into the parent compiler process. The parent must call
the checked-artifact public pipeline, run ARC insertion, and publish a
target-specific viewable `LirRuntimeImage` through the existing shared-memory
handoff for IPC execution. Delete the old shared-memory/embedded
`ModuleEnv` payload shape, `ModuleEnvHeader`, platform/app `CIR.Def.Idx`
entrypoint tables, and child-side CIR-to-LIR lowering path. The child shim
maps shared memory, constructs zero-copy LIR runtime-image views, and invokes
the LIR interpreter on explicit root proc ids.

Remove helper names that refer to old stages.

Commit when `rg` finds no old post-check imports or pipeline labels, no
`lowerTypedCIRToLir*` or `lowerTypedCIRToSemanticEval*` public entrypoints, and
no public semantic lowering result that can return `NoRootProc`,
`NoRootDefinition`, `MissingRoot`, `MethodNotFound`, `UnsupportedSourceType`,
`UnsupportedLayout`, `SchemaLayoutMismatch`, or `MissingInterfaceCapability`.

### 13. Rewire Compile-Time Constant Evaluation

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
  -> CompileTimeValueStore + ConstInstantiationStore
     + CallableBindingInstantiationStore
     + SemanticInstantiationProcedureTable + private capture graph
     + promoted checked templates + promoted procedure table
```

This work must preserve the current valid architecture:

- compile-time constants are evaluated by the LIR interpreter
- runtime bytes and callable leaves are reified into explicit
  constant-graph/value nodes
- top-level constant bindings point at `ConstRef` templates whose concrete uses
  are selected by `ConstInstantiationKey` and addressed by concrete
  `ConstInstanceRef` values
- top-level constant value graphs may contain callable leaves, and each
  finite callable leaf is an explicit sealed procedure value plus exact
  canonical source function type; each erased callable leaf is an exact erased
  callable code ref plus capture materialization and non-empty `BoxBoundaryId`
  provenance
- top-level callable bindings publish as `procedure_binding` after compile-time
  callable promotion when promotion is needed, or as sealed callable eval
  templates when the binding is generalized and requires a future concrete
  function type
- serialized compile-time values travel inside cached checked artifacts
- artifact-owned `ConstInstantiationStore` rows are sealed before checked
  artifact publication; a post-check stage never creates, finishes, or mutates a
  concrete constant instance
- artifact-owned `CallableBindingInstantiationStore` rows are sealed before
  checked artifact publication; a post-check stage never creates, finishes, or
  mutates a concrete callable binding instance
- procedure templates generated while instantiating constants or callable
  bindings are sealed in the artifact-owned
  `SemanticInstantiationProcedureTable` before checked artifact publication
- imported modules expose compile-time constants through their serialized value
  store and exported const-template views, and expose function-valued bindings
  through exported procedure-binding views
- imported template closures carry artifact-qualified private refs, private
  capture nodes, callable result plans, callable promotion plans, constant
  reification plans, dependency summary templates, and semantic-instantiation
  procedure entries required by the exported template
- top-level constants, including constants with callable leaves, are consumed
  by `ConstRef` plus a concrete `ConstInstantiationKey`
- function declarations and function-valued declarations are consumed as
  procedure bindings; direct top-level functions use their original procedure
  values through `TopLevelProcedureBindingRef`, and compile-time callable roots
  use either promoted procedure values or callable eval templates through
  `TopLevelProcedureBindingRef`
- promoted procedure values are published only after their
  `PromotedProcedureTable` rows and checked procedure templates have been
  sealed in the same checked artifact

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
- no post-check creation or evaluation of `ConstInstantiationStore`,
  `CallableBindingInstantiationStore`, `SemanticInstantiationProcedureTable`,
  promoted procedures, private capture graphs, callable result plans, callable
  promotion plans, constant reification plans, or dependency summary templates

Introduce an explicit compile-time root table before LIR interpretation. The
table must record the source module, top-level pattern, expression, procedure
value/root handle, root kind, dependency summary request, and either the
`ConstGraphReificationPlan` for non-function compile-time constants or the
callable-result record for function-valued bindings. Root selection must happen
before the LIR interpreter runs. The interpreter must only execute listed roots;
it must not decide which top-level declarations are constants or callable
bindings.

Introduce `CompileTimeRootDependencyGraph` before root evaluation. The graph
must include compile-time constant roots, callable binding roots, and expect
roots. Its edges must be resolved-symbol dependencies between roots or imported
checked artifacts, not name strings or source expression scans. Direct
top-level functions are procedure values, not compile-time evaluation roots
merely because another root calls them.

Build the graph from summary-only callable-aware lowering records, not merely
from root expression bodies and not from mono MIR alone. Checking finalization
must lower each required summary through the MIR-family pipeline far enough to
produce sealed `CallSiteInfo`, callable emission plans, constant-graph
reification plans, and callable-result plans. Each summary records local
top-level values, imported top-level values, `call_proc` targets, finite
`call_value` member targets, erased `call_value` code refs and capture
dependencies, callable leaves in constant graphs, and callable leaves in
promoted capture graphs. Direct erased code refs consume the exact
`ProcValueErasePlan`. Finite-set erased code refs name the `ErasedAdapterKey`
and every member target reachable through the adapter's `callable_match`,
including singleton sets.
Checking finalization computes an SCC fixed point over those summaries so a
root depends on top-level values referenced by every procedure or callable
member it can call during compile-time evaluation.

Generic constant and callable templates store dependency summary templates, not
concrete dependency summaries. `ConstEvalTemplate` and `CallableEvalTemplate`
contain `ComptimeDependencySummaryTemplate` values whose entries are still
parameterized by requested source type and concrete mono specialization. A
concrete `ConstInstance` or `CallableBindingInstance` resolves that template in
the requesting artifact's concrete mono context and stores the resulting
`ComptimeDependencySummaryId` next to the evaluated value or callable result.
This keeps the generic template reusable while ensuring post-check lowering
never has to infer dependencies, rerun checking, or ask an imported artifact to
add a missing concrete summary.

The dependency graph is the checking-time availability graph. It is not a
dynamic demand trace of the particular LIR interpreter path for one execution.
If a sealed summary for a root or reachable procedure body contains a resolved
reference to a non-procedure top-level value, that value is a prerequisite even
when it appears under a branch that is not dynamically taken for one set of
inputs. Such conservative edges are allowed only when they are the same edges
checking uses for top-level constant cycle detection and they come from sealed
lowering records. They must not come from syntax scanning, name matching, or
environment lookup after lowering.

Add the checking-finalization-only unfilled-root summary mode at the same time.
While computing the dependency graph, a top-level lookup of an unfilled local
constant template or pending callable root records `AvailabilityUse.local_root`;
it does not lower to a concrete `ConstInstantiationRequest`, does not lower to
`procedure_binding`, and does not emit runnable MIR. After the dependency graph
is ordered, actual root lowering must happen only after every local-root
dependency for that root has filled its reserved `ConstRef` template or published
its promoted `procedure_binding`.

Delete the current semantic-eval shortcuts: syntax predicates such as
`topLevelExprNeedsEvaluation`, lowering every definition as a compile-time root,
ordering roots with checked-CIR `evaluation_order` instead of the explicit
dependency graph, and treating the last generated root procedure as a default
interpreter entrypoint. Those helpers may remain only in tests that verify the
deletion audit rejects them.

Evaluate roots topologically. Before a dependent root lowers through the
MIR-family pipeline, every local root it depends on must already have published
its reserved `ConstRef` template or promoted `procedure_binding` into the in-progress
`TopLevelValueTable`. Cycles among non-procedure roots are checking diagnostics
before artifact publication.

Initialize the in-progress `TopLevelValueTable` before any root evaluation. The
initial table contains direct top-level functions and already-procedure
top-level lambdas as `procedure_binding` entries, and every non-function
top-level constant as a reserved `const_template` entry. Function-valued roots
that need compile-time callable evaluation start as `pending_callable_root`
entries visible only inside checking finalization. Each evaluated non-function
compile-time constant fills its reserved `ConstRef` template, including constants
whose graphs contain callable leaves; each promoted callable binding replaces
its `pending_callable_root` with `procedure_binding`. The published artifact
must contain no `pending_callable_root` entry.

Add compile-time callable promotion to checking finalization. Function-valued
top-level bindings whose expressions are not already top-level functions must
run as `callable_binding` roots when their requested function type is concrete.
Their interpreter result must be reified as a compiler-owned `ComptimeCallable`,
promoted to a closed procedure value, and published as `procedure_binding` only
after its checked procedure template is sealed. The promoted procedure must have
no runtime capture environment. A generalized function-valued binding whose
callable result cannot be represented as an already-sealed direct callable
template must publish a sealed `CallableEvalTemplate`; concrete
`CallableBindingInstantiationRequest` values evaluate that same callable-binding
root path only during the checking finalization of the artifact that owns the
concrete instance, at the requested function type payload. The published artifact
must not contain a `ProcedureUseTemplate` that requires post-check lowering to
finish the callable evaluation.
This includes trivial references to existing procedures such as `also_inc = inc`.
Those bindings still run through compile-time callable evaluation. If the
evaluated `ComptimeCallable` is a finite existing procedure with no captures,
checking finalization may publish the existing procedure value directly for the
binding. That direct publication is an outcome of reifying the evaluated
callable value, not a separate alias concept, not a syntax shortcut, and not an
`exact_callable_aliases` replacement.
Serializable top-level captures become reads of their published `ConstRef`
templates plus concrete instantiation requests. Local captures are reified into
private structural promoted-capture data: serializable leaves become
artifact-private `ConstRef` templates or concrete private `ConstInstanceRef`
leaves, and function-typed callable leaves become explicit finite
`FiniteCallableLeafTemplate` records that instantiate to
`FiniteCallableLeafInstance` records at concrete use sites. A finite callable
leaf instance records both the sealed procedure template and the exact canonical
source function type for that occurrence; a bare procedure value is not enough.
Existing
source/imported/hosted/platform-required procedure bindings remain existing
procedure templates and are mono-specialized from explicit requests; local closure leaves and
evaluated callable leaves are recursively promoted to private procedure values
with sealed checked templates. Records, tuples, tags, lists, boxes, transparent
aliases, and nominals preserve the source container shape as private
compiler-owned capture nodes. Public non-function constants use the same
constant-template node kinds for nested callable leaves and must be published as
`ConstRef` templates, not reported as invalid or converted to runtime globals.
When a local non-function capture subtree contains erased boxed callable slots,
promotion must store that subtree as a concrete private `ConstInstanceRef` leaf
and leave erased callable materialization to executable MIR's const
materialization path; it must not put an erased boxed callable leaf in
`PrivateCaptureRef`.

`ComptimeCallable` has finite and erased cases. The finite case names the
procedure, lifted lambda, or callable-set member plus compiler-owned captures.
The erased case names the `ErasedFnSigKey`, exact erased callable code ref, and
compiler-owned erased capture value. Erased callable promotion is allowed only
when the erased callable carries explicit `BoxBoundaryId` provenance. Promotion
must produce a closed ordinary procedure identity whose body is owned by
executable MIR and performs the known erased call from the sealed
`ErasedPromotedWrapperBodyPlan`. Mono, row-finalized mono, lifted MIR, and
lambda-solved MIR carry that procedure identity opaquely at its source function
type; they must not lower the erased body or read the erased ABI fields. It must
not publish a runtime packed erased callable object, runtime top-level closure
object, runtime global callable-value object, or runtime thunk.

Promotion consumes `CallablePromotionPlan` records produced before evaluation.
The plan names the root, source function type, dependency summary, and
`CallableResultPlan`. During promotion, captured top-level constants consume
their already-published `ConstRef`; captured local values are reified by walking
the structural `CaptureSlotReificationPlan`; serializable leaves become
artifact-private capture constants in `CompileTimeValueStore`; callable leaves
become private promoted procedures through recursive reserve/fill/seal; and
structural containers become private capture graph nodes. Promotion must not
discover new top-level dependencies by inspecting runtime capture memory, source
syntax, or lowered procedure bodies after dependency ordering has finished.

Each concrete callable evaluation publishes a complete `CallableBindingInstance`
in the owning artifact. The row stores the
`CallableBindingInstantiationKey`, concrete `ComptimeDependencySummaryId`,
private `ComptimeOnlyExecutableRootId`, concrete `CallableResultPlan`, optional
`CallablePromotionPlan`, promotion output, and final `ProcedureCallableRef`.
The promotion output is either an existing sealed procedure value or a promoted
procedure value whose promoted-procedure row and checked template are already
sealed in the same artifact. Later MIR stages consume this row as immutable
input; they do not recompute callable results, allocate promoted procedures, or
walk source expressions to rediscover callable identity.

The promotion implementation order is:

1. Build stable `PromotedCallableNodeKey` records for the root callable result
   and every callable leaf reachable through captured values.
2. Reserve procedure values, `ProcBaseKeyRef` values, `PromotedCallableNodeId`
   values, and checked template slots for the whole recursive promotion group.
3. Build `PrivateCaptureRef` graphs for local captures. Whole private capture
   aggregates and projected leaves both remain explicit value handles. Subtrees
   that require boxed-erased callable materialization are represented as concrete
   private const-instance leaves. Neither form becomes a source-visible top-level
   value.
4. Fill every `PromotedCallableWrapper` body by referencing ordinary parameters
   and explicit private-capture operands.
5. Seal the promoted checked templates.
6. Insert `PromotedProcedureTable` rows that point at those sealed templates.
7. Insert any instantiation-generated procedure templates into
   `SemanticInstantiationProcedureTable`.
8. Publish root `procedure_binding` entries into `TopLevelValueTable`.

Any missing private capture node, missing promoted template, unresolved callable
leaf, missing semantic-instantiation procedure row, or promoted procedure value
without a `PromotedProcedureTable` row after step 8 is a compiler invariant
violation: debug-only assertion in debug builds,
`unreachable` in release builds.

Non-function top-level constants whose source type contains callable slots are
valid compile-time constants. This includes callable slots inside `Box(T)`,
records, tuples, tags, `List(T)`, transparent aliases, and nominals. Checking
finalization must reify them into explicit `ConstRef` template graphs whose
callable leaves are sealed callable leaf templates. Concrete uses instantiate
those templates through `ConstInstantiationRequest` before executable MIR or
materialization. Private promoted-capture graphs use the same callable-leaf
template/instance discipline, but they are never imported or exported by source
name and never appear in `TopLevelValueTable`.

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

Add `ConstRef` or the exact equivalent. Mono MIR lookup of a compile-time
top-level constant must emit a constant-reference node that carries this handle.
Executable MIR/LIR must turn the handle into an explicit target-specific
`ConstMaterializationPlan` containing target executable type, a recursive
materialization node graph, layout, reference-counting plan, callable
materialization decisions, and storage strategy. Backends must only emit
requested static data or heap setup and follow explicit LIR `incref` and
`decref` statements.

Replace the current checked-module cache shape with the checked artifact cache
described above. The checked artifact key must use `source_hash`,
`compiler_artifact_hash`, `module_identity`, `checking_context_identity`, and
direct imported checked artifact keys. It must not include target/layout inputs.

A checked artifact cache hit and its compile-time value store must be accepted
together or rejected together. There must not be an independently accepted
compile-time-value sidecar. If target-specific constant materialization is ever
cached, that cache is separate from the checked artifact cache and is keyed by
the checked artifact key, `ConstInstantiationKey`, and target/layout inputs.

Commit when compile-time evaluation uses the MIR-family pipeline, runtime
codegen cannot see `comptime_only` roots, cached/imported constants are loaded
only from the compile-time value store through `ConstRef` plus
`ConstInstantiationKey`, and target-specific constant materialization is outside
the checked artifact cache.

### 14. Strengthen Audits

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

Forbid raw `Symbol` as semantic procedure identity in exported MIR nodes,
checked artifacts, imported artifact views, compile-time value graphs,
callable-set keys, erased code refs, executable specialization keys, method
targets, root requests, and cache keys. A raw `Symbol` may appear only as an
implementation-local binder/temporary/procedure handle while the owning store is
alive, or as a generated backend/debug name after the semantic procedure identity
has already been selected.

Forbid raw `Ident.Idx` in exported MIR nodes, row-shape keys, method keys,
static-dispatch plans consumed by mono MIR, checked-artifact cache keys,
compile-time value graphs, hosted/platform tables, root metadata, executable
specialization keys, callable-set keys, capture-shape keys, erased adapter keys,
LIR semantic inputs, and backend semantic inputs. Any post-check value that needs
a source-facing or ABI-facing name must store the appropriate canonical name id
such as `RecordFieldLabelId`, `TagLabelId`, `MethodNameId`, `ExportNameId`, or
`ExternalSymbolNameId`.

Forbid foreign artifact-local canonical name ids in exported MIR nodes,
row-shape keys, method keys, static-dispatch plans consumed by mono MIR,
compile-time value graphs, hosted/platform tables, executable specialization
keys, callable-set keys, capture-shape keys, erased adapter keys, LIR semantic
inputs, and backend semantic inputs. Imported checked artifact data must be
remapped into the lowering-run canonical-name store before any of those records
are created.

Forbid raw type-store ids in executable specialization keys.

Forbid any erased-boundary record whose root is not `Box(T)`.

Forbid executable erased-shape compatibility helpers from making semantic
lowering decisions. Boxed erased-boundary decisions must come from lambda-solved
`BoxBoundary` records and `BoxPayloadRepresentationPlan` values. They
may be rechecked only by debug-only verifiers.

Forbid executable MIR from deciding erased callable packaging by checking
whether the current source expression is syntactically enclosed by `Box.box(...)`
or `Box.unbox(...)`. Erased callable packaging must consume
`CallableValueEmissionPlan` values with non-empty `BoxBoundaryId` provenance.

Forbid solved erased callable representation without non-empty `BoxBoundaryId`
provenance. Forbid any erased callable provenance case other than explicit
`BoxBoundaryId`.

Forbid non-primitive `LowLevelCall` nodes without a complete
`LowLevelValueFlowSignature`. Low-level ABI metadata and RC-effect metadata are
not allowed to stand in for representation value-flow metadata.

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

Forbid compile-time dependency discovery by scanning only root expressions.
Compile-time root dependencies must include the fixed-point summaries of
callable-aware summary records reachable through `call_proc`, finite
`call_value` member targets including singleton sets, erased `call_value`
code refs and capture dependencies, constant-graph callable leaves, and
promoted-capture callable leaves.

Forbid post-check root selection by scanning exports, top-level declarations,
checked expressions, procedure order, hosted lambda expressions, or generated
symbol names. Root selection must consume `RootRequestTable`.

Forbid eager mono lowering of top-level or exported functions. These names and
families must not exist outside deletion-audit tests:

```text
lowerAllTopLevelFunctions
lowerEveryTopLevelFunction
lowerAllExportedFunctions
emitAllTopLevelFunctions
allDefs as mono function lowering order
export scan as mono specialization source
```

Mono function bodies must be produced only by `MonoSpecializationQueue` entries
with concrete `MonoSpecializationKey` values.

Forbid direct source procedure calls represented as ordinary `call` nodes whose
callee expression is a bare `var_` procedure handle. A resolved direct
procedure call, resolved static-dispatch target, or resolved custom equality
target must use `call_proc` until executable MIR lowers it to `call_direct`.

Forbid the old semantic-eval helper family outside deletion-audit tests:

```text
topLevelExprNeedsEvaluation
topLevelExprNeedsBindingSchema
synthesizeSemanticEvalComptimeInitProc
lowerTypedCIRToSemanticEvalProgram
lowerTypedCIRToSemanticEvalProgramForTarget
SemanticEvalTopLevelRoot
SemanticEvalProgram
evaluation_order as compile-time evaluation order
allDefs as compile-time root selection
last generated root proc as interpreter entrypoint
```

Forbid post-check mutation of hosted indices or platform-required lookup
targets inside checked CIR or `ModuleEnv`. Hosted and platform data must come
from `HostedProcTable` and `PlatformRequiredBindingTable`.

Forbid public semantic lowering APIs that return semantic errors after artifact
publication. Post-check semantic lowering may return resource errors only.

Forbid imported representation recovery from module bodies, opaque backing
syntax, display names, or layout shapes. Cross-module representation data must
come from `ModuleInterfaceCapabilities`.

Forbid runtime constant materialization from hidden top-level thunks or imported
module LIR re-execution. Top-level constants, including constants whose graphs
contain callable leaves, must be consumed through `ConstRef` and
`CompileTimeValueStore`, with concrete uses addressed through
`ConstInstantiationStore`.

Forbid publishing `TopLevelValueTable` entries in a `pending_callable_root`
state. The table may contain `pending_callable_root` entries only inside checking
finalization; non-function constants must have reserved `ConstRef` entries
instead of pending table rows.

Forbid treating non-function top-level constants whose source type contains
callable slots as unsupported. Such bindings are valid Roc. They must serialize
as explicit `ConstRef` template graphs with callable leaf templates that point
at sealed checked procedure templates and instantiate to concrete callable leaf
instances at use sites. They must not become runtime globals, runtime initializer
procedures, runtime top-level closure objects, runtime packed callable globals,
or interpreter pointers.

Forbid invalid Roc syntax in plan examples and tests that are intended to be Roc
source. In particular, forbid Haskell-style run declarations, backslash-arrow
lambdas, and whitespace function application. Roc examples must use lambdas like
`|x| x` and calls like `f(x, y)`.

Forbid bare procedure-handle `var_` values outside mono MIR and lifted MIR input
pattern matching.

Forbid dispatch variants outside checked CIR and mono MIR input lowering:

```text
dispatch_call
type_dispatch_call
method_eq
```

Commit when guarded semantic audits pass.

### 15. Rewrite Tests

Rewrite old intermediate-stage tests.

Obsolete expectations:

- dispatch survives into monotype
- dispatch survives into lambdasolved
- accumulator side tables are visible before executable MIR
- executable side tables contain old source-level expression records
- old stage type stores contain old-stage shapes

Replacement expectations:

- checked artifacts contain checked procedure templates for source procedures
  without eagerly lowering those templates to mono MIR
- checked CIR contains dispatch only with normalized `StaticDispatchCallPlan`
  values where appropriate
- mono MIR procedures are produced by concrete `MonoSpecializationQueue`
  requests, never by scanning all top-level or exported procedures
- mono MIR contains `call_proc` and no dispatch
- mono MIR direct procedure calls use `call_proc`, not `call(var_(proc), args)`
- lifted MIR contains explicit `CaptureSlot`s, explicit `proc_value`
  `CaptureArg`s, and no dispatch
- lambda-solved MIR contains explicit callable sets and no dispatch
- executable MIR contains direct/erased calls, finite callable-set
  `callable_match`
  lowering, packed erased functions, bridges, and no source-expression side
  tables
- IR/LIR contain direct/erased calls only
- mono MIR through IR carry source literal payloads as `ProgramLiteralId`, not
  raw `base.StringLiteral.Idx`, raw `CheckedStringLiteralId`, per-node byte
  slices, or artifact string-table pointers

## Required Structural Tests

Add MIR-family verification tests for each stage.

Checking finalization and compile-time constants:

- checking finalization publishes a complete `CheckedModuleArtifact` or no
  artifact
- published artifacts include checked type store, checked body store, method
  registry, static dispatch plans, checked procedure template table, root request
  table, hosted procedure table,
  platform-required binding table, resolved value-reference table, interface
  capabilities, and compile-time value store
- published artifacts, imported artifact views, checked-artifact cache keys,
  compile-time value graphs, root requests, hosted/platform tables, method keys,
  and static-dispatch plans contain canonical names and procedure identities, not
  raw `Ident.Idx` or raw `Symbol`
- two modules whose `Ident.Store` values assign different local indexes to the
  same field, tag, method, export, or ABI spelling produce the same canonical
  name ids at the artifact boundary; a test-only artifact with an `Ident.Idx` in
  any exported key must fail debug verification immediately
- a test-only artifact that stores raw `Symbol` as a callable leaf, method target,
  root procedure identity, promoted procedure identity, erased code ref, or cache
  key must fail debug verification immediately; release builds use `unreachable`
  for the equivalent compiler-invariant path and do not retain verifier metadata
- test-only callable-set descriptors with a corrupted `CallableSetMemberId`,
  missing `ProcedureCallableRef`, wrong `ProcedureCallableRef.source_fn_ty`,
  reordered `CaptureSlot.index`, mismatched `CaptureShapeKey`, or capture slot
  whose `CanonicalExecValueTypeKey` disagrees with the member descriptor must fail
  debug verification immediately; release builds use `unreachable` for the
  equivalent compiler-invariant path and do not retain verifier metadata
- every promoted callable `procedure_binding` published in `TopLevelValueTable`
  has a `PromotedProcedureTable` row, a stable `ProcBaseKeyRef`, a
  `PromotedCallableNodeId`, a sealed `CheckedProcedureTemplate`, and a concrete
  `ProcedureValueRef`
- `CheckedProcedureTemplate.body` points at a sealed `CheckedBodyId` or an
  explicit compiler-created wrapper variant. It never points at a raw
  `CIR.Expr.Idx`, and mono MIR consumes the template body variant instead of
  searching for bodies through symbol names.
- artifact verification rejects any exported/imported `CanonicalTypeKey` or
  `CanonicalTypeSchemeKey` without a checked type payload, and any checked body
  node without a checked type id
- artifact verification rejects any checked body string literal id outside the
  owning checked artifact's `CheckedBodyStore.string_literals` table
- checked artifact tests prove that source `base.StringLiteral.Idx` values are
  copied to artifact-owned checked string bytes during publication and are not
  retained as exported checked body payload
- artifact views exposed to importers and lowering are read-only
- a cache hit does not patch module identity after deserialization
- hosted procedure ordering is stored in `HostedProcTable`, not by mutating
  checked CIR
- platform-required bindings are stored in `PlatformRequiredBindingTable`, not
  by mutating checked module lookup targets after publication
- root requests exist before MIR lowering starts
- root requests cover app entrypoints, concrete provided exports that require
  runtime entrypoints, platform-required bindings, hosted exports, tests,
  REPL/dev expressions, and compile-time roots
- generic exports are represented as checked procedure templates, not root
  requests, until a concrete consumer requests one mono specialization
- no eval, REPL, snapshot, CLI, glue, build, or test helper selects roots by
  scanning exports, declaration names, expression syntax, hosted lambda nodes,
  or procedure order
- imported modules expose representation capabilities through
  `ModuleInterfaceCapabilities` and exported checked procedure templates through
  `ImportedModuleView`
- importing modules do not inspect imported private definitions, opaque backing
  syntax, exporter `ModuleEnv`, raw checked expression ids, or exporter checker
  type stores to rebuild representation capability data, checked body payloads,
  or checked type payloads
- importing modules do not inspect exporter `ModuleEnv` or exporter
  `base.StringLiteral.Store` to lower imported string literals; they consume the
  imported checked artifact's checked string bytes and intern them into the
  lowered program literal pool
- checked artifact verification rejects any value-like reference that lacks a
  `ResolvedValueRefRecord`
- checked artifact verification distinguishes local shadowing from top-level or
  imported value references by resolved binding identity, not display name
- method registry tests use canonical `MethodNameId` values and canonical owner
  keys. They must prove that mono MIR does not perform method lookup with
  module-local `Ident.Idx`, display text from the wrong store, or generated
  procedure handles.
- compile-time constant evaluation runs before checked artifacts are published
- user-facing compile-time crashes, expect failures, numeric conversion
  failures, and evaluation errors are reported as checking problems
- every compile-time evaluation root appears in the explicit compile-time root
  table
- compile-time roots are evaluated through an explicit dependency graph
- direct top-level functions are procedure values, not compile-time evaluation
  roots
- dependent roots lower only after local root dependencies have published
  filled `ConstRef` templates or promoted `procedure_binding` entries into
  `TopLevelValueTable`
- non-procedure compile-time root cycles are reported before artifact
  publication
- no LIR interpreter code path selects compile-time roots by inspecting source
  expression syntax
- private aggregate LIR roots used by compile-time evaluation are marked
  `comptime_only`
- no `comptime_only` root reaches runtime root lists, backend input, generated
  program entry metadata, or runtime codegen
- compile-time value reification stores `ConstRef` template graphs and concrete
  `ConstInstanceRef` nodes, not raw runtime addresses
- compile-time reification plans are built from resolved source types, selected
  layouts, and sealed callable representation data
- root function source types publish as `procedure_binding`; callable leaves
  nested under non-function constant roots stay inside explicit `ConstRef`
  templates and instantiate through concrete `ConstInstantiationRequest` values
- function-valued top-level bindings that evaluate to closed callable values are
  promoted during checking finalization to closed procedure values
- cached checked artifacts include serialized compile-time values and hit or
  miss as one unit
- checked artifact keys use `source_hash`, `compiler_artifact_hash`,
  `module_identity`, `checking_context_identity`, and direct imported checked
  artifact keys
- checked artifact keys do not include target/layout inputs
- top-level constants are consumed through `ConstRef` plus
  `ConstInstantiationKey`, including constants whose template graphs contain
  callable leaves
- function declarations and function-valued declarations are consumed as
  procedure values after any required callable promotion, not serialized
  constants
- imported constants are not evaluated by re-running imported module LIR roots
  after the imported artifact is published
- target-specific constant materialization uses explicit layout,
  reference-counting, and storage plans outside the checked artifact cache

Mono MIR:

- every expression has a mono type
- no dispatch nodes exist
- static dispatch becomes `call_proc`
- static dispatch consumes checked `StaticDispatchCallPlan` values and the
  checked method registry, never syntax-derived method lookup in mono MIR
- static-dispatch plans consumed by mono MIR carry canonical `MethodNameId` and
  `dispatcher_ty`; mono tests must prove identical method spellings from
  different identifier stores resolve through the same canonical method name and
  never through raw `Ident.Idx`
- static-dispatch `call_proc.requested_fn_ty` is the mono-store unification of
  `StaticDispatchCallPlan.callable_ty` and the target procedure type
- every `call_proc.proc` and `proc_value.proc` is a `ProcedureValueRef`, never a
  raw `Symbol`; implementation-local procedure handles are allowed only inside
  the live mono procedure store and cannot appear in exported mono MIR snapshots
- nominal/custom equality becomes `call_proc` to `is_eq`
- structural equality remains structural
- transparent aliases preserve source/debug identity but do not create runtime
  wrappers; nominals preserve nominal identity
- `proc_value` is distinct from `call_proc`
- mono `proc_value` captures are empty
- `call_proc` carries exact requested mono source function types
- every `call_proc` and `call_value` carries all fixed-arity source arguments
- no call node encodes automatic currying or partial application
- `call_proc` is not an executable direct call
- `call_proc` does not target local functions or closures
- mono MIR consumes `ResolvedValueRef` for every checked value reference
- top-level and imported compile-time constants become `const_ref`, never
  ordinary local `var_`
- top-level, imported, hosted, platform, and promoted procedure calls become
  `call_proc`, never `call_value(var_(proc), args)`
- top-level, imported, hosted, platform, and promoted procedure values become
  empty-capture `proc_value`
- local bindings that shadow top-level or imported names still lower as local
  value refs and may be captured later
- string literals, string interpolation segments, bytes literal payloads, string
  pattern tests, and user-written crash messages are interned into the
  program-owned literal pool and represented in mono MIR as `ProgramLiteralId`
- imported checked procedure templates with literal payloads read bytes from the
  imported checked artifact's string table, never from exporter or importer
  `ModuleEnv`

Row-finalized mono MIR:

- every row-finalized mono MIR expression still has a mono type
- every literal-bearing node still references the same program literal pool, or
  an explicitly remapped pool whose remap rewrote all references in the same pass
- no name-only record construction, access, update, or destructuring node exists
- no name-only tag construction, pattern, or payload projection node exists
- every record operation carries `RecordShapeId` and `RecordFieldId` values
- every tag operation carries `TagUnionShapeId`, `TagId`, and `TagPayloadId`
  values
- shape interning reuses one logical shape record across repeated uses of the
  same field or tag-union shape
- row shape keys do not include payload slot types
- row shape keys store canonical `RecordFieldLabelId` and `TagLabelId` values, not
  module-local `Ident.Idx`; cross-module tests must prove identical source labels
  with different local identifier-store indexes intern to the same logical row
  labels, and test-only wrong-store corruptions panic in debug builds
- record construction stores source evaluation order separately from finalized
  `RecordFieldId` assembly order
- record update stores base/update evaluation order separately from finalized
  `RecordFieldId` assembly order
- tag construction stores source payload evaluation order separately from full
  `TagUnionShapeId`, selected `TagId`, and finalized `TagPayloadId` assembly
  order
- `Err("x")` in a full `[Ok(I64), Err(Str)]` context uses the finalized `Err`
  ID from the full union, never a singleton `[Err(Str)]` shape
- no later-stage API can lazily compute logical row indexes by sorting names,
  scanning rows, scanning expressions, or inspecting physical layout order

Lifted MIR:

- every lifted procedure target exists
- lifted MIR keeps literal ids as `ProgramLiteralId` and does not re-resolve
  checked artifact string ids
- every procedure capture is an explicit `CaptureSlot`
- every captured value reference is an explicit `capture_ref`
- recursive local-function groups compute captures to a fixed point
- capture discovery is keyed by resolved symbols and mutable-version records,
  not display-name comparisons
- source `match`, destructuring, `for`, block-local, and shadowed binders are
  represented in the capture scope stack
- captured mutable values are explicit version or phi records
- every local function or closure value is an explicit `proc_value`
- aliases of local functions and closures become explicit `proc_value`
- every `proc_value` capture arg is explicit and in slot order
- every call through a local function or closure is `call_value`
- no captured source symbol remains as ordinary `var_`
- no bare procedure-symbol `var_` values exist
- `call_proc` and `proc_value` targets are rewritten by explicit maps
- lifted MIR capture discovery never subtracts a global/top-level symbol set as
  part of the algorithm
- lifted MIR capturable scopes contain only local params, local binders, pattern
  binders, mutable versions, and local procedures
- top-level/imported/hosted/platform-required/promoted values cannot appear as
  `CaptureSlot` sources
- no dispatch nodes exist

Lambda-solved MIR:

- callable sets are explicit
- lambda-solved MIR keeps literal ids as `ProgramLiteralId` and does not attach
  literal payloads through expression-indexed maps
- captures are attached to callable members
- callable-set members are canonical ordered finite maps
- repeated callable member instances have identical capture slots inside the same
  specialization-local lambda-solved type store
- mismatched capture slots for the same specialization-local callable member
  instance trigger debug-only assertions
- erasure requirements are explicit
- `BoxBoundary` stores box type, payload source type, payload boundary type,
  direction, representation roots, and `BoxPayloadRepresentationPlan`
- boxed payload boundary types structurally rewrite every reachable function
  slot to erased callable representation, including function arguments and
  returns reachable inside the explicit `Box(T)` payload
- `RepresentationStore` has distinct roots for every expression result, binder,
  pattern binder, procedure parameter, procedure return, capture slot,
  callable requested-function occurrence, mutable variable version, and loop phi
- `ValueInfoStore` has explicit value metadata for every expression result,
  binder, pattern binder, mutable version, capture slot, projection, call result,
  and procedure-value occurrence
- recursive specialization instances reserve public parameter, return, capture,
  and whole-function value roots before body lowering can reference them
- recursive specialization SCCs use exactly one sealed
  `RepresentationSolveSession` per SCC, including one-member non-recursive SCCs
- recursive cross-procedure value-flow edges use `ProcPublicValueRoots`, not
  source symbols, procedure names, environment lookup, or body scans
- no unsealed `ValueInfoId`, `BindingInfoId`, `CallSiteInfoId`, projection info,
  solve session, callable emission plan, adapter key, capture-shape key, or
  executable specialization key reaches executable MIR
- every expression and binder references its `ValueInfoId`/`BindingInfoId`
  directly instead of relying on an expression-indexed map
- lexical scope construction maps source symbols only to `BindingInfoId` values
  and no ad hoc procedure-target or boxed/aggregate/callable semantic fields
- aliases such as `f = inc` produce value-flow edges and callable metadata on
  the binder/use, not an `exact_callable_aliases` entry
- callable values stored in records, tuples, tags, lists, captures,
  compile-time constants, branch joins, mutable versions, parameters, and
  returns remain reachable through explicit `ValueInfoId` metadata
- `CallSiteInfo` exists for every `call_value` and names the callee value,
  argument values, result value, requested whole-function root, and dispatch
  plan
- first-class `Box.box` and `Box.unbox` calls create `BoxBoundaryId` records
  through checked procedure metadata and call-site metadata
- every aggregate projection has `ProjectionInfo`; record and tag projections
  use row-finalized IDs
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
- hosted, platform, and intrinsic callable ABI metadata never introduces
  non-`Box(T)` erasure
- imported, opaque, hosted, and platform-owned boxed payload traversal requires
  explicit representation capabilities
- opaque nominal atomic traversal requires an explicit
  `NoReachableCallableSlotsProof` for the exact nominal identity and
  instantiated type arguments
- finite callable-set erasure preserves source member metadata for executable
  adapter synthesis
- erased adapter keys include finite callable-set identity, erased function
  signature with `ErasedFnAbiKey`, and capture shape
- generalized templates are clone-instantiated and fully resolved before executable
  lowering consumes them
- generalized template instantiation allocates fresh callable variables and
  representation variables together with ordinary type variables
- generalized procedure templates are never consumed directly by executable MIR
- procedure and recursive-SCC template generalization excludes variables
  reachable from the already-bound outer environment
- recursive specialization SCCs reserve all procedure/callable/capture/adapter
  nodes before body solving publishes executable keys
- executable specialization keys are canonical structural keys from
  specialization-local instantiated representation stores
- executable specialization keys recursively encode nested function-valued
  argument, return, capture, record, tag, list, box, and nominal slots as
  callable-set or erased-fn executable value keys
- boxed payload representation plans, callable-set keys, capture-shape keys,
  erased signature keys, and erased adapter keys are computed from the
  specialization-local lambda-solved type store
- every erased callable representation carries non-empty `BoxBoundaryId`
  provenance
- every erased `CallableValueEmissionPlan` names either an already-erased value,
  a `ProcValueErasePlan`, or an `ErasedAdapterKey`
- instantiated capture refs get their types from `CaptureSlotInstance`, not from
  environment lookup or body scanning
- every `CaptureShapeKey` slot is encoded as `CanonicalExecValueTypeKey` or the
  canonical erased capture type key, never as a source type id, lambda-solved
  type id, layout id, source name, generated symbol text, or expression id
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
- executable MIR owns or forwards a complete program literal pool and every
  literal-bearing executable node references it with `ProgramLiteralId`
- executable MIR verifier rejects `base.StringLiteral.Idx`,
  `CheckedStringLiteralId`, raw byte slices, or checked-artifact string table
  pointers in executable literal, bytes, source-match string test, or crash
  payload fields
- every `call_proc` lowers through `CallProcExecutablePlan` before becoming
  `call_direct`
- direct call arg/result types match signatures
- direct, erased, and callable-set calls preserve fixed Roc arity
- every finite callable-set call lowers to an explicit `callable_match`
- every finite callable-set value construction consumes a
  `CallableSetConstructionPlan` for that exact value occurrence
- every present `CallableSetConstructionPlan` is owned by exactly one
  `CallableValueInfo`, has `construction.result` equal to that occurrence's
  `ValueInfoId`, and the occurrence's `CallableValueEmissionPlan` is
  `finite_callable_set(construction.callable_set_key)`
- no finite non-erased `proc_value` occurrence that must be emitted as a value
  reaches executable MIR without a `CallableSetConstructionPlan`, and no carried
  finite callable-set value from a parameter, projection, branch join, call
  result, return, capture, aggregate field, bridge, or constant incorrectly
  carries a construction plan
- every `CallableSetConstructionPlan` points at a valid
  `CanonicalCallableSetDescriptor` member, and its `capture_values` count and
  order match that member's `CaptureSlot.index` ordered capture schema
- every `CallableSetConstructionPlan.source_fn_ty` exactly equals the selected
  descriptor member's `ProcedureCallableRef.source_fn_ty` after canonical type
  normalization
- debug-only invalid executable-MIR verifier tests corrupt
  `CallableSetConstructionPlan.result`, corrupt the owning
  `CallableValueInfo.emission_plan`, remove the construction plan from a
  finite-emitted `proc_value`, add a construction plan to an already-carried
  finite callable-set value, and change `construction.source_fn_ty`; each
  corruption must panic at the first executable boundary that consumes the
  record
- executable MIR evaluates callable-set construction captures exactly once and
  stores them in one member payload before any later `callable_match` can
  destructure them
- every `callable_match` binds the callable expression and original call
  arguments once, before member branching
- every `callable_match.requested_source_fn_ty` exactly equals the original
  `call_value.requested_fn_ty` after canonical type normalization
- every `callable_match` branch corresponds to exactly one callable-set member
- every `callable_match` branch names its member through
  `CallableSetMemberRef { callable_set_key, member_index }`, and derives the
  member procedure value, discriminant/tag order, capture-slot schema, and capture
  shape from that key under debug verification; it must not store raw `Symbol` as
  member identity
- every `callable_match` branch's descriptor member
  `ProcedureCallableRef.source_fn_ty` and
  `ExecutableSpecializationKey.requested_fn_ty` exactly equal
  `callable_match.requested_source_fn_ty`; same-template/different-type members
  must not share one executable specialization
- debug-only invalid executable-MIR verifier tests corrupt
  `callable_match.requested_source_fn_ty`, corrupt one branch's descriptor-member
  source function type, and corrupt one branch's
  `ExecutableSpecializationKey.requested_fn_ty`; each corruption must panic
  before IR lowering
- every `callable_match` branch has a reserved executable specialization
- every `callable_match` branch stores exact `direct_args` as
  `ExecutableValueRef` handles, never as arbitrary expressions
- every `callable_match` branch passes all fixed-arity source arguments exactly
  once, plus only the optional trailing capture record
- every `callable_match` branch produces a branch-local result and bridges it
  into one shared result temp when needed
- `no_return` callable-match branches do not constrain returning branch result
  representation
- ordinary source `match` does not satisfy callable-set lowering verification
- every ordinary source `match` lowers to a concrete `SourceMatch` node
- every `SourceMatch` evaluates its scrutinees exactly once
- every `SourceMatch` consumes an explicit `PatternDecisionPlan`
- every `SourceMatch` extracts selected tag payload records once before
  projecting payload binders by finalized `TagPayloadId`
- every `SourceMatch` supports records, tuples, lists, literals, opaque unwraps,
  newtypes, tags, and guards through explicit decision-path records
- every returning `SourceMatch` branch joins into one shared result temp through
  explicit bridges when needed
- singleton finite callable-set calls still lower to `callable_match`
- every callable-set value has explicit member capture payload metadata
- every callable-set value has deterministic member tag ordering
- every callable-set capture payload has deterministic field ordering
- callable-set member tag ordering uses `ProcOrderKey`, not `Symbol.raw()`
- every packed erased function has explicit capture metadata
- packed erased functions distinguish no capture from typed zero-sized captures
- hidden erased capture arguments are appended after fixed-arity source
  arguments only when the erased signature records a capture type
- finite callable-set values crossing erased `Box(T)` boundaries synthesize
  erased adapters
- finite callable-set values crossing erased representation at a branch join,
  return, capture, mutable join, or aggregate field also synthesize adapters
  when the solved `CallableValueEmissionPlan` has `BoxBoundaryId` provenance
- executable MIR consumes `BoxPayloadRepresentationPlan` and does not make
  semantic erased-shape compatibility decisions
- executable MIR consumes `CallableValueEmissionPlan` and does not decide erased
  packaging by checking whether the value occurrence is syntactically inside a
  `Box(T)` expression
- executable MIR consumes `ValueInfoId`, `BindingInfoId`, `ProjectionInfoId`,
  and `CallSiteInfoId` records and does not recover callable identity from
  syntax, source aliases, environment fields, or procedure-name lookup
- executable MIR variable lowering uses the occurrence's exported value metadata;
  the variable name is only a lexical handle
- executable MIR lowers first-class intrinsic calls from call-site dispatch
  metadata and `ProcTarget` intrinsic role, not from callee expression shape
- checking reports erased-boundary roots other than `Box(T)` before executable
  MIR; executable MIR only debug-verifies that none reached it
- checking or lambda-solved debug verification rejects erased callable
  representation with empty or non-`Box(T)` provenance before executable MIR can
  consume it
- checking reports imported, opaque, hosted, and platform-owned boxed payload
  traversal without explicit representation capabilities before executable MIR;
  executable MIR only debug-verifies that none reached it
- every erased capture record has deterministic field ordering
- every erased call has an explicit erased function type
- every erased call has an explicit `ErasedFnAbiKey` inside `ErasedFnSigKey`
- every runtime mutation site has an explicit runtime uniqueness check
- executable MIR contains no semantic parameter-mode solver output
- first-class intrinsic references use explicit wrapper procedures
- every non-primitive `LowLevelCall` has a complete
  `LowLevelValueFlowSignature`
- `Box.box` low-level value-flow creates a `BoxBoundaryId`, links the payload
  argument to the boxed payload representation, and creates
  `require_box_erased(boundary)`
- `Box.unbox` low-level value-flow creates a `BoxBoundaryId`, links the boxed
  payload representation to the result, and attaches that boundary as erased
  callable provenance when the payload contains callable slots
- list low-level value-flow signatures connect element and container
  representations explicitly for `List.get_unsafe`, `List.set`,
  `List.append`, `List.prepend`, `List.concat`, `List.split_first`, and
  `List.split_last`
- logical field indexes are preserved until LIR resolves physical offsets
- bridges connect concrete executable types
- bridges consume `ExecutableValueRef` handles only, never arbitrary source
  expressions
- every bridged operand with evaluation, allocation, reference-counting, or
  control-flow significance is evaluated exactly once before the bridge consumes
  it
- callable lowering, erased packaging, boxed payload boundaries, mutation sites,
  and bridges expose explicit operands and RC-effect metadata for LIR ARC
  insertion
- no source-expression side tables exist

IR/LIR:

- public checked-artifact-to-LIR lowering APIs return only resource errors such
  as `Allocator.Error`
- IR owns or forwards the program literal pool from executable MIR and represents
  literal and crash payloads with `ProgramLiteralId`
- IR-to-LIR lowering interns each used `ProgramLiteralId` into `LirStore.strings`
  exactly once per distinct literal byte payload and rewrites LIR payloads to
  `base.StringLiteral.Idx`
- no MIR or IR AST type contains `base.StringLiteral.Idx`, and no LIR/backend
  code reads strings from checked artifacts or source module string stores
- public semantic lowering APIs do not return `NoRootProc`, `NoRootDefinition`,
  `MissingRoot`, `MethodNotFound`, `UnsupportedSourceType`,
  `UnsupportedLayout`, `SchemaLayoutMismatch`, or `MissingInterfaceCapability`
- direct calls lower to direct calls
- erased calls lower to erased calls
- no method/dispatch operation exists
- logical layout graph commitment reserves nodes before children and handles
  recursive physical indirection by SCC over slot edges
- recursive physical indirection is not source `Box(T)` and not erased callable
  representation
- constructors, source `match` payload extraction, field access, capture access,
  callable-set payload access, erased capture access, and RC plans consume the
  same committed recursive-slot mapping
- every value-producing LIR statement exposes sufficient operands and
  refcounted-layout metadata for ARC insertion
- LIR ARC insertion computes explicit `incref`, `decref`, and `free` from LIR
  values and control flow; it does not infer procedure contracts as semantic
  truth
- `RcInsert` is the only non-builtin stage that emits explicit `incref`,
  `decref`, and `free`
- backends execute explicit LIR RC statements and perform no ordinary RC
  analysis

## Required Behavioral Tests

Cross-artifact literals:

- imported string constant:

  ```roc
  # A.roc
  foo = "from A"

  # Main.roc
  import A
  main = A.foo
  ```

  must lower by reading `"from A"` from `A`'s checked artifact string table,
  interning it into the lowered program literal pool, and finally interning it
  into `LirStore`; no stage may read `A`'s `ModuleEnv` string store after
  artifact publication.
- imported generic function specialization containing a string literal must
  produce the same runtime bytes at every concrete instantiation, and the mono
  specialization must not carry raw `base.StringLiteral.Idx` from the exporter.
- imported function containing `crash "boom from A"` must report the imported
  crash message through LIR/backends by way of the lowered program literal pool
  and `LirStore`, not by preserving a checked artifact string id.
- source `match` with string literal patterns must build `PatternTest.str_literal`
  from `ProgramLiteralId` values and must compare by literal bytes after lowering,
  not by source string-store index.
- compile-time constants containing strings, byte literals, lists of strings,
  records with strings, tags with strings, and callable-containing constants with
  string captures must reify through the program literal pool when materialized
  as runtime values.
- two modules with different source string-store indexes for the same literal
  bytes must lower to one program literal id after deduplication within one
  lowered program; two different byte payloads must never alias even if their
  artifact-local ids are numerically equal.

Static dispatch:

- generic dispatch specializes to different nominal method targets
- generic target methods specialize at exact monomorphic function types
- generic procedures containing static dispatch are registered as checked
  procedure templates and lower only when a concrete mono specialization is
  requested; tests must fail if the implementation visits every top-level
  function before root/call specialization requests exist
- generic exported procedures containing static dispatch remain export-table
  templates until imported at concrete types; exporting the module alone must not
  perform method-owner lookup
- ambiguous static dispatch whose `dispatcher_ty` is not determined by the
  checked callable type, enclosing expression type, and requested mono function
  type is reported before artifact publication
- chained dispatch uses each dispatch site's own `StaticDispatchCallPlan` and
  the earlier call's unified return slot
- chained dispatch does not call any expression-based method resolver
- type-var alias dispatch resolves from the specialized dispatcher type selected
  by `StaticDispatchCallPlan.dispatcher_ty`
- primitive methods resolve through builtin primitive owners
- list methods resolve through the builtin `List` owner
- box methods resolve through the builtin `Box` owner
- custom equality lowers to `call_proc` `is_eq` before executable MIR and direct
  executable `is_eq` after executable MIR
- inequality lowers to `call_proc` `is_eq` plus `bool_not` before executable MIR
  and direct executable `is_eq` plus `bool_not` after executable MIR
- resolved direct calls, static-dispatch calls, and custom equality calls are
  represented as `call_proc`; no test expectation may accept
  `call(var_(proc), args)` as an equivalent mono MIR shape
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
- closure references to top-level constants, imported constants, top-level
  procedures, imported procedures, hosted procedures, platform-required
  procedure bindings, and promoted top-level procedures do not create
  `CaptureSlot`s
- closure references to local bindings that shadow top-level or imported names
  do create ordinary local captures when used from an inner procedure
- top-level/imported/hosted/platform-required/promoted procedure values inside closures
  lower as empty-capture `proc_value`, not as captured variables
- recursive local function
- mutually recursive local functions
- recursive local functions that reference sibling procedure values require
  fixed-point capture propagation
- recursive local functions that return sibling procedure values reserve public
  value roots before any sibling body is solved
- recursive local functions that capture values containing self/sibling
  procedure values solve through one shared recursive specialization
  `RepresentationSolveSession`
- recursive local functions that return or capture aggregates containing
  self/sibling procedure values, such as records, tuples, tags, lists, boxes, and
  source `match` branch results, create `CaptureProcValueEdge` records during
  lifted MIR graph construction and converge through the same fixed point as
  direct self/sibling procedure-value references
- closure returned from a function
- closure passed as an argument
- `call_proc` with exact executable argument and result representations lowers
  through `CallProcExecutablePlan` to `call_direct`
- `call_proc` whose argument or result contains an explicit `Box(T)` erased
  payload still honors the boxed payload representation plan before emitting its
  direct call
- singleton `call_value(proc_value(...))` lowers to `callable_match`
- captured local function calls lower through explicit `proc_value` captures
- a source `match` whose branches return different closures constructs finite
  callable-set values in the branch bodies, joins them as ordinary values, allows
  the joined callable to be stored in a record/tag/list or returned from the
  function, and later calls it through `callable_match`
- a callable value stored in an aggregate before being called preserves the
  original `CallableSetConstructionPlan`, canonical callable-set member, and
  evaluated capture payload; the later call must not recover the member from
  source syntax or rebuild captures from source variables
- finite callable-set calls evaluate callable and arguments exactly once before
  branch dispatch
- finite callable-set construction evaluates captured values exactly once before
  constructing the member payload, and later `callable_match` branches consume
  only the stored payload
- finite callable-set branch direct calls receive source args plus optional
  trailing capture record
- finite callable-set branch results bridge into one shared callable-match
  result temp
- bridges around calls, boxes, tag construction, record construction, constants,
  erased adapters, and result joins consume already-evaluated values only
- boxed erased-boundary packaging with capture record
- finite callable-set value crossing an erased `Box(T)` boundary synthesizes an
  erased adapter whose body uses `callable_match`
- finite callable-set branch joined with `Box.unbox(...)` result synthesizes an
  erased adapter from solved `CallableValueEmissionPlan` provenance even though
  the finite branch is not syntactically inside `Box.box(...)`
- structural erased-boundary coercion through records, tuples, tags, `List(T)`,
  nested `Box(T)`, function arguments and returns, and nominal backing types only
  inside an explicit `Box(T)` payload
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
  representation metadata; erased-call ABI metadata is allowed only for callable
  slots that are already erased because of explicit `Box(T)` boundaries
- finite callable-set calls use the uniform ARC call boundary; no branch
  computes or exposes a special parameter mode for the whole `callable_match`
- `Box.unbox` is an ordinary value operation for ARC purposes; no test may rely
  on move-out semantics for `Box.unbox`
- `packed_erased_fn`, erased adapters, `callable_match`, `Box.box`,
  `Box.unbox`, mutation sites, and bridges expose explicit operands and
  RC-effect metadata consumed by LIR ARC insertion

Cor-derived lowering stress tests:

These tests use `~/code/cor/experiments/lss` as a semantic reference for
lambda-set and closure lowering, not as syntax to copy. Cor uses `when`, curried
functions, whitespace application, and runtime top-level thunks. The production
tests must use current Roc syntax: `match`, fixed-arity functions, `|x| x`
lambdas, parenthesized comma-separated calls, and no runtime top-level callable
objects.

- port the generic higher-order callable specialization shape from
  `test/generic-higher-order-call.roc`: one generic identity-like procedure
  instantiated at two concrete callable shapes, one captureless and one with
  captures. The MIR-family assertion is that the two executable specializations
  have distinct recursive `CanonicalExecValueTypeKey` argument keys, distinct
  callable-set keys, and no raw type-store ids in their keys.
- extend that generic higher-order shape with the same source procedure template
  used at two concrete function types, such as `I64 -> I64` and
  `Str -> Str`. The expected lambda-solved output must contain two finite
  callable leaves whose descriptor members differ by
  `ProcedureCallableRef.source_fn_ty`, and the expected executable output must
  reserve separate `ExecutableSpecializationKey` rows. A test that only compares
  procedure template identity is insufficient.
- add a finite callable construction/call round-trip test where a source
  `match` branch constructs a selected callable member, a later branch join
  carries the already-constructed callable-set value, and the later call lowers
  through `callable_match`. The expected records must show that only the branch
  construction occurrence has a `CallableSetConstructionPlan`; the join value
  and final callee value carry finite callable-set emission metadata without
  inventing a selected member.
- add a same-source-function-type test with two unrelated local values of type
  `I64 -> I64`, one captureless and one capturing an `I64`. The expected
  lambda-solved output must allocate distinct callable variables at import, and
  the expected executable keys must remain distinct unless an explicit
  value-flow edge joins the two values.
- port the generic guarded-closure shape from
  `test/generic-call-with-guarded-closure.roc`: a local identity function inside a
  generic procedure specializes independently for each concrete argument type.
  The MIR-family assertion is that lifted local procedure identity includes the
  owning mono specialization, so capture-slot equality is checked only within the
  specialization-local callable member instance.
- port the recursive captured callable shapes from
  `test/capture-recursive-function.roc`,
  `test/lambda-set-basic/captures-call-recursive.roc`, and
  `test/lambda-set-basic/recursive-call.roc`: recursive local functions that
  return or capture procedure values must compute captures to a fixed point,
  build explicit `proc_value` payloads for self/sibling references, and produce
  finite canonical callable/capture keys.
- port `test/lambda-set-basic/dispatch-closure.roc`: source `match` branches
  return closures with different capture payloads. The expected output must join
  the branch results into one finite callable-set value, preserve each branch's
  capture payload exactly, and lower the later call through mandatory
  `callable_match`.
- port `test/lambda-set-basic/dispatch-mixed.roc`: source `match` branches
  return a mixture of captureless and captured closures. The expected output
  must distinguish no-capture members from captured members in the canonical
  callable-set key and must not invent an empty runtime closure object for the
  captureless branch.
- port `test/lambda-set-basic/dispatch-toplevel.roc`: source `match` branches
  return top-level procedures. The expected output must construct finite
  callable-set values whose members are procedure values, and the later call
  must still lower through `callable_match`, not through a direct-call shortcut.
- port `test/record/specialize-record-size.roc`: a row-polymorphic field access
  used at different concrete record shapes must produce distinct mono
  specializations and distinct finalized `RecordShapeId`/`RecordFieldId` pairs.
  No stage after row finalization may recover field positions by name.
- port `test/record/empty.roc`: the empty record and zero-field row case must
  survive every MIR-family boundary. Row finalization must intern a real
  zero-field `RecordShapeId`, construction must produce the zero-field
  structural value, pattern/destructuring lowering must not assume at least one
  field, executable MIR must preserve the zero-field value type, and IR/LIR must
  materialize the correct zero-sized or empty-struct representation for the
  target. This test must exercise top-level constants, local values, parameters,
  return values, record update where applicable, and source `match` patterns
  involving `{}`.
- port `test/linked-list/length.roc` and `test/linked-list/map.roc`: recursive
  tag-union layout and recursive higher-order specialization must lower through
  graph-based recursive physical indirection, finite callable-set dispatch, and
  stable executable specialization keys.
- port `test/identity.roc` and `test/map-int.roc` as low-cost sentinel fixtures
  for the full monotype, row-finalized, lifted, lambda-solved, executable MIR,
  IR, and LIR path.
- port the task/CPS encodings from `test/task/handler-simple.roc`,
  `test/task/stdin-stdout-annotated.roc`, and `test/task/roc-issue-5464.roc`.
  These stress recursive tag unions containing function values, continuations
  stored in tag payloads, nested callable dispatch, and repeated specialization.
  The assertions must target MIR-family invariants, not Cor's runtime thunking or
  curried call shape.
- also port the inferred task/CPS shape from
  `test/task/roc-issue-5464-infer.roc`. The inferred variant must prove that the
  recursive continuation/callable representation is solved from value flow and
  does not depend on source annotations to select callable members.
- add boxed erased callable tests where a direct `proc_value` with captures crosses
  an explicit `Box(T)` boundary. The expected executable MIR must contain
  `ProcValueErasePlan`, reserve the erased executable specialization before
  packing, and emit `ErasedFnValue` with the exact `ErasedFnSigKey`.
- add boxed erased callable tests where a finite callable-set value crosses an
  explicit `Box(T)` boundary. The expected executable MIR must synthesize an
  erased adapter keyed by `ErasedAdapterKey`, and the adapter body must dispatch
  with `callable_match`.
- add branch-join erased callable tests where one branch returns
  `Box.unbox(boxed)` and another branch returns a finite closure. The expected
  lambda-solved output must put both branch results in one erased representation
  class with `BoxBoundaryId` provenance, and executable MIR must pack the finite
  closure from its `CallableValueEmissionPlan` even though that branch is not
  syntactically inside `Box.box(...)`.
- add higher-order boxed erased tests where the boxed payload type contains a
  function in an argument position, a return position, and both positions. The
  expected lambda-solved output must show `BoxPayloadRepresentationPlan.function`
  recursively transforming function arguments and returns before rewriting the
  callable child to erased representation.
- add an explicit promoted-wrapper argument-transform test where a finite
  callable value is passed through an erased function argument position:

  ```roc
  make_boxed : {} -> Box((I64 -> I64) -> I64)
  make_boxed = |_| Box.box(|f| f(41))

  apply_boxed : (I64 -> I64) -> I64
  apply_boxed = Box.unbox(make_boxed({}))

  main : I64
  main = apply_boxed(|x| x + 1)
  ```

  The expected checked artifact publishes an erased promoted wrapper whose
  ordinary argument has an explicit `ExecutablePayloadTransformPlan` from the
  finite callable-set endpoint to the erased-call argument endpoint. Executable
  MIR must lower that transform by emitting `packed_erased_fn` with the finite
  callable-set value as the hidden capture and an adapter keyed by the full
  `ErasedAdapterKey`. A structural bridge is insufficient and forbidden for
  this conversion.
- add an explicit promoted-wrapper result-transform test where a boxed erased
  callable returns another callable:

  ```roc
  make_boxed : {} -> Box(I64 -> (I64 -> I64))
  make_boxed = |_| Box.box(|n| |x| x + n)

  make_adder : I64 -> (I64 -> I64)
  make_adder = Box.unbox(make_boxed({}))

  main : I64
  main = make_adder(5)(10)
  ```

  The expected representation after unboxing keeps the returned callable in
  erased representation. Executable MIR must not attempt to recover a finite
  callable set from the erased return value.
- add explicit aggregate payload-transform tests where the erased boundary
  reaches callable leaves inside records and lists:

  ```roc
  make_boxed : {} -> Box({ f : I64 -> I64 } -> I64)
  make_boxed = |_| Box.box(|r| r.f(1))

  apply_record : { f : I64 -> I64 } -> I64
  apply_record = Box.unbox(make_boxed({}))
  ```

  ```roc
  make_boxed : {} -> Box(List(I64 -> I64) -> I64)
  make_boxed = |_| Box.box(|fs| List.len(fs))

  apply_list : List(I64 -> I64) -> I64
  apply_list = Box.unbox(make_boxed({}))
  ```

  The expected transform recursively rebuilds or maps only the aggregate slots
  whose executable representation changes. The record field `f` and the list
  element value are packed through explicit callable transforms with
  `BoxBoundaryId` provenance. `list_reinterpret` is forbidden when the element
  representation changes.
- add explicit tag-union payload-transform tests where callable leaves are
  stored in tag payloads and cross an erased boxed boundary:

  ```roc
  make_boxed : {} -> Box([Apply(I64 -> I64), Keep(I64)] -> I64)
  make_boxed = |_| Box.box(|value|
      match value {
          Apply(f) => f(1)
          Keep(n) => n
      }
  )

  apply_tag : [Apply(I64 -> I64), Keep(I64)] -> I64
  apply_tag = Box.unbox(make_boxed({}))

  main : I64
  main = apply_tag(Apply(|x| x + 1))
  ```

  The expected checked artifact publishes a `tag_union` payload transform with
  one explicit `PayloadTransformTagCase` for `Apply` and one for `Keep`. The
  `Apply` case maps source payload index `0` to target payload index `0` through
  a callable-to-erased child transform with `BoxBoundaryId` provenance. The
  `Keep` case uses identity. Executable MIR must lower this as a
  compiler-owned tag transform switch, not as source `match` and not as a
  structural bridge.
- add singleton/full tag reshaping tests where the singleton payload contains a
  callable child whose representation changes. The expected transform must use
  explicit tag cases and payload edges; `singleton_to_tag_union` and
  `tag_union_to_singleton` structural bridges are valid only when the payload
  transform is identity or another true structural bridge.
- add nested `Box(T)` payload-transform tests where an existing boxed aggregate
  contains callable leaves:

  ```roc
  make_boxed : {} -> Box({ inner : Box(I64 -> I64) } -> I64)
  make_boxed = |_| Box.box(|record| Box.unbox(record.inner)(1))

  apply_record : { inner : Box(I64 -> I64) } -> I64
  apply_record = Box.unbox(make_boxed({}))

  main : I64
  main = apply_record({ inner: Box.box(|x| x + 1) })
  ```

  The expected transform uses `box_to_box` or `box_to_payload` only where the
  explicit boundary requires it. The nested box transform does not itself
  authorize callable erasure; only the callable leaf transform with non-empty
  `box_erasure` provenance may change finite callable representation to erased
  callable representation.
- add construction-mode aggregate tests proving that values built under a known
  erased representation are constructed directly in that representation. For a
  record, tag, or list literal whose target representation is already the boxed
  erased payload representation, executable MIR must lower child callable leaves
  directly to erased values while constructing the aggregate. It must not first
  construct a finite aggregate and then emit a record, tag, or list
  existing-value transform.
- add a Box-adapted `test/erased/erased-function-call.roc`: a closure capturing
  another callable plus ordinary data must cross an explicit
  `Box(I64 -> I64)` boundary, be unboxed, and later join with the original
  finite callable path. The expected lambda-solved output must name the exact
  `BoxBoundaryId` responsible for erased representation. The expected
  executable MIR must contain the `ProcValueErasePlan` or erased adapter needed
  for that boundary, promote/lower the captured callable correctly, and keep the
  non-boxed finite callable path as a finite callable-set value until it reaches
  the explicit erased join.
- add a Box-adapted `test/erased/erased-map2.roc`: a generic map2-like shape
  must run through boxed `a`, boxed `b`, and a boxed mapper. The test must cover
  both non-callable boxed payloads such as `Box(I64)` and callable boxed
  payloads such as `Box({} -> I64)` through the same generic path. The expected
  output must prove that `Box(I64)` does not introduce callable erasure, that
  `Box({} -> I64)` introduces callable erasure only through its explicit
  `BoxBoundaryId`, and that records and tag payloads containing function values
  keep explicit finite or erased callable child representations rather than a
  runtime function object.
- add a Box-adapted `test/erased/unsafe-cast.roc`: any Roc source shape that
  attempts to treat one boxed callable payload as an incompatible boxed callable
  payload must be rejected during type checking or checking finalization before
  artifact publication. Post-check stages must never see an untyped erased
  value, an arbitrary boxed cast, or a request to recover an erased signature
  from runtime bytes.
- add non-boxed higher-order container tests where records, tags, lists, and
  nominals contain function values but do not flow into `Box(T)`. The expected
  executable value types must contain `callable_set` or `erased_fn` for the
  function-valued slots, never a runtime function object with argument and return
  fields, and never erased representation unless an explicit `Box(T)` boundary
  reaches that slot.
- add hosted/platform callable ABI tests proving that hosted metadata does not
  introduce erasure for non-`Box(T)` callable slots. A hosted callable slot that
  requires erased representation must be exposed through an explicit `Box(T)`
  slot during checking; otherwise checking reports the problem before artifact
  publication.
- add debug-verifier tests that intentionally construct invalid internal MIR in
  test-only helpers: unresolved callable variables in executable inputs,
  generalized variables in executable inputs, and function-typed executable
  values that did not collapse to `callable_set` or `erased_fn`. Debug builds
  must assert immediately; release builds use `unreachable` for the equivalent
  compiler-invariant path.
- add hidden-capture ABI tests distinguishing no capture, typed zero-sized
  capture, and boxed runtime capture. The expected keys must prove
  `ErasedFnSigKey.capture_ty` participates in equality; same args, same return,
  and same `ErasedFnAbiKey` with different `capture_ty` must not merge.
- add source `match` tests with nested tag patterns, multi-scrutinee patterns,
  record and tuple patterns, list exact/spread/rest patterns, guards that can
  fail after structural tests pass, and binders under several nested paths. The
  expected executable MIR must contain `PatternPathValuePlan` records and
  materialize them once per selected path at the control-flow points where they
  are needed. It must not contain a single branch-level payload-record temporary.

Compile-time constants:

- simple top-level constants evaluate through the LIR interpreter and appear in
  the compile-time value store
- top-level constants that call helper functions evaluate through the same LIR
  interpreter path
- compile-time root dependencies are evaluated in topological order
- compile-time root dependencies include callable-aware summary records for
  `call_proc`, finite `call_value` member targets including singleton sets,
  erased `call_value` code refs and capture dependencies, constant-graph callable
  leaves, and promoted-capture callable leaves
- compile-time dependency summary collection may record
  `AvailabilityUse.local_root` only inside checking finalization and only while
  building `CompileTimeRootDependencyGraph`; runnable mono MIR must never contain
  pending top-level values or uninstantiated top-level use templates
- a compile-time constant that calls a direct function whose body references a
  callable binding depends on that callable binding root even when the root
  expression does not name it directly
- a compile-time constant that calls a promoted function-valued binding depends
  on that callable binding root and consumes it as `procedure_binding`
- compile-time dependency tests include a resolved non-procedure top-level
  reference in a branch that is not dynamically taken by the interpreter. The
  expected result is still an availability edge and, if it forms a cycle, a
  checking diagnostic before artifact publication. This test proves dependency
  ordering follows the checking-time availability graph, not a demand trace of
  one execution.
- non-procedure compile-time root cycles are checking diagnostics
- compile-time constants containing strings, lists, records, tuples, tag unions,
  boxes, transparent aliases, and nominals reify to logical constant nodes.
  Transparent aliases emit no runtime wrapper; nominals materialize through
  their recorded nominal representation capability.
- top-level function declarations do not become runtime constant thunks
- top-level function-valued declarations do not become runtime closure objects,
  runtime global callable-value objects, runtime initializer procedures, or
  runtime thunks
- top-level function declarations are published directly as `procedure_binding`
  without being evaluated just to prove they are callable
- top-level function-typed expressions such as `add5 = make_adder(5)` evaluate
  as `callable_binding` roots during checking finalization
- generic top-level function-typed expressions that require computation publish
  `CallableEvalTemplate` and instantiate through
  `CallableBindingInstantiationKey`. A test must cover:

  ```roc
  id : a -> a
  id = |x| x

  choose : Bool, (a -> a), (a -> a) -> (a -> a)
  choose = |b, f, g| if b { f } else { g }

  also_id = choose(True, id, id)

  use_int = also_id(1)
  use_str = also_id("x")
  ```

  The expected checked artifact contains one `TopLevelProcedureBindingRef` for
  `also_id` whose body is `callable_eval_template`, plus two concrete
  `CallableBindingInstanceRef` values in the requesting instantiation store: one
  at `I64 -> I64` and one at `Str -> Str`. No generalized executable MIR,
  generalized interpreter value, runtime top-level closure object, runtime thunk,
  or callable-alias table is allowed.
- top-level callable aliases such as `also_inc = inc` also evaluate as
  `callable_binding` roots during checking finalization. The interpreter result
  reifies to a selected finite callable value whose callable set has the `inc`
  member selected and whose evaluated capture list is empty;
  publication may map `also_inc` directly to the same `procedure_binding` as
  `inc`, or to a sealed synthetic wrapper binding if distinct export identity is
  required. There is no separate callable-alias category,
  alias side table, syntax shortcut, runtime thunk, or runtime top-level
  closure object.
- `add5 = make_adder(5)` promotes to a closed procedure whose captured `5` is a
  private capture `ConstRef`, not a source top-level binding and not a runtime
  closure environment
- a promoted callable may capture a private record containing a callable leaf,
  for example `add1 = make_caller({ f: |x| x + 1 })` where `make_caller = |r|
  |x| (r.f)(x)`. The record must be represented as a private promoted-capture
  record node, the `f` field must recursively promote to a private synthetic
  procedure template, and the sealed `add1` body must read an explicit
  `procedure_value` from the private capture path. The record must not appear in
  `TopLevelValueTable`, must not be importable by source name, and must not
  become a runtime top-level closure object.
- a promoted callable may capture a private record containing an existing
  procedure value, for example `add1 = make_caller({ f: inc })`. The `f` field
  must be a finite callable leaf instance whose `proc_value.template` is
  `CallableProcedureTemplateRef.checked(template_ref_of_source_proc(inc))` and
  whose `proc_value.source_fn_ty` is the exact canonical function type of that
  field occurrence, for example `I64 -> I64`; it must not force a promoted
  wrapper merely because the procedure value appears inside a private capture
  graph.
- promoted callable tests must cover private tuples, tags, lists, boxes,
  transparent aliases, and nominals whose leaves include a mix of serializable
  values and callable values. Non-`Box(T)` containers must keep finite callable
  leaves finite. A private `Box({} -> I64)` capture may erase only the boxed
  callable payload named by an explicit `BoxBoundaryId`; a private `Box(I64)`
  capture must remain ordinary serializable data.
- promoted callable tests must cover a whole private capture aggregate being
  passed to another promoted procedure and returned from a promoted procedure.
  For example, a promoted wrapper that captures `{ f: I64 -> I64, n: I64 }`
  must be able to pass the whole record to a helper without publishing the
  record in `TopLevelValueTable` or allocating a runtime top-level closure
  object.
- promoted callable tests must cover private capture projections and whole
  aggregate materialization in the same body. The same private capture graph may
  be used to read `r.f`, read `r.n`, and pass `r` as a whole; all three uses
  must lower through explicit `PrivateCaptureRef` value handles.
- promoted callable tests must cover exported promoted callable roots imported
  by another module. The importing module must see a normal `procedure_binding`
  whose procedure value has a checked template in the exporting artifact; it must not
  import or inspect the exporting module's private capture graph by source name.
- recursive function-valued top-level bindings adapted from Cor's
  `test/capture-recursive-function.roc` promote by reserving every reachable
  promoted procedure before sealing any body
- top-level function-valued declarations are not reified as `ConstRef` templates;
  root function values publish as `procedure_binding`
- top-level function-valued declarations that evaluate to closed callable values
  are promoted before artifact publication to closed top-level procedures and
  then consumed as `procedure_binding`
- top-level function-valued declarations that evaluate to erased callable values
  with explicit `BoxBoundaryId` provenance are promoted to closed ordinary
  procedure identities whose bodies are emitted by executable MIR from sealed
  `ErasedPromotedWrapperBodyPlan` records. Mono may call or pass those procedure
  identities at their source function type, but mono must not lower their bodies
  and must not carry `ErasedFnSigKey`, executable payload transforms, hidden
  capture arguments, executable result type keys, or erased capture ABI data.
- top-level function-valued declarations that evaluate to erased callable values
  must include the direct promoted-procedure case:

  ```roc
  make_boxed : {} -> Box(I64 -> I64)
  make_boxed = |_| Box.box(|x| x + 1)

  add1 : I64 -> I64
  add1 = Box.unbox(make_boxed({}))

  main : I64
  main = add1(10)
  ```

  The expected checked artifact publishes `add1` as a normal
  `procedure_binding` whose promoted procedure has a sealed erased promoted
  procedure plan with `ErasedPromotedProcedureExecutableSignature`. The expected
  executable MIR contains the compiler-created procedure body that performs the
  known erased call. Mono, row-finalized mono, lifted MIR, and lambda-solved MIR
  must carry only the opaque procedure identity/source function type for `add1`;
  they must not lower the erased body, build a runtime thunk, allocate a runtime
  top-level closure object, publish a runtime erased callable object, or use a
  canonical executable key as a substitute for an explicit executable type
  payload.
- top-level function-valued declarations that evaluate to erased callable values
  must include the captured finite-callable adapter case:

  ```roc
  make_adder : I64 -> Box(I64 -> I64)
  make_adder = |n| Box.box(|x| x + n)

  add5 : I64 -> I64
  add5 = Box.unbox(make_adder(5))

  main : I64
  main = add5(10)
  ```

  The expected checked artifact publishes `add5` as a normal
  `procedure_binding` whose erased promoted wrapper body has an
  `ErasedCaptureExecutableMaterializationPlan`. The hidden capture materializes
  a finite callable-set value whose selected member is the lambda created by
  `|x| x + n`, and whose single capture is the executable-ready materialization
  of `5`. The sealed erased wrapper body must not contain `PrivateCaptureRef`,
  `CallableLeafInstance`, `CaptureSlotReificationPlanId`, `CallableResultPlanId`,
  or any runtime closure object. Executable MIR packs that finite callable-set
  value as the erased hidden capture, reserves the adapter using the full
  `ErasedAdapterKey`, and the adapter body dispatches with `callable_match`,
  even though the callable set is a singleton.
- erased compile-time callable values and erased callable leaves must store
  exact erased code refs. Direct erased code refs carry
  `ErasedDirectProcCodeRef { proc_value, capture_shape_key }`. Finite-set
  adapter refs embedded in erased callable leaves must lower through the exact
  `ErasedAdapterKey { source_fn_ty, callable_set_key, erased_fn_sig_key,
  capture_shape_key }`, and adapter bodies must dispatch with `callable_match`,
  including singleton sets. Tests must prove a bare code symbol, bare procedure
  value, finite callable leaf, source function type plus callable-set key
  without erased signature, source function type plus callable-set key without
  capture shape, erased signature alone, executable specialization key, or
  generated symbol text is insufficient and rejected by debug verifiers. Adapter
  deduplication tests must include two adapters with the same
  `source_fn_ty + callable_set_key` but different `erased_fn_sig_key`, and two
  adapters with the same `source_fn_ty + callable_set_key + erased_fn_sig_key`
  but different `capture_shape_key`.
- sealed erased promoted wrapper materialization must be executable-ready. Tests
  must corrupt an erased promoted wrapper by inserting a `PrivateCaptureRef`,
  `CallableLeafInstance`, `CaptureSlotReificationPlanId`, or
  `CallableResultPlanId` in its sealed capture and require a debug-only verifier
  panic. Tests must also corrupt the callable-set descriptor store by removing
  the selected member, changing the member's `ProcedureCallableRef`, changing
  the member's `CaptureShapeKey`, changing a capture slot's `exec_value_ty`, or
  changing the capture arity. Each corruption is a compiler invariant violation
  in debug builds and `unreachable` in release builds.
- promoted procedures have no runtime capture environment; serializable
  top-level captures are consumed through published `ConstRef` templates plus
  sealed concrete `ConstInstanceRef` values, local captures are consumed through
  private structural promoted-capture nodes, serializable leaves are private
  capture `ConstRef`/`ConstInstanceRef` data, and function-typed callable leaves
  are explicit finite `FiniteCallableLeafTemplate` records that instantiate to
  `FiniteCallableLeafInstance` records. Finite callable leaf instances store the
  sealed checked procedure template plus the exact canonical source function type
  for the occurrence; captured local callable leaves are recursively promoted to
  private procedure values before being stored as finite leaves. Erased boxed
  callable leaves are forbidden in private capture graphs; a captured
  non-function subtree containing erased boxed callable slots must be stored as a
  concrete private `ConstInstanceRef` leaf and materialized by executable MIR.
- promotion consumes `CallablePromotionPlan` and `CallableResultPlan` records and
  does not discover new root dependencies by inspecting runtime capture memory
  or source syntax after dependency ordering is complete
- serializable private capture leaves are serialized in `CompileTimeValueStore`
  with `ConstOwner.promoted_capture`; mixed private capture graphs are stored in
  the same value store and addressed by `PrivateCaptureRef`; neither form
  appears in `TopLevelValueTable` or can be imported or exported by source name
- every private capture finite `callable_leaf` template instantiates to
  `FiniteCallableLeafInstance { proc_value }`. `proc_value.template` points at a
  sealed `CallableProcedureTemplateRef`, and `proc_value.source_fn_ty` is the
  exact canonical function type of the occurrence. When the template names a
  promoted procedure, it must be `CallableProcedureTemplateRef.synthetic` and
  have a `PromotedProcedureTable` row whose `template` points at a sealed
  compiler-created procedure template; existing source/imported/hosted/platform
  procedures consume their already-published procedure metadata or capability
  records through `CallableProcedureTemplateRef.checked`. A test must use a
  generic procedure in two finite leaves at different concrete function types and
  prove the leaves differ by `source_fn_ty`, not by procedure template alone.
- private capture graphs must reject erased callable payloads at the source graph
  boundary. A debug-only verifier test must corrupt a source private capture node
  by inserting `ErasedCallableLeafInstance`, `ErasedFnSigKey`,
  `ErasedCallableCodeRef`, `ErasedCaptureExecutableMaterializationPlan`, or
  `CallableResultPlanId` and require a loud panic. A separate positive test must
  capture a non-function aggregate containing `Box(I64 -> I64)` and prove the
  source private capture graph stores a concrete private `ConstInstanceRef` leaf
  while executable MIR materializes the boxed erased callable before IR.
- debug-only artifact verifier tests corrupt a promoted `procedure_value` to
  remove its checked template, corrupt a private capture promoted
  `callable_leaf` to point at a symbol with no `PromotedProcedureTable` row,
  corrupt an existing-procedure `callable_leaf` to point at a missing checked
  template/capability, corrupt a finite callable leaf by removing or changing
  its `source_fn_ty`, and introduce a `PrivateCaptureRef` outside a promoted callable
  wrapper. Each verifier must panic loudly in debug builds; release builds must
  not retain deletion-scan or verifier metadata for these checks.
- public non-function top-level constants may contain callable leaves. For
  example `table = { f: |x| x + 1 }` publishes `table` as a `ConstRef` template
  whose record field `f` is a finite callable leaf template pointing at a sealed
  promoted procedure template. A concrete use instantiates that leaf with the
  exact canonical function type of the record field. There is no runtime
  top-level closure object, runtime thunk, runtime global initializer, or
  interpreter pointer.
- public non-function top-level constants may contain existing procedure values.
  For example:

  ```roc
  table : { f : I64 -> I64 }
  table = { f: inc }
  ```

  This publishes `table` as a `ConstRef` whose concrete `I64` instance has this
  `f` field:

  ```zig
  CallableLeafInstance.finite(.{
      .proc_value = .{
          .template = CallableProcedureTemplateRef.checked(template_ref_of_source_proc(inc)),
          .source_fn_ty = canonical("I64 -> I64"),
      },
  })
  ```

  when the field type is `I64 -> I64`; it must not create a runtime global
  callable object and must not promote a wrapper unless export/debug provenance
  explicitly requires a distinct procedure value. A polymorphic procedure value
  in a public constant must store one callable leaf template and instantiate it
  per concrete occurrence with that occurrence's exact canonical function type,
  for example `{ f: id }` at `{ f : I64 -> I64 }` instantiates
  `source_fn_ty = canonical("I64 -> I64")`, while `{ f: id }` at
  `{ f : Str -> Str }` instantiates
  `source_fn_ty = canonical("Str -> Str")`. The unannotated generic constant
  `table = { f: id }` remains one `ConstRef` template and is consumed through
  separate `ConstInstantiationRequest` values for those two requested record
  type payloads.
- public generic constants whose values require computation publish
  `ConstEvalTemplate`, not a pre-evaluated generic value graph. A test must cover:

  ```roc
  id : a -> a
  id = |x| x

  choose : Bool, (a -> a), (a -> a) -> (a -> a)
  choose = |b, f, g| if b { f } else { g }

  table = { f: choose(True, id, id) }

  use_int = (table.f)(1)
  use_str = (table.f)("x")
  ```

  The expected checked artifact contains one exported `ConstRef` for `table`, an
  eval template for that constant, and two concrete `ConstInstanceRef` values in
  the requesting `ConstInstantiationStore`: one at `{ f : I64 -> I64 }` and one
  at `{ f : Str -> Str }`. No runtime thunk, runtime global initializer,
  imported-module LIR re-execution, or syntax-derived callable recovery is
  allowed.
- public generic constants whose callable leaves come from inline lambdas or local
  functions must not store fake lifted callable templates. A test must cover:

  ```roc
  table = { f: |x| x }

  use_int = (table.f)(1)
  use_str = (table.f)("x")
  ```

  The expected checked artifact contains one exported `ConstRef` for `table`
  whose template is `ConstEvalTemplate` unless the compiler has already promoted
  the lambda to a sealed synthetic checked template independent of any future
  owner mono specialization. The `I64` and `Str` uses instantiate separate
  concrete constant instances. Each instance creates its callable leaf only after
  concrete mono specialization and lifting have run for that requested record
  type. Debug verification must reject any generalized `ConstValueGraphTemplate`
  that contains `CallableProcedureTemplateRef.lifted` with a missing or
  placeholder `owner_mono_specialization`.
- public callable-containing constants may capture serializable data. For
  example `make = |n| { f: |x| x + n }` and `table = make(41)` publishes
  `table` as a `ConstRef` template record, promotes `f` to a closed procedure
  template, and stores `41` as compiler-owned capture data for that promoted
  procedure.
- public callable-containing constants may nest callable leaves through
  records, tuples, tags, `List(T)`, `Box(T)`, transparent aliases, and
  nominals. Non-`Box(T)` containers must not introduce erased representation. A
  `Box(I64 -> I64)` constant graph may erase only the boxed callable payload
  named by an explicit `BoxBoundaryId`.
- public callable-containing constants may contain erased boxed callable leaves
  with non-empty captures. For example:

  ```roc
  make_adder : I64 -> Box(I64 -> I64)
  make_adder = |n| Box.box(|x| x + n)

  table : { f : Box(I64 -> I64) }
  table = { f: make_adder(41) }

  main : I64
  main = Box.unbox(table.f)(1)
  ```

  Reifying `table` must consume the interpreter result for `make_adder(41)`
  while the hidden erased capture is still physically available. The stored
  callable leaf in `CompileTimeValueStore` must be an
  `ErasedCallableLeafInstance` whose capture is a complete
  `ErasedCaptureExecutableMaterializationPlan` for the captured `41`. It must
  not store an `ErasedCaptureReificationPlan`, `CaptureSlotReificationPlanId`,
  `CallableResultPlanId`, runtime closure pointer, runtime packed-function
  bytes, or a request to re-run compile-time evaluation later. Importing modules
  must materialize the boxed callable from the exported checked artifact's
  zero-copy views and must not deserialize a separate runtime constant payload,
  execute the exporting module's LIR, or request any source/module reanalysis.
- constant materialization tests must prove `ConstMaterializationPlan` is a
  recursive target-specific graph, not flat storage. A nested constant such as
  `{ xs: [{ f: inc }], boxed: Box.box(|x| x + 1) }` must materialize records,
  lists, callable leaves, and the boxed erased payload from explicit
  materialization nodes. The finite `inc` leaf must remain finite outside the
  `Box(T)` boundary, the boxed lambda may erase only through its recorded
  `BoxBoundaryId`, and no runtime top-level thunk, runtime global callable
  object, or interpreter pointer may appear.
- imported callable-containing constants are consumed through the exporting
  artifact's `ConstRef`, imported const-template view, `ConstInstantiationKey`,
  `CompileTimeValueStore`, and the importing artifact's
  `ConstInstantiationStore`; the importing module must not re-run the exporting
  module's interpreter or inspect source declarations to recover callable leaves.
- imported generic callable eval bindings are consumed through
  `ImportedProcedureBindingView` and `CallableBindingInstantiationKey`. A test
  must put the `also_id = choose(True, id, id)` example in one module, import it
  from another module, and use it at both `I64 -> I64` and `Str -> Str`. The
  importing artifact must own the two concrete callable binding instances it
  requests, and it must not inspect the exporting module's source, mutate the
  exporting artifact, or rerun exported compile-time roots after the exporting
  artifact has been published.
- debug-only artifact verifier tests must reject generalized executable MIR
  inputs that came from a `CallableEvalTemplate` without a concrete
  `CallableBindingInstantiationKey`; imported procedure binding views whose
  template closure omits a required checked body, checked type root, checked type
  scheme, checked callable body, checked constant body, checked procedure
  template, callable eval template, const template, static dispatch plan,
  resolved value-reference table, method-registry entry, or interface capability;
  generic value graphs whose callable leaf stores a lifted
  template with no real owner mono specialization; concrete
  `CallableBindingInstance` rows missing their concrete dependency summary,
  executable root, callable result plan, promotion plan when promotion occurred,
  promotion output, or final procedure value; concrete `ConstInstance` rows
  missing their concrete dependency summary, reification plan, or generated
  procedure list; and any `ConstInstance` or `CallableBindingInstance` that
  lists a generated procedure absent from `SemanticInstantiationProcedureTable`.
  These verifier failures panic loudly in debug builds; release builds use
  `unreachable` for equivalent compiler-invariant paths and must not retain
  verifier metadata.
- debug-only imported-closure verifier tests must reject an imported template
  closure that uses exporter-local private ids without artifact qualification or
  explicit remapping into the importer-owned closure namespace. Separate corrupt
  fixtures must omit a reachable checked body, checked type root, checked type
  scheme, private capture node, private capture constant template,
  semantic-instantiation procedure row, callable result plan, callable promotion
  plan, constant reification plan, dependency summary template, or checked
  procedure template needed by an imported `ConstEvalTemplate`,
  `CallableEvalTemplate`, or exported generic procedure template. The verifier
  must panic at import/checking finalization time, before executable MIR can see
  the template.
- concrete instantiation ownership tests must put `table = { f: |x| x }` in one
  module, import it in another module, and use it at both `{ f : I64 -> I64 }`
  and `{ f : Str -> Str }`. The importer must own two sealed concrete
  `ConstInstance` rows plus any instantiation-generated synthetic procedure
  templates in its own `SemanticInstantiationProcedureTable`; the exporter must
  remain immutable after publication, and post-check lowering must not create
  semantic instances.
- callable binding ownership tests must put `also_id = choose(True, id, id)` in
  one module, import it in another module, and call it at both `I64 -> I64` and
  `Str -> Str`. The importer must own two sealed
  `CallableBindingInstance` rows with concrete dependency summaries, executable
  roots, result plans, promotion outputs, and final procedure values. Removing
  any of those rows after publication must be a debug-only verifier failure, not
  a trigger for post-check recovery.
- semantic-instantiation procedure table tests must cover generated procedure
  templates from public callable-containing constants, generated procedure
  templates from callable binding promotion, and generated procedure templates
  private to promoted-capture graphs. Each generated procedure must have an
  artifact-qualified `SemanticInstantiationProcedureRef`, a sealed checked
  template in the same artifact, and any required `PromotedProcedureTable` row
  before the artifact is published.
- `TopLevelValueTable` is the only post-check source for deciding whether a
  top-level binding is a `ConstRef`-backed compile-time constant or
  `procedure_binding`
- `TopLevelValueTable` is seeded before root evaluation with direct top-level
  functions and already-procedure top-level lambdas as `procedure_binding`, and
  with reserved `ConstRef` entries for non-function constants
- no published `TopLevelValueTable` entry is `pending_callable_root`
- non-function top-level constants whose source type contains callable slots are
  valid compile-time constants and must be represented as explicit `ConstRef`
  template graphs with callable leaf templates and concrete instantiations
- user-written compile-time crashes and failed `expect` statements are reported
  before checked artifacts are published
- division by zero and numeric conversion failures during compile-time constant
  evaluation are reported before checked artifacts are published
- cross-module constants are consumed from the imported module's serialized
  compile-time value store and imported const-template view
- cross-module constants are referenced by `ConstRef` plus
  `ConstInstantiationKey` and materialize through the consuming
  `ConstInstantiationStore`, not by generated zero-argument procedures
- a cached checked artifact restores compile-time values, sealed concrete
  constant instances, sealed concrete callable binding instances, and
  semantic-instantiation procedure table rows with the rest of the checked
  artifact
- a cached checked artifact restores promoted procedure rows, promoted checked
  templates, serializable private capture leaves, and private capture graph
  nodes with the rest of the checked artifact; it must not accept a cache entry
  where a promoted `procedure_binding` survives but its template, procedure
  value, or private capture graph is missing
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
  static dispatch plans, checked type stores, checked body stores, checked
  procedure template tables, compile-time value stores, constant instantiation
  stores, callable binding instantiation stores, and semantic-instantiation
  procedure tables
- checked artifact string literals are artifact-owned checked bytes addressed by
  `CheckedStringLiteralId`; no checked artifact exports raw
  `base.StringLiteral.Idx` as lowering payload
- published checked artifacts are consumed through read-only views and are not
  patched after cache load or publication
- no post-check stage mutates checked CIR or `ModuleEnv` to assign hosted
  indices, platform-required lookup targets, roots, or module identity
- root requests are built before MIR lowering and no later stage selects roots
  by scanning exports, declarations, expression syntax, hosted lambda nodes, or
  procedure order
- generic public exports remain checked procedure templates until a concrete
  consumer requests a concrete mono function type; exporting a generic procedure
  never lowers its body eagerly
- REPL and development expressions become temporary checked artifacts with
  explicit roots
- imported modules expose representation capabilities, compile-time constants,
  exported checked procedure templates, and exported callable binding templates
  only through their checked artifacts, checked type/body store views, imported
  procedure-binding views, imported const-template views, artifact-qualified
  imported template closures, sealed const instantiation stores, sealed callable
  binding instantiation stores, and sealed semantic-instantiation procedure
  tables
- imported checked template literals lower from imported checked artifact string
  bytes into the lowered program literal pool; no post-check imported template
  lowering path reads exporter or importer `ModuleEnv` string stores
- public post-check semantic lowering APIs return resource errors only; semantic
  missing-data conditions are checking diagnostics before publication or
  compiler invariant violations after publication
- compile-time constant evaluation runs during checking finalization through
  `checked CIR -> mono MIR -> row-finalized mono MIR -> lifted MIR ->
  lambda-solved MIR -> executable MIR -> IR -> LIR -> LIR interpreter ->
  compile-time value store -> constant instantiation store -> callable binding
  instantiation store -> semantic-instantiation procedure table`
- checked artifact data is not published until compile-time constant evaluation
  has either produced a complete compile-time value store or appended all
  user-facing checking problems
- compile-time roots are evaluated through an explicit dependency graph; direct
  top-level functions are procedure values, not compile-time evaluation roots
- dependent compile-time roots lower only after local dependencies have
  filled `ConstRef` templates or promoted `procedure_binding` entries into
  `TopLevelValueTable`, and every runnable constant use carries a concrete
  `ConstInstantiationKey`
- generic `ConstEvalTemplate` and `CallableEvalTemplate` records contain
  dependency summary templates; concrete `ConstInstance` and
  `CallableBindingInstance` rows contain concrete dependency summaries resolved
  in the owning artifact's concrete mono context
- no post-check lowering stage creates or finishes a `ConstInstance`,
  `CallableBindingInstance`, `SemanticInstantiationProcedureTable` row,
  promoted procedure row, private capture graph, callable result plan, callable
  promotion plan, constant reification plan, or dependency summary template
- no runtime top-level constant thunks, runtime global initializer procedures
  for constants, runtime zero-argument constant wrappers, runtime top-level
  closure objects, or runtime global callable-value objects exist for top-level
  bindings
- private `comptime_only` LIR roots are visible only to compile-time evaluation
  and never reach backend input
- imported constants are consumed through `ConstRef` plus
  `ConstInstantiationKey` from serialized compile-time value stores and
  imported const-template views, not by re-running imported module LIR roots
  after checking
- target-specific constant materialization uses explicit layout,
  reference-counting, and storage plans outside the checked artifact cache
- no public pipeline imports old top-level post-check modules
- `roc run` and interpreter-shim IPC execution split at a viewable
  ARC-inserted LIR runtime image allocated/published through the existing
  shared-memory infrastructure: the parent lowers through the checked-artifact
  public pipeline and publishes `LirRuntimeImage`; the child maps shared memory,
  constructs zero-copy LIR views, and interprets LIR only
- `roc build --opt=interpreter` keeps the same semantic split at ARC-inserted
  LIR and must not reintroduce child-side semantic lowering; any embedded
  runtime-image format must preserve view-oriented execution and must not
  dictate the IPC handoff used by `roc run`
- interpreter-shim transports contain no live or cached `ModuleEnv`, no CIR, no
  checked artifact, no MIR, no IR, no checker vars, no `CIR.Def.Idx` entrypoint
  tables, and no post-check lookup requests
- interpreter-shim child code contains no semantic lowering, root selection,
  static-dispatch resolution, platform-required lookup, or compile-time
  evaluation path
- static dispatch exists only in checked CIR and mono MIR input pattern matching
- every checked static-dispatch node exported to mono MIR carries a
  `StaticDispatchCallPlan`, or has already been rewritten to `structural_eq`
- every checked `StaticDispatchCallPlan` is determinate for each concrete mono
  specialization that can reach it; checking rejects ambiguous dispatcher
  variables before artifact publication
- mono MIR is produced only through the `MonoSpecializationQueue`; no
  `lowerAllTopLevelFunctions`, lower-every-export, or equivalent eager lowering
  path remains
- mono MIR creates the program literal pool and stores lowered source literals
  only as `ProgramLiteralId`
- mono MIR consumes checked `StaticDispatchCallPlan` values plus the checked
  method registry and never chooses the dispatcher variable from receiver
  expressions, result positions, method names, or environment lookup
- mono MIR has one normalized static-dispatch lowering path after consuming
  `StaticDispatchCallPlan`; it has no separate ordinary-dispatch,
  type-dispatch, or equality target model
- mono MIR output has no dispatch nodes
- mono MIR output uses `call_proc`, not `call_direct`, for resolved static
  dispatch
- mono MIR output uses `call_proc`, not `call(var_(proc), args)`, for every
  resolved direct procedure call, resolved static dispatch call, and resolved
  custom equality call
- row-finalized mono MIR, lifted MIR, lambda-solved MIR, executable MIR, and IR
  forward the program literal pool and contain no raw `base.StringLiteral.Idx` or
  bare `CheckedStringLiteralId`
- every static-dispatch-produced `call_proc.requested_fn_ty` is the mono-store
  unification of `StaticDispatchCallPlan.callable_ty` and the instantiated
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
- lifted MIR capture discovery uses resolved symbols and mutable-version
  records, not display-name comparison
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
- row-finalized construction nodes preserve source evaluation order separately
  from finalized logical assembly order before later stages need positional
  construction
- representation merge consumes finalized row IDs and never sorts names, scans
  rows, or relies on physical layout order to compute logical indexes
- tag construction representation edges use the full checked tag-union type,
  never a singleton constructor type reconstructed from syntax
- lambda-solved MIR computes boxed payload representation plans and
  callable/capture keys only from specialization-local clone-instantiated and
  fully resolved type stores
- lambda-solved MIR stores generalized representation templates separately from
  executable representation instances
- lambda-solved MIR reserves procedure/callable/capture/adapter nodes for a
  recursive specialization SCC before publishing executable keys
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
- executable MIR lowers `call_proc` through `CallProcExecutablePlan` before
  emitting `call_direct`
- executable MIR lowers every finite non-erased callable-set call to explicit
  `callable_match`
- executable MIR lowers finite callable-set value construction only from
  `CallableSetConstructionPlan` records attached to the exact value occurrence
- executable MIR verifies that every finite callable-set construction plan's
  owning `CallableValueInfo`, finite emission plan, selected descriptor member,
  capture schema, and canonical `source_fn_ty` agree before emitting
  `callable_set_value`
- callable-set construction evaluates capture operands exactly once, stores them
  in a member payload in `CaptureSlot.index` order, and later calls consume that
  payload instead of rebuilding captures
- executable MIR verifies that every `callable_match` branch uses the same
  canonical requested source function type as the call site, the descriptor
  member, and the reserved `ExecutableSpecializationKey`
- executable MIR synthesizes erased adapters for finite callable-set values
  crossing erased `Box(T)` boundaries
- executable MIR records exact branch `direct_args` as `ExecutableValueRef`
  handles
- executable MIR gives every `callable_match` one result type and one result
  temp, and every returning branch bridges into that temp when needed
- executable MIR direct, erased, and callable-set calls preserve fixed Roc arity
- executable MIR ordinary source `match` and callable-set `callable_match` are
  structurally distinguishable
- executable MIR ordinary source `match` evaluates scrutinees once, consumes an
  explicit `PatternDecisionPlan`, materializes `PatternPathValuePlan` records
  once per selected path at the control-flow points where they are needed before
  projecting binders by finalized path ids, and joins returning branches into one
  shared result temp
- executable MIR keeps callable-set values distinct from packed erased function
  values
- packed erased function values carry an `ErasedFnSigKey` that distinguishes no
  capture from typed zero-sized captures and defines hidden capture-argument ABI
- packed erased function values carry typed capture metadata that distinguishes
  no capture, zero-sized typed capture, and boxed runtime capture payload
- callable-set member ordering uses `ProcOrderKey`, not `Symbol.raw()`
- callable-set member identity resolves through the artifact-owned
  `CallableSetDescriptorStore`; sealed wrapper captures and materialized finite
  callable-set values carry only `source_fn_ty`, `callable_set_key`,
  `selected_member`, and executable-ready captures, and must not duplicate member
  procedure refs, capture shapes, capture-slot schemas, executable proc ids, or
  layout data
- executable specialization keys use semantic base/type/representation keys,
  not `ProcOrderKey`, raw type ids, expression ids, or side-table ids
- executable MIR consumes `BoxPayloadRepresentationPlan` instead of making
  semantic erased-shape compatibility decisions
- executable MIR consumes erased-call ABI shapes from `ErasedFnSigKey` through
  explicit `ErasedFnAbiKey` values instead of recovering ABI behavior from
  runtime function pointers or capture layouts
- erased callable merge requires exact `ErasedFnSigKey` equality, including
  hidden capture type and `ErasedFnAbiKey`; hidden-capture or ABI-shape
  mismatches require explicit adapters or bridges
- executable MIR has no semantic parameter-mode solver and produces no
  procedure parameter-mode contracts before IR lowering
- executable MIR preserves explicit values, call ABI shapes, low-level
  RC-effect records, and runtime-uniqueness mutation sites for LIR
- executable MIR bridges consume only `ExecutableValueRef` handles; every bridged
  operand with evaluation, allocation, reference-counting, or control-flow
  significance is evaluated exactly once before bridging
- `Box.unbox` is ordinary value materialization only; consuming move-out requires
  a separate explicit operation
- logical field indexes are resolved to physical offsets only through the layout
  store
- recursive physical layout indirection is committed once by SCC over logical
  layout slot edges, and all constructors/accessors/RC plans consume the same
  recursive-slot mapping
- recursive physical layout indirection is not source `Box(T)` and not erased
  callable representation
- no exact callable alias side tables remain
- requested function type verifiers are debug-only compiler assertions and do not
  add runtime checks to user programs
- no source/executable side tables remain
- no expression-based method owner resolver remains
- no syntax-derived source type reconstruction remains
- checked static dispatch exports only normalized `StaticDispatchCallPlan`
  values; mono MIR consumes those plans and the checked method registry
- IR lowering consumes executable MIR only
- IR owns or forwards the program literal pool from executable MIR and stores
  literal/crash/string-pattern payloads only as `ProgramLiteralId`
- IR-to-LIR lowering interns all `ProgramLiteralId` payloads into `LirStore`, and
  LIR/backends/interpreter read literal bytes only through `LirStore`
- IR lowering preserves explicit values, ABI shapes, RC-effect records, and
  runtime-uniqueness mutation sites for LIR
- LIR ARC insertion computes explicit RC statements from LIR values and control
  flow; it does not infer procedure contracts as semantic truth
- LIR `RcInsert` is the only non-builtin stage that emits explicit `incref`,
  `decref`, and `free`
- LIR/backends do not know about source methods
- backends do not perform ordinary reference-counting analysis and only execute
  explicit LIR RC statements
- semantic audits forbid the deleted families
- guarded eval and glue gates pass
