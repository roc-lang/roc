# Plan: Remove Late Recovery From Static Dispatch, Mono, Nominals, And Structural Children

This plan documents the long-term design for four places where the implementation
still has enough information and API surface to rediscover facts in later stages.
The target state is that each fact is published once by the phase that naturally
owns it, then consumed by id/reference only. No post-check stage may choose an
owner, declaration, backing, record child, tag payload, or type relationship by
scanning registries, names, argument positions, source shapes, runtime bytes, or
backend state. The one exception for labels is the explicit producer that
canonicalizes symbolic row data into row ids and publishes the resulting child
maps; later consumers use those maps only.

## 1. Static-Dispatch Owner Resolution

### Current Concern

Checked artifact publication currently can attach a `resolved_owner` to
static-dispatch constraint rows by scanning visible method registries, collecting
owners that publish the first method, and filtering those owners by the rest of
the constraint group.

That is the wrong ownership boundary. A constraint row should say only that a
type variable is constrained by a method and callable type. It should not also
record a runtime method owner selected from visible target availability.

### Target Design

`CheckedStaticDispatchConstraint` contains only checked semantic constraint
data:

```zig
const CheckedStaticDispatchConstraint = struct {
    fn_name: MethodNameId,
    fn_ty: CheckedTypeId,
    origin: StaticDispatchConstraintOrigin,
    binop_negated: bool,
    num_literal: ?NumericLiteralConstraint,
};
```

There is no `resolved_owner` field.

Every actual runtime dispatch need is represented by an explicit obligation:

- ordinary source dispatch: `StaticDispatchCallPlan`
- iterator `for` lowering: `IteratorDispatchObligation`
- value-level method symbols: checked metadata that names the resolved
  `MethodOwner`, `MethodTarget`, method name, and checked callable type

Mono graph finalization, not checked constraint publication and not body
emission, resolves dispatch obligations. Resolution order is fixed:

1. Clone-instantiate the dispatch callable type and dispatcher type into the
   specialization-local source type graph.
2. Connect normalized argument endpoints and result endpoint.
3. Fully resolve the dispatcher source type.
4. Derive `MethodOwner` from that concrete source type identity only.
5. Look up `(MethodOwner, MethodNameId)` in the checked method registry.
6. Connect the target callable type to the dispatch callable endpoint.
7. Publish a finalized dispatch record for body emission.

The method registry is only a keyed lookup table. It never decides which owner a
call means.

```zig
const FinalizedStaticDispatch = struct {
    obligation: StaticDispatchObligationRef,
    dispatcher_ref: ConcreteSourceTypeRef,
    method: MethodNameId,
    callable_ref: ConcreteSourceTypeRef,
    arg_refs: Span(ConcreteSourceTypeRef),
    result_ref: ConcreteSourceTypeRef,
    resolution: StaticDispatchResolution,
};

const StaticDispatchResolution = union(enum) {
    method_target: struct {
        owner: MethodOwner,
        target: MethodTarget,
    },
    structural_equality,
};

const StaticDispatchObligationRef = union(enum) {
    source_call: StaticDispatchPlanId,
    iterator_for: IteratorDispatchKey,
};

const IteratorDispatchKey = struct {
    plan: IteratorForPlanId,
    obligation: enum { iter, next },
};

const FinalizedIteratorDispatch = struct {
    key: IteratorDispatchKey,
    dispatch: FinalizedStaticDispatch,
};
```

Ownerless structural equality is allowed only when the checked dispatch plan
published `result_mode.equality.structural_allowed`. All value dispatch and
non-structural equality must resolve to a concrete owner and registry target
during mono graph finalization.

### Implementation Work

- Delete owner-candidate scanning during checked artifact publication.
- Delete `resolved_owner` from checked static-dispatch constraint serialization,
  remapping, verification, and canonical-name remapping.
- Add finalized dispatch tables to `FinalizedMonoSpecializationGraph`.
- Move static-dispatch owner lookup and target callable connection into mono
  graph finalization.
- Make mono body emission consume finalized dispatch ids only.
- Add debug-only invariant checks that no owner-bearing constraint rows exist
  and no body emitter path can resolve an owner.

### Done State

- Searching the code for `resolved_owner` finds nothing.
- Searching for owner candidate intersection finds nothing.
- Method registry APIs expose exact lookup by `(owner, method)` and no owner
  discovery API.
- Mono body emission has no method-owner resolution code.

## 2. Mono Solving Must Finish Before Body Emission

### Current Concern

Mono body lowering still has access to mutable type-instantiation and unification
state. It can connect type variables, collect demands, materialize source
payloads, choose static-dispatch targets, and ask type-only questions while
emitting MIR.

That leaves body emission responsible for both deciding types and emitting
values. The long-term design requires those responsibilities to be separate.

### Target Design

Mono has three real phase types:

```text
MonoSpecializationGraphBuilder
  -> FinalizedMonoSpecializationGraph
  -> MonoBodyEmitter
```

`MonoSpecializationGraphBuilder` owns mutable graph construction. It walks
checked bodies, checked plans, patterns, calls, dispatch obligations, callback
endpoints, expected-type slots, and block-local demand. It records graph edges
and obligations only. It emits no MIR values.

`FinalizedMonoSpecializationGraph` is the frozen result of solving that graph.
It contains every type fact, dispatch fact, nominal-backing instantiation,
callback endpoint, and demand result that body emission needs.

`MonoBodyEmitter` receives only a const finalized graph:

```zig
const MonoBodyEmitter = struct {
    graph: *const FinalizedMonoSpecializationGraph,
    ...
};
```

It cannot import or hold the mutable graph builder or type instantiator.

The finalized graph publishes lookup tables such as:

```zig
const FinalizedMonoSpecializationGraph = struct {
    body_instances: Map(MonoBodyInstanceId, MonoBodyInstance),
    root_body: MonoBodyInstanceId,
    exprs: Map(ScopedExpr, FinalizedExprInfo),
    patterns: Map(ScopedPattern, FinalizedPatternInfo),
    binders: Map(ScopedBinder, FinalizedBinderInfo),
    calls: Map(ScopedExpr, FinalizedCallInfo),
    local_proc_instances: Map(LocalProcInstanceKey, MonoBodyInstanceId),
    static_dispatches: Map(StaticDispatchPlanId, FinalizedStaticDispatch),
    iterator_dispatches: Map(IteratorDispatchKey, FinalizedStaticDispatch),
    nominal_backings: Map(NominalBackingUseId, InstantiatedNominalBacking),
    callback_returns: Map(CallbackReturnEndpointId, ConcreteSourceTypeRef),
};
```

Body emission may lower expressions, allocate locals, emit calls, emit matches,
emit loops, and request already-finalized mono specializations. It must not:

- unify types
- materialize source type payloads
- apply numeric defaults
- close rows or tag tails
- choose method owners
- instantiate target callable types
- scan declarations, bodies, names, method registries, or source syntax to fill
  missing type information

### Implementation Work

- Split current mono specialization into builder, finalizer, and emitter types.
- Move every `unify*`, `materialize*`, demand-propagation, type-only query, and
  dispatch-resolution operation out of body emission.
- Make the mutable type instantiator private to builder/finalizer modules.
- Change lowering helpers to accept finalized ids or `ConcreteSourceTypeRef`
  values published by the finalized graph.
- Add debug-only checks that the finalized graph has no pending links,
  unresolved numeric defaults, unresolved dispatch obligations, open row closures
  that require closure, or unchecked callback returns.

### Done State

- `MonoBodyEmitter` has no `TypeInstantiator` field.
- Body emission contains no calls to `unify*`, `materialize*`, or
  method-owner resolution helpers.
- Type-only result queries are builder/finalizer operations only.
- Mono MIR output is produced only after graph finalization succeeds.

## 3. Nominal Backing Selection Must Use Published Representation Authority

### Current Concern

Mono can rediscover generic nominal backings by scanning nominal declarations
and interface capability rows by remapped module/type names and instantiated
argument keys. This makes the consumer reconstruct which declaration or
capability owns a nominal backing.

### Target Design

Every checked nominal occurrence carries exact representation authority:

```zig
const CheckedNominalType = struct {
    nominal: CanonicalNominalId,
    args: Span(CheckedTypeId),
    backing: CheckedTypeId,
    representation: CheckedNominalRepresentationRef,
};

const ImportedNominalDeclarationRef = struct {
    artifact: CheckedModuleArtifactKey,
    declaration: CheckedNominalDeclarationId,
};

const LocalBoxPayloadCapabilityRef = struct {
    capability: BoxPayloadCapabilityId,
    opaque_atomic_proof: ?OpaqueAtomicProofId,
};

const ImportedBoxPayloadCapabilityRef = struct {
    artifact: CheckedModuleArtifactKey,
    capability: BoxPayloadCapabilityId,
    opaque_atomic_proof: ?OpaqueAtomicProofId,
};

const CheckedNominalRepresentation = union(enum) {
    builtin: BuiltinNominalIdentity,
    local_declaration: CheckedNominalDeclarationId,
    imported_declaration: ImportedNominalDeclarationRef,
    local_box_payload_capability: LocalBoxPayloadCapabilityRef,
    imported_box_payload_capability: ImportedBoxPayloadCapabilityRef,
    opaque_without_backing,
};
```

The representation ref is the authority. Mono finalization follows that ref:

- local declaration: instantiate that declaration's published formal roots and
  backing template
- imported declaration: instantiate through the imported template closure that
  authorized the declaration
- local or imported box-payload capability: use the exact capability row and
  exact optional opaque proof named by the representation ref
- builtin: use the builtin nominal declaration/representation
- opaque without backing: reject any attempt to inspect backing as an invariant
  violation

Nominal backing caches are allowed only for deduplication after the authority
has already been selected:

```zig
const NominalBackingInstantiationKey = struct {
    authority: CheckedNominalRepresentationRef,
    actuals: Span(ConcreteSourceTypeRef),
};
```

The cache key must not be used to find a declaration or capability by name.

### Implementation Work

- Assign stable artifact-local ids to nominal declarations, imported nominal
  templates, box-payload capabilities, and opaque proofs.
- Store the exact representation ref on every checked nominal payload.
- Preserve representation refs through import projection and canonical-name
  remapping.
- Replace name/key scans in mono with direct representation-ref dispatch.
- Keep canonical type keys as deduplication keys only, never as authority for
  selecting a nominal backing.

### Done State

- Mono has no helpers that scan nominal declarations or boxed-payload
  capabilities by module/type name.
- Missing representation authority is detected at checked artifact publication
  or by a debug-only invariant at the post-check boundary.
- Instantiating a nominal backing starts from a declaration/capability id, not
  from an occurrence-local nominal name.

## 4. Record/Tag Child Relationships Must Be Published By Row Identity

### Current Concern

Lambda-solved and executable-building code can recover structural children by
matching record field labels, tag labels, and tag payload logical indexes. Those
matches are usually exact, but they are still late recovery. The stage that
creates row ids should publish the child relationship once.

### Target Design

Row finalization owns the conversion from symbolic row data to row identities:

- record field label -> `RecordFieldId`
- tag label -> `TagId`
- tag payload position -> `TagPayloadId`

At the same time, row finalization publishes structural child maps for every
source type and representation root whose children later stages need:

```zig
const StructuralChildSourceMap = struct {
    parent: ConcreteSourceTypeRef,
    children: Span(StructuralChildSource),
};

const StructuralChildSource = struct {
    kind: RepresentationChildKind,
    child: ConcreteSourceTypeRef,
};

const RepresentationChildKind = union(enum) {
    function_arg: u32,
    function_return,
    function_callable,
    record_field: RecordFieldId,
    tuple_elem: u32,
    tag_payload: TagPayloadId,
    list_elem,
    box_payload,
    nominal_backing: NominalTypeKey,
};
```

Later phases consume `(parent, child kind) -> child` directly. They do not look
up a record field by label or a tag payload by label/index.

Const-backed values use the same rule. When a compile-time schema/value is
rehydrated into a lowering session, the materialization publishes a child map:

```zig
const ConstBackedChildMap = struct {
    record_fields: Map(RecordFieldId, ConstBackedValueInfo),
    tuple_elems: Map(u32, ConstBackedValueInfo),
    tag_payloads: Map(TagPayloadId, ConstBackedValueInfo),
    list_elems: Map(u32, ConstBackedValueInfo),
    box_payload: ?ConstBackedValueInfo,
    nominal_backing: ?ConstBackedValueInfo,
};
```

Projection lowering consumes that map. It does not rescan const schemas by
label or payload index.

### Implementation Work

- Add row-finalized structural child source maps.
- Add representation-store child maps keyed by `RepresentationChildKind`.
- Add const-backed child maps during compile-time value materialization import.
- Replace lambda-solved/executable helpers that recover child types by label or
  payload index with direct map lookups.
- Keep label/index matching only inside the producer that builds the row maps;
  no consumer gets a helper that can repeat the lookup.

### Done State

- Late-stage helpers named like `sourceRecordField`, `sourceTagPayload`,
  `recordFieldPayloadByLabel`, `tagVariantPayloadByLabel`, or
  `tagPayloadByLogicalIndex` are gone outside the row-map producer.
- Representation and executable payload builders use child ids and published
  child maps only.
- Const-backed projections use `ConstBackedChildMap`.

## Implementation Order

1. Static dispatch owner cleanup: remove owner publication from constraint rows
   and add finalized dispatch records.
2. Mono phase split: make graph finalization real and make body emission
   lookup-only.
3. Nominal representation authority: add exact declaration/capability refs to
   nominal occurrences and remove nominal backing scans.
4. Structural child maps: publish row-id child maps and remove late label/index
   recovery.

This order removes the broadest downstream recovery APIs first, then removes
the remaining places where consumers still recover exact identities from names
or shape.
