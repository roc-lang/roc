# Plan: Long-Term Fixes For Remaining Late Guessing And Recovery

This file records the investigation for each problem in `problems.md` and gives
the long-term recommendation. The goal is not to find the smallest local patch.
The goal is to make the compiler design keep facts in the stage that owns them
and pass those facts forward explicitly.

The standard for all recommendations:

- Source selection must use explicit source payload ids or explicit producer
  records.
- Canonical keys may validate identity and dedupe already-selected payloads.
- Runtime encodings must have one published conversion point.
- Layouts may be used for memory representation, not for source semantics.
- Backends and interpreters should reject impossible LIR with invariant errors.

## 1. Static Data Callable-Set Capture Uses Raw Member Index

### Current State

`src/check/canonical_names.zig` defines the callable-set runtime encoding:

- `callableSetRuntimeDiscriminantForMember(member)`
- `callableSetRuntimeIndexForMember(member)`
- `callableSetMemberForRuntimeDiscriminant(discriminant)`
- `callableSetMemberForRuntimeIndex(index)`

The comments there say that finite callable-set member ids are the runtime
discriminants used in callable-set layouts, value construction, callable matches,
and compile-time result decoding.

Most of the new callable-set lowering now uses this shared encoding. However,
`StaticDataBuilder.writeFiniteCallableSetCapture` in
`src/compile/static_data_exports.zig` still does this:

```zig
const member_index: usize = @intFromEnum(finite.selected_member);
...
const payload_layout_idx =
    tag_info.variants.get(@intCast(member_index)).payload_layout;
...
self.writeDiscriminant(..., @intCast(member_index));
```

It does verify that the selected member exists in the executable callable-set
payload, but it uses the raw enum value both as the layout variant index and the
runtime discriminant.

### Investigation

This path is materializing static bytes for erased capture data. It is not
merely inspecting a value; it commits runtime representation into emitted static
data. That makes it one of the places that most needs to follow the exact same
runtime encoding contract as normal codegen and finalization.

The current helper implementation happens to return the raw member id. So today
the direct `@intFromEnum` and the helper produce the same number. That is why the
bug is not necessarily observable right now.

The design problem is that this code bypasses the contract. If we later change
runtime order, add a compacted representation, reserve discriminants, sort
members by layout, or introduce an ABI-specific encoding, this static-data path
will keep compiling and silently use stale assumptions.

### Long-Term Ideal

Callable-set runtime encoding should have one owner. No caller should convert
member ids to variant indexes or discriminants by hand.

Static data materialization should:

1. Resolve the selected member semantically from the callable-set payload.
2. Convert that member id to a runtime variant index through
   `canonical.callableSetRuntimeIndexForMember`.
3. Convert that member id to a runtime discriminant through
   `canonical.callableSetRuntimeDiscriminantForMember`.
4. Validate that the runtime variant index is present in the tag-union layout.
5. Use the runtime variant's payload layout for capture tuple materialization.
6. Write the runtime discriminant returned by the helper.

### Recommendation

Replace the raw `@intFromEnum(finite.selected_member)` path with the shared
runtime helpers.

Concretely:

- In `writeFiniteCallableSetCapture`, compute:
  - `runtime_index = canonical.callableSetRuntimeIndexForMember(finite.selected_member)`
  - `runtime_discriminant = canonical.callableSetRuntimeDiscriminantForMember(finite.selected_member)`
- Use `runtime_index` for `tag_info.variants.get(...)`.
- Use `runtime_discriminant` for `writeDiscriminant`.
- Keep the semantic `findCallableSetMemberPayload` check.
- Add a debug verification helper near the callable-set layout construction that
  checks layout variant count and runtime order against the callable-set member
  table. Static data should consume that verified contract, not infer it.

Tests:

- Add a static-data eval test that materializes an erased callable with a
  non-empty finite callable-set capture in static data.
- The test should cover at least two callable-set members so the selected member
  is not only the zero case.
- If feasible, assert through the existing eval paths that interpreter, dev, and
  wasm agree on the selected callable behavior.

Priority:

High. This is a direct leftover from the callable-set runtime-encoding cleanup.

## 2. Nominal Backing Is Rediscovered By Name And Argument Keys

### Current State

`TypeInstantiator.materializePublishedNominalBacking` and
`TypeInstantiator.publishedNominalBackingRef` in
`src/mir/mono/specialize.zig` derive a nominal key and argument keys, then call
`publishedNominalBackingRootForRefs`.

That function searches:

1. The root artifact.
2. Imports.
3. Relation artifacts.

For each artifact, `publishedNominalBackingRootInArtifact` tries:

1. `instantiatePublishedNominalDeclarationBacking`, which finds a nominal
   declaration by nominal key and instantiates its backing.
2. `exported_nominal_representations`, matching nominal name and
   `capability.instantiated_args` against the requested argument keys.

`CheckedTypeProjector.publishedNominalBacking` in
`src/check/checked_artifact.zig` has a similar search. It first checks exported
nominal representations in the target artifact, then target nominal
declarations, then imported exported representations, then imported nominal
declarations.

There is already a better-shaped capability mechanism:

- `ModuleInterfaceCapabilities.fromModule` publishes
  `ExportedNominalRepresentation`.
- `BoxPayloadCapabilityEntry` records `source_ty`, `backing_ty`,
  `backing_ty_key`, `instantiated_args`, and opacity.
- `BoxPayloadPlanFinalizer.nominalCapability` resolves nominal capability by
  `source_ty`.

So the compiler has two models:

- A source-type-based publication model.
- A late search model based on nominal name plus argument keys.

### Investigation

The source-type-based model is closer to the long-term ideal. A nominal payload
is not just "a name plus some arguments." It is a checked source payload with a
particular backing payload. The module that checked the nominal declaration knows
that relation exactly.

The late search model works by rebuilding the relationship:

1. Recover the nominal key from the current artifact's name store.
2. Recover argument keys from concrete argument refs.
3. Search artifacts for a matching nominal declaration or exported nominal
   representation.
4. Instantiate or materialize the backing.

That is exactly the kind of search we are trying to avoid. It is better than
guessing from layout because it uses canonical keys and verifies arity, but it is
still a consumer recreating a producer-owned relationship.

The cache in `Program.nominal_backing_instantiations` is not the core problem.
Caching instantiated backing roots by artifact, nominal, and arg keys is a
reasonable implementation detail once the backing source has been selected. The
problem is selecting the source by searching nominal/arg keys instead of
consuming an explicit nominal representation reference.

### Long-Term Ideal

Nominal backing should be resolved from the nominal source payload, not by
artifact-wide search.

Every checked nominal payload that can cross a compile-time, mono, lambda, or
executable boundary should be able to answer:

- What is the source type key for this nominal instantiation?
- If the nominal is not builtin, what published nominal representation owns it?
- What exact backing checked payload ref should a consumer use?
- If the backing must be instantiated from a declaration, what declaration ref
  and exact argument payload refs are used?

The consumer should not know how to search artifacts for nominal declarations or
exported nominal representations. It should ask the publication boundary for the
representation attached to this nominal source payload.

### Recommendation

Consolidate nominal backing around source payload publication.

Concrete design:

1. Add an explicit nominal representation ref type. It should identify:
   - artifact key
   - source nominal checked payload id, or source type key plus payload id
   - backing checked payload id
   - backing type key
   - instantiated argument payload ids
   - instantiated argument keys for validation
   - whether the nominal is opaque

2. Extend `ExportedNominalRepresentation` or the capability it points to so the
   source payload id is carried, not just `source_ty`.

3. Build a per-artifact map from nominal source payload identity to nominal
   representation. The lookup key should be the exact source payload id for
   same-artifact data, or an artifact-qualified payload ref for imported data.
   Canonical keys can be stored alongside it for validation.

4. Replace `publishedNominalBackingRootForRefs` with a function whose input is
   the nominal source ref. The function should:
   - resolve the nominal representation attached to that source ref
   - verify the source key still matches
   - project or instantiate the published backing payload
   - return the exact backing source ref/root

5. Keep declaration instantiation, but move it behind the same publication API.
   A declaration can be the producer of a representation for a local nominal
   payload. Mono should not scan declarations by name.

6. After the new API is in place, remove:
   - artifact scans by nominal key from mono
   - `CheckedTypeProjector.publishedNominalBacking` searches by nominal name
   - duplicated nominal argument key matching in consumers

Tests:

- A nominal from an imported module with the same structural backing as another
  nominal should resolve through its own published representation, not whichever
  key search finds first.
- An opaque nominal should preserve the same representation path as a transparent
  nominal, with opacity checked as metadata rather than inferred from where the
  backing was found.
- Recursive or mutually recursive nominal backing instantiation should continue
  to use a reservation cache, but the cache should be keyed after the
  representation has been selected.

Priority:

High. This is the largest remaining design duplication in this audit.

## 3. Record And Tag Source Children Are Recovered By Label Text

### Current State

There are two main source-child lookup implementations:

- `BodySolver.sourceRecordField` and `BodySolver.sourceTagPayload` in
  `src/mir/lambda_solved/solve.zig`
- `SessionExecutableTypePayloadBuilder.sourceRecordField` and
  `SessionExecutableTypePayloadBuilder.sourceTagPayload` in
  `src/mir/lambda_solved/representation.zig`

Both walk checked source payloads and match labels like this:

```zig
source_names.recordFieldLabelText(source_field)
self.canonical_names.recordFieldLabelText(logical_field)
```

and similarly for tags.

There are related const-backed helper functions in
`src/mir/lambda_solved/solve.zig`:

- `constRecordFieldMatches`
- `constTagMatches`

There is already a projection mechanism in `CheckedTypeProjector` that remaps
record field labels and tag labels into the target name store when it projects
checked type payloads. There is also `ArtifactNamePublisher`, which interns
lowering-run names into checked artifact names.

### Investigation

The current text matching is not arbitrary string guessing. It exists because
different artifacts and lowering runs have different canonical-name stores.
Record field label id `5` in one store is not necessarily label id `5` in
another.

However, comparing text every time a later stage needs a child source payload is
still the wrong boundary. The later stage wants a payload relationship, not a
name-store reconciliation operation.

The current code repeats this work in several places:

- record fields
- tag payloads
- erased boundary type-key hashing
- erased boundary endpoint construction
- const-backed record/tag value handling

Each repetition is a chance to forget row extension behavior, mishandle imported
name stores, or treat "same text" as enough information after the precise source
payload relationship has been lost.

### Long-Term Ideal

Cross-name-store label reconciliation should happen exactly once at the boundary
where the payload crosses stores.

After that boundary, later stages should use explicit child selectors that are
valid in the current store, or explicit child source refs.

The model should be:

- A record field child is selected by a remapped canonical field id plus the
  exact checked child payload id.
- A tag payload child is selected by a remapped canonical tag id, payload index,
  and exact checked child payload id.
- Row extension traversal should be part of a checked-type child projection API,
  not duplicated in consumers.
- Missing children should be invariant errors when the consumer had a source
  payload and the logical type says the child exists.

### Recommendation

Introduce a checked-source child projection API and make both lambda solving and
executable payload building use it.

Concrete design:

1. Add a helper owned by the checked type projection/source module, not by lambda
   solving:

   - `checkedRecordFieldChild(source_ref, target_field_label) -> source_ref`
   - `checkedTagPayloadChild(source_ref, target_tag_label, payload_index) -> source_ref`
   - plus tuple/list/box/function/nominal child helpers for consistency

2. The helper should accept enough name-store context to remap once:
   - source artifact/name store
   - target name store
   - source checked payload ref
   - target logical child selector

3. Intern or remap target labels into the source boundary before lookup, or build
   a small correspondence table when projecting a source payload:
   - target field label id -> source checked child id
   - target tag label id and payload index -> source checked child id

4. Store child source refs in representation roots or source-root publication
   records where consumers repeatedly need them. This avoids repeated traversal
   and makes the graph explicit.

5. Remove local text comparison helpers from:
   - `BodySolver`
   - `SessionExecutableTypePayloadBuilder`
   - const-backed record/tag materialization paths

6. Keep text equality only in boundary code that imports or projects names into
   a shared store. That boundary should return ids/refs, not booleans.

Tests:

- Import a module with records and tags, then use those fields/tags through
  lambda-solved and erased-boundary paths. The test should force source and
  target name stores to be different.
- Exercise row-extension traversal so the selected field/tag is not in the
  first payload node.
- Include a const-backed record/tag value so the const materialization path uses
  the same child projection API.

Priority:

High. This is a recurring boundary leak and should be consolidated before more
erased-boundary cases are added.

## 4. Imported Checked-Type Projection Still Selects Source Roots By Key

### Current State

`ConcreteSourceType.Store` is already designed around explicit payload refs. Its
file comment says canonical type keys are identity keys and later lowering should
retain explicit payload refs.

The store root has:

```zig
key: CanonicalTypeKey
source: artifact { artifact, ty } | local ty
```

So an artifact-backed concrete source type already knows the exact imported
checked payload id.

However, `publishConcreteConstProducerType` and
`publishConcreteConstDependencyType` in `src/mir/mono/specialize.zig` still call
`CheckedTypeProjector.projectImportedCheckedTypeForKey` for imported data.

`projectImportedCheckedTypeForKey` scans `imported.checked_types.roots` looking
for a matching key and then projects that root.

For producers, `ConcreteConstProducer` already contains:

- producer artifact
- producer payload id
- producer key

For concrete refs, `ConcreteSourceTypeRoot.source.artifact` already contains:

- artifact key
- checked payload id

### Investigation

There are two different uses of keys here:

1. Target dedupe:
   - If the target checked type store already has a root for the same key, reuse
     it.
   - This is fine. The source payload has already been selected, and the key is
     being used as an interning key.

2. Source selection:
   - Search an imported artifact for a root with this key.
   - This is the problem. It discards exact source identity and tries to find it
     again.

The current code is unlikely to pick the wrong source often because checked
type roots are usually canonical-key unique. But the whole point of the recent
payload-publication work was to stop relying on that property for source
selection.

### Long-Term Ideal

All imported projection should start from an artifact-qualified checked payload
ref.

The projector should expose public APIs that make this distinction obvious:

- `projectImportedCheckedType(imported, ty)` for exact source projection.
- `projectCheckedTypeViewRoot(source_view, ty)` for exact local/view projection.
- `rootForKey` only inside projection internals for target dedupe.

There should not be a public `projectImportedCheckedTypeForKey` used by lowering
or finalization paths.

### Recommendation

Remove key-based source selection from compile-time dependency publication.

Concrete changes:

1. Make exact imported projection public if necessary:

   ```zig
   pub fn projectImportedCheckedType(
       self: *CheckedTypeProjector,
       imported: ImportedModuleView,
       ty: CheckedTypeId,
   ) Allocator.Error!CheckedTypeId
   ```

2. Change `publishConcreteConstProducerType`:

   - For same-artifact producer, keep projecting `producer.payload`.
   - For imported producer, find the imported artifact by artifact key, then call
     `projectImportedCheckedType(imported, producer.payload)`.
   - Verify the projected root key equals `producer.key`.

3. Change `publishConcreteConstDependencyType`:

   - For `.local`, keep projecting the local root with names.
   - For `.artifact`, use `artifact_ref.ty` directly after finding the artifact.
   - Verify the projected root key equals `root.key`.

4. Delete or deprecate `projectImportedCheckedTypeForKey`.

5. Keep `rootForKey` in:
   - target checked type store dedupe
   - synthetic root interning
   - projector active recursion handling

Tests:

- Add a compile-time dependency summary test where an imported concrete type is
  projected from a payload id and validated by key.
- Add a debug-only invariant test if the projected payload key does not match the
  expected key. That should fail loudly rather than search for another root.

Priority:

Medium-high. This is not as dangerous as nominal backing search because the
exact refs already exist and the local change is straightforward.

## 5. Wasm Numeric Low-Level Lowering Uses Return Layout For Some Signedness

### Current State

`src/backend/wasm/WasmCodeGen.zig` has one large helper,
`emitNumericLowLevel`, for scalar and composite numeric low-level operations.

It already uses operand layout for:

- comparisons
- `num_mod_by`
- `num_abs_diff`
- composite i128/Dec comparisons and `abs_diff`

But it still uses `ret_layout` for signedness in:

- `num_div_by`
- `num_div_trunc_by`
- `num_rem_by`

The dev backend now routes numeric/comparison operations through an explicit
`operand_layout` derived from `args[0]`, and the recent `abs_diff` fix validates
that both argument layouts match before codegen.

The interpreter also passes `arg_layout` into numeric operations.

### Investigation

For normal integer division, return layout and operand layout are expected to be
the same. That makes the current Wasm code probably correct for current valid
LIR.

But the invariant is accidental. The semantic question "signed or unsigned?" is
answered by the input operands, not by the output location. We already hit this
class of problem with `I8.abs_diff`, where the result layout is unsigned but the
comparison must use signed operand semantics.

The Wasm helper's `use_operand_layout` flag currently includes comparisons and
`abs_diff`, but not division or remainder. That flag is doing two jobs:

- deciding whether composite-ness should be detected from operands
- deciding which valtype/layout drives operation semantics

Those should be clearer.

### Long-Term Ideal

Every low-level numeric operation should receive an explicit numeric operation
context:

```zig
const NumericOpContext = struct {
    op: LowLevel,
    operand_layout: layout.Idx,
    ret_layout: layout.Idx,
    operand_val_type: ValType,
    ret_val_type: ValType,
};
```

The backend should:

- validate that required operands have matching layouts
- derive signedness from `operand_layout`
- derive result storage from `ret_layout`
- use `ret_layout` for memory/register result shape only
- reject impossible layout combinations in debug builds

### Recommendation

Refactor Wasm numeric lowering to separate operand semantics from result shape.

Concrete changes:

1. At the start of `emitNumericLowLevel`, compute:
   - `operand_layout = self.procLocalLayoutIdx(args[0])`
   - `operand_vt = self.procLocalValType(args[0])`
   - `ret_vt = self.resolveValType(ret_layout)`

2. For binary numeric operations, validate that `args[1]` has the expected
   operand layout. Shifts are the exception: lhs has numeric layout, rhs is the
   shift count layout.

3. Use `operand_layout` for signedness in:
   - division
   - truncating division
   - remainder
   - modulo
   - comparisons
   - `abs_diff`
   - shifts where signedness matters

4. Use `ret_layout` only for:
   - result valtype when the operation naturally returns a different type
   - memory storage/canonicalization
   - return-by-pointer or composite result shape

5. Rename or remove `use_operand_layout`; it should not be a semantic switch
   hidden inside the helper.

Tests:

- Mirror the existing dev/eval signedness tests through Wasm where possible.
- Add direct Wasm eval coverage for signed and unsigned division/remainder with
  operands whose values distinguish signed and unsigned behavior.
- Keep the `I8.abs_diff` test as a regression for "result layout differs from
  operand semantics."

Priority:

Medium. It may not currently be user-visible, but it is the same design smell as
the bug we just fixed.

## 6. LIR Interpreter Numeric Helpers Silently Default On Unsupported Layouts

### Current State

`src/eval/interpreter.zig` has several numeric helpers that switch on
`helper.sizeOf(arg_layout)`.

Some paths fail correctly:

- `numCmpOp` returns an invariant error for unsupported non-equality compare
  sizes.
- `valuesEqual` reports invariant errors for unsupported scalar and opaque
  pointer sizes.

Some paths silently default:

- `evalCompare` returns `1`, meaning equality, for unsupported sizes.
- `numShiftOp` does nothing for unsupported sizes and returns the allocated
  result.
- `numBinOp` ignores unsupported sizes both in division-by-zero checking and in
  the actual operation.
- `numWiden` does nothing for unsupported return sizes.

These defaults can return zeroed or equality-looking values for invalid LIR.

### Investigation

The interpreter is supposed to be a dumb executor of valid LIR. That does not
mean it should accept invalid LIR. A dumb interpreter is useful precisely when it
fails at the point where LIR violates its contract.

Silent defaults weaken that role:

- Invalid LIR can appear to pass in eval.
- A backend can fail or panic later on the same program.
- Debugging points at the backend even though the interpreter had enough
  information to reject the program earlier.

The issue is not that the interpreter lacks type information. These helpers have
`arg_layout` and `ret_layout`. If a numeric low-level op reaches them with a
non-numeric layout or unsupported size, that is an invariant violation.

### Long-Term Ideal

The interpreter should have one numeric layout classifier and all numeric helpers
should use it.

For example:

```zig
const NumericOperandKind = union(enum) {
    unsigned_int: struct { bits: u16 },
    signed_int: struct { bits: u16 },
    float: struct { bits: u16 },
    dec,
};
```

The classifier should:

- inspect the layout tag, not just byte size
- distinguish integer, float, Dec, and unsupported layouts
- return an invariant error for anything outside the numeric domain
- be reused by binops, comparisons, compare, shifts, pow/log/round/floor/ceiling,
  widening, and truncation helpers where applicable

Then the numeric helpers should switch on `NumericOperandKind`, not raw sizes.

### Recommendation

Replace silent numeric defaults with explicit invariant errors and centralize
numeric layout decoding.

Concrete changes:

1. Add a helper near the interpreter numeric functions:

   ```zig
   fn numericOperandKind(self: *LirInterpreter, layout: layout_mod.Idx) Error!NumericOperandKind
   ```

2. Use it in:
   - `numBinOp`
   - `numUnaryOp`
   - `numCmpOp`
   - `evalCompare`
   - `numShiftOp`
   - `evalNumPow`
   - `evalNumSqrt`
   - `evalNumLog`
   - `evalNumRound`
   - `evalNumFloor`
   - `evalNumCeiling`
   - numeric widening/truncation helpers where layout sizes are decoded

3. Replace every `else => {}` or `else => 1` in numeric helpers with
   `invariantFailedError`.

4. Validate binary operand layouts before interpreting binary low-level numeric
   ops. Shifts should validate the lhs numeric layout and rhs shift-count layout
   separately.

5. Add focused interpreter tests that construct invalid LIR at the unit-test
   level and assert that the interpreter reports an invariant error. Do not rely
   on source Roc programs for invalid LIR cases.

Tests:

- Existing eval tests should still pass.
- Add unit tests for unsupported compare/binop/shift sizes.
- Add valid tests for signed/unsigned numeric behavior to make sure the shared
  classifier does not regress semantics.

Priority:

Medium. This is mostly hardening, but it directly protects us from
interpreter/backend disagreement.

## Consolidated Order

Recommended implementation order:

1. Fix static data callable-set runtime indexing. It is small, concrete, and
   directly related to the recent callable-set work.
2. Remove key-based imported source selection. The exact refs already exist, so
   this should be a contained cleanup.
3. Refactor Wasm numeric signedness to use operand layout for all numeric
   semantics.
4. Replace interpreter silent numeric defaults with invariant failures and a
   shared numeric layout classifier.
5. Consolidate record/tag child source projection. This is larger because it
   crosses lambda solving, executable payload building, and const-backed paths.
6. Redesign nominal backing publication around source payload representations.
   This is the most invasive and should be done after the smaller remaining
   payload-boundary leaks are gone.

The final desired state is that only dedicated boundary modules do name remapping,
source payload projection, runtime encoding conversion, and checked-type graph
projection. All later lowering, backend, and interpreter stages should consume
explicit refs and fail loudly if those refs are missing or inconsistent.
