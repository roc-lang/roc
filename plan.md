# Plan: Long-Term Fixes For Late Guessing And Recovery

This file records the result of investigating each suspect in `problems.md`.

The standard for every recommendation is the same:

- The stage that knows a fact directly must publish it.
- Later stages must consume explicit published facts.
- Missing published facts are compiler invariant violations.
- No source-name lookup, type-key search, layout shape matching, or value-shape guessing should substitute for missing metadata.

## Implementation Status: 2026-05-17

This branch implements the consolidated checked-payload publication design for
the const/callable/capture problems below.

Completed:

- Problem 1: the dev backend now requires matching `Num.abs_diff` argument
  layouts, passes the operand layout into code generation, and no longer falls
  back to signedness guesses from literals or result layout.
- Problem 2: const and callable instantiation records now preserve the checked
  payload ids from their requests, dependency summaries carry payload-bearing
  requests, and finalization consumes those requests instead of rediscovering
  payloads from canonical type keys.
- Problem 3: capture-slot planning is payload-driven. Capture slot sources read
  published `ValueInfo.source_ty_payload`, project it once when necessary, and
  store the checked payload on the capture plan. The const record-extra-field
  path now also consumes the child value's published source payload instead of
  calling `rootForKey(child_info.source_ty)`.
- Problem 4: private capture finalization is payload-driven. Capture-slot plan
  nodes carry `source_ty`, `source_ty_payload`, and `source_scheme`; serializable
  leaves use the enclosing node's payload; `PrivateCaptureRef` carries
  `source_ty_payload`; mono lowering uses that payload directly.
- Problem 5: `layout.Idx.named_fn` has been removed. Static/direct call identity
  stays in call IR; layouts describe runtime memory representation only.
- Problem 6: callable-set runtime encoding is explicit. Member ids convert to
  runtime discriminants through shared canonical helpers, IR carries the
  callable-set key on runtime discriminant reads, and compile-time finalization
  verifies decoded members against runtime-discriminant order.
- `design.md` now describes this as the long-term model: canonical keys validate
  payload identity, checked payload ids are carried through the boundaries that
  consume them, layouts do not encode call identity, numeric low-level lowering
  consumes operand layouts, and callable-set runtime discriminants are centrally
  encoded. It also records the comparison with Cor's LSS experiment.
- The callable-in-data crash repros are covered by eval tests and pass through
  interpreter, dev, and wasm.
- `Num.abs_diff` signedness coverage exercises signed `I8` operands whose result
  layout is unsigned.

Verification on the final code:

```sh
zig build fmt
zig build test-eval -- --filter "compile-time callable result reused" --filter "Bool constants inside heap containers" --threads 1 --verbose
zig build test-eval -- --filter "I8.abs_diff uses signed operand layout" --threads 1 --verbose
zig build minici
```

All passed. On macOS, the final coverage step in `minici` reports that kcov
coverage is unsupported on this platform, but the `minici` command exits 0.

Still left from this plan:

- Hardening: add semantic-audit checks for the completed payload-boundary rules
  and runtime-encoding rules so these recovery patterns cannot silently return
  in future changes.

## 1. Dev Backend `Num.abs_diff` Signedness Guess

### Current State

`Num.abs_diff` is a low-level numeric operation. In Roc source, signed integer `abs_diff` returns an unsigned result:

- `I8.abs_diff : I8, I8 -> U8`
- `I16.abs_diff : I16, I16 -> U16`
- `I32.abs_diff : I32, I32 -> U32`
- `I64.abs_diff : I64, I64 -> U64`
- `I128.abs_diff : I128, I128 -> U128`

That means the return layout is not enough to decide how to compare the operands. The backend needs the operand layout, because signed operands require signed comparison and unsigned operands require unsigned comparison.

The dev backend currently has the right fact available at the call site:

- `assign_low_level` stores argument locals.
- each local has a layout in the LIR store.
- `src/backend/dev/LirCodeGen.zig` calls `self.valueLayout(args[0])` before calling `generateAbsDiff`.

But `generateAbsDiff` still accepts `arg_layout: ?layout.Idx` and falls back to `inferAbsDiffInputLayout` if it is missing. That fallback guesses signedness from the return layout and whether either operand is a negative immediate.

### Why This Is The Same Class Of Problem

Signedness is not a backend inference problem. It is already known by checking/lowering and is present in the argument local layout. The fallback is dangerous because it creates a second, weaker source of truth:

- negative immediates imply signed input only for some cases;
- non-negative literals do not prove unsigned input;
- return type is explicitly not authoritative for signed `abs_diff`;
- future refactors could accidentally call `generateAbsDiff` without the operand layout and get plausible but wrong code.

### Long-Term Ideal

There should be exactly one source of truth for `abs_diff` operand signedness in backend code: the operand layout attached to the LIR argument local.

The dev backend should not infer signedness from value shape or return layout.

### Concrete Plan

1. Change `generateAbsDiff` to require `operand_layout: layout.Idx`, not `?layout.Idx`.
2. Delete `inferAbsDiffInputLayout`, `immediateIsNegative`, and `signedCounterpartLayout` from the dev backend.
3. At the low-level call lowering site, compute:

   ```zig
   const lhs_layout = self.valueLayout(args[0]);
   const rhs_layout = self.valueLayout(args[1]);
   ```

   Then require `lhs_layout == rhs_layout`, except for operations whose ABI deliberately allows mixed layouts. `Num.abs_diff` should not be mixed.
4. Pass `lhs_layout` into `generateAbsDiff`.
5. Add an invariant message if the argument layouts differ:

   ```text
   dev backend invariant violated: num_abs_diff argument layouts differ
   ```

6. Add eval coverage for signed non-negative variables, not only negative literals. The test should force a signed input layout even when neither immediate is negative:

   ```roc
   x : I8
   x = 120

   y : I8
   y = -120

   I8.abs_diff(x, y)
   ```

   This guards the actual signed comparison path rather than the literal fallback.
7. Add a small backend/unit test or audit rule preventing `inferAbsDiffInputLayout` from returning.

### Expected End State

`Num.abs_diff` signedness is carried by LIR local layouts all the way to codegen. Backend code generation either receives the operand layout explicitly or reports a compiler invariant violation. It never guesses from literals or result layout.

## 2. Checked-Type Payload Lookup From Type Keys During Dependency Finalization

### Current State

Const and callable binding instantiation requests already have the right shape at their public API boundary:

```zig
pub const ConstInstantiationRequest = struct {
    key: ConstInstantiationKey,
    requested_source_ty_payload: CheckedTypeId,
};

pub const CallableBindingInstantiationRequest = struct {
    key: CallableBindingInstantiationKey,
    requested_source_fn_ty_payload: CheckedTypeId,
};
```

The stores validate those payloads against the key:

- the payload id must be in the current artifact's checked type store;
- the payload root key must match the requested canonical type key.

But after validation, the payload id is discarded. `ConstInstantiationRecord` stores only:

```zig
id
key
state
```

and `CallableBindingInstantiationRecord` does the same. Dependency summaries also store only keys for const/callable instances:

```zig
pub const ComptimeConcreteValueUse = union(enum) {
    const_instance: ConstInstantiationKey,
    callable_binding_instance: CallableBindingInstantiationKey,
    ...
};
```

Because the payload id was thrown away, `ensureDependencySummaryConcreteDependency` later has to call:

- `checkedTypePayloadForConstInstanceDependency`
- `checkedTypePayloadForCallableBindingDependency`

Those functions search the current artifact and imported artifacts for a checked type root whose canonical key matches the requested type key.

### Why This Is The Same Class Of Problem

The earlier stage had the checked payload id. We validate that it is correct, then discard it, then later reconstruct it by searching for the same key.

That creates the exact shape of bug we want to eliminate:

1. A producer knows a concrete fact.
2. The persisted/published data keeps only a lossy key.
3. A consumer later searches stores to recover the missing fact.
4. If the key is ambiguous, stale, missing from the searched imports, or not projected yet, the consumer has to guess, broaden the search, or fail late.

The current code is better than the old callable descriptor bug because it validates the recovered payload before using it. But validation after recovery is still weaker than carrying the payload directly.

### Long-Term Ideal

Instantiation identity and instantiation payload should be separate, but both should be preserved.

- The key remains the deduplication identity.
- The checked payload id is stored as required payload attached to the row/request/use.
- Later stages never call `rootForKey` to recreate that payload.

### Concrete Plan

1. Extend `ConstInstantiationRecord`:

   ```zig
   pub const ConstInstantiationRecord = struct {
       id: ConstInstanceId,
       key: ConstInstantiationKey,
       requested_source_ty_payload: CheckedTypeId,
       state: ConstInstantiationState,
   };
   ```

2. Extend `CallableBindingInstantiationRecord`:

   ```zig
   pub const CallableBindingInstantiationRecord = struct {
       id: CallableBindingInstanceId,
       key: CallableBindingInstantiationKey,
       requested_source_fn_ty_payload: CheckedTypeId,
       state: CallableBindingInstantiationState,
   };
   ```

3. Change `reserveRequest` so it stores the validated payload id. If a key is already present, verify the incoming payload id matches the existing record's payload id. A mismatch is a checked artifact invariant violation.

4. Add store accessors:

   ```zig
   pub fn requestForRef(self: *const ConstInstantiationStore, ref: ConstInstanceRef) ConstInstantiationRequest
   pub fn requestForRef(self: *const CallableBindingInstantiationStore, ref: CallableBindingInstanceRef) CallableBindingInstantiationRequest
   pub fn requestForKey(self: *const ..., key: ...) ?...
   ```

5. Change `ComptimeConcreteValueUse` to carry full requests for instance dependencies:

   ```zig
   pub const ComptimeConcreteValueUse = union(enum) {
       const_instance: ConstInstantiationRequest,
       callable_binding_instance: CallableBindingInstantiationRequest,
       procedure_callable: canonical.ProcedureCallableRef,
       procedure_callable_with_payloads: ProcedureCallableDependency,
   };
   ```

   If copying full request structs everywhere is too verbose, introduce named dependency structs, but they must contain both key and payload.

6. Update dependency summary builders:

   - When the dependency is the original compile-time request, use the request payload already in hand.
   - When the dependency comes from an existing instance ref, read the request from the instantiation store.
   - When the dependency comes from an imported const/callable use, project the checked payload once at dependency-summary construction time, then store the projected payload id in the summary.

7. Delete these recovery helpers:

   - `checkedTypePayloadForConstInstanceDependency`
   - `checkedTypePayloadForCallableBindingDependency`

8. Change `ensureDependencySummaryConcreteDependency` so it consumes the request from the summary directly:

   ```zig
   .const_instance => |request| ensureConstInstanceRequest(..., request)
   .callable_binding_instance => |request| ensureCallableBindingInstanceRequest(..., request)
   ```

9. Add semantic audit checks preventing the old recovery helpers from returning:

   - `checkedTypePayloadForConstInstanceDependency`
   - `checkedTypePayloadForCallableBindingDependency`
   - direct `rootForKey(key.requested_source_ty)` in finalization
   - direct `rootForKey(key.requested_source_fn_ty)` in finalization

### Expected End State

Concrete dependency summaries are no longer lossy. A const/callable instance dependency carries the exact checked payload root required to reserve or evaluate it. Finalization does not search checked type stores by canonical key to recover missing payloads.

## 3. Capture And Const Planning Recover Checked Payloads From Source Type Keys

### Current State

There are two related but different paths in `src/lir/checked_pipeline.zig`.

Capture-slot planning is still primarily key-driven. `CaptureSlotPlanBuilder.planFor` receives:

```zig
source_ty: CanonicalTypeKey
value_info: ValueInfoId
```

and `buildPlan` recovers the checked payload with:

```zig
artifact.checked_types.rootForKey(source_ty)
```

That is exactly the recovery pattern we want to eliminate. The source value metadata already has more precise information:

```zig
pub const ValueInfo = struct {
    source_ty: canonical.CanonicalTypeKey,
    source_ty_payload: ?ConcreteSourceType.ConcreteSourceTypeRef,
    ...
};
```

The lambda-solved stage validates that `source_ty_payload`, when present, matches `source_ty`. The LIR checked pipeline also already has a good projection helper:

```zig
projectCheckedPayloadForConcreteSourcePayload(...)
```

That helper projects a concrete source type payload into the current artifact's checked type store and validates the expected canonical key. It is already used in callable-result planning, so the compiler has the right model available. Capture-slot planning is just not using it consistently.

Const-graph planning is in better shape. `ConstGraphPlanBuilder.buildPlan` starts from a `CheckedTypeId`, and most recursive const planning passes child checked payload ids directly. The suspect path is the "extra record field" path in `recordFields`. When aggregate metadata contains a field that is not present in the checked record payload, the code reads the child's `ValueInfo` and then calls:

```zig
artifact.checked_types.rootForKey(child_info.source_ty)
```

That is another recovery from a key even though the child `ValueInfo` can carry `source_ty_payload`.

### Why This Is The Same Class Of Problem

The producer of `ValueInfo` knows the value's concrete source type payload, or it knows that publishing a payload failed and should have been an invariant violation earlier. Capture and const planning should not replace that payload with only a canonical key and later search checked type stores for a root with the same key.

The canonical key is not enough information for long-term compiler correctness:

- it does not identify which artifact published the payload;
- it does not identify which checked payload root should be used when several roots share a key through imports or projection;
- it hides whether the value metadata was incomplete;
- it makes later stages responsible for recreating information they did not produce.

### Long-Term Ideal

Capture and const reification planning should be payload-driven.

The planning APIs should accept either a checked payload id or a concrete source payload reference that can be projected exactly once into a checked payload id. A bare `CanonicalTypeKey` may be used only as a validation key, never as the thing from which the checked payload is rediscovered.

### Concrete Plan

1. Introduce an explicit planning input for source values, for example:

   ```zig
   const SourceValuePayload = struct {
       key: canonical.CanonicalTypeKey,
       payload: ConcreteSourceType.ConcreteSourceTypeRef,
   };
   ```

   If the call site already has a checked payload id, use a sibling form:

   ```zig
   const CheckedValuePayload = struct {
       key: canonical.CanonicalTypeKey,
       payload: checked_artifact.CheckedTypeId,
   };
   ```

   The important rule is that the key is validation context, not lookup input.

2. Change `CaptureSlotPlanBuilder.planFor` so it receives a payload-bearing input instead of `source_ty` alone. The common path should read:

   ```zig
   const value_info = artifact.getValueInfo(value_info_id);
   const payload_ref = value_info.source_ty_payload orelse invariant(...);
   const checked_ty = projectCheckedPayloadForConcreteSourcePayload(..., payload_ref, value_info.source_ty, ...);
   ```

3. For capture-slot descriptors that currently publish only `source_ty`, extend the descriptor data so each capture slot has the source payload reference too. If there are call sites where the slot source is already known as a checked type, publish that checked payload directly.

   The descriptor should not force consumers to reconstruct the payload. A capture slot means "this value with this source type payload is stored in the closure/callable value", so the payload belongs in the slot metadata.

4. Delete the `rootForKey(source_ty)` call from `CaptureSlotPlanBuilder.buildPlan`. Missing payload metadata should produce a checked-pipeline invariant violation with a direct message such as:

   ```text
   capture slot source type payload was not published
   ```

5. Fix the const-graph "extra record field" path the same way. When it reads `child_info`, it should require `child_info.source_ty_payload`, project it with `projectCheckedPayloadForConcreteSourcePayload`, and pass the resulting `CheckedTypeId` into the recursive const planning path.

6. Audit all value metadata that can be followed by const/capture planning. Aggregate fields, tuple elements, tag payloads, list elements, boxes, nominal backing values, and callable captures should publish a child `ValueInfoId` whose `source_ty_payload` is present whenever the child is later reified.

7. Add semantic audit checks that fail if these patterns return:

   - `CaptureSlotPlanBuilder` calling `rootForKey(source_ty)`
   - const planning calling `rootForKey(child_info.source_ty)`
   - any checked-pipeline reification planner deriving `CheckedTypeId` from only a `CanonicalTypeKey`

8. Add regression coverage that exercises the specific paths:

   - a closure capture whose source type payload comes from an imported artifact;
   - a captured boxed callable value;
   - a record constant with an extra runtime field whose child type is not recoverable by a local root key;
   - a recursive callable/record case where key lookup would find the wrong root if payload identity were lost.

### Expected End State

Capture-slot planning and const planning consume exact payload metadata. Source type keys remain useful for validation and error messages, but they are not used as database queries to recover checked payload roots.

## 4. Private Capture Finalization Recovers Checked Payloads

### Current State

Private capture publication happens in `src/eval/compile_time_finalization.zig` through `PrivateCaptureBuilder`.

There are three concrete recovery sites:

1. `captureRef` receives a top-level capture `source_ty` from a callable-set descriptor slot and calls:

   ```zig
   artifact.checked_types.rootForKey(source_ty)
   ```

   It only needs this to produce a `source_scheme` for `PrivateCaptureRef`.

2. `serializableLeaf` receives `SerializableCaptureLeafPlan`, which contains `requested_source_ty`, `source_scheme`, `schema`, and `reification_plan`. It reserves a promoted const template using `leaf.source_scheme`, then calls:

   ```zig
   artifact.checked_types.rootForKey(leaf.requested_source_ty)
   ```

   to reserve the const instance.

3. `executableSerializableLeaf` does the same thing for executable capture leaves that contain callable slots and therefore must be persisted as promoted const instances.

The plan builder already had the exact checked payload when it created serializable leaves:

```zig
serializableLeafPlan(checked_ty, source_ty, value_info_id)
```

It uses `checked_ty` to build the const reification plan and to compute `source_scheme`, but the payload id is not stored in `SerializableCaptureLeafPlan`. Finalization therefore recreates it from `requested_source_ty`.

For the top-level `PrivateCaptureRef`, the situation is worse. The `captureRef` call is driven by `canonical.CallableSetCaptureSlot`, whose fields are currently:

```zig
slot
source_ty
exec_value_ty
```

The descriptor slot does not publish the source payload for the captured value. Finalization combines that descriptor key with a capture-slot plan id, then uses `rootForKey` to get the root.

`PrivateCaptureRef` itself also stores only `source_scheme`, not the checked payload root. Later mono lowering resolves that scheme key back to a root before lowering the private capture node.

### Why This Is The Same Class Of Problem

Private capture finalization is a consumer of plans. It should not be discovering type payloads.

The checked-pipeline planner and lambda-solved value metadata know the payload. Finalization only sees a runtime value, a capture plan id, and a callable descriptor slot. When finalization calls `rootForKey`, it is compensating for a lossy plan boundary.

The `source_scheme` field is better than a bare type key for some public cross-artifact cases, but for private capture lowering it is still a key that must be resolved later. A private capture is already an artifact-local graph node with an artifact key, so it can refer to the exact checked root directly.

### Long-Term Ideal

Private capture plans and private capture refs should be checked-payload-driven.

Every `CaptureSlotReificationPlanId` should identify both:

- the exact checked payload root for the value represented by the slot;
- the reification shape for that payload.

Finalization should consume that payload directly when it creates:

- `PrivateCaptureRef`;
- promoted const templates;
- promoted const instance requests;
- lowered private capture expressions.

### Concrete Plan

1. Replace the bare union storage for capture-slot plans with a payload-bearing record:

   ```zig
   pub const CaptureSlotReificationPlan = struct {
       source_ty: canonical.CanonicalTypeKey,
       source_ty_payload: CheckedTypeId,
       source_scheme: canonical.CanonicalTypeSchemeKey,
       kind: CaptureSlotReificationPlanKind,
   };
   ```

   Move the current union cases into `CaptureSlotReificationPlanKind`.

   This makes the payload/scheme part of every node, not only serializable leaves.

2. Remove `requested_source_ty` and `source_scheme` duplication from `SerializableCaptureLeafPlan`, or keep them only as cached derived fields with debug verification. The ideal data is:

   ```zig
   pub const SerializableCaptureLeafPlan = struct {
       schema: ComptimeSchemaId,
       reification_plan: ConstGraphReificationPlanId,
   };
   ```

   The enclosing `CaptureSlotReificationPlan` supplies `source_ty`, `source_ty_payload`, and `source_scheme`.

3. Change `PrivateCaptureBuilder.captureRef` to accept only `plan_id`, `physical`, and `capture_index`. It should read the plan record:

   ```zig
   const plan = artifact.comptime_plans.captureSlot(plan_id);
   const source_payload = plan.source_ty_payload;
   const source_scheme = plan.source_scheme;
   ```

   Then it can verify that the descriptor slot, if one is available at the call site, agrees with `plan.source_ty`. It should not use the descriptor slot as the source of type metadata.

4. Extend callable-set capture slots or their checked-pipeline plan input so descriptor slot metadata is not the only source of top-level capture type identity.

   The better long-term model is:

   - lambda-solved `ValueInfo` carries `source_ty_payload`;
   - `CaptureSlotPlanBuilder` projects that payload to a `CheckedTypeId`;
   - the resulting `CaptureSlotReificationPlan` stores that `CheckedTypeId`;
   - descriptor slots remain executable-shape descriptors and validation keys.

   If `canonical.CallableSetCaptureSlot` remains a canonical descriptor, it may keep `source_ty` and `exec_value_ty`, but finalization must not rely on it to recover checked payload identity.

5. Change `serializableLeaf` and `executableSerializableLeaf` to reserve const instances using the plan's `source_ty_payload`:

   ```zig
   .requested_source_ty_payload = plan.source_ty_payload
   ```

   Delete both `rootForKey(leaf.requested_source_ty)` calls.

6. Change `PrivateCaptureRef` from scheme-only to payload-direct:

   ```zig
   pub const PrivateCaptureRef = struct {
       artifact: CheckedModuleArtifactKey,
       owner: PromotedCaptureId,
       node: PrivateCaptureNodeId,
       source_ty_payload: CheckedTypeId,
   };
   ```

   If some serialized/public boundary still needs `source_scheme`, keep it as a derived compatibility field temporarily, but mono lowering should use `source_ty_payload` directly. A private capture is concrete, so mono should not need to resolve a type scheme key to discover its root.

7. Update mono lowering:

   - `lowerPrivateCaptureExpr` should read `capture.source_ty_payload`;
   - it should validate the id is in `capture.artifact`'s checked type store;
   - it should pass that id to `lowerPrivateCaptureNode`;
   - it should not call `checkedTypeSchemeForKey` for private captures.

8. Add checked-artifact verification:

   - each capture-slot plan's `source_ty_payload` is in range;
   - `checked_types.roots[source_ty_payload].key == source_ty`;
   - `source_scheme` resolves to the same root, while the compatibility field exists;
   - serializable leaf const instances use the enclosing plan's payload;
   - `PrivateCaptureRef.source_ty_payload` is in range for `PrivateCaptureRef.artifact`.

9. Add semantic audit checks preventing these recovery calls:

   - `PrivateCaptureBuilder.captureRef` calling `rootForKey`;
   - `serializableLeaf` calling `rootForKey(leaf.requested_source_ty)`;
   - `executableSerializableLeaf` calling `rootForKey(leaf.requested_source_ty)`;
   - `lowerPrivateCaptureExpr` resolving a private capture type through only `source_scheme`.

### Expected End State

Private capture finalization becomes a pure consumer of capture plans. The capture plan says what type payload each captured value has, and finalization uses that payload when publishing private capture handles and promoted const instances. No private capture path recovers a checked root by matching a canonical type key.

## 5. `named_fn` Layout Sentinel And Backend Symbol Lookup

### Current State

`layout.Idx.named_fn` exists in two layout modules:

- `src/layout/layout.zig`
- `src/interpreter_layout/layout.zig`

The comment says it is a sentinel for call expressions where the function is resolved by name, and that the dev backend resolves these via symbol lookup.

However, a repository search shows no consumers. The only matches for `named_fn` are the two definitions. The new LIR call representation also does not need this sentinel:

```zig
assign_call: struct {
    target: LocalId,
    proc: LirProcSpecId,
    args: LocalSpan,
    next: CFStmtId,
},
assign_call_erased: struct {
    target: LocalId,
    closure: LocalId,
    args: LocalSpan,
    next: CFStmtId,
},
```

Direct calls carry a procedure spec. Erased calls carry the callable value local. Packed erased callable construction carries the concrete procedure spec and capture layout. That is the right split: call target identity is not a layout.

### Why This Is The Same Class Of Problem

If this sentinel were active, it would be the wrong design. A layout id should describe the runtime representation of a value. It should not mean "the callee will be recovered later by name".

That would mix two independent facts:

- value representation;
- callable identity / dispatch target.

The current compiler appears not to use the sentinel, which makes this less dangerous than the other problems. But leaving the sentinel in the public layout index enum documents and preserves a bad escape hatch.

### Long-Term Ideal

Remove `named_fn` entirely.

There should be no layout value for "named function". Named/static calls should be represented by call IR fields, such as `LirProcSpecId`, `ProcedureCallableRef`, `ProcedureBindingRef`, or an explicit static-dispatch plan. Callable values should be represented by closure/callable layouts only when an actual runtime value exists.

### Concrete Plan

1. Delete `Idx.named_fn` from `src/layout/layout.zig`.
2. Delete `Idx.named_fn` from `src/interpreter_layout/layout.zig`.
3. Keep `Idx.none` as the only non-layout sentinel in the layout index type if the map implementation still needs it.
4. Add a semantic audit check:

   ```text
   rg named_fn src
   ```

   should return no matches.

5. Add or keep tests that prove direct named calls do not need a function layout:

   - top-level direct function call;
   - method-style builtin call such as `List.map` or `List.iter`;
   - static dispatch through an associated method;
   - erased callable call, to prove actual runtime callable values still use callable-specific representation.

6. If any future lowering path wants a "no closure layout needed" marker, it should use an explicit optional field:

   ```zig
   capture_layout: ?layout.Idx
   ```

   or a call-target union:

   ```zig
   const CallTarget = union(enum) {
       direct: LirProcSpecId,
       erased_value: LocalId,
       hosted: HostedProcRef,
   };
   ```

   It should not encode this in `layout.Idx`.

### Expected End State

Layouts describe memory representation only. Static call identity travels through call IR. There is no layout sentinel that implies a backend should recover a function by name.

## 6. Callable-Set Member Selection By Runtime Discriminant

### Current State

Compile-time finalization selects a finite callable-set member by decoding the runtime callable-set value:

```zig
const discriminant = info.data.readDiscriminant(value.ptr);
return .{
    .member = @enumFromInt(discriminant),
    .payload_layout = info.variants.get(@intCast(discriminant)).payload_layout,
};
```

At first glance this looks like a guess. After tracing the producer side, it is not currently arbitrary:

- `RepresentationStore` canonicalizes callable-set members with dense member ids, using slice position as the id.
- checked-artifact verification requires published callable-set descriptor members to be dense canonical ids.
- executable type lowering keeps callable-set members as member-id-indexed variants.
- IR lowering builds callable-set layouts as tag unions where `variants[member_id]` is the member payload layout.
- IR lowering constructs callable-set values with:

  ```zig
  .discriminant = @intCast(@intFromEnum(callable.member.member_index))
  ```

- IR lowering switches on callable-set values using branch values equal to `member.member_index`.

So the current representation contract is:

```text
callable-set runtime discriminant == dense canonical CallableSetMemberId
```

That is a valid runtime encoding. The problem is that this encoding is implicit and scattered. Finalization sees a plain tag-union layout plus a callable result plan, then casts the tag discriminant to a member id without naming the encoding it is consuming.

### Why This Is Related But Not The Same Bug

This is legitimate value decoding, not type/layout recovery. Runtime values must be decoded somehow, and a finite callable set is represented as a tag union.

The risk is not that finalization has no producer fact. The producer fact exists: member ids are dense and are used as discriminants. The risk is that the fact is not first-class. Several stages independently rely on the convention:

- descriptor member order;
- executable callable-set type order;
- tag-union variant order;
- callable value construction discriminants;
- callable match branch discriminants;
- finalization's discriminant-to-member cast.

If any one stage later changes member ordering or allows sparse member ids, finalization can still compile while decoding the wrong member.

### Long-Term Ideal

Make callable-set runtime encoding explicit and centrally verified.

The long-term design can keep the efficient identity mapping:

```text
discriminant == member id
```

That is a good representation. But it should be expressed as a named compiler invariant or a published encoding object, not as repeated raw casts.

### Concrete Plan

1. Define the runtime encoding in one place, preferably next to canonical callable-set descriptors:

   ```zig
   pub const CallableSetRuntimeVariant = struct {
       discriminant: u16,
       member: CallableSetMemberId,
       payload_exec_ty: ?CanonicalExecValueTypeKey,
   };

   pub const CallableSetRuntimeEncoding = struct {
       callable_set_key: CanonicalCallableSetKey,
       variants: []const CallableSetRuntimeVariant,
   };
   ```

   If we choose not to allocate a separate object, define equivalent helper functions and document that `CanonicalCallableSetDescriptor.members` is ordered by runtime discriminant.

2. Centralize the invariant with helpers:

   ```zig
   pub fn callableSetDiscriminantForMember(member: CallableSetMemberId) u16
   pub fn callableSetMemberForDiscriminant(encoding: CallableSetRuntimeEncoding, discriminant: u16) CallableSetMemberId
   pub fn verifyCallableSetRuntimeEncoding(descriptor: CanonicalCallableSetDescriptor, encoding: CallableSetRuntimeEncoding) void
   ```

   The helper may return `@enumFromInt(discriminant)` internally, but only after checking the descriptor/encoding is dense and in range.

3. Make the producer publish the encoding.

   The natural producer is lambda-solved representation finalization, where callable-set descriptors are canonicalized and member ids are assigned. At that point the compiler should publish either:

   - `CallableSetRuntimeEncoding` alongside each descriptor; or
   - a descriptor invariant that says `descriptor.members[i].member == @enumFromInt(i)` and that descriptor order is runtime discriminant order.

4. Make executable type lowering consume the encoding.

   `src/ir/lower.zig` currently builds callable-set layouts with local `seen` checks and `variants[index] = payload_layout`. Keep the checks, but route through the shared helper:

   ```zig
   const discriminant = callableSetDiscriminantForMember(member.member);
   variants[discriminant] = payload_layout;
   ```

   If a first-class encoding object exists, iterate `encoding.variants` instead of trusting local member order.

5. Make callable-set value construction consume the encoding.

   `lowerCallableSetValue` should not manually cast `member.member_index`. It should call:

   ```zig
   const discriminant = callableSetDiscriminantForMember(callable.member.member_index);
   ```

   or ask the encoding for the discriminant.

6. Make callable match consume the encoding.

   `lowerCallableMatch` should build switch branch values from the same helper/encoding. It should also verify every branch has a member in the encoding for that callable-set key.

7. Make finalization consume the encoding.

   Change `selectFiniteCallableSetMember` so it does not do:

   ```zig
   .member = @enumFromInt(discriminant)
   ```

   It should do one of these:

   ```zig
   const variant = encoding.variantForDiscriminant(discriminant);
   .member = variant.member
   ```

   or, if `FiniteCallableResultPlan.members` remains the published runtime-order table:

   ```zig
   const planned_variant = members[discriminant];
   verifyCallableSetDiscriminantForMember(planned_variant.member, discriminant);
   .member = planned_variant.member
   ```

   The second option is smaller and still good if the field is renamed/documented as runtime-discriminant order.

8. Strengthen checked-artifact and lambda-solved verification:

   - every callable-set descriptor has at least one member;
   - member ids are dense and equal to their runtime discriminant;
   - `CallableResultMemberPlan` arrays are in runtime-discriminant order;
   - executable callable-set payload members are in runtime-discriminant order;
   - callable-set layout variant count equals descriptor member count;
   - every non-ZST payload layout corresponds to the member capture payload type.

9. Consider making `DiscriminantSource.runtime_callable_set` carry the callable-set key or encoding id:

   ```zig
   runtime_callable_set: CanonicalCallableSetKey
   ```

   or:

   ```zig
   runtime_callable_set: CallableSetRuntimeEncodingId
   ```

   `lower_ir` can still lower it exactly like a tag-union discriminant read, but the IR node would retain enough metadata for verification and debugging.

10. Add tests/audits:

   - a finite callable set with multiple closed members;
   - a finite callable set with captured members whose payload layouts differ;
   - an erased finite-set adapter that switches over multiple members;
   - a semantic audit that no code outside the encoding helper casts a callable-set runtime discriminant directly with `@enumFromInt(discriminant)`.

### Expected End State

Callable-set runtime decoding remains cheap: one discriminant read. But the relationship between runtime discriminants and callable-set members is explicit, centralized, and verified. Finalization decodes a value using a published runtime encoding instead of relying on an implicit convention.

## Recommended Implementation Order

These should not be fixed as six unrelated patches. The payload-related items share the same root cause and should be consolidated into one payload-publication design.

1. First, fix the small independent backend issue:

   - make `Num.abs_diff` require operand layout;
   - delete the fallback signedness inference;
   - add signed non-literal eval coverage.

2. Then do the checked-payload publication consolidation:

   - store payload ids in const/callable instantiation records;
   - store full payload-bearing instantiation requests in dependency summaries;
   - make capture-slot plans payload-bearing records;
   - make private capture refs and private capture const leaves consume payload ids directly;
   - delete all finalization/capture-planning `rootForKey` recovery helpers covered above.

3. After that, clean up dead layout vocabulary:

   - delete `Idx.named_fn`;
   - prove direct/static calls still lower through explicit call targets.

4. Finally, make callable-set runtime encoding explicit:

   - centralize the discriminant/member mapping;
   - route IR lowering and finalization through the same helper or encoding table;
   - add verifier/audit coverage.

The most important invariant across the whole plan is this:

```text
canonical keys validate payload identity; they do not recover payload identity
```

Any time a later stage needs a checked payload, callable identity, executable payload, or runtime encoding, that fact must have been published by the stage that knew it directly.
