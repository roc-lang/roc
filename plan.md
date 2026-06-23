# Destination-Passing and Allocation Reuse Plan

## Goal

Roc should avoid constructing large temporary return values when the caller has
storage that can receive the result directly. This includes:

- updating a consumed `Box(T)` by writing the new `T` into the existing box
  payload when uniqueness permits
- repacking consumed boxed lambdas by reusing their erased-callable allocation
  when uniqueness and payload layout permit
- lowering large aggregate returns through caller-provided result slots
- lowering record and tag construction into a demanded destination
- lowering string and list producer calls so they can append into a unique
  caller accumulator

All ownership and reuse decisions must be explicit before backend codegen.
Backends lower LIR statements mechanically and must not infer uniqueness,
capture layout, refcount policy, or result destinations from local code shape.

## Current Shape To Improve

The WASM-4 platform currently wraps the game model like this:

```roc
update_for_host! : Box(Model) => Box(Model)
update_for_host! = |boxed| {
    update_fn! = main.update!
    Box.box(update_fn!(Box.unbox(boxed)))
}
```

Today that creates these costs:

- `Box.unbox` copies the full `Model` payload out of the box.
- `update! : Model -> Model` builds a full replacement `Model`.
- `Box.box` allocates a fresh box and copies the replacement into it.
- The old box is released after the call.

The Rust Rocci Bird port instead stores one mutable state value and writes
fields in place. Roc should not copy that programming model, but the compiler
should be able to produce the same broad storage pattern when Roc ownership
makes it legal.

## Design Summary

Add a bounded set of result demands:

```text
return_slot(T)          write a by-memory result into ptr(T)
reuse_box(T)            consume Box(T), return a unique Box(T) suitable as T's slot
reuse_erased_callable   consume an erased callable and repack it when layouts permit
append_into(Str)        build a returned Str by appending into a unique Str
append_into(List(T))    build a returned List(T) by appending into a unique List(T)
```

The variants are keyed by proc id, result demand, and committed layouts.
Identical keys share one variant. Root and host ABI signatures do not change;
wrappers may call internal demand variants.

## Phase 0: Baseline And Test Harness

1. Capture the current optimized Rocci Bird wasm size and function section
   summary.
   - Build `/home/rtfeldman/code/roc-wasm4/examples/rocci-bird.roc` with the
     local compiler and `--opt=size`.
   - Record total wasm bytes, code section bytes, data section bytes, and top
     function body sizes.
   - Keep the script local unless it becomes stable enough for CI.

2. Add compiler tests for the shapes this work must preserve:
   - `Box.box(f(Box.unbox(x)))` with `T` containing a record, a tag, a list, and
     a string.
   - a function returning a boxed lambda whose capture contains a refcounted
     value.
   - a function returning a large record used immediately by another record
     update.
   - a string producer that concatenates two strings and is then concatenated
     onto a unique string by its caller.

3. Add debug-only IR inspection helpers if needed.
   - Prefer focused Zig tests over text snapshots.
   - Assert LIR operation kinds and RC effects, not backend instruction spelling.

Success criteria:

- Current behavior is covered before any rewrite lands.
- Tests can distinguish "fresh allocation" from "reuse-capable operation."
- Rocci Bird baseline numbers are written in the PR notes or a local scratch
  file for comparison.

## Phase 1: `reuse_box(T)`

### LIR Operation

Add a low-level op:

```text
box_prepare_update : Box(T) -> Box(T)
```

Meaning:

- Consumes the input box.
- Returns a unique `Box(T)` containing the old payload.
- If the input box is unique, returns the same allocation.
- If the input box is not unique, allocates a fresh box, copies the old payload
  into it, retains nested refcounted values as needed, and releases the consumed
  input box.

`RcEffect`:

```text
may_allocate = true
may_retain_or_release = true
may_runtime_uniqueness_check_args = arg 0
consume_args = arg 0
result_aliases_consumed_args = arg 0
result_unique = true
```

ARC may set `unique_args` for arg 0 using the existing born-unique and
no-live-borrow rules. Codegen skips the runtime uniqueness check when that bit
is set.

### Lowering Shape

Before:

```text
old_payload = box_unbox(boxed)
new_payload = update(old_payload)
new_box = box_box(new_payload)
ret new_box
```

After:

```text
prepared_box = box_prepare_update(boxed)
payload_ptr = ptr_cast(prepared_box)
old_payload = ptr_load(payload_ptr)
new_payload = update(old_payload)
ptr_store(payload_ptr, new_payload)
ret prepared_box
```

This first form still materializes `old_payload` and `new_payload`, but it
removes the outer box allocation and final box copy. Later phases replace the
call and stores with destination-aware lowering.

### Implementation Steps

1. Add `box_prepare_update` to `src/base/LowLevel.zig`.
2. Add RC metadata and debug comments beside `box_box` and `box_unbox`.
3. Lower the op in:
   - `src/backend/wasm/WasmCodeGen.zig`
   - `src/backend/dev/LirCodeGen.zig`
   - `src/backend/llvm/MonoLlvmCodeGen.zig`
   - the LIR interpreter if it handles this op directly
4. Add helper code for copying a box payload into a fresh box on the non-unique
   runtime path.
5. Add a pre-ARC LIR rewrite pass after TRMC and before ARC insertion.
   - Detect the exact direct shape first: returned `Box.box(call(Box.unbox(arg)))`.
   - Require matching `Box(T)` layouts.
   - Reject shared statement tails, multiple uses of the unboxed value, and any
     shape that is not represented explicitly.
6. Wire the pass in `src/lir/checked_pipeline.zig`.
7. Add certifier coverage for `box_prepare_update` through `RcEffect`.

Success criteria:

- The regression test shows `box_prepare_update` instead of `box_box` at the
  final rebox point.
- ARC output consumes the incoming box exactly once and returns one owned box.
- Dev, wasm, and LLVM backends all pass focused tests.
- Rocci Bird optimized wasm still runs.

## Phase 2: Boxed Lambda Reuse

### LIR Statement

Erased callables are not ordinary boxes. Add a dedicated statement or extend
`assign_packed_erased_fn` with an optional reuse input:

```text
assign_repacked_erased_fn {
    target: erased_callable
    reuse: erased_callable
    proc: LirProcSpecId
    capture: LocalId?
    capture_layout: layout.Idx?
    on_drop: ErasedCallableOnDrop
    next: CFStmtId
}
```

Meaning:

- Consumes `reuse`.
- If `reuse` is unique and its payload size/alignment match the new payload,
  call the old drop callback on the old capture, overwrite function pointer,
  overwrite drop callback, write new capture bytes, and return the same
  allocation.
- Otherwise allocate a new erased callable exactly as `assign_packed_erased_fn`
  does today, then release `reuse`.

The first implementation should require same payload size and same payload
alignment. Do not infer old payload size from an arbitrary `Box(a -> b)`.

### Implementation Steps

1. Add the statement to `src/lir/LIR.zig`, or add an explicit `reuse` field to
   `assign_packed_erased_fn` if that is cleaner after code inspection.
2. Extend ARC solving and insertion for the new statement.
   - `reuse` is consumed and may be checked for uniqueness.
   - `capture` is stored into the result and must be owned at the store point.
   - The result aliases consumed `reuse` only on the reuse path, and is unique.
3. Add a helper in each backend for erased-callable runtime uniqueness checks.
4. Add overwrite code:
   - read old drop callback
   - call it with old capture pointer when present
   - write new callable entry
   - write new drop callback
   - copy or store new capture bytes
5. Add direct lowering only for known same-shape cases.
   - A consumed erased callable immediately replaced with another erased
     callable of the same payload layout is eligible.
   - Unknown boxed-function values are not eligible.
6. Add tests with:
   - a primitive capture
   - a capture containing a string or list
   - nested boxed callable capture teardown

Success criteria:

- Same-shape boxed lambda replacement emits the repack statement.
- Old capture teardown runs exactly once.
- Non-unique runtime path produces correct results and no leaks.
- Unknown-size boxed function values keep ordinary fresh allocation behavior.

## Phase 3: General `return_slot(T)`

### LIR Shape

For by-memory return layouts, create internal variants shaped like:

```text
original: f(args...) -> T
slot:     f_slot(out: ptr(T), args...) -> {}
```

The slot variant lowers the function body directly into `out`. Scalar, pointer,
and zero-sized returns keep ordinary value returns.

### Implementation Steps

1. Add a result-demand key type in the LIR lowering layer.
   - Include proc id, demand kind, result layout, and any element layout.
   - Do not include call-site ids.
2. Add proc-variant construction for `return_slot(T)`.
   - Add a synthetic first local for `out: ptr(T)`.
   - Use a zero-sized return layout for the slot variant.
3. Add `lowerExprIntoPtr(dest_ptr, expr, layout, next)`.
   - Base rule: lower `expr` normally into a temp, then `ptr_store(dest_ptr, temp)`.
   - Specialized rules are added in later phases.
4. Update direct call lowering:
   - When the caller has a result slot, call the slot variant.
   - Otherwise call the ordinary variant.
5. Ensure root and host wrappers adapt between ABI value results and internal
   slot variants.
6. Add backend tests for by-memory returns on wasm, dev, and LLVM.

Success criteria:

- Large internal returns can be written directly to a caller slot.
- ABI-facing roots keep the same external behavior.
- No backend chooses result slots on its own.
- Compile-time evaluation still uses the single-variant mode unless explicitly
  updated.

## Phase 4: Destination-Aware Record And Tag Construction

### New LIR Forms

Add explicit destination writes for aggregates:

```text
store_struct {
    dest: ptr(Struct)
    fields: LocalSpan
    next: CFStmtId
}

store_tag {
    dest: ptr(TagUnion)
    variant_index: u16
    discriminant: u16
    payload: LocalId?
    next: CFStmtId
}
```

If direct field stores are needed for in-place updates, add field-level forms:

```text
field_ptr(ptr(Struct), field_index) -> ptr(Field)
tag_payload_ptr(ptr(TagUnion), variant_index) -> ptr(Payload)
```

These must carry layout indexes or committed field positions explicitly. A
backend must never recover field offsets from names.

### Lowering Rules

1. Closed record literal into slot:
   - Lower each field.
   - Store each field directly into `dest`.

2. Record update into a distinct slot:
   - Read unchanged fields from the base value.
   - Lower changed fields.
   - Store all fields into `dest`.

3. Record update where `dest` aliases the consumed base:
   - Identify fields whose old values are needed after their slot might be
     overwritten.
   - Move those fields to temporaries first.
   - Write changed fields.
   - Leave unchanged fields in place when their value is still valid and their
     storage layout matches.
   - Release overwritten old refcounted fields exactly once.

4. Tag construction into slot:
   - Lower payload into the slot's payload storage when possible.
   - Write discriminant after payload writes that depend on the old tag payload.

5. Tag transition where `dest` aliases the consumed old tag:
   - Read/move every old payload field needed by the new value before changing
     the discriminant.
   - Release old payload fields not moved into the new value.
   - Write new payload and discriminant in the explicit order chosen by LIR.

### Implementation Steps

1. Preserve record-update structure long enough for LIR lowering.
   - Current monotype lowering expands `{ ..base, field: value }` into field
     reads plus a fresh record.
   - Add an explicit lowered expression form for record update, or record enough
     data during lowering to reconstruct the update without source inspection.
2. Add `lowerRecordIntoPtr` and `lowerTagIntoPtr`.
3. Add alias-aware lowering for consumed base equals destination.
4. Extend ARC for `store_struct`, `store_tag`, and any field-pointer forms.
5. Extend the debug certifier for field moves and overwrite releases.
6. Implement backend lowering by direct stores into the supplied pointer.
7. Add tests for:
   - record update with primitive fields
   - record update with strings/lists
   - tag transition with moved payload fields
   - same-slot update where write order matters

Success criteria:

- LIR for a same-slot record update does not allocate a full replacement record.
- Refcounted overwritten fields are released exactly once.
- Tag transitions preserve results and pass the certifier.
- Rocci Bird's `Model` update path no longer builds a full boxed replacement
  before writing the result.

## Phase 5: Connect `reuse_box(T)` To Slot Variants

After Phases 1, 3, and 4 exist, rewrite:

```text
Box.box(update(Box.unbox(boxed)))
```

to:

```text
prepared_box = box_prepare_update(boxed)
slot = ptr_cast(prepared_box)
update_slot(slot, slot)
ret prepared_box
```

`update_slot(slot, slot)` means the old payload is read from the same slot that
receives the new payload. This is legal only when the slot variant's lowering
uses the alias-aware record/tag rules from Phase 4.

Success criteria:

- The Rocci Bird platform wrapper calls the slot variant of `main.update!`.
- The hot frame update does not allocate a fresh outer model box.
- The wasm `update` body is materially smaller than the Phase 0 baseline.

## Phase 6: `append_into(Str)`

### Lowering Rules

Under `append_into(Str)` demand:

- string literal: append literal bytes to the accumulator
- string variable: append that string to the accumulator
- `Str.concat(left, right)`: lower `left` under the same append demand, then
  lower `right` under the same append demand
- direct call returning `Str`: call the callee's `append_into(Str)` variant
- branch returning `Str`: each branch writes into the same accumulator
- anything else: materialize ordinary `Str`, then append it explicitly

The accumulator must be unique at the append site. Existing string runtime
uniqueness checks remain unless ARC proves them redundant.

### Implementation Steps

1. Add a result-demand key for `append_into(Str)`.
2. Add an internal proc variant:
   - input accumulator local
   - ordinary args
   - return updated accumulator, or mutate an accumulator slot and return unit
     after choosing the cleaner LIR shape
3. Add lowering rules for literals, concat, direct calls, and branches.
4. Use the ordinary materialize-then-append rule for all remaining expressions.
5. Add tests with allocation counts:
   - nested concat in one function
   - concat producer called by a concat caller
   - branch returning a concat
   - non-unique accumulator takes the ordinary copy path

Success criteria:

- A producer that returns `a ++ b` can append both pieces into the caller's
  unique accumulator.
- No call-site-specific variant explosion occurs.
- Existing `Str.concat` behavior stays correct for non-unique inputs.

## Phase 7: `append_into(List(T))`

Repeat Phase 6 for lists after the string form is stable.

Additional constraints:

- Element layout is part of the demand key.
- Appended elements are owned at the point they are stored.
- Seamless slices and list header layout rules remain enforced by existing list
  operations.

Tests:

- list literal appended into a unique list
- direct list producer called under append demand
- branch returning list
- refcounted element list
- layout mismatch keeps ordinary materialization

Success criteria:

- List builders can append into a unique caller accumulator.
- Refcounted elements move into the destination exactly once.
- Existing in-place `List.map` rules are unchanged.

## Phase 8: Rocci Bird Verification

1. Rebuild the compiler in optimized mode.
2. Rebuild Rocci Bird with `--opt=size`.
3. Run Binaryen size optimization through the integrated wrapper.
4. Record:
   - final wasm byte size
   - code section byte size
   - data section byte size
   - largest function body sizes
5. Disassemble or parse the wasm enough to confirm:
   - exported `update` is smaller than the Phase 0 baseline
   - the platform wrapper does not allocate a fresh outer model box each frame
   - sprite/static data remains in the data section
   - hot model updates are not emitted as full aggregate copy chains
6. Serve both optimized and dev builds through WASM-4 for manual testing.

Success criteria:

- The game works in optimized and dev builds.
- The optimized wasm size moves toward the Rust port's 10,655-byte result.
- Any remaining gap is explained by concrete remaining codegen or runtime costs,
  especially dynamic Roc lists versus Rust fixed-capacity arrays.

## Cross-Cutting Checks

- Backends contain no ownership or uniqueness decisions.
- Every new operation has explicit `RcEffect` or explicit ARC handling.
- Debug certifier covers every new ownership-moving statement.
- Root and host ABI behavior is unchanged.
- Compile-time evaluation remains deterministic and uses a bounded variant set.
- Variant keys are structural and do not include call-site ids.
- `--opt=dev`, `--opt=size`, and `--opt=speed` agree on program results.
- Focused tests pass before running broad suites.
- Broad suites to run before landing:
  - `zig build run-test-zig-module-lir_core`
  - `zig build run-test-zig-module-lir`
  - `zig build run-test-zig-trmc-lir`
  - `zig build run-test-cli -- --include-llvm --filter "[size]"`
  - Rocci Bird local `--opt=size` build

## Open Design Decisions To Resolve During Implementation

- Whether erased-callable reuse is a new statement or an extension of
  `assign_packed_erased_fn`.
- Whether slot variants return `{}` or return the destination pointer for easier
  chaining.
- Whether `append_into(Str)` should return the updated string header or mutate a
  caller-owned header slot.
- The exact field-pointer LIR shape for aggregate slot writes.
- How much record-update structure should be preserved in Monotype versus
  Lambda Solved before direct LIR lowering consumes it.
