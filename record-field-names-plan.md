# Record Field Names Migration Plan

## Summary

The end state should be:

- CIR stays name-based, because source programs are name-based.
- Monotypes stay name-aware at the type level, because record semantics and diagnostics still need names.
- MIR becomes purely structural for fixed-width product types.
- LIR stays purely layout-based.
- Layout metadata keeps only canonical field indices plus layouts; it does not keep field names.

In other words, we want to cleanly separate:

- source identity: field name
- semantic identity after CIR lowering: canonical field index
- physical identity after layout: layout slot index

That separation is the real goal. Deleting `layout.StructField.name` is the concrete payoff, but the broader motivation is to stop mixing those three concepts together.

## Why This Change Is Worth Doing

Today `layout.StructField.name: Ident.Idx` is wrong by construction. `Ident.Idx` is module-local, but layout data intentionally erases module ownership. Every time layout code or layout consumers read a field name out of `StructField`, they are depending on a best-effort guess about which ident store that `Ident.Idx` came from.

That causes several architectural problems:

- layout is supposed to be structural, but records still smuggle names through it
- MIR record logic and LIR struct logic do not line up cleanly
- several record operations still do O(n^2) name joins long after layout already exists
- record ordering rules are spread across multiple phases instead of being a single early invariant
- tag payloads and tuples do not benefit from the same structural treatment as records

The target architecture fixes that by making the phase boundary explicit:

- CIR resolves names
- MIR resolves canonical field positions
- LIR resolves physical field positions
- layout stores only the information needed to compute physical layout

## Prerequisite Gate: Eliminate Every Live Use Of `layout.StructField.name`

Before the final MIR structural refactor lands, we should make `StructField.name` completely unused. The architectural refactor is much cleaner if this is treated as an explicit gate instead of an incidental cleanup.

A fresh repo scan currently finds these remaining buckets of `StructField.name` / layout-name usage.

### Compile-Time And Layout-Lowering Users

These are the important non-legacy consumers that must be rewritten as part of the migration.

- `src/layout/store.zig`
  - `putRecord` sorts by `alignment desc, field name asc`.
  - `gatherRecordFields` currently gathers names and vars but does not assign a canonical pre-layout index.
  - `finishRecord` sorts resolved fields by `alignment desc, field name asc`.
  - `getFieldName` and `getRecordFieldOffsetByName` are layout-name helper APIs that exist only because names still leak into layout.

- `src/lir/MirToLir.zig`
  - `runtimeRecordLayoutFromExprs` still passes field names into `layout_store.putRecord`.
  - `runtimeRecordLayoutFromPattern` matches MIR record destructure fields to monotype fields by name.
  - `lowerRecord` reorders MIR record fields into layout order by matching layout field names.
  - `lowerRecordAccess` finds the LIR field slot by scanning layout field names.
  - record-destructure binding registration and record-pattern lowering still join MIR fields to layout fields by name.

These are exactly the uses that should move to:

- canonical MIR field order established before layout
- layout sort key `alignment desc, canonical_index asc`
- MIR-to-LIR translation by canonical index, never by name

### Legacy Runtime / Display Users

These are not part of the long-term compiler architecture and should be removed rather than preserved.

- `src/eval/StackValue.zig`
  - `RecordAccessor.findFieldIndex` compares field-name text through layout metadata.
  - Replacement: delete this helper along with the interpreter-driven record-name lookup paths that require it.

- `src/eval/interpreter.zig`
  - uses `findFieldIndex`, `getRecordFieldOffsetByName`, `getFieldName`, and direct `StructField.name` checks in multiple places
  - also uses layout field names to distinguish record-style tag-union structs from tuple-style ones
  - Replacement: delete the interpreter. This entire bucket is legacy.

- `src/values/RocValue.zig`
  - formats record-like structs by reading names out of layout fields
  - Replacement: delete `RocValue`.

- `src/eval/render_helpers.zig`
  - delegates canonical rendering to `values.RocValue.format()`
  - also uses type-name-to-layout-name matching for record display
  - Replacement: route display through `Str.inspect` instead of layout-name-driven formatting.

- `src/repl/eval.zig`
  - still has manual record formatting logic that matches layout fields by name
  - already partly wraps evaluation in `Str.inspect`
  - Replacement: finish the `Str.inspect` migration and delete the remaining manual formatting path.

- `src/layout/store_test.zig`
  - still tests name-based helpers such as `getRecordFieldOffsetByName`
  - Replacement: replace these with original-index-based layout tests, then delete the name-based APIs entirely.

The intended replacements for this legacy bucket are:

- delete the interpreter
- delete `RocValue`
- use `Str.inspect` for user-facing rendering instead of reading names from layout metadata

## Target Architecture

### Phase Boundaries

- CIR
  - records have names
  - tuples have element positions
  - tag payload syntax is still surface-syntax-aware

- Monotype
  - records remain name-bearing and sorted alphabetically
  - tuples remain positional
  - tag unions remain name-bearing
  - this stays the source of truth for canonical closed-record field order

- MIR
  - fixed-width product types are structural
  - records, tuples, and multi-field tag payloads use the same structural product representation
  - field identity is a canonical index, not a name

- LIR
  - fixed-width product types are layout structs
  - field identity is a layout slot index
  - MIR index to LIR index translation happens only during lowering

- Layout
  - struct fields store only:
    - canonical index
    - field layout
  - there is no field name stored here

### MIR Product Representation

MIR should move to a unified structural product concept. The right end state is something like:

- `MIR.Expr.struct_`
- `MIR.Expr.struct_access { field_idx }`
- `MIR.Pattern.struct_destructure`

For records, tuples, and tag payloads, the meaning of the MIR field index is:

- records: canonical closed-record field index, alphabetized before layout
- tuples: element index
- multi-field tag payloads: payload position index

`struct_access { field_idx }` in MIR should use a plain `u32`.

That is the right semantic type for MIR because:

- it is just an index into canonical product-field order
- it is not name-bearing
- it should not inherit layout-store packing constraints

LIR can keep its existing physical field-slot index representation for now if that is convenient. The important thing is that MIR uses a plain `u32` semantic field index.

### Tag Union Payloads

MIR should also model tag union payloads as these structural MIR structs.

This matters because tag payloads are positional, just like tuples, even though they are not spelled as tuples in source code. Once MIR is structural, they should not need their own special product model.

The clean end state is:

- zero-payload tag: no payload value
- one-payload tag: payload stored directly
- multi-payload tag: payload is a `MIR.Expr.struct_`

The same should be true for patterns:

- zero-payload tag pattern: no payload pattern
- one-payload tag pattern: payload pattern directly
- multi-payload tag pattern: payload is a `MIR.Pattern.struct_destructure`

This lets tuples, records, and tag payloads all flow through the same structural lowering rules.

### Ordering Invariants

We want one clear ordering invariant per level:

- Monotype closed record fields are alphabetical.
- MIR record fields are stored in monotype order, which is therefore alphabetical.
- MIR tuple fields are stored by element index.
- MIR multi-payload tag structs are stored by payload position.
- Layout sorts all structs by:
  - alignment descending
  - canonical MIR index ascending

That last tie-breaker must be explicit. We should never depend on sort stability implicitly.

## Architectural Decisions

### 1. Keep Names At The Type Boundary, Not In MIR/Layout

We should not try to make type-level records name-free. Type checking, row operations, diagnostics, and source semantics still need record names.

The change is specifically:

- names stay in CIR and monotypes
- names stop at the CIR-to-MIR boundary for product operations

### 2. Canonical Record Order Comes From Monotype Fields

We do not need a second source of truth for record ordering.

`src/mir/Monotype.zig` already documents and enforces that closed record fields are sorted by name. That should remain the canonical record-field order. MIR should simply reuse that order.

### 3. Partial Record Destructures Should Become Full-Arity MIR Struct Destructures

This is an important design choice.

Record destructures are name-based in source and may mention only a subset of fields. If MIR becomes positional, the simplest representation is not "subset plus indices". The simplest representation is:

- build a full canonical field-pattern array
- place the user-specified patterns in their canonical slots
- fill every omitted field with wildcard

That gives MIR a single structural destructure shape with no record-specific name mapping.

It does increase MIR size a bit for sparse record destructures, but it greatly simplifies lowering, layout translation, and later cleanup. If this ever becomes a real memory issue, we can revisit compact encodings later.

### 4. MIR-To-LIR Translation Should Be Ephemeral

We should not store a permanent MIR-index-to-LIR-index table in LIR nodes.

Instead:

- layout stores the canonical index on each field
- MirToLir computes the translation when lowering
- LIR nodes store only layout slot indices

This keeps LIR physically oriented and keeps the semantic-to-physical translation localized to a single phase.

### 5. Reuse The Existing Original-Index Infrastructure

The tuple path already largely works this way today:

- layout structs store original indices
- tuple lowering already finds fields by original index
- `structFieldInfoByOriginalIndex` already exists in `src/lir/MirToLir.zig`

The record path should converge to that same mechanism instead of keeping a separate name-based path alive.

## Migration Plan

## Phase 0: Remove Layout-Name Dependence First

This phase is the gate. The later MIR refactor should assume `StructField.name` is no longer a live dependency.

### 0A. Make Record Layout Construction Index-Based

Change `src/layout/work.zig` and `src/layout/store.zig` so record layout construction mirrors tuple layout construction.

Concrete steps:

- Add a canonical/original field index to pending and resolved record-field work items.
- In `gatherRecordFields`, flatten row segments, sort record fields alphabetically once, and assign canonical indices there.
- In `putRecord`, sort by `alignment desc, canonical_index asc`.
- In `finishRecord`, sort by `alignment desc, canonical_index asc`.
- Stop using field names as the layout tie-breaker.

After this, the layout store no longer needs record names in order to construct record layouts correctly.

### 0B. Replace Name-Based LIR Lowering With Index-Based Translation

Once layout fields carry canonical indices, the remaining compile-time users become straightforward.

Concrete steps:

- `lowerRecord`
  - MIR record fields are in canonical order.
  - layout fields already carry their canonical index.
  - lowering becomes: iterate layout fields, pick `mir_fields[layout_field.index]`.

- `lowerRecordAccess`
  - MIR access already knows the canonical field index.
  - use `structFieldInfoByOriginalIndex` (or a renamed equivalent) to get the LIR slot and field layout.

- record destructure pattern lowering
  - MIR struct-destructure patterns are full-arity and canonical.
  - iterate layout fields, then pick `mir_patterns[layout_field.index]`.

- runtime record-layout synthesis in `MirToLir`
  - stop passing names into layout
  - pass canonical-order field layouts only

At that point the compile-time/layout-lowering path no longer needs names after layout construction.

### 0C. Delete Legacy Runtime / Display Layout-Name Users

This is not a compiler architecture task; it is a cleanup gate.

Concrete steps:

- delete the interpreter paths that still perform runtime record-name lookup
- delete `StackValue.RecordAccessor.findFieldIndex`
- delete `layout_store.getFieldName`
- delete `layout_store.getRecordFieldOffsetByName`
- delete `RocValue`
- finish the REPL `Str.inspect` migration and delete the manual formatting fallback
- remove `render_helpers` dependence on `RocValue`

The plan should treat these as prerequisites, not as something the new MIR architecture is supposed to preserve.

## Phase 1: Canonicalize Records In MIR Before Unifying Product Types

This is the safest first compiler-facing step. Do not start by collapsing `record` and `tuple` into a single MIR variant. First make records positional while their existing names are still easy to find at the CIR/monotype boundary.

### 1A. Make Record Literals Canonical In MIR

`src/mir/Lower.zig` currently preserves source order for non-extension record literals. That must change.

`lowerRecord` should always produce fields in monotype order:

- for plain record literals, reorder provided fields into closed-record monotype order
- for record updates, keep the existing "expand to full closed record" behavior, but emit fields in the same canonical monotype order

This ensures that MIR record values are always full-arity and canonical, regardless of source order.

### 1B. Make Record Access Positional In MIR

Change MIR record access from:

- `record_access { field_name }`

to:

- `record_access { field_idx: u32 }`

The field index should be computed only at the CIR-to-MIR boundary by looking up the accessed name in the monotype field list.

Name lookup is still correct there, because the source module and the monotype are both known there.

### 1C. Make Record Destructures Positional In MIR

Change MIR record destructures so they no longer carry a parallel `field_names` span.

Instead:

- allocate a full canonical field-pattern array
- fill user-mentioned fields by looking up their canonical index in the record monotype
- fill all omitted fields with wildcard

After this step, record destructure semantics are fully positional inside MIR even though source syntax is still by-name.

### 1D. Update MIR Name-Based Helper Passes

Several helper passes currently depend on MIR record names even if they do not touch layout names directly.

Examples:

- `src/mir/LambdaSet.zig`
- `lambdaSetForRecordField` / `runtimeLayoutForRecordField` in `src/lir/MirToLir.zig`

These should switch from record-field-name lookup to direct canonical indexing. Once MIR values and accesses are canonical, these rewrites are mostly mechanical.

## Phase 2: Introduce A Structural MIR Product Model

Once records are already positional, collapsing MIR product types becomes much easier.

### 2A. Add MIR `struct_`, `struct_access`, And `struct_destructure`

Introduce structural MIR nodes:

- `Expr.struct_`
- `Expr.struct_access`
- `Pattern.struct_destructure`

Initially, it is fine to add these alongside the existing `record` / `tuple` forms if that makes the transition easier.

### 2B. Migrate Tuple Producers And Consumers

Tuples are already positional, so this is mostly a rename/unification step:

- tuple literals become `struct_`
- tuple access becomes `struct_access`
- tuple destructures become `struct_destructure`

This should be low-risk because the tuple path already uses original indices heavily.

### 2C. Migrate Record Producers And Consumers

Records should now also become `struct_` / `struct_access` / `struct_destructure`, using the canonical record index scheme introduced in Phase 1.

At this point MIR no longer needs:

- `FieldNameSpan`
- `record.field_names`
- `record_access.field_name`
- `record_destructure.field_names`

## Phase 3: Move Tag Union Payloads Onto MIR Structs

This is the step that completes the structural-product story.

### 3A. Represent Multi-Field Payloads As MIR Structs

Change MIR tag representations so that multi-field payloads use structural MIR products instead of raw payload spans.

The recommended end state is:

- `Expr.tag { name, payload }`
- `Pattern.tag { name, payload }`

where:

- no payload uses `ExprId.none` / `PatternId.none`
- single payload uses the payload directly
- multi-field payload uses `Expr.struct_` / `Pattern.struct_destructure`

### 3B. Lower Payloads In Positional Order

Payload field indices are positional by definition.

That means:

- payload position 0 stays position 0 in MIR
- layout sorts by `alignment desc, payload_index asc`
- MirToLir translates positional payload indices to layout slots exactly the same way it does for tuples

### 3C. Reuse The Same Structural Helpers Everywhere

After this change:

- record construction lowering
- tuple construction lowering
- multi-field payload construction lowering

can all use the same structural lowering helper.

Likewise for:

- access lowering
- destructure lowering
- pattern binding registration
- runtime layout synthesis

## Phase 4: Simplify MirToLir Around One Structural Translation Path

Once MIR products are unified, `src/lir/MirToLir.zig` should stop having separate record-vs-tuple logic for structural products.

The core lowering rule becomes:

1. MIR field indices describe canonical semantic order.
2. Layout fields describe physical order and carry their canonical semantic index.
3. MirToLir uses layout-field metadata to translate semantic index to physical slot.

That single rule should drive:

- struct construction
- struct field access
- struct destructure pattern lowering
- closure capture structs
- single-tag multi-payload layouts
- multi-tag variant payload layouts

This also removes the name-based record-only special cases that still exist today.

## Phase 5: Delete The Old Scaffolding

After all previous phases land, the remaining cleanup should be straightforward.

Delete:

- `layout.StructField.name`
- `layout_store.getFieldName`
- `layout_store.getRecordFieldOffsetByName`
- name-based layout tests
- `mir.FieldNameSpan`
- MIR record-specific name-bearing fields
- MIR record-vs-tuple product duplication, if any temporary compatibility layer remains
- legacy interpreter / RocValue / manual REPL rendering paths

At that point the intended architecture is finally real instead of partially simulated.

## Testing And Validation Plan

This migration touches invariants, so the tests should be organized around those invariants instead of just around individual functions.

### Canonical MIR Ordering Tests

Add or update tests to prove that:

- record literals lower to canonical alphabetical order regardless of source order
- record updates lower to full canonical field arrays
- record destructures lower to full canonical field-pattern arrays with wildcards in omitted positions
- tuple literals/destructures remain positional

### Layout Ordering Tests

Add or update tests to prove that:

- record layout sorting is `alignment desc, canonical_index asc`
- tuple layout sorting is `alignment desc, canonical_index asc`
- multi-field tag payload layout sorting is `alignment desc, canonical_index asc`
- equal-alignment ties do not depend on sort stability

### Translation Tests

Add or update tests to prove that:

- MIR struct construction reorders canonical fields into layout order correctly
- MIR `struct_access { field_idx }` lowers to the correct LIR slot
- record and tuple destructures both lower through the same index-based translation rule
- multi-payload tag constructors and tag patterns use the same structural translation rule

### Legacy-Removal Tests

As the legacy paths are deleted:

- replace name-based layout tests with original-index-based tests
- replace REPL/manual formatting tests with `Str.inspect`-driven expectations
- remove tests whose only purpose was to keep layout-name helpers alive

## Expected Benefits

This architecture has several concrete benefits:

- layout no longer stores invalid-by-construction module-local names
- record lowering stops doing late name joins
- the compiler gets one structural-product model instead of separate record and tuple concepts in MIR
- tag payloads become first-class structural products in MIR
- alignment sorting applies uniformly to records, tuples, and tag union payloads
- MIR becomes clearer: "field index" always means semantic position, not field name and not layout slot

That last point is especially important. By sorting all MIR structs by alignment during LIR lowering, we get the memory-layout win not only for records, but also for tuples and tag union payloads. That is both cleaner architecturally and better for generated layouts.

## Recommended Rollout Order

If we want to minimize risk and keep diffs reviewable, the recommended order is:

1. Remove layout-name dependence from layout construction and LIR lowering.
2. Delete legacy runtime/display users of layout names.
3. Make record MIR canonical and indexed while still keeping `record` / `tuple` distinct.
4. Introduce MIR `struct_` / `struct_access` / `struct_destructure`.
5. Move tag payloads onto MIR structs.
6. Delete the old name-bearing scaffolding.

That order avoids doing a giant "rename everything to struct" change before the real invariants are in place.
