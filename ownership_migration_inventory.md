# Ownership Migration Inventory

## Purpose

This document is the Phase 3 migration table for making LIR the only
non-builtin source of ownership semantics.

It records every currently known non-builtin ownership decision site that still
needs to disappear, what earlier fact it is compensating for, and what the LIR
representation must eventually encode instead.

This inventory is based on the current enforcement-phase source tree after:

- explicit RC choke points were introduced
- builtin/internal RC paths were named explicitly
- the ownership-boundary checker was added and made to pass structurally

## Current Ground Truth in LIR

Existing LIR ownership representation already provides these building blocks:

- `ResultSemantics` in [`src/lir/LIR.zig`](/Users/rtfeldman/.codex/worktrees/1d55/roc/src/lir/LIR.zig)
- `ParamRefContract` in [`src/lir/LIR.zig`](/Users/rtfeldman/.codex/worktrees/1d55/roc/src/lir/LIR.zig)
- `ProcResultContract` in [`src/lir/LIR.zig`](/Users/rtfeldman/.codex/worktrees/1d55/roc/src/lir/LIR.zig)
- ownership propagation in [`src/lir/Ownership.zig`](/Users/rtfeldman/.codex/worktrees/1d55/roc/src/lir/Ownership.zig)
- RC insertion in [`src/lir/RcInsert.zig`](/Users/rtfeldman/.codex/worktrees/1d55/roc/src/lir/RcInsert.zig)
- primitive arg/result ownership metadata in [`src/base/LowLevel.zig`](/Users/rtfeldman/.codex/worktrees/1d55/roc/src/base/LowLevel.zig)

Those pieces are not yet sufficient to remove all backend/interpreter-side
ownership decisions. The remaining sites below show the specific gaps.

## Migration Table

### Interpreter forbidden ordinary-path ownership

1. `coerceValueIntoBox`
   Files:
   - [`src/eval/interpreter.zig:5054`](/Users/rtfeldman/.codex/worktrees/1d55/roc/src/eval/interpreter.zig:5054)
   Current behavior:
   - The interpreter allocates box storage, copies the payload, and then conditionally `incref`s copied refcounted children.
   Missing earlier fact:
   - LIR still routes some box-pack paths through the forbidden aggregate-coercion island instead of a fully explicit owned-transfer/fresh-owner path.
   LIR change needed:
   - Finish replacing aggregate coercion with explicit box-pack lowering that consumes owned payloads or retains borrowed payloads via `RcInsert`, using the central low-level contract.
   Replacement:
   - No interpreter-side retain; only explicit LIR RC.

2. `coerceStructValue`
   Files:
   - [`src/eval/interpreter.zig:5106`](/Users/rtfeldman/.codex/worktrees/1d55/roc/src/eval/interpreter.zig:5106)
   Current behavior:
   - The interpreter synthesizes a new struct value and then conditionally retains its children.
   Missing earlier fact:
   - LIR does not represent aggregate rebuilds as explicit ownership-taking constructions.
   LIR change needed:
   - `assign_struct` must have enough ownership semantics for `RcInsert` to emit explicit retains when the constructed aggregate becomes fresh owner of refcounted fields.
   Replacement:
   - Fresh aggregate construction followed by explicit LIR `incref` statements as required.

3. `coerceTagUnionValue`
   Files:
   - [`src/eval/interpreter.zig:5185`](/Users/rtfeldman/.codex/worktrees/1d55/roc/src/eval/interpreter.zig:5185)
   Current behavior:
   - The interpreter synthesizes a fresh tag payload and conditionally retains payload children.
   Missing earlier fact:
   - Same gap as `assign_struct`, but for `assign_tag`.
   LIR change needed:
   - `assign_tag` must fully encode payload ownership so `RcInsert` can emit child retains/drops explicitly.
   Replacement:
   - No interpreter retain logic in tag reconstruction.

### Dev backend builtin-internal ownership sites that should move into LIR

These are currently named as builtin/internal, which is acceptable during
enforcement, but long-term they should disappear unless they are truly part of
primitive helper semantics.

4. List element extraction builtins in `generateLowLevel`
   Files:
   - [`src/backend/dev/LirCodeGen.zig:1746`](/Users/rtfeldman/.codex/worktrees/1d55/roc/src/backend/dev/LirCodeGen.zig:1746)
   - [`src/backend/dev/LirCodeGen.zig:4167`](/Users/rtfeldman/.codex/worktrees/1d55/roc/src/backend/dev/LirCodeGen.zig:4167)
   - [`src/backend/dev/LirCodeGen.zig:4240`](/Users/rtfeldman/.codex/worktrees/1d55/roc/src/backend/dev/LirCodeGen.zig:4240)
   Current behavior:
   - Dev backend copies list elements into stack temporaries and conditionally `incref`s them.
   Missing earlier fact:
   - LIR still relies on backend/helper lowering details for some element-extraction/container-result cases even though the central low-level contract now classifies them directly.
   LIR change needed:
   - `LowLevel.procResultSemantics()` / arg ownership metadata must distinguish “result aliases list element” vs “result is a copied owned value”.
   Replacement:
   - Explicit RC emitted from LIR for element extraction.

5. `Box.box` / `Box.unbox` builtin lowering
   Files:
   - [`src/backend/dev/LirCodeGen.zig:3444`](/Users/rtfeldman/.codex/worktrees/1d55/roc/src/backend/dev/LirCodeGen.zig:3444)
   Current behavior:
   - Dev backend retains payload children while writing box contents.
   Missing earlier fact:
   - LIR does not encode box-pack / box-unpack ownership consequences explicitly.
   LIR change needed:
   - Dedicated ownership semantics for boxing/unboxing, ideally as explicit LIR statements or low-level contracts with emitted RC.
   Replacement:
   - Backend only copies bytes; LIR carries the retains.

6. List construction and list-manipulation builtins
   Files:
   - [`src/backend/dev/LirCodeGen.zig:1498`](/Users/rtfeldman/.codex/worktrees/1d55/roc/src/backend/dev/LirCodeGen.zig:1498)
   - [`src/backend/dev/LirCodeGen.zig:1782`](/Users/rtfeldman/.codex/worktrees/1d55/roc/src/backend/dev/LirCodeGen.zig:1782)
   - [`src/backend/dev/LirCodeGen.zig:1840`](/Users/rtfeldman/.codex/worktrees/1d55/roc/src/backend/dev/LirCodeGen.zig:1840)
   - [`src/backend/dev/LirCodeGen.zig:2935`](/Users/rtfeldman/.codex/worktrees/1d55/roc/src/backend/dev/LirCodeGen.zig:2935)
   - [`src/backend/dev/LirCodeGen.zig:3739`](/Users/rtfeldman/.codex/worktrees/1d55/roc/src/backend/dev/LirCodeGen.zig:3739)
   - [`src/backend/dev/LirCodeGen.zig:3849`](/Users/rtfeldman/.codex/worktrees/1d55/roc/src/backend/dev/LirCodeGen.zig:3849)
   - [`src/backend/dev/LirCodeGen.zig:3937`](/Users/rtfeldman/.codex/worktrees/1d55/roc/src/backend/dev/LirCodeGen.zig:3937)
   - [`src/backend/dev/LirCodeGen.zig:4069`](/Users/rtfeldman/.codex/worktrees/1d55/roc/src/backend/dev/LirCodeGen.zig:4069)
   - [`src/backend/dev/LirCodeGen.zig:4283`](/Users/rtfeldman/.codex/worktrees/1d55/roc/src/backend/dev/LirCodeGen.zig:4283)
   - [`src/backend/dev/LirCodeGen.zig:4416`](/Users/rtfeldman/.codex/worktrees/1d55/roc/src/backend/dev/LirCodeGen.zig:4416)
   - [`src/backend/dev/LirCodeGen.zig:4466`](/Users/rtfeldman/.codex/worktrees/1d55/roc/src/backend/dev/LirCodeGen.zig:4466)
   - [`src/backend/dev/LirCodeGen.zig:9118`](/Users/rtfeldman/.codex/worktrees/1d55/roc/src/backend/dev/LirCodeGen.zig:9118)
   Current behavior:
   - Dev backend still determines “elements refcounted?” to parameterize builtin helper behavior.
   Missing earlier fact:
   - The primitive helper ABI still expects backend-supplied ownership metadata derived from layouts.
   LIR change needed:
   - Decide whether these remain true builtin/runtime internals or whether the helper ABI should be normalized so ownership consequences are fully driven by explicit RC stmts plus data-only helper parameters.
   Replacement:
   - Long-term ideal is that helper calls take only data-shape parameters; ownership transitions happen in explicit surrounding LIR.

### Wasm backend builtin-internal ownership sites that should move into LIR

7. Wasm list element extraction / helper traversal
   Files:
   - [`src/backend/wasm/WasmCodeGen.zig:1192`](/Users/rtfeldman/.codex/worktrees/1d55/roc/src/backend/wasm/WasmCodeGen.zig:1192)
   - [`src/backend/wasm/WasmCodeGen.zig:1407`](/Users/rtfeldman/.codex/worktrees/1d55/roc/src/backend/wasm/WasmCodeGen.zig:1407)
   Current behavior:
   - Wasm helper generation still performs backend-local traversal/drop work for list payloads.
   Missing earlier fact:
   - LIR/low-level metadata still does not provide a precomputed helper-plan artifact or equivalent explicit child-traversal summary.
   LIR change needed:
   - Keep helper traversal fully builtin/runtime-internal, or move helper plans into an earlier shared artifact that wasm consumes mechanically.

8. Wasm `Box.box` / `Box.unbox`
   Files:
   - [`src/backend/wasm/WasmCodeGen.zig:7972`](/Users/rtfeldman/.codex/worktrees/1d55/roc/src/backend/wasm/WasmCodeGen.zig:7972)
   - [`src/backend/wasm/WasmCodeGen.zig:8036`](/Users/rtfeldman/.codex/worktrees/1d55/roc/src/backend/wasm/WasmCodeGen.zig:8036)
   - [`src/backend/wasm/WasmCodeGen.zig:8101`](/Users/rtfeldman/.codex/worktrees/1d55/roc/src/backend/wasm/WasmCodeGen.zig:8101)
   - [`src/backend/wasm/WasmCodeGen.zig:8115`](/Users/rtfeldman/.codex/worktrees/1d55/roc/src/backend/wasm/WasmCodeGen.zig:8115)
   - [`src/backend/wasm/WasmCodeGen.zig:8148`](/Users/rtfeldman/.codex/worktrees/1d55/roc/src/backend/wasm/WasmCodeGen.zig:8148)
   Current behavior:
   - Wasm boxing/unboxing still retains payload children in backend lowering.
   Missing earlier fact:
   - Same box-pack / box-unpack ownership gap as dev/interpreter.
   LIR change needed:
   - Same as item 6.

9. Wasm list/tag/box RC helper generation
    Files:
    - [`src/backend/wasm/WasmCodeGen.zig:1289`](/Users/rtfeldman/.codex/worktrees/1d55/roc/src/backend/wasm/WasmCodeGen.zig:1289)
    - [`src/backend/wasm/WasmCodeGen.zig:1477`](/Users/rtfeldman/.codex/worktrees/1d55/roc/src/backend/wasm/WasmCodeGen.zig:1477)
    Current behavior:
    - Wasm RC helper generation still derives child traversal/refcounted-ness from layouts at codegen time.
    Missing earlier fact:
    - LIR/low-level metadata does not yet fully describe helper child traversal needs as an earlier explicit artifact.
    LIR change needed:
    - Decide whether helper-generation plans remain backend-local runtime-helper internals or move into a shared earlier “RC helper plan” artifact produced before backend codegen.
    Replacement:
    - If helpers remain primitive internals, keep them isolated and allowlisted.
    - If long-term ideal is stricter single-sourcing, produce shared helper plans earlier and make backends consume them mechanically.

### Shared design gaps inferred from the inventory

1. Aggregate rebuild operations (`assign_struct`, `assign_tag`, box pack/unpack) are under-specified.
   Required change:
   - explicit “fresh aggregate takes ownership of children” semantics in LIR

2. Some low-level primitives still rely on backend-supplied ownership facts.
   Required change:
   - strengthen [`src/base/LowLevel.zig`](/Users/rtfeldman/.codex/worktrees/1d55/roc/src/base/LowLevel.zig) contracts so helper calls do not require backend ownership inference

3. Backend RC helper generation is still partly layout-driven.
   Required change:
   - decide whether helper plans become earlier explicit artifacts or remain isolated builtin/runtime internals with no leakage into ordinary lowering

## Recommended Next Implementation Order

1. Finish LIR semantics for aggregate rebuilds and box pack/unpack.
2. Extend `LowLevel` ownership/result metadata for list element extraction and list-manipulation primitives.
3. Decide whether RC helper plans become an earlier shared artifact or remain strictly builtin/runtime-internal.
4. After the semantics are expressible in LIR, remove the corresponding interpreter ordinary-path retains first.
5. Then remove dev backend builtin/internal ownership decisions where they are no longer primitive-internal by design.
6. Then remove wasm counterparts.
