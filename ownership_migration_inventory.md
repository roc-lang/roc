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

1. `for_list` element materialization
   Files:
   - [`src/eval/interpreter.zig:1374`](/Users/rtfeldman/.codex/worktrees/1d55/roc/src/eval/interpreter.zig:1374)
   - [`src/eval/interpreter.zig:1375`](/Users/rtfeldman/.codex/worktrees/1d55/roc/src/eval/interpreter.zig:1375)
   Current behavior:
   - The interpreter materializes each loop element and then decides whether to `incref` it based on layout refcounted-ness.
   Missing earlier fact:
   - LIR does not yet explicitly state whether `for_list` element materialization yields a borrowed alias or a fresh owned local.
   LIR change needed:
   - Give `for_list` an explicit result/ownership contract for the element local, or lower `for_list` away into explicit assign + `incref`/`decref` statements before the interpreter/backend stage.
   Replacement:
   - Explicit `incref` before loop-body consumption when the element local becomes owned.

2. `coerceValueIntoBox`
   Files:
   - [`src/eval/interpreter.zig:5019`](/Users/rtfeldman/.codex/worktrees/1d55/roc/src/eval/interpreter.zig:5019)
   - [`src/eval/interpreter.zig:5020`](/Users/rtfeldman/.codex/worktrees/1d55/roc/src/eval/interpreter.zig:5020)
   Current behavior:
   - The interpreter allocates box storage, copies the payload, and then conditionally `incref`s copied refcounted children.
   Missing earlier fact:
   - LIR does not yet represent “copy into freshly owned box payload retains children” as explicit ownership semantics.
   LIR change needed:
   - Introduce an explicit box-pack ownership contract, either as:
     - a dedicated `assign_box_pack`-style stmt with explicit child-retain semantics, or
     - explicit `incref` statements emitted before/after the copy according to a low-level op ownership contract.
   Replacement:
   - No interpreter-side retain; only explicit LIR RC.

3. `coerceStructValue`
   Files:
   - [`src/eval/interpreter.zig:5121`](/Users/rtfeldman/.codex/worktrees/1d55/roc/src/eval/interpreter.zig:5121)
   - [`src/eval/interpreter.zig:5122`](/Users/rtfeldman/.codex/worktrees/1d55/roc/src/eval/interpreter.zig:5122)
   Current behavior:
   - The interpreter synthesizes a new struct value and then conditionally retains its children.
   Missing earlier fact:
   - LIR does not represent aggregate rebuilds as explicit ownership-taking constructions.
   LIR change needed:
   - `assign_struct` must have enough ownership semantics for `RcInsert` to emit explicit retains when the constructed aggregate becomes fresh owner of refcounted fields.
   Replacement:
   - Fresh aggregate construction followed by explicit LIR `incref` statements as required.

4. `coerceTagUnionValue`
   Files:
   - [`src/eval/interpreter.zig:5176`](/Users/rtfeldman/.codex/worktrees/1d55/roc/src/eval/interpreter.zig:5176)
   - [`src/eval/interpreter.zig:5177`](/Users/rtfeldman/.codex/worktrees/1d55/roc/src/eval/interpreter.zig:5177)
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

5. List element extraction builtins in `generateLowLevel`
   Files:
   - [`src/backend/dev/LirCodeGen.zig:1746`](/Users/rtfeldman/.codex/worktrees/1d55/roc/src/backend/dev/LirCodeGen.zig:1746)
   - [`src/backend/dev/LirCodeGen.zig:4167`](/Users/rtfeldman/.codex/worktrees/1d55/roc/src/backend/dev/LirCodeGen.zig:4167)
   - [`src/backend/dev/LirCodeGen.zig:4240`](/Users/rtfeldman/.codex/worktrees/1d55/roc/src/backend/dev/LirCodeGen.zig:4240)
   Current behavior:
   - Dev backend copies list elements into stack temporaries and conditionally `incref`s them.
   Missing earlier fact:
   - LIR does not say whether `list_get_unsafe`, `list_first`, and `list_last` produce borrowed aliases or fresh owned values.
   LIR change needed:
   - `LowLevel.procResultSemantics()` / arg ownership metadata must distinguish “result aliases list element” vs “result is a copied owned value”.
   Replacement:
   - Explicit RC emitted from LIR for element extraction.

6. `Box.box` / `Box.unbox` builtin lowering
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

7. List construction and list-manipulation builtins
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

8. Wasm `for_list` and list element extraction
   Files:
   - [`src/backend/wasm/WasmCodeGen.zig:5073`](/Users/rtfeldman/.codex/worktrees/1d55/roc/src/backend/wasm/WasmCodeGen.zig:5073)
   - [`src/backend/wasm/WasmCodeGen.zig:5099`](/Users/rtfeldman/.codex/worktrees/1d55/roc/src/backend/wasm/WasmCodeGen.zig:5099)
   - [`src/backend/wasm/WasmCodeGen.zig:5107`](/Users/rtfeldman/.codex/worktrees/1d55/roc/src/backend/wasm/WasmCodeGen.zig:5107)
   - [`src/backend/wasm/WasmCodeGen.zig:6859`](/Users/rtfeldman/.codex/worktrees/1d55/roc/src/backend/wasm/WasmCodeGen.zig:6859)
   - [`src/backend/wasm/WasmCodeGen.zig:6893`](/Users/rtfeldman/.codex/worktrees/1d55/roc/src/backend/wasm/WasmCodeGen.zig:6893)
   - [`src/backend/wasm/WasmCodeGen.zig:6901`](/Users/rtfeldman/.codex/worktrees/1d55/roc/src/backend/wasm/WasmCodeGen.zig:6901)
   - [`src/backend/wasm/WasmCodeGen.zig:6932`](/Users/rtfeldman/.codex/worktrees/1d55/roc/src/backend/wasm/WasmCodeGen.zig:6932)
   - [`src/backend/wasm/WasmCodeGen.zig:6952`](/Users/rtfeldman/.codex/worktrees/1d55/roc/src/backend/wasm/WasmCodeGen.zig:6952)
   - [`src/backend/wasm/WasmCodeGen.zig:6960`](/Users/rtfeldman/.codex/worktrees/1d55/roc/src/backend/wasm/WasmCodeGen.zig:6960)
   - [`src/backend/wasm/WasmCodeGen.zig:6971`](/Users/rtfeldman/.codex/worktrees/1d55/roc/src/backend/wasm/WasmCodeGen.zig:6971)
   - [`src/backend/wasm/WasmCodeGen.zig:7000`](/Users/rtfeldman/.codex/worktrees/1d55/roc/src/backend/wasm/WasmCodeGen.zig:7000)
   - [`src/backend/wasm/WasmCodeGen.zig:7008`](/Users/rtfeldman/.codex/worktrees/1d55/roc/src/backend/wasm/WasmCodeGen.zig:7008)
   Current behavior:
   - Wasm copies list elements and performs builtin/internal retains.
   Missing earlier fact:
   - Same gap as dev/interpreter: element extraction semantics are not fully explicit in LIR.
   LIR change needed:
   - Same as item 5, but the wasm backend also needs a clear distinction between pointer-result aliasing and copied owned results.

9. Wasm `Box.box` / `Box.unbox`
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

10. Wasm list/tag/box RC helper generation
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

1. `for_list` is under-specified from an ownership perspective.
   Required change:
   - either eliminate it before backends/interpreter, or give it explicit element ownership semantics

2. Aggregate rebuild operations (`assign_struct`, `assign_tag`, box pack/unpack) are under-specified.
   Required change:
   - explicit “fresh aggregate takes ownership of children” semantics in LIR

3. Some low-level primitives still rely on backend-supplied ownership facts.
   Required change:
   - strengthen [`src/base/LowLevel.zig`](/Users/rtfeldman/.codex/worktrees/1d55/roc/src/base/LowLevel.zig) contracts so helper calls do not require backend ownership inference

4. Backend RC helper generation is still partly layout-driven.
   Required change:
   - decide whether helper plans become earlier explicit artifacts or remain isolated builtin/runtime internals with no leakage into ordinary lowering

## Recommended Next Implementation Order

1. Finish LIR semantics for aggregate rebuilds and box pack/unpack.
2. Make `for_list` ownership explicit or lower it away before runtime backends.
3. Extend `LowLevel` ownership/result metadata for list element extraction and list-manipulation primitives.
4. After the semantics are expressible in LIR, remove the corresponding interpreter ordinary-path retains first.
5. Then remove dev backend builtin/internal ownership decisions where they are no longer primitive-internal by design.
6. Then remove wasm counterparts.

