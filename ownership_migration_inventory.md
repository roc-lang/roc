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

### Backend builtin-internal list helper ABI still needs a shared source

1. Dev backend list construction and list-manipulation builtins
   Files:
   - [`src/backend/dev/LirCodeGen.zig:1498`](/Users/rtfeldman/.codex/worktrees/1d55/roc/src/backend/dev/LirCodeGen.zig:1498)
   - [`src/backend/dev/LirCodeGen.zig:1782`](/Users/rtfeldman/.codex/worktrees/1d55/roc/src/backend/dev/LirCodeGen.zig:1782)
   - [`src/backend/dev/LirCodeGen.zig:3697`](/Users/rtfeldman/.codex/worktrees/1d55/roc/src/backend/dev/LirCodeGen.zig:3697)
   - [`src/backend/dev/LirCodeGen.zig:3911`](/Users/rtfeldman/.codex/worktrees/1d55/roc/src/backend/dev/LirCodeGen.zig:3911)
   - [`src/backend/dev/LirCodeGen.zig:4140`](/Users/rtfeldman/.codex/worktrees/1d55/roc/src/backend/dev/LirCodeGen.zig:4140)
   - [`src/backend/dev/LirCodeGen.zig:8938`](/Users/rtfeldman/.codex/worktrees/1d55/roc/src/backend/dev/LirCodeGen.zig:8938)
   - [`src/layout/store.zig:1791`](/Users/rtfeldman/.codex/worktrees/1d55/roc/src/layout/store.zig:1791)
   Current behavior:
   - Dev backend now consumes store-published RC helper plans and uses a thin compatibility wrapper over the store-published list helper ABI.
   Missing earlier fact:
   - The primitive helper ABI still expects backend-supplied list metadata instead of consuming a shared earlier artifact.
   LIR change needed:
   - Decide whether builtin helper ABI facts remain primitive/runtime-internal, or whether an earlier shared helper-ABI artifact should be emitted before backend codegen.
   Replacement:
   - Long-term ideal is one shared helper-ABI artifact consumed mechanically by all backends, with ownership transitions still driven only by explicit surrounding LIR.

2. Wasm backend list helper ABI and list payload traversal
   Files:
   - [`src/backend/wasm/WasmCodeGen.zig:1303`](/Users/rtfeldman/.codex/worktrees/1d55/roc/src/backend/wasm/WasmCodeGen.zig:1303)
   - [`src/backend/wasm/WasmCodeGen.zig:7018`](/Users/rtfeldman/.codex/worktrees/1d55/roc/src/backend/wasm/WasmCodeGen.zig:7018)
   - [`src/backend/wasm/WasmCodeGen.zig:7219`](/Users/rtfeldman/.codex/worktrees/1d55/roc/src/backend/wasm/WasmCodeGen.zig:7219)
   - [`src/backend/wasm/WasmCodeGen.zig:7422`](/Users/rtfeldman/.codex/worktrees/1d55/roc/src/backend/wasm/WasmCodeGen.zig:7422)
   - [`src/backend/wasm/WasmCodeGen.zig:11017`](/Users/rtfeldman/.codex/worktrees/1d55/roc/src/backend/wasm/WasmCodeGen.zig:11017)
   - [`src/backend/wasm/WasmCodeGen.zig:1181`](/Users/rtfeldman/.codex/worktrees/1d55/roc/src/backend/wasm/WasmCodeGen.zig:1181)
   - [`src/layout/store.zig:1791`](/Users/rtfeldman/.codex/worktrees/1d55/roc/src/layout/store.zig:1791)
   Current behavior:
   - Wasm now consumes store-published RC helper plans and uses a thin compatibility wrapper over the store-published list helper ABI, but list payload teardown still performs backend-local element traversal inside builtin helper internals.
   Missing earlier fact:
   - LIR/low-level metadata still does not provide a shared helper-plan artifact or equivalent explicit child-traversal summary.
   LIR change needed:
   - Keep helper traversal fully builtin/runtime-internal, or move helper plans into an earlier shared artifact that wasm consumes mechanically.
   Replacement:
   - If helpers remain primitive internals, keep them isolated and allowlisted.
   - If long-term ideal is stricter single-sourcing, produce shared helper plans earlier and make all backends consume them mechanically.

### Shared design gaps inferred from the inventory

1. Aggregate rebuild operations (`assign_struct`, `assign_tag`, box pack/unpack) are now sourced from explicit LIR ownership, but backend helper ABIs still need cleanup.
   Required change:
   - delete the remaining backend/helper dependence on runtime child-RC metadata where explicit surrounding LIR should suffice

2. Some low-level primitives still rely on backend-supplied helper ABI facts.
   Required change:
   - strengthen [`src/base/LowLevel.zig`](/Users/rtfeldman/.codex/worktrees/1d55/roc/src/base/LowLevel.zig) contracts so helper calls do not require backend-local ABI derivation

3. Backend RC helper generation is still partly layout-driven.
   Required change:
   - decide whether helper plans become earlier explicit artifacts or remain isolated builtin/runtime internals with no leakage into ordinary lowering

## Recommended Next Implementation Order

1. Extend `LowLevel` ownership/result metadata for list element extraction and list-manipulation primitives.
2. Decide whether RC helper plans become an earlier shared artifact or remain strictly builtin/runtime-internal.
3. Then remove dev backend builtin/internal ownership decisions where they are no longer primitive-internal by design.
4. Then remove wasm counterparts.
