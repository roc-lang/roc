# Ownership Migration Inventory

## Status

Phase 3 semantic centralization is complete enough to begin Phase 4 correctness
work.

The previously open items have been resolved into one of two categories:

1. **Centralized as explicit earlier facts**
   - RC helper planning is now consumed through [`src/layout/store.zig`](/Users/rtfeldman/.codex/worktrees/1d55/roc/src/layout/store.zig).
   - Backend/interpreter code no longer constructs `RcHelperResolver` objects.
   - Shared primitive helper ABI for lists and boxes is now published through
     [`src/layout/store.zig`](/Users/rtfeldman/.codex/worktrees/1d55/roc/src/layout/store.zig).
   - Wasm list teardown now consumes explicit `RcListPlan` child facts rather
     than recovering child layout from the list layout.

2. **Explicitly accepted primitive-internal behavior**
   - Runtime/helper implementations still contain their own internal loops and
     memory-manipulation code for primitive operations such as list payload
     teardown.
   - That behavior is intentionally classified as builtin/runtime-internal,
     which is allowed by the governing invariant.

## What Is No Longer Considered A Phase-3 Violation

- A backend calling into a builtin/runtime helper implementation.
- A builtin/runtime helper implementation using published list/box ABI facts
  from `layout.Store`.
- A builtin/runtime helper implementation iterating over primitive payloads
  using explicit helper-plan data.

## Remaining Work Happens In Phase 4

The remaining work is no longer about missing ownership facts. It is about:

- compiling again
- enabling and satisfying the boundary checks
- deleting any remaining forbidden non-builtin RC behavior that still surfaces
  under real builds/tests
- proving the invariant with passing correctness suites
