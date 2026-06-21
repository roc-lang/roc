# Signals Migration - Next Steps

`DESIGN.md` is the target design. This file is the live work queue for the
remaining gaps only; completed phase notes and retired findings belong in git
history.

## Current State (2026-06-21)

- The public app surface uses retained `Elem`/`Signal`/`Html`/`Ui` values. Apps
  no longer import the removed `Reactive`/`Graph`/`UiRuntime` surface.
- Host identity is dense and scope-relative. Only identity-bearing sites advance
  ordinals: `Ui.state`, `Ui.component`, `Ui.when`, and `Ui.each`.
- The host owns source ids, retained descriptor streams, active scopes, keyed
  rows, state values, dirty queues, and simulated DOM patches.
- Active source state, signal sink caches, sink reads, event payloads,
  structural conditions, and keyed row key/item slots use opaque `HostValue`
  handles with retained typed equality/drop/read thunks owned by the exact
  descriptor edge.
- Debug and safe builds attach carrier type tags to every `HostValue` stored in
  the host registry and assert those tags at typed read/take boundaries. Release
  builds compile out the registry tag field and assertions.
- `SignalExpr.Ref` binds to explicit host source node ids. Non-`Ref`
  `SignalExpr` variants carry explicit signal tokens, and the host consumes
  those tokens into shared retained `HostSignalRecord`s within the active
  descriptor stream.
- Shared signal records retain their transform/equality/drop thunks and cache
  their current opaque typed output. Dirty evaluation memoizes each record once
  per dirty batch and prunes parent transforms when a child output is unchanged.
- The host rebuilds an active signal graph from retained signal records after
  each descriptor stream. Dirty source node ids route to source signal records,
  then walk explicit dependent edges in rank order. Leaf and structural sinks
  fan out from the changed terminal signal record ids.
- Non-structural dirty updates patch only matching text/value/checked/disabled
  sinks. Structural dirty updates patch DOM incrementally, reuse/move surviving
  keyed row DOM, and patch event bindings by changed slot.
- Changed `Ui.when` and `Ui.each` outputs retain the host-owned data needed to
  collect only affected branch/row scopes. Dirty structural work splices those
  replacement descriptors into retained active descriptors without re-entering
  the root `Elem` or materializing a full next active descriptor stream.
  Preserved-order `Ui.each` churn splices only removed/changed/new row scopes
  before applying one DOM patch to the affected each site; reorder churn uses an
  explicit whole-site replacement because row order actually changed.
- `Ui.component` introduces reusable local scopes for helper-owned state. The
  component app proves multiple stateful instances keep separate construction
  identities across keyed row movement and dispose state when the owning row
  scope is removed.

## Remaining Design Gaps

These map one-to-one to the Definition of Done in `DESIGN.md`. The platform is
"done" when all are closed and the success-metric specs are green.

1. **Effects and subscriptions are unimplemented (DoD 4).** `Signal.from_task`,
   `Signal.interval`, `Ui.on_change`, and `Ui.on_cleanup` need a host-owned
   lifecycle tied to explicit scope ownership and cleanup.

## Next Green Slices

Take one slice at a time and commit each green result.

1. **Effect lifecycle scaffold (closes gap 4).** Add the smallest host-owned
   subscription lifecycle for one cleanup-safe effect source.

### Capability apps (add alongside the slices, smallest proof only)

Each app maps to exactly one Definition-of-Done capability. Keep them minimal and
assertion-tight; no catalog fixtures.

- **Async / effects app** — `Signal.from_task` with injected fake results,
  `[Loading, Done, Failed]` rendering, `Ui.on_change` firing a request, scope
  disposal cancelling in-flight work via `Ui.on_cleanup` (proves gap 4).

Avoid broad fixture catalogs, extra DOM polish, new metric counters, or coverage
around already-solved identity behavior unless it is the smallest proof for one
of these gaps.

## Green Gates

Use the smallest gate that proves the slice, then run the full signal gate before
committing code changes.

- Focused Zig host work:
  `zig test test/signals/platform/host.zig`
- Platform Roc or ABI changes:
  `./zig-out/bin/roc check test/signals/apps/checkout_wizard.roc`
  `./zig-out/bin/roc check test/signals/apps/component_composition.roc`
  `./zig-out/bin/roc check test/signals/apps/identity_stress.roc`
  `./zig-out/bin/roc check test/signals/apps/kanban_board.roc`
  `./zig-out/bin/roc check test/signals/apps/ops_dashboard.roc`
- End-to-end signal specs:
  `zig build run-test-signals --summary failures`
- Optimized benchmark gate:
  `zig build run-signals-bench`

For doc-only updates, `git diff --check` is enough.
