# Signals Migration - Next Steps

`DESIGN.md` is the target design. This file is the live work queue for the
remaining gaps only; completed phase notes and retired findings belong in git
history.

## Current State (2026-06-21)

- The public app surface uses retained `Elem`/`Signal`/`Html`/`Ui` values. Apps
  no longer import the removed `Reactive`/`Graph`/`UiRuntime` surface.
- Host identity is dense and scope-relative. Only identity-bearing sites advance
  ordinals: `Ui.state`, `Ui.when`, and `Ui.each`.
- The host owns source ids, retained descriptor streams, active scopes, keyed
  rows, state values, dirty queues, and simulated DOM patches.
- Active source state, signal sink caches, sink reads, event payloads,
  structural conditions, and keyed row key/item slots use opaque `HostValue`
  handles with retained typed equality/drop/read thunks owned by the exact
  descriptor edge.
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
- Changed `when`/`each` outputs still rebuild an active descriptor stream before
  patching. Unchanged keyed row items can reuse their retained row descriptor
  subtree unless a descendant structural descriptor depends on the dirty source.

## Remaining Design Gaps

1. **Structural no-rebuild is still incomplete.** Current structural updates are
   DOM patches rather than DOM resets, but changed structural outputs still
   rebuild descriptor streams. Structural sites need host-owned records so dirty
   work updates only the affected branch/row scopes.
2. **Effects and subscriptions are unimplemented.** `Signal.from_task`,
   `Signal.interval`, `Ui.on_change`, and `Ui.on_cleanup` should wait until
   structural site ownership is stable enough to avoid cleanup tied to whole
   stream rebuilds.
3. **Optimized benchmark validation is blocked.** `zig build run-signals-bench`
   still hits roc-lang/roc#9717 in the optimized ops-dashboard build. Keep the
   benchmark case visible; do not skip it or silently downgrade it to dev mode.

## Next Green Slices

Take one slice at a time and commit each green result.

1. **Structural site record.** Convert one `Ui.when` or `Ui.each` site so a dirty
   source updates that site without rebuilding the whole active descriptor
   stream.
2. **Effect lifecycle scaffold.** Add the smallest host-owned subscription
   lifecycle needed for one cleanup-safe effect source after structural updates
   no longer rebuild the whole active descriptor stream.

Avoid broad fixture catalogs, extra DOM polish, new metric counters, or coverage
around already-solved identity behavior unless it is the smallest proof for one
of these blockers.

## Green Gates

Use the smallest gate that proves the slice, then run the full signal gate before
committing code changes.

- Focused Zig host work:
  `zig test test/signals/platform/host.zig`
- Platform Roc or ABI changes:
  `./zig-out/bin/roc check test/signals/apps/checkout_wizard.roc`
  `./zig-out/bin/roc check test/signals/apps/identity_stress.roc`
  `./zig-out/bin/roc check test/signals/apps/kanban_board.roc`
  `./zig-out/bin/roc check test/signals/apps/ops_dashboard.roc`
- End-to-end signal specs:
  `zig build run-test-signals --summary failures`
- Optimized benchmark gate:
  `zig build run-signals-bench`

For doc-only updates, `git diff --check` is enough.
