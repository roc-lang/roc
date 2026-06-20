# Signals Migration - Next Steps

`DESIGN.md` is the target design. This file is the live work queue for the
remaining gaps only; completed phase notes and retired findings belong in git
history.

## Current State (2026-06-21)

- The public app surface uses retained `Elem`/`Signal`/`Html`/`Ui` values. Apps
  no longer import `NodeValue`/`Node` or the removed
  `Reactive`/`Graph`/`UiRuntime` surface.
- Host identity is dense and scope-relative. Only identity-bearing sites advance
  ordinals: `Ui.state`, `Ui.when`, and `Ui.each`.
- The host owns source ids, retained descriptor streams, active scopes, keyed
  rows, state values, dirty queues, and simulated DOM patches.
- Active source state, signal sink caches, and keyed row key/item slots still use
  `HostValueCell`: a bridge `NodeValue` plus the retained equality thunk for the
  edge.
- `SignalExpr.Ref` binds to explicit host source node ids. Non-`Ref`
  `SignalExpr` variants carry explicit signal tokens, and the host consumes
  those tokens into shared retained `HostSignalRecord`s within the active
  descriptor stream.
- Shared signal records retain their transform/equality thunks and cache their
  current bridge output. Dirty evaluation prunes parent transforms when a child
  output is unchanged.
- Dirty leaf and structural checks route by source node id through
  `active_text_signal_routes`, `active_bool_signal_routes`, and
  `active_structural_signal_routes`.
- Non-structural dirty updates patch only matching text/value/checked/disabled
  sinks. Structural dirty updates patch DOM incrementally, reuse/move surviving
  keyed row DOM, and patch event bindings by changed slot.
- Changed `when`/`each` outputs still rebuild an active descriptor stream before
  patching. Unchanged keyed row items can reuse their retained row descriptor
  subtree unless a descendant structural descriptor depends on the dirty source.

## Remaining Design Gaps

1. **Confined erasure is still incomplete.** Platform internals still serialize
   values through `NodeValue` and still contain crash-on-decode-mismatch wrappers
   in `Signal.roc` and `Ui.roc`. The design requires opaque typed value cells
   owned by the exact generated thunks for that typed edge.
2. **Signal records are not yet the final graph node table.** The host now shares
   derived records by explicit token, but dirty propagation is still route-driven
   and recursively evaluates records from each sink/structural site. The design
   requires explicit input ids, dependent edges, ranks, cached typed values, and
   one recompute per dirty batch.
3. **Structural no-rebuild is still incomplete.** Current structural updates are
   DOM patches rather than DOM resets, but changed structural outputs still
   rebuild descriptor streams. Structural sites need host-owned records so dirty
   work updates only the affected branch/row scopes.
4. **Effects and subscriptions are unimplemented.** `Signal.from_task`,
   `Signal.interval`, `Ui.on_change`, and `Ui.on_cleanup` should wait until the
   typed source/update path exists.
5. **Optimized benchmark validation is blocked.** `zig build run-signals-bench`
   still hits roc-lang/roc#9717 in the optimized ops-dashboard build. Keep the
   benchmark case visible; do not skip it or silently downgrade it to dev mode.

## Next Green Slices

Take one slice at a time and commit each green result.

1. **Typed value-cell vertical path.** Pick one real path:
   `Ui.state` source value -> one `Signal.map`/`map2` -> one signal-backed sink.
   Replace the bridge `NodeValue` on that path with an opaque host-owned value
   plus the edge's retained equality thunk.
2. **Dirty pass over shared signal records.** Add explicit dependent edges/ranks
   for shared records so one source update recomputes each affected record once,
   then fans out to sinks/structural sites from the record's equality result.
3. **Typed keyed-row data.** Move `Ui.each` key and item storage off the bridge
   and onto typed value cells. Row reuse must keep using explicit key and item
   equality thunks.
4. **Structural site record.** Convert one `Ui.when` or `Ui.each` site so a dirty
   source updates that site without rebuilding the whole active descriptor
   stream.
5. **Remove crash-on-decode wrappers.** Do this only after the typed value path
   has replaced the bridge for the affected API. A type mismatch must be
   structurally impossible.

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
