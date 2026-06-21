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
  the root `Elem`. Unchanged keyed row items can reuse their retained row
  descriptor subtree unless a descendant structural descriptor depends on the
  dirty source.

## Remaining Design Gaps

These map one-to-one to the Definition of Done in `DESIGN.md`. The platform is
"done" when all are closed and the success-metric specs are green.

1. **`Ui.each` scaling is not yet proven linear (DoD 1, highest risk).** Dirty
   `Ui.when` and `Ui.each` work no longer rebuilds from the root `Elem`, but the
   host still materializes an updated active descriptor stream and applies the
   structural patch against that stream. This is purely a **host-side patch
   locality** problem: invoking Roc to *build* a new row is already solved (the
   host calls the retained `row` builder closure directly — no entrypoint, see
   `DESIGN.md` "Retained closures"). What remains is splicing the returned row
   sub-tree into only the affected scope rather than rebuilding the active
   descriptor stream. Until that lands, list churn is not provably O(changed
   rows). This is the headline scaling property and the most important gap.
2. **`Ui.component` is unimplemented (DoD 2).** Named scopes for local state
   across helper functions returning `Elem` do not exist yet. Needed to prove
   local state composes without identity leaks across multiple instantiations.
3. **Effects and subscriptions are unimplemented (DoD 4).** `Signal.from_task`,
   `Signal.interval`, `Ui.on_change`, and `Ui.on_cleanup` wait until structural
   site ownership (gap 1) is stable enough to avoid cleanup tied to whole-stream
   rebuilds.

## Next Green Slices

Take one slice at a time and commit each green result. Slices are ordered so the
highest-risk scaling property closes first and effects land last.

1. **Scope-local structural patch (closes gap 1).** Apply a dirty
   `Ui.when`/`Ui.each` replacement stream directly to the affected parent/row
   scopes instead of applying structural patches against a full updated active
   stream. Prove with a list-churn spec asserting `nodes_recomputed`,
   `patches_emitted`, and `rows_*` track changed rows, not list size.
2. **`Ui.component` named scopes (closes gap 2).** Add the scope primitive and a
   small composition app instantiating a stateful component more than once.
3. **Effect lifecycle scaffold (closes gap 4).** Once structural updates no
   longer rebuild the whole active descriptor stream, add the smallest host-owned
   subscription lifecycle for one cleanup-safe effect source.

### Capability apps (add alongside the slices, smallest proof only)

Each app maps to exactly one Definition-of-Done capability. Keep them minimal and
assertion-tight; no catalog fixtures.

- **Derived-graph / diamond app** — wide fan-in re-join; assert glitch-free
  single recompute and `is_eq` pruning suppressing unchanged branches under high
  update volume (proves scaling on derivation depth, supports gap 1's metrics).
- **Async / effects app** — `Signal.from_task` with injected fake results,
  `[Loading, Done, Failed]` rendering, `Ui.on_change` firing a request, scope
  disposal cancelling in-flight work via `Ui.on_cleanup` (proves gap 4).
- **Component-composition app** — a reusable stateful `Ui.component` instantiated
  multiple times and moved/disposed (proves gap 2).

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
  `./zig-out/bin/roc check test/signals/apps/identity_stress.roc`
  `./zig-out/bin/roc check test/signals/apps/kanban_board.roc`
  `./zig-out/bin/roc check test/signals/apps/ops_dashboard.roc`
- End-to-end signal specs:
  `zig build run-test-signals --summary failures`
- Optimized benchmark gate:
  `zig build run-signals-bench`

For doc-only updates, `git diff --check` is enough.
