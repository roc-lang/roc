# Signals Migration - Next Steps

`DESIGN.md` is the target design. This file is the live work queue for getting
there. It is not a changelog; completed phase logs and old benchmark tables live
in git history.

The previous phase plan is retired. Treat completed work as invariants to keep
green, not as areas to keep expanding.

## Current State (2026-06-21)

- The host owns the active runtime mechanics: source ids, retained descriptor
  streams, active scopes, keyed rows, state values, dirty queues, and simulated
  DOM patches.
- Apps use the retained `Elem`/`Signal`/`Html`/`Ui` surface. They no longer
  import `NodeValue` or `Node`, define app-local `NodeValue` encode/decode
  boilerplate, or use the removed `Reactive`/`Graph`/`UiRuntime` surface.
- Host identity is dense and scope-relative. Only identity-bearing descriptor
  sites advance ordinals: `Ui.state`, `Ui.when`, and `Ui.each`. Ordinary markup
  does not shift state identity.
- Active source state, signal caches, and keyed row key/item slots use
  `HostValueCell`: a current bridge value plus the retained equality thunk that
  owns that edge.
- Active descriptors bind `SignalExpr.Ref` to explicit host node ids and retain
  host-owned signal expression trees through `HostSignalBinding`.
- Non-`Ref` `SignalExpr` variants now carry explicit copied signal tokens from
  the typed construction site. Those tokens are retained by the host and are the
  identity data to use for shared derived records; do not infer sharing from
  pointer identity, callable identity, or expression shape.
- Host-bound `ConstValue`/`Map`/`Map2`/`Combine` expressions retain their own
  output caches. Dirty evaluation stops before parent transforms when a child
  output is unchanged.
- Dirty leaf and structural checks are routed by source node id through
  `active_text_signal_routes`, `active_bool_signal_routes`, and
  `active_structural_signal_routes`. The removed render-sink route tables should
  not come back.
- Non-structural dirty updates patch only matching text/value/checked/disabled
  sinks and prune unchanged outputs through retained equality thunks.
- Structural dirty updates first prune unchanged `when`/`each` outputs. Changed
  structural outputs still rebuild the active descriptor stream, but apply the
  result as a structural DOM patch: removed branch/row DOM is detached, surviving
  keyed row DOM is moved/reused, and event bindings are patched by changed slot.
- Keyed row scopes retain key and item value cells. Reused rows with unchanged
  item values copy their previous retained row descriptor subtree unless a
  descendant structural descriptor explicitly depends on the current dirty
  source.

## Remaining Gaps Against `DESIGN.md`

1. **Confined erasure is still incomplete.** The app surface no longer exposes
   `NodeValue`, but platform internals still serialize values through
   `NodeValue` and still contain crash-on-decode-mismatch wrappers in
   `Signal.roc` and `Ui.roc`. The design requires the host to store opaque typed
   values and call only the per-edge thunks generated for that typed edge.

2. **Derived nodes are not yet final host-owned records.** The host retains
   explicit signal tokens, bound signal expression trees, source-id routes, and
   per-expression output caches, but those caches are still per bound tree and
   still use the bridge value representation. The target is a host node table
   keyed by those explicit tokens that owns each retained transform, equality
   thunk, current value cell, input edges, and dependent edges.

3. **Structural no-rebuild is still blocked by typed retained data.** Current
   structural updates are no longer DOM resets, but changed `when`/`each` outputs
   still rebuild the active descriptor stream. The final path needs explicit
   host-owned structural node records so dirty structural work does not re-walk
   unrelated descriptors.

4. **Effects and subscriptions are unimplemented.** `Signal.from_task`,
   `Signal.interval`, `Ui.on_change`, and `Ui.on_cleanup` should wait until the
   typed source/update path is real. They reuse the same source update and dirty
   propagation machinery.

5. **Optimized benchmark validation is blocked.** `zig build run-signals-bench`
   still hits roc-lang/roc#9717 in the optimized ops-dashboard build. Keep the
   benchmark case visible; do not skip it or silently downgrade it to dev mode.

## Current Priority

The next slices should retire design blockers, not polish the current bridge.

1. **Replace the internal `NodeValue` bridge with typed value cells.** Each
   source, derived output, sink, key, and item edge should own one opaque value
   plus the exact equality thunk generated for that edge. The host must never
   choose a decoder or recover a type from bytes.
2. **Promote bound signal expressions into host-owned derived node records.**
   Retain transform and equality thunks once per live node, cache the node's
   current typed value, and propagate dirty changes through explicit dependent
   edges.
3. **Remove the structural descriptor rebuild.** Once structural sites own typed
   values, retained closures, and dependency data directly, update only the dirty
   branch/row scopes and emit DOM patches from those explicit records.
4. **Add effects/subscriptions after the typed core.** Effects are just sources
   whose updates come from host tasks, timers, or cleanup hooks.

Avoid broad fixture catalogs, extra DOM polish, new metric counters, or more
coverage around solved identity behavior unless they are the smallest way to
prove one of these blockers.

## Next Green Slices

Take one slice at a time and commit each green result.

1. **Typed value-cell vertical path.** Pick one path through the real platform
   boundary: `Ui.state` source value -> one `Signal.map` or `map2` -> one
   signal-backed sink. Replace the bridge value on that path with an opaque
   host-owned value cell and the edge's retained equality thunk. Keep ownership
   explicit: retain exactly once when the host stores a value, release exactly
   once when the owning node/scope is disposed.

2. **Derived node record.** Convert a host-bound derived expression into a real
   shared host node record keyed by its explicit signal token, with input node
   ids, retained transform/equality thunks, and a cached output value cell. Dirty
   evaluation should update that record and prune dependents from the record's
   equality result.

3. **Typed keyed-row data.** Move `Ui.each` key and item storage off the bridge
   and onto typed value cells. Row reuse must continue to use the explicit key
   equality thunk; unchanged row bodies must continue to use the explicit item
   equality thunk.

4. **Structural site record.** Convert one `Ui.when` or `Ui.each` structural
   site so a dirty source updates that site without rebuilding the whole active
   descriptor stream. Keep the current DOM patch semantics: detach removed DOM,
   move/reuse surviving keyed rows, and patch event binding slots by explicit id.

5. **Remove crash-on-decode wrappers.** Do this only after the typed value path
   has replaced the bridge for the affected API. A type mismatch must be
   structurally impossible, not converted into a different runtime crash.

## Retired Findings To Keep

- Roc-side identity threading was discarded. Roc builds a pure descriptor tree;
  the host walks it deterministically and owns mutation.
- Inline pure `Signal.state(...)` cannot mint stable unique identity. State must
  stay behind explicit closure binders such as `Ui.state(init, |state| body)`.
- Direct `Box({})` erasure is not viable. Roc keeps `Box(a)` typed, so the value
  carrier must be created at a real monomorphized typed edge or represented by a
  host-owned protocol generated for that edge.
- Nominal key/value examples should use nominal-over-single-tag-union wrappers,
  for example `TodoId := [Tid(U64)]`; do not revert examples to bare primitive
  nominal constructors.
- The old `Reactive`/`Graph`/`UiRuntime` surface is gone. Do not recreate string
  identity, string event keys, or the old full-rerender runtime.
- The active descriptor-stream rebuild is transitional. Fix correctness bugs in
  it, but do not expand it as the final architecture.

## Green Gates

Use the smallest gate that proves the slice, then run the full signal gate
before committing code changes.

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
