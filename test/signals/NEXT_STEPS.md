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
- Active signal records carry explicit dense ids while they are in the active
  graph, and the active graph owns a retained reference to each record while it
  points at it. Record→id lookup is O(1), and `active_graph_records_rebuilt`
  counts graph records reconstructed by full or structural rebuilds.
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
  **Caveat (foundation gap):** the DOM patch is local, but the signal-graph
  maintenance underneath it is not — every structural splice still calls a full
  clear-and-rebuild of the active signal graph. See *Foundation Gaps* below.
- `Ui.component` introduces reusable local scopes for helper-owned state. The
  component app proves multiple stateful instances keep separate construction
  identities across keyed row movement and dispose state when the owning row
  scope is removed.
- `Signal.fake_task`/`Signal.from_task`, `Signal.fold_task`, `Signal.start_str`,
  `Signal.interval`, `Ui.on_change`, and `Ui.on_cleanup` now have host-owned
  lifecycle support.
  The async effects app proves fake task result injection, `[Loading, Done,
  Failed]` rendering through a fold, pending request cancellation when the
  owning scope is disposed, deterministic interval ticks, active interval
  cancellation when the owning scope is disposed, and cleanup descriptor
  execution.
- The Roc allocation ledger stores an explicit header-owned ledger index and
  updates moved indexes after `swapRemove`, so allocation free/realloc removal is
  O(1). Runtime metrics expose `allocs_this_event` and
  `deallocs_this_event` through specs and the benchmark CSV.

## Foundation Gaps (the current frontier)

Functional correctness is green on the small fixtures. The next frontier is
data-structure quality, telemetry, and benchmark coverage — the work that makes
the scaling claim true *in the code*, not just on paper. Each gap below is a
violation of the Complexity Discipline budget in `DESIGN.md` and is invisible to
the current metrics, so each must ship with the counter and spec assertion that
would have caught it.

Severity order (highest first). Take one slice at a time and commit each green
result. Each slice lands its fix *and* the assertion that locks it in.

### G-F1 — Incremental signal-graph maintenance (Critical)

- **Problem:** every structural change clears and rebuilds the entire active
  signal graph, so reconstruction is O(total active graph records) even when the
  structural splice touches one scope.
- **Fix:** on a splice, edit only the affected scope's records and patch
  adjacency/rank in place instead of clear-and-rebuild. Initial ingestion may be
  O(N); nothing after it may be.
- **Counter + assertion:** the generated large-N each app asserts
  `expect_metric_delta active_graph_records_rebuilt 0` on a single-row item
  change and a bound proportional to moved rows on reorder.

### G-F2 — O(1) descriptor lookup (Critical)

- **Problem:** `findElementDesc`/`findTextNodeDesc`/`findSignalTextNodeDesc`/
  `streamHasTextField`/`streamHasBoolField` are linear scans over the stream and
  are called inside loops over all render nodes, yielding O(render_nodes²)
  structural patch paths; `applySplicedStructuralNodeDescriptorTarget` also scans
  the whole active stream gated only by a `seen[]` bit.
- **Fix:** index descriptors by `elem_id` (dense array keyed by elem id);
  restrict structural patching to the spliced subtree's nodes, not the whole
  stream.
- **Counter + assertion:** add `stream_nodes_scanned`; assert it is bounded by
  the changed set on a single-row update in the large-N app.

### G-F3 — Key-hash-indexed `Ui.each` diff (High)

- **Problem:** keyed diff matches by linear `is_eq` scan (O(L²)), and the
  duplicate-key check is a second O(L²) pass; the implemented `Ui.each` API does
  not even carry the `key.hash` constraint `DESIGN.md` specifies.
- **Fix:** add the `key.hash`/`Hasher` constraint to the `Ui.each` API (an API
  addition, not a host workaround) and build a `HashMap(key → row scope id)` per
  each site for both the diff and the duplicate check.
- **Counter + assertion:** add `each_key_compares`; assert it tracks L (not L²)
  under churn in the large-N app.

### G-F4 — Moves-only reorder (High)

- **Problem:** a pure permutation of surviving rows re-collects and re-splices
  every row at the site (whole-site replacement) instead of emitting only the
  displaced moves.
- **Fix:** compute a longest-stable-subsequence keep set and emit DOM moves only
  for displaced rows; do not re-collect descriptors or rebuild the site graph for
  a permutation. Whole-site replacement stays reserved for genuine set changes
  that cannot be expressed as moves-plus-local-splices, named explicitly and
  asserted, never reached by fallthrough.
- **Counter + assertion:** the large-N reorder spec asserts `create_element 0`,
  bounded `append_child` proportional to displaced rows, and bounded
  `active_graph_records_rebuilt`.

### G-F5 — Telemetry and benchmark-gate coverage (High)

- **Problem:** the current counters measure emitted patches and recomputed nodes
  but not scanned nodes or key compares;
  the benchmark gate (`run-signals-bench`) excludes `identity_stress`,
  `component_composition`, and `async_effects` — the apps that exercise structural
  churn, reorder, and async lifecycle.
- **Fix:** land the remaining new counters (G-F2, G-F3) and the CSV columns for
  them; flip `bench = true` for the three excluded apps with explicit
  regression thresholds. Keep timing as corroborating CSV evidence only — never
  a gate, since a timing-only check can pass while real work grows.
- **Spec-vs-CSV split:** scaling invariants go in `expect_metric_delta`
  assertions; timing/aggregate evidence stays CSV-only (see `DESIGN.md` →
  Metrics).

### G-F6 — Foundation coverage apps and assertions (Medium, lands alongside the fixes)

- **Generated large-N each app** (the one place large N is allowed, because it is
  generated systematically, not a handwritten catalog). N is a build parameter;
  specs assert the budget for single-row update / append / remove / filter /
  reorder using the new counters. This is the primary proof for G-F1..G-F4 and
  the allocation-flatness part of the success metrics.
- **`kanban_board`** reorder and filter steps gain `mark_metrics` /
  `expect_metric_delta` blocks bounding work.
- **`async_effects`** gains a `retained_alloc_delta` / closure-balance assertion
  after the open/close cancel cycle to prove no lifecycle leak.
- **`ops_dashboard`** gains a real-event fanout assertion (not just the synthetic
  no-op) bounding `nodes_recomputed` / `derived_calls_into_roc`.
- **Long-session leak experiment:** reuse one `HostEnv` across many replayed
  events and assert the live `allocs − deallocs` gauge is flat after warmup
  (`retained_alloc_delta` as currently computed cannot prove this — it resets per
  iteration).

## Green Gates

Use the smallest gate that proves the slice, then run the full signal gate before
committing code changes. Each foundation slice (G-F1..G-F6) must land its fix and
its locking assertion together; a fix without the counter/spec that would catch a
regression is not done.

- Focused Zig host work:
  `zig test test/signals/platform/host.zig`
- Platform Roc or ABI changes:
  `./zig-out/bin/roc check test/signals/apps/checkout_wizard.roc`
  `./zig-out/bin/roc check test/signals/apps/component_composition.roc`
  `./zig-out/bin/roc check test/signals/apps/async_effects.roc`
  `./zig-out/bin/roc check test/signals/apps/identity_stress.roc`
  `./zig-out/bin/roc check test/signals/apps/kanban_board.roc`
  `./zig-out/bin/roc check test/signals/apps/ops_dashboard.roc`
- End-to-end signal specs:
  `zig build run-test-signals --summary failures`
- Optimized benchmark gate:
  `zig build run-signals-bench`

For doc-only updates, `git diff --check` is enough.
