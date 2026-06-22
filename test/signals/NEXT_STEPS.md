# Signals Migration - Next Steps

`DESIGN.md` is the target design. This file is the live work queue for the
remaining gaps only; completed phase notes and retired findings belong in git
history.

## Current State (2026-06-22)

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
  points at it. Active graph membership is ref-counted separately from normal
  signal ownership, record→id lookup is O(1), and `active_graph_records_rebuilt`
  counts graph records constructed by full ingestion or newly activated
  structural roots.
- The host builds the active signal graph from retained signal records during
  initial/full descriptor ingestion. Dirty structural splices release removed
  descriptor roots, retain inserted descriptor roots, and patch source routes
  plus dependent edges in place; after the stream mutation only sink routes are
  rebuilt. Dirty source node ids route to source signal records, then walk
  explicit dependent edges in rank order. Leaf and structural sinks fan out from
  the changed terminal signal record ids.
- Non-structural dirty updates patch only matching text/value/checked/disabled
  sinks. Structural dirty updates patch DOM incrementally, reuse/move surviving
  keyed row DOM, and patch event bindings by changed slot.
- `Ui.each` currently carries an explicit `key -> U64` hash thunk because Roc
  user code does not expose a public `Hasher.finish`/builtin hash finalizer.
  The host buckets incoming and existing row keys by that explicit hash, uses
  typed `is_eq` only inside matching hash buckets, exposes `each_key_compares`,
  and the current specs assert linear probe counts for keyed churn. The target
  API remains a `key.hash`/`Hasher` constraint once Roc exposes the needed
  finalizer.
- Changed `Ui.when` and `Ui.each` outputs retain the host-owned data needed to
  collect only affected branch/row scopes. Dirty structural work splices those
  replacement descriptors into retained active descriptors without re-entering
  the root `Elem` or materializing a full next active descriptor stream.
  Preserved-order `Ui.each` churn splices only removed/changed/new row scopes
  before applying one DOM patch to the affected each site. Pure row permutations
  reorder existing active render-row segments in place, update nested scope-site
  insertion points, and patch the parent child list with moves counted by the
  longest stable subsequence; they do not re-run row bodies, rebuild the active
  signal graph, or rebind row events. Mixed set-change plus reorder churn now
  uses the named local row-splice plus survivor-move path instead of whole-site
  replacement.
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
- `run-signals-bench` includes all six representative signal apps:
  `ops_dashboard`, `checkout_wizard`, `kanban_board`, `identity_stress`,
  `component_composition`, and `async_effects`.

## Foundation Gaps (the current frontier)

Functional correctness is green on the small fixtures. The next frontier is
data-structure quality, telemetry, and benchmark coverage — the work that makes
the scaling claim true *in the code*, not just on paper. Each gap below is a
violation of the Complexity Discipline budget in `DESIGN.md` and is invisible to
the current metrics, so each must ship with the counter and spec assertion that
would have caught it.

Severity order (highest first). Take one slice at a time and commit each green
result. Each slice lands its fix *and* the assertion that locks it in.

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

### G-F3 — Key-hash-indexed `Ui.each` diff (High, host/platform slice done)

- **Status:** keyed diff no longer scans linearly by `is_eq`. The platform now
  passes an explicit `key -> U64` thunk, and the host builds hash buckets for
  incoming keys and existing row scopes before running typed equality inside a
  bucket.
- **Counter + assertion:** `each_key_compares` is exposed in specs and the
  benchmark CSV. Host coverage asserts exact `3 * L` probes for a 64-row
  reorder with distinct hashes; `identity_stress` asserts the counter for
  reorder/insert/filter churn.
- **Remaining API cleanup:** replace the explicit hash argument with the target
  `key.hash`/`Hasher` constraint after Roc exposes a public hash finalizer such
  as `Hasher.finish : Hasher -> U64`. The generated large-N proof remains part
  of G-F6.

### G-F4 — Moves-only reorder (High, local row-move slices done)

- **Status:** pure `Ui.each` permutations no longer re-collect row descriptors
  or whole-site splice. The dirty path reorders existing active render-row
  segments, updates descendant scope-site insertion indexes, and patches the
  parent DOM child list using a longest-stable-subsequence move count. Mixed
  set-change plus reorder churn first splices only removed/changed/new row
  scopes, then moves survivor row segments into the requested order.
- **Counter + assertion:** host coverage asserts a `3 -> 1 -> 2` dirty reorder
  emits one `append_child`, creates no elements, makes no row-body calls, and
  rebuilds no active graph records. Host coverage also asserts `1,2,3 -> 3,1,4`
  re-enters only the new row body, creates one row element, removes one row, and
  rebuilds no active graph records. `kanban_board` now asserts the same
  zero-create/one-move/no-rebuild budget for its real reorder action.
- **Remaining:** the generated large-N app still needs to pin the same invariant
  across N.

### G-F5 — Telemetry and benchmark-gate coverage (High, representative gate done)

- **Status:** `stream_nodes_scanned`, `each_key_compares`, per-event allocation
  counters, and render command counters are spec-visible runtime metrics and are
  present in the benchmark CSV. `run-signals-bench` now builds and replays all
  six representative apps, including `identity_stress`, `component_composition`,
  and `async_effects`.
- **Spec-vs-CSV split:** scaling invariants go in `expect_metric_delta`
  assertions; timing/aggregate evidence stays CSV-only (see `DESIGN.md` →
  Metrics).
- **Remaining:** the generated large-N app still needs to extend the
  benchmark/spec surface beyond the six representative fixtures.

### G-F6 — Foundation coverage apps and assertions (Medium, lands alongside the fixes)

- **Generated large-N each app** (the one place large N is allowed, because it is
  generated systematically, not a handwritten catalog). N is a build parameter;
  specs assert the budget for single-row update / append / remove / filter /
  reorder using the new counters. This extends the G-F1 active-graph canary from
  host unit coverage to a generated app, and is the primary proof for G-F2..G-F4
  plus the allocation-flatness part of the success metrics.
- **`kanban_board`** reorder and filter steps now carry `mark_metrics` /
  `expect_metric_delta` blocks bounding work.
- **`async_effects`** now asserts the pending-task cancel/close cycle drops the
  retained allocation gauge.
- **`ops_dashboard`** now has a real-event fanout assertion (not just the
  synthetic no-op) bounding `nodes_recomputed`, `derived_calls_into_roc`, text
  patches, and active-graph rebuilds.
- **Long-session leak experiment:** host coverage now reuses one `HostEnv`
  across 100 dispatched events and asserts the live `allocs − deallocs` gauge is
  flat after warmup.

## Green Gates

Use the smallest gate that proves the slice, then run the full signal gate before
committing code changes. Each foundation slice (G-F2..G-F6) must land its fix and
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
