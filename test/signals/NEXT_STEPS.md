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

## Browser Runtime (the new priority frontier)

`BROWSER_RUNTIME_DESIGN.md` is the target design. We are now prioritising a
working in-browser implementation to demonstrate the signals vision end to end
in a real environment, and — just as important — to let real-world experience
answer the open questions that the simulated host is structurally blind to.

The recommended architecture is **host owns logical identity and emits a
patch-op stream; JS is a thin executor that owns DOM identity**. The browser
host is a **dedicated `wasm_host.zig`** (today carrying only alloc + `HostValue`
cells) that **reuses the reactive engine logic where that makes sense** and has
its own browser/DOM boundary — it is *not* `native_host.zig` recompiled. The
reactive engine (node table, scheduler, scopes, keyed diff, refcount discipline)
should be shared rather than reimplemented; the boundary (patch emission, event
arrival, timers/`fetch`) is written fresh for the browser. The native host's
simulated DOM and spec runner are native-specific and not part of the browser
host.

**Ordering principle: validate the highest-risk unknowns first, with the
smallest spike that can refute the architecture.** The risks below are ordered so
that a wrong assumption is discovered before it is built upon. Each browser slice
(G-B*) lands a spike or implementation *and* the assertion/finding that locks in
what we learned. A spike whose only output is "it worked" without a recorded
finding or a regression guard is not done. The `DESIGN.md` Definition of Done is
simulated-host scoped; these gaps are the explicitly-deferred next stage, not a
regression of it.

The open questions (O1–O9) referenced below are defined in
`BROWSER_RUNTIME_DESIGN.md` §11.

### G-B0 — Factor the shared reactive engine (Structural prerequisite to G-B4)

- **Why:** the reactive engine (ingestion, node table, rank scheduler, dirty
  propagation with `is_eq` pruning, scope forest, keyed-row diff) lives inline in
  `native_host.zig`, interleaved with the simulated DOM and spec runner. The
  dedicated `wasm_host.zig` must *reuse* this logic, not copy-paste or
  reimplement it (design O9). Settle the seam before G-B4 so the two hosts never
  diverge into separate engines.
- **Work:** extract the host-agnostic engine into a shared module (alongside the
  primitives already in `roc_platform_abi.zig`) that both hosts drive, with the
  patch-emission/renderer boundary kept behind an interface each host implements
  (preferred seam: a render-command sink the engine writes to). Decide how much
  extracts cleanly given the native host's `DomElement`-array coupling.
- **Guard:** `zig test test/signals/src/native_host.zig` stays green after the
  extraction (native host now drives the shared engine); the engine module
  builds for the `wasm32` target. This is a refactor with no behavior change —
  the existing native specs are the regression guard.
- **Status:** first boundary extraction landed: `src/render_commands.zig` owns
  the host-independent render ops, command counts, metrics accumulator, and
  fixed-width command-buffer record. `native_host.zig` consumes those shared
  types, and `wasm_host.zig` exposes an empty command-buffer surface for the
  future browser sink. Remaining G-B0 work is the actual engine extraction
  (scheduler/scope/node-table/keyed-diff logic) out of the native simulated-DOM
  coupling.

### G-B1 — Controlled-input / focus / IME spike (Critical, initial guard landed)

- **Why first:** this is the single risk the simulated DOM cannot surface
  (`DESIGN.md` Open Questions, design O5). A `SetValue` on a focused `<input>`
  mid-composition can clobber the caret/selection or break IME. If this forces a
  first-class effectful/pull primitive rather than a blind executor patch, it
  changes the boundary contract — so it must be learned *before* the executor and
  command set are finalised.
- **Spike:** a hand-written minimal WASM-stub or even a JS-only mock that drives a
  real controlled `<input>` through `SetValue` patches while focused, typing, and
  composing (CJK/IME). No full engine required.
- **Status:** `browser/controlled_input_policy.mjs` now carries the executor
  policy and `browser/controlled_input_policy.test.mjs` asserts it. The manual
  real-browser harness is `browser/controlled_input_spike.html`.
- **Finding:** `SetValue` cannot be applied unconditionally. Equal values are
  no-ops; differing values are deferred while the input is focused or composing;
  the latest deferred value is applied after blur unless a later input echo
  already matched it. The command set stays unchanged for G-B4, but focused
  masking/validation needs an explicit future input-reconciliation design.
- **Remaining:** run the harness in the target browser/IME matrix and record the
  observed behavior before declaring text-input-heavy browser apps complete.
  G-B2 is the next implementation slice.

### G-B2 — `memory.grow` view invalidation + marshalling spike (Critical, guard landed)

- **Why early:** any `roc_alloc` during a host call can grow linear memory and
  detach JS typed-array views (design O4). Getting this wrong is silent memory
  corruption, and it has no simulated-host analogue. The marshalling primitives
  (UTF-8 in/out, in-place writes, packed return flags) are foundational to every
  later slice.
- **Spike:** instantiate a tiny module, force `memory.grow` via repeated
  `roc_alloc`, and prove JS view rebuild is correct. Decide the mechanism: a
  host-bumped "memory generation" export vs. rebuild-on-every-host-call.
- **Status:** `browser/wasm_memory_views.mjs` now carries the view-cache helper
  and tiny Wasm fixture; `browser/wasm_memory_views.test.mjs` asserts stale view
  detachment, refreshed-view writes, and Wasm reads across the grow boundary.
- **Finding:** choose rebuild-on-every-allocating-host-call for the initial
  runtime. JS refreshes cached `Uint8Array`/`Int32Array`/`DataView` objects after
  every host export that may allocate and before reading command buffers or
  string/payload bytes. No host-bumped memory generation export is needed for
  G-B4.
- **Remaining:** wire the helper into the eventual browser executor. G-B3 is the
  next implementation slice.

### G-B3 — Command-buffer wire format + executor contract (Critical, native guard landed)

- **Why here:** this is the boundary itself. It decides O1 (`RemoveNode` /
  `MoveBefore`) and O2 (shared command set vs. two emit paths). Settling it
  shared lets the native host's metrics assert move-only reorder and keeps both
  hosts behind one contract.
- **Status:** `RemoveNode`/`MoveBefore` are now first-class native render-command
  counters. Structural removals emit `remove_node`; pure keyed reorders emit
  `move_before` rather than `append_child`. The shared command module also
  defines the fixed-width WASM record shape, and `wasm_host.zig` exposes the
  command-buffer pointer/length/record-width/clear exports.
- **Finding + guard:** the op-code table is documented; native-host tests assert
  reorder emits only `MoveBefore` for displaced rows, and the kanban/identity
  specs assert the new command counters for reorder and removal cases.
- **Remaining:** wire the shared engine's browser render sink to the command
  buffer, define the single `env.host_flush` drain protocol, and implement the
  JS executor cases for the command set including `RemoveNode`/`MoveBefore`.

### G-B4 — Minimal end-to-end counter in a real browser (High, the milestone)

- **Why:** the `BROWSER_RUNTIME_DESIGN.md` §10 milestone — the smallest proof the
  whole boundary works on a real DOM. Depends on G-B0 (shared engine) and
  G-B1..G-B3 findings.
- **Work:** drive the shared reactive engine (from G-B0) inside `wasm_host.zig`
  for the non-structural subset (`source`, `map`, `map2`) plus the
  `event_id -> source` route — reusing the engine, not reimplementing it; export
  `roc_ui_mount` / `roc_ui_event` / `roc_ui_unmount`; emit the initial patch
  stream; ship `roc-ui-runtime.js` (drain buffer, build DOM, one delegated
  `click` listener, packed-flag handling).
- **Counter + assertion:** clicking `+`/`-` in a real browser changes the count;
  exactly one `nodes_recomputed` and one `SetText` patch per click;
  `roc_ui_unmount` drops all retained closures with `closure_retains ==
  closure_releases` (no leak). Out of scope here: `Ui.each`/`Ui.when`, async,
  intervals, IME — those are G-B6/G-B7.

### G-B5 — Event payload accessor path (High)

- **Why:** real events are cyclic and not serializable; only the fields a reducer
  needs may cross (design O6, research-note Tactic 5). The accessor descriptor
  must be carried in the descriptor tree (extend `OnEvent` / `__AnonStruct56` if
  needed), never reconstructed in JS, and must map to the host's typed
  `EventPayloadKind` (unit/str/bool).
- **Work + guard:** JS walks the accessor path against the live `Event`,
  serialises only requested leaves into a `roc_alloc`'d buffer, transfers
  ownership on `roc_ui_event`. Prove `on_input` delivers `target.value` with no
  whole-event copy; a test asserts the payload byte length tracks only the
  requested leaves.

### G-B6 — Structural splicing against a live DOM (High)

- **Why:** `Ui.each`/`Ui.when` need real detach/move (`RemoveNode`/`MoveBefore`
  from G-B3), honouring the `DESIGN.md` reorder budget ("reorder moves, it does
  not rebuild") and event-delegation so listeners don't churn per splice.
- **Counter + assertion:** mirror the native-host reorder/filter budgets against
  the real DOM — displaced rows produce only moves, surviving rows keep DOM and
  local state, disposed scopes clear `nodes[]`/listeners and drop closures.

### G-B7 — Async effects, timers, and cancellation in the browser (Medium)

- **Why:** effects are sources (design O7); the browser must route real `fetch` /
  `setInterval` results back through the one propagation queue via new
  `roc_ui_resolve` / `roc_ui_timer` exports and host-assigned `request_id` /
  `token` (never app strings), with `AbortController` / `clearInterval` driven by
  host-emitted cancel commands on scope dispose.
- **Counter + assertion:** an async request folds `[Loading, Done, Failed]` into
  real DOM; disposing the owning scope aborts the in-flight `fetch` and runs
  `Ui.on_cleanup`; the retained allocation gauge returns to baseline across the
  open/close cycle (browser analogue of the `async_effects` cancel assertion).

### G-B8 — Multiple instances / module reuse (Low, defer)

- **Why later:** all host state in `wasm_host.zig` is module-global (design O8).
  Milestone uses one WASM instance per `mount()`. Revisit shared-module
  multi-mount only after the single-mount path is proven end to end.

## Green Gates

Use the smallest gate that proves the slice, then run the full signal gate before
committing code changes. Each foundation slice (G-F2..G-F6) must land its fix and
its locking assertion together; a fix without the counter/spec that would catch a
regression is not done.

- Focused Zig host work:
  `zig test test/signals/src/native_host.zig`
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

Browser-runtime slices (G-B*) additionally gate on:

- Wasm host build links and exports the control surface:
  `zig build build-test-hosts -Dplatform=signals` (wasm32 target)
- Controlled-input spike policy:
  `node --test test/signals/browser/controlled_input_policy.test.mjs`
- `memory.grow` view invalidation policy:
  `node --test test/signals/browser/wasm_memory_views.test.mjs`
- The browser executor + spike findings: each G-B slice records its finding in
  `BROWSER_RUNTIME_DESIGN.md` (O1–O8) and lands a JS/host test or assertion that
  would catch a regression. A browser spike with no recorded finding or guard is
  not done.

For doc-only updates, `git diff --check` is enough.
