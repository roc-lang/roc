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
- The browser path now has a dedicated `src/wasm_host.zig` that links Signals
  apps as wasm reactors, ingests the non-structural descriptor subset, owns
  state/event routing, evaluates source/const/map/map2/combine signals, and
  serializes render commands for `browser/runtime.mjs`. `browser/counter.html`
  loads the runtime, and `serve.py` builds the ReleaseSmall host plus a wasm32
  app and serves only `test/signals/browser`.
- The browser executor and the end-to-end counter now have automated guards.
  `browser/executor.test.mjs` drives the real `runtime.mjs` through every render
  op against a mock host plus a dependency-free DOM double
  (`browser/dom_double.mjs`), and `browser/counter_app.test.mjs` drives the real
  `counter.wasm` end to end (mount tree, one `set_text` per click, zero retained
  host values on unmount). The wasm host exports `roc_ui_live_host_values()` as
  the leak gauge, and `runtime.mjs` records `lastCommands` per host call so the
  patch budget is assertable.

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
host is a **dedicated `wasm_host.zig`** that now carries the first
non-structural browser runtime and **reuses shared primitives where available**,
but it still needs the full shared-engine extraction before structural browser
work should grow. It has its own browser/DOM boundary — it is *not*
`native_host.zig` recompiled. The reactive engine (node table, scheduler,
scopes, keyed diff, refcount discipline) should be shared rather than
reimplemented; the boundary (patch emission, event arrival, timers/`fetch`) is
written fresh for the browser. The native host's simulated DOM and spec runner
are native-specific and not part of the browser host.

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

### G-B0 — Factor the shared reactive engine (Structural prerequisite before G-B6)

- **Why:** the reactive engine (ingestion, node table, rank scheduler, dirty
  propagation with `is_eq` pruning, scope forest, keyed-row diff) lives inline in
  `native_host.zig`, interleaved with the simulated DOM and spec runner. The
  dedicated `wasm_host.zig` must *reuse* this logic, not copy-paste or
  reimplement it (design O9). The minimal counter path now exists, but this seam
  must be settled before browser structural work so the two hosts never diverge
  into separate engines.
- **Work:** extract the host-agnostic engine into a shared module (alongside the
  primitives already in `roc_platform_abi.zig`) that both hosts drive, with the
  patch-emission/renderer boundary kept behind an interface each host implements
  (preferred seam: a render-command sink the engine writes to). Decide how much
  extracts cleanly given the native host's `DomElement`-array coupling.
- **Guard:** `zig test test/signals/src/native_host.zig` stays green after the
  extraction (native host now drives the shared engine); the wasm host's
  existing `comptime` block instantiates the real `Engine(WasmCtx)`, so
  `build-test-hosts` proves the wasm-specialized engine compiles under wasm32.
  This is a refactor with no behavior change — the existing native specs are the
  regression guard.
- **Status:** first boundary extraction landed: `src/render_commands.zig` owns
  the host-independent render ops, command counts, metrics accumulator, and
  fixed-width command-buffer record. `native_host.zig` consumes those shared
  types, and `wasm_host.zig` now serializes browser commands into the shared
  fixed-width record shape. Second extraction: `src/signal_graph.zig` owns
  active signal graph node shape, dependent-edge mutation, reachable-dependent
  traversal, and rank sorting; `native_host.zig` consumes it and `wasm_host.zig`
  instantiates it during build so wasm32 breakage is caught. Third extraction:
  `src/scope_tree.zig` owns scope branch identity, root/component/when/row scope
  interning, active-row lookup, and ancestry queries; native still owns row
  payload refcounts and disposal. Fourth extraction: `src/identity_table.zig`
  owns node/DOM identity interning and one-based DOM ids. Fifth extraction:
  `src/keyed_rows.zig` owns the host-agnostic keyed-row match plan: duplicate
  detection, hash-bucket probing, survivor matching, create slots, and removed
  row ids. Native currently supplies the HostValue/thunk/scope adapter that
  executes the plan; the wasm host must supply the same adapter surface when it
  grows row scopes. Sixth extraction: `src/host_value_registry.zig` owns the
  shared HostValue handle table used by the `roc_host_value_*` ABI in both
  hosts: one-based handles, vacant-slot reuse, clone/get/take, and debug type
  tags. Seventh extraction: `src/erased_calls.zig` owns the pure-ABI Roc
  erased-callable invocation adapter (the `Erased*Args` packed-arg structs and
  the `callErased*` thunk family); both hosts deleted their byte-identical
  copies and alias it. Eighth extraction: `src/host_values.zig` owns the shared
  `RegistryOps` adapter (refcounting boxes/tags through one `roc_host`) and the
  boxed-value constructors (`makeUnit`/`makeStr`/`makeBool`/`makeI64`) behind a
  tiny `store`/`recordKind` host ctx; native keeps its test-kind bookkeeping in
  that ctx, the wasm host makes `recordKind` a no-op. These two land the planned
  PR1 (no behavior change; native 52 tests, wasm build, and the browser tests
  stay green). The `Engine(comptime Ctx)` struct plus the
  `verifyCtx`/`verifyRegistryOps` contract checks landed in Slice 4b-ii (see the
  status entry below); the contract is extended incrementally as 4c/4d move the
  methods that need `allocator()`/`rocHost()`/`recordKind()`/`metrics()`/`sink()`.
  The current wasm host still has a small non-structural
  runtime path for the counter milestone; it is not the final G-B0 extraction.

- **Status (Slice 4a + 4b data layer done):** the entire descriptor data layer
  is now in `src/engine.zig` (host-agnostic: `anytype` metrics, explicit
  `roc_host`, `ctx.cloneHostValue`, `@panic`; compiles for `wasm32`). Moved
  across seven green commits: the retained value-cell + scope-step adapter
  (`HostValueCell`, `HostSignalCacheSlot`, `HostScopeStep`/`HostEachRowScopeStep`,
  `retainHostCallable`, `deinitHostScopeStep`), the signal-record cluster
  (`HostSignalRecord`, `HostSignalBinding`, `rememberSignalRecordTree`), the
  descriptor data types, and the full descriptor stream
  (`HostNodeDescriptorStream` + `append*`). `erased_calls.zig` +
  `host_values.zig` (PR1) and the `NoMetrics` seam (Slice 3) also landed.

- **Status (Slice 4b-ii done — `Engine(comptime Ctx)` struct):** the engine
  state now lives on a generic `Engine(comptime Ctx)` (`engine.zig:1630`);
  `HostEnv` embeds `engine: Engine(NativeCtx)` (`native_host.zig:748`). The
  `Ctx` contract is enforced by `verifyCtx`/`verifyRegistryOps`
  (`engine.zig:193`, `:179`) — real `@compileError`-on-missing-decl checks
  called at every `Engine()` instantiation, not duck typing. A **real**
  `WasmCtx` (not a stub) is instantiated via `_ = engine.Engine(WasmCtx);` in
  the existing `comptime` block (`wasm_host.zig:30`, `:81`), so
  `build-test-hosts -Doptimize=ReleaseSmall` proves the wasm-specialized engine
  compiles under wasm32 on every build. Seven green commits moved the safe
  engine-only method batches (lookups, scope/identity ops, descriptor accessors,
  cleanup/lifecycle). Verified: 53/53 native, `build-test-hosts` green,
  `run-test-signals` green, browser tests 19/19. `engine.zig` ~1,881 lines;
  `native_host.zig` ~10,286 (down from 11,821 at the start of the arc). 4c/4d
  untouched.

- **Status (Slice 4c done — scheduler/dirty/keyed):** the rank-sorted dirty-id
  scheduler moved into `Engine` (`engine.zig:2183`, `:2225`), dirty propagation
  with memoized `is_eq` pruning moved into `Engine` (`engine.zig:2507`, `:2529`),
  and the keyed-row hash/equality plan path now calls `keyed_rows.buildPlan`
  from `Engine` (`engine.zig:2705`). `verifyCtx` now checks the 4c hooks
  (`allocator`, `cloneHostValue`, `stateValueByNodeId`, `stateEqCallable`,
  `stateDropCallable`), and `WasmCtx` implements them (`wasm_host.zig:36`,
  `:61`).
  Native stayed behavior-identical: three 4c commits each passed 53/53 native,
  `build-test-hosts -Dplatform=signals -Doptimize=ReleaseSmall`,
  `run-test-signals --summary failures`, and browser tests 19/19 with
  `--test-force-exit`. The active-graph rebuild canaries for dirty structural
  paths are present in the host tests and remained green. `engine.zig` ~2,376
  lines; `native_host.zig` ~9,935.

- **Status (Slice 4d done — render sink / scalar cache / topology cache):**
  `src/render_sink.zig` now defines the host-facing `DomSink`
  (`render_sink.zig:15`), and `verifyCtx` checks `sink()` as an explicit
  contract decl (`engine.zig:205`). Native exposes a pure simulated-DOM sink
  (`native_host.zig:767`, `:823`), while render decisions now live in `Engine`:
  scalar text/bool/metadata diffing uses engine-owned per-element caches
  (`engine.zig:1687`, `:1984`), and structural topology state
  (tag/active/parent/child order/bound events) is engine-owned through
  `resetRenderTree`, `replaceRenderChildren`, and event-binding methods
  (`engine.zig:1731`, `:1807`, `:1869`). Structural apply paths call the engine
  and then assert engine-index-to-descriptor-stream and engine-index-to-DOM
  consistency in Debug (`engine.zig:1923`, `:1967`;
  `native_host.zig:5558`, `:5809`). Scope disposal no longer mutates DOM active
  state directly; render removal owns that lifecycle. Verified for each 4d
  sub-slice: native 53/53, `build-test-hosts -Dplatform=signals
  -Doptimize=ReleaseSmall`, `run-test-signals --summary failures`, and browser
  tests 19/19 with `--test-force-exit`. `engine.zig` ~2,778 lines;
  `native_host.zig` ~9,904; `render_sink.zig` 75.

- **Remaining G-B0 work — 5a/5b.** The shared engine is now factored on the
  native side and compiled for `WasmCtx`; the remaining behavior-changing work
  is to point wasm at the shared engine:

  - **5a — non-structural wasm path:** replace wasm's
    `BoundSignal`/`evalBoundSignal`/`updateSignalSinks` path with
    `Engine(WasmCtx)` plus a command-buffer sink for scalar text/value/bool
    updates. Wasm gains the same `is_eq` memoization as native. Rebaseline JS
    executor/counter assertions deliberately in two commits: first prove the
    initial render op stream is structurally identical, then reduce redundant
    update-count assertions for pruned patches.
  - **5b — structural wasm path:** gate structural support behind a comptime
    `wasm_structural_enabled` kill switch until the path is green; then enable
    `Ui.each`, `Ui.when`, `remove_node`, `move_before`, and async in the wasm
    executor/browser tests.
  - **Triage the browser-test hang before 5b (not a current blocker).** The
    `node --test test/signals/browser/*.test.mjs` run completes its assertions
    green but then hangs without `--test-force-exit`, indicating an open handle
    (timer / `WebAssembly.Memory`-backed resource / unclosed fixture) that is not
    torn down. Harmless today, but Slice 5 makes wasm drive the engine and G-B7
    adds async/timer paths, where a lingering handle gets worse. Investigate and
    fix the teardown before 5b rather than leaving it permanently masked by the
    force-exit flag.


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

### G-B2 — `memory.grow` view invalidation + marshalling spike (Critical, runtime wired)

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
- **Status update:** `browser/runtime.mjs` now uses the helper for host calls,
  event-payload writes, command-buffer reads, and string-buffer reads.
- **Remaining:** keep the `memory.grow` guard in the browser gate and add a
  runtime-level stress case once the executor has an automated DOM smoke test.

### G-B3 — Command-buffer wire format + executor contract (Critical, browser path landed)

- **Why here:** this is the boundary itself. It decides O1 (`RemoveNode` /
  `MoveBefore`) and O2 (shared command set vs. two emit paths). Settling it
  shared lets the native host's metrics assert move-only reorder and keeps both
  hosts behind one contract.
- **Status:** `RemoveNode`/`MoveBefore` are now first-class native render-command
  counters. Structural removals emit `remove_node`; pure keyed reorders emit
  `move_before` rather than `append_child`. The shared command module also
  defines the fixed-width WASM record shape. `wasm_host.zig` writes render
  records and string payloads into linear-memory buffers and exports the
  pointer/length/record-width/clear surface. `browser/runtime.mjs` drains the
  buffer after `roc_ui_mount` / `roc_ui_event` and implements the current command
  set, including `RemoveNode` and `MoveBefore`.
- **Finding + guard:** the op-code table is documented; native-host tests assert
  reorder emits only `MoveBefore` for displaced rows, and the kanban/identity
  specs assert the new command counters for reorder and removal cases.
- **Finding:** the first runtime does not use an imported `env.host_flush`;
  JS calls a host export, refreshes memory views, then drains the command buffer
  synchronously. That keeps the boundary smaller for the counter milestone.
- **Status (executor smoke test done):** `browser/executor.test.mjs` drives the
  real `SignalsRuntime` through a mock host (backed by a real
  `WebAssembly.Memory`) and the dependency-free `browser/dom_double.mjs`, and
  asserts command records become the expected DOM for every render op:
  element/text creation, attribute/value/checked/disabled/role/label/test-id
  patches, click/input/check dispatch, structural `RemoveNode`/`MoveBefore`, and
  a `memory.grow` mid-dispatch. The runtime now records `lastCommands` per host
  call so the patch budget is assertable.
- **Remaining:** decide whether the native spec runner should migrate to the
  same command-buffer consumer to reduce divergent render surfaces.

### G-B4 — Minimal end-to-end counter in a real browser (High, automated guard landed)

- **Why:** the `BROWSER_RUNTIME_DESIGN.md` §10 milestone — the smallest proof the
  whole boundary works on a real DOM. It uses the G-B1..G-B3 findings; the full
  G-B0 shared-engine extraction still needs to happen before structural browser
  work grows.
- **Status:** first manual counter path landed. `src/wasm_host.zig` exports
  `roc_ui_mount` / `roc_ui_event` / `roc_ui_unmount`, ingests
  `Element`/`Text`/`TextSignal`/`State`, handles static and signal text/bool
  attributes plus `OnEvent`, routes unit/string/bool payloads, evaluates
  `Ref`/`ConstValue`/`Map`/`Map2`/`Combine`, and emits initial/update command
  buffers. `browser/runtime.mjs` instantiates wasm, drains records into DOM
  nodes, binds click/input/check events, and forwards payloads. `apps/counter.roc`
  lives with the other examples, `browser/counter.html` loads it, and
  `serve.py` builds and serves the manual QA page.
- **Status (automated guard landed):** `browser/counter_app.test.mjs` loads the
  real Roc-compiled `counter.wasm` (building it via `serve.py --no-server` when
  absent and the toolchain is present; skipping loudly otherwise) and drives it
  through the real executor against `browser/dom_double.mjs`. It asserts the
  mount tree and 17-command mount budget (one `reset_dom`, two `bind_click`, one
  `create_text`), that clicking `Increment`/`Decrement` changes the count with
  exactly one `set_text` per click, and that `roc_ui_unmount` drops every
  retained host value — `wasm_host.zig` now exports `roc_ui_live_host_values()`
  (backed by `host_value_registry.liveCount()`) which returns 1 while mounted and
  0 after unmount, and a clean re-mount succeeds.
- **Remaining:** run the same flow in a *real* browser (the `counter.html` page
  is still the manual QA surface; the automated guard uses a DOM double, not a
  real browser/IME). The single-sink counter proves a `set_text == 1` budget, but
  the wasm host still updates all active sinks after a changed state, so the
  general multi-sink `nodes_recomputed == 1` pruning is unproven and belongs with
  the G-B0 shared-engine extraction (or a multi-sink browser fixture), not more
  bespoke wasm-host pruning. Out of scope here: `Ui.each`/`Ui.when`, async,
  intervals, IME — those are G-B6/G-B7.

### G-B5 — Event payload accessor path (High)

- **Why:** real events are cyclic and not serializable; only the fields a reducer
  needs may cross (design O6, research-note Tactic 5). The accessor descriptor
  must be carried in the descriptor tree (extend `OnEvent` / `__AnonStruct56` if
  needed), never reconstructed in JS, and must map to the host's typed
  `EventPayloadKind` (unit/str/bool).
- **Status:** the initial runtime covers the current typed payload path:
  `BindClick` sends unit, `BindInput` serializes `event.currentTarget.value` as
  UTF-8 into `roc_alloc` memory, and `BindCheck` sends a bool. The host validates
  the payload kind against the descriptor-owned event route before running the
  retained reducer thunk.
- **Remaining work + guard:** generalize from the hard-coded click/input/check
  payload fields to the explicit accessor descriptor path. JS walks the accessor
  path against the live `Event`, serializes only requested leaves into a
  `roc_alloc`'d buffer, transfers ownership on `roc_ui_event`, and a test asserts
  the payload byte length tracks only the requested leaves.

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
- Shared engine instantiates under wasm32 (from Slice 4b-ii, via the wasm host's
  existing `comptime _ = Engine(WasmCtx)` block):
  `zig build build-test-hosts -Dplatform=signals -Doptimize=ReleaseSmall`
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
  `zig build build-test-hosts -Dplatform=signals -Doptimize=ReleaseSmall`
- Browser host + counter app build, both backends:
  `test/signals/serve.py --no-server --app-opt dev`
  `test/signals/serve.py --no-server --app-opt size`
- Controlled-input spike policy:
  `node --test test/signals/browser/controlled_input_policy.test.mjs`
- `memory.grow` view invalidation policy:
  `node --test test/signals/browser/wasm_memory_views.test.mjs`
- Browser executor smoke test (command records become expected DOM):
  `node --test test/signals/browser/executor.test.mjs`
- End-to-end counter guard (requires the serve.py wasm build above, or builds it
  on demand):
  `node --test test/signals/browser/counter_app.test.mjs`
- The browser executor + spike findings: each G-B slice records its finding in
  `BROWSER_RUNTIME_DESIGN.md` (O1–O9) and lands a JS/host test or assertion that
  would catch a regression. A browser spike with no recorded finding or guard is
  not done.

For doc-only updates, `git diff --check` is enough.
