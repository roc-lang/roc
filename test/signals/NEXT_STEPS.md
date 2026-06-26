# Signals — Next Steps

`DESIGN.md` is the enduring target design (one engine, two thin hosts; native is
the spec/telemetry/debug host, wasm is the browser boundary; the same apps run in
both). This file is the live work queue: the ordered drive toward that design,
plus the optimisation backlog held behind it. Completed phase notes and retired
findings belong in git history, not here.

## Drive order

The goal of this phase is to bring all our apps online — running in the browser
alongside the native spec test runner — on top of a fully host-agnostic engine,
then harden the erased-value ownership boundary before spending time on
optimisation. We deliberately defer further optimisation of the implementation
until:

1. the refactor that finishes the engine extraction is complete (**done** —
   `engine.zig` now owns the structural collection/splice/apply/rebind and
   effect-source dispatch path; no reactive or structural logic remains in either
   host file, only host-specific glue),
2. the architecture and abstractions have been reviewed (**done** —
   `verifyCtx`/`verifyRegistryOps`/`verifySink`/`verifyMetrics` pin the host
   seam contracts explicitly, native/wasm host files only expose host glue and
   sinks, and the seam unit modules plus native app/spec gate are green),
3. retained `HostValue` edges carry coherent app-compiled ownership
   capabilities instead of scattered split/eq/drop descriptors, and
4. baseline measurements have been updated and reviewed to inform decisions.

Only then do we re-prioritise the optimisation backlog. The phases below are in
order; take the earliest unfinished one.

## Phase 1 — Bring the app suite online in both hosts

With the engine fully extracted, enable structural behaviour in the wasm host and
get every app in the suite running in the browser *and* under the native spec
runner from the same Roc source.

- [x] Enable the structural wasm path (`Ui.each`, `Ui.when`, `remove_node`,
      `move_before`, async) now that the structural engine is shared. The
      previously-noted comptime kill switch becomes unnecessary once the path is
      green — remove it rather than leaving a dormant flag.
- [x] Event-payload accessor path: carry the accessor descriptor in the
      descriptor tree; JS walks it against the live `Event` and serializes only
      the requested leaves into a `roc_alloc`'d buffer, transferring ownership on
      `roc_ui_event`. No payload reconstruction in JS.
- [x] Structural splicing against a live DOM: `Ui.each`/`Ui.when` detach/move via
      the shared `RemoveNode`/`MoveBefore` commands; surviving rows keep DOM and
      local state; disposed scopes clear `nodes[]`/listeners and drop closures.
- [x] Async/timers/cancellation in the browser: `roc_ui_resolve` / `roc_ui_timer`
      exports with host-assigned `request_id` / `token`, `AbortController` /
      `clearInterval` driven by host-emitted cancel commands on scope dispose.
- [x] `serve.py` builds and serves **any** app in the suite (parameterised app
      selection + both backends), not just the counter/demo. With no app
      argument it builds the maintained six-app suite and serves the suite index;
      pass one app path for targeted QA.
- [ ] Run each of the six apps in the browser as manual QA, and confirm each runs
      green under the native spec runner. The native spec runner and JS runtime
      smoke are green for all six; the suite index now makes the human browser
      pass available from one URL.

We do **not** add an automated real-browser harness. The native spec runner
already asserts semantics and work budgets — the things the browser cannot show
us — and the JS runtime is a thin wrapper whose only meaningful contract is the
cmd/patch codec. A headless-browser harness would re-test DOM behaviour we do not
own and engine semantics already covered natively, for no added signal.

### JS test surface cleanup (part of Phase 1)

The current browser tests re-prove engine semantics through a DOM double, which
duplicates native coverage across the boundary. Keep only the JS↔WASM contract
guards.

- [x] Remove `browser/counter_app.test.mjs`, `browser/executor.test.mjs`, and
      `browser/stable_text_app.test.mjs` — these assert engine semantics (count
      changes, one `set_text` per click, pruning, retained-value budget) that the
      native spec runner already owns and asserts more precisely.
- [x] Keep `browser/wasm_memory_views.test.mjs` (memory.grow view invalidation)
      and `browser/controlled_input_policy.test.mjs` (`SetValue` focus/IME
      policy) — these guard the genuine JS↔WASM boundary contract, not engine
      semantics.
- [x] Ensure the remaining JS guards cover the codec contract: op-code →
      DOM-op mapping, payload marshalling round-trip, and the view-refresh rule.

## Phase 2 — Architecture and abstractions review

A deliberate review pass, once the app suite is online, before any optimisation.

- [x] Are `Ctx` and `sink()` good seams? Is the `Ctx` contract minimal and
      explicit (every decl checked by `verifyCtx`/`verifyRegistryOps`, no duck
      typing)?
- [x] Is the native/wasm split clean — does anything reactive or structural still
      leak into a host file?
- [x] Do we have good unit tests at each seam (engine, scope tree, keyed rows,
      identity table, host-value registry, render sink), or are seams only covered
      transitively through app specs?
- [x] Record the review outcome and any seam changes; fix seam defects here rather
      than carrying them into the optimisation phase.

## Phase 3 — Bundle coherent app-compiled HostValue capabilities

Status: completed as the current architecture.

The immediate wasm `Str.to_utf8` lifetime trap is fixed in the typed backend
where it belongs. The broader ownership-design gap is now closed by making every
retained `HostValue` cell carry one app-compiled capability object. The prebuilt
Zig host remains app-type blind; platform Roc is compiled with the app, so each
concrete `Signal(a)`, state binder, event payload, row key/item, and sink read
site packages the exact operations for its concrete `a`.

The host-visible ABI shape is intentionally erased:

```roc
HostValue.CapabilityHandle := {
    clone : Box((HostValue -> HostValue)),
    eq : Box((HostValue, HostValue -> Bool)),
    drop : Box((HostValue -> {})),
}
```

`Capability(a)` is still a typed Roc wrapper. It builds a private typed
`split : Box(a) -> { keep : Box(a), out : Box(a) }` closure and captures that
split closure inside the app-compiled `clone`, `eq`, and `drop` operations. The
descriptor graph stores only the erased handle above, so heterogeneous `Elem`
and `Node.SignalExpr` payloads no longer under-apply a parameterized
`CapabilityHandle(a)`.

The registry cell is conceptually:

```zig
{ box: RocBox, capability: HostValueCapabilityHandle }
```

Ownership contract:

- `store_with_capability` transfers a boxed value plus its owned capability into
  the registry.
- `store_with_existing_capability` stores a new box under the source value's
  retained capability; it is used by app-compiled clone operations.
- `get_with_capability` checks the supplied handle against the stored handle,
  clones through the stored capability, then consumes the clone. This remains
  split-and-replace semantically; it is never a borrow.
- `take_with_capability` checks the supplied handle, removes the cell, releases
  the registry's retained capability exactly once, and transfers the box to Roc.
- `get_with_split` and `take_with_split` are only for capability internals. The
  host permits them only while it is executing a Roc callable under an active
  frame containing the value's owning capability.

Host sequencing changed accordingly. Signal const/map/map2/combine/interval/task
sources, signal-backed text/bool sinks, `Ui.when`, event payload descriptors,
task request descriptors, row key/item scopes, state cells, active descriptor
cleanup, and unmount teardown now retain, compare, invoke, and release the same
capability handle. The host does not reconstruct a value's split/drop/eq path
from independent descriptor fields.

Task payload callbacks still use consuming ownership. `Done(Str)` and
`Failed(Str)` payloads are assigned the task payload capability, passed to the
app-compiled callback, and then the host asserts the payload handle was consumed
before completing the callback. The host does not drop a payload after ownership
has transferred.

Debug hardening:

- reads before capability assignment fail;
- full capability mismatches fail;
- split/take internals fail if no owning capability is active;
- conflicting capability assignment fails;
- released-handle access fails;
- unconsumed consuming callbacks fail;
- clone results must be a different handle owned by the same capability;
- taking a value releases the stored capability once, and a second take fails.

The remaining edge operations now travel as app-compiled extension records
rather than scattered free-standing thunks. Signal text/bool reads, `Ui.when`
condition reads, task request reads, event reducers, and `Ui.each` operations
carry the owning capability and operation together at the host boundary. The
universal retained-value contract remains `clone`/`eq`/`drop`; edge-specific
operations extend that contract only where the descriptor explicitly asks for
them.

Validation completed for this phase:

- `./zig-out/bin/roc check test/signals/apps/task_to_utf8_lifetime.roc`
- `./zig-out/bin/roc check test/signals/apps/ops_dashboard.roc`
- `node --test test/signals/browser/runtime_contract.test.mjs`
- task lifetime wasm built with `test/signals/serve.py`
- `node test/signals/browser/task_lifetime_harness.mjs ... success`
- `node test/signals/browser/task_lifetime_harness.mjs ... failure`
- ops dashboard wasm flow: loading text disappears after resolving the real
  `/api/ops/dashboard` payload, dashboard content renders, and unmount returns
  retained `HostValue` count to zero
- `zig build run-test-zig -- --test-filter "host value registry"`
- `zig build run-test-signals`

The exact glue regeneration command used for the ABI update was:

```sh
./zig-out/bin/roc glue src/glue/src/ZigGlue.roc test/signals/src test/signals/platform/main.roc
```

Judgment: proceed with the capability architecture. The host now uses a
first-class capability object, not only a Roc-side wrapper, and the edge thunks
cross as capability-owned extension records.

## Phase 3.5 — Remove remaining capability-edge legacy

Status: completed as the current architecture.

The descriptor graph no longer carries edge-specific thunks next to separate
capability handles:

- signal-backed text reads use `HostValue.TextReadHandle`;
- signal-backed bool reads and `Ui.when` conditions use
  `HostValue.BoolReadHandle`;
- task request reads use `HostValue.TaskRequestReadHandle`;
- event reducers use `HostValue.EventReducerHandle`;
- `Ui.each` operations travel in one erased ops record containing the items,
  item, and key capabilities plus `items_to_values`, `key_of`, `key_hash`, and
  `row`.

The operation and owning capability are now one object at the host boundary. The
host-visible shapes remain erased and stable, with no `Box(a)` fields in
heterogeneous descriptors. The host stores/passes one extension object per
retained edge instead of reconstructing a capability plus free-standing thunk.

`HostValue.get` stays split-and-replace, `HostValue.take` stays consuming, and
task payload callbacks stay on the consuming ownership path. The host still has
no Roc layout/refcount knowledge; it only invokes the app-compiled operations it
was handed.

Do not reintroduce the abandoned parameterized host-visible handle shape
(`CapabilityHandle(a)` in `Elem`/`Node` descriptors), compiler fallbacks, or
host-side Roc layout/refcount knowledge.

The exact glue regeneration command used for this ABI update was:

```sh
./zig-out/bin/roc glue src/glue/src/ZigGlue.roc test/signals/src test/signals/platform/main.roc
```

Validation completed for this phase:

- `zig build roc`
- `./zig-out/bin/roc check test/signals/apps/task_to_utf8_lifetime.roc`
- `./zig-out/bin/roc check test/signals/apps/ops_dashboard.roc`
- `node --test test/signals/browser/runtime_contract.test.mjs`
- `python3 test/signals/serve.py test/signals/apps/task_to_utf8_lifetime.roc --output /private/tmp/task_to_utf8_lifetime.wasm --no-server --skip-tailwind`
- `node test/signals/browser/task_lifetime_harness.mjs /private/tmp/task_to_utf8_lifetime.wasm success`
- `node test/signals/browser/task_lifetime_harness.mjs /private/tmp/task_to_utf8_lifetime.wasm failure`
- `zig build run-test-zig -- --test-filter "signals host descriptors carry capability-owned extension records"`
- `zig build run-test-zig -- --test-filter "signals host"`
- `zig build run-test-signals`

## Phase 4 — Update and review baseline measurements

- [x] Re-run `run-signals-bench` across all six apps and record fresh baselines.
- [x] Capture the current values of the work counters (`nodes_recomputed`,
      `patches_emitted`, `active_graph_records_rebuilt`, `stream_nodes_scanned`,
      `each_key_compares`, per-event allocation deltas) for the representative
      events of each app.
- [ ] Review the baselines together: which paths are actually expensive, and on
      what inputs? This review is what turns the optimisation hypotheses below
      from suspicion into prioritised, evidence-backed work.
- [x] Wire the foundation counters into `expect_metric_delta` budget assertions
      (not just captured numbers) on a representative event — at minimum
      `active_graph_records_rebuilt` on a non-structural event and a single-row
      splice. These now gate through the app specs: `kanban_board.txt` asserts
      `active_graph_records_rebuilt` for reorder/splice events, and the
      identity-stress spec asserts keyed-row budgets such as `each_key_compares`
      and row reuse/create/remove counts.

Fresh `zig build run-signals-bench` single-sample baselines captured on
2026-06-26:

| case | actions | nodes_recomputed | patches_emitted | active_graph_records_rebuilt | stream_nodes_scanned | each_key_compares | allocs_this_event | deallocs_this_event | retained_alloc_delta |
| --- | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: |
| signals-ops-dashboard | 60 | 60 | 3900 | 20 | 820 | 20 | 33760 | 21200 | 12560 |
| signals-kanban-board | 300 | 300 | 22500 | 3500 | 3580 | 361240 | 202080 | 178240 | 23800 |
| signals-async-effects | 180 | 220 | 2660 | 60 | 420 | 12880 | 16640 | 13700 | 2900 |
| signals-component-composition | 100 | 100 | 1500 | 120 | 180 | 7420 | 9700 | 7240 | 2420 |
| signals-identity-stress | 180 | 180 | 4000 | 300 | 700 | 75720 | 25380 | 20340 | 5000 |
| signals-checkout-wizard | 180 | 180 | 4280 | 320 | 780 | 16120 | 17840 | 13180 | 4620 |

## Phase 5 — Re-prioritise the optimisation backlog

After the capability-bundling phase and baseline review, re-prioritise the items
below using the measured evidence. Each is stated as a falsifiable hypothesis:
what we believe it helps, why we suspect it, and how we will know. Do not promote
an item to active work until the Phase 4 baseline supports its hypothesis; an
optimisation without measured evidence is speculation.

Two of the items below (the persistent rank-ordered propagation queue and
incremental sink-route maintenance on splice) are **not pure optimisations**:
they are gaps against `DESIGN.md`'s mandated propagation core and Complexity
Discipline budget. They are listed here because they are still measured against
the Phase 4 baselines, but they may be promoted ahead of the discretionary items
when the baseline confirms the budget is being violated rather than merely
suboptimal. The remaining items are genuine optimisations and stay strictly
evidence-gated.

### Persistent rank-ordered propagation queue (design gap, not optimisation)

- **Hypothesis:** a single Engine-owned, rank-bucketed dirty queue — reusing the
  existing `last_dirty_generation` stamp for dedup and `clearRetainingCapacity`
  between events — replaces the per-event `ArrayList` → `toOwnedSlice` →
  insertion-sort path with the queue `DESIGN.md` actually mandates.
- **Why we suspect it:** `DESIGN.md` calls for "a single dirty priority queue per
  propagation" keyed by topological rank, but
  `dirtyActiveSignalRecordIdsForSources` builds a fresh worklist, owns-slices it,
  and `sortActiveSignalRecordIdsByRank` re-sorts it on every event. The
  generation-counter half of a proper dirty-set already exists; only the reused
  queue half is missing. This is closing a stated design gap, not a speculative
  tuning.
- **How we'll know:** propagation produces the same dirty/changed sets and rank
  ordering as today (specs green), and per-event allocation deltas for the
  worklists drop to zero after warmup on a reused `HostEnv`.

### Incremental sink-route maintenance on splice (design gap, not optimisation)

- **Hypothesis:** patching sink routes only for records in the spliced subtree —
  rather than re-walking the whole active stream — makes structural work scale
  with the change, not the tree, as `DESIGN.md` requires.
- **Why we suspect it:** `spliceActiveStreamReplacingTarget` calls
  `rebuildActiveSinkSignalRoutesFromStream`, which re-walks every signal-bearing
  table of the entire active stream to rebuild every sink route even for a
  one-row change. `DESIGN.md` explicitly forbids "clear-and-rebuild of the whole
  active signal graph on a structural change" and budgets dependency-graph
  maintenance at O(affected scope). This is the single biggest algorithmic gap.
- **How we'll know:** `expect_metric_delta active_graph_records_rebuilt` on a
  single-row splice in the large-N app is bounded by the changed set, not N — the
  counter `DESIGN.md` designed specifically to fail this path. The assertion is
  the gate; wiring it is part of the item.

### Per-cycle scratch/arena to remove dispatch allocation churn

- **Hypothesis:** threading one arena (reset `.retain_capacity` per dispatch/
  render cycle) plus a few persistent reused scratch buffers on the Engine
  (`seen: []bool`, the dirty/changed/structural worklists) eliminates the bulk of
  the transient malloc/free churn per event without changing behaviour.
- **Why we suspect it:** every transient in a dispatch cycle currently round-trips
  the safety GPA — the dirty/changed/structural worklists, `seen[]` (allocated
  separately three times), per-element `next_children` `ArrayList`s, the
  signal-graph route-rebuild temporaries, and the recursive `binder_stack`. These
  are cycle-scoped buffers whose shape repeats every event.
- **Constraint:** the arena is for engine-internal bookkeeping only. Boxed Roc
  values outlive the cycle and must stay on the GPA with their refcounts and the
  `roc_alloc` ledger — they must not be moved onto the arena.
- **How we'll know:** the long-session `allocs − deallocs` gauge (see the leak
  experiment) flattens earlier, and per-event allocation deltas for the named
  buffers drop to zero after warmup; specs stay green.

### O(1) identity/descriptor lookup (kill linear-scan-by-id)

- **Hypothesis:** dense side tables for `elem_id → descriptor`, `token → record`,
  and `node_id → active-stream index` — maintained on insert/remove, the way
  `descriptor_indexes_by_elem_id` already is — plus restricting structural
  patching to the spliced subtree, remove the O(render_nodes²) behaviour in the
  structural patch paths.
- **Why we suspect it:** `DESIGN.md` forbids answering "what id is this record?"
  or "what descriptor owns this elem_id?" by walking a list, yet
  `signalRecordByToken`, `stateIndexByNodeId`, `activeScopeSiteByNodeId`,
  `activeWhenIndexByNodeId`, and `activeEachIndexByNodeId` are linear scans —
  several called inside the per-change loop in `applyDirtyStructuralSignalsLocally`
  — and `findElementDesc` / `findTextNodeDesc` / `findSignalTextNodeDesc` /
  `streamHasTextField` / `streamHasBoolField` scan the stream inside loops over
  render nodes. The accelerator pattern already exists in the file; it just is not
  applied to these lookups.
- **How we'll know:** add `stream_nodes_scanned` assertions on a single-row update
  in the large-N app; the counter must be bounded by the changed set, not by N,
  before and after — and the baseline must show the scan is actually a hot path
  worth removing.

### Slot reclamation for monotonic identity tables

- **Hypothesis:** a free-list of vacant slots for `scopes`, `node_identities`, and
  `dom_identities` keeps long-session cost flat where it currently degrades.
- **Why we suspect it:** these tables deactivate via an `active=false` tombstone
  and never reclaim slots, so every linear scan over them pays for dead entries
  and a long session degrades even where the algorithm is nominally bounded.
  `DESIGN.md` budgets ledger ops at O(1) with the index stored in the header; the
  same discipline should apply here.
- **How we'll know:** the long-session leak experiment shows table sizes plateau
  (not grow monotonically) under sustained churn, and identity-lookup
  `stream_nodes_scanned` stays flat across a long session at fixed live N.

### Generated large-N `Ui.each` scaling app

- **Hypothesis:** a generated large-N each app (N a build parameter, generated
  systematically — never a handwritten catalog) is the primary proof that the
  scaling budgets hold across N, extending the active-graph canary from host unit
  coverage to a real app.
- **Why we suspect it:** the current fixtures are small; budget violations that
  only appear at scale can hide behind low patch counts at small N.
- **How we'll know:** specs assert the budget for single-row update / append /
  remove / filter / reorder using `active_graph_records_rebuilt`,
  `stream_nodes_scanned`, `each_key_compares`, and per-event allocations across
  N ∈ {small, large}; flatness across N confirms or refutes the scaling claim.

### Moves-only reorder proof at large N

- **Hypothesis:** pure permutations emit only DOM moves for displaced rows
  (longest-stable-subsequence move count), with no row-body re-run and no active
  graph rebuild, and this holds as N grows.
- **Why we suspect it:** the local row-move path is implemented and asserted on
  small fixtures, but only the generated large-N app can pin that reorder does not
  degrade to whole-site re-collect at scale.
- **How we'll know:** a large-N reorder host test fails if reorder degrades from
  moves-only to whole-site re-collect/rebuild.

### `key.hash` / `Hasher` API cleanup

- **Hypothesis:** replacing the explicit `key -> U64` hash thunk on `Ui.each` with
  the target `key.hash` / `Hasher` constraint removes a platform wart once Roc
  exposes a public hash finalizer (e.g. `Hasher.finish : Hasher -> U64`).
- **Why we suspect it:** the explicit hash argument exists only because Roc does
  not yet expose `Hasher.finish`; it is a stand-in, not the intended API.
- **How we'll know:** the API matches `DESIGN.md`'s `Ui.each` signature and
  `each_key_compares` still tracks L (not L²) under churn after the swap.

### Long-session leak experiment

- **Hypothesis:** retained memory over a long session is flat after warmup, and an
  O(1) allocation-free path keeps session cost linear rather than O(allocs²).
- **Why we suspect it:** per-iteration `retained_alloc_delta` cannot establish
  long-session flatness; only a sustained reuse of one `HostEnv` across many
  events can.
- **How we'll know:** reuse one `HostEnv` across many dispatched events and assert
  the live `allocs − deallocs` gauge is flat after warmup.

### Benchmark-gate breadth

- **Hypothesis:** extending the benchmark/spec surface beyond the six
  representative apps (specifically to the generated large-N app) is what turns
  the scaling claim from "true on small fixtures" into "true across N".
- **Why we suspect it:** scaling regressions are invisible to the current
  representative gate at small N.
- **How we'll know:** the large-N app's scaling invariants are in
  `expect_metric_delta` assertions and the bench CSV carries its timing evidence.

## Green Gates

Use the smallest gate that proves the slice, then run the full signal gate before
committing. For a pure refactor slice the existing native specs are the
regression guard; a behaviour-changing slice must also land the assertion that
locks it in.

- Focused Zig host/engine work:
  `zig build run-test-zig -- --test-filter "native_host"`
- Shared engine instantiates under wasm32:
  `zig build build-test-hosts -Dplatform=signals -Doptimize=ReleaseSmall`
- Platform Roc or ABI changes (any app):
  `./zig-out/bin/roc check test/signals/apps/<app>.roc`
- Focused wasm HostValue lifetime regression:
  `python3 test/signals/serve.py test/signals/apps/task_to_utf8_lifetime.roc --output /private/tmp/task_to_utf8_lifetime.wasm --no-server --skip-tailwind`
  `node test/signals/browser/task_lifetime_harness.mjs /private/tmp/task_to_utf8_lifetime.wasm success`
  `node test/signals/browser/task_lifetime_harness.mjs /private/tmp/task_to_utf8_lifetime.wasm failure`
- End-to-end signal specs:
  `zig build run-test-signals --summary failures`
- Optimized benchmark gate:
  `zig build run-signals-bench`
- Browser host + apps build, both backends:
  `test/signals/serve.py --no-server --app-opt dev`
  `test/signals/serve.py --no-server --app-opt size`
- JS↔WASM contract guards (codec/boundary only):
  `node --test test/signals/browser/wasm_memory_views.test.mjs`
  `node --test test/signals/browser/controlled_input_policy.test.mjs`

For doc-only updates, `git diff --check` is enough.
