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
      argument it builds the maintained app suite and serves the suite index;
      pass one app path for targeted QA.
- [ ] Run each maintained app in the browser as manual QA, and confirm each runs
      green under the native spec runner. The native spec runner and JS runtime
      smoke are green for the maintained app set; the suite index now makes the human browser
      pass available from one URL.

We do **not** add an automated real-browser harness. The native spec runner
already asserts semantics and work budgets — the things the browser cannot show
us — and the JS runtime is a thin wrapper whose only meaningful contract is the
cmd/patch codec. A headless-browser harness would re-test DOM behaviour we do not
own and engine semantics already covered natively, for no added signal.

### Attribute/event/payload boundary slice evaluation

- **Status:** implemented. Custom text attrs remain on the existing
  `SetAttrText` / `RemoveAttr` dynamic record path. General named events now use
  explicit `BindEvent` / `ClearEvent` dynamic records with event name, static
  listener option bits, payload kind, and payload descriptor. `keydown` requests
  only `{ event.key, event.shiftKey }`; JS encodes layout-independent bytes and
  the Roc app-facing `Ui.State.on_key` decoder constructs `{ key, shift_key }`.
  Submit uses a unit descriptor plus the static prevent-default option.
- **Alternatives rejected:** JSON payloads, JS decoding Roc record/list/string
  layouts, DOM-state inference, global interning of payload values, and
  generalizing fixed hot click/input/check/pointer ops before measurement.
- **Coverage added:** `event_payload_boundary.roc` / `.txt` covers `href`,
  `aria-label`, `data-*`, `id`, `placeholder`, static and signal-backed custom
  attrs, keyboard payload updates, and submit prevent-default. Native host spec
  actions now include `key_down` and `submit`. JS contract tests cover dynamic
  event extraction, listener options, malformed descriptors/records, and memory
  view refresh after payload allocation.
- **Measured result:** `zig build run-signals-bench` includes
  `signals-event-payload-boundary` at 20 iterations / 80 actions:
  `commands=1100`, `bind_event=80`, `allocs_this_event=7120`,
  `deallocs_this_event=5620`, `retained_alloc_delta=1460`,
  `host_alloc_bytes_this_event=1451800`,
  `host_dealloc_bytes_this_event=698040`, `host_retained_bytes_delta=751360`.
  Existing command-count and allocation metrics were sufficient; no new metric
  counter was needed for this slice.
- **Remaining risks / next slice:** general boolean custom attrs such as
  `required` are still next because the demo and host specs only needed text
  attrs plus existing checked/disabled bool fields. Broader event descriptors
  should add more explicit leaf constants only when an app needs them. Native and
  wasm still have two render-surface sinks behind the same command enum; command
  wire parity remains an open design question.

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
- `Ui.each_str` operations travel in one erased ops record containing the items,
  item, and key capabilities plus `items_to_values`, `key_of`, `key_text`, and
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
- [x] Review the baselines together: which paths are actually expensive, and on
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

Post host-allocation-instrumentation `zig build run-signals-bench` single-sample
baseline captured on 2026-06-27. This is the comparison point for future
scratch-buffer work; timing is included for context, but the allocation count
and byte columns are the primary signal.

| case | actions | total_ns | allocs_this_event | deallocs_this_event | host_allocs_this_event | host_deallocs_this_event | host_alloc_bytes_this_event | host_dealloc_bytes_this_event | host_retained_alloc_delta | host_retained_bytes_delta |
| --- | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: |
| signals-ops-dashboard | 60 | 51619910 | 33760 | 21200 | 47500 | 23900 | 8296620 | 4079080 | 23600 | 4217540 |
| signals-large-each-64 | 120 | 816326234 | 160640 | 139940 | 241500 | 189460 | 200148680 | 189401620 | 51960 | 10745940 |
| signals-async-effects | 180 | 24911045 | 16640 | 13700 | 27340 | 20460 | 4984060 | 3599480 | 6800 | 1383760 |
| signals-checkout-wizard | 180 | 22675915 | 17800 | 13140 | 32720 | 22860 | 7700300 | 5560300 | 9800 | 2139200 |
| signals-identity-stress | 180 | 38397540 | 24180 | 19140 | 43980 | 32080 | 11723240 | 8834440 | 11820 | 2887980 |
| signals-kanban-board | 320 | 555115227 | 242180 | 219460 | 317400 | 261380 | 87414760 | 76704060 | 55960 | 10703340 |
| signals-component-composition | 100 | 9469786 | 9260 | 6800 | 17240 | 10980 | 3241120 | 1967400 | 6180 | 1272740 |

Post keyed-row-site index `zig build run-signals-bench` single-sample comparison
captured on 2026-06-27. The keyed row path now keeps an explicit active
`(parent_scope_id, site_ordinal) -> rows` table with per-site hash heads/links,
so dirty diffs no longer scan the scope forest or rebuild existing-row bucket
arrays. The large-N specs tightened `each_key_compares` from four per row to two
per row for no-collision diffs. The earlier large keyed-list readings were a CSV
alignment mistake: the million-scale values are `stream_nodes_scanned`, not
`each_key_compares`. Key comparison work is now linear and small relative to
structural stream scanning; host allocation churn still drops on the keyed-list
cases.

| case | actions | total_ns | stream_nodes_scanned | each_key_compares | allocs_this_event | deallocs_this_event | host_allocs_this_event | host_deallocs_this_event | host_alloc_bytes_this_event | host_dealloc_bytes_this_event | host_retained_alloc_delta | host_retained_bytes_delta |
| --- | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: |
| signals-ops-dashboard | 60 | 51992067 | 20 | 0 | 33760 | 21200 | 47500 | 23900 | 8296620 | 4079080 | 23600 | 4217540 |
| signals-large-each-64 | 120 | 722977782 | 1118520 | 14680 | 160640 | 139940 | 224120 | 171880 | 195850600 | 184879540 | 52160 | 10969940 |
| signals-async-effects | 180 | 24981420 | 10200 | 0 | 16640 | 13700 | 27340 | 20460 | 4984060 | 3599480 | 6800 | 1383760 |
| signals-checkout-wizard | 180 | 22923971 | 10280 | 40 | 17800 | 13140 | 32800 | 22760 | 7716620 | 5547820 | 9980 | 2168000 |
| signals-identity-stress | 180 | 38606668 | 37060 | 660 | 24180 | 19140 | 42760 | 30600 | 11595060 | 8654820 | 12080 | 2939420 |
| signals-kanban-board | 320 | 554785712 | 454320 | 1400 | 242180 | 219460 | 313920 | 257520 | 87030340 | 76245640 | 56340 | 10777340 |
| signals-component-composition | 100 | 12397755 | 5800 | 220 | 9260 | 6800 | 16600 | 10140 | 3177080 | 1871940 | 6380 | 1304160 |

Stream-scan diagnostic counters added on 2026-06-27 isolate the next large
structural cost:

| case | stream_nodes_scanned | remove_target | splice | render_scope | children | events | dirty_scope | each_key_compares |
| --- | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: |
| signals-large-each-64 | 1118520 | 451840 | 300800 | 236360 | 110680 | 12520 | 6320 | 14680 |
| signals-kanban-board | 454320 | 128520 | 79280 | 82480 | 132340 | 29860 | 1840 | 1400 |

Sparse render-child index `zig build run-signals-bench` single-sample comparison
captured on 2026-06-27. `HostNodeDescriptorStream` now keeps explicit sparse
render metadata keyed by elem id, so `streamDirectChildren` walks child links
instead of scanning `render_nodes`. The first dense-field/dense-side-table shapes
were rejected because they eliminated scans but regressed host allocation bytes
materially; the kept sparse map shape preserves the scan win with allocation
bytes close to the keyed-row-site baseline.

| case | actions | total_ns | stream_nodes_scanned | stream_nodes_scanned_children | each_key_compares | host_allocs_this_event | host_deallocs_this_event | host_alloc_bytes_this_event | host_dealloc_bytes_this_event | host_retained_alloc_delta | host_retained_bytes_delta |
| --- | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: |
| signals-large-each-64 | 120 | 777513618 | 1007840 | 0 | 14680 | 241960 | 189700 | 198628840 | 187283540 | 52180 | 11344180 |
| signals-kanban-board | 320 | 601585598 | 321980 | 0 | 1400 | 337480 | 281060 | 90453540 | 79294600 | 56360 | 11151580 |

Outcome:

- The child bucket is gone: `signals-large-each-64` drops exactly 110680 stream
  scans and `signals-kanban-board` drops exactly 132340 stream scans.
- Host allocation bytes are close but not free: large-each is about +2.8MB over
  the keyed-row-site baseline, and kanban is about +3.4MB over it. The rejected
  dense shapes were much worse, at roughly +27MB and +8MB respectively.
- Single-sample wall time is noisy and not improved here. Treat this as the
  bounded child-bucket win plus a probe that confirms the next larger targets
  are the remaining `remove_target`, `splice`, and `render_scope` buckets and
  the still-dark whole-stream splice tail work.

Dark-path attribution counters added on 2026-06-27 and measured with
`zig build run-signals-bench` after the sparse render-child index. These counters
are not part of `stream_nodes_scanned`; they count the whole-stream/whole-graph
work that was previously invisible in the scan split.

| case | signal_record_table_rebuilt | active_intervals_synced | render_indexes_refreshed |
| --- | ---: | ---: | ---: |
| signals-large-each-64 | 69240 | 141080 | 90920 |
| signals-kanban-board | 20960 | 33040 | 20280 |

This confirms that the splice tail is a major uncounted structural cost. On the
large-N canary, the hidden counters sum to 301240 units, with interval sync as
the largest single dark path. The next implementation slice should remove the
whole-stream signal-record table rebuild and active interval graph walk from
splices before moving to the `remove_target` descriptor index work.

Incremental splice-tail signal-record/interval maintenance landed on
2026-06-27. `HostNodeDescriptorStream` now carries explicit descriptor-tree
owner counts for signal records, so splices register and forget only the
removed/inserted signal-record trees instead of rebuilding the active token
table. Active interval sources now start/cancel at active graph retain/release
points, so splice updates no longer walk the whole active signal graph to sync
intervals. The remaining interval-sync count below is the full-rebuild/init
residue, not the per-splice tail.

| case | total_ns | signal_record_table_rebuilt | active_intervals_synced | render_indexes_refreshed | host_alloc_bytes_this_event |
| --- | ---: | ---: | ---: | ---: | ---: |
| signals-large-each-64 | 758172373 | 0 | 2600 | 90920 | 198918280 |
| signals-kanban-board | 594475033 | 0 | 2120 | 20280 | 90631460 |

Elem-owned descriptor removal worklists landed on 2026-06-27. Structural
splices now consume the explicit `removed_elem_ids` list and the active stream's
`descriptor_indexes_by_elem_id` table to remove render-owned descriptor rows
directly: elements, text nodes, signal text nodes, scalar attrs, signal attrs,
and events. Removal indexes are collected before mutation, sorted descending,
and applied with swap-removal so moved rows update their descriptor indexes and
signal/event routes exactly once. Scope-owned descriptor tables still use the
target-scope membership set and are the remaining `remove_target` residue.

| case | total_ns | stream_nodes_scanned | remove_target | render_scope | splice | events | dirty_scope | render_indexes_refreshed | host_alloc_bytes_this_event |
| --- | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: |
| signals-large-each-64 | 698619001 | 561600 | 5600 | 236360 | 300800 | 12520 | 6320 | 90920 | 198930940 |
| signals-kanban-board | 544532300 | 186780 | 3200 | 77800 | 75640 | 28300 | 1840 | 17680 | 90282660 |

Outcome:

- `stream_nodes_scanned_remove_target` collapsed from 451840 to 5600 on
  `signals-large-each-64`, and from 128520 to 3200 on
  `signals-kanban-board`.
- Single-sample wall time improved by about 6.1% on large-each relative to the
  target-scope-set probe and about 8.5% on kanban. Host allocation bytes are
  effectively flat on large-each and slightly lower on kanban.
- Event descriptor swap-removal reduces incidental event-id churn: the
  identity-stress filter path now rebinds one moved tail event instead of every
  later compacted event row. The visible DOM and row reuse/remove assertions are
  unchanged.
- The remaining measured structural scan work is now dominated by
  `splice + render_scope`: 537160 scans on large-each and 153440 on kanban.
  The render-index refresh tail is also still visible at 90920 / 17680.

Splice touched-parent collection was folded into the target render-node pass on
2026-06-27. The splice path now records removed elem ids and candidate touched
parents in one full render-node walk, then filters the small parent candidate
list against the completed removed-elem set.

| case | total_ns | stream_nodes_scanned | remove_target | render_scope | splice | events | dirty_scope | render_indexes_refreshed | host_alloc_bytes_this_event |
| --- | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: |
| signals-large-each-64 | 706742673 | 411920 | 5600 | 236360 | 151120 | 12520 | 6320 | 90920 | 198930940 |
| signals-kanban-board | 535414668 | 150260 | 3200 | 77800 | 39120 | 28300 | 1840 | 17680 | 90282660 |

Outcome:

- `stream_nodes_scanned_splice` fell from 300800 to 151120 on large-each and
  from 75640 to 39120 on kanban.
- This is a clear attribution win but not yet the full range-index fix. The
  remaining largest measured buckets are `render_scope` plus the remaining
  splice range discovery: 387480 scans on large-each and 116920 on kanban.
- Single-sample wall time is mixed: large-each is noisier/slower in this sample,
  kanban improves from 544532300 to 535414668. Keep using scan counters for
  direction and median-of-N for timing claims.

Splice target range discovery was then changed to consume the explicit
`render_insert_index` passed by the caller on 2026-06-27. Instead of scanning
the whole active render stream to rediscover the target range, splice walks from
the insertion point through the contiguous target range and stops at the first
non-target boundary node.

| case | total_ns | stream_nodes_scanned | remove_target | render_scope | splice | events | dirty_scope | render_indexes_refreshed | host_alloc_bytes_this_event |
| --- | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: |
| signals-large-each-64 | 699562249 | 265020 | 5600 | 236360 | 4220 | 12520 | 6320 | 90920 | 198930940 |
| signals-kanban-board | 549882230 | 116660 | 3200 | 77800 | 5520 | 28300 | 1840 | 17680 | 90282660 |

Outcome:

- `stream_nodes_scanned_splice` fell from 151120 to 4220 on large-each and from
  39120 to 5520 on kanban.
- Total measured stream scans are now dominated by `render_scope`:
  236360 / 265020 on large-each and 77800 / 116660 on kanban.
- Timing remains noisy in single samples: large-each returns near the
  elem-owned-removal reading, while kanban is slower than the previous sample.
  The scan attribution is the reliable signal here.

Each-row splices now use explicit per-event row render ranges, captured on
2026-06-27. The render-order scan already needed for each diff classification
now returns `[scope_id, render_start, len]` segments. Row splices build a local
range map from those segments and update it after each splice, so insertion
points consume explicit range data instead of repeatedly scanning the active
render stream for first/last nodes in a row scope.

| case | total_ns | stream_nodes_scanned | remove_target | render_scope | splice | events | dirty_scope | render_indexes_refreshed | host_alloc_bytes_this_event |
| --- | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: |
| signals-large-each-64 | 697220749 | 43940 | 5600 | 15280 | 4220 | 12520 | 6320 | 90920 | 200502620 |
| signals-kanban-board | 551420216 | 69800 | 3200 | 30940 | 5520 | 28300 | 1840 | 17680 | 90407940 |

Outcome:

- `stream_nodes_scanned_render_scope` fell from 236360 to 15280 on large-each
  and from 77800 to 30940 on kanban.
- Total measured stream scans fell to 43940 on large-each and 69800 on kanban.
  The largest remaining counted buckets are now event scans and the residual
  render-scope scan.
- The local range map adds transient allocation: host allocation bytes rose by
  about 1.6MB on large-each and 0.1MB on kanban in this single sample. This is a
  good candidate for the next scratch-buffer pass once the remaining structural
  tail is understood.
- The biggest remaining non-counted structural tail is now
  `render_indexes_refreshed`, especially on large-each at 90920.

Outcome:

- `signal_record_table_rebuilt` dropped to zero in the two structural canaries.
- `active_intervals_synced` fell from 141080 to 2600 on large-each and from
  33040 to 2120 on kanban; the remaining count comes from full stream rebuilds,
  not splice-local updates.
- `render_indexes_refreshed` is unchanged, confirming render-index restamping is
  now the only dark splice-tail counter left.
- Wall time improved in this single sample by about 2.5% on large-each and 1.2%
  on kanban, while host allocation bytes moved slightly upward. Treat this as a
  real structural cleanup and measurement win, but not the main scan reduction.

Baseline review outcome:

- Structural `Ui.each` work remains the clear first target, but the hot metric is
  stream scanning around structural patching, not key comparison. The keyed row
  lookup path is now linear. The `children` bucket is removed by explicit child
  links, and the render-owned side of `remove_target` is removed by
  elem-indexed deletion worklists. The larger remaining structural targets are
  now `splice`, `render_scope`, and the render-index refresh tail.
- The child-index slice validated the semantic direction: indexed
  `streamDirectChildren` drops `stream_nodes_scanned_children` to zero and
  reduces total stream scans by the former `children` bucket. The keeper storage
  is sparse render metadata keyed by elem id, not extra fields on the dense
  `HostElemDescriptorIndex`.
- The former dark splice-tail paths are now attributed. The signal-record table
  rebuild and interval graph walk have been removed from splice updates; render
  index refresh remains a measured O(N)-capable tail cost.
- The generated large-N `Ui.each` app is now in the native spec/bench surface at
  N = 8 and N = 64, so the next structural fixes are guarded by N-sensitive
  assertions, not just aggregate six-app samples.
- `stream_nodes_scanned` is the dominant measured structural cost in the
  large-N and kanban apps. The split counters above should be tightened as the
  active stream grows explicit indexes for scope ranges and children.
- Per-event allocations are high, especially in kanban, but they are currently
  mixed with whole-site structural work and retained-value churn. Address
  algorithmic structural gaps before arena/scratch tuning so allocation wins are
  attributable.
- The persistent rank-ordered queue remains a design gap, but the present
  `nodes_recomputed` counts mostly track event count. Prioritise it after the
  structural scaling gates unless implementation work in the structural path
  touches the same dirty-worklist ownership.
- Slot reclamation requires a long-session experiment; the single-sample
  `retained_alloc_delta` table cannot prove or disprove plateau behaviour.
- App-authored keyed-row hashes have been replaced with host-private hashing of
  explicit key text; keep the large-N evidence current as the keyed diff evolves.

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

Current priority after the Phase 4 review:

1. Remove or bound the render-index refresh tail so splices do not restamp from
   the splice point to the end of the render-node table.
2. Reduce event descriptor scans in the structural apply path, especially for
   kanban, after the render-scope range work.
3. Move the per-event each row segment/range-map allocations to scratch if the
   allocation counters stay elevated after the render-index/event work.
4. Revisit scope-owned descriptor removal only if the now-small
   `remove_target` residue grows under a broader app or long-session gate.
5. Then move remaining transient structural buffers to scratch, starting with
   making `streamDirectChildren` allocation-free for callers.
6. Keep the long-session leak/plateau gate in the loop for monotonic identity
   and dense `_by_elem_id` tables.
7. Keep scoped browser command-wire string dedupe as a measured hypothesis, not
   as active work, until wire byte/decode counters show it is larger than the
   remaining structural tail.

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

- **Status:** implemented in `engine.zig`. Structural splice now removes,
  rewrites, and appends sink-route entries as descriptor tables change, and
  active signal record `swapRemove` moves route buckets with the moved record id.
  The large-N app now gates active graph rebuilds by changed rows: append/filter
  allow four records for one created row, remove allows zero, and re-expand
  allows four records per created row.
- **Hypothesis:** patching sink routes only for records in the spliced subtree —
  rather than re-walking the whole active stream — makes structural work scale
  with the change, not the tree, as `DESIGN.md` requires.
- **Why we suspected it:** the old splice path re-walked every signal-bearing
  table of the entire active stream to rebuild every sink route even for a
  one-row change. That specific gap is closed; do not reintroduce a full
  `rebuildActiveSinkSignalRoutesFromStream` call on splice.
- **How we'll know:** `expect_metric_delta active_graph_records_rebuilt` on a
  single-row splice in the large-N app is bounded by the changed set, not N — the
  counter `DESIGN.md` designed specifically to fail this path. The assertion is
  the gate; wiring it is part of the item.

### Structural attribution counters for dark O(N) splice work

- **Status:** implemented. Runtime metrics and benchmark CSV now expose
  `signal_record_table_rebuilt`, `active_intervals_synced`, and
  `render_indexes_refreshed`, all gated by the existing `metrics` build option.
- **Hypothesis:** some remaining one-row structural splice cost is hidden outside
  `stream_nodes_scanned`: the splice tail still rebuilds the active stream's
  signal-record token table, syncs active intervals by walking the whole signal
  graph, and refreshes render indexes from the splice point to the end of the
  render-node table.
- **Why we suspect it:** these paths are whole-table/whole-graph walks adjacent
  to the measured splice path, but the current scan split only attributes
  descriptor-stream scans. That makes the optimization backlog partially blind.
- **How we'll know:** add metrics for `signal_record_table_rebuilt`,
  `active_intervals_synced`, and `render_indexes_refreshed`, then rerun the
  large-N and kanban benchmarks before promoting the next large structural
  change. These counters should be gated by `build_options.metrics`.

### Incremental signal-record table and interval sync on splice

- **Status:** implemented for splice-local updates. Signal-record token lookup is
  maintained from explicit descriptor-tree owner counts, and interval sources are
  started/cancelled from active graph retain/release. Full stream rebuilds still
  use the existing graph-wide interval sync path.
- **Hypothesis:** maintaining the active stream signal-record token table and
  active intervals for only removed/inserted structural descriptors removes
  hidden O(total signal-bearing descriptors) and O(total signal graph records)
  work from every splice.
- **Why we suspect it:** `rebuildActiveStreamSignalRecordTable` clears and
  re-walks all signal-bearing descriptor tables after each splice, and
  `syncActiveIntervalsFromGraph` walks the entire active signal graph. These are
  the same clear-and-rebuild pattern already removed from sink routes.
- **Result:** `signals-large-each-64` now reports
  `signal_record_table_rebuilt=0` and `active_intervals_synced=2600`; kanban
  reports `0` and `2120`. The non-zero interval values are full-rebuild/init
  residue. The next dark tail is `render_indexes_refreshed`.

### Scope-owned descriptor removal index

- **Status:** partially implemented for render-owned descriptors; no longer the
  next major target.
- **Hypothesis:** a maintained scope-to-owned-descriptor index makes
  `removeActiveNonRenderDescriptorsInTarget` scale with the replaced subtree
  instead of linearly scanning and compacting every descriptor table.
- **Why we suspected it:** before elem-indexed deletion, the `remove_target`
  bucket was the largest measured structural bucket on `signals-large-each-64`.
  It came from the many
  remove-in-target helpers scanning descriptor tables and asking whether each
  descriptor belongs to the replacement target.
- **How we'll know:** add or tighten large-N assertions so a one-row splice's
  `stream_nodes_scanned_remove_target` is bounded by changed descriptors, not
  total active descriptors. Precomputing the replacement-subtree membership set
  should remove repeated ancestry walks inside the removal loops.
- **Probe result:** precomputing a replacement-target scope membership set once
  per splice is valid and removes repeated ancestry walks from descriptor
  removal checks, but it does not reduce `stream_nodes_scanned_remove_target`
  because the descriptor tables are still scanned and compacted. Single-sample
  benchmark after the splice-tail commit: `signals-large-each-64` total
  `743778712` with `remove_target=451840`; `signals-kanban-board` total
  `595055581` with `remove_target=128520`. Keep the target-scope set as a
  supporting structure, but the next real win is explicit descriptor owner
  indexes/removal worklists.
- **Implemented slice:** render-owned descriptor tables now use explicit
  elem-id descriptor indexes and scratch removal worklists. This drops
  `remove_target` to `5600` on large-each and `3200` on kanban.
- **Remaining decision:** a persistent scope-owned descriptor index would address
  the residual scope-owned scans, but current measurements make it smaller than
  `splice`, `render_scope`, event scans, and render-index refresh. Do not promote
  this again until a benchmark shows the residue growing.

### Scope render-range index

- **Hypothesis:** maintaining `scope_id -> [render_start, render_end)` for active
  scope subtrees collapses the current render-range discovery scans used by
  splice and render-scope lookup.
- **Why we suspect it:** `splice` and `render_scope` together are more than 40%
  of the large-N stream-scan split. After elem-indexed descriptor removal, they
  are the largest remaining measured scan buckets: `300800 + 236360` on
  `signals-large-each-64` and `75640 + 77800` on `signals-kanban-board`. Both
  answer the same ownership question: which contiguous render-node range belongs
  to this scope subtree?
- **Implemented slice:** splice's touched-parent collection now shares the
  target render-node pass. `stream_nodes_scanned_splice` is down to `151120` on
  large-each and `39120` on kanban.
- **Implemented slice:** splice now consumes the explicit `render_insert_index`
  and scans only the contiguous target range. `stream_nodes_scanned_splice` is
  down again to `4220` on large-each and `5520` on kanban.
- **Implemented slice:** each-row structural splices now reuse explicit
  per-event row render segments and update a local range map after each splice.
  `stream_nodes_scanned_render_scope` is down to `15280` on large-each and
  `30940` on kanban.
- **Remaining decision:** a persistent active scope range index could remove the
  residual render-scope scans, but the next measured targets are now
  `render_indexes_refreshed` and event descriptor scans. Revisit the persistent
  range index only if the residual bucket grows again or overlaps with the
  render-index refresh fix.

### Per-cycle scratch/arena to remove dispatch allocation churn

- **Status:** partial. `EngineScratch` now owns reused buffers for pure reorder
  child-index/LIS work, debug render-cache `seen`/expected-child checks,
  descriptor-collection binder stacks, and inline `Ui.each` key arrays. Native
  and wasm teardown release the retained scratch storage. The native host now
  exposes separate `host_allocs_this_event`, `host_deallocs_this_event`,
  `host_alloc_bytes_this_event`, `host_dealloc_bytes_this_event`,
  `host_retained_alloc_delta`, and `host_retained_bytes_delta` metrics for total
  native host-managed allocator traffic, while the existing `allocs_this_event`
  and `deallocs_this_event` metrics remain the Roc ABI request counters inside
  that total. Runtime telemetry is behind the comptime `metrics` build option so
  non-metrics host builds take the direct allocator path and skip counter bumps.
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
- **How we'll know:** first make `streamDirectChildren` support a caller-owned
  buffer or iterator so indexed child walks do not allocate an owned slice by
  default. Then the long-session `host_retained_alloc_delta` /
  `host_retained_bytes_delta` gauges flatten earlier, and per-event
  `host_allocs_this_event` / `host_alloc_bytes_this_event` deltas for the named
  buffers drop to zero after warmup; specs stay green. Roc heap churn continues
  to be categorized separately by `allocs_this_event` / `deallocs_this_event`.

### O(1) identity/descriptor lookup (kill linear-scan-by-id)

- **Status:** node/elem descriptor lookup slices and keyed row-site indexes have
  landed. Render-child indexing has also landed using sparse render metadata
  keyed by elem id rather than widening the dense elem descriptor index.
  `HostNodeDescriptorStream` maintains explicit
  `node_id -> scope_site/state/when/each` and `elem_id -> descriptor` indexes;
  the engine maintains `node_id -> state cell` indexes for runtime state; and
  signal record reuse now uses an explicit `token -> record` map. Keyed
  `Ui.each` row diffs consume an engine-owned active row-site table keyed by
  `(parent_scope_id, site_ordinal)`, with per-site `key_hash -> row` heads and
  linked rows retained across diffs. The direct lookup helpers
  (`stateIndexByNodeId`, `activeScopeSiteByNodeId`,
  `activeWhenIndexByNodeId`, `activeEachIndexByNodeId`,
  `streamNodeIdInReplacementTarget`, `streamNodeIdInScopeSubtree`,
  `findElementDesc`, `findTextNodeDesc`, `findSignalTextNodeDesc`,
  `streamHasTextField`, `streamHasBoolField`, `signalRecordByToken`, and
  `activeEachRowScopes`) no longer scan descriptor tables or the scope forest.
  Indexed child collection no longer scans render nodes. Remaining work is the
  render-node subtree scans and render-range lookup.
- **Hypothesis:** finishing explicit active render-node/subtree ownership indexes
  — maintained on insert/remove, the way `descriptor_indexes_by_elem_id` already
  is — plus restricting structural patching to the spliced subtree, remove the
  O(render_nodes²) behaviour in the structural patch paths.
- **Why we suspect it:** `DESIGN.md` forbids answering "what id is this record?"
  or "what descriptor owns this elem_id?" by walking a list. The descriptor and
  token lookups now follow that rule, but the remaining subtree and child
  collection paths still walk active render nodes to rediscover ownership. The
  accelerator pattern already exists in the file; the remaining work is applying
  it to those ownership questions without changing behaviour.
- **How we'll know:** child indexing removes the `children` bucket without
  increasing host allocation bytes materially from the keyed-row-site baseline.
  The later scope-range work adds `stream_nodes_scanned` assertions on a
  single-row update in the large-N app; the counter must be bounded by the
  changed set, not by N.

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

- **Status:** implemented by `test/signals/apps/generate_large_each.py`, with
  generated N = 8 and N = 64 fixtures in the native spec suite and the N = 64
  fixture in the benchmark surface. Future structural scaling work should
  tighten these budgets; do not weaken them without measured evidence.
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

### Host-private keyed-row hashing

- **Hypothesis:** replacing the app-authored `key -> U64` hash thunk with
  `Ui.each_str` key material keeps keyed diff lookup O(L) without exposing hash
  finalization to Roc apps.
- **Why we suspect it:** the host still builds the same hash index, but now hashes
  explicit key text privately and stores existing row hashes on row scopes.
- **How we'll know:** `each_key_compares` still tracks L (not L²) under churn and
  large-N reorder tests do not re-run row bodies or rebuild the each site.

### Scoped command-wire string dedupe and JS decode cache

- **Hypothesis:** a per-drain string table for the wasm command wire, paired with
  a JS decode cache keyed by explicit string identity or `(offset, len)`, reduces
  command-buffer bytes, host-side string copies, and repeated UTF-8 decode work
  during initial mount and structural splices.
- **Why we suspect it:** fixed string commands currently append every occurrence
  into `roc_ui_string_buffer_*`, dynamic attr records inline each `name` and
  `value`, and JS decodes every referenced byte slice independently. Repeated
  tags, metadata names (`role`, `aria-label`, `data-testid`, `class`), and
  repeated class/text attribute values are common in large rows and dashboards.
  Ordinary unchanged signal text is already pruned before command emission, so
  the expected win is command-wire churn for patches that really are emitted, not
  reactive equality or DOM-churn avoidance.
- **Constraint:** scope this to the browser command wire and per-drain JS cache.
  Do not globally intern Roc `Str`, `HostValue` payloads, keyed-row values, or
  capability-owned data. Do not replace keyed-row typed equality with intern ids;
  keyed rows continue to use host-private hashes plus typed equality for
  collisions and duplicate-key checks.
- **How we'll know:** add metrics for fixed string bytes, dynamic string bytes,
  deduped string-table bytes, string table hits/misses, and JS decode-cache
  hits/misses. Promote the work only if large-N mount/splice or attr-heavy app
  samples show material byte/decode reduction without increasing retained host
  memory or weakening the wire contract.

### Long-session leak experiment

- **Hypothesis:** retained memory over a long session is flat after warmup, and an
  O(1) allocation-free path keeps session cost linear rather than O(allocs²).
- **Why we suspect it:** per-iteration `retained_alloc_delta` cannot establish
  long-session flatness; only a sustained reuse of one `HostEnv` across many
  events can.
- **How we'll know:** reuse one `HostEnv` across many dispatched events and assert
  the live `allocs − deallocs` gauge is flat after warmup.

### Benchmark-gate breadth

- **Hypothesis:** extending the benchmark/spec surface beyond the maintained
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

- Pre-commit tidy gate:
  `zig build run-check-tidy`
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
  `node --test test/signals/browser/runtime_contract.test.mjs`
  `node --test test/signals/browser/wasm_memory_views.test.mjs`
  `node --test test/signals/browser/controlled_input_policy.test.mjs`

For doc-only updates, `git diff --check` is enough.
