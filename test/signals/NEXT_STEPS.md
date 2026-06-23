# Signals — Next Steps

`DESIGN.md` is the enduring target design (one engine, two thin hosts; native is
the spec/telemetry/debug host, wasm is the browser boundary; the same apps run in
both). This file is the live work queue: the ordered drive toward that design,
plus the optimisation backlog held behind it. Completed phase notes and retired
findings belong in git history, not here.

## Drive order

The goal of this phase is to bring all our apps online — running in the browser
alongside the native spec test runner — on top of a fully host-agnostic engine.
We deliberately defer further optimisation of the implementation until:

1. the refactor that finishes the engine extraction is complete,
2. the architecture and abstractions have been reviewed (are the seams good, do
   we have good unit tests), and
3. baseline measurements have been updated and reviewed to inform decisions.

Only then do we re-prioritise the optimisation backlog. The phases below are in
order; take the earliest unfinished one.

## Phase 1 — Finish the engine extraction (refactor, no behaviour change)

The engine is already factored on the native side for the non-structural path,
scheduler, dirty propagation, keyed-row plan, render sink, and scalar/topology
caches; the wasm host drives the engine for the non-structural path. The
remaining gap is the **structural orchestration still living in
`native_host.zig`**, which is the seam that must be settled before the browser
can drive structural behaviour without duplicating algorithms or rebuilding whole
trees (both forbidden by `AGENTS.md`).

This is a refactor with no behaviour change. The native specs are the regression
guard; `Engine(WasmCtx)` must keep compiling under `wasm32`. There is not much
code, so this is a quick, mechanical move done as small green commits.

Move behind `Engine(comptime Ctx)` + `Ctx.sink()`:

- [ ] Structural descriptor collection — root/active collection, `Ui.when`,
      `Ui.each`, components, cleanup, on-change branch/row descriptor gathering.
- [ ] Active-stream splice/copy — replacing scopes, each-site targets, subtree
      descriptor copying, active-stream splicing.
- [ ] Structural apply orchestration — dirty structural signal handling,
      each-row splice/move decisions, structural node application.
- [ ] Structural event rebinding — rebinding row/branch events by changed slot.
- [ ] Effect-source dispatch — routing effect/timer source updates through the
      one propagation queue.

Leave host-specific only:

- [ ] Native: the spec runner, simulated-DOM sink, allocation ledger, telemetry
      surfaces, lldb-friendly debug affordances.
- [ ] Wasm: linear memory, command-buffer exports, event-payload marshalling, the
      JS runtime contract.

Done when: `engine.zig` owns the structural collection/splice/apply/rebind path;
no reactive or structural logic remains in either host file; native specs and the
wasm build stay green.

## Phase 2 — Bring the app suite online in both hosts

With the engine fully extracted, enable structural behaviour in the wasm host and
get every app in the suite running in the browser *and* under the native spec
runner from the same Roc source.

- [ ] Enable the structural wasm path (`Ui.each`, `Ui.when`, `remove_node`,
      `move_before`, async) now that the structural engine is shared. The
      previously-noted comptime kill switch becomes unnecessary once the path is
      green — remove it rather than leaving a dormant flag.
- [ ] Event-payload accessor path: carry the accessor descriptor in the
      descriptor tree; JS walks it against the live `Event` and serializes only
      the requested leaves into a `roc_alloc`'d buffer, transferring ownership on
      `roc_ui_event`. No payload reconstruction in JS.
- [ ] Structural splicing against a live DOM: `Ui.each`/`Ui.when` detach/move via
      the shared `RemoveNode`/`MoveBefore` commands; surviving rows keep DOM and
      local state; disposed scopes clear `nodes[]`/listeners and drop closures.
- [ ] Async/timers/cancellation in the browser: `roc_ui_resolve` / `roc_ui_timer`
      exports with host-assigned `request_id` / `token`, `AbortController` /
      `clearInterval` driven by host-emitted cancel commands on scope dispose.
- [ ] `serve.py` builds and serves **any** app in the suite (parameterised app
      selection + both backends), not just the counter/demo.
- [ ] Run each of the six apps in the browser as manual QA, and confirm each runs
      green under the native spec runner.

We do **not** add an automated real-browser harness. The native spec runner
already asserts semantics and work budgets — the things the browser cannot show
us — and the JS runtime is a thin wrapper whose only meaningful contract is the
cmd/patch codec. A headless-browser harness would re-test DOM behaviour we do not
own and engine semantics already covered natively, for no added signal.

### JS test surface cleanup (part of Phase 2)

The current browser tests re-prove engine semantics through a DOM double, which
duplicates native coverage across the boundary. Keep only the JS↔WASM contract
guards.

- [ ] Remove `browser/counter_app.test.mjs`, `browser/executor.test.mjs`, and
      `browser/stable_text_app.test.mjs` — these assert engine semantics (count
      changes, one `set_text` per click, pruning, retained-value budget) that the
      native spec runner already owns and asserts more precisely.
- [ ] Keep `browser/wasm_memory_views.test.mjs` (memory.grow view invalidation)
      and `browser/controlled_input_policy.test.mjs` (`SetValue` focus/IME
      policy) — these guard the genuine JS↔WASM boundary contract, not engine
      semantics.
- [ ] Ensure the remaining JS guards cover the codec contract: op-code →
      DOM-op mapping, payload marshalling round-trip, and the view-refresh rule.

## Phase 3 — Architecture and abstractions review

A deliberate review pass, once the app suite is online, before any optimisation.

- [ ] Are `Ctx` and `sink()` good seams? Is the `Ctx` contract minimal and
      explicit (every decl checked by `verifyCtx`/`verifyRegistryOps`, no duck
      typing)?
- [ ] Is the native/wasm split clean — does anything reactive or structural still
      leak into a host file?
- [ ] Do we have good unit tests at each seam (engine, scope tree, keyed rows,
      identity table, host-value registry, render sink), or are seams only covered
      transitively through app specs?
- [ ] Record the review outcome and any seam changes; fix seam defects here rather
      than carrying them into the optimisation phase.

## Phase 4 — Update and review baseline measurements

- [ ] Re-run `run-signals-bench` across all six apps and record fresh baselines.
- [ ] Capture the current values of the work counters (`nodes_recomputed`,
      `patches_emitted`, `active_graph_records_rebuilt`, `stream_nodes_scanned`,
      `each_key_compares`, per-event allocation deltas) for the representative
      events of each app.
- [ ] Review the baselines together: which paths are actually expensive, and on
      what inputs? This review is what turns the optimisation hypotheses below
      from suspicion into prioritised, evidence-backed work.

## Phase 5 — Re-prioritise the optimisation backlog

After the baseline review, re-prioritise the items below using the measured
evidence. Each is stated as a falsifiable hypothesis: what we believe it helps,
why we suspect it, and how we will know. Do not promote an item to active work
until the Phase 4 baseline supports its hypothesis; an optimisation without
measured evidence is speculation.

### O(1) descriptor lookup by `elem_id`

- **Hypothesis:** indexing descriptors by `elem_id` (a dense array keyed by elem
  id) and restricting structural patching to the spliced subtree removes
  O(render_nodes²) behaviour in structural patch paths.
- **Why we suspect it:** `findElementDesc` / `findTextNodeDesc` /
  `findSignalTextNodeDesc` / `streamHasTextField` / `streamHasBoolField` are
  linear scans over the stream called inside loops over render nodes, and
  `applySplicedStructuralNodeDescriptorTarget` scans the whole active stream gated
  only by a `seen[]` bit.
- **How we'll know:** add `stream_nodes_scanned` assertions on a single-row update
  in the large-N app; the counter must be bounded by the changed set, not by N,
  before and after — and the baseline must show the scan is actually a hot path
  worth removing.

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
committing. For a refactor slice (Phase 1) the existing native specs are the
regression guard; a behaviour-changing slice must also land the assertion that
locks it in.

- Focused Zig host/engine work:
  `zig test test/signals/src/native_host.zig`
- Shared engine instantiates under wasm32:
  `zig build build-test-hosts -Dplatform=signals -Doptimize=ReleaseSmall`
- Platform Roc or ABI changes (any app):
  `./zig-out/bin/roc check test/signals/apps/<app>.roc`
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
