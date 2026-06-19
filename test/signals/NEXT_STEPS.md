# Signals Migration — Next Steps

This document evaluates the current implementation against the target design in
`DESIGN.md` and lays out the recommended sequence to finish the migration. Read
`DESIGN.md` first; this file assumes its vocabulary (sources, derived nodes,
scopes, confined erasure, push-based propagation).

## Where We Are

Substantial structural progress has been made toward the target. What already
matches the design:

- **The host owns the reactive runtime mechanics.** `host.zig` builds per-signal
  `rank` (topological height), `signal_dependents` adjacency, and a
  rank-ordered dirty queue (`appendSignalAndDependents`,
  `dirtySignalIdsForEvent`, the rank-keyed insertion at lines ~1190). This is the
  push-based, glitch-free scheduler the design calls for, living in the host.
- **The full-re-render dispatch is gone.** The single `roc_ui_dispatch` that
  re-walked the tree has been replaced by the split protocol: `roc_ui_init`,
  `roc_ui_recompute`, `roc_ui_render`, `roc_ui_drop`.
- **Identity is dense integer ids at the boundary.** Signals carry a
  `Graph.SignalIdentity.RegisteredSignal(U64)`; events carry integer ids; the
  host routes by id. Descriptors (`SignalDesc`, `EventDesc`, `StateDesc`,
  render descriptors) cross the boundary as explicit data, and the host indexes
  them by id rather than scanning strings at dispatch time.
- **Recompute is scoped to dirty signals.** `recompute` evaluates only the
  `dirty_signal_ids` plan the host sends, and `eval_signal` consults host-sent
  caches (`cached_signals`, `cached_states`) instead of re-deriving the world.

This is the hard part of the engine, and it is largely in place. The remaining
work is concentrated in three areas: the **app-facing API still exposes the old
surface**, **identity is still string-keyed underneath the integer ids**, and
**erasure / dynamic lifecycle are not yet at the target**.

## Gap Analysis (current vs. `DESIGN.md`)

Ordered roughly by how much of the design they block.

1. **String-key identity persists.** `register_state_key` and
   `register_signal_descriptor` still key state slots and the signal registry by
   author-written `Str` keys (`StateSlot.key`, `SignalRegistryEntry.key`,
   `signal_registry_lookup` doing linear `entry.key == key` scans). The integer
   ids are assigned *on top of* string keys; the strings are still the real
   identity and still drive dedup. The design requires construction-site identity
   within explicit scopes, with **no string keys anywhere**.

2. **App API is the old `Reactive` / `Elem` surface.** All three apps still
   import `Reactive`, `NodeValue`, call `Reactive.Signal.fold_i64`,
   `map_i64_str_keyed`, `unit_channel`, `Elem.each`, and hand-write
   `Str.concat("line_quantity:", id)` keys. The target `Signal` / `Html` / `Ui`
   modules (with `Signal.state`/`send`, `Html.on_click(msg)`,
   `Ui.each(items, key_fn, |key, row| ...)`) do not exist yet.

3. **`NodeValue` is still the value representation, with `encode`/`decode` in
   app code.** Every app type defines `encode`/`decode` against `NodeValue`, and
   `eval_signal` decodes erased values with `crash`-on-mismatch wrappers. The
   design requires confined per-edge erasure (per-edge `is_eq` and, where a value
   must serialize, `encode`/`decode` thunks resolved by static dispatch on the
   edge's value type and pinned to it) so a mismatch is a compile error, not a
   runtime crash.

4. **The combinator zoo still exists.** `MapI64I64`, `MapI64Str`, `Map2I64I64`,
   `Map2I64I64Str`, `FoldI64`, `FoldBoolToggle`, and the `*_keyed` variants are
   still distinct `Graph.SignalExpr` constructors and distinct `Reactive`
   functions. The design collapses these to a few polymorphic functions
   specialized by monomorphization.

5. **Dynamic lists do not retain per-row scopes.** `Each`/`DynamicKeyed` in
   `render_elem` still call `key_fn` and discard the result
   (`_ = key_fn(item)`), re-`register_elem` every row on every render, and bump
   `keyed_creates` without real reuse. There is no key-set diff, no per-row
   scope, no `keyed_reuses`/`keyed_removes` accounting. Per-row local state only
   appears to survive because it is *also* string-keyed by `item.id`.

6. **No scope model / lifecycle / disposal.** There is no host-owned scope
   forest. Conditionals (`Dynamic`/`when`) and list rows do not own ids or
   retained closures, so nothing is disposed when a branch flips or a row leaves;
   the registry only grows. The design requires scopes that own ids and
   closures and are disposed deterministically (with the one-refcount-per-live-
   closure leak invariant).

7. **`render_only` still re-renders the whole tree.** After a recompute,
   `roc_ui_render` rebuilds the entire `Elem` tree and re-emits all descriptors;
   the host then diffs. The design wants the host to emit **minimal patches**
   for sinks whose value changed, driven by the dirty set — not a full descriptor
   rebuild per event.

8. **Metrics are partway through the design migration.** `RuntimeMetrics` now
   carries the design counter names (`nodes_recomputed`,
   `propagation_prunes`, `derived_calls_into_roc`, `patches_emitted`, row/scope
   and closure counters). Scope, closure, and true row-reuse counters remain
   zero until later phases output that lifecycle data. Source-level equal-output
   pruning now suppresses downstream recompute work in the Roc recompute plan;
   full per-edge `is_eq` thunk pruning is still pending until confined erasure
   moves typed edge thunks into the runtime.

9. **No effects/subscriptions.** `Signal.from_task`, `Signal.interval`,
   `Ui.on_change`, `Ui.on_cleanup` are unimplemented. (Lowest priority; the
   design treats effects as sources reusing the same path, so they slot in after
   the core is right.)

## De-Risk First (gating experiments)

`DESIGN.md` names three experiments that gate the architecture. Two of them are
now partially answered by the existing host, but should be confirmed *before*
building the new API on top, because if they fail the API shape changes:

- **G1 — Retained closures across FFI.** The design's end state stores boxed Roc
  transform/reducer closures in the host node table and re-invokes only the
  changed ones. The current code does **not** do this yet — it re-sends dirty
  signal *ids* and Roc re-walks those signals' boxed transforms each recompute.
  Before committing to per-node retained closures, run the 10k-closure /
  100k-event retain-and-invoke benchmark from `DESIGN.md` and confirm
  refcount-flat RSS and acceptable ns/call. If per-call FFI is too costly, keep
  the **batched** `ui_recompute` (it already passes a plan list) as the
  permanent protocol rather than one-call-per-node.
- **G2 — Construction-site identity under dynamic shape.** This is the riskiest
  open item because the codebase has *not* started it (still string-keyed). A
  first `identity_stress` fixture now runs in `zig build run-test-signals` and
  proves row-local state survives reorder through `when -> each -> when`.
  Broader branch disposal/filter assertions still need to be added as
  scope-relative minted ids replace the string tables.
- **G3 — `is_eq`-pruned glitch-free propagation.** The rank scheduler exists and
  the Zig host has the diamond test (`a->b`, `a->c`, `(b,c)->d`) asserting the
  join appears once in the dirty plan. The ops dashboard now includes a
  high-fan-out no-op source whose unchanged output prunes all downstream labels
  (`nodes_recomputed +1`, `propagation_prunes +1`, `patches_emitted +0` for the
  scripted click). Full per-edge `is_eq` pruning is deferred to the confined
  erasure phase, where typed edge thunks exist.

Also confirm the two maturity unknowns: that static dispatch on
`is_eq`/`encode`/`decode` methods resolves and monomorphizes across the platform
boundary (there is no ability auto-derivation, so each value type must define
those methods), and monomorphization/compile-time behavior of deeply nested
generic `Signal`/`Elem` combinators.

## Recommended Sequence

Each phase ends green: `roc check` on the apps plus `zig build run-test-signals`
passing. Do not advance until the current phase's section passes (per the
`minici` discipline in `AGENTS.md`). Keep the three representative apps and their
`.txt` specs as the regression oracle throughout.

### Phase 1 — Lock in the host engine and metrics (low risk, high leverage)

Goal: make the scaling claim measurable before changing semantics.

Status as of 2026-06-19: the metrics surface has been replaced and
`zig build run-signals-bench` records baseline rows with the new counter names.
The host has a Zig diamond dirty-plan test wired into `run-test-zig`, and the
ops dashboard script has a high-fan-out no-op source A/B that proves equal
source output prunes downstream work and patches. The remaining pruning work is
the fuller design target: per-edge `is_eq` thunk pruning once confined erasure
provides typed edge thunks.

- Replace the old-model `RuntimeMetrics` fields with the design's counters:
  `nodes_recomputed`, `propagation_prunes`, `derived_calls_into_roc`,
  `recompute_batches`, `patches_emitted`, `scopes_created`, `scopes_disposed`,
  `rows_reused`, `rows_created`, `rows_removed`, `closure_retains`,
  `closure_releases`, `retained_alloc_delta`. Keep `events_processed`.
- Keep the G3 diamond test and high-fan-out pruning A/B green while later phases
  move pruning from `NodeValue` equality to typed `is_eq` thunks.
- Establish baseline benchmark numbers (`zig build run-signals-bench`) so every
  later phase can be checked for regressions.

Exit: metrics reflect current changed-node work; baseline recorded.

Baseline recorded on 2026-06-19 (`zig build run-signals-bench`, 20 iterations,
1 sample):

| case | actions | nodes_recomputed | propagation_prunes | derived_calls_into_roc | recompute_batches | patches_emitted | retained_alloc_delta |
| --- | ---: | ---: | ---: | ---: | ---: | ---: | ---: |
| signals-ops-dashboard | 120 | 4060 | 40 | 1240 | 120 | 7780 | 2980 |
| signals-kanban-board | 80 | 2980 | 0 | 560 | 80 | 5280 | 2460 |
| signals-checkout-wizard | 180 | 1500 | 0 | 440 | 180 | 4840 | 2220 |

### Phase 2 — Replace string identity with minted scope-relative ids

This is the structural keystone and the biggest correctness risk; do it before
the API rewrite so the new API never exposes keys.

- Keep G2's `identity_stress` harness green and extend it as scope boundaries
  land. It currently covers row-local state through `when -> each -> when` plus
  reorder; branch disposal and filter assertions remain to be added once the
  scope model exists.
- Move identity assignment fully into graph ingestion: the platform threads a
  node-id counter during pure construction (state ids, signal ids, event ids,
  scope ids) and emits dense descriptors. Delete `StateSlot.key`,
  `SignalRegistryEntry.key`, `signal_registry_lookup`'s string compare, and
  `register_state_key`/`register_event_key` string dedup.
- Introduce explicit **scope** boundaries in the descriptor stream
  (`root`, conditional branch, list row) so identity is construction-site
  *within a scope* and sibling scopes are insulated from positional shifts.
- The host already indexes by integer id; remove the string tables it still
  carries.

Exit: no `Str` key participates in identity; apps still pass (they may still
build keys at the call site — those become inert and are removed in Phase 3).

### Phase 3 — New app-facing API: `Signal`, `Html`, `Ui`

Build the target surface and port the apps.

- Create `Signal.roc` (opaque `Signal(a)` over a node id; `state`/`send`, `const`,
  `map`, `map2`, `combine`), `Html.roc` (`div`/`button`/`input`/`text`/`text_s`,
  signal-backed attrs, `on_click`/`on_input`/`on_check` taking typed messages),
  and `Ui.roc` (`when`, `each(items, key_fn, |key, row| ...)`, `component`).
- `Ui.each` carries a `where [key.hash : ..., key.is_eq : ...]` static-dispatch
  constraint on its key type; no string keys. The key type provides `hash` and
  `is_eq` through its `.{ }` method block.
- Port `ops_dashboard`, `checkout_wizard`, `kanban_board` to the new API,
  deleting every `Str.concat("...:", id)` key and every `unit_channel`/`fold_i64`
  call. Re-run their `.txt` specs unchanged (the specs assert user-facing
  behavior, so they are the migration oracle).

Exit: apps import only `Signal`/`Html`/`Ui`; specs pass; `Reactive`/`Elem` are
no longer imported by apps.

### Phase 4 — Confined erasure; delete `NodeValue` from app code

- Resolve per-edge `is_eq` (and, where a value must serialize, `encode`/`decode`)
  thunks by static dispatch on the surrounding `Signal(a)`'s value type, pinned
  at the call site and specialized by monomorphization at the platform-glue
  layer. The host stores boxed opaque values and calls only that edge's thunk; it
  never selects a decoder.
- Remove the `NodeValue` `encode`/`decode` boilerplate from app types; instead
  the value type defines `is_eq` (and a key type also defines `hash`) in its
  method block.
- Remove the `crash`-on-decode-mismatch wrappers; a mismatch is now a type error.

Exit: no app defines `NodeValue` conversions; no `crash` on value type mismatch;
static dispatch on the `is_eq`/`encode`/`decode` methods confirmed resolving and
monomorphizing across the boundary.

### Phase 5 — Collapse the combinator zoo

- Reduce `Graph.SignalExpr` to the minimal generic node kinds (source, map,
  map2, combine) plus the value-carrying constants; delete `MapI64I64`,
  `MapI64Str`, `Map2I64I64`, `Map2I64I64Str`, `FoldI64`, `FoldBoolToggle`, and
  all `*_keyed` registration paths. Monomorphization specializes the generic
  `map`/`map2` for `I64`/`Str`/`Bool` automatically.
- Regenerate `roc_platform_abi.zig` glue and re-fix any ABI struct-size asserts.

Exit: one polymorphic family in the API and a small generic node set in `Graph`;
specs pass; compile-time monomorphization cost confirmed acceptable.

### Phase 6 — Scopes, dynamic lists, and minimal patches

This delivers the headline feature (real keyed reuse) and true incremental render.

- Implement the host scope forest: each conditional branch and list row is a
  scope owning its minted ids and retained closures.
- `Ui.each`: host diffs the new typed key-set against the old; reuse surviving
  row scopes (and their local state), mint new ones, dispose removed ones (drop
  one refcount per retained closure, run `on_cleanup`, detach DOM, count
  `rows_removed`). Make `rows_reused` real.
- `Ui.when`: dispose the losing branch scope on flip; keep-alive only via an
  explicit flag.
- Replace `render_only`'s full-tree rebuild with **minimal patch emission**: the
  host emits `SetText`/`SetValue`/`SetChecked`/`SetDisabled`/attr patches only
  for sinks whose value changed in the dirty set, rather than rebuilding all
  descriptors and diffing.

Exit: `rows_reused`/`rows_disposed` reflect actual reuse; reorder/filter
preserves per-row state; `patches_emitted` and `nodes_recomputed` track changed
nodes, not tree size (verify against Phase 1 baseline — this should show the
biggest win).

### Phase 7 — Effects and subscriptions

- Add `Signal.from_task` (yielding `[Loading, Done a, Failed err]`),
  `Signal.interval`, `Ui.on_change`, `Ui.on_cleanup`.
- Effects are host-owned sources: a resolved task/timer sets its source node and
  runs the same propagation path as a user event. Request identity is
  scope+node, not app strings; disposing a scope cancels in-flight work.
- Add a fixture app exercising an async flow and a fake-result injection in the
  test host.

Exit: async results flow through one scheduler; cancellation on disposal works;
fixture spec passes.

### Phase 8 — Docs and cleanup

- Delete `Reactive.roc`, `Elem.roc`'s dynamic/erased constructors, `NodeValue.roc`,
  and the old `UiRuntime` evaluator once nothing references them.
- Update `README.md` (it still references `UiRuntime.roc`, `NodeValue`, and
  string keys) to match `DESIGN.md`/`GUIDE.md`.
- Confirm `GUIDE.md` examples compile against the shipped API.

## Success Criteria (from `DESIGN.md`)

The migration is complete when, measured against the Phase 1 baseline:

- **Update amplification:** `nodes_recomputed` and `patches_emitted` per event
  track the number of *changed* nodes, not graph or tree size.
- **Per-event Roc calls:** bounded (one `recompute` plan + batched evaluation,
  plus one render), independent of tree size.
- **Allocations:** flat per event and under list churn; surviving rows reused.
- **Retained memory:** `retained_alloc_delta` near zero after warmup over a long
  session.
- **Row reuse:** `rows_reused` reflects real subtree reuse; per-row state
  survives reorder/filter.
- **Type safety:** no app `encode`/`decode`; no `crash` on value type mismatch.
- **No strings as identity:** no `Str` key participates in state/signal/event
  identity.
- **Determinism:** each app spec produces the same patch sequence every run.

## Risks and Watch-Items

- **Phase 2 is the highest-risk step.** Swapping string identity for
  construction-site/scope identity can silently move state between rows/branches.
  Gate it on G2 and tag every state node with its minted id + scope path in the
  stress harness.
- **Phases 4–5 depend on Roc static-dispatch and monomorphization maturity**
  across the platform boundary. If method resolution/monomorphization for the
  per-edge `is_eq`/`encode`/`decode` thunks is not ready, the batched
  opaque-value protocol can carry the migration further than per-node typed
  closures while that matures; keep that protocol as an explicit design option.
- **Keep apps + specs green every phase.** They are the only end-to-end oracle;
  a phase that breaks a spec is not done.
- **Watch `roc_platform_abi.zig` struct-size asserts** after any boundary type
  change (Phases 2, 4, 5, 7) — regenerate glue and re-fix the `comptime` size
  checks rather than hand-editing.
