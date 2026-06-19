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
   modules now exist, but the apps have not been ported to `Ui.state`
   state-handle reducers, `Html.on_click(msg)`, or
   `Ui.each(items, key_fn, |key, row| ...)`.

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

### Confirmed maturity findings (2026-06-19 probe)

A throwaway compile probe (since removed) pinned down three facts on the current
compiler that reshape the new-API surface and the host equality mechanism:

1. **`where [k.hash : k, Hasher -> Hasher, k.is_eq : k, k -> Bool]` static-dispatch
   constraints compile and are satisfiable across the platform boundary.** The new
   keyed-identity API shape is viable.
2. **Key/value nominal types must wrap a single-tag union, not a bare primitive.**
   The `TodoId := U64.{ hash = |TodoId(n), ...| ... }` form previously shown in
   `DESIGN.md` and `GUIDE.md` does **not** typecheck — `TodoId(n)` is rejected
   as a tag-union pattern against the nominal `TodoId` (error:
   `[TodoId(_c), ..]` vs `TodoId`).
   The form that compiles is `TodoId := [Tid(U64)]`, constructed `TodoId.Tid(n)`
   and destructured `match id { Tid(n) => ... }` (matches the passing
   `test/snapshots/eval/custom_wrapper_equality.md`). DESIGN.md/GUIDE.md examples
   have been corrected to this form.
3. **Roc cannot construct or inspect a `Hasher`** (opaque builtin), so Roc never
   runs `hash`/`is_eq` itself. The **host owns hashing and equality**; the `where`
   constraint exists to require the methods exist and let the platform-glue capture
   each key/value type's `is_eq` as a boxed `RocErasedCallable` the host invokes
   (confined erasure). Note: the current host *never invokes* erased callables —
   all transform evaluation happens inside the Roc `recompute`/`render` walk — so
   host-invoked `is_eq` thunks for the keyed-scope diff and value pruning are a new
   host capability (call-convention wiring + per-type trampolines).
4. **A polymorphic `value : v` flowing out of a higher-order scope-body closure
   panics the current compiler** (`canonical type key requested for erroneous
   checked type`). Builder combinators must not be generic in the body's return
   payload; scope bodies return a concrete descriptor carrier.

DESIGN.md and GUIDE.md now reflect these findings: local state is introduced
through `Ui.state(init, |state| ...)`, key examples use nominal-over-single-tag
unions such as `TodoId := [Tid(U64)]`, and the host/equality text describes
captured per-type thunks rather than runtime string keys.

### Identity threading: the host walks, Roc does not thread ordinals (2026-06-19)

A first attempt threaded a pure `BuildCtx { path, ordinal }` through construction
in Roc (a state monad). This is the **wrong** design and was discarded:

- It destroys the nested-expression ergonomics the API needs, and finding (4)
  above shows the generic scope-body it requires also crashes the compiler.
- More fundamentally, the inline `{ signal, send } = Signal.state(0)` previously
  shown in `DESIGN.md`/`GUIDE.md` is **mathematically impossible to give a stable unique
  identity in a pure language**: a pure call with identical arguments must return
  an identical result, so a plain let-bound `state` cannot mint a per-site
  ordinal. State that needs identity must be introduced through an explicit
  **closure binder** (`Ui.state(init, |{ signal, send }| body)` /
  `Ui.component`); the closure boundary is the construction site.

The sound model:

- `build` returns a **pure, immutable descriptor tree**. There is no ordinal
  counter in Roc. The deterministic single-threaded walk that assigns
  construction-order identity lives in the **host** (legitimate: the host owns
  mutation), as a pre-order traversal of that tree.
- **Only identity-bearing descriptors advance the ordinal** within a scope: state
  binders, `when` sites, and `each` sites. Ordinary markup (`div`, `text`,
  static attrs) does **not** consume identity, so cosmetic markup edits never
  shift state identity.
- A scope-path step carries a **per-site ordinal** plus its selector, not just the
  selector: `When(site_ordinal)/Branch(tag)` and `Each(site_ordinal)/Keyed(key)`.
  Without the site ordinal, two sibling `each` blocks with colliding keys would
  alias.
- `Signal.map`/`map2`/`combine` and event sinks carry a **reference** to a state/
  source binder (by its descriptor position); a declaration mints identity, a use
  does not.
- DESIGN.md/GUIDE.md must be corrected: `state` is a closure binder, not a plain
  let value.

## Implementation Status (2026-06-19)

### Done — new Roc app-facing API (compiles, additive, legacy untouched)

The string-free, closure-binder API exists and typechecks end to end. New
platform modules:

- `platform/Node.roc` — pure immutable descriptor tree the host walks. `SignalExpr`
  (`Ref(BinderRef)`/`ConstValue`/`Map`/`Map2`/`Combine`), `Attr`, and the
  identity-bearing `Elem` scope binders `State`/`When`/`Each`. `State` and `Each`
  carry boxed `is_eq` / key-`is_eq` thunks (confined erasure). `Msg` is a type
  alias `{ binder : BinderRef, payload_kind : U64, transform : Box(...) }`.
  Binder refs are de-Bruijn-style (`BinderRef(0)` = nearest enclosing `Ui.state`).
- `platform/Signal.roc` — opaque `Signal(a)`: `const`/`const_i64`/`const_str`/
  `const_bool`, `map`, `map2`, `combine`. Confined erasure (decode input / encode
  output) per edge.
- `platform/Ui.roc` — `state(init, |State(a)| body)` closure binder with
  `signal`/`on_unit`/`on_value`; `when(cond, true_body, false_body)` (branch
  scopes); `each(items, key_fn, |key, row_signal| ...)` with
  `where [k.hash, k.is_eq]` and boxed key equality. Key types are
  nominal-over-tag-union.
- `platform/Html.roc` — `div`/`section`/`heading`/`paragraph`/`text`/`text_s`/
  `button`/`button_s`/`text_input`/`checkbox`.
- `platform/main.roc` — imports + exposes `Signal`, `Html`, `Ui` (exposed modules
  MUST be imported by `main.roc`). The temporary `UiRuntime.roc` placeholder
  aliases that anchored `Node.Elem`/`Signal.Signal` have been removed.

Function types are standardized to 2-param (`NodeValue, NodeValue -> X`), not
tuple-arg, to match the descriptor thunk signatures.

The full G2 pattern (keyed `each` over rows, each row owning a `Ui.state` bump
counter with a `Signal.map` label and `on_unit` reducer) was validated to compile
in a throwaway `apps/api_check.roc` (removed before commit). `roc check` is green
on all four existing apps plus that scratch app.

### Remaining work (the host-side / ABI half)

In dependency order. Each sub-step ends green per `minici` discipline.

1. **UiRuntime ingestion of the `Node.Elem` tree.** Replace the descriptor builder
   so `build` returns `Node.Elem` and the runtime emits a *scope/node descriptor
   stream* from a pure pre-order shape (the host assigns ordinals; Roc does not
   thread a counter). Flip `main.roc` `requires` to `main : {} -> Node.Elem`.
   Delete the string-keyed paths: `StateSlot.key`, `SignalRegistryEntry.key`,
   `signal_registry_lookup` string compare, `register_state_key`/
   `register_event_key`.
2. **Regenerate `roc_platform_abi.zig`** after the boundary types change:
   `./zig-out/bin/roc glue src/glue/src/ZigGlue.roc test/signals/platform test/signals/platform/main.roc`.
   Re-fix the `comptime` struct-size asserts.

   Progress: `Node.Elem` is now reachable in the generated platform ABI through
   `roc_ui_node_abi_probe`, giving the host generated `NodeElem`,
   `NodeSignalExpr`, and `NodeAttr` layouts before the main app boundary flips.
   `Node.SignalExpr.Const` was renamed to `ConstValue` because the generated Zig
   payload field for `Const` collided with the `const` keyword.
3. **Host scope forest + identity walk.** Pre-order walk of the descriptor tree
   that assigns construction-order ordinals to identity-bearing nodes only (state
   binders, `when` sites, `each` sites); ordinary markup does not advance the
   ordinal. Scope path step = `When(site_ordinal)/Branch(tag)` and
   `Each(site_ordinal)/Keyed(key)`. Intern `(scope_path, ordinal)` to dense node
   ids (reuse existing rank/adjacency/dirty machinery). No strings; no `Dict(Str,_)`.

   Progress: `platform/host.zig` now has host-owned scope interning for root,
   `when` branch scopes, keyed row scopes, and dense `(scope_id, ordinal) ->
   node_id` interning. The keyed-row path uses the boxed key equality thunk rather
   than byte/string comparison. The host now has a generated-ABI `NodeElem`
   identity-site walker that advances ordinals only for `State`/`When`/`Each`;
   branch bodies are walked through explicit branch scopes. The host also has an
   owned descriptor stream collector for generated-ABI `NodeElem` trees covering
   DOM elements, static text nodes, signal-backed text nodes/attrs, bool attrs,
   events, and retained `State`/`When`/`Each` site descriptors. The collector
   carries an explicit binder stack, resolves event `BinderRef`s to state node
   ids, records source node ids for signal-backed descriptors, and stores the
   active binder context on scope sites so branch/row bodies can resolve outer
   state refs. The remaining work is wiring this stream into the app init/render
   lifecycle.
4. **Host-invoked `is_eq` thunks (new host capability).** The host currently never
   invokes erased callables. Wire the `RocErasedCallableFn(host, ret, args, capture)`
   call convention + per-type marshaling so the host can call the boxed key-`is_eq`
   for the keyed-row diff and the value `is_eq` for change pruning. Duplicate keys
   within one keyed scope = hard host error (not a silent alias).

   Progress: `platform/host.zig` now has typed helper calls for
   `NodeValue -> NodeValue`, `(NodeValue, NodeValue) -> NodeValue`, and
   `(NodeValue, NodeValue) -> Bool`, with Zig unit coverage that allocates boxed
   erased callables through the real ABI payload/capture path. The host also has
   the `(NodeValue, NodeValue) -> NodeElem` helper needed to invoke `Ui.each` row
   thunks and explicitly release the returned descriptor tree. The remaining work
   is wiring those helpers into keyed-row diffing, reducer dispatch, and
   value-pruning once the host owns the new `Node.Elem` graph.
5. **Keyed-row diff + disposal.** Diff new typed key-set against old via the key
   `is_eq` thunk; reuse surviving row scopes (and their local state) → `rows_reused`;
   mint new keys → `rows_created`; dispose removed keys → drop one refcount per
   retained closure (`decrefErasedCallable`), detach DOM subtree, `rows_removed`.
   `Ui.when` flip disposes the losing branch scope. `keep_alive` only via explicit
   flag.

   Progress: `platform/host.zig` now has recursive scope disposal that retires
   subtree node identities, drops keyed-row keys exactly once, and prevents a
   later matching key from reusing disposed local state. It is not yet wired into
   the `Ui.when`/`Ui.each` descriptor walk or DOM detachment. The host also has a
   typed-key row diff helper that reuses, creates, and disposes row scopes by the
   boxed key equality thunk and records `rows_reused`/`rows_created`/
   `rows_removed`.
6. **Port `identity_stress.roc`** to the new API; get its current `.txt` spec green
   first (the smallest honest slice).
7. **Port `ops_dashboard`, `checkout_wizard`, `kanban_board`** to the new API;
   keep their `.txt` specs green unchanged (they are the regression oracle).
8. **Delete legacy:** `Reactive.roc`, `Elem.roc`, `Graph.roc`'s string-keyed paths,
   old `UiRuntime` string tables; drop them from `main.roc` `exposes`/imports.
   Apps then import only `Signal`/`Html`/`Ui`.
9. **Done — docs corrected** (`DESIGN.md`, `GUIDE.md`): key types are
   `TodoId := [Tid(U64)]` (nominal-over-tag-union), `state` is a closure binder,
   and the host owns equality/hash through captured per-type thunks rather than
   string identity.
10. **Extend `identity_stress.txt`** with the mid-list-insert assertion (new row
    gets fresh state while every existing row's count survives unmoved) — only
    once honestly supported. Do not weaken existing assertions.
11. **Validate:** `roc check` on all four apps, `zig build run-test-signals`, and
    `zig build run-signals-bench` (compare against the Phase 1 baseline; row reuse
    should be the biggest win).

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
