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
  `roc_ui_recompute`, `roc_ui_render`, `roc_ui_drop`, and now by the app-boundary
  flip where `roc_ui_init` returns the retained descriptor tree and the host owns
  subsequent recomputation/rendering.
- **Identity is dense integer ids at the boundary.** The host walks the retained
  `Elem` descriptor tree, interns scope-relative identity-bearing sites, and
  routes events/signals by dense ids. Descriptors cross the boundary as explicit
  data, and the host indexes them by id rather than scanning strings at dispatch
  time.
- **Recompute is scoped to dirty signals.** `recompute` evaluates only the
  `dirty_signal_ids` plan the host sends, and `eval_signal` consults host-sent
  caches (`cached_signals`, `cached_states`) instead of re-deriving the world.

This is the hard part of the engine, and it is largely in place. The remaining
work is concentrated in three areas: **confined erasure still uses internal
`NodeValue`**, **structural render patching is not yet at final granularity**,
and **effects/subscriptions are not implemented**. Dirty leaf sinks now patch
directly from the retained descriptor stream; structural `when`/`each` changes
still rebuild the active descriptor stream, but apply it as a DOM patch instead
of resetting/recreating the simulated DOM. The old `Reactive`/`Graph`/
`UiRuntime` Roc surface has been removed from the active platform.

From here, avoid widening the test harness or adding new representative apps
unless the work directly retires one of the remaining design blockers. The
existing four apps, their specs, focused Zig unit coverage, and the benchmark
gate are enough regression surface for the next phase. New infrastructure must
be justified by a specific risk in confined erasure, retained thunk ownership,
or structural no-rebuild patching.

## Gap Analysis (current vs. `DESIGN.md`)

Ordered roughly by how much of the design they block.

1. **Internal erasure still uses `NodeValue`.** Apps no longer import
   `NodeValue` or internal `Node`, define app-local row encode/decode
   boilerplate, or receive `NodeValue`/`Node` from the platform exposes list,
   but the platform still serializes values through `NodeValue` and still has
   crash-on-decode-mismatch wrappers inside `Signal.roc`/`Ui.roc`. The design
   requires confined per-edge erasure so a mismatch is a compile error, not a
   runtime crash.

2. **Minimal patch emission is partial.** For state changes that do not feed an
   active `Ui.when` condition or `Ui.each` item list, dispatch now reuses the
   retained descriptor stream and applies only dirty signal-backed text/value/
   checked/disabled sinks. If the dirty source feeds a structural site, the host
   rebuilds the active descriptor stream, then patches DOM shape from explicit
   host-owned DOM identities: removed branch/row DOM is detached, surviving keyed
   row DOM is moved/reused, and the simulated DOM is not reset. Event bindings are
   compared against the next explicit event descriptor stream, so unchanged
   element/kind/event-id slots are left alone and only added, removed, or shifted
   bindings are patched. `Ui.each` now also retains an item equality thunk and
   row scopes cache their latest item value, so the host has explicit data to
   distinguish reused unchanged rows from reused updated rows. The active
   collector uses that data to copy the previous retained descriptor subtree for
   reused unchanged rows instead of invoking the row thunk, unless a descendant
   `when`/`each` descriptor depends on the current dirty source. Remaining work is
   eliminating the broader descriptor rebuild once retained typed closures replace
   the internal `NodeValue` path.

3. **Metrics are partway through the design migration.** `RuntimeMetrics` now
   carries the design counter names (`nodes_recomputed`,
   `propagation_prunes`, `derived_calls_into_roc`, `patches_emitted`, row/scope
   and closure counters). Scope and keyed-row counters are real for active
   branch/row churn, and closure counters now track descriptor-stream ownership
   of retained event transforms, state equality thunks, each key/item/row thunks,
   and direct map/map2 signal thunks. Leaf sink patch counts now track changed
   fields on non-structural updates, while structural updates count actual creates,
   child moves, changed fields, and event bindings rather than a full
   reset/recreate. Source-level equal-output pruning suppresses downstream work,
   and retained `Map`/`Map2`/`Combine` output equality thunks now prune unchanged
   dirty sink and structural-site outputs in the current `NodeValue` bridge.
   The remaining design work is moving those retained thunks onto opaque typed
   value cells so the internal `NodeValue` encode/decode path disappears.

4. **No effects/subscriptions.** `Signal.from_task`, `Signal.interval`,
   `Ui.on_change`, `Ui.on_cleanup` are unimplemented. (Lowest priority; the
   design treats effects as sources reusing the same path, so they slot in after
   the core is right.)

5. **Optimized benchmark validation is blocked by a compiler bug.** `roc check`,
   dev builds, and `zig build run-test-signals` are the current regression gates.
   `zig build run-signals-bench` currently hits
   [roc-lang/roc#9717](https://github.com/roc-lang/roc/issues/9717) while
   compiling the optimized ops-dashboard benchmark. Do not skip that case or
   silently downgrade it to dev mode; rerun the full benchmark when the compiler
   bug is fixed.

## Priority Reset (2026-06-20)

The next work should be risk-first, not infrastructure-first. The active test
host is already broad enough to expose regressions in the maintained scenarios.
The highest value work is now changing the platform boundary so the
implementation actually matches `DESIGN.md`.

1. **Confined erasure is the current top priority.** Replace internal
   `NodeValue` traffic with boxed opaque values plus per-edge `is_eq` thunks
   captured by static dispatch. The host must store values without choosing a
   decoder, and crash-on-decode-mismatch wrappers must disappear because the
   mismatch is no longer representable.
2. **Retained thunk ownership is the next gate.** Move from "dirty ids sent back
   to Roc for a render/eval walk" toward host-owned node records that retain
   transform/reducer/equality thunks exactly once and invoke only the dirty
   thunks, batched if that remains the right FFI shape.
3. **Structural no-rebuild follows retained typed thunks.** Once `when`/`each`
   sites carry the explicit retained data needed to rebuild only affected
   branch/row scopes, remove the active descriptor-stream rebuild from
   structural updates. Do not spend more effort polishing the current rebuild
   path beyond correctness fixes.
4. **Effects/subscriptions remain deferred.** They reuse the same source update
   path once the typed core is right; implementing them before confined erasure
   would expand surface area around the wrong value protocol.

Do not prioritize more simulated DOM commands, broader fixture catalogs,
additional metric counters, or new app specs unless they are the smallest way to
prove one of the risks above. The next green slice should retire a design
blocker, not merely measure the current compromise more precisely.

## De-Risk First (gating experiments)

`DESIGN.md` names three experiments that gate the architecture. G2 and G3 are
now answered well enough by the active host and app suite to stop expanding test
coverage around them. G1's retained-callable ownership path is proven through the
current bridge; the unresolved architecture gate is replacing that bridge with
retained typed value cells owned by the host.

- **G1 — Retained closures across FFI.** The design's end state stores boxed Roc
  transform/reducer/equality closures in the host node table and re-invokes only
  the changed ones. The active host now invokes retained reducer, row, key
  equality, item equality, `Map`/`Map2` transform, and `Map`/`Map2`/`Combine`
  output equality thunks through the generated ABI. This answers the FFI
  ownership/call-shape question for the current bridge, but the host still
  evaluates retained `SignalExpr` trees over internal `NodeValue` and still
  rebuilds descriptor streams for structural changes. Next action: replace
  those `NodeValue` payloads with opaque typed value cells owned by explicit
  retained edge thunks. If
  per-call FFI becomes too costly at that point, keep a **batched** recompute
  shape as the permanent protocol rather than one-call-per-node.
- **G2 — Construction-site identity under dynamic shape.** This is the riskiest
  solved item: the active host now mints scope-relative ids without strings, and
  `identity_stress` proves row-local state survives reorder, filtering/removal,
  re-add, and enclosing branch disposal. Keep it green, but do not add more
  identity fixture surface unless a scoped-id bug appears.
- **G3 — `is_eq`-pruned glitch-free propagation.** The rank scheduler exists and
  the Zig host has the diamond test (`a->b`, `a->c`, `(b,c)->d`) asserting the
  join appears once in the dirty plan. The ops dashboard now includes a
  high-fan-out no-op source whose unchanged output prunes all downstream labels
  (`nodes_recomputed +1`, `propagation_prunes +1`, `patches_emitted +0` for the
  scripted click). Dirty sink and structural-site caches now also use retained
  signal output equality thunks to suppress unchanged leaf patches and unchanged
  structural rebuilds. Do not add more pruning harnesses before confined erasure
  replaces the temporary `NodeValue` bridge with typed edge value cells.

Also confirm the two maturity unknowns: that static dispatch on
`is_eq`/`encode`/`decode` methods resolves and monomorphizes across the platform
boundary (there is no ability auto-derivation, so each value type must define
those methods), and monomorphization/compile-time behavior of deeply nested
generic `Signal`/`Elem` combinators.

### Confirmed maturity findings (2026-06-19 probe)

A throwaway compile probe (since removed) pinned down three facts on the current
compiler that reshape the new-API surface and the host equality mechanism:

1. **`where [k.to_hash : k, Hasher -> Hasher, k.is_eq : k, k -> Bool]` static-dispatch
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

### Opaque value carrier probe (2026-06-20)

A temporary compile probe confirmed that `Box(a)` cannot be hidden inside a
non-generic descriptor payload such as `Opaque := [Opaque(Box({}))]`. Roc keeps
the payload typed as `Box(a)` and rejects the coercion to `Box({})`. The generic
form `Opaque(a) := [Opaque(Box(a))]` compiles, which means the type parameter
remains part of the descriptor shape and cannot directly inhabit the current
heterogeneous `Elem` tree.

Consequence: confined erasure cannot be implemented as a mechanical
`NodeValue -> Box({})` field replacement. The next real boundary change needs an
explicit descriptor-shape change that creates an erasure boundary at each
identity/sink edge, or a host-owned value-cell protocol generated at the typed
call site. Do not add a fake `Box({})` carrier or any host-side cast that asks
the host to guess the hidden type.

## Implementation Status (2026-06-19)

### Done — new Roc app-facing API (compiles, additive, legacy untouched)

The string-free, closure-binder API exists and typechecks end to end. New
platform modules:

- `platform/Node.roc` — pure immutable descriptor tree the host walks. `SignalExpr`
  (`Ref(BinderRef)`/`ConstValue`/`Map`/`Map2`/`Combine`), `Attr`, and the
  identity-bearing `Elem` scope binders `State`/`When`/`Each`. `State` and `Each`
  carry boxed `is_eq` / key-`is_eq` thunks (confined erasure). `Msg` is a type
  alias `{ binder : BinderRef, payload_kind : U64, transform : Box(...) }`.
  Binder refs now carry boxed binder tokens minted by `Ui.state`; the host maps
  those tokens to active state node ids during the descriptor walk. This replaced
  the earlier de-Bruijn prototype, which could not represent captured outer
  state refs inside nested state bodies.
- `platform/Signal.roc` — opaque `Signal(a)`: `const`, `map`, `map2`,
  `combine`. `Signal` now boxes/clones retained
  `SignalExpr` descriptors so multiple sinks can share a source without handing
  the host dangling descriptor pointers. `Map`/`Map2` use positional tag payloads
  because the record payload containing an erased callable and boxed recursive
  field generated a host/Roc field-order mismatch.
- `platform/Ui.roc` — `state(init, |State(a)| body)` closure binder with
  `signal`/`on_unit`/`on_value`; `when(cond, true_body, false_body)` (branch
  scopes); `each(items, key_fn, |key, row_signal| ...)` with
  `where [k.to_hash, k.is_eq]` and boxed key equality. Key types are
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

### Done — app boundary flip into the host (end-to-end green)

The platform boundary has been flipped for the signals test host:

- `roc_ui_init` returns a retained `Elem` descriptor tree. The host stores it,
  walks it, evaluates `SignalExpr`, applies DOM patches, and dispatches events
  directly through retained reducer callables.
- The host owns active scope/key/state lifetimes for root, `when` branches, and
  keyed `each` rows. Keyed rows use the boxed key-equality thunk; removed scopes
  are disposed instead of being silently reused later.
- Host calls into erased Roc callables now retain `NodeValue` arguments before
  invocation. This fixed use-after-free when Roc consumed borrowed list/state/
  scope values.
- Runtime metrics are host-owned for host-owned work while preserving cumulative
  Roc counters, so per-event deltas no longer go negative after the boundary flip.

Verified on this slice:

- `roc check` is green for `checkout_wizard`, `identity_stress`, `kanban_board`,
  and `ops_dashboard`.
- `zig test test/signals/platform/host.zig` is green.
- After merging `origin/main` at `ea1b0c0001`, `checkout_wizard` no longer hits
  the postcheck invariant. The merged checker exposed a regular missing-method
  error from an unpinned numeric state; pinning that row quantity to `I64` fixed
  it.
- `zig build run-test-signals` is green for all four app specs against the
  flipped host.

### Remaining work (after the app-boundary flip)

In dependency order. Each sub-step ends green per `minici` discipline.

1. **Host scope forest + identity walk.** Pre-order walk of the descriptor tree
   that assigns construction-order ordinals to identity-bearing nodes only (state
   binders, `when` sites, `each` sites); ordinary markup does not advance the
   ordinal. Scope path step = `When(site_ordinal)/Branch(tag)` and
   `Each(site_ordinal)/Keyed(key)`. Intern `(scope_path, ordinal)` to dense node
   ids. No strings; no `Dict(Str,_)`.

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
   state refs. The host now keeps DOM identity separate from state/scope
   identity, so structural patches can detach removed DOM and move/reuse
   surviving keyed row DOM. The remaining work is adding broader assertions
   around branch/filter churn and removing the descriptor-stream rebuild.
2. **Host-invoked `is_eq` thunks.** Keep the erased-callable call convention +
   per-type marshaling as the only way the host compares typed keys/values.
   Duplicate keys within one keyed scope = hard host error (not a silent alias).

   Progress: `platform/host.zig` now has typed helper calls for
   `NodeValue -> NodeValue`, `(NodeValue, NodeValue) -> NodeValue`, and
   `(NodeValue, NodeValue) -> Bool`, with Zig unit coverage that allocates boxed
   erased callables through the real ABI payload/capture path. `Signal.map`,
   `Signal.map2`, and `Signal.combine` descriptors now carry retained output
   equality thunks produced by static dispatch at the call site. The host also
   has the `(NodeValue, NodeValue) -> NodeElem` helper needed to invoke `Ui.each`
   row thunks and explicitly release the returned descriptor tree. These helpers
   are now wired into keyed-row diffing, reducer dispatch, signal transforms, and
   dirty output pruning. Signal-backed text/value/checked/disabled sinks and
   active `When`/`Each` structural sites cache their last output value and
   compare dirty evaluations with the retained output equality thunk before
   emitting patches or rebuilding structure.
3. **Keyed-row diff + disposal.** Diff new typed key-set against old via the key
   `is_eq` thunk; reuse surviving row scopes (and their local state) → `rows_reused`;
   mint new keys → `rows_created`; dispose removed keys → drop one refcount per
   retained closure (`decrefErasedCallable`), detach DOM subtree, `rows_removed`.
   `Ui.when` flip disposes the losing branch scope. `keep_alive` only via explicit
   flag.

   Progress: `platform/host.zig` now has recursive scope disposal that retires
   subtree node identities, drops keyed-row keys exactly once, and prevents a
   later matching key from reusing disposed local state. It is wired into the app
   init/render lifecycle and structural DOM patching. The host also has a typed-key
   row diff helper that reuses, creates, and disposes row scopes by the boxed key
   equality thunk and records `rows_reused`/`rows_created`/`rows_removed`. The
   `NodeElem` descriptor collector can now consume explicit
   evaluated item values for an `Each` site, invoke `key_of`, run the typed row
   diff, call the row thunk, and walk each returned row body in its keyed row
   scope so row-local state ids survive reorder and removed rows are disposed.
   It can also collect the active `When` branch while disposing the inactive
   branch scope, so branch-local state is retired on flips and never silently
   reused. The app init/render lifecycle uses this active collector for
   structural updates, and structural DOM application now detaches removed
   branch/row DOM while moving/reusing surviving keyed row DOM through explicit
   host-owned DOM identities. Structural event application now compares the
   current DOM event slots with the next explicit event stream and patches only
   added, removed, or shifted event-id bindings. For reused keyed rows whose
   cached item value is unchanged, active collection now copies the previous
   retained row descriptor subtree instead of invoking the row thunk; rows with a
   descendant structural descriptor fed by the dirty source still rebuild from
   explicit dependency data.
4. **Done — delete legacy surface.** `Reactive.roc`, the old string-keyed
   `Graph.roc`, and the old `UiRuntime.roc` evaluator have been removed. The
   active platform surface is the retained descriptor API in `Elem`/`Node` plus
   `Signal`/`Html`/`Ui`, and apps import only `Elem`/`Signal`/`Html`/`Ui`.
5. **Partial — incremental render patches.** Dispatch now classifies changed
   source nodes against the active descriptor stream. Non-structural changes
   apply only the matching signal text/value/checked/disabled sinks and leave the
   descriptor stream and DOM shape intact. Dirty sources that feed active
   `When`/`Each` descriptors rebuild the active descriptor stream, then apply a
   structural DOM patch rather than resetting the DOM.
6. **Done — docs corrected** (`DESIGN.md`, `GUIDE.md`): key types are
   `TodoId := [Tid(U64)]` (nominal-over-tag-union), `state` is a closure binder,
   and the host owns equality/hash through captured per-type thunks rather than
   string identity.
7. **Done — extend `identity_stress.txt`.** The G2 fixture now covers reorder,
   mid-list insert, filter/remove/re-add, enclosing branch disposal, strict
   row reuse/create/remove metric deltas, absence assertions for removed DOM,
   and reset of row-local state after branch disposal.
8. **Validate:** `roc check` on all four apps and `zig build run-test-signals`.
   `zig build run-signals-bench` remains the optimized benchmark gate, but it is
   currently blocked by roc-lang/roc#9717 in the LLVM optimized ops-dashboard
   build; keep the failure visible rather than skipping or downgrading the case.

## Recommended Sequence (Risk-First Reset)

Each phase ends green: `roc check` on the apps plus `zig build run-test-signals`
passing. Do not advance until the current phase's section passes (per the
`minici` discipline in `AGENTS.md`). Keep the three representative apps and their
`.txt` specs as the regression oracle throughout.

As of 2026-06-20, Phases 1, 2, 3, 5, and most of 6 are no longer where the main
risk lives. Treat them as invariants to preserve, not as places to keep adding
coverage. The execution order from here is:

1. Finish Phase 4's confined-erasure boundary.
2. Use the typed retained thunks from Phase 4 to complete the final Phase 6
   no-descriptor-rebuild structural path.
3. Add Phase 7 effects/subscriptions after the typed source/update path exists.
4. Do Phase 8 docs/cleanup continuously, but only to keep landed behavior
   accurate.

### Phase 1 — Lock in the host engine and metrics (low risk, high leverage)

Goal: make the scaling claim measurable before changing semantics.

Status as of 2026-06-20: the metrics surface has been replaced and
`zig build run-signals-bench` records baseline rows with the new counter names.
The host has a Zig diamond dirty-plan test wired into `run-test-zig`, and the
ops dashboard script has a high-fan-out no-op source A/B that proves equal
source output prunes downstream work and patches. Descriptor-stream closure
retain/release counters are live for the host-retained thunk fields. Retained
signal output equality thunks now prune dirty leaf sinks and unchanged
structural-site outputs in the active `NodeValue` bridge. The remaining pruning
work is the fuller design target: opaque typed value cells once confined erasure
removes `NodeValue`.

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

Status as of 2026-06-20: complete for the active platform surface. The host
walks the pure descriptor tree, advances ordinals only for identity-bearing
sites, and interns dense ids inside explicit root/branch/row scopes. The old
string-keyed state/signal/event registration tables lived in the deleted
`UiRuntime.roc`/`Graph.roc` path.

- Keep G2's `identity_stress` harness green as the lifecycle model changes. It
  now covers row-local state through `when -> each -> when`, reorder, mid-list
  insert, filtering/removal, and enclosing branch disposal.
- Preserve the host-owned identity walk: Roc never threads a mutable id counter
  and ordinary markup never consumes identity.
- Keep explicit **scope** boundaries in the descriptor stream (`root`,
  conditional branch, list row) so identity is construction-site *within a
  scope* and sibling scopes are insulated from positional shifts.

Exit: no `Str` key participates in identity; apps still pass (they may still
build keys at the call site — those become inert and are removed in Phase 3).

### Phase 3 — New app-facing API: `Signal`, `Html`, `Ui`

Build the target surface and port the apps.

Status as of 2026-06-20: complete for the maintained app suite. The
representative apps and identity stress app import `Elem`/`Signal`/`Html`/`Ui`,
and the old `Reactive` module has been deleted.

- Create `Signal.roc` (opaque `Signal(a)` over a node id; `state`/`send`, `const`,
  `map`, `map2`, `combine`), `Html.roc` (`div`/`button`/`input`/`text`/`text_s`,
  signal-backed attrs, `on_click`/`on_input`/`on_check` taking typed messages),
  and `Ui.roc` (`when`, `each(items, key_fn, |key, row| ...)`, `component`).
- `Ui.each` carries static-dispatch constraints for `item.is_eq`,
  `key.to_hash`, and `key.is_eq`; no string keys. The key type provides `hash`
  and `is_eq` through its `.{ }` method block, and the item type provides
  `is_eq` so row reuse and row value changes are distinct host facts.
- Port `ops_dashboard`, `checkout_wizard`, `kanban_board` to the new API,
  deleting every `Str.concat("...:", id)` key and every `unit_channel`/`fold_i64`
  call. Re-run their `.txt` specs unchanged (the specs assert user-facing
  behavior, so they are the migration oracle).

Exit: apps import only `Elem`/`Signal`/`Html`/`Ui`; specs pass; `Reactive` and
`NodeValue` are no longer imported by apps.

### Phase 4 — Confined erasure; delete `NodeValue` from app code

Status as of 2026-06-20: the representative apps and `identity_stress` no
longer import `NodeValue` or define app-local encode/decode boilerplate for row
fixtures. They use `List(Str)` row labels until the platform API removes
`NodeValue` internally and captures typed per-edge thunks directly. The old
typed-specialized `Signal.const_i64`/`const_str`/`const_bool` helpers have been
removed; maintained apps use the generic `Signal.const` constructor so constants
also go through the static-dispatch value path. Unused `NodeValue` helper
constructors and crash extractors have been removed; `NodeValue` remains only as
the internal encode/decode bridge used by the current retained thunk wrappers,
with its tag set narrowed to the value forms those wrappers can actually encode
and decode.

Current priority: do this next. This is the largest remaining correctness gap
against `DESIGN.md`, and it unlocks the retained-thunk node table, true
per-edge equality pruning, and the final structural no-rebuild path. Avoid
parallel work that only improves the current `NodeValue` compromise.

Progress: `Ui.state` no longer stores its initial value as a direct
heterogeneous `NodeValue` field in `Elem.State`. The descriptor now carries a
retained initializer thunk, and the host calls that thunk only when a new state
node is created. This matches the target `ValueInitThunk` ownership shape while
the thunk body still returns the current internal `NodeValue` bridge value.
`Signal.const` and row item signals now use the same retained initializer-thunk
shape through `SignalExpr.ConstValue`, and `ConstValue` now carries its own
retained output equality thunk, so constants no longer embed direct `NodeValue`
payloads in signal descriptors and signal caches no longer need a nullable
equality edge.
`Signal.combine` now also carries a retained output thunk; the host still uses
the temporary `NodeValue` list as the current bridge input, but the output edge
is owned by the call-site thunk rather than by host-side output construction.
Live host state records now retain their equality thunk directly, so event
updates prune through state-owned data instead of recovering equality from the
active descriptor stream. Dirty signal sink and structural-site caches now also
retain the equality thunk beside the cached bridge value, so dirty pruning uses
cache-owned edge data rather than looking equality back up through the retained
`SignalExpr`. Active source state now stores its current bridge value and
retained equality thunk in a single host value-cell record, matching the shape
needed for the eventual opaque typed value carrier; signal caches use that same
value-cell record now that every signal output edge has equality. Active event
records now own reducer transforms after stream activation, so dispatch no
longer reads reducer callables from `active_stream.events`.

- Resolve per-edge `is_eq` (and, where a value must serialize, `encode`/`decode`)
  thunks by static dispatch on the surrounding `Signal(a)`'s value type, pinned
  at the call site and specialized by monomorphization at the platform-glue
  layer. The host stores boxed opaque values and calls only that edge's thunk; it
  never selects a decoder.
- Introduce the host value-cell representation needed for boxed opaque values,
  including explicit retain/release ownership and a single associated equality
  thunk per edge. This replaces ad hoc decode/re-encode paths rather than
  wrapping them.
- Convert one vertical path first: `Ui.state` source value, a `Signal.map` or
  `map2`, one signal-backed sink, and one `Ui.each` keyed diff using typed key
  equality. That slice should run through the real app boundary, not a detached
  catalog fixture.
- Remove the `NodeValue` `encode`/`decode` boilerplate from app types; instead
  the value type defines `is_eq` (and a key type also defines `hash`) in its
  method block.
- Remove the `crash`-on-decode-mismatch wrappers; a mismatch is now a type error.

Exit: no app defines `NodeValue` conversions; no `crash` on value type mismatch;
static dispatch on the `is_eq`/`encode`/`decode` methods confirmed resolving and
monomorphizing across the boundary.

### Phase 5 — Collapse the combinator zoo

Status as of 2026-06-20: complete for the active platform surface. The old
`Graph.roc` combinator family and `Reactive` wrappers have been deleted; the
active descriptor model uses `Node.SignalExpr.Ref`, `ConstValue`, `Map`, `Map2`,
and `Combine`.

- Keep the active generic descriptor family small as confined erasure replaces
  internal `NodeValue`; do not reintroduce type-specific map/fold constructors.
- Regenerate `roc_platform_abi.zig` glue after exposed platform type changes and
  re-fix any ABI struct-size asserts.

Exit: one polymorphic family in the API and a small generic node set in `Node`;
specs pass; compile-time monomorphization cost confirmed acceptable.

### Phase 6 — Scopes, dynamic lists, and minimal patches

This delivers the headline feature (real keyed reuse) and true incremental render.

Status as of 2026-06-20: the host scope forest, keyed-row diff/reuse/removal,
and active `Ui.when` branch disposal are wired into the active app lifecycle.
Dirty non-structural source changes now patch only matching leaf sinks from the
retained descriptor stream, and unchanged dirty outputs are pruned with the
retained signal equality thunk. Structural source changes first compare the
active `When`/`Each` output cache with the retained equality thunk; changed
structural outputs still rebuild the active descriptor stream, but the host now
applies the result as a structural DOM patch using explicit DOM identities scoped
by branch/row and compares event binding slots rather than rebinding every active
event. Reused keyed rows with unchanged item values copy their previous retained
descriptor subtrees during that rebuild, unless a descendant structural site has
an explicit dependency on the dirty source that triggered the rebuild.

Current priority: pause feature work here until Phase 4 gives structural sites
retained typed data. Correctness fixes are still in scope, but the remaining
descriptor-stream rebuild cannot be removed cleanly while the host still depends
on the internal `NodeValue` path and Roc-side active descriptor evaluation.

- Keep the host scope forest explicit: each conditional branch and list row is a
  scope owning its minted ids and retained closures.
- Keep `Ui.each` keyed-row reuse/removal real: surviving row scopes retain local
  state, new keys mint scopes, and removed keys dispose scopes and count
  `rows_removed`.
- Keep `Ui.when` branch disposal real: the losing branch scope is disposed on
  flip; keep-alive only via an explicit flag.
- Done for leaf sinks: non-structural dirty source nodes emit only matching
  `SetText`/`SetValue`/`SetChecked`/`SetDisabled` patches, with no descriptor
  rebuild and no DOM reset; unchanged outputs are pruned through the retained
  signal equality thunk before any patch is emitted.
- Done for structural DOM shape: dirty structural source nodes detach removed
  branch/row DOM, move/reuse surviving keyed row DOM, create only new DOM nodes,
  and avoid the full DOM reset when a dirty source feeds `when`/`each`. Unchanged
  `when` condition or `each` item-list outputs are pruned before the structural
  descriptor stream is rebuilt.
- Done for structural event bindings: unchanged event-id slots stay bound; only
  added, removed, or shifted event-id bindings are patched.
- Done for unchanged keyed-row bodies: when a reused row's retained item equality
  thunk says the item is unchanged, the active collector copies the previous
  retained descriptor subtree instead of invoking the row thunk. This shortcut is
  disabled for row subtrees whose descendant `when`/`each` descriptor explicitly
  depends on the dirty source being rendered.
- Remaining structural patch work: remove the active descriptor-stream rebuild
  when retained typed closures make per-node structural plans explicit.

Exit: `rows_reused`/`rows_removed` reflect actual reuse; reorder/filter
preserves per-row state; non-structural `patches_emitted` tracks changed sinks,
and structural updates no longer reset/recreate the whole DOM.

### Phase 7 — Effects and subscriptions

Current priority: explicitly deferred until confined erasure and the typed
source/update path are in place.

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

- Delete `NodeValue.roc` once confined erasure removes the internal platform
  representation.
- Keep `README.md`, `DESIGN.md`, `GUIDE.md`, and this file aligned as each
  platform boundary changes.
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

- **Phase 4 is now the highest-risk step.** Confined erasure changes the real
  host boundary and must make type-mismatch crashes structurally impossible.
  Any proposed work that leaves `NodeValue` on the hot path should be treated as
  transitional and should not grow new surface area.
- **Retained thunk ownership is the key technical risk inside Phase 4.** The
  host must retain exactly one refcount per live closure/value edge, release it
  on scope disposal, and avoid both double-retains and borrowed-value use after
  free. Add only the smallest focused coverage needed to prove that ownership.
- **Phase 2 identity remains a keep-green invariant, not the active frontier.**
  If a scoped-id regression appears, fix it immediately, but do not keep adding
  identity stress cases while confined erasure is still unresolved.
- **Static dispatch and monomorphization maturity remain watch-items.** If
  method resolution/monomorphization for per-edge `is_eq`/`encode`/`decode`
  thunks is not ready, keep the batched opaque-value protocol as the explicit
  design option and document the blocker. Do not paper over the gap with
  decode-choice logic in the host.
- **Keep apps + specs green every phase.** They are the only end-to-end oracle;
  a phase that breaks a spec is not done.
- **Watch `roc_platform_abi.zig` struct-size asserts** after any boundary type
  change (Phases 2, 4, 5, 7) — regenerate glue and re-fix the `comptime` size
  checks rather than hand-editing.
