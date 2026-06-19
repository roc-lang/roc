# Signals UI Platform — Target Design

This document is the authoritative design for the Signals UI platform. It
describes the architecture we are building toward.

The host for this design is a **test-runner host** that simulates a real DOM and
applies render patches against an in-memory element tree. The simulated host
lets us benchmark and characterize the framework before a browser
implementation exists. The Roc-facing API and the host boundary are designed so
that the test-runner host and a future browser host implement the *same*
contract.

## Non-Negotiable Constraints

These constraints come from `AGENTS.md`, `design.md`, and the platform's role.
Every part of this design must respect them.

1. **No compiler changes.** This is a platform. We may not add dataflow
   analysis passes, dependency-graph extraction, or any new compiler behavior.
   Everything is ordinary Roc plus a Zig host.
2. **No workarounds, fallbacks, heuristics, or best-effort recovery** outside of
   parsing and error reporting. The host never *guesses* what changed, never
   scans to rediscover identity, and never reconstructs missing information. It
   consumes explicit data produced by the Roc graph description.
3. **Work scales with the number of changed nodes, not with tree size.** This is
   the entire point of signals. Per event, the host re-invokes only the Roc
   transform closures whose inputs actually changed, in dependency order. There
   is no full-tree re-walk and no full re-render per event.
4. **Mutation lives only in the host.** Roc is pure and value-oriented. The
   reactive runtime — the dirty set, the scheduler, the in-place node table — is
   intrinsically mutable, so it lives in the Zig host, which is the one place
   mutation is legal.
5. **Type-mismatch crashes are structurally impossible.** A typed `Signal a`
   stays typed end to end. Erasure is confined to one generated thunk per edge,
   pinned to that edge's monomorphized types. There is no second, independently
   typed read site that can disagree with the writer.

## Why Not Elm, Why Not Solid

**Elm** rebuilds the view and diffs per event. That is O(view size) per event
and is the opposite of signals. Rejected.

**Solid** discovers dependencies by running effects and watching reads through a
mutable global "current observer." Roc cannot observe its own reads and has no
mutable globals, and we cannot add compiler support to fake it. Rejected as a
*mechanism*.

**The reframing that makes signals work in pure Roc:** in Solid, dependencies
are *discovered* at runtime. In a pure language, dependencies are *declared* —
they are already present in the structure of each `map`/`map2`/`combine` call
before anything runs. When the app writes `Signal.map2(price, qty, f)`, the
edges `price -> result` and `qty -> result` are *data*. So the host never needs
a current-observer stack and never runs a closure to find out what it reads. The
**dependency graph is handed to the host as a value, once.** The host then owns
a mutable node table and runs push-based incremental propagation over it.

This is "the graph is Roc data, the runtime engine is the host." It keeps
signals' linear-with-events scaling, fits Roc's purity, and needs zero compiler
changes.

## Architecture Overview

```text
   App (Roc)                Platform (Roc)              Host (Zig)
   ---------                --------------              ----------
   build : Signals -> Ui    Ui.* / Signal.* / Html.*    node table (mutable)
   pure description    ───▶  encode graph as a value ──▶ mint ids, adjacency,
                                                          dirty set, scheduler
                                                          simulated DOM

   user event           ◀── route event id ──────────── DOM listener fires
   reducer closure (Roc) ─▶ host calls it ─────────────▶ mark source dirty,
                                                          propagate in dep order,
                                                          call only changed
                                                          derived closures,
                                                          emit minimal patches
```

The Roc side is built **once** per app lifetime into a graph description that
the host ingests. After that, the only things crossing the boundary per event
are: one event-id-to-reducer dispatch, and N derived-closure invocations where N
is the number of nodes whose value actually changed.

## Core Concepts

- **Signal a** — a continuous, always-present value of type `a`. Opaque type
  wrapping a host node id (`U64`), phantom-typed in `a`. The `a` exists only in
  Roc's type system; the wire payload is the id.
- **Source** — a node whose value is set by host input (a DOM event, a timer, an
  effect result). Created by `Signal.state` / `Signal.input` and by effect
  combinators.
- **Derived node** — `map`, `map2`, `combine`. Holds a retained Roc transform
  closure plus its input node ids. The host recomputes it when an input changes.
- **Reducer** — a pure `a -> a` (or `a -> (a, Cmd)`) closure attached to a source
  via `send`. The host retains it and calls it when the bound event fires.
- **Scope** — a host-owned region that owns minted node ids and retained
  closures: the root, each conditional branch, and each list row are scopes.
  Disposing a scope drops its refcounts and detaches its DOM.
- **Elem** — a pure description of UI structure that references signals for
  dynamic text/attrs and references reducers for event handlers.
- **Cmd / Sub** — typed effect requests and subscriptions; results re-enter the
  graph as source updates, using the same propagation path as a click.

## Identity: Construction-Site Within Explicit Scopes

There are no author-written string keys anywhere. Identity is assigned by the
host during graph ingestion.

- Within a scope, node identity is **construction order** (the order the app
  built the nodes). The app build is pure and deterministic, so this order is
  stable across rebuilds of the same scope.
- **Scopes contain positional shifting.** Because conditional branches and list
  rows are first-class scopes, adding or removing UI inside one scope does not
  shift identities in sibling scopes. This is the new failure mode we design
  around: "where you built it is your identity," so the seams that can shift
  (branches, lists) are explicit scope boundaries.
- **Dynamic lists use typed keys, not position.** `Signal.each` takes a typed
  key function `item -> key where key implements Hash & Eq`. A row's identity is
  its key, so per-row local state survives reorder/insert/delete. Duplicate keys
  are an `Eq` question the host answers explicitly (reported as a host error),
  never a silent alias.

This replaces string-collision/rename hazards with explicit, typed structure.

## Confined Erasure: No NodeValue, No Decode Crash

The host's node table is heterogeneous, so values cross the boundary as opaque
payloads. But erasure is confined to **one generated set of thunks per edge**,
pinned to that edge's monomorphized types:

- For each `map`/`map2`/`combine`/`state`/sink, the platform-glue layer emits,
  via Roc abilities, the concrete `Eq` (for change pruning) and — only where a
  value must be stored or moved as bytes — `Encode`/`Decode` thunks. These are
  generated from the same call site whose argument types are tied to the source
  `Signal a`'s `a`.
- The host **never chooses** a decoder. It stores a boxed, opaque Roc value and
  hands it back to the one thunk that owns that edge. There is no second,
  independently typed read site, so a mismatch is a compile-time type error, not
  a runtime decode crash.

Hot-path values are stored as **boxed typed Roc values the host never
inspects**, with an `Eq` thunk used for change pruning. Byte serialization is
reserved for persistence and the wire, never forced on every event. We do not
`memcmp` encoded bytes for equality (fragile for floats/maps); we call the `Eq`
ability thunk.

There is no untyped value representation crossing the boundary. The public API
is a few polymorphic functions (below); monomorphization generates concrete code
for each instantiation, so there is no hand-written family of type-specialized
combinators.

## App-Facing API

The app sees `Signal a`, typed keys, `Elem`, `Cmd`, `Sub`, and a small set of
polymorphic functions. It never sees ids, string keys, `NodeValue`, lifecycle
tokens, or subscriptions internals.

### Module surface

```roc
# Opaque to the app:
Signal a
Elem
Cmd a
Sub a
Scope            # only appears in `Ui.component` / cleanup signatures

# Signal construction and combination
Signal.state  : a -> { signal : Signal a, send : (a -> a) -> Msg } where a implements Eq
Signal.const  : a -> Signal a where a implements Eq
Signal.map    : Signal a, (a -> b) -> Signal b where b implements Eq
Signal.map2   : Signal a, Signal b, (a, b -> c) -> Signal c where c implements Eq
Signal.combine : List (Signal a) -> Signal (List a) where a implements Eq

# Async / effects as sources (same propagation path as user events)
Signal.from_task : Task a err -> Signal [Loading, Done a, Failed err]
Signal.interval  : Duration -> Signal Instant
Ui.on_change     : Signal a, (a -> Cmd msg) -> Elem   # sink: fires a Cmd when value changes
Ui.on_cleanup    : Task {} {} -> Elem                 # runs at scope disposal

# Structure
Html.div     : List Attr, List Elem -> Elem
Html.button  : List Attr, List Elem -> Elem
Html.input   : List Attr, List Elem -> Elem
Html.text    : Str -> Elem            # static text
Html.text_s  : Signal Str -> Elem     # signal-backed text (a sink)

# Attributes (signal-backed where dynamic)
Html.value     : Signal Str -> Attr
Html.checked   : Signal Bool -> Attr
Html.disabled  : Signal Bool -> Attr
Html.on_click  : Msg -> Attr
Html.on_input  : (Str -> Msg) -> Attr
Html.on_check  : (Bool -> Msg) -> Attr

# Dynamic structure (explicit scopes)
Ui.when  : Signal Bool, ({} -> Elem), ({} -> Elem) -> Elem
Ui.each  : Signal (List item), (item -> key), (key, Signal item -> Elem) -> Elem
           where key implements Hash & Eq

# Components (named scopes for local state)
Ui.component : ({} -> Elem) -> Elem
```

`Msg` here is the unit of host-to-Roc dispatch: a `send`-bound reducer plus an
optional payload. `Html.on_input(NameChanged)` means "when this input fires,
route the typed payload through `NameChanged` and apply the bound reducer." The
app never names an event id; the host mints and routes them.

### Example: counter

```roc
counter : Elem
counter =
    { signal: count, send } = Signal.state(0i64)
    Html.div([], [
        Html.button([Html.on_click(send(|n| n - 1))], [Html.text("-")]),
        Html.text_s(Signal.map(count, Num.to_str)),
        Html.button([Html.on_click(send(|n| n + 1))], [Html.text("+")]),
    ])
```

### Example: derived value

```roc
full_name : Signal Str, Signal Str -> Signal Str
full_name = |first, last| Signal.map2(first, last, |f, l| "${f} ${l}")
```

### Example: text input with retained state

```roc
name_field : Elem
name_field =
    { signal: text, send } = Signal.state("")
    Html.input(
        [Html.value(text), Html.on_input(|new| send(|_| new))],
        [],
    )
```

`text` survives across events because the host holds this node by its minted id,
not by a string and not by re-derived tree position.

### Example: keyed list with per-row local state

```roc
TodoId := U64 implements [Hash, Eq]

todo_list : Signal (List Todo) -> Elem
todo_list = |todos|
    Html.ul([], [
        Ui.each(todos, .id, |_key, row|
            { signal: editing, send: set_editing } = Signal.state(Bool.false)
            Html.li([], [
                Html.text_s(Signal.map(row, .title)),
                Html.button(
                    [Html.on_click(set_editing(|e| !e))],
                    [Html.text_s(Signal.map(editing, |e| if e "done" else "edit"))],
                ),
            ]),
        ),
    ])
```

`editing` is a per-row source keyed by the row's typed `key`. It survives
reorder/filter because identity is the key, not the index.

## The Roc Platform Layer

The platform turns the app's pure description into a graph value, hands it to the
host, and provides the retained closures the host calls back into. The platform
has three responsibilities and no reactive runtime of its own.

### 1. Graph description as an explicit value

`Signal a` is an opaque wrapper over a `U64` node id. The platform builds a graph
description as it threads a node-id counter through pure construction. Each node
records its kind and input ids:

```roc
NodeDesc := [
    Source({ initial : Box(OpaqueValue), eq : EqThunk }),
    Map({ input : U64, transform : MapThunk, eq : EqThunk }),
    Map2({ left : U64, right : U64, transform : Map2Thunk, eq : EqThunk }),
    Combine({ inputs : List(U64), eq : EqThunk }),
]

GraphDesc := {
    nodes : List(NodeDesc),       # index is the node id, dense, construction order
    sinks : List(SinkDesc),       # text/attr bindings: which node feeds which DOM slot
    elems : ElemTree,             # static structure with holes referencing node ids
    scopes : List(ScopeDesc),     # branch/list/component scope boundaries
}
```

`MapThunk`/`Map2Thunk`/`EqThunk` are boxed monomorphized closures (the confined
erasure). They are produced from `Signal.map`/`map2`/`state` at the call site, so
their input and output types are pinned to the surrounding `Signal a`.

The platform does **not** evaluate the graph. It only describes it. There is no
`eval_signal`, no dirty propagation, no cache in Roc.

### 2. Entry points

The platform keeps the existing three-entrypoint shape so the host lifecycle is
unchanged in spirit, but the payloads change:

```roc
# platform/main.roc (target shape)
ui_init     : {} -> Box(InitResult)
ui_event    : Box(HostState), U64, Box(OpaqueValue) -> Box(EventResult)
ui_recompute : Box(HostState), RecomputeBatch -> Box(RecomputeResult)
ui_drop     : Box(HostState) -> {}
```

- `ui_init` runs `build({})` once, produces a `GraphDesc`, and returns it plus
  the initial render patch list. The host ingests the `GraphDesc`, mints ids, and
  builds its node table and adjacency.
- `ui_event` is called when a bound DOM event fires. The host passes the source
  node id and the typed payload (already the right monomorphized type for that
  source). Roc applies the source's reducer and returns the new source value plus
  any `Cmd`s. This is one Roc call.
- `ui_recompute` is how the host invokes derived closures. To keep FFI overhead
  bounded, the host batches: it passes the list of `(node_id, input_values)` for
  the dirty derived nodes it wants recomputed, in dependency order, and Roc
  returns the list of new output values. Batching is part of the protocol from
  day one (see Risks).
- `ui_drop` releases the retained `HostState` box.

> Note on `HostState`: the mutable node table lives in the host, not in Roc. The
> `Box(HostState)` threaded through these signatures is an opaque handle the host
> owns; Roc never reads it. We keep the box in the signatures so refcount and
> lifetime follow the existing ABI helpers in `roc_platform_abi.zig`
> (`allocateBox`, `decrefBoxWith`, `RocErasedCallable`).

### 3. Retained closures

The transform closures (`MapThunk`, etc.) and reducer closures are boxed Roc
values. The host stores them in its node table and re-invokes them. The platform
provides the trampoline that unboxes a thunk and calls it with host-supplied
input values, using the `RocErasedCallable` machinery already in the ABI
(`callable_fn_ptr`, capture pointer, `on_drop`).

The platform holds **exactly one refcount** per retained closure on the host's
behalf; the host drops it via `decrefErasedCallable` when a scope is disposed.

## What Crosses the Host Boundary

Per phase, precisely:

**Ingestion (once):**
- Roc -> host: a `GraphDesc` value (nodes with input ids and boxed thunks,
  sinks, static elem tree, scope boundaries).
- host -> Roc: nothing; the host mints ids and builds adjacency from the desc.

**Initial render (once):**
- host computes initial node values by calling derived thunks in dependency
  order (via `ui_recompute`), then emits a patch list for the simulated DOM.

**Per user event:**
- host -> Roc (`ui_event`): `(source_node_id, typed_payload)`. One call.
- Roc -> host: new source value (boxed opaque) + `List Cmd`.
- host marks the source dirty, computes the dirty derived set in topological
  order, and calls Roc (`ui_recompute`) with **only** the changed derived nodes
  and their input values — batched.
- Roc -> host: new output values for those nodes (boxed opaque).
- host prunes propagation where an output is `Eq` to its cached value, then emits
  minimal DOM patches for sinks whose value changed.

**Per effect result / timer tick:**
- Same as a user event: the host sets the effect's source node and runs the same
  propagation. Async results re-enter through one scheduler and one path.

The boxed opaque values and thunks never have their bytes interpreted by the
host except through the generated `Eq`/`Encode`/`Decode` thunks for that exact
edge.

## The Host's Responsibilities

The host is the mutable reactive runtime plus the (simulated) DOM.

### Node table and graph

Per node id the host stores:
- kind (source / map / map2 / combine / sink),
- forward adjacency (source id -> list of dependent ids) built from the desc,
- a topological **rank** (height) computed once at ingestion (the desc is a DAG;
  cycles are a host error),
- the cached current value (boxed opaque Roc value),
- the retained transform thunk (for derived nodes) or reducer thunk (for
  sources),
- the owning scope id.

Adjacency, ranks, and the dirty set are dense integer-indexed structures. No
string keys, no scans to rediscover identity, no `Dict(Str, _)`; identity is
dense integers throughout.

### Propagation algorithm (push-based, glitch-free, value-pruned)

On a source update:

1. Set the source node's value; push it on a priority queue keyed by topological
   rank.
2. Pop nodes in increasing rank order. For each derived node whose input changed,
   request its recompute (batched) from Roc with input values already in the
   table.
3. If the new value is `Eq` to the cached value, **stop** — do not dirty its
   dependents. Otherwise update the cache and enqueue its dependents.
4. For sink nodes whose value changed, emit the minimal DOM patch (`SetText`,
   `SetValue`, `SetChecked`, `SetDisabled`, attribute set).

Rank ordering guarantees a diamond (`a->b`, `a->c`, `(b,c)->d`) recomputes `d`
exactly once after both `b` and `c` settle — glitch freedom at runtime with no
re-sort. Value pruning is the second half of linear-with-changes scaling.

### Event routing

The host maintains a dense `event_id -> source_node_id` table built from
`BindClick`/`BindInput`/`BindCheck` sinks in the desc. When a simulated DOM
listener fires, it looks up the source node in O(1) and calls `ui_event`. No
scan, no string lookup.

### Scopes and lifecycle

The host owns a forest of scopes. On a `Ui.when` flip or a `Ui.each` key-set
change:
- diff the new structure against the old (key-set diff for lists, branch flip for
  conditionals),
- mint a scope for new branches/keys (run that scope's `build` once, ingesting
  the sub-desc Roc returns for it),
- dispose scopes for removed branches/keys: remove their ids from the table and
  adjacency, call `decrefErasedCallable` on each retained closure (Roc reclaims
  captured environments), run any `Ui.on_cleanup` task, and detach the DOM
  subtree,
- reorder list rows by moving DOM nodes, never rebuilding surviving rows.

`keep_alive` is an explicit per-scope flag, never a heuristic.

**Leak invariant:** the host holds exactly one refcount per live retained
closure/value and zero for disposed ones. The graph is a DAG; the host's
back-references are not Roc-visible, so there are no refcount cycles.
Reclamation is deterministic, no GC.

### Simulated DOM (test-runner specifics)

The host keeps the existing simulated DOM shape from `host.zig`: a flat array of
`DomElement` records (tag, role, label, test_id, text, value, checked, disabled,
parent, children, bound events, per-field update counters) and the
semantic-locator test spec parser (`role:`, `label:`, `text:`, `test_id:`, and
`expect_*` / `click` / `fill` / `check`). This is retained because it is what
lets us assert UI behavior in user-facing terms and benchmark update counts.

The host applies render patches to this array exactly as a browser host would
apply them to a real DOM, and dispatches spec actions (`click`, `fill`, `check`)
by firing the bound event id back into `ui_event`.

The patch command set is the typed, host-independent set already present:
`ResetDom`, `CreateElement`, `AppendChild`, `SetText`, `SetValue`, `SetChecked`,
`SetDisabled`, `SetRole`, `SetLabel`, `SetTestId`, `BindClick`, `BindInput`,
`BindCheck`. A browser host implements the same commands against the real DOM.

### Metrics

The host retains a metrics record for benchmarking. The meaningful counters
are: `events_processed`, `nodes_recomputed` (should track changed nodes, not
graph size), `propagation_prunes` (Eq short-circuits), `derived_calls_into_roc`,
`recompute_batches`, `patches_emitted`, `scopes_created`, `scopes_disposed`,
`rows_reused`, `rows_created`, `rows_removed`, `closure_retains`,
`closure_releases`, and `retained_alloc_delta`. `rows_reused` must count actual
subtree reuse — a row is only counted as reused when its scope (and local state)
is preserved across the update.

## Glitch Freedom, Ordering, and Async

- **Glitch freedom:** topological-rank scheduling, computed once at ingestion.
- **Update ordering:** a single dirty priority queue per propagation; effect
  results and timer ticks enter the same queue, so there is one ordering
  authority.
- **Async / cancellation:** `Cmd` requests carry a host-assigned request id tied
  to the owning scope and node — never an app string. Disposing the scope cancels
  the request. `Sub`s are declared by structure; the host diffs the declared
  subscription set against the live set and starts/stops accordingly.
- **Errors:** `Signal.from_task` yields `[Loading, Done a, Failed err]`, so error
  states are ordinary signal values the app folds and renders. There is no
  effect-inside-signal-evaluation; effects are sources.

## Implementation Plan

1. Build the simulated DOM, spec parser, and ABI box/refcount helpers in the
   host.
2. Implement the `roc_ui_init` ingestion of `GraphDesc`, the `ui_event` and
   batched `ui_recompute` protocol, and `ui_drop`.
3. Implement the Roc graph-description builder (`Signal`, `Html`, `Ui`) and the
   retained-thunk trampolines over `RocErasedCallable`.
4. Build the three representative apps (`ops_dashboard`, `checkout_wizard`,
   `kanban_board`) on the API and run their specs against the simulated host.

## Risks to De-Risk Before Building the Full API

These three experiments gate the design. Build them as tiny host+Roc harnesses
first.

1. **Retained Roc closures across FFI: refcount correctness and per-call cost.**
   The host stashes boxed Roc closures and calls them many times, dropping
   exactly one refcount on disposal. Prototype: retain 10k boxed `I64 -> I64`
   closures, invoke ~1% per simulated event over 100k events; measure ns/call and
   confirm RSS stays flat via the host allocator hooks. If per-call FFI dominates,
   the `ui_recompute` batch protocol (one call returning many outputs) is the
   mitigation — which is why batching is in the protocol from the start. If
   retention/refcount is unsafe on current Roc, this design does not stand; test
   it first.

2. **Construction-site identity under dynamic shape.** This replaces string
   collisions with a positional-shift failure mode, safe only if scope seams
   fully contain shifting. Prototype: nested `Ui.when` inside `Ui.each` inside
   `Ui.when`; script insert/remove/reorder/toggle; assert per-row/branch local
   state is preserved or correctly disposed, tagging each state node with its
   minted id and key path. Watch for state "teleporting" between rows.

3. **Glitch-free, Eq-pruned propagation plus thunk cost.** Prototype the diamond
   graph; flood updates to the root; assert the join recomputes exactly once per
   event. A/B the runtime with and without Eq-pruning on a high-fan-out graph of
   mostly-unchanged values to confirm pruning pays for the `Eq` thunk calls.

Also confirm two maturity unknowns alongside: that `Encode`/`Decode`/`Hash`/`Eq`
ability derivation and dispatch work across the platform boundary on current Roc,
and that deeply nested generic `Signal`/`Elem` combinators do not blow up
monomorphization or compile time.

## Success Metrics

- **Update amplification:** nodes recomputed and patches emitted per event should
  track the number of *changed* nodes, not graph or tree size. This is the
  headline metric for the platform.
- **Per-event Roc calls:** bounded (one `ui_event` + batched `ui_recompute`),
  independent of tree size.
- **Allocations per event and under list churn:** flat; surviving rows reused.
- **Retained memory over a long session:** flat (`retained_alloc_delta` near
  zero after warmup).
- **Row reuse correctness:** `rows_reused` reflects actual subtree reuse across
  reorder/filter, and per-row state survives.
- **Determinism:** the same spec produces the same patch sequence every run.

## Open Questions

- **Component-local state vs. threaded sources.** `Ui.component` scopes give
  local state, but how local state composes across helper functions returning
  `Elem` needs validation (experiment 2).
- **Animation / high-frequency continuous values.** A push graph driven by
  discrete updates may need a dedicated `interval`-driven path for smooth
  animation; revisit after benchmarks.
- **Uncontrolled native state** (focus, IME composition, selection, scroll) is
  not in the signal graph; the browser host will need a reconciliation story that
  the simulated host can stub.
- **Recompute batch granularity.** The exact batching boundary in `ui_recompute`
  (whole dirty frontier vs. per-rank) is a tuning parameter to measure, not fix,
  up front.
