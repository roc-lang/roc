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
   is no full-tree re-walk and no full re-render per event. **This constraint
   binds the data structures, not just the algorithm.** A path that is "linear in
   the changed set" on paper but reaches that set through a linear scan of all
   nodes, a linear pointer→id lookup, or a full graph rebuild is a violation, not
   an implementation detail. Identity→id resolution, descriptor lookup, and
   dependency-graph maintenance must be O(1) or O(changed), never O(total). See
   *Complexity Discipline* below for the precise budget every code path owes.
4. **Mutation lives only in the host.** Roc is pure and value-oriented. The
   reactive runtime — the dirty set, the scheduler, the in-place node table — is
   intrinsically mutable, so it lives in the Zig host, which is the one place
   mutation is legal.
5. **Type-mismatch crashes are structurally impossible.** A typed `Signal(a)`
   stays typed end to end. Erasure is confined to one generated thunk per edge,
   pinned to that edge's monomorphized types. There is no second, independently
   typed read site that can disagree with the writer.

## First Principles, Not Imitation

We are not porting Solid, Elm, or Incremental to Roc. We are deriving the model
that *fits Roc* — a pure, value-oriented language with no mutable globals, no
ability auto-derivation, and a hard "no compiler changes" rule. Those three
frameworks are useful reference points for what works and what does not, but the
design below follows from Roc's constraints, not from any of them.

The reference points:

**Elm** rebuilds the view and diffs per event. That is O(view size) per event
and is the opposite of signals. We reject the *mechanism* (per-event re-render),
not the discipline — Elm's "pure description of UI, effects as data" is exactly
the shape we keep.

**Solid** discovers dependencies by *running* effects and watching reads through
a mutable global "current observer." Its great strength is precise, lazy edges:
a node that conditionally reads `B` only depends on `B` while it actually reads
it. Roc cannot observe its own reads and has no mutable globals, and we cannot
add compiler support to fake it. So we cannot copy Solid's *mechanism*.

**Jane Street's Incremental / Adapton** are the honest lineage of what we *can*
build: an explicitly-constructed dependency graph, a topological-rank scheduler,
and a value cutoff (`is_eq`) that stops propagation when a recomputed value is
unchanged. This is push-based incremental computation over a graph the program
declares rather than one the runtime discovers.

**The reframing that makes signals work in pure Roc:** in Solid, dependencies
are *discovered* at runtime. In a pure language, dependencies are *declared* —
they are already present in the structure of each `map`/`map2`/`combine` call
before anything runs. When the app writes `Signal.map2(price, qty, f)`, the
edges `price -> result` and `qty -> result` are *data*. So the host never needs
a current-observer stack and never runs a closure to find out what it reads. The
**dependency graph is handed to the host as a value, once.** The host then owns
a mutable node table and runs push-based incremental propagation over it.

This is "the graph is Roc data, the runtime engine is the host." It keeps
signals' linear-with-changes scaling, fits Roc's purity, and needs zero compiler
changes.

**The tradeoff we accept, stated plainly.** Declared edges are *eager*, not
lazy. A node that depends on `{cond, A, B}` to express `if cond then A else B`
stays subscribed to all three; when `cond = true` and only `B` changes, the host
still wakes the node and runs its transform, and `is_eq` pruning only suppresses
the *output* after the work is done. Solid would not wake the node at all. We
accept this because the alternative — lazy, read-tracked edges — requires
observing Roc's reads, which the no-compiler-changes rule forbids. The escape
valve for genuinely dynamic dependency *structure* (a derivation over a
value-dependent set of inputs) is the same scope mechanism that powers `Ui.each`
(see Identity and Dynamic Structure): a sub-graph that is rebuilt when its shape
changes, not a static edge that is always live. Dynamic-cardinality reactivity
and dynamic list structure must therefore share one mechanism, never two.

## Architecture Overview

```text
   App (Roc)                Platform (Roc)              Host (Zig)
   ---------                --------------              ----------
   build : {} -> Elem       Ui.* / Signal.* / Html.*    node table (mutable)
   pure description    ───▶  retained descriptor tree ─▶ mint ids, adjacency,
   (built once)              with boxed typed thunks      dirty set, scheduler
                            (roc_ui_init, once)           simulated DOM

   DOM listener fires ─────────────────────────────────▶ route event id -> source
                                                          call retained reducer thunk
   retained closures   ◀── host calls them in-process ── propagate in dep order,
   (transforms/eq/      ──▶ pure value out ─────────────▶ invoke only changed
    reducers)                                             derived closures,
                                                          emit minimal patches
```

The Roc side is built **once** per app lifetime into a descriptor tree that the
host ingests through a single `roc_ui_init` entrypoint. The descriptor tree
carries retained, boxed, monomorphized closures (transforms, equality, reducers)
at every typed edge. After ingestion the host drives all subsequent events
**in-process**: it owns the mutable node table and calls the retained Roc
closures directly through the ABI's `RocErasedCallable` machinery. There is no
per-event FFI round-trip back into a Roc entrypoint — the only Roc entrypoint is
`roc_ui_init`. This is a deliberate choice (see Entry points): it removes
per-event call-boundary overhead entirely, which is a stronger answer to the
retained-closure cost risk than batching calls would have been.

## Core Concepts

- **Signal(a)** — a continuous, always-present value of type `a`. Opaque typed
  descriptor that references a source binder or derived expression. The `a`
  exists only in Roc's type system; the host assigns the runtime node id when it
  ingests the descriptor tree.
- **Source** — a node whose value is set by host input (a DOM event, a timer, an
  effect result). Local state sources are introduced by the `Ui.state` closure
  binder; effect combinators introduce effect sources.
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
- **Dynamic lists use typed keys, not position.** `Ui.each` takes a typed key
  function `item -> key` where `key` provides `hash` and `is_eq` methods (the
  `where [key.hash : ..., key.is_eq : ...]` static-dispatch constraints below).
  The item type also provides `is_eq`, so the host can distinguish "same row
  identity, unchanged row value" from "same row identity, changed row value"
  without guessing from bytes. A row's identity is its key, so per-row local
  state survives reorder/insert/delete. Duplicate keys are an `is_eq` question
  the host answers explicitly (reported as a host error), never a silent alias.

This replaces string-collision/rename hazards with explicit, typed structure.

## Confined Erasure: No NodeValue, No Decode Crash

The host's node table is heterogeneous, so values cross the boundary as opaque
payloads. But erasure is confined to **one generated set of thunks per edge**,
pinned to that edge's monomorphized types:

- For each `map`/`map2`/`combine`/`state`/sink, the platform-glue layer emits the
  concrete `is_eq` comparison (for change pruning) and — only where a value must
  be stored or moved as bytes — `encode`/`decode` thunks, by static dispatch on
  the value type. These are generated from the same call site whose argument
  types are tied to the source `Signal(a)`'s `a`. (Static dispatch resolves each
  call to a concrete method on the value's type; there is no auto-derivation, so
  the value's type must define those methods, and monomorphization specializes
  the thunk per instantiation.)
- The host **never chooses** a decoder. It stores a boxed, opaque Roc value and
  hands it back to the one thunk that owns that edge. There is no second,
  independently typed read site, so a mismatch is a compile-time type error, not
  a runtime decode crash.

Hot-path values are stored as **boxed typed Roc values the host never
inspects**, with an `is_eq` thunk used for change pruning. Byte serialization is
reserved for persistence and the wire, never forced on every event. We do not
`memcmp` encoded bytes for equality (fragile for floats/maps); we call the
generated `is_eq` thunk.

This opaque carrier must be produced at a real typed edge boundary. It is not a
generic `Box({})` field in the descriptor tree: Roc keeps `Box(a)` typed, so a
generic `Opaque(a)` value cannot be placed directly into heterogeneous `Elem`
payloads. The platform boundary must either generate an erased host value cell at
the monomorphized call site or reshape descriptors so each stored value is owned
by the one typed thunk that can read it.

There is no untyped value representation crossing the boundary. The public API
is a few polymorphic functions (below); monomorphization generates concrete code
for each instantiation, so there is no hand-written family of type-specialized
combinators.

### Where the invariant actually lives, and how we check it

Confined erasure moves the type-mismatch hazard, it does not delete it. Roc's
type system guarantees that *each* thunk is internally type-correct. It does
**not** guarantee that the host hands a given opaque payload to the *right*
thunk: that correctness is a property of the host's wiring matching the
descriptor that produced the thunks. A wiring bug — delivering the box from edge
X to the thunk that owns edge Y — is therefore not a clean Roc error but
undefined behavior in the thunk.

Two rules keep this invariant honest:

1. **The routing is consumed, never reconstructed.** Per `AGENTS.md`, the host
   builds its `event_id -> source`, edge, and sink tables from explicit tokens
   in the descriptor. It never re-derives which thunk owns which value by
   guessing from structure or bytes.
2. **Debug-only carrier type tags.** Every opaque `HostValue` cell carries a
   monomorphized type tag, set where the value is produced and asserted in the
   thunk trampoline before the value is read. The tag and its assertions compile
   **only** in debug/safe builds (`std.debug.assert` / a `builtin.mode` gate) and
   are stripped from release builds, so they add confidence with zero release
   cost. If these assertions never fire across the full spec suite, the displaced
   invariant is demonstrably holding. This is the cheapest possible proof that
   confined erasure is wired correctly, and it is part of the design, not an
   optional extra.

## App-Facing API

The app sees `Signal(a)`, typed keys, `Elem`, `Cmd(a)`, `Sub(a)`, and a small
set of polymorphic functions. It never sees ids, string keys, `NodeValue`,
lifecycle tokens, or subscription internals.

### Module surface

Signatures use current Roc: parenthesized type application (`Signal(a)`,
`List(Elem)`), and `where [...]` static-dispatch constraints naming the methods
a type variable must provide. There is no `implements`/ability syntax; a
constraint such as `a.is_eq : a, a -> Bool` says "the concrete type bound to `a`
must define an `is_eq` method of that signature," which monomorphization
resolves and specializes.

```roc
# Opaque to the app:
Signal(a)
Elem
Cmd(a)
Sub(a)
Scope            # only appears in Ui.component / cleanup signatures

# Signal construction and combination
Signal.const : a -> Signal(a)
    where [a.is_eq : a, a -> Bool]
Signal.map : Signal(a), (a -> b) -> Signal(b)
    where [b.is_eq : b, b -> Bool]
Signal.map2 : Signal(a), Signal(b), (a, b -> c) -> Signal(c)
    where [c.is_eq : c, c -> Bool]
Signal.combine : List(Signal(a)) -> Signal(List(a))
    where [a.is_eq : a, a -> Bool]

# Async / effects as sources (same propagation path as user events)
Signal.fake_task : Str, (Str -> a), (Str -> err) -> Task(a, err)
Signal.from_task : Task(a, err) -> Signal([Loading, Done(a), Failed(err)])
Signal.fold_task : Task(a, err), b, (a -> b), (err -> b) -> Signal(b)
Signal.start_str : Task(a, err), Str -> Cmd
Signal.cleanup : Str -> Cleanup
Signal.interval : U64 -> Signal(U64)  # test host: period ms -> tick count
Ui.on_change : Signal(a), (a -> Cmd) -> Elem  # sink: fires a Cmd when value changes
Ui.on_cleanup : Cleanup -> Elem               # runs at scope disposal

# Structure
Html.div : List(Attr), List(Elem) -> Elem
Html.button : List(Attr), List(Elem) -> Elem
Html.input : List(Attr), List(Elem) -> Elem
Html.text : Str -> Elem            # static text
Html.text_s : Signal(Str) -> Elem  # signal-backed text (a sink)

# Attributes (signal-backed where dynamic)
Html.value : Signal(Str) -> Attr
Html.checked : Signal(Bool) -> Attr
Html.disabled : Signal(Bool) -> Attr
Html.on_click : Msg -> Attr
Html.on_input : (Str -> Msg) -> Attr
Html.on_check : (Bool -> Msg) -> Attr

# Dynamic structure (explicit scopes)
Ui.state : a, (State(a) -> Elem) -> Elem
    where [a.is_eq : a, a -> Bool]
State.signal : State(a) -> Signal(a)
State.on_unit : State(a), (a -> a) -> Msg
State.on_value : State(a), (a, payload -> a) -> Msg
Ui.when : Signal(Bool), ({} -> Elem), ({} -> Elem) -> Elem
Ui.each : Signal(List(item)), (item -> key), (key, Signal(item) -> Elem) -> Elem
    where [
        item.is_eq : item, item -> Bool,
        key.hash : key, Hasher -> Hasher,
        key.is_eq : key, key -> Bool,
    ]

# Components (named scopes for local state)
Ui.component : ({} -> Elem) -> Elem
```

`Msg` here is the unit of host-to-Roc dispatch: a `send`-bound reducer plus an
optional payload. `Html.on_input(name_changed)` means "when this input fires,
route the typed payload through `name_changed` and apply the bound reducer." The
app never names an event id; the host mints and routes them.

### Example: counter

```roc
counter : Elem
counter =
    Ui.state(0i64, |count_state| {
        count = count_state.signal
        dec = count_state.on_unit(|n| n - 1)
        inc = count_state.on_unit(|n| n + 1)

        Html.div(
            [],
            [
                Html.button([Html.on_click(dec)], [Html.text("-")]),
                Html.text_s(Signal.map(count, |n| n.to_str())),
                Html.button([Html.on_click(inc)], [Html.text("+")]),
            ],
        )
    })
```

### Example: derived value

```roc
full_name : Signal(Str), Signal(Str) -> Signal(Str)
full_name = |first, last| Signal.map2(first, last, |f, l| "${f} ${l}")
```

### Example: text input with retained state

```roc
name_field : Elem
name_field =
    Ui.state("", |text_state| {
        text = text_state.signal
        Html.input(
            [Html.value(text), Html.on_input(|new| text_state.on_value(|_, _payload| new))],
            [],
        )
    })
```

`text` survives across events because the `Ui.state` binder is an
identity-bearing construction site. The host holds it by its minted id, not by a
string and not by re-derived tree position.

### Example: keyed list with per-row local state

```roc
# A nominal key type provides hash and is_eq through its method block, so it
# satisfies the `where [key.hash, key.is_eq]` constraint on Ui.each.
TodoId := [Tid(U64)].{
    hash : TodoId, Hasher -> Hasher
    hash = |id, hasher| match id {
        Tid(n) => Hasher.write_u64(hasher, n)
    }

    is_eq : TodoId, TodoId -> Bool
    is_eq = |left, right| match left {
        Tid(a) => match right {
            Tid(b) => a == b
        }
    }
}

todo_list : Signal(List(Todo)) -> Elem
todo_list = |todos|
    Html.ul(
        [],
        [
            Ui.each(
                todos,
                |todo| todo.id,
                |_key, row| {
                    Ui.state(Bool.false, |editing_state| {
                        editing = editing_state.signal
                        Html.li(
                            [],
                            [
                                Html.text_s(Signal.map(row, |t| t.title)),
                                Html.button(
                                    [Html.on_click(editing_state.on_unit(|e| !e))],
                                    [Html.text_s(Signal.map(editing, |e| if e { "done" } else { "edit" }))],
                                ),
                            ],
                        )
                    })
                },
            ),
        ],
    )
```

`editing` is a per-row source inside the row scope. It survives reorder/filter
because the row scope is keyed by the typed `key`, not by the index.

## The Roc Platform Layer

The platform turns the app's pure description into a retained descriptor tree,
hands it to the host once, and provides the retained closures the host calls
back into. The platform has three responsibilities and no reactive runtime of
its own.

### 1. The descriptor tree as an explicit value

`Signal(a)` is an opaque descriptor that references a state/source binder or a
derived expression. Roc does not thread an ordinal counter while building the
tree; `build` returns a pure descriptor tree (`Elem` with embedded `SignalExpr`
edges), and the host assigns dense ids by walking identity-bearing construction
sites in deterministic pre-order. Each signal edge records its kind and inputs:

```roc
# Conceptual shape. The implementation calls the signal node type `SignalExpr`
# and carries identity via explicit per-edge tokens rather than dense list
# indices; the host interns those tokens into shared records at ingestion.
SignalExpr := [
    Ref(U64),                                          # bound to a host source node id
    ConstValue({ value : HostValue, eq : EqThunk }),
    Map({ input : SignalExpr, transform : MapThunk, eq : EqThunk }),
    Map2({ left : SignalExpr, right : SignalExpr, transform : Map2Thunk, eq : EqThunk }),
    Combine({ inputs : List(SignalExpr), eq : EqThunk }),
]
```

`MapThunk`/`Map2Thunk`/`EqThunk` are boxed monomorphized closures (the confined
erasure). They are produced from `Signal.map`/`map2`/`Ui.state` at the call site,
so their input and output types are pinned to the surrounding `Signal(a)`. A
source's initial value, sink reads, event payloads, structural conditions, and
keyed row key/item slots are all carried as opaque `HostValue` handles with the
typed equality/drop/read thunks owned by that exact edge. A `HostValue` is **not**
a literal `Box(OpaqueValue)` field in the heterogeneous descriptor tree; Roc
cannot erase `Box(a)` that way. The value is produced at the monomorphized edge
and stored in a host value cell; the descriptor carries only the opaque handle
plus the thunks that own it.

The platform does **not** evaluate the graph. It only describes it. There is no
`eval_signal`, no dirty propagation, no cache in Roc.

### 2. Entry point

There is exactly **one** Roc entrypoint. The host owns the mutable node table
and drives every event in-process, calling retained Roc closures directly. There
is deliberately no per-event Roc entrypoint and no `ui_recompute` round-trip.

```roc
# platform/main.roc (current and target shape)
roc_ui_init : {} -> Box(Elem)
```

- `roc_ui_init` runs `main({})` once and returns the boxed descriptor tree. The
  host ingests it, mints dense ids, interns signal tokens into shared records,
  builds adjacency and topological ranks, computes initial values by calling the
  retained transform thunks in dependency order, and emits the initial render
  patches.
- **Per event there is no Roc entrypoint call.** A bound DOM listener fires; the
  host routes the event id to its source node (O(1)), calls that source's
  retained reducer thunk directly through `RocErasedCallable`, then propagates in
  rank order, invoking only the changed derived nodes' retained transform thunks.
  Every Roc call per event is a direct closure invocation, not an FFI entrypoint
  crossing.
- **Dynamic structure (`Ui.each` rows, `Ui.when` branches) also needs no
  entrypoint.** When a new list key or branch appears at runtime, Roc must
  *produce new UI structure* that did not exist at init — but it does so through a
  **retained builder closure**, not a new entrypoint. The row builder you pass to
  `Ui.each` (`|key, row| ...`) and each branch body of `Ui.when` are captured at
  init as `RocErasedCallable` values (the host stores `row`, `when_true`,
  `when_false`). When the host needs a new row, it calls the retained `row`
  closure directly with the new key/item and receives a fresh `Elem` sub-tree —
  the exact same kind of direct pointer call as a reducer, except it returns
  *structure* instead of a *value*. This is why no `roc_ui_each` or similar
  entrypoint is needed or wanted: the host already holds a direct pointer to the
  specific builder for that specific site. Adding an entrypoint would reintroduce
  the boundary crossing this model exists to remove, and force the host to ask
  Roc "which builder?" when it already knows. `Ui.each` patch locality is
  host-side: the host splices returned row sub-trees into affected scopes and
  preserves surviving row scopes instead of re-entering the root descriptor.
- **Why a single entrypoint, not the batched protocol.** An earlier design
  sketched four entrypoints (`ui_init` / `ui_event` / `ui_recompute` / `ui_drop`)
  with a batched recompute round-trip to amortize FFI cost. The in-process model
  supersedes it: there is no per-event entrypoint crossing to amortize, so the
  batching machinery is unnecessary. The retained-closure-cost risk is answered
  by *not making the call a boundary crossing at all*, which is strictly cheaper.
  The hosted `roc_host_value_*` functions exist purely so Roc can mint/read/drop
  values in the host's value-cell registry; they are not a per-event dispatch
  path.

The mutable node table lives in the host, not in Roc. There is no `HostState`
box threaded through Roc; refcount and lifetime of retained closures follow the
existing ABI helpers in `roc_platform_abi.zig` (`allocateBox`, `decrefBoxWith`,
`increfErasedCallable`, `decrefErasedCallable`, `RocErasedCallable`). The host
drops the descriptor tree (and with it every retained closure) at shutdown.

### 3. Retained closures

Every callback the host ever needs is handed to it **once**, inside the `Elem`
descriptor that `roc_ui_init` returns, as a boxed Roc closure (`RocErasedCallable`:
a compiled-Roc function pointer plus its captured environment stored inline). The
host increfs and stores these in its node table and re-invokes them by direct
function-pointer call. The platform provides the trampoline that unboxes a thunk
and calls it with host-supplied inputs, using the `RocErasedCallable` machinery
already in the ABI (`callable_fn_ptr`, capture pointer, `on_drop`).

There are two kinds, invoked **identically** (the same direct pointer call), so
neither needs an entrypoint:

- **Value closures** — reducers (`Msg`), `map`/`map2`/`combine` transforms, `eq`,
  `read`, `key_of`. Take values, return a value. Run on every relevant event.
- **Structure closures** — `Ui.each` row builders and `Ui.when` branch bodies.
  Take values (a key/item, or unit), return an `Elem` sub-tree. Run when a new
  row or branch must be materialized at runtime.

The only difference between the two is the *return type* (a value vs. a piece of
UI). Both are pre-compiled Roc functions the host points at directly. This is the
key to the cost model: **all per-event and all dynamic-structure work is direct
closure invocation; the only exported entrypoint is the one-time
`roc_ui_init`.** A structure closure producing new UI at runtime is not a special
case requiring a generic door back into Roc — the host already holds the pointer
to the exact builder for that exact construction site.

The platform holds **exactly one refcount** per retained closure on the host's
behalf; the host drops it via `decrefErasedCallable` when a scope is disposed.

## What Crosses the Host Boundary

Per phase, precisely:

**Ingestion (once, `roc_ui_init`):**
- Roc -> host: a boxed `Elem` descriptor tree (signal edges with inputs and boxed
  thunks, sinks, structure, scope boundaries, opaque `HostValue` cells).
- host -> Roc: nothing; the host mints ids, interns tokens, and builds adjacency
  and ranks from the descriptor.

**Initial render (once):**
- host computes initial node values by calling the retained transform thunks
  directly in dependency order, then emits a patch list for the simulated DOM.

**Per user event (no Roc entrypoint crossing):**
- host -> Roc: a direct call to the source's retained reducer thunk with the
  typed payload. Returns the new source value (opaque `HostValue`).
- host marks the source dirty, walks the dirty derived set in topological-rank
  order, and calls each changed node's retained transform thunk directly with
  input values already in the table.
- host prunes propagation where an output is `is_eq` to its cached value, then
  emits minimal DOM patches for sinks whose value changed.

**Per effect result / timer tick (target):**
- Same as a user event: the host sets the effect's source node and runs the same
  propagation. Async results re-enter through one scheduler and one path.

The opaque `HostValue` payloads and thunks never have their bytes interpreted by
the host except through the generated `is_eq`/`encode`/`decode` thunks for that
exact edge.

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

### Complexity Discipline (the foundation budget)

The scaling claim must be true *in the data structures*, not only in the
algorithm. Every host path owes an explicit complexity budget, and a path that
exceeds its budget is a defect of the same severity as a wrong output — it
silently breaks Constraint 3. A path that is "linear in the changed set" on paper
but reaches that set through a linear scan of all nodes, a linear pointer→id
lookup, or a full graph rebuild does not meet its budget.

The budgets, by operation (N = total live signal records/render nodes in the
active tree; C = changed set for this event; K = rows touched by a structural
change; L = rows at the affected `Ui.each` site):

| Operation | Required budget | Forbidden |
|---|---|---|
| record/elem identity → id lookup | O(1) | linear pointer scan over the node table |
| descriptor lookup by `elem_id` | O(1) | linear scan over the descriptor arrays |
| non-structural event propagation | O(C + fanout) | O(N); O(fanout²) dedup/sort |
| `Ui.when` branch flip | O(changed subtree) | O(N) field/route/graph rebuild |
| `Ui.each` keyed diff | O(L) via key hash index | O(L²) `is_eq` scan |
| `Ui.each` append/remove/filter | O(K) | O(N) per touched row |
| `Ui.each` reorder | O(K moved) DOM moves | O(L) whole-site re-collect + rebuild |
| dependency-graph maintenance after a splice | O(affected scope) | full clear-and-rebuild of the active graph over N |
| host allocation / free bookkeeping | O(1) per alloc/free | O(live allocations) scan per free |
| spec/bench action target resolution | acceptable O(DOM) for the *harness*, but excluded from `dispatch_apply_ns` | folding harness lookup time into measured framework cost |

Non-negotiable structural rules that follow from the budget:

- **Identity is resolved through stored ids, never rediscovered.** A signal
  record, a render node, and a DOM element each carry (or index into) their dense
  id directly. The host must never answer "what id is this record?" by walking a
  list comparing pointers, and never answer "what descriptor owns this elem_id?"
  by scanning a descriptor array. Both are the "scan to rediscover identity" that
  Constraint 2 forbids, restated as a performance invariant.
- **The dependency graph is maintained incrementally.** A structural splice
  edits only the records in the affected scope and patches their adjacency/rank
  in place. There is no clear-and-rebuild of the whole active signal graph on a
  structural change. Initial ingestion may be O(N); nothing after it may be.
- **`Ui.each` carries a key hash index.** The `key.hash` constraint in the API is
  load-bearing, not decorative: the host builds a `HashMap(key → row scope id)`
  for each `each` site and uses it for the keyed diff and the duplicate-key
  check. Linear `is_eq` matching is a budget violation. If the implemented API
  ever drops `key.hash`, that is a regression to fix in the API, not a host
  workaround to absorb.
- **Reorder moves, it does not rebuild.** A pure permutation of surviving rows
  must emit only DOM moves for displaced rows (computed against a longest-stable
  subsequence so unmoved rows cost nothing) and must not re-collect row
  descriptors or rebuild the site's signal graph. Whole-site replacement is
  reserved for the case where the *set* of rows changed in a way that genuinely
  cannot be expressed as moves-plus-local-splices, and that case must be named
  explicitly and asserted, never reached by falling through.
- **Allocation bookkeeping is O(1).** The host's allocation ledger (used for
  leak accounting and the allocation metrics) must support O(1) free; storing the
  ledger index in the allocation header is the expected shape. An O(live) scan
  per free makes session cost O(allocs²) and poisons the very allocation
  telemetry it feeds.

### Propagation algorithm (push-based, glitch-free, value-pruned)

On a source update:

1. Set the source node's value; push it on a priority queue keyed by topological
   rank.
2. Pop nodes in increasing rank order. For each derived node whose input changed,
   request its recompute (batched) from Roc with input values already in the
   table.
3. If the new value is `is_eq` to the cached value, **stop** — do not dirty its
   dependents. Otherwise update the cache and enqueue its dependents.
4. For sink nodes whose value changed, emit the minimal DOM patch (`SetText`,
   `SetValue`, `SetChecked`, `SetDisabled`, attribute set).

Rank ordering guarantees a diamond (`a->b`, `a->c`, `(b,c)->d`) recomputes `d`
exactly once after both `b` and `c` settle — glitch freedom at runtime with no
re-sort. Value pruning is the second half of linear-with-changes scaling.

### Event routing

The host maintains a dense `event_id -> source_node_id` table built from
`BindClick`/`BindInput`/`BindCheck` sinks in the descriptor. When a simulated DOM
listener fires, it looks up the source node in O(1) and calls that source's
retained reducer thunk directly. No scan, no string lookup.

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
by firing the bound event id into the source node's retained reducer thunk.

The patch command set is the typed, host-independent set already present:
`ResetDom`, `CreateElement`, `AppendChild`, `RemoveNode`, `MoveBefore`,
`SetText`, `SetValue`, `SetChecked`, `SetDisabled`, `SetRole`, `SetLabel`,
`SetTestId`, `BindClick`, `BindInput`, `BindCheck`. A browser host implements
the same commands against the real DOM.

### Metrics

The host retains a metrics record for benchmarking. The meaningful counters
are: `events_processed`, `nodes_recomputed` (should track changed nodes, not
graph size), `propagation_prunes` (`is_eq` short-circuits), `derived_calls_into_roc`
(direct retained-thunk invocations per event), `recompute_batches`,
`patches_emitted`, render command counters (`reset_dom`, `create_element`,
`append_child`, `remove_node`, `move_before`, `set_text`, `set_value`,
`set_checked`, `set_disabled`, `set_metadata`, `bind_event`),
`scopes_created`, `scopes_disposed`, `rows_reused`, `rows_created`,
`rows_removed`, `closure_retains`,
`closure_releases`, and `retained_alloc_delta`. `rows_reused` must count actual
subtree reuse — a row is only counted as reused when its scope (and local state)
is preserved across the update. These counters are what the simulated host buys
us: they let a spec assert *exactly* how much work an event caused, which is the
property we most need to prove and which a real browser would not expose.

Counters that measure update amplification (`patches_emitted`,
`nodes_recomputed`) are necessary but not sufficient: they count *emitted* and
*recomputed* work, so an O(N²) splice or a full graph rebuild can sit underneath a
low patch count undetected. The telemetry must therefore also expose the
foundation-level work the Complexity Discipline budget governs — *scanned* nodes,
*rebuilt* graph records, key compares, and allocations per event — each named so a
spec can assert a hard bound:

- **`active_graph_records_rebuilt`** — number of signal-graph records whose
  adjacency/rank was (re)constructed this event. For a non-structural event this
  is `0`; for a local structural splice it is bounded by the affected scope, not
  by N. A spec asserting `expect_metric_delta active_graph_records_rebuilt 0` on a
  single-row item change is the canary that fails loudly if a full
  clear-and-rebuild path is introduced.
- **`stream_nodes_scanned`** — number of descriptor/render-node entries visited
  while applying this event's patches. This is the counter that exposes
  full-stream scans hiding behind a low `patches_emitted`.
- **`each_key_compares`** — `is_eq`/hash probes performed in keyed diffs this
  event. With a hash index this tracks L; linear matching makes it track L²,
  which a spec can pin.
- **`allocs_this_event` / `deallocs_this_event`** — per-event allocation deltas,
  so "allocations per event are flat" is an assertion rather than an assumption.

Telemetry placement is deliberate:

- **Spec assertions (`expect_metric_delta`)** carry the scaling *invariants* that
  must hold regardless of timing: `nodes_recomputed`, `patches_emitted`,
  render command counters, `derived_calls_into_roc`, `rows_reused/created/removed`,
  `active_graph_records_rebuilt`, `stream_nodes_scanned`, `each_key_compares`,
  and per-event allocation deltas. These fail the build when a path does O(N)
  work where the budget allows only O(changed).
- **Benchmark CSV only** carries the *timing and aggregate* evidence:
  `dispatch_roc_ns`, `dispatch_apply_ns`, total allocs/deallocs, command
  category counts. Timing corroborates but never gates — a check that can pass
  while real work grows is worse than no check, so timing is never the primary
  guard.

`retained_alloc_delta` measures the allocation residue of a single init-and-replay
cycle, not growth across a long session. Proving "retained memory over a long
session is flat" requires a distinct experiment that reuses one `HostEnv` across
many events and watches the live `allocs − deallocs` gauge over time (see Success
Metrics); per-iteration deltas cannot establish it.

## Glitch Freedom, Ordering, and Async

- **Glitch freedom:** topological-rank scheduling, computed once at ingestion.
- **Update ordering:** a single dirty priority queue per propagation; effect
  results and timer ticks enter the same queue, so there is one ordering
  authority.
- **Async / cancellation:** `Cmd` requests carry a host-assigned request id tied
  to the owning scope and node — never an app string. Disposing the scope cancels
  the request. `Sub`s are declared by structure; the host diffs the declared
  subscription set against the live set and starts/stops accordingly.
- **Errors:** `Signal.from_task` yields `[Loading, Done(a), Failed(err)]`, so error
  states are ordinary signal values the app folds and renders. There is no
  effect-inside-signal-evaluation; effects are sources.

## Implementation Plan

This records the order the platform was built. The steps below are implemented
on the simulated host; `NEXT_STEPS.md` is reserved for newly discovered gaps.

1. Build the simulated DOM, spec parser, and ABI box/refcount helpers in the
   host. **(done)**
2. Implement the `roc_ui_init` ingestion of the descriptor tree, in-host event
   driving over `RocErasedCallable`, and shutdown reclamation. **(done)**
3. Implement the Roc descriptor builder (`Signal`, `Html`, `Ui`) and the
   retained-thunk trampolines, with confined erasure via opaque `HostValue`
   cells. **(done)**
4. Build the representative apps (`ops_dashboard`, `checkout_wizard`,
   `kanban_board`, plus the `identity_stress` fixture) and run their specs.
   **(done)**
5. Make `Ui.each` structural updates scope-local — no active descriptor-stream
   rebuild. **(done)**
6. Add `Ui.component` named scopes for local state across helper functions.
   **(done)**
7. Add debug-only carrier type-tag assertions on the erasure boundary.
   **(done)**
8. Implement effects/subscriptions as sources (`Signal.from_task`,
   `Signal.interval`, `Ui.on_change`, `Ui.on_cleanup`). **(done)**

## Definition of Done

"Done" for this platform is **demonstrating the features a real UI framework
requires, with the scaling properties signals promise, on the simulated host.**
It is not a browser implementation and not exhaustive component coverage. We
declare the signals platform demonstrated when **all** of the following hold,
each proven by a spec or host test that fails if the property regresses:

1. **Linear-with-change scaling holds for every dynamic construct, including
   `Ui.each`.** A spec drives an event into a large list and asserts via
   `expect_metric_delta` that `nodes_recomputed`, `patches_emitted`, and
   `rows_reused`/`rows_created`/`rows_removed` track the *changed* rows, not the
   list size — with no active descriptor-stream rebuild (Plan step 5).
2. **`Ui.component` composes local state across helper functions** without
   identity leaks, proven by an app where reusable UI carrying its own state is
   instantiated more than once and exercised through reorder/disposal.
3. **The confined-erasure invariant is checked, not just argued.** Debug builds
   carry carrier type tags and assert them in every thunk trampoline; the full
   spec suite runs clean in a safe build, and the tags compile out of release
   builds (Plan step 7).
4. **Effects and subscriptions work as sources through the one propagation
   path.** An app issues an async request (via the test host's fake-result
   injection), folds `[Loading, Done, Failed]` into the UI, and a scope disposal
   cancels in-flight work and runs `Ui.on_cleanup` — all asserted by spec.
5. **The success metrics below are all green** across the representative apps and
   the capability apps, and **the same spec produces the same patch sequence
   every run.**
6. **The Complexity Discipline budget holds, proven not argued.** Every operation
   in the budget table meets its bound, verified by:
   - a generated large-N `Ui.each` app (N a build parameter, generated
     systematically — never a handwritten catalog) whose single-row item change
     asserts `expect_metric_delta active_graph_records_rebuilt 0`,
     `stream_nodes_scanned <= O(changed)`, bounded `patches_emitted`, and flat
     per-event allocations across N ∈ {small, large};
   - a reorder step on that app asserting only displaced rows produce DOM moves
     and `active_graph_records_rebuilt` stays bounded by moved rows;
   - the keyed diff backed by a `key.hash` index, with `each_key_compares`
     tracking L (not L²) under churn;
   - an O(1) allocation free path, proven by a long-session experiment that
     reuses one `HostEnv` and shows the live `allocs − deallocs` gauge flat after
     warmup;
   - the benchmark gate (`run-signals-bench`) including the structural/reorder/
     async apps with explicit regression thresholds, not just the scalar apps.

Explicitly **out of scope** for "done": a real-browser host, uncontrolled native
state reconciliation (focus/IME/selection/scroll), animation/high-frequency
paths, and persistence/wire serialization. These are tracked as future stages,
not gates — see Open Questions. The single highest external risk (a browser host
surfacing controlled-input/focus problems the simulated DOM cannot show) is a
deliberate *next* stage, gated behind this Definition of Done, not part of it.

## Proving Breadth and Depth: the App Suite

The representative apps are not demos; each exists to make one capability fail
loudly if it regresses. The bar for adding an app is **"it exercises an
otherwise-unproven capability,"** never size or visual richness. Today's suite:

- `ops_dashboard` — scalar chains, fanout, conditional branch, keyed alert rows,
  input state.
- `checkout_wizard` — keyed wizard steps, form/checkbox state, disabled actions,
  list replacement, per-row state.
- `kanban_board` — keyed reorder/archive/reset, per-card state, `map2` filtering.
- `identity_stress` — `when -> each -> when` row-local state through
  reorder/insert/filter/disposal.

Additional capability fixtures and host tests close the Definition-of-Done
coverage:

- `identity_stress` proves nested structural churn, row reuse, row creation and
  row disposal with metric assertions over `Ui.when`/`Ui.each`.
- `component_composition` proves reusable stateful `Ui.component` instances keep
  local state across keyed row movement and dispose with the owning row scope.
- `async_effects` proves fake task result injection, `[Loading, Done, Failed]`
  rendering through a fold, `Ui.on_change` request issuance, pending-request
  cancellation, deterministic interval ticks, interval cancellation, and cleanup
  execution.
- Host tests cover topological rank ordering, diamond deduplication, confined
  erasure through carrier tags, retained closure lifecycle accounting, dirty
  cache pruning, and local structural splicing.

Keep each new app minimal: the smallest structure that exercises the capability
and the tightest `expect_metric_delta` assertions that prove the scaling
property. Avoid catalog-style fixtures and avoid re-proving already-green
identity behavior.

**Foundation coverage the suite must carry.** Proving behavior is not enough; the
suite must also assert *work*, so a regression to O(N) work fails the build rather
than passing silently. The required proofs, each the smallest that establishes its
property:

- A **generated large-N `Ui.each` app** (the scaling fixture). N is a build
  parameter; the rows are generated programmatically, not handwritten. It is the
  one place where large N is allowed, precisely because it is systematic rather
  than a catalog. Its specs assert the budget for single-row update, append,
  remove, filter, and reorder — including the `active_graph_records_rebuilt`,
  `stream_nodes_scanned`, `each_key_compares`, and per-event allocation counters.
- **Work assertions on the structural apps.** `kanban_board` reorder and filter
  steps, and the `async_effects` cancel cycle, carry `mark_metrics` /
  `expect_metric_delta` blocks that bound work and prove no retained-closure or
  allocation leak across the async open/close cycle.
- **Real-event fanout assertions in `ops_dashboard`.** Beyond any synthetic
  "no-op fanout" probe, the real fanout events bound `nodes_recomputed` /
  `derived_calls_into_roc` so diamond/shared-signal amplification is pinned on the
  live path.
- **A reorder host test at large N** that fails if reorder degrades from
  moves-only to whole-site re-collect.

These are tight, assertion-first additions, not new UI surface.

## Retired Risks

The original gating risks are green on the simulated host: retained closures are
in-process and refcount-correct, construction-site identity survives dynamic
shape changes, debug/safe builds assert carrier type tags at the erasure
boundary, and effects/timers enter the same propagation path as user events.

The Complexity Discipline budget and its telemetry are part of the Definition of
Done (criterion 6), not a separate risk register: a path that exceeds its budget,
or a counter that would let O(N) work pass unnoticed, is a defect to fix, held to
the same bar as a wrong output.

The remaining *external* risks are future-stage questions, not gates on the
simulated-host Definition of Done; see Open Questions.

## Success Metrics

- **Update amplification:** nodes recomputed and patches emitted per event track
  the number of *changed* nodes, not graph or tree size — including under list
  churn, where it must track changed rows. This is the headline metric.
- **Per-event boundary crossings:** zero Roc entrypoint crossings per event; only
  direct retained-closure invocations, their count bounded by the changed set and
  independent of tree size.
- **Allocations per event and under list churn:** flat; surviving rows reused.
- **Retained memory over a long session:** flat (`retained_alloc_delta` near
  zero after warmup).
- **Row reuse correctness:** `rows_reused` reflects actual subtree reuse across
  reorder/filter, and per-row state survives.
- **Determinism:** the same spec produces the same patch sequence every run.

## Open Questions

These are future-stage questions, not gates on the Definition of Done.

- **Real-browser host (highest future risk).** The simulated DOM is structurally
  blind to controlled-`<input>` cursor/selection/IME behavior, event arrival
  during propagation, and layout-driven reads (`getBoundingClientRect`). The G-B1
  spike in `BROWSER_RUNTIME_DESIGN.md` found that `SetValue` must not be a blind
  DOM assignment: equal values are no-ops, and differing values are deferred
  while the input is focused or composing. Full focused-input normalization and
  browser/IME matrix validation remain future-stage work, and may still require
  a first-class input-reconciliation primitive rather than an escape hatch.
- **Animation / high-frequency continuous values.** A push graph driven by
  discrete updates may need a dedicated `interval`-driven path for smooth
  animation; revisit after benchmarks.
- **Uncontrolled native state** (focus, IME composition, selection, scroll) is
  not in the signal graph; the browser host will need a reconciliation story that
  the simulated host can stub.
- **Recompute granularity.** Whether any future batching of in-host recompute
  buys anything is a measurement, not a fixed decision.
