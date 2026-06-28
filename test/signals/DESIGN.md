# Signals UI Platform — Target Design

This document is the single authoritative design for the Signals UI platform. It
describes the architecture we are building toward and the invariants every part
of it must hold. It is forward-looking and enduring: it describes the system as
it is meant to be, not the current state of the work queue. Live work tracking
lives in `NEXT_STEPS.md`.

## Purpose and Dual-Host Architecture

The product is a **host-agnostic reactive engine**: a mutable node table,
topological-rank scheduler, dirty set, `is_eq` value pruning, scope forest,
keyed-row diff, identity tables, and structural splice/collect/apply. The engine
owns all reactive and structural logic. It is the single source of truth for how
a Signals app behaves.

The engine is driven by **two thin hosts** that implement one contract — a
`Ctx` (host capabilities the engine calls) plus a `sink()` (where the engine
writes render commands). The hosts differ only in their boundary, never in their
reactive behaviour:

- **Native host** — the spec/telemetry/debug host. It backs the engine with a
  simulated DOM (a flat `DomElement` array), a semantic-locator spec runner, an
  allocation ledger, and fine-grained work counters. It compiles to a native
  binary, so ordinary tooling (`lldb`, allocation tracing) can inspect crashes
  and memory behaviour directly. Its job is **low-level observability and
  semantic assertion** — the things a real browser cannot show us.

- **Wasm host** — the browser-boundary host. It backs the engine with a
  command-buffer sink serialized into linear memory, plus the JS↔WASM boundary:
  UTF-8 marshalling, event-payload codec, `memory.grow` view coordination, and
  timer/`fetch` bridges. Its job is **the JS↔WASM contract only**. It contains
  no reactive or structural logic; that all lives in the engine.

The same Roc apps compile against both hosts. The native spec runner asserts
semantics and work budgets; the browser runs the apps for real. The JS runtime
is a thin executor of the engine's already-computed command stream — it never
reconstructs meaning, holds reactive state, or re-decides patches.

```text
                          ┌──────────────────────────────┐
                          │   Engine (host-agnostic)     │
   App (Roc)              │   node table, scheduler,      │
   build : {} -> Elem     │   dirty set, is_eq pruning,   │
   pure descriptor   ───▶ │   scope forest, keyed diff,   │
   (roc_ui_init, once)    │   structural splice/apply     │
                          └───────────────┬──────────────┘
                                          │ Ctx + sink()
                       ┌──────────────────┴───────────────────┐
                       ▼                                       ▼
        Native host (Zig binary)                 Wasm host (Zig → wasm32)
        ─────────────────────────                ─────────────────────────
        simulated DOM (DomElement[])             command buffer in linear mem
        spec runner + work counters       ──▶    JS executor applies ops to DOM
        allocation ledger, lldb-debuggable       payload codec, memory.grow,
                                                  timers / fetch bridge
```

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
   intrinsically mutable, so it lives in the Zig engine/host, which is the one
   place mutation is legal.
5. **Type-mismatch crashes are structurally impossible.** A typed `Signal(a)`
   stays typed end to end. Erasure is confined to one generated set of ownership
   operations per edge — a **capability** bundling clone, equality, and drop,
   with typed split private to those operations and edge-specific extension
   records carrying their owning capability — pinned to that edge's
   monomorphized types. There is no host-authored read site that can disagree
   with the writer, and no host-side knowledge of the value's layout. See
   *Confined Erasure*.
6. **One engine, two thin hosts.** All reactive and structural logic lives in the
   shared engine. A host file contains only its boundary (sink, marshalling,
   spec runner / JS bridge) and its `Ctx` implementation. Reactive or structural
   logic appearing in a host file is a defect: it lets the two hosts diverge,
   which this architecture exists to prevent.

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

There are no author-written node ids or event ids. Identity is assigned by the
host during graph ingestion; keyed rows use app-provided stable key material.

- Within a scope, node identity is **construction order** (the order the app
  built the nodes). The app build is pure and deterministic, so this order is
  stable across rebuilds of the same scope.
- **Scopes contain positional shifting.** Because conditional branches and list
  rows are first-class scopes, adding or removing UI inside one scope does not
  shift identities in sibling scopes. This is the new failure mode we design
  around: "where you built it is your identity," so the seams that can shift
  (branches, lists) are explicit scope boundaries.
- **Dynamic lists use stable key material, not position.** `Ui.each_str` takes a
  key function `item -> Str`. The host stores the typed key value, hashes the key
  text privately for its row lookup table, and uses exact key equality for
  collisions and duplicate-key checks. The item type also provides `is_eq`, so
  the host can distinguish "same row identity, unchanged row value" from "same
  row identity, changed row value" without guessing from bytes. A row's identity
  is its key, so per-row local state survives reorder/insert/delete. Duplicate
  keys are reported as a host error, never silently aliased.

This replaces string-collision/rename hazards with explicit, typed structure.

## Confined Erasure: No NodeValue, No Decode Crash

The host's node table is heterogeneous, so values cross the boundary as opaque
payloads. Erasure is confined to **one capability per retained edge**, pinned to
that edge's monomorphized types:

- For each `map`/`map2`/`combine`/`state`/source/sink edge, platform Roc builds a
  concrete capability at the call site. Static dispatch resolves the value's
  required operations (`is_eq`, key hashing, sink reads, and similar
  edge-specific functions), and monomorphization specializes the capability and
  any capability-owned extension record for that edge's concrete `a`.
- The host **never chooses** a decoder, destructor, comparator, or reader. It
  stores a boxed, opaque Roc value and invokes the capability that owns that edge.
  There is no second, independently typed read site, so a mismatch is a routing
  assertion failure in debug builds, not a runtime decode crash.

Hot-path values are stored as **boxed typed Roc values the host never
inspects**. Equality uses the capability's typed `eq`; byte serialization is
reserved for persistence and the wire, never forced on every event. We do not
`memcmp` encoded bytes for equality (fragile for floats/maps), and the host never
reconstructs type semantics from bytes.

This opaque carrier must be produced at a real typed edge boundary. It is not a
generic `Box({})` field in the descriptor tree: Roc keeps `Box(a)` typed, so a
generic `Opaque(a)` value cannot be placed directly into heterogeneous `Elem`
payloads. The platform boundary produces a `HostValue` cell at the monomorphized
call site and carries the capability that owns that cell.

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
2. **Capability ownership assertions.** Every opaque `HostValue` cell carries
   the app-compiled capability that owns it. Public get/take operations must
   present the same capability, and internal split/take operations are accepted
   only while the host is executing an app-compiled callable under an active
   frame containing that owning capability. If a value crosses to the wrong edge,
   the host reports a capability mismatch instead of trying to recover. This is
   part of the design, not an optional extra.

### The capability: bundled ownership operations per retained value

A thunk that *reads* an erased value is not enough. The host does not only read
these values — it **owns their lifecycle**. After `roc_ui_init` the host owns the
mutable runtime graph: state cells, source caches, derived-signal caches, keyed
row key/item cells, pending task payloads, and sink values. Those cells are
replaced during propagation, pruned when unchanged, cloned for non-consuming
reads, and destroyed when a scope is disposed or the app unmounts. At each of
those moments the host is the only code that knows a particular value is now
dead, so the host is the code that must release it.

The prebuilt host cannot release an app value by inspecting it. The host can own
an opaque cell, but `a` comes from the app, not the platform. When a retained
`Box(a)` reaches its final release, the payload's nested refcounted fields — a
`Str` backing, a `List`, a record of lists, a tag union carrying a heap string —
must also be released, and that requires the concrete monomorphized layout the
prebuilt host was compiled without.

Every retained `HostValue` is therefore paired with a **capability**: one bundled
record of app-compiled, monomorphized operations for that value's exact type,
produced at the same typed edge that produced the value. The capability is the
typed instruction manual attached to the opaque cell. Conceptually:

```roc
# Produced at the monomorphized edge, stored beside the opaque cell.
CapabilityHandle := {
    clone : Box((HostValue -> HostValue)),        # split-and-store clone
    eq    : Box((HostValue, HostValue -> Bool)),  # value pruning
    drop  : Box((HostValue -> {})),               # release, incl. nested fields
}
```

- **Clone, equality, and drop are the universal trio** every retained value
  needs, because every retained value can be copied for a read, compared for
  pruning, and released on disposal. They are bundled into one object so a value
  and the operations that own it cannot drift apart.
- **Typed split is private to the app-compiled capability operations.** The
  platform Roc wrapper for `Capability(a)` builds a typed
  `split : Box(a) -> { keep : Box(a), out : Box(a) }` closure and captures it in
  the generated `clone`, `eq`, and `drop` callables. The heterogeneous descriptor
  graph stores only the erased handle above, not a parameterized
  `CapabilityHandle(a)`.
- **Reads, reducers, and row operations are edge-specific extensions**, not part
  of the universal trio. A signal-backed text edge owns a
  `{ capability, read : HostValue -> Str }` record; a `Ui.when` condition owns a
  `{ capability, read : HostValue -> Bool }` record; a task request read and an
  event reducer carry their operation the same way; `Ui.each_str` owns one ops
  record containing its item/key/items capabilities plus `items_to_values`,
  `key_of`, `key_text`, and `row`. These records are carried by the edge that
  needs them, never invented by the host.
- **The capability is app-compiled, not host-authored.** The prebuilt host sees
  only the platform ABI; `a` is made concrete by the *application* (`Signal(a)`,
  `Model`, a row item type). The capability's closures are emitted by
  monomorphization when the app is built and handed to the host as ordinary
  `RocErasedCallable` values. The host stores and invokes them; it never inspects
  the layout they encapsulate.

**The split law.** `get`/`get_tagged` is *split-and-replace clone, never a
borrow*. The capability's app-compiled clone operation uses its private typed
split closure to turn the stored owned `Box(a)` into two independently owned
boxes: `keep` is written back into the source cell and `out` is stored as the
clone. A public get then consumes the clone. Dropping either box must not
invalidate the other, and any nested refcounted field (`Str`, `List`, record,
tag union, boxed closure) must end up independently owned in both. The host
relies on this law but never enforces it by inspecting bytes; correctness is the
capability's responsibility, expressed in typed Roc and lowered by the backend.

**Boundary discipline.** The host never walks a payload, never increments a
nested refcount, and never identifies a value by pointer shape. Every ownership
action — clone, compare, release — is a capability call. Typed aliasing
operations that share backing must retain that backing in typed Roc/compiler
lowering, not in the host. The active capability frame above is the host-side
guardrail: the host asserts that a value is only ever handed to the capability
that produced or owns its edge, which is what makes a routing bug a caught
contract violation rather than undefined behavior.

This is dictionary passing made concrete: a retained cell is morally an
existential `exists a. { value : Box(a), cap : clone/eq/drop closures for a }`.
The host holds the package without knowing `a`; Roc owns all type knowledge; the
capability bridges the two.

## App-Facing API

The app sees `Signal(a)`, typed keys, `Elem`, `Cmd(a)`, `Sub(a)`, and a small
set of polymorphic functions. It never sees ids, string keys, `NodeValue`,
lifecycle tokens, or subscription internals. The API is identical regardless of
which host runs the app — apps are written once and run under both the native
spec runner and the browser.

### Module surface

Signatures use Roc syntax: parenthesized type application (`Signal(a)`,
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
Signal.interval : U64 -> Signal(U64)  # period ms -> tick count
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
Html.attr : Str, Str -> Attr
Html.attr_s : Str, Signal(Str) -> Attr
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
Ui.each_str : Signal(List(item)), (item -> Str), (Str, Signal(item) -> Elem) -> Elem
    where [
        item.is_eq : item, item -> Bool,
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
todo_list : Signal(List(Todo)) -> Elem
todo_list = |todos|
    Html.ul(
        [],
        [
            Ui.each_str(
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
# Conceptual signal expression shape. Identity is carried via explicit
# per-edge tokens rather than dense list indices; the host interns those
# tokens into shared records at ingestion.
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
keyed row key/item slots are all carried as opaque `HostValue` handles, each
paired with the per-edge **capability** (clone/split, equality, drop) plus any
capability-owned extension record for the edge-specific operation — see *The
capability* under Confined Erasure. The conceptual `eq` fields above are the
equality member of that capability, not a free-floating thunk. A `HostValue` is
**not** a literal
`Box(OpaqueValue)` field in the heterogeneous descriptor tree; Roc cannot erase
`Box(a)` that way. The value is produced at the monomorphized edge and stored in
a host value cell; the descriptor carries only the opaque handle plus the
capability that owns it.

The platform does **not** evaluate the graph. It only describes it. There is no
`eval_signal`, no dirty propagation, no cache in Roc.

### 2. Entry point

There is exactly **one** Roc entrypoint. The host owns the mutable node table
and drives every event in-process, calling retained Roc closures directly. There
is deliberately no per-event Roc entrypoint and no `ui_recompute` round-trip.

```roc
# platform/main.roc
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

## The Engine: Host-Agnostic Reactive Core

The engine is the mutable reactive runtime, factored as `Engine(comptime Ctx)`.
It owns identity, ownership, dirtiness, scopes, the keyed diff, and the
structural splice/collect/apply algorithms. It calls the host through the `Ctx`
contract and writes all output through `sink()`. It never knows whether it is
running under the simulated DOM or the browser.

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
- **`Ui.each_str` carries a host-private key hash index.** The key text is
  load-bearing, not decorative: the host hashes it into a `HashMap` for each
  each site and uses typed key equality to resolve collisions and duplicate-key
  checks. Linear equality-only matching is a budget violation. Dropping the hash
  index is a regression to fix, not a host workaround to absorb.
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
4. For sink nodes whose value changed, emit the minimal render command (`SetText`,
   `SetValue`, `SetChecked`, `SetDisabled`, attribute set) through `sink()`.

Rank ordering guarantees a diamond (`a->b`, `a->c`, `(b,c)->d`) recomputes `d`
exactly once after both `b` and `c` settle — glitch freedom at runtime with no
re-sort. Value pruning is the second half of linear-with-changes scaling.

### Event routing

The host maintains a dense `event_id -> source_node_id` table built from
`BindClick`/`BindInput`/`BindCheck` sinks in the descriptor. When a DOM listener
fires (a simulated one on the native host, a real one in the browser), it looks
up the source node in O(1) and calls that source's retained reducer thunk
directly. No scan, no string lookup.

### Scopes and lifecycle

The host owns a forest of scopes. On a `Ui.when` flip or a `Ui.each` key-set
change:
- diff the new structure against the old (key-set diff for lists, branch flip for
  conditionals),
- mint a scope for new branches/keys (run that scope's `build` once, ingesting
  the sub-desc Roc returns for it),
- dispose scopes for removed branches/keys: remove their ids from the table and
  adjacency, call `decrefErasedCallable` on each retained closure (Roc reclaims
  captured environments), run any `Ui.on_cleanup` task, and detach the rendered
  subtree through `sink()`,
- reorder list rows by moving DOM nodes, never rebuilding surviving rows.

`keep_alive` is an explicit per-scope flag, never a heuristic.

**Leak invariant:** the host holds exactly one refcount per live retained
closure/value and zero for disposed ones. The graph is a DAG; the host's
back-references are not Roc-visible, so there are no refcount cycles.
Reclamation is deterministic, no GC.

### The render-command sink

The engine never touches a DOM directly. It writes to a `sink()` the host
supplies. The command set is the typed, host-independent vocabulary:
`ResetDom`, `CreateElement`, `CreateText`, `AppendChild`, `RemoveNode`,
`MoveBefore`, `SetText`, `SetValue`, `SetChecked`, `SetDisabled`, `SetRole`,
`SetLabel`, `SetTestId`, `SetClass`, pointer-event binds, timer/task commands,
and event bind/clear operations. The browser wire also has an `Extended` fixed
record whose operands point at a dynamic byte record for less common operations
such as arbitrary text attributes. The shared command vocabulary, command
counters, metrics accumulator, fixed-width command record, and dynamic-record
framing live in `src/render_commands.zig`. Each host implements the sink:

- the **native host** applies each command to its `DomElement` array, including
  a separate owned custom-attribute table for `Html.attr`/`Html.attr_s`;
- the **wasm host** serializes each command into a fixed-width record in linear
  memory for the JS executor to apply, with dynamic byte records for metadata
  attributes (`role`, `aria-label`, `data-testid`, `class`) and open-ended
  custom text attributes.

Because the logical command set is shared, a spec on the native host asserts the
same render semantics the browser will execute. The browser wire can choose a
compact fixed record or an `Extended` dynamic record without changing the engine
or native host semantics.

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
many events and watches the live `allocs − deallocs` gauge over time (see Measures
of Effectiveness); per-iteration deltas cannot establish it.

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

## Native Host Specifics

The native host is the engine plus a simulated DOM, a spec runner, and
telemetry. It is the place where we prove semantics and characterize work,
because it can observe things a real browser structurally cannot.

- **Simulated DOM.** A flat array of `DomElement` records (tag, role, label,
  test_id, text, value, checked, disabled, parent, children, bound events,
  per-field update counters). The native sink applies render commands to this
  array exactly as a browser host applies them to a real DOM.
- **Spec runner.** A semantic-locator spec parser (`role:`, `label:`, `text:`,
  `test_id:`, and `expect_*` / `click` / `fill` / `check`) that lets a spec
  assert UI behavior in user-facing terms and assert the exact work an event
  caused via `expect_metric_delta`. Spec actions (`click`, `fill`, `check`) fire
  the bound event id into the source node's retained reducer thunk.
- **Allocation ledger and telemetry.** The O(1) allocation ledger and the
  work counters above. This is the observability surface; it does not exist in
  the browser host.

These are native-specific and are **not** part of the browser host.

## Wasm Host and Browser Boundary

The wasm host is the engine plus the JS↔WASM boundary. The framing is **host
owns logical identity; JS owns DOM identity; they stay in lockstep by integer
ids**. The JS runtime is a thin executor of the engine's command stream — it
holds no reactive state, runs no diff, and never reconstructs meaning.

```
  Roc app (wasm)         Engine (Zig, in wasm)              JS runtime (browser)
  --------------         ---------------------              --------------------
  main : {} -> Elem      node table (mutable)               nodes[]   : Node[]
  pure descriptor   ──▶  scheduler / dirty set / scopes ──▶ listeners[]: Fn[]
  (roc_ui_init, once)    reducer + transform thunks         applyCmd(op, args...)
                         keyed each diff, ranks              forward event(id,payload)
  retained closures ◀──  host calls them in-process
  (no per-event FFI)     emits patch ops ─────────────────▶ exactly one DOM call per op
```

### Boundary contract

- **WASM exports a tiny integer-only control surface; JS never calls
  `roc_ui_init` directly.** JS asks the host to init; the Zig host calls
  `roc_ui_init` inside WASM, exactly as the native host does.
- **WASM owns logical identity; JS owns DOM identity, kept in lockstep by integer
  ids.** JS holds `nodes: (Node|null)[]`; the host holds dense node ids. DOM
  nodes never cross the boundary.
- **The crossing is a patch-op stream (host→JS) plus an event call (JS→host).**
  Not a serialized tree, not a pull-based inspection API, not a JS-side diff.

### Host C-ABI exports

```
roc_ui_mount() -> void          // host runs roc_ui_init, ingests, emits initial patch stream
roc_ui_event(event_id, payload_kind, payload_ptr, payload_len, bool_value) -> void
roc_ui_timer(token) -> void                 // drive interval/timer source
roc_ui_resolve(request_id, ptr, len, failed) -> void   // async result
roc_ui_unmount() -> void        // dispose all scopes, drop descriptor, free retained closures

roc_alloc / roc_dealloc / roc_realloc        // marshalling
memory                                       // exported linear memory

roc_ui_protocol_version() -> u32
roc_ui_protocol_features() -> u32
roc_ui_command_record_words() -> usize
roc_ui_command_buffer_ptr() -> usize
roc_ui_command_buffer_len() -> usize
roc_ui_string_buffer_ptr() -> usize
roc_ui_string_buffer_len() -> usize
roc_ui_dynamic_buffer_ptr() -> usize
roc_ui_dynamic_buffer_len() -> usize
roc_ui_last_error_ptr() -> usize
roc_ui_last_error_len() -> usize
roc_ui_live_host_values() -> usize
```

The host drives the engine entirely inside WASM. `roc_ui_event` enters the Zig
host, routes the event id to its source node, and calls the retained reducer
thunk via `RocErasedCallable` in-process. There is no per-event Roc entrypoint
crossing — this is the reason the boundary is cheap.

### Command-buffer wire format

The browser wire is versioned. JS reads `roc_ui_protocol_version()` and
`roc_ui_protocol_features()` before mounting and requires version `1` with the
`dynamic_attrs` and `dynamic_events` feature bits. A version or feature mismatch
is a boundary error, not a compatibility shim.

The host appends fixed-width records to `roc_ui_command_buffer_*`: six little
endian `u32` words (`op`, then five integer operands). Hot operations fit
entirely in those operands. Free-form text for hot string ops (`CreateElement`,
`CreateText`, `SetText`, `SetValue`, task name/request) is stored in
`roc_ui_string_buffer_*`, and fixed records carry `(offset, len)` slices into
that buffer. JS never decodes a `RocStr` header, tag union, list layout, or Roc
payload to infer meaning.

Less common variable-shape commands use fixed op `Extended`. Its operands are:

```text
record.op = Extended
record.a  = byte offset in roc_ui_dynamic_buffer_*
record.b  = byte length of this dynamic record
```

Each dynamic record is self-framed:

```text
u16 dynamic_op
u16 flags       # currently 0
u32 payload_len
payload bytes
zero padding to 4-byte alignment
```

Version 1 defines two dynamic attribute ops and two dynamic event ops:

```text
SetAttrText:
  u32 elem_id
  u32 name_len
  name bytes
  u32 value_len
  value bytes

RemoveAttr:
  u32 elem_id
  u32 name_len
  name bytes

BindEvent:
  u32 elem_id
  u32 event_id
  u32 event_name_len
  event_name bytes
  u32 listener_options
  u32 payload_kind
  u32 payload_descriptor_len
  payload_descriptor bytes

ClearEvent:
  u32 elem_id
  u32 event_name_len
  event_name bytes
```

Strings in dynamic records are UTF-8 byte slices. The runtime validates the
header, flags, aligned outer length, payload consumption, operand bounds, and
UTF-8 before touching the DOM. Unknown dynamic ops and malformed records are
reported as contract errors. This keeps JS a decoder/executor for explicit data
the host emitted; it does not reconstruct missing render intent.

Current wasm-host emission uses dynamic records for metadata text attributes
(`role`, `aria-label`, `data-testid`, and `class`) and for app-authored custom
text attributes from `Html.attr` / `Html.attr_s`. The Roc descriptor makes the
custom path explicit with `Node.field_custom` plus a `name` field on text attrs;
fixed text fields must carry an empty name. `SetText`, `SetValue`, bool fields,
fixed click/input/check/pointer event binds, timers, and tasks remain fixed
records. General named events use `BindEvent`/`ClearEvent`, carrying the event
name, static listener option bits, payload kind, and payload descriptor emitted
by the host from Roc descriptors.

Dynamic event payload descriptors are independent of Roc value layout. They are
small byte descriptors that name only event/target/currentTarget leaves JS may
read. Version 1 supports unit payloads, scalar text/bool payloads, and explicit
records. The current record descriptor used by `Html.on_key_down` asks JS to
read `event.key` and `event.shiftKey`; JS encodes `{ key, shift_key }` as:

```text
u32 key_utf8_len
key UTF-8 bytes
u8 shift_key   # 0 or 1
```

The host receives those bytes as a `List(U8)` `HostValue`, and the app-facing
`Ui.State.on_key` decoder constructs the typed Roc record. JS never decodes Roc
records, tag unions, list headers, or string layouts. Unsupported payload kind /
accessor pairs, malformed descriptors, duplicate record fields, trailing bytes,
and invalid listener option bits are host/runtime contract errors.

### Marshalling and memory discipline

Any `roc_alloc` during a host call can grow linear memory and detach JS
typed-array views. The rule is **rebuild cached views after every allocating
host call, before reading any command buffer or string/payload bytes** — JS
compares `memory.buffer` identity and rebuilds `Uint8Array`/`Int32Array`/
`DataView` only when it changed. No host-bumped memory-generation export is
required.

### Controlled inputs

`SetValue` is a guarded op, not a blind assignment. Equal values are no-ops;
differing values are deferred while the target input is focused or composing
(IME); the latest deferred value is applied after blur unless a later input echo
already matched it. Focused masking/validation and selection-preserving
normalization are a known future input-reconciliation concern (see Open
Questions), kept out of the thin executor.

### Refcount ownership split

- The host holds exactly one refcount per live retained closure/value (the Leak
  invariant above). JS never owns Roc refcounts; JS holds DOM nodes and integer
  ids only. On `RemoveNode`, JS detaches the DOM node and clears `nodes[id]`; the
  refcount drop happens inside the host's scope-dispose path. That drop releases
  the value through its per-edge **capability** (see Confined Erasure), never by
  the host walking the payload layout: the prebuilt host cannot know how to free
  the nested fields of an app-typed `Box(a)`, so releasing it is a capability
  call.
- String buffers JS receives are borrowed for the drain; the host owns and frees
  them. Buffers JS produces for event payloads are `roc_alloc`'d by JS and
  ownership transfers to the host on `roc_ui_event`.

### Async in the browser

Effects are sources. Timers/`Signal.interval` are ingested at init; JS runs the
real `setInterval(period_ms)` keyed by `token` and calls `roc_ui_timer(token)`
each tick. Tasks/`fetch` declare a request; the host assigns a `request_id`, JS
performs the `fetch`, and on settle calls `roc_ui_resolve(request_id, ptr, len,
ok)`, which the host folds into `[Loading, Done, Failed]`. Disposing a scope
cancels in-flight requests (host-emitted cancel → JS `AbortController` /
`clearInterval`) and runs `Ui.on_cleanup`. All of it enters the one propagation
queue; JS scheduling stays a single synchronous path.

### What is worth testing on the JS side

The JS runtime is a thin executor, so engine semantics and work budgets are
proven by the native spec runner, **never re-tested through JS**. The only
JS-side behavior worth an automated guard is the **JS↔WASM contract** itself:
the cmd/patch codec, protocol version/feature negotiation, dynamic-record
validation, event-payload marshalling, telemetry byte accounting, and the
`memory.grow` view-refresh discipline. A JS test that re-asserts "clicking
changes the count" or "one patch per event" is duplicating the engine's own
coverage across the boundary and pulls no additional weight.

## Measures of Effectiveness

These are the outcomes by which we judge whether the platform meets its intended
goals. Each is a property we can observe and that should hold for the life of the
platform; each is backed by a spec, host test, or measurement that fails if the
property regresses.

1. **One engine, two thin hosts.** All reactive and structural logic lives in the
   shared engine. Neither host file contains reactive or structural logic; each
   is a `Ctx` + `sink()` implementation plus its boundary. *We know this holds
   when:* the hosts cannot drift apart, because there is only one implementation
   of behaviour to drift from, and the same engine instantiates under both the
   native build and `wasm32`.

2. **Same apps, both environments.** The app suite is written once in Roc and runs
   under the native spec runner and in the browser. *We know this holds when:*
   every app in the suite builds and runs in both, with `serve.py` able to build
   and serve any app, not just one.

3. **Semantics proven where we can observe them.** The native spec runner asserts
   behaviour and work budgets — the observability a real browser structurally
   cannot give us. The JS runtime is thin enough that the only automated JS test
   is the JS↔WASM codec contract. *We know this holds when:* engine semantics are
   covered by native specs only, and JS coverage is the codec/boundary contract
   only.

4. **Work scales with change, not tree size.** Per event, nodes recomputed,
   patches emitted, and rows touched track the *changed* set — including under
   list churn — never graph or tree size, with no full-tree re-walk, no full
   graph rebuild, and no scan-to-rediscover-identity. *We know this holds when:*
   `expect_metric_delta` assertions over `nodes_recomputed`, `patches_emitted`,
   `active_graph_records_rebuilt`, `stream_nodes_scanned`, `each_key_compares`,
   and the row counters bound work to the changed set, and per-event allocations
   are flat across input size.

5. **No leaks; reclamation is deterministic.** The host holds exactly one
   refcount per live retained closure/value and zero for disposed ones. *We know
   this holds when:* `closure_retains == closure_releases` after teardown, the
   live `allocs − deallocs` gauge is flat after warmup across a long session, and
   carrier type-tag assertions never fire across the full safe-build spec suite.

6. **Determinism.** The same spec produces the same command sequence every run.

7. **Confined erasure cannot crash.** A typed `Signal(a)` stays typed end to end;
   the displaced wiring invariant is checked by debug/safe-build carrier tags that
   compile out of release. *We know this holds when:* the safe-build spec suite
   runs clean with tags asserted, and the tags are absent from release builds.

## Proving Breadth and Depth: the App Suite

The representative apps are not demos; each exists to make one capability fail
loudly if it regresses. The bar for adding an app is **"it exercises an
otherwise-unproven capability,"** never size or visual richness. The suite:

- `ops_dashboard` — scalar chains, fanout, conditional branch, keyed alert rows,
  input state.
- `checkout_wizard` — keyed wizard steps, form/checkbox state, disabled actions,
  list replacement, per-row state.
- `event_payload_boundary` — app-authored `href`/`aria-*`/`data-*`/`id`/
  `placeholder` attrs, signal-backed custom attrs, keyboard payload bytes, and
  submit with static prevent-default policy.
- `kanban_board` — keyed reorder/archive/reset, per-card state, `map2` filtering.
- `identity_stress` — `when -> each -> when` row-local state through
  reorder/insert/filter/disposal.
- `component_composition` — reusable stateful `Ui.component` instances keep local
  state across keyed row movement and dispose with the owning row scope.
- `async_effects` — fake task result injection, `[Loading, Done, Failed]`
  rendering through a fold, `Ui.on_change` request issuance, pending-request
  cancellation, deterministic interval ticks, interval cancellation, and cleanup
  execution.

Host tests cover topological rank ordering, diamond deduplication, confined
erasure through carrier tags, retained closure lifecycle accounting, dirty cache
pruning, and local structural splicing.

Keep each app minimal: the smallest structure that exercises the capability and
the tightest `expect_metric_delta` assertions that prove the scaling property.
Avoid catalog-style fixtures and avoid re-proving already-green identity
behavior.

**Foundation coverage the suite must carry.** Proving behavior is not enough; the
suite must also assert *work*, so a regression to O(N) work fails the build rather
than passing silently:

- A **generated large-N `Ui.each` app** (the scaling fixture). N is a build
  parameter; the rows are generated programmatically, not handwritten. It is the
  one place where large N is allowed, precisely because it is systematic rather
  than a catalog. Its specs assert the budget for single-row update, append,
  remove, filter, and reorder — including the `active_graph_records_rebuilt`,
  `stream_nodes_scanned`, `each_key_compares`, and per-event allocation counters.
- **Work assertions on the structural apps.** `kanban_board` reorder/filter
  steps and the `async_effects` cancel cycle carry `expect_metric_delta` blocks
  that bound work and prove no retained-closure or allocation leak across the
  async open/close cycle.
- **Real-event fanout assertions in `ops_dashboard`** bounding `nodes_recomputed`
  / `derived_calls_into_roc` so diamond/shared-signal amplification is pinned on
  the live path.
- **A reorder host test at large N** that fails if reorder degrades from
  moves-only to whole-site re-collect.

## Open Questions

These are genuine unknowns that require inspecting compiler behavior, generated
ABI, layout rules, or browser constraints.

- **Controlled inputs / focus / IME / selection.** The guarded `SetValue` rule
  (equal = no-op, differing deferred while focused/composing) is enough for apps
  without focused text editing, but full focused masking, validation, and
  selection-preserving normalization may require a first-class
  input-reconciliation primitive rather than an executor rule.
- **Animation / high-frequency continuous values.** A push graph driven by
  discrete updates may need a dedicated `interval`-driven path for smooth
  animation; whether a rAF-coalescing layer buys anything is a measurement.
- **Multiple instances per page.** Host state is module-global; two mounts on one
  page need either two WASM instances or an explicit per-mount handle.
- **Recompute granularity.** Whether any future batching of in-host recompute
  buys anything is a measurement, not a fixed decision.
- **Native vs. browser render-surface parity.** Whether the native spec runner
  should consume the same command-buffer wire format the browser does, to keep a
  single render surface rather than two emit paths behind one command enum.
