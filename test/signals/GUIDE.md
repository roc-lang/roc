# Signals Guide for Web Developers

This guide introduces the signals platform as a future browser UI platform. The
host in this directory is a simulated DOM and test runner, but the programming
model is the same shape a browser implementation would use:

1. A Roc app builds a pure description of its UI: signals, structure, and event
   handlers.
2. The platform hands that description to the host **once**.
3. The host mints node ids, builds the dependency graph, applies the initial
   render patches, and binds event listeners.
4. A browser event fires; the host routes it to the right source node and asks
   Roc to apply that source's reducer.
5. The host propagates the change through the dependency graph, re-invoking
   **only** the derived computations whose inputs actually changed, and emits
   the minimal render patches.

If you know Roc and browser fundamentals, the main shift is this: the DOM is not
where application state lives, and the UI is **not** rebuilt on every event. Roc
describes a reactive graph once; the host owns that graph, owns the retained
state, and updates only what changed. Work scales with the number of values that
change, not with the size of your UI.

## The Mental Model

A **signal** is a continuous, always-present value. A `Signal(Str)` always has a
current string; a `Signal(Bool)` always has a current boolean. Signals are what
you read while describing the UI.

Signals come in two flavors, but you build both with the same small toolkit:

- **Source signals** are set from the outside: a text input, a checkbox, a
  button-driven counter, a timer, or an effect result. Local source state is
  introduced with the `Ui.state` closure binder, which gives the body a typed
  state handle for reading and updating it.
- **Derived signals** are computed from other signals with `Signal.map`,
  `Signal.map2`, or `Signal.combine`. You never update a derived signal
  directly; it recomputes when an input changes.

When you write `Signal.map2(price, qty, |p, q| p * q)`, you are not just
computing a value — you are *declaring a dependency*. The edges
`price -> total` and `qty -> total` are data in the description. The host reads
those edges directly, so it never has to run your code to discover what depends
on what. This is what lets the platform update incrementally without any
compiler magic and without scanning the graph at runtime.

An `Elem` is a pure description of UI structure. Static parts are plain values;
dynamic parts (text, input values, checked/disabled states) reference signals;
event handlers reference reducers. Conditional regions and lists are explicit
constructs so the host can manage their identity and lifecycle.

## Why Use This Model?

The reason to consider signals is not familiarity. It is that UI behavior
becomes explicit, testable, and cheap to update.

- **One owner for app state.** Retained state lives in the host's reactive
  runtime. The host applies render patches and routes events. State is not split
  between Roc, JavaScript, DOM properties, and framework internals.
- **Derived data stays derived.** Labels, disabled states, filtered lists, and
  review text are signals computed from other signals. You represent them once
  and never resynchronize duplicates by hand.
- **Updates are incremental by construction.** Because dependencies are declared
  in your `map`/`map2`/`combine` calls, the host updates exactly the affected
  nodes. A change that produces an equal value stops propagating immediately.
- **Roc types cover the whole UI model.** Form state, list items, and derived
  values are ordinary typed Roc values. There is no untyped escape hatch.
- **Tests speak in user-facing terms.** The host replays browser-style specs
  against roles, labels, text, values, and checked states. A browser
  implementation keeps the same testing model.
- **Rendering is a data boundary.** Roc describes; the host applies patches such
  as `SetText`, `SetValue`, and `BindClick`. The same description targets the
  test host and a future browser host.

This model pays off most for apps with rich state: forms, wizards, dashboards,
editors, lists with local row state, and filtered/reordered views. It is less
compelling for mostly static pages or projects whose value comes from an
existing JavaScript component ecosystem.

## A Small Browser-Style Form

A signals app imports the platform modules and builds an `Elem`:

```roc
app [main] { pf: platform "../platform/main.roc" }

import pf.Signal exposing [Signal]
import pf.Html
import pf.Ui exposing [Elem]
```

Here is a small form with retained input state, derived text, a click counter,
and a disabled action:

```roc
main : {} -> Elem
main = |_|
    Ui.state("", |name_state| {
        name = name_state.signal

        greeting : Signal(Str)
        greeting =
            Signal.map(
                name,
                |value| if value == "" { "Enter your name" } else { "Hello, ${value}" },
            )

        submit_disabled : Signal(Bool)
        submit_disabled = Signal.map(name, |value| value == "")

        Ui.state(0i64, |submit_state| {
            submit_count = submit_state.signal

            submit_label : Signal(Str)
            submit_label =
                Signal.map(submit_count, |count| "Submissions: ${count.to_str()}")

            Html.div(
                [],
                [
                    Html.heading("Profile"),
                    Html.input(
                        [Html.label("Name"), Html.value(name), Html.on_input(|new| name_state.on_value(|_, _payload| new))],
                        [],
                    ),
                    Html.text_s(greeting),
                    Html.button(
                        [Html.label_s(Signal.const("Save profile")), Html.disabled(submit_disabled), Html.on_click(submit_state.on_unit(|n| n + 1))],
                        [],
                    ),
                    Html.text_s(submit_label),
                ],
            )
        })
    })
```

The important pieces:

`Ui.state("", |name_state| ...)` creates a source signal with an initial value
and a scoped state handle. `name_state.on_value(|_, _payload| new)` builds the
message that replaces the current value with the latest input. You pass
`name_state.signal` itself to `Html.value` so the input stays bound to the
source.

`Signal.map(name, ...)` derives display text. There is no key; the host knows
this derived node's identity from where you built it. When `name` changes, the
host recomputes `greeting` and `submit_disabled` — and nothing else.

`Ui.state(0i64, ...)` plus `submit_state.on_unit(|n| n + 1)` is a counter.
Clicks do not mutate a variable; the click's message describes how to produce
the next value from the current one.

## How Browser Events Flow

For a real browser host, the initial load looks like this:

1. Call `roc_ui_init`. Roc runs `main({})` once and returns the descriptor tree
   (an `Elem` with embedded signal edges and retained closures).
2. The host ingests the descriptor: it mints a dense integer id per node, builds
   the dependency adjacency and a topological ordering, stores each node's
   retained transform closure, computes the initial values by calling those
   closures in dependency order, and produces the initial render patches.
3. The host applies the initial patches to the DOM and binds listeners from
   `BindClick`, `BindInput`, and `BindCheck`.

When a user types in the `Name` input:

1. The bound DOM listener fires. The host looks up the input's source node id in
   O(1) and calls that source's retained reducer closure directly with the typed
   value `"Ada"`. There is no per-event call back into a Roc entrypoint; after
   the one-time `roc_ui_init`, the host drives every event in-process by invoking
   the retained closures it already holds.
2. The reducer produces the new source value (and any effect requests).
3. The host marks that source dirty and walks its dependents **in dependency
   order**, invoking only the retained transform closures whose inputs changed.
   Where a recomputed value equals its previous value (`is_eq`), propagation
   stops there.
4. The host emits the minimal render patches for the sinks whose value changed.

The host never stores Roc callbacks inside DOM nodes, never evaluates signals
itself, and never scans the graph to guess what changed. It calls the retained
closures it holds and applies explicit patches from the runtime.

The patch command set:

- `ResetDom`
- `CreateElement` and `AppendChild`
- `SetText`, `SetValue`, `SetChecked`, and `SetDisabled`
- `SetRole`, `SetLabel`, and `SetTestId`
- `BindClick`, `BindInput`, and `BindCheck`

The simulated host applies these against an in-memory element tree; a browser
host maps the same commands onto real DOM operations.

## One Call Into Roc at Startup, Direct Calls After

A natural question: if the host calls `roc_ui_init` only once, how does Roc keep
running your code — your button handlers, your derived values, and the new rows a
growing list needs?

The answer is that `roc_ui_init` does not just return *data*; it returns your
*functions*, packaged inside the description. Every handler you write
(`|n| n + 1`), every `Signal.map` transform, and every `Ui.each` row builder
(`|key, row| ...`) is a real Roc function. They all travel to the host once,
inside the `Elem`, as retained closures — the host keeps a direct pointer to each
compiled function plus the values it captured.

After that, the host runs your Roc code by **calling those pointers directly**,
not by re-entering a Roc entry point:

- **A click** runs your reducer function directly to produce the next state
  value.
- **A changed signal** runs your `map`/`map2`/`combine` functions directly to
  recompute derived values.
- **A new list row or a flipped branch** runs your row builder or branch function
  directly to produce a fresh piece of UI structure.

That last point is the one that surprises people: even when Roc must *create new
UI* at runtime (a new row appears in a list), it does not need a new entry point.
The host already holds your row-builder function from init; it just calls it with
the new row's data and gets back the new `Elem`. The only difference from a click
handler is that this function returns *UI* instead of a *value*.

This is the heart of the performance story. There is exactly one boundary
crossing into Roc — `roc_ui_init`, at startup. Everything after it is an ordinary
in-process function call into code that is already compiled and already in memory,
with its captured data riding along. Nothing is marshaled across a generic FFI
door per event, and the host never asks Roc "what should I do?" — it already holds
the exact function for the job.

## Identity Without Keys

Signals apps build their description once. State must survive across updates, but
you do not manage identity with strings.

- **Plain signals** get their identity from where you construct them. Because the
  build is pure and deterministic, that position is stable.
- **Conditional regions and list rows are explicit scopes.** Adding or removing
  UI inside one scope never disturbs the identity of sibling scopes. This is why
  branches and lists are first-class: they are the seams that contain change.
- **List rows are identified by a typed key**, not by position. A row keeps its
  local state when the list is reordered or filtered, because identity is the
  key.

So instead of writing keys, you write structure. Use `Ui.when` for conditionals
and `Ui.each` for lists, and let the host manage identity and lifecycle.

## Dynamic UI

Use `Ui.when` for a boolean choice between two subtrees:

```roc
Ui.when(
    incident_active,
    |_| Html.paragraph("Incident room open"),
    |_| Html.paragraph("No active incident"),
)
```

Each branch is its own scope. When the condition flips, the host disposes the
losing branch (releasing its state and detaching its DOM) and mounts the other.

Use `Ui.each` for lists. You supply a typed key function and a row renderer. The
item type provides `is_eq`; key types wrap a single-tag union and provide `hash`
and `is_eq` methods through their method block:

```roc
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

The row renderer receives the row's key and a `Signal(item)` for that row's data.
Any `Ui.state` binder you create inside the renderer is local to that row scope,
so it survives reorder and filter. The key function must
return a stable identity from the data (a database id, slug, or durable client
id) — never the item's current position. Two rows that produce the same key are
reported as a host error rather than silently aliased.

When the list changes, the host diffs the new key set against the old: surviving
rows are reused (including their local state), new keys mint a fresh row scope,
and removed keys are disposed. The item `is_eq` method tells the host whether a
surviving row's value actually changed. Only changed rows touch the DOM.

## Typed Values

App code works with ordinary typed Roc values throughout. Signals are typed
(`Signal(Str)`, `Signal(List(Todo))`), and the transforms you pass to `map`,
`map2`, and `combine` are ordinary typed functions. There is no untyped value
representation and no hand-written conversion boilerplate to write.

For a custom type to be used as a signal value or a list key, it must define the
methods those positions require. The platform resolves these by static dispatch
on the value's type (there is no auto-derivation, so you define the methods on
the type):

- Any signal value type defines `is_eq : t, t -> Bool`. The runtime uses it to
  stop propagation when a recomputed value is unchanged.
- Any `Ui.each` key type defines both `hash : t, Hasher -> Hasher` and
  `is_eq : t, t -> Bool`.
- Any `Ui.each` item type also defines `is_eq : t, t -> Bool`, so keyed row
  reuse and row value changes are separate explicit facts.

```roc
Todo := { id : TodoId, title : Str, done : Bool }.{
    is_eq : Todo, Todo -> Bool
    is_eq = |a, b| a.id.is_eq(b.id) and a.title == b.title and a.done == b.done
}

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
```

You define the comparison (and, where a value must serialize, `encode`/`decode`)
methods on the type. The platform captures the type-specific equality/hash
thunks at each edge or key site; the host invokes those thunks rather than
choosing a decoder or comparing erased bytes. Built-in types such as `Str`,
`Bool`, and `I64` already provide what the common cases need.

## Choosing The Right Primitive

- Use `Signal.const` for a value that never changes.
- Use `Ui.state` for local state the user updates: input text, checkbox state,
  counters, toggles, wizard steps, replaced lists. The state handle's reducer
  (`|current| next`) describes the transition.
- Use `Signal.map` for display-only derived data from one signal, `Signal.map2`
  for two, and `Signal.combine` for a list of signals.
- Use `Html.text` for static text and `Html.text_s` for signal-backed text.
- Use `Html.value`, `Html.checked`, and `Html.disabled` for signal-backed
  attributes; `Html.on_click`, `Html.on_input`, and `Html.on_check` for handlers.
- Use `Ui.when` for conditional regions and `Ui.each` for lists. Use
  `Ui.component` to introduce a named scope when a reusable piece of UI needs its
  own local state.

## Real-World Effects

> Status: effects are implemented in the test host with deterministic fake tasks
> and timer ticks. The current app-facing helpers are intentionally small:
> `Signal.fake_task`, `Signal.from_task`, `Signal.fold_task`,
> `Signal.start_str`, `Signal.interval`, `Ui.on_change`, and
> `Ui.on_cleanup`.

Effects (HTTP, timers, storage, navigation) follow the same ownership rule as
rendering: Roc describes what should happen; the host performs it; results
re-enter the graph as source updates, using the exact same propagation path as a
click. A signal is never evaluated by running an effect, and an effect never
mutates the DOM outside the patch stream.

Effects are modeled as **sources**:

```roc
save_state : Signal([NotAsked, Saving, Saved, Failed(Str)])
save_state =
    Signal.from_task(
        save_request,
        |result| match result {
            Ok(_) => Saved
            Err(message) => Failed(message)
        },
    )
```

`Signal.from_task` produces a signal whose value reflects the lifecycle of the
work (`Loading`, then `Done` or `Failed`). When the task resolves, the host sets
that source node and runs the same dirty-propagation a user event would. Error
states are ordinary signal values you fold and render.

Related helpers:

- `Signal.interval(period_ms)` is a timer source. In the test host it uses `U64`
  milliseconds and yields a `U64` tick count.
- `Ui.on_change(signal, |value| cmd)` fires an effect request when a signal
  changes, and `Ui.on_cleanup(cleanup)` runs when the surrounding scope is
  disposed.

Request identity comes from the owning scope and node, not from app-written
strings, so re-evaluation never refires a request, and disposing a scope cancels
its in-flight work. A test host can assert that an app requested an HTTP call and
then inject a fake result without running a network stack.

## Common Mistakes

- **Do not treat a signal like a mutable variable.** You do not assign to a
  signal. You create local source signals with `Ui.state` and describe
  transitions through its reducer messages; derived signals follow from
  `map`/`map2`/`combine`.
- **Do not let the host own app state.** The host owns the reactive runtime, but
  the state belongs to the platform's runtime, not to a second model in
  JavaScript.
- **Do not use unstable list keys.** A row's key must come from item identity,
  not from list position, or its local state will follow the wrong item.
- **Do not push work into the host's lap.** If the host needs to update
  something, the runtime emits a patch. If Roc needs to know something happened,
  the host sends an explicit event. Nothing is recovered by guessing.

## Where To Look Next

The maintained examples in `apps/` show larger versions of these patterns:

- `ops_dashboard.roc` shows scalar signal chains, fanout, conditional regions,
  keyed alert rows, and input state.
- `checkout_wizard.roc` shows conditional wizard steps, form state, disabled
  actions, list replacement, and per-row local state.
- `kanban_board.roc` shows list reorder, archive/reset flows, filtering through
  `Signal.map2`, and per-card local state.
- `identity_stress.roc` is the Phase 2 identity harness for
  `when -> each -> when` row-local state through reorder.

Run the representative suite from the repository root:

```sh
zig build run-test-signals
```

That builds each app and replays its browser-style interaction spec against the
simulated DOM host. For the architecture and the host boundary in detail, see
`DESIGN.md` in this directory.
