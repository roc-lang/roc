# Signals Guide for Web Developers

This guide introduces the signals platform as a future browser UI platform.
The current host in this directory is a simulated DOM and test runner, but the
programming model is the same shape a browser implementation would use:

1. A Roc app returns a pure `Elem`.
2. The Roc runtime evaluates signals and emits DOM commands.
3. The host applies those commands to the DOM.
4. Browser events are sent back to Roc through event ids.
5. Roc returns the next boxed runtime plus another command batch.

If you know Roc and browser fundamentals, the main shift is this: the DOM is
not where application state lives. Roc owns the reactive graph and retained
state. The browser host owns DOM nodes, event listeners, and command
application.

## The Mental Model

Signals are continuous values. A `Reactive.Signal(Str)` always has a current
string value, and a `Reactive.Signal(Bool)` always has a current boolean value.
Signals are what you read while rendering.

Events are discrete occurrences. A `Reactive.Event(Str)` may occur when a user
types into an input. A `Reactive.Event(Reactive.Unit)` may occur when a user
clicks a button. Events are what change retained state.

Event senders are write-only handles used by controls. You pass an
`EventSender(a)` to `Elem.text_input`, `Elem.checkbox`, or `Elem.action_button`.
The host binds that sender to a DOM event id. When the browser event fires, the
host sends the event id and value back to Roc.

State is created by turning events into signals:

- `Signal.hold(key, initial, event)` stores the latest event value.
- `Signal.fold(key, initial, event, step)` stores accumulated state.
- Specialized forms such as `fold_i64` and `fold_bool_toggle` avoid erased
  value work for common cases.

Derived values are ordinary signal graph nodes:

- `Signal.const_*` creates values that never change.
- `Signal.map` derives one signal from another.
- `Signal.map2` derives one signal from two inputs.
- Keyed variants such as `map_keyed` and `map2_keyed` let the runtime retain
  cached results across rebuilt graph descriptions.

An `Elem` is a pure description of UI. Some elements contain signals for their
text, values, checked states, or disabled states. Dynamic elements render a
child tree from the current value of a signal.

## Why Use This Model?

Signals can look intimidating if you are used to component state, effect hooks,
reducers, or manually mutating DOM state. The reason to consider this model is
not that it is familiar. The reason is that it tries to make UI behavior more
explicit, easier to test, and easier for the runtime to update predictably.

The proposed benefits are:

- One owner for app state. Roc owns retained state inside the runtime, and the
  host applies commands. That avoids splitting the same state between Roc,
  JavaScript, DOM properties, and framework internals.
- Explicit event flow. User actions enter through named event channels, and
  state changes happen through `hold` and `fold`. There is no hidden callback
  graph to reconstruct when debugging an update.
- Derived data stays derived. Values such as labels, disabled states, filtered
  lists, and review text are signals computed from other signals. You do not
  store them separately and remember to keep them in sync.
- Tests can speak in user-facing terms. The current host already replays
  browser-style specs against roles, labels, text, values, and checked states.
  A browser implementation can keep that testing model instead of depending on
  component internals.
- Rendering is a data boundary. Roc emits commands such as `SetText`,
  `SetValue`, and `BindClick`; the host applies them. That gives a clear line
  between application logic and DOM integration.
- Incremental work is driven by explicit dependencies. Signals carry dependency
  data, keyed nodes identify retained work, and the runtime can skip clean
  cached signals or emit only changed non-structural DOM commands.
- Roc types cover more of the UI model. Form state, events, list items, and
  derived values are ordinary typed Roc values rather than untyped JavaScript
  objects passed through framework conventions.
- The same app model can target more than one host. The current host is a test
  runner, while a future host could be a browser DOM implementation. The app
  code still describes events, signals, and elements.

This model is most likely to pay off for apps with rich state: forms, wizards,
dashboards, editors, lists with local row state, filtered/reordered views, and
flows where correctness matters more than quick DOM scripting. It is less
compelling for mostly static pages, small marketing sites, or projects whose
main value comes from an existing JavaScript component ecosystem.

You can tell whether it is worth learning by building a small but real slice of
your product and checking the result against a few concrete questions:

- Can a new developer follow the event channels and signal definitions without
  chasing hidden update paths?
- Are derived values represented once, or are you still manually synchronizing
  duplicate state?
- Do keyed dynamic sections and lists preserve the state users expect when
  branches switch, items reorder, or filters change?
- Can important flows be tested through roles, labels, text, and form values
  instead of framework internals?
- Does the host boundary stay boring: apply commands, bind events, dispatch
  events back to Roc?
- Do runtime counters and command counts show that common interactions avoid
  unnecessary recomputation and DOM updates?
- Is the extra upfront vocabulary smaller than the bugs and complexity it
  removes from the UI you are actually building?

Switching is not automatically worth it. The model asks developers to learn a
different vocabulary and to make state identity explicit with keys. The payoff
should be visible in simpler state reasoning, stronger tests, fewer duplicated
state updates, and predictable host integration. If a prototype does not show
those wins for your app shape, it is reasonable to keep using a more familiar
web framework.

## A Small Browser-Style Form

A signals app imports the platform modules and exposes `main : {} -> Elem.Elem`.
The current research platform uses this header:

```roc
app [main] { pf: platform "../platform/main.roc" }

import pf.Elem
import pf.NodeValue exposing [NodeValue]
import pf.Reactive
```

Here is a small form with retained input state, derived text, a click counter,
and a disabled action:

```roc
main : {} -> Elem.Elem
main = |_| {
    { sender: name_send, receiver: name_changes } =
        Reactive.Event.channel("name_change")

    name : Reactive.Signal(Str)
    name =
        Reactive.Signal.hold("name", "", name_changes)

    greeting : Reactive.Signal(Str)
    greeting =
        Reactive.Signal.map_keyed(
            "greeting",
            name,
            |value| if value == "" {
                "Enter your name"
            } else {
                Str.concat("Hello, ", value)
            },
        )

    submit_disabled : Reactive.Signal(Bool)
    submit_disabled =
        Reactive.Signal.map_keyed(
            "submit_disabled",
            name,
            |value| value == "",
        )

    { sender: submit_send, receiver: submit_clicks } =
        Reactive.Event.unit_channel("submit_click")

    submit_deltas : Reactive.Event(I64)
    submit_deltas =
        Reactive.Event.map_unit_i64_const(submit_clicks, 1)

    submit_count : Reactive.Signal(I64)
    submit_count =
        Reactive.Signal.fold_i64(
            "submit_count",
            0,
            submit_deltas,
            |current, delta| current + delta,
        )

    submit_label : Reactive.Signal(Str)
    submit_label =
        Reactive.Signal.map_i64_str_keyed(
            "submit_label",
            submit_count,
            |count| Str.concat("Submissions: ", count.to_str()),
        )

    Elem.div(
        [
            Elem.heading("Profile"),
            Elem.text_input(
                {
                    label: "Name",
                    value: name,
                    on_input: name_send,
                    disabled: Reactive.Signal.const_bool(False),
                },
            ),
            Elem.label(greeting),
            Elem.action_button(
                {
                    on_click: submit_send,
                    label: Reactive.Signal.const_str("Save profile"),
                    disabled: submit_disabled,
                },
            ),
            Elem.label(submit_label),
        ],
    )
}
```

The important pieces are the channels and keys.

`Reactive.Event.channel("name_change")` creates an input channel. The
`name_send` value is passed to the text input. The `name_changes` value is the
stream of submitted strings.

`Reactive.Signal.hold("name", "", name_changes)` creates retained state. Before
any input, the signal value is `""`. After the user types, the latest input
value becomes the current signal value.

`Reactive.Signal.map_keyed("greeting", name, ...)` derives display text from
the current input. The key tells the runtime this derived node is the same
logical node when `main` returns a fresh `Elem` tree after an event.

`Reactive.Signal.fold_i64("submit_count", 0, submit_deltas, ...)` accumulates a
counter. Click events do not directly mutate a variable. Instead, the event
stream carries deltas, and the fold describes how to produce the next value.

## How Browser Events Flow

For a real browser host, the initial load would look like this:

1. Call `roc_ui_init`.
2. Store the returned boxed runtime.
3. Apply the returned command batch to the DOM.
4. Install listeners from commands such as `BindClick`, `BindInput`, and
   `BindCheck`.

When a user types in the `Name` input, the browser host uses the bound event id
to create a Roc host event:

```text
Input({ event: name_event_id, value: "Ada" })
```

Then it:

1. Passes the previous boxed runtime and boxed host event to `roc_ui_dispatch`.
2. Stores the returned boxed runtime.
3. Applies the returned command batch to the DOM.

The host does not call Roc callbacks stored in DOM nodes. It does not evaluate
signals. It does not inspect the graph to decide what changed. It only sends
explicit events to Roc and applies explicit commands from Roc.

The current command set includes:

- `ResetDom`
- `CreateElement` and `AppendChild`
- `SetText`, `SetValue`, `SetChecked`, and `SetDisabled`
- `SetRole`, `SetLabel`, and `SetTestId`
- `BindClick`, `BindInput`, and `BindCheck`

The simulated host uses the same command shape a browser implementation would
map onto DOM operations.

## Real-World Effects

The current research platform only models UI events and render commands. It
does not yet expose browser effects such as HTTP requests, file picking,
clipboard access, local storage, IndexedDB, timers, or navigation.

A browser platform should keep the same ownership rule as rendering: Roc
describes what should happen, and the host performs the browser operation.
Effect results come back to Roc as explicit events.

The shape is:

1. A user event enters Roc, such as clicking `Save`.
2. Roc updates state and emits an effect request command, such as `HttpSend`.
3. The browser host performs `fetch`, file IO, storage access, or another
   browser API call.
4. When the operation finishes, the host dispatches a result event back to Roc.
5. Roc folds that result into retained state and renders the next UI.

This keeps signal evaluation pure. A signal should not call `fetch` while it is
being evaluated, read browser storage as a hidden dependency, or mutate the DOM
outside the command stream. If an app needs an outside operation, that operation
should be represented as explicit data crossing the Roc/host boundary.

For example, a future HTTP capability could look like this at the platform
boundary:

```text
Roc command:
HttpSend({
    id: request_id,
    method: "POST",
    url: "/api/profile",
    body: json_bytes,
    on_result: event_id,
})

Host result event:
HttpResult({
    event: event_id,
    status: 200,
    body: response_bytes,
})
```

The app would still model its state with signals:

```roc
save_state =
    Reactive.Signal.fold(
        "save_state",
        NotAsked,
        save_results,
        |_current, result| match result {
            Ok(_) => Saved
            Err(message) => Failed(message)
        },
    )
```

The exact API is future work, but the design goal is not to hide effects inside
signal callbacks. It is to make effects visible as requested work and completed
work.

Effect requests also need identity. Re-rendering the same state should not send
the same HTTP request again just because a signal was evaluated again. A future
effect API should tie requests to event occurrences or explicit state
transitions, carry request ids across the boundary, and let responses identify
which request completed.

Different browser effects fit the same pattern:

- HTTP: Roc emits a request command; the host dispatches a response event.
- File picker: Roc emits an open-file command; the host dispatches selected
  file metadata or bytes.
- Browser storage: Roc emits get/set/remove commands; reads dispatch value
  events, while writes can dispatch success or failure events when needed.
- Timers: Roc emits start/stop timer commands; the host dispatches tick or
  timeout events.
- Navigation: Roc emits navigation commands, and route changes enter Roc as
  explicit location events.

This is different from many web frameworks, where effectful code often lives
inside component lifecycle hooks. In this model, lifecycle timing belongs to
the platform runtime, and effect requests are part of the app's explicit output.
That makes tests and hosts simpler: a test host can assert that the app emitted
an HTTP request, then inject a fake response event without running a browser
network stack.

## Keys Preserve Meaning

Signals apps rebuild graph descriptions often. That does not mean state should
reset. State is retained by explicit string keys.

Use stable keys for anything that should survive a rebuild:

```roc
email =
    Reactive.Signal.hold("email", "", email_changes)

submit_count =
    Reactive.Signal.fold_i64("submit_count", 0, submit_deltas, |current, delta| current + delta)
```

The key is not a label for humans. It is identity for the runtime. If the same
state appears after the next event, use the same key. If it is a different
piece of state, use a different key.

For repeated UI, include the item identity in each local key:

```roc
render_line = |line| {
    { sender: add_send, receiver: add_clicks } =
        Reactive.Event.unit_channel(Str.concat("line_add_click:", line.id))

    quantity =
        Reactive.Signal.fold_i64(
            Str.concat("line_quantity:", line.id),
            1,
            Reactive.Event.map_unit_i64_const(add_clicks, 1),
            |current, delta| current + delta,
        )

    Elem.section(
        line.label,
        [
            Elem.action_button(
                {
                    on_click: add_send,
                    label: Reactive.Signal.const_str(Str.concat("Increase ", line.label)),
                    disabled: Reactive.Signal.const_bool(False),
                },
            ),
            Elem.label(
                Reactive.Signal.map_i64_str_keyed(
                    Str.concat("line_quantity_label:", line.id),
                    quantity,
                    |n| Str.concat("Quantity: ", n.to_str()),
                ),
            ),
        ],
    )
}
```

This lets a row keep its local state when a list is reordered or filtered.
Unstable keys, such as indexes that change when items move, make the runtime
associate old state with the wrong item.

## Dynamic UI

Static elements are enough for simple forms. Real apps also need conditional
sections and lists.

Use `Elem.when` for a boolean choice:

```roc
Elem.when(
    incident_active,
    Elem.paragraph("Incident room open"),
    Elem.paragraph("No active incident"),
)
```

Use `Elem.dynamic` when a signal value chooses a child tree:

```roc
Elem.dynamic(
    step,
    |current_step| if current_step == 0 {
        Elem.section("Cart", cart_children)
    } else {
        Elem.section("Review", review_children)
    },
)
```

Use `Elem.dynamic_keyed` when switching between branches that have independent
local state:

```roc
Elem.dynamic_keyed(
    step,
    |current_step| current_step.to_str(),
    |current_step| if current_step == 0 {
        Elem.section("Cart", cart_children)
    } else {
        Elem.section("Review", review_children)
    },
)
```

Use `Elem.each` for lists:

```roc
Elem.each(tasks, |task| task.id, render_task)
```

The key function should return a stable identity from the data, such as a
database id, slug, or durable client id. It should not return the item's
current position in the list.

## Typed Values And NodeValue

Most app code works with typed Roc values. The runtime stores dynamic signal
and event payloads in `NodeValue` so graph nodes can carry lists and custom
values through one representation.

Built-in values such as `Str`, `Bool`, `I64`, and `Reactive.Unit` already have
the methods needed by the current APIs. When a custom type flows through
generic APIs such as `Event.map`, `Signal.fold`, `Elem.dynamic`, or `Elem.each`,
define `encode` and `decode` methods for that type:

```roc
Task := { id : Str, label : Str }.{
    make : Str, Str -> Task
    make = |id, label| { id, label }

    encode : Task, NodeValue -> Try(NodeValue, [])
    encode = |task, fmt| [task.id, task.label].encode(fmt)

    decode : NodeValue, NodeValue -> (Try(Task, [TypeMismatch]), NodeValue)
    decode = |nv, fmt| {
        (result, rest) =
            NodeValue.decode_list(fmt, nv, |source, f| Str.decode(source, f))

        task_result =
            match result {
                Ok(fields) =>
                    match (List.get(fields, 0), List.get(fields, 1)) {
                        (Ok(id), Ok(label)) => Ok(Task.make(id, label))
                        _ => Err(TypeMismatch)
                    }

                Err(err) => Err(err)
            }

        (task_result, rest)
    }
}
```

This keeps the public app code typed while giving the runtime an explicit value
format for retained state, dynamic rendering, and list rendering.

## Choosing The Right Primitive

Use `Signal.const_str`, `Signal.const_bool`, or `Signal.const_i64` when a value
does not depend on user interaction.

Use `Event.channel` for controls that carry values, such as text inputs and
checkboxes. Use `Event.unit_channel` for controls where the occurrence itself
is the data, such as button clicks.

Use `Signal.hold` when the next state is simply the latest event value. Text
input values and checkbox checked states usually use `hold`.

Use `Signal.fold` or `Signal.fold_i64` when the next state depends on the
previous state. Counters, toggles, undo stacks, wizard steps, and list
replacement flows usually use folds.

Use `Signal.map` for display-only derived data. Use `Signal.map2` when derived
data depends on two signals. Prefer keyed variants for derived values that are
part of a long-lived UI path.

Use `Elem.label` for signal-backed text. Use `Elem.text` and `Elem.paragraph`
for text that never changes.

Use `Elem.action_button` when the disabled state is signal-backed. Use
`Elem.button` for a basic click target without disabled state.

Use `Elem.dynamic_keyed` and `Elem.each` when branch or row identity matters,
especially when local state under that branch or row should survive rebuilds.

## Common Mistakes

Do not treat a signal like a mutable variable. You do not assign to a signal.
You describe how events produce retained state, and how other signals derive
from it.

Do not let the browser host own Roc app state. The host stores the boxed
runtime, but the state inside it belongs to the Roc runtime. The host should
not mirror a second application model in JavaScript.

Do not reuse one event key for different controls. Event keys identify event
sources. Two different controls should have two different event keys unless
they are intentionally the same source.

Do not use unstable list keys. If a row has local state, the row key must come
from item identity, not from list position.

Do not recover missing information in the host. If the host needs to know what
to update, Roc should emit a command. If Roc needs to know what happened, the
host should send an explicit event.

## Where To Look Next

The maintained examples in `apps/` show larger versions of the same patterns:

- `ops_dashboard.roc` shows scalar signal chains, conditional UI, keyed alert
  rows, and input state.
- `checkout_wizard.roc` shows wizard steps, form state, disabled actions, list
  replacement, and keyed line-item state.
- `kanban_board.roc` shows list reorder, archive/reset flows, filtering, and
  per-card local state.

Run the representative suite from the repository root:

```sh
zig build run-test-signals
```

That builds each app and replays its browser-style interaction spec against
the simulated DOM host.
