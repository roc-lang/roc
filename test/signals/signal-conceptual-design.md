# Signal-Based UI Platform for Roc

## Design Document v3.1

> **Implementation Status**: A working prototype exists in `test/signals/platform/`.
> The core Signal, Event, and Elem APIs are implemented and type-check successfully.
> See `app.roc` for an example counter application.

---

## Table of Contents

1. [Introduction](#introduction)
2. [Core Concepts](#core-concepts)
3. [The Signal API](#the-signal-api)
4. [The Event API](#the-event-api)
5. [Bridging Signals and Events](#bridging-signals-and-events)
6. [Channels: Sending and Receiving Events](#channels-sending-and-receiving-events)
7. [Combining Signals](#combining-signals)
8. [Sampling with zip_with](#sampling-with-zip_with)
9. [Components and Elem.translate](#components-and-elemtranslate)
10. [Dynamic Structure](#dynamic-structure)
11. [Lifecycle Events](#lifecycle-events)
12. [HTTP and Async Resources](#http-and-async-resources)
13. [Routing](#routing)
14. [Complete Example: Product Selector](#complete-example-product-selector)
15. [Platform Implementation Notes](#platform-implementation-notes)

---

## Introduction

### What Problem Does This Solve?

Traditional UI architectures like Elm's TEA (The Elm Architecture) work by re-running a `render` function every time state changes, producing a virtual tree, then diffing it against the previous tree to determine what DOM updates are needed. This works well but has performance costs:

- Re-rendering entire component trees even when only one value changed
- Tree diffing algorithms that scale with tree size
- Need for manual optimization (like `Html.lazy`) to avoid unnecessary work

**Signal-based UI takes a different approach: render runs once at startup, building a dependency graph. When state changes, updates propagate through the graph directly to affected DOM nodes—no re-rendering, no diffing.**

### The Core Insight

Instead of:
```
state changes → re-render entire tree → diff old vs new → patch DOM
```

We have:
```
event fires → signal updates → propagate through graph → update only affected DOM nodes
```

This is what all the modern "signals" libraries in JavaScript are doing (Solid, Preact Signals, Angular Signals, Vue's reactivity system). This document describes how to bring this architecture to Roc.

### Design Goals

1. **Effect-agnostic**: The UI system should work across platforms—desktop, mobile, web, editor plugins
2. **No virtual DOM**: Updates should be surgical, not require tree diffing
3. **Type-safe composition**: Parent-child component relationships should be enforced by the type system
4. **Clear semantics**: Distinguish between continuous values (Signals) and discrete occurrences (Events)
5. **Leverage Roc's features**: Use Record Builder syntax, static dispatch, and Roc's type system effectively
6. **Familiar patterns**: Borrow concepts from Rust channels and functional reactive programming

---

## Core Concepts

### Signals vs Events: The Fundamental Distinction

This design uses two distinct types to model time-varying data:

| Type | Description | Has Current Value? | Example |
|------|-------------|-------------------|---------|
| `Signal(a)` | Continuous value over time | Yes, always | Mouse position, form field value, computed total |
| `Event(a)` | Discrete occurrences | No | Button click, key press, HTTP response |

This distinction is crucial:
- A **Signal** always has a current value you can read
- An **Event** represents something that *happens* at specific moments

### What is a Signal?

A **Signal** is a value that changes over time. Think of it as a container that:
- Always has a current value
- Can notify dependents when that value changes
- Can be transformed and combined with other signals

```roc
# A signal containing a string
greeting : Signal(Str)

# A signal containing a number
count : Signal(I64)

# A signal containing a record
user : Signal({ name : Str, age : U8 })
```

### What is an Event?

An **Event** represents discrete occurrences over time. Unlike signals:
- Events don't have a "current value"
- Events fire at specific moments
- Events can be transformed, filtered, and merged

```roc
# An event that fires when a button is clicked
click : Event({})

# An event carrying the new text when an input changes
text_changed : Event(Str)

# An event carrying an HTTP response
response : Event(Result(User, HttpError))
```

### What is an EventSender?

An **EventSender** is a write-only handle for firing events. When you create a channel, you get both:
- An `EventSender` for firing events
- An `Event` for reacting to those events

This separation enforces clear data flow: UI elements that produce events (like buttons) get senders, while elements that react to events get the Event.

### The Dependency Graph

When your `render` function runs (once, at startup), it builds a **dependency graph**:

```
┌─────────────────────────────────────────────────────────────────────┐
│                    Dependency Graph                                 │
│                                                                     │
│   Event A ──────▶ Signal C ──────┬──────▶ DOM Element 1            │
│                        │         │                                  │
│   Signal B ────────────┘         └──────▶ DOM Element 2            │
│                                                                     │
│   Event D ──────▶ Signal E ─────────────▶ DOM Element 3            │
│                                                                     │
└─────────────────────────────────────────────────────────────────────┘
```

When Event A fires:
1. Signal C is recalculated (it depends on Event A and Signal B)
2. DOM Elements 1 and 2 are updated
3. Event D, Signal E, and DOM Element 3 are untouched

This is **fine-grained reactivity**: only the nodes that actually depend on what changed get updated.

---

## The Signal API

### Creating Constant Signals

```roc
Signal.const : a -> Signal(a)
```

Creates a signal that never changes. This is useful for static values in your UI:

```roc
title = Signal.const("Welcome to My App")
max_items = Signal.const(100)
```

### Transforming Signals

```roc
Signal.map : Signal(a), (a -> b) -> Signal(b)
```

Transforms a signal's values. The function runs whenever the source signal changes:

```roc
count : Signal(I64)
count_text : Signal(Str)
count_text = count.map(Num.to_str)

# When count is 5, count_text is "5"
# When count becomes 6, count_text becomes "6"
```

### Combining Signals

```roc
Signal.map2 : Signal(a), Signal(b), (a, b -> c) -> Signal(c)
```

Combines two signals into one. The result updates whenever either input changes:

```roc
quantity : Signal(I64)
price : Signal(F64)

subtotal : Signal(F64)
subtotal = Signal.map2(quantity, price, |q, p| Num.to_f64(q) * p)
```

### Getting Changes as Events

```roc
Signal.changes : Signal(a) -> Event(a)
```

Converts a signal into an event stream that fires whenever the signal's value changes:

```roc
count : Signal(I64)
count_changed : Event(I64)
count_changed = Signal.changes(count)
```

### The Full Signal Type

```roc
Signal(a) := ... .{
    # Create a constant signal
    const : a -> Signal(a)

    # Transform values
    map : Signal(a), (a -> b) -> Signal(b)

    # Combine two signals
    map2 : Signal(a), Signal(b), (a, b -> c) -> Signal(c)

    # Convert event to signal (with initial value)
    hold : a, Event(a) -> Signal(a)

    # Fold events into a signal
    fold : a, Event(e), (a, e -> a) -> Signal(a)

    # Sample signal when event fires
    zip_with : Signal(a), Event(e), (a, e -> a) -> Signal(a)

    # Get changes as events
    changes : Signal(a) -> Event(a)
}
```

---

## The Event API

### Creating Channels

```roc
Event.init : {} -> (EventSender(a), Event(a))
```

Creates a channel—a sender/receiver pair. The sender is given to things that produce events; the event is used to react to those events:

```roc
# Create a channel for button clicks
(click_send, click_recv) = Event.init()

# click_send : EventSender({})
# click_recv : Event({})
```

### Transforming Events

```roc
Event.map : Event(a), (a -> b) -> Event(b)
```

Transforms event payloads:

```roc
key_press : Event(KeyCode)
is_enter : Event(Bool)
is_enter = key_press.map(|code| code == Enter)
```

### Filtering Events

```roc
Event.filter : Event(a), (a -> Bool) -> Event(a)
```

Only passes through events that satisfy a predicate:

```roc
all_keys : Event(KeyCode)
enter_only : Event(KeyCode)
enter_only = all_keys.filter(|code| code == Enter)
```

### Merging Events

```roc
Event.merge : Event(a), Event(a) -> Event(a)
```

Combines two event streams into one:

```roc
inc_click : Event({})
keyboard_inc : Event({})

any_inc : Event({})
any_inc = Event.merge(inc_click, keyboard_inc)
```

### Gating Events with Signals

```roc
Event.gate : Event(a), Signal(Bool) -> Event(a)
```

Only passes events when the signal is `Bool.true`:

```roc
submit_click : Event({})
form_valid : Signal(Bool)

valid_submit : Event({})
valid_submit = Event.gate(submit_click, form_valid)
```

### Attaching Signal Values to Events

```roc
Event.with_latest : Event(e), Signal(a) -> Event({ event : e, value : a })
```

Samples a signal's current value whenever an event fires:

```roc
submit_click : Event({})
form_data : Signal(FormData)

submission : Event({ event : {}, value : FormData })
submission = Event.with_latest(submit_click, form_data)
```

### Timing Controls

```roc
Event.debounce : Event(a), U64 -> Event(a)  # milliseconds
Event.throttle : Event(a), U64 -> Event(a)  # milliseconds
```

Control event frequency:

```roc
key_input : Event(Str)

# Only fire after 300ms of no input (for search-as-you-type)
debounced_input : Event(Str)
debounced_input = key_input.debounce(300)

# Fire at most once per 100ms (for scroll handlers)
throttled_scroll : Event(ScrollPos)
throttled_scroll = scroll_event.throttle(100)
```

### The Full Event Type

```roc
Event(a) := ... .{
    # Create a channel
    init : {} -> (EventSender(a), Event(a))

    # Transform
    map : Event(a), (a -> b) -> Event(b)

    # Filter
    filter : Event(a), (a -> Bool) -> Event(a)

    # Combine
    merge : Event(a), Event(a) -> Event(a)

    # Gate by signal
    gate : Event(a), Signal(Bool) -> Event(a)

    # Sample signal on event
    with_latest : Event(e), Signal(a) -> Event({ event : e, value : a })

    # Timing
    debounce : Event(a), U64 -> Event(a)
    throttle : Event(a), U64 -> Event(a)
}
```

---

## Bridging Signals and Events

### Event → Signal: `hold` and `fold`

Events are discrete; signals are continuous. To convert:

**`Signal.hold`** — Hold the most recent event value:
```roc
Signal.hold : a, Event(a) -> Signal(a)

# Example: track the last key pressed
last_key : Signal(KeyCode)
last_key = Signal.hold(NoKey, key_press_event)
```

**`Signal.fold`** — Accumulate events into state:
```roc
Signal.fold : a, Event(e), (a, e -> a) -> Signal(a)

# Example: count total clicks
click_count : Signal(I64)
click_count = Signal.fold(0, click_event, |count, _| count + 1)
```

### Signal → Event: `changes`

**`Signal.changes`** — Get an event stream of signal changes:
```roc
Signal.changes : Signal(a) -> Event(a)

# Example: react when selection changes
selection : Signal(ItemId)
selection_changed : Event(ItemId)
selection_changed = Signal.changes(selection)
```

### The Bridge Pattern

A common pattern: Event → Signal → Event

```roc
# User types in search box (events)
search_input : Event(Str)

# Debounce and hold as signal
search_query : Signal(Str)
search_query = search_input.debounce(300) |> Signal.hold("")

# Convert back to event for API call trigger
search_trigger : Event(Str)
search_trigger = Signal.changes(search_query)
```

---

## Channels: Sending and Receiving Events

### The Problem Channels Solve

In a UI, we need to connect user interactions (button clicks, input changes, etc.) to state updates. Channels provide a clean, type-safe way to do this.

### How Channels Work

```roc
(sender, receiver) = Event.init()
```

This creates two connected handles:

| Type | Can Read? | Can Write? | Given To |
|------|-----------|------------|----------|
| `Event(a)` (receiver) | ✓ | ✗ | Things that react to events |
| `EventSender(a)` (sender) | ✗ | ✓ | Things that produce events |

### Example: Button Click

```roc
render = |state| {
    # Create a channel for the button
    (click_send, click_recv) = Event.init()

    # click_send goes to the button (producer)
    # click_recv is used in our logic (consumer)

    count = state.map(.count)
        .zip_with(click_recv, |c, _| c + 1)  # increment on click

    div([
        button(click_send, "Click me"),      # button fires to sender
        label(count.map(Num.to_str)),        # label reads from signal
    ])
}
```

### Data Flow

```
User clicks button
        │
        ▼
┌───────────────────┐
│ button fires {}   │
│ to click_send     │
└─────────┬─────────┘
          │
          ▼
┌───────────────────┐
│ click_recv fires  │
│ (Event receives)  │
└─────────┬─────────┘
          │
          ▼
┌───────────────────┐
│ zip_with triggers │
│ count + 1         │
└─────────┬─────────┘
          │
          ▼
┌───────────────────┐
│ label updates     │
│ via .map(to_str)  │
└───────────────────┘
```

### Multiple Channels

Each channel is independent:

```roc
render = |state| {
    (inc_send, inc_recv) = Event.init()
    (dec_send, dec_recv) = Event.init()
    (reset_send, reset_recv) = Event.init()

    count = state.map(.count)
        .zip_with(inc_recv, |c, _| c + 1)
        .zip_with(dec_recv, |c, _| c - 1)
        .zip_with(reset_recv, |_, _| 0)

    div([
        button(dec_send, "-"),
        label(count.map(Num.to_str)),
        button(inc_send, "+"),
        button(reset_send, "Reset"),
    ])
}
```

---

## Combining Signals

### map2: Combining Two Signals

```roc
Signal.map2 : Signal(a), Signal(b), (a, b -> c) -> Signal(c)
```

Combines two signals into one. The result updates whenever either input changes:

```roc
quantity : Signal(I64)
price : Signal(F64)

subtotal : Signal(F64)
subtotal = Signal.map2(quantity, price, |q, p| Num.to_f64(q) * p)

# quantity=2, price=10.0 → subtotal=20.0
# quantity=3, price=10.0 → subtotal=30.0  (quantity changed)
# quantity=3, price=15.0 → subtotal=45.0  (price changed)
```

### The Record Builder Pattern

Roc's Record Builder syntax makes combining many signals elegant. If `Signal` implements `map2`, you can write:

```roc
{ a: signal_a, b: signal_b, c: signal_c }.Signal
```

This desugars to chained `map2` calls:

```roc
# Two fields:
{ a: fa, b: fb }.Signal
# becomes:
Signal.map2(fa, fb, |a, b| { a, b })

# Three fields:
{ a: fa, b: fb, c: fc }.Signal
# becomes:
Signal.map2(fa, Signal.map2(fb, fc, |b, c| (b, c)), |a, (b, c)| { a, b, c })
```

### Practical Example: Form Data

```roc
Form := { email : Str, password : Str, age : U8, agreed : Bool }.{
    render : Signal(Form) -> Elem
    render = |state| {
        (email_send, email_recv) = Event.init()
        (pass_send, pass_recv) = Event.init()
        (age_send, age_recv) = Event.init()
        (agree_send, agree_recv) = Event.init()

        email = state.map(.email).zip_with(email_recv, |_, e| e)
        password = state.map(.password).zip_with(pass_recv, |_, p| p)
        age = state.map(.age).zip_with(age_recv, |_, a| a)
        agreed = state.map(.agreed).zip_with(agree_recv, |_, a| a)

        # Combine all fields using Record Builder!
        form_data : Signal(Form)
        form_data = {
            email: email,
            password: password,
            age: age,
            agreed: agreed,
        }.Signal

        # Now form_data updates whenever ANY field changes
        # Validation can work on the combined signal:
        is_valid = form_data.map(|f|
            Str.contains(f.email, "@")
            and Str.len(f.password) >= 8
            and f.age >= 18
            and f.agreed
        )

        # ... render form ...
    }
}
```

---

## Sampling with zip_with

### The Key Primitive

```roc
Signal.zip_with : Signal(a), Event(e), (a, e -> a) -> Signal(a)
```

`zip_with` is the bridge between continuous signals and discrete events. It says:

> "When an event fires, sample the signal's current value and apply the function."

### How It Differs from map2

| Primitive | When does the function run? | Use case |
|-----------|----------------------------|----------|
| `map2` | Whenever either signal changes | Combining current values |
| `zip_with` | Only when the event fires | Responding to discrete events |

### Example: Counter

```roc
count = state.map(.count)
    .zip_with(inc_recv, |c, _| c + 1)
    .zip_with(dec_recv, |c, _| c - 1)
```

Timeline:

```
Time    state.count    inc_recv    dec_recv    count (result)
─────────────────────────────────────────────────────────────────
T1      5              -           -           5       ← follows state
T2      5              click       -           6       ← inc fired: 5+1
T3      5              click       -           7       ← inc fired: 6+1
T4      5              -           click       6       ← dec fired: 7-1
T5      10             -           -           10      ← state changed externally
T6      10             click       -           11      ← inc fired: 10+1
```

Key observations:
- The result follows the base signal when no events fire
- Events transform the current value
- External changes to the base signal are respected
- Multiple `zip_with` calls can be chained

### Chaining zip_with

Each `zip_with` adds another event source:

```roc
count = base_count                              # Signal(I64)
    .zip_with(inc_recv, |c, _| c + 1)          # +1 on inc
    .zip_with(dec_recv, |c, _| c - 1)          # -1 on dec
    .zip_with(double_recv, |c, _| c * 2)       # *2 on double
    .zip_with(reset_recv, |_, _| 0)            # reset to 0
```

### State Ownership Rules

Understanding how state flows is critical:

1. **`zip_with` follows base signal AND accumulates events**: When the base signal changes externally, the result "rebases" to that new value. Local event-driven changes are applied on top.

2. **For truly local state, use `Signal.fold`**:
   ```roc
   # This count is independent of any parent state
   local_count = Signal.fold(0, inc_recv, |c, _| c + 1)
   ```

3. **For controlled components, parent owns state via `Elem.translate`**: The child component receives state from parent and sends changes back up.

---

## Components and Elem.translate

### The Challenge of Composition

Components need to:
1. Receive state from their parent
2. Render UI based on that state
3. Handle local events
4. Push state changes back to the parent

Without a pattern for this, you end up with messy manual wiring.

### Elem.translate: The Solution

```roc
Elem.translate :
    (Signal(child) -> Elem),       # child's render function
    (parent -> child),             # getter: extract child state (pure function)
    (parent, child -> parent)      # setter: update parent with new child (pure function)
    -> (Signal(parent) -> Elem)
```

This function:
1. Takes a child component that works with `Signal(child)`
2. Provides a pure getter function to extract `child` from `parent`
3. Provides a pure setter function to update `parent` with new `child` state
4. Returns a component that works with `Signal(parent)`

The platform handles all the signal-level plumbing internally.

### Example: Counter Component

First, define a reusable counter:

```roc
Counter := { count : I64 }.{
    init : I64 -> Counter
    init = |count| { count }

    render : Signal(Counter) -> Elem
    render = |state| {
        (inc_send, inc_recv) = Event.init()
        (dec_send, dec_recv) = Event.init()

        count = state
            .map(.count)
            .zip_with(inc_recv, |c, _| c + 1)
            .zip_with(dec_recv, |c, _| c - 1)

        div({ class: "counter" }, [
            button(dec_send, "-"),
            label(count.map(Num.to_str)),
            button(inc_send, "+"),
        ])
    }
}
```

Now, use it in a parent with `Elem.translate`:

```roc
App := { left : I64, right : I64 }.{
    render : Signal(App) -> Elem
    render = |state| {
        # Wire up left counter to state.left
        left_counter = Elem.translate(
            Counter.render,
            |parent| { count: parent.left },             # getter
            |parent, child| { ..parent, left: child.count },  # setter
        )

        # Wire up right counter to state.right
        right_counter = Elem.translate(
            Counter.render,
            |parent| { count: parent.right },
            |parent, child| { ..parent, right: child.count },
        )

        # Derive total from both
        total = state.map(|s| s.left + s.right)

        div([
            label("Left:"),
            left_counter(state),
            label("Right:"),
            right_counter(state),
            label(total.map(|t| "Total: ${Num.to_str(t)}")),
        ])
    }
}
```

### Data Flow with Elem.translate

```
┌─────────────────────────────────────────────────────────────────────────┐
│                              PARENT                                     │
│                                                                         │
│    app_state : Signal({ left: I64, right: I64 })                       │
│         │                                                               │
│         │                                                               │
│    ┌────┴─────────────────────────────┐                                │
│    │                                  │                                 │
│    ▼ getter                           ▼ getter                          │
│  .map(.left)                        .map(.right)                        │
│    │                                  │                                 │
│    ▼                                  ▼                                 │
│ ┌──────────────────────┐  ┌──────────────────────┐                     │
│ │   Left Counter       │  │   Right Counter      │                     │
│ │                      │  │                      │                     │
│ │ sees Signal({ count })│  │ sees Signal({ count })│                     │
│ │ doesn't know about   │  │ doesn't know about   │                     │
│ │ right, app, etc.     │  │ left, app, etc.      │                     │
│ │                      │  │                      │                     │
│ │ inc/dec clicks       │  │ inc/dec clicks       │                     │
│ │       │              │  │       │              │                     │
│ └───────┼──────────────┘  └───────┼──────────────┘                     │
│         │                         │                                     │
│         ▼ setter                  ▼ setter                              │
│    { ..p, left: c }          { ..p, right: c }                         │
│         │                         │                                     │
│         └────────┬────────────────┘                                     │
│                  │                                                      │
│                  ▼                                                      │
│             app_state (updated)                                         │
│                  │                                                      │
│                  ▼                                                      │
│             total derived                                               │
│                                                                         │
└─────────────────────────────────────────────────────────────────────────┘
```

### Benefits of Elem.translate

| Benefit | Explanation |
|---------|-------------|
| **Encapsulation** | Counter only knows about `Signal(Counter)`, not the parent's structure |
| **Reusability** | Same Counter works with any parent that can provide/accept an I64 |
| **Type Safety** | Compiler ensures getter/setter types are consistent |
| **Explicit Data Flow** | Dependencies visible in type signatures |
| **Simple** | Getter and setter are pure functions—no signal manipulation needed |

---

## Dynamic Structure

### The Problem

So far, we've shown static UI structures—the same elements exist throughout the app's lifetime. But real apps need:
- Conditional rendering (show a modal only when open)
- Dynamic lists (add/remove items)
- Routing (swap entire page contents)

### Signal.when: Conditional Elements

```roc
Signal.when : Signal(Bool), Elem -> Elem
```

Shows or hides an element based on a boolean signal:

```roc
show_modal : Signal(Bool)

div([
    button(open_modal_send, "Open"),
    Signal.when(show_modal, modal_content()),  # only exists when true
])
```

When `show_modal` changes:
- `true`: Platform creates the modal's subgraph and DOM nodes
- `false`: Platform destroys the modal's subgraph and DOM nodes

### Signal.switch: Multi-Branch Rendering

```roc
Signal.switch : Signal(a), (a -> Elem) -> Elem
```

Renders different elements based on a signal's value:

```roc
Page : [Home, Products, Cart, NotFound]

current_page : Signal(Page)

div([
    header(),
    Signal.switch(current_page, |page|
        match page {
            Home => home_page()
            Products => products_page()
            Cart => cart_page()
            NotFound => not_found_page()
        }
    ),
    footer(),
])
```

When `current_page` changes:
1. Platform destroys the old page's entire subgraph
2. Platform calls the builder function with the new value
3. Platform creates the new page's subgraph and DOM nodes

### Signal.each: Dynamic Lists

```roc
Signal.each :
    Signal(List(item)),
    (item -> Str),            # key function for efficient updates
    (Signal(item) -> Elem)    # render function per item
    -> Elem
```

Renders a list of elements that can grow, shrink, and reorder:

```roc
Todo : { id : Str, text : Str, done : Bool }

todos : Signal(List(Todo))

ul([
    Signal.each(
        todos,
        |todo| todo.id,                    # key function
        |todo_signal| todo_item(todo_signal),  # render each item
    ),
])
```

When the list changes, the platform:
- **Added items**: Creates new subgraph, inserts DOM node
- **Removed items**: Destroys subgraph, removes DOM node
- **Reordered items**: Moves existing DOM nodes (no recreate)
- **Updated items**: Pushes new value to existing `Signal(item)`

### Why Keys Matter

Keys let the platform identify which items are "the same" across updates:

```
Before: [{ id: "1", text: "Buy milk" }, { id: "2", text: "Write code" }]
After:  [{ id: "2", text: "Write code" }, { id: "1", text: "Buy milk" }]

Without keys: Destroy all, recreate all (expensive)
With keys:    Just swap DOM node order (cheap)
```

### The Graph Is Dynamic

The dependency graph is NOT pre-built for every possible state. Instead:

```
┌───────────────────────────────────────────────────────────────────────┐
│  Platform Runtime                                                     │
│                                                                       │
│  Signal.switch node:                                                  │
│  ├── source: current_page                                            │
│  ├── builder_fn: |page| match page { ... }  ← STORED, not executed   │
│  └── current_branch: Home                   ← ACTIVE, built          │
│                       Products              ← NOT BUILT               │
│                       Cart                  ← NOT BUILT               │
│                                                                       │
└───────────────────────────────────────────────────────────────────────┘
```

The platform stores builder functions but only executes them when their branch becomes active.

---

## Lifecycle Events

### Component Lifecycle

Components have a lifecycle tied to their presence in the DOM:

```roc
on_mount : {} -> Event({})
on_unmount : {} -> Event({})
```

**`on_mount`** fires when a component is created and added to the DOM:
```roc
render = |state| {
    # Fetch data when component mounts
    user_resource = fetch({
        trigger: on_mount(),
        url: Signal.const("/api/user"),
        decoder: user_decoder,
    })

    # ...
}
```

**`on_unmount`** fires when a component is about to be destroyed:
```roc
render = |state| {
    # Clean up when component is removed
    _ = on_unmount().map(|_| {
        cancel_subscription()
        save_draft()
    })

    # ...
}
```

### Cleanup Pattern

Resources are automatically cleaned up when their containing component unmounts:
- Pending HTTP requests are cancelled
- Event subscriptions are removed
- Child signal graphs are garbage collected

For manual cleanup:
```roc
render = |state| {
    (cleanup_send, cleanup_recv) = Event.init()

    # Set up some resource
    subscription = subscribe_to_websocket()

    # Clean up on unmount
    _ = on_unmount().map(|_| {
        subscription.close()
    })

    # ...
}
```

---

## HTTP and Async Resources

### The Resource Pattern

HTTP requests are represented as signals that go through states:

```roc
Resource(a) : Signal([Idle, Loading, Success(a), Failure(Str)])
```

### Fetching Data

```roc
fetch : {
    trigger : Event(*),        # when to make the request
    url : Signal(Str),         # URL to fetch
    decoder : List(U8) -> Result(a, Str),
} -> Resource(a)
```

Example:

```roc
render = |state| {
    (load_send, load_recv) = Event.init()

    user_resource = fetch({
        trigger: load_recv,
        url: Signal.const("https://api.example.com/user/1"),
        decoder: user_decoder,
    })

    content = user_resource.map(|res|
        match res {
            Idle => "Click to load"
            Loading => "Loading..."
            Success(user) => "Hello, ${user.name}!"
            Failure(err) => "Error: ${err}"
        }
    )

    div([
        button(load_send, "Load User"),
        label(content),
    ])
}
```

### Chaining Requests

Dependent requests happen naturally:

```roc
# First request
user_resource = fetch({
    trigger: on_mount(),
    url: Signal.const("/api/user"),
    decoder: user_decoder,
})

# Second request depends on first
posts_url = user_resource.map(|res|
    match res {
        Success(user) => Some("/api/posts?user=${user.id}")
        _ => None
    }
)

# Trigger when URL becomes Some
posts_trigger = Signal.changes(posts_url)
    .filter(|opt| opt != None)

posts_resource = fetch({
    trigger: posts_trigger,
    url: posts_url.map(|opt| opt |> Result.with_default("")),
    decoder: posts_decoder,
})
```

### Posting Data

```roc
post : {
    trigger : Event(*),
    url : Signal(Str),
    body : Signal(List(U8)),
    decoder : List(U8) -> Result(a, Str),
} -> Resource(a)
```

Example with form submission:

```roc
(submit_send, submit_recv) = Event.init()

form_data = { email: email, password: password }.Signal

submission = post({
    trigger: submit_recv,
    url: Signal.const("/api/register"),
    body: form_data.map(encode_json),
    decoder: response_decoder,
})
```

### Error Handling Philosophy

Roc has no exceptions—fallible operations return `Try(ok, err)`. Errors are just data:

```roc
# The Resource type already encodes success/failure
Resource(a) : Signal([Idle, Loading, Success(a), Failure(Str)])

# Pattern match to handle all cases
content = user_resource.map(|res|
    match res {
        Idle => "Click to load"
        Loading => "Loading..."
        Success(user) => "Hello, ${user.name}!"
        Failure(err) => "Error: ${err}"  # Error is just another state
    }
)
```

For validation:

```roc
ValidatedField(a) : { value : a, error : [None, Invalid(Str)] }

email : Signal(ValidatedField(Str))
email = email_input.map(|text|
    if Str.contains(text, "@") then
        { value: text, error: None }
    else
        { value: text, error: Invalid("Must contain @") }
)

# Show error message when invalid
div([
    input({ value: email.map(.value), on_input: email_send }),
    Signal.when(
        email.map(|e| e.error != None),
        label({ class: "error" }, email.map(|e|
            match e.error {
                Invalid(msg) => msg
                None => ""
            }
        )),
    ),
])
```

The type system ensures you handle errors where they occur—no catch-all boundaries needed.

---

## Routing

### Router as a Signal

Routing is just `Signal.switch` over a route signal:

```roc
Route : [Home, Products, ProductDetail(Str), Cart, NotFound]

Router : {
    current : Signal(Route),
    navigate : Route -> {},
}

router : { initial : Route } -> Router
router = |{ initial }| {
    (nav_send, nav_recv) = Event.init()

    # Listen for browser back/forward
    popstate = on_popstate().map(parse_route)

    # Combine programmatic navigation with browser navigation
    current = Signal.fold(
        initial,
        Event.merge(nav_recv, popstate),
        |_, new| new
    )

    navigate = |route| {
        push_history(route_to_path(route))
        # Platform internally fires nav_send
    }

    { current, navigate }
}
```

### Using the Router

```roc
main = {
    app_router = router({ initial: Home })

    div([
        nav([
            link(app_router, Home, "Home"),
            link(app_router, Products, "Products"),
            link(app_router, Cart, "Cart"),
        ]),

        Signal.switch(app_router.current, |route|
            match route {
                Home => home_page()
                Products => products_page(app_router)
                ProductDetail(id) => product_detail_page(id)
                Cart => cart_page()
                NotFound => not_found_page()
            }
        ),
    ])
}
```

### Page Lifecycle

When navigating from Home to Products:

```
1. app_router.current changes: Home → Products

2. Signal.switch detects the change

3. DESTROY Home:
   - on_unmount fires
   - Cancel pending requests
   - Remove DOM nodes
   - Garbage collect signals

4. BUILD Products:
   - Call builder function with Products
   - Create signal subgraph
   - Create DOM nodes
   - on_mount fires → fetch products

5. Products page is now active
```

### Shared State Survives Navigation

State defined at the app root persists across page changes:

```roc
main = {
    app_router = router({ initial: Home })

    # This persists across ALL pages
    cart_items : Signal(List(CartItem))
    cart_items = Signal.fold([], add_to_cart_event, |items, item| List.append(items, item))

    div([
        # Header always shows cart count
        header(cart_items.map(List.len)),

        Signal.switch(app_router.current, |route|
            match route {
                ProductDetail(id) =>
                    # Can add to cart_items
                    product_detail_page(id, cart_items)
                Cart =>
                    # Can read/modify cart_items
                    cart_page(cart_items)
                _ => ...
            }
        ),
    ])
}
```

---

## Complete Example: Product Selector

This example ties together all the concepts.

### Counter.roc

```roc
Counter := { count : I64 }.{
    init : I64 -> Counter
    init = |count| { count }

    render : { min : Signal(I64), max : Signal(I64) } -> (Signal(Counter) -> Elem)
    render = |{ min, max }| |state| {
        (inc_send, inc_recv) = Event.init()
        (dec_send, dec_recv) = Event.init()

        count = state.map(.count)

        # Check bounds
        can_inc = { c: count, m: max }.Signal.map(|{ c, m }| c < m)
        can_dec = { c: count, m: min }.Signal.map(|{ c, m }| c > m)

        # Apply changes
        new_count = count
            .zip_with(inc_recv, |c, _| c + 1)
            .zip_with(dec_recv, |c, _| c - 1)

        div({ class: "counter" }, [
            button({
                send: dec_send,
                disabled: can_dec.map(Bool.not),
            }, "-"),
            label(new_count.map(Num.to_str)),
            button({
                send: inc_send,
                disabled: can_inc.map(Bool.not),
            }, "+"),
        ])
    }
}
```

### ProductSelector.roc

```roc
ProductSelector := { product_id : Str, quantity : I64 }.{
    render : Signal(ProductSelector) -> Elem
    render = |state| {
        product_id = state.map(.product_id)

        # Fetch product on mount
        product_resource = fetch({
            trigger: on_mount(),
            url: product_id.map(|id| "https://api.store.com/products/${id}"),
            decoder: product_decoder,
        })

        # Extract product data
        product_name = product_resource.map(|r|
            match r {
                Success(p) => p.name
                Loading => "Loading..."
                _ => "Unknown"
            }
        )

        product_price = product_resource.map(|r|
            match r {
                Success(p) => p.price
                _ => 0.0
            }
        )

        stock = product_resource.map(|r|
            match r {
                Success(p) => p.stock
                _ => 0
            }
        )

        # Wire up counter with Elem.translate
        quantity_counter = Elem.translate(
            Counter.render({ min: Signal.const(1), max: stock }),
            |parent| { count: parent.quantity },
            |parent, child| { ..parent, quantity: child.count },
        )

        # Calculate subtotal
        quantity = state.map(.quantity)
        subtotal = { q: quantity, p: product_price }.Signal
            .map(|{ q, p }| Num.to_f64(q) * p)

        # Add to cart handling
        (cart_send, cart_recv) = Event.init()

        div({ class: "product-selector" }, [
            h1(product_name),
            label(product_price.map(|p| "Price: $${Num.to_str(p)}")),

            div({ class: "quantity" }, [
                label("Quantity:"),
                quantity_counter(state),
            ]),

            div({ class: "subtotal" }, [
                label(subtotal.map(|s| "Subtotal: $${Num.to_str(s)}")),
            ]),

            button(cart_send, "Add to Cart"),
        ])
    }
}
```

---

## Platform Implementation Notes

### What the Platform Does

1. **At Startup**:
   - Call `main` once
   - Walk the returned `Elem` tree
   - Build the dependency graph
   - Create actual DOM/native elements
   - Subscribe to event sources (clicks, timers, etc.)

2. **On Each Event**:
   - Fire the event into the graph
   - Mark dependent signals as "dirty"
   - Propagate through the graph in topological order
   - Recompute only affected nodes
   - Batch DOM mutations
   - Apply mutations

### Glitch-Free Semantics

The platform guarantees **glitch-free** updates:

> A signal is only evaluated after ALL its dependencies have been updated. Intermediate inconsistent states are never observed.

Example:
```roc
a = Signal.const(1)
b = a.map(|x| x + 1)      # depends on a
c = a.map(|x| x * 2)      # depends on a
d = Signal.map2(b, c, |x, y| x + y)  # depends on b and c
```

When `a` changes to 2:
```
a changes (1 → 2)
  → mark b dirty
  → mark c dirty
  → evaluate b (becomes 3)
  → evaluate c (becomes 4)
  → mark d dirty
  → evaluate d (becomes 7)
  → batch DOM updates

d NEVER sees (b=3, c=2) or (b=2, c=4) - only consistent states
```

### Push-Pull Hybrid Model

The platform uses a hybrid push-pull evaluation strategy:

| Phase | Model | Description |
|-------|-------|-------------|
| Invalidation | Push | Source changes immediately mark dependents "dirty" |
| Evaluation | Pull | Dirty signals are recomputed lazily when needed |
| Batching | - | Multiple changes per frame are batched; DOM updates once at end |

This combines the efficiency of push (only dirty nodes tracked) with the consistency of pull (no redundant computation).

### Graph Node Types

| Type | Platform Behavior |
|------|-------------------|
| `Signal.const` | Static value, never updates |
| `Signal.map` | Recompute when source changes |
| `Signal.map2` | Recompute when either source changes |
| `Signal.fold` | Update when event fires, accumulate |
| `Signal.zip_with` | Sample signal when event fires |
| `Event.init` | Entry point for external events |
| `Signal.switch` | Create/destroy subgraphs on value change |
| `Signal.each` | Maintain keyed pool of subgraphs |
| `element` | Update DOM properties when signal changes |

### Why This Is Fast

| Virtual DOM | Signals |
|-------------|---------|
| Re-render component tree | Only affected signals update |
| Diff old tree vs new tree | No diffing needed |
| Patch DOM based on diff | Direct DOM updates |
| O(tree size) | O(affected nodes) |

### Retained Mode Mapping

This architecture maps well to retained-mode graphics APIs:

| Platform | How It Maps |
|----------|-------------|
| DOM | Elements are persistent objects; signals update properties |
| iOS UIKit | UIViews are retained; signals update properties |
| Android Views | Views are retained; signals update properties |
| Native toolkits (GTK, Qt) | Widgets are retained; signals update properties |

The platform creates objects once and updates their properties—exactly what retained-mode APIs are designed for.

---

## Summary

### Key Primitives

| Primitive | Type | Purpose |
|-----------|------|---------|
| `Signal.const` | `a -> Signal(a)` | Static value |
| `Signal.map` | `Signal(a), (a -> b) -> Signal(b)` | Transform signal |
| `Signal.map2` | `Signal(a), Signal(b), (a,b -> c) -> Signal(c)` | Combine signals |
| `Signal.hold` | `a, Event(a) -> Signal(a)` | Event → Signal |
| `Signal.fold` | `a, Event(e), (a,e -> a) -> Signal(a)` | Accumulate events |
| `Signal.zip_with` | `Signal(a), Event(e), (a,e -> a) -> Signal(a)` | Sample + fold |
| `Signal.changes` | `Signal(a) -> Event(a)` | Signal → Event |
| `Event.init` | `{} -> (EventSender(a), Event(a))` | Create channel |
| `Event.map` | `Event(a), (a -> b) -> Event(b)` | Transform events |
| `Event.filter` | `Event(a), (a -> Bool) -> Event(a)` | Filter events |
| `Event.merge` | `Event(a), Event(a) -> Event(a)` | Combine streams |
| `Event.gate` | `Event(a), Signal(Bool) -> Event(a)` | Filter by signal |
| `Event.with_latest` | `Event(e), Signal(a) -> Event({event:e, value:a})` | Attach signal value |
| `Event.debounce` | `Event(a), U64 -> Event(a)` | Debounce |
| `Event.throttle` | `Event(a), U64 -> Event(a)` | Throttle |
| `Signal.when` | `Signal(Bool), Elem -> Elem` | Conditional rendering |
| `Signal.switch` | `Signal(a), (a -> Elem) -> Elem` | Multi-branch rendering |
| `Signal.each` | `Signal(List(a)), (a -> Str), (Signal(a) -> Elem) -> Elem` | Dynamic lists |
| `Elem.translate` | `(Signal(c) -> Elem), (p -> c), (p, c -> p) -> (Signal(p) -> Elem)` | Parent-child state wiring |

### Core Ideas

1. **Signals vs Events**: Continuous values vs discrete occurrences—a fundamental distinction
2. **Render runs once**: No re-rendering, no diffing
3. **Dependency graph**: Updates propagate to exactly what's affected
4. **Glitch-free**: Consistent states guaranteed, no intermediate garbage
5. **Channels separate concerns**: Senders for producing, events for consuming
6. **zip_with bridges events and signals**: Sample current values when events fire
7. **Record Builder for composition**: Combine N signals elegantly
8. **Elem.translate for components**: Type-safe parent-child relationships with simple lens-style API
9. **Errors are data**: Use `Try` and pattern matching, not exception boundaries

### The Mental Model

```
User Interactions / Timers / HTTP
              │
              ▼
         Events (discrete occurrences)
              │
              ├── Event.map/filter/merge ──▶ Transformed events
              │
              ├── Signal.hold/fold ────────▶ Signals (continuous)
              │                                    │
              │                                    ├── Signal.map/map2 ──▶ Derived signals
              │                                    │
              └── Signal.zip_with ─────────────────┘
                                                   │
                                                   ├── Signal.switch/each ──▶ Dynamic structure
                                                   │
                                                   ▼
                                          DOM Elements (surgical updates)
```

This is the signal-based UI architecture for Roc: declarative, type-safe, and efficient.

---

## Implementation Notes

### Current Implementation Status

The core types are implemented and type-check in `test/signals/platform/`:

| File | Status | Description |
|------|--------|-------------|
| `Signal.roc` | ✓ Working | Signal type with phantom parameter, type-specific methods |
| `Event.roc` | ✓ Working | Event type with phantom parameter, channel creation |
| `EventSender.roc` | ✓ Working | Write-only event handle |
| `SignalNode.roc` | ✓ Working | Type-erased signal graph nodes |
| `EventNode.roc` | ✓ Working | Type-erased event graph nodes |
| `NodeValue.roc` | ✓ Working | Universal value container |
| `Elem.roc` | ✓ Working | UI element tree |
| `app.roc` | ✓ Working | Counter example application |

### Record Wrapper Pattern for Phantom Types

The new Zig-based Roc compiler requires a special pattern for structural lifting with phantom type parameters. Instead of:

```roc
Signal(a) := SignalNode  # Doesn't work - structural lifting fails
```

We use:

```roc
Signal(a) := { node : SignalNode }  # Works - record literals auto-lift
```

Methods then return record literals which structurally lift correctly:

```roc
const_i64 : I64 -> Signal(I64)
const_i64 = |value| {
    node: SignalNode.make_const(NodeValue.from_i64(value)),
}
```

### Syntax Notes for New Compiler

The new Zig-based compiler uses different syntax:

| Feature | Old Syntax | New Syntax |
|---------|------------|------------|
| Pattern matching | `when x is` | `match x {` |
| Pattern arm | `Pattern -> body` | `Pattern => body` |
| Unreachable | `crash("msg")` | `...` |
| Type application | `List U8` | `List(U8)` |

### Polymorphic vs Type-Specific Methods

The current implementation uses type-specific methods (e.g., `Signal.const_i64`, `Signal.map_i64_to_str`) rather than polymorphic ones. This is a pragmatic choice that works with the current compiler. Once where-clause constraints are fully supported, these can evolve to polymorphic versions:

```roc
# Current (works now)
Signal.const_i64 : I64 -> Signal(I64)
Signal.const_str : Str -> Signal(Str)

# Future (with where clauses)
Signal.const : a -> Signal(a)
    where [a.encode : a, NodeValue -> Try(NodeValue, [])]
```

---

## File Reference

| File | Purpose |
|------|---------|
| `app.roc` | Example counter application |
| `platform/main.roc` | Platform entry point |
| `platform/Signal.roc` | Signal type with phantom parameter |
| `platform/Event.roc` | Event type with phantom parameter |
| `platform/EventSender.roc` | Write-only event handle |
| `platform/SignalNode.roc` | Type-erased signal graph nodes |
| `platform/EventNode.roc` | Type-erased event graph nodes |
| `platform/NodeValue.roc` | Universal value container |
| `platform/Elem.roc` | UI element tree |
| `signal-conceptual-design.md` | This document - concepts and API design |
| `detailed-design-pure.md` | Implementation architecture and Zig host design |
