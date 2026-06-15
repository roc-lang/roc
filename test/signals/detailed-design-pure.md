# Signal-Based UI Platform - Pure Graph + Walk Design

> **Implementation Status**: Core types implemented in `test/signals/platform/`.
> The app type-checks successfully with polymorphic where-clause methods.
> Host (Zig) and Walker not yet implemented.
>
> **Known Limitation**: Unit type `{}` doesn't have built-in encode/decode, requiring
> type-specific methods like `Event.map_unit_to_i64` for unit-typed events.

## Overview

This document describes an implementation architecture for a signal-based reactive UI platform in Roc. The key design principle is:

**User code is pure; the platform walks the resulting graph to register with the host.**

```
┌─────────────────────────────────────────────────────────────────────────┐
│                         USER'S APP (Pure)                               │
│                                                                         │
│  main : {} -> Elem                                                      │
│  main = |{}| {                                                          │
│      { sender: click, receiver: click_recv } = Event.channel()          │
│      count = Signal.fold(0, click_recv, |acc, _| acc + 1)               │
│      count_str = count.map(I64.to_str)                                  │
│      div([button({ on_click: click, label: Signal.const("-") }),        │
│           label(count_str)])                                            │
│  }                                                                      │
│                                                                         │
│  Returns: Elem tree containing Signal/Event graphs with boxed closures  │
└─────────────────────────────────────────────────────────────────────────┘
                                   │
                                   ▼
┌─────────────────────────────────────────────────────────────────────────┐
│                      PLATFORM (Effectful Walk)                          │
│                                                                         │
│  main_for_host! : {} => {}                                              │
│  main_for_host! = |{}| {                                                │
│      elem = main({})           # Call user's pure main                  │
│      root = Host.create_root!({})                                       │
│      Walker.walk_elem!(elem, root)                                      │
│  }                                                                      │
│                                                                         │
│  Walker traverses the Elem tree:                                        │
│    - Creates host graph nodes                                           │
│    - Passes boxed closures to host for storage                          │
│    - Creates DOM elements and bindings                                  │
└─────────────────────────────────────────────────────────────────────────┘
                                   │
                                   ▼
┌─────────────────────────────────────────────────────────────────────────┐
│                            HOST (Zig)                                   │
│                                                                         │
│  Stores:                                                                │
│    - Graph nodes with dependencies                                      │
│    - Boxed closures (received from Roc, stored opaquely)                │
│    - Current values as NodeValue                                        │
│    - DOM bindings                                                       │
│                                                                         │
│  On events:                                                             │
│    - Propagate through graph                                            │
│    - Call Roc entrypoints with stored closures                          │
│    - Update DOM                                                         │
└─────────────────────────────────────────────────────────────────────────┘
```

---

## Design Principles

### 1. Closures Live in Host, Not Roc

Roc has no mutable global state. We cannot store closures in a Roc registry.

**Solution**: Box closures during graph construction and pass them to the host. The host stores them and passes them back to Roc for evaluation.

### 2. Type Erasure at the Boundary

Roc's type system doesn't support existential types. We can't store `Signal(I64)` and `Signal(Str)` in the same collection while preserving their types.

**Solution**: Use `NodeValue` as a universal container at the Roc/Host boundary. Closures are wrapped to work on `NodeValue`, using the encode/decode pattern with where clause method constraints.

### 3. Pure User API

The user's `main` function is pure. All graph construction happens by building data structures. Effects only occur in the platform's walk phase.

### 4. Channels Connect UI to Events

UI elements (buttons, inputs) need to fire events. The `Event.channel()` function returns a sender/receiver pair:
- **EventSender** goes to UI elements (button's `on_click`)
- **Event** is used in signal logic (folded into state)

---

## Type Definitions

### NodeValue - Universal Value Container

```roc
## Universal value type for crossing the Roc/Host boundary.
## All signal and event values are encoded to this format.
NodeValue := [
    NvI64(I64),
    NvStr(Str),
    NvBool(Bool),
    NvUnit,
    NvF64(F64),
    NvList(List(NodeValue)),
    NvRecord(List({ key : Str, value : NodeValue })),
].{
    ## Format instance for encode/decode pattern
    format : NodeValue
    format = NvUnit

    ## Encode an I64 to NodeValue
    encode_i64 : NodeValue, I64 -> Try(NodeValue, [])
    encode_i64 = |_, n| Ok(NvI64(n))

    ## Decode an I64 from NodeValue
    decode_i64 : NodeValue, NodeValue -> (Try(I64, [TypeMismatch]), NodeValue)
    decode_i64 = |_fmt, nv| {
        match nv {
            NvI64(n) => (Ok(n), nv)
            _ => (Err(TypeMismatch), nv)
        }
    }

    ## Encode a Str to NodeValue
    encode_str : NodeValue, Str -> Try(NodeValue, [])
    encode_str = |_fmt, s| Ok(NvStr(s))

    ## Decode a Str from NodeValue
    decode_str : NodeValue, NodeValue -> (Try(Str, [TypeMismatch]), NodeValue)
    decode_str = |_fmt, nv| {
        match nv {
            NvStr(s) => (Ok(s), nv)
            _ => (Err(TypeMismatch), nv)
        }
    }

    ## Encode a Bool to NodeValue
    encode_bool : NodeValue, Bool -> Try(NodeValue, [])
    encode_bool = |_fmt, b| Ok(NvBool(b))

    ## Decode a Bool from NodeValue
    decode_bool : NodeValue, NodeValue -> (Try(Bool, [TypeMismatch]), NodeValue)
    decode_bool = |_fmt, nv| {
        match nv {
            NvBool(b) => (Ok(b), nv)
            _ => (Err(TypeMismatch), nv)
        }
    }

    ## Encode an F64 to NodeValue
    encode_f64 : NodeValue, F64 -> Try(NodeValue, [])
    encode_f64 = |_fmt, f| Ok(NvF64(f))

    ## Decode an F64 from NodeValue
    decode_f64 : NodeValue, NodeValue -> (Try(F64, [TypeMismatch]), NodeValue)
    decode_f64 = |_fmt, nv| {
        match nv {
            NvF64(f) => (Ok(f), nv)
            _ => (Err(TypeMismatch), nv)
        }
    }

    ## Encode unit to NodeValue
    ## Note: Unit type {} doesn't get automatic encode/decode from built-in abilities
    encode_unit : NodeValue, {} -> Try(NodeValue, [])
    encode_unit = |_fmt, {}| Ok(NvUnit)

    ## Decode unit from NodeValue
    decode_unit : NodeValue, NodeValue -> (Try({}, [TypeMismatch]), NodeValue)
    decode_unit = |_fmt, nv| {
        match nv {
            NvUnit => (Ok({}), nv)
            _ => (Err(TypeMismatch), nv)
        }
    }

    ## Helper: Extract I64 from NodeValue (crashes on type mismatch)
    to_i64 : NodeValue -> I64
    to_i64 = |nv| {
        match nv {
            NvI64(n) => n
            _ => ...
        }
    }

    ## Helper: Extract Str from NodeValue (crashes on type mismatch)
    to_str : NodeValue -> Str
    to_str = |nv| {
        match nv {
            NvStr(s) => s
            _ => ...
        }
    }

    ## Helper: Create NodeValue from I64
    from_i64 : I64 -> NodeValue
    from_i64 = |n| NvI64(n)

    ## Helper: Create NodeValue from Str
    from_str : Str -> NodeValue
    from_str = |s| NvStr(s)
}
```

### Boxed Closure Types

```roc
## A unary transform: NodeValue -> NodeValue
Transform : Box(NodeValue -> NodeValue)

## A binary step function for folds: (NodeValue, NodeValue) -> NodeValue
Step : Box((NodeValue, NodeValue) -> NodeValue)

## A predicate for filtering: NodeValue -> Bool
Predicate : Box(NodeValue -> Bool)

## A builder function for dynamic rendering: NodeValue -> Elem
Builder : Box(NodeValue -> Elem)

## A key function for list reconciliation: NodeValue -> Str
KeyFn : Box(NodeValue -> Str)

## An item builder for Signal.each: NodeId -> Elem
ItemBuilder : Box(NodeId -> Elem)
```

### SignalNode - Internal Non-Parametric Type

```roc
## Internal representation of a signal node (type-erased).
## All values are stored as NodeValue; closures are boxed.
SignalNode := [
    Const(NodeValue),
    Map({ source : SignalNode, transform : Transform }),
    Map2({ source1 : SignalNode, source2 : SignalNode, transform : Transform }),
    Hold({ initial : NodeValue, event : EventNode }),
    Fold({ initial : NodeValue, event : EventNode, step : Step }),
    ZipWith({ source : SignalNode, event : EventNode, combine : Step }),
]
```

### EventNode - Internal Non-Parametric Type

```roc
## Internal representation of an event node (type-erased).
## All values are stored as NodeValue; closures are boxed.
EventNode := [
    Source,
    Map({ source : EventNode, transform : Transform }),
    Filter({ source : EventNode, predicate : Predicate }),
    Merge({ left : EventNode, right : EventNode }),
    Changes({ signal : SignalNode }),
    Gate({ source : EventNode, condition : SignalNode }),
    WithLatest({ event : EventNode, signal : SignalNode }),
]
```

---

## Phantom Types for Type Safety

The user-facing `Signal(a)` and `Event(a)` types are phantom type wrappers around non-parametric internal types. This provides compile-time type safety while allowing type erasure at runtime.

```
┌─────────────────────────────────────────────────────────────────┐
│  User-facing API (compile-time type safety)                     │
│                                                                 │
│  Signal(I64)  ──┐                                               │
│  Signal(Str)  ──┼──►  SignalNode (runtime, type-erased)         │
│  Signal(Bool) ──┘                                               │
│                                                                 │
│  Event(I64)   ──┐                                               │
│  Event({})    ──┼──►  EventNode (runtime, type-erased)          │
│  Event(Str)   ──┘                                               │
│                                                                 │
│  EventSender(I64) ──┐                                           │
│  EventSender({})   ──┼──►  EventNode.Source (runtime)           │
│  EventSender(Str)  ──┘                                          │
└─────────────────────────────────────────────────────────────────┘
```

The phantom type parameter `a` is never stored - it exists only to enforce type compatibility at compile time.

### Record Wrapper Pattern (Implementation Detail)

The new Zig compiler requires a record wrapper for structural lifting to work with phantom types:

```roc
# Actual implementation uses record wrappers:
Signal(a) := { node : SignalNode }
Event(a) := { node : EventNode }
EventSender(a) := { node : EventNode }
```

This allows methods to return record literals that auto-lift to the phantom-typed wrapper. See `platform/Signal.roc` for the working implementation.

---

## Signal API

> **Implementation**: See `platform/Signal.roc` for the current working implementation.

```roc
## A Signal represents a time-varying value.
## Uses record wrapper pattern for structural lifting to work.
Signal(a) := { node : SignalNode }.{
    ## Create a constant signal that never changes
    const : a -> Signal(a)
        where [a.encode : a, NodeValue -> Try(NodeValue, [])]
    const = |value| {
        A : a
        result = A.encode(value, NodeValue.format)
        nv =
            match result {
                Ok(encoded) => encoded
            }
        { node: SignalNode.make_const(nv) }
    }

    ## Transform a signal's values using a function
    map : Signal(a), (a -> b) -> Signal(b)
        where [
            a.decode : NodeValue, NodeValue -> (Try(a, [TypeMismatch]), NodeValue),
            b.encode : b, NodeValue -> Try(NodeValue, []),
        ]
    map = |signal, f| {
        A : a
        B : b
        source = signal.node
        wrapped : NodeValue -> NodeValue
        wrapped = |input_nv| {
            (decode_result, _remainder) = A.decode(NodeValue.format, input_nv)
            typed_input =
                match decode_result {
                    Ok(val) => val
                    Err(_) => ...
                }
            typed_output = f(typed_input)
            encode_result = B.encode(typed_output, NodeValue.format)
            match encode_result {
                Ok(output_nv) => output_nv
            }
        }

        { node: SignalNode.make_map_signal(source, Box.box(wrapped)) }
    }

    ## Combine two signals using a function
    ## This signature enables Record Builder: { a: sig_a, b: sig_b }.Signal
    map2 : Signal(a), Signal(b), (a, b -> c) -> Signal(c)
        where [
            a.decode : NodeValue, NodeValue -> (Try(a, [TypeMismatch, ..]), NodeValue),
            b.decode : NodeValue, NodeValue -> (Try(b, [TypeMismatch, ..]), NodeValue),
            c.encode : c, NodeValue -> Try(NodeValue, []),
        ]
    map2 = |sig_a, sig_b, f| {
        ## Host packs two values as Record with fields "a" and "b"
        wrapped : NodeValue -> NodeValue
        wrapped = |packed_nv| {
            A : a
            B : b
            C : c
            match packed_nv {
                NvRecord(fields) -> {
                    a_nv = fields
                        .keep_if(|field| field.key == "a")
                        .first()
                        .map(|field| field.value)
                        .with_default(NvUnit)
                    b_nv = fields
                        .keep_if(|field| field.key == "b")
                        .first()
                        .map(|field| field.value)
                        .with_default(NvUnit)

                    (a_result, _) = A.decode(a_nv, NodeValue.format)
                    (b_result, _) = B.decode(b_nv, NodeValue.format)

                    match (a_result, b_result) {
                        (Ok(typed_a), Ok(typed_b)) -> {
                            typed_output = f(typed_a, typed_b)
                            match C.encode(typed_output, NodeValue.format) {
                                Ok(output_nv) -> output_nv
                                Err(_) -> crash("Signal.map2: encode failed")
                            }
                        }
                        _ -> crash("Signal.map2: decode failed")
                    }
                }
                _ -> crash("Signal.map2: expected Record")
            }
        }

        Map2({
            source1: sig_a,
            source2: sig_b,
            transform: Box.box(wrapped),
        })
    }

    ## Hold the most recent event value as a signal
    hold : a, Event(a) -> Signal(a)
        where [a.encode : a, NodeValue -> Try(NodeValue, [])]
    hold = |initial, event| {
        A : a
        initial_nv = match A.encode(initial, NodeValue.format) {
            Ok(nv) -> nv
            Err(_) -> crash("Signal.hold: encode failed")
        }

        Hold({
            initial: initial_nv,
            event: event,
        })
    }

    ## Fold events into a signal using an accumulator
    fold : a, Event(e), (a, e -> a) -> Signal(a)
        where [
            a.encode : a, NodeValue -> Try(NodeValue, []),
            a.decode : NodeValue, NodeValue -> (Try(a, [TypeMismatch]), NodeValue),
            e.decode : NodeValue, NodeValue -> (Try(e, [TypeMismatch]), NodeValue),
        ]
    fold = |initial, event, step_fn| {
        A : a
        E : e
        initial_result = A.encode(initial, NodeValue.format)
        initial_nv =
            match initial_result {
                Ok(nv) => nv
            }
        event_node = Event.to_node(event)
        wrapped : (NodeValue, NodeValue) -> NodeValue
        wrapped = |(acc_nv, evt_nv)| {
            (acc_result, _) = A.decode(NodeValue.format, acc_nv)
            (evt_result, _) = E.decode(NodeValue.format, evt_nv)
            acc =
                match acc_result {
                    Ok(val) => val
                    Err(_) => ...
                }
            evt =
                match evt_result {
                    Ok(val) => val
                    Err(_) => ...
                }
            new_acc = step_fn(acc, evt)
            encode_result = A.encode(new_acc, NodeValue.format)
            match encode_result {
                Ok(nv) => nv
            }
        }

        {
            node: SignalNode.make_fold(
                initial_nv,
                event_node,
                Box.box(wrapped),
            ),
        }
    }

    ## Sample signal when event fires, apply combine function
    ## Different from fold: rebases when source signal changes externally
    zip_with : Signal(a), Event(e), (a, e -> b) -> Signal(b)
        where [
            a.decode : NodeValue, NodeValue -> (Try(a, [TypeMismatch, ..]), NodeValue),
            e.decode : NodeValue, NodeValue -> (Try(e, [TypeMismatch, ..]), NodeValue),
            b.encode : b, NodeValue -> Try(NodeValue, []),
        ]
    zip_with = |signal, event, combine| {
        A : a
        E : e
        B : b

        wrapped_combine : (NodeValue, NodeValue) -> NodeValue
        wrapped_combine = |signal_nv, event_nv| {
            (sig_result, _) = A.decode(signal_nv, NodeValue.format)
            (evt_result, _) = E.decode(event_nv, NodeValue.format)

            match (sig_result, evt_result) {
                (Ok(typed_sig), Ok(typed_evt)) -> {
                    result = combine(typed_sig, typed_evt)
                    match B.encode(result, NodeValue.format) {
                        Ok(nv) -> nv
                        Err(_) -> crash("Signal.zip_with: encode failed")
                    }
                }
                _ -> crash("Signal.zip_with: decode failed")
            }
        }

        ZipWith({
            source: signal,
            event: event,
            combine: Box.box(wrapped_combine),
        })
    }

    ## Convert signal changes to an event stream
    changes : Signal(a) -> Event(a)
    changes = |signal| Changes({ signal: signal })

    ## Get the inner SignalNode (for platform use)
    ## Note: Since Signal(a) := SignalNode, this is just the identity function
    to_node : Signal(a) -> SignalNode
    to_node = |signal| signal

    ## Create from inner SignalNode (for platform use)
    ## Note: Since Signal(a) := SignalNode, this is just the identity function
    from_node : SignalNode -> Signal(a)
    from_node = |node| node
}
```

---

## Event API

```roc
## EventSender is the write-only handle for firing events.
## Given to UI elements that produce events (buttons, inputs).
EventSender(a) := EventNode.{
    ## Get the inner EventNode (for platform use)
    ## Note: Since EventSender(a) := EventNode, this is just the identity function
    to_node : EventSender(a) -> EventNode
    to_node = |sender| sender
}

## An Event represents discrete occurrences over time.
## Uses record wrapper pattern for structural lifting to work.
Event(a) := { node : EventNode }.{
    ## Create a channel - returns sender/receiver pair
    ## The sender goes to UI elements; the receiver is used in signal logic
    ##
    ## Usage:
    ##   { sender, receiver } = Event.channel({})
    ##   button({ on_click: sender }, Signal.const("Click"))
    ##   count = Signal.fold(0, receiver, |c, _| c + 1)
    channel : {} -> { sender : EventSender(a), receiver : Event(a) }
    channel = |{}| {
        event_node = EventNode.make_source({})
        {
            sender: { node: event_node },
            receiver: { node: event_node },
        }
    }

    ## Transform event payloads using a function
    ## Note: Unit type {} doesn't have built-in encode/decode - use map_unit_to_* methods
    map : Event(a), (a -> b) -> Event(b)
        where [
            a.decode : NodeValue, NodeValue -> (Try(a, [TypeMismatch]), NodeValue),
            b.encode : b, NodeValue -> Try(NodeValue, []),
        ]
    map = |event, f| {
        A : a
        B : b
        source = event.node
        wrapped : NodeValue -> NodeValue
        wrapped = |input_nv| {
            (decode_result, _) = A.decode(NodeValue.format, input_nv)
            typed_input =
                match decode_result {
                    Ok(val) => val
                    Err(_) => ...
                }
            typed_output = f(typed_input)
            encode_result = B.encode(typed_output, NodeValue.format)
            match encode_result {
                Ok(output_nv) => output_nv
            }
        }

        { node: EventNode.make_map_event(source, Box.box(wrapped)) }
    }

    ## Map unit event to I64 (workaround for unit type lacking encode/decode)
    map_unit_to_i64 : Event({}), ({} -> I64) -> Event(I64)
    map_unit_to_i64 = |event, f| {
        source = event.node
        wrapped : NodeValue -> NodeValue
        wrapped = |_nv| NodeValue.from_i64(f({}))

        { node: EventNode.make_map_event(source, Box.box(wrapped)) }
    }

    ## Filter events using a predicate
    filter : Event(a), (a -> Bool) -> Event(a)
        where [a.decode : NodeValue, NodeValue -> (Try(a, [TypeMismatch, ..]), NodeValue)]
    filter = |event, pred| {
        wrapped : NodeValue -> Bool
        wrapped = |input_nv| {
            A : a
            (result, _) = A.decode(input_nv, NodeValue.format)
            match result {
                Ok(typed_input) -> pred(typed_input)
                Err(_) -> Bool.False
            }
        }

        Filter({
            source: event,
            predicate: Box.box(wrapped),
        })
    }

    ## Merge two event streams - fires when either fires
    merge : Event(a), Event(a) -> Event(a)
    merge = |left, right| Merge({ left: left, right: right })

    ## Gate events by a boolean signal - only pass when signal is true
    gate : Event(a), Signal(Bool) -> Event(a)
    gate = |event, condition| Gate({
        source: event,
        condition: condition,
    })

    ## Attach current signal value to event when it fires
    with_latest : Event(e), Signal(a) -> Event({ event : e, value : a })
    with_latest = |event, signal| WithLatest({
        event: event,
        signal: signal,
    })

    ## Get the inner EventNode (for platform use)
    ## Note: Since Event(a) := EventNode, this is just the identity function
    to_node : Event(a) -> EventNode
    to_node = |event| event

    ## Create from inner EventNode (for platform use)
    ## Note: Since Event(a) := EventNode, this is just the identity function
    from_node : EventNode -> Event(a)
    from_node = |node| node
}
```

---

## Elem API

```roc
## UI Element tree containing signal and event graphs.
Elem := [
    Div({ children : List(Elem) }),
    Button({ on_click : EventSender({}), label : Signal(Str) }),
    Label({ text : Signal(Str) }),
    Input({ value : Signal(Str), on_input : EventSender(Str) }),
    Text(Str),
    When({ condition : SignalNode, child : Elem }),
    Switch({ source : SignalNode, builder : Builder }),
    Each({ source : SignalNode, key_fn : KeyFn, item_builder : ItemBuilder }),
    Translate({
        child_render : Builder,
        getter : Transform,
        setter : Step,
        parent_signal : SignalNode,
    }),
]

## Create a div element
div : List(Elem) -> Elem
div = |children| Div({ children })

## Create a button element
button : { on_click : EventSender({}), label : Signal(Str) } -> Elem
button = |config| Button(config)

## Create a label element
label : Signal(Str) -> Elem
label = |text_signal| Label({ text: text_signal })

## Create a text input element
input : { value : Signal(Str), on_input : EventSender(Str) } -> Elem
input = |config| Input(config)

## Create a static text element
text : Str -> Elem
text = |s| Text(s)
```

---

## Dynamic Rendering

### Signal.when - Conditional Rendering

```roc
## Show or hide an element based on a boolean signal
Signal.when : Signal(Bool), Elem -> Elem
when = |condition, child| When({
    condition: condition,
    child: child,
})
```

When condition becomes `true`, the platform creates the child's subgraph and DOM.
When condition becomes `false`, the platform destroys the subgraph and removes DOM.

### Signal.switch - Multi-Branch Rendering

```roc
## Render different elements based on a signal's value
Signal.switch : Signal(a), (a -> Elem) -> Elem
    where [a.decode : NodeValue, NodeValue -> (Try(a, [TypeMismatch, ..]), NodeValue)]
switch = |signal, builder| {
    A : a

    wrapped_builder : NodeValue -> Elem
    wrapped_builder = |nv| {
        (result, _) = A.decode(nv, NodeValue.format)
        match result {
            Ok(typed_val) -> builder(typed_val)
            Err(_) -> crash("Signal.switch: decode failed")
        }
    }

    Switch({
        source: signal,
        builder: Box.box(wrapped_builder),
    })
}
```

When signal value changes, the platform:
1. Destroys the old branch's subgraph
2. Calls the builder with the new value
3. Creates the new branch's subgraph

### Signal.each - Dynamic Lists

```roc
## Render a dynamic list of elements
Signal.each : Signal(List(item)), (item -> Str), (Signal(item) -> Elem) -> Elem
    where [
        item.encode : item, NodeValue -> Try(NodeValue, []),
        item.decode : NodeValue, NodeValue -> (Try(item, [TypeMismatch, ..]), NodeValue),
    ]
each = |list_signal, key_fn, item_render| {
    Item : item

    wrapped_key_fn : NodeValue -> Str
    wrapped_key_fn = |nv| {
        (result, _) = Item.decode(nv, NodeValue.format)
        match result {
            Ok(typed_item) -> key_fn(typed_item)
            Err(_) -> crash("Signal.each: key decode failed")
        }
    }

    ## The item_builder receives a NodeId and wraps it in Signal(item)
    wrapped_item_builder : NodeId -> Elem
    wrapped_item_builder = |item_node_id| {
        item_signal : Signal(item) = Signal.from_node_id(item_node_id)
        item_render(item_signal)
    }

    Each({
        source: list_signal,
        key_fn: Box.box(wrapped_key_fn),
        item_builder: Box.box(wrapped_item_builder),
    })
}
```

The platform uses keys for efficient reconciliation:
- **Added items**: Create signal node, call builder, insert DOM
- **Removed items**: Destroy subgraph, remove DOM
- **Reordered items**: Move DOM nodes (no rebuild)
- **Updated items**: Update item signal's value

---

## Component Composition: Elem.translate

```roc
## Wire a child component to parent state with getter/setter
Elem.translate :
    (Signal(child) -> Elem),        # child's render function
    (parent -> child),               # getter: extract child from parent
    (parent, child -> parent)        # setter: update parent with new child
    -> (Signal(parent) -> Elem)
    where [
        parent.encode : parent, NodeValue -> Try(NodeValue, []),
        parent.decode : NodeValue, NodeValue -> (Try(parent, [TypeMismatch, ..]), NodeValue),
        child.encode : child, NodeValue -> Try(NodeValue, []),
        child.decode : NodeValue, NodeValue -> (Try(child, [TypeMismatch, ..]), NodeValue),
    ]
translate = |child_render, getter, setter| {
    Parent : parent
    Child : child

    ## Return a function that takes Signal(parent) and returns Elem
    |parent_signal| {
        wrapped_getter : NodeValue -> NodeValue
        wrapped_getter = |parent_nv| {
            (result, _) = Parent.decode(parent_nv, NodeValue.format)
            match result {
                Ok(typed_parent) -> {
                    child_val = getter(typed_parent)
                    match Child.encode(child_val, NodeValue.format) {
                        Ok(nv) -> nv
                        Err(_) -> crash("translate: getter encode failed")
                    }
                }
                Err(_) -> crash("translate: getter decode failed")
            }
        }

        wrapped_setter : (NodeValue, NodeValue) -> NodeValue
        wrapped_setter = |parent_nv, child_nv| {
            (p_result, _) = Parent.decode(parent_nv, NodeValue.format)
            (c_result, _) = Child.decode(child_nv, NodeValue.format)
            match (p_result, c_result) {
                (Ok(typed_parent), Ok(typed_child)) -> {
                    new_parent = setter(typed_parent, typed_child)
                    match Parent.encode(new_parent, NodeValue.format) {
                        Ok(nv) -> nv
                        Err(_) -> crash("translate: setter encode failed")
                    }
                }
                _ -> crash("translate: setter decode failed")
            }
        }

        wrapped_child_render : NodeValue -> Elem
        wrapped_child_render = |child_nv| {
            ## Create a Signal(child) from the NodeValue
            ## (Platform provides the actual signal node)
            child_signal : Signal(child) = Signal.from_node_value(child_nv)
            child_render(child_signal)
        }

        Translate({
            child_render: Box.box(wrapped_child_render),
            getter: Box.box(wrapped_getter),
            setter: Box.box(wrapped_setter),
            parent_signal: parent_signal,
        })
    }
}
```

---

## Host Interface

### Host Module

```roc
## Hosted effects - implemented by host (Zig), called by Roc.
## The host stores all state including boxed closures.

## Node ID returned by host
NodeId := U64

## Element ID returned by host
ElemId := U64

Host := [].{
    ## Create the root element container
    create_root! : {} => ElemId

    ## Create a DOM element by tag name
    create_element! : Str => ElemId

    ## Set element text content
    set_text! : ElemId, Str => {}

    ## Append child to parent
    append_child! : ElemId, ElemId => {}

    ## Create an event source node in the graph
    create_event_source! : {} => NodeId

    ## Create an event map node (host stores the transform closure)
    create_event_map! : NodeId, Transform => NodeId

    ## Create an event filter node (host stores the predicate closure)
    create_event_filter! : NodeId, Predicate => NodeId

    ## Create an event merge node
    create_event_merge! : NodeId, NodeId => NodeId

    ## Create an event gate node
    create_event_gate! : NodeId, NodeId => NodeId

    ## Create an event with_latest node
    create_event_with_latest! : NodeId, NodeId => NodeId

    ## Create a signal changes node (signal -> event)
    create_signal_changes! : NodeId => NodeId

    ## Create a constant signal node
    create_signal_const! : NodeValue => NodeId

    ## Create a signal map node (host stores the transform closure)
    create_signal_map! : NodeId, Transform => NodeId

    ## Create a signal map2 node (host stores the transform closure)
    create_signal_map2! : NodeId, NodeId, Transform => NodeId

    ## Create a signal hold node
    create_signal_hold! : NodeValue, NodeId => NodeId

    ## Create a signal fold node (host stores the step closure)
    create_signal_fold! : NodeValue, NodeId, Step => NodeId

    ## Create a signal zip_with node (host stores the combine closure)
    create_signal_zip_with! : NodeId, NodeId, Step => NodeId

    ## Bind a signal to an element's text content
    bind_text! : ElemId, NodeId => {}

    ## Bind click events to an event node
    bind_click! : ElemId, NodeId => {}

    ## Bind input events to an event node, value signal
    bind_input! : ElemId, NodeId, NodeId => {}

    ## Fire an event (for programmatic triggers)
    fire_event! : NodeId, NodeValue => {}
}
```

### Platform Entrypoints

```roc
platform ""
    requires {
        main : {} -> Elem
    }
    exposes [Signal, Event, Elem, NodeValue]
    packages {}
    provides {
        main_for_host!: "main",
        evaluate_transform!: "evaluate_transform",
        evaluate_step!: "evaluate_step",
        evaluate_predicate!: "evaluate_predicate",
        evaluate_builder!: "evaluate_builder",
    }
    targets: {
        files: "targets/",
        exe: {
            arm64mac: ["libhost.a", app],
        }
    }

import Host exposing [Host, NodeId, ElemId]
import Walker exposing [Walker]
import Elem exposing [Elem]

## Called by host at startup to build the UI
main_for_host! : {} => {}
main_for_host! = |{}| {
    elem = main({})
    root = Host.create_root!({})
    Walker.walk_elem!(elem, root)
}

## Called by host to evaluate a transform closure
evaluate_transform! : Transform, NodeValue => NodeValue
evaluate_transform! = |boxed_fn, input| {
    fn = Box.unbox(boxed_fn)
    fn(input)
}

## Called by host to evaluate a step closure (for fold, zip_with)
evaluate_step! : Step, NodeValue, NodeValue => NodeValue
evaluate_step! = |boxed_step, acc, event_val| {
    step = Box.unbox(boxed_step)
    step(acc, event_val)
}

## Called by host to evaluate a predicate closure (for filter)
evaluate_predicate! : Predicate, NodeValue => Bool
evaluate_predicate! = |boxed_pred, input| {
    pred = Box.unbox(boxed_pred)
    pred(input)
}

## Called by host to evaluate a builder closure (for switch, translate)
evaluate_builder! : Builder, NodeValue => Elem
evaluate_builder! = |boxed_builder, value| {
    builder = Box.unbox(boxed_builder)
    builder(value)
}
```

### Walker Module

```roc
## Walks the Elem tree and registers everything with the host.

Walker := [].{
    ## Walk an element tree, creating DOM and graph nodes
    walk_elem! : Elem, ElemId => {}
    walk_elem! = |elem, parent_id| {
        match elem {
            Div({ children }) -> {
                div_id = Host.create_element!("div")
                Host.append_child!(parent_id, div_id)

                for child in children {
                    Walker.walk_elem!(child, div_id)
                }
            }

            Button({ on_click, label }) -> {
                btn_id = Host.create_element!("button")
                Host.append_child!(parent_id, btn_id)

                label_node_id = Walker.walk_signal!(Signal.to_node(label))
                Host.bind_text!(btn_id, label_node_id)

                click_node_id = Walker.walk_event!(EventSender.to_node(on_click))
                Host.bind_click!(btn_id, click_node_id)
            }

            Label({ text }) -> {
                span_id = Host.create_element!("span")
                Host.append_child!(parent_id, span_id)

                text_node_id = Walker.walk_signal!(Signal.to_node(text))
                Host.bind_text!(span_id, text_node_id)
            }

            Input({ value, on_input }) -> {
                input_id = Host.create_element!("input")
                Host.append_child!(parent_id, input_id)

                value_node_id = Walker.walk_signal!(Signal.to_node(value))
                input_node_id = Walker.walk_event!(EventSender.to_node(on_input))
                Host.bind_input!(input_id, value_node_id, input_node_id)
            }

            Text(s) -> {
                span_id = Host.create_element!("span")
                Host.set_text!(span_id, s)
                Host.append_child!(parent_id, span_id)
            }

            When({ condition, child }) -> {
                ## Host manages show/hide based on condition
                container_id = Host.create_element!("div")
                Host.append_child!(parent_id, container_id)
                _cond_node_id = Walker.walk_signal!(condition)
                ## TODO: Host.bind_visibility!(container_id, cond_node_id)
                Walker.walk_elem!(child, container_id)
            }

            Switch({ source, builder }) -> {
                ## Host manages branch switching
                container_id = Host.create_element!("div")
                Host.append_child!(parent_id, container_id)
                ## TODO: Host.create_switch!(container_id, source, builder)
            }

            Each({ source, key_fn, item_builder }) -> {
                ## Host manages list reconciliation
                container_id = Host.create_element!("div")
                Host.append_child!(parent_id, container_id)
                ## TODO: Host.create_each!(container_id, source, key_fn, item_builder)
            }

            Translate({ child_render, getter, setter, parent_signal }) -> {
                ## Host manages bidirectional binding
                ## TODO: Host.create_translate!(parent_id, parent_signal, getter, setter, child_render)
            }
        }
    }

    ## Walk a signal graph, creating host nodes
    walk_signal! : SignalNode => NodeId
    walk_signal! = |signal| {
        match signal {
            Const(nv) ->
                Host.create_signal_const!(nv)

            Map({ source, transform }) -> {
                source_id = Walker.walk_signal!(source)
                Host.create_signal_map!(source_id, transform)
            }

            Map2({ source1, source2, transform }) -> {
                src1_id = Walker.walk_signal!(source1)
                src2_id = Walker.walk_signal!(source2)
                Host.create_signal_map2!(src1_id, src2_id, transform)
            }

            Hold({ initial, event }) -> {
                event_id = Walker.walk_event!(event)
                Host.create_signal_hold!(initial, event_id)
            }

            Fold({ initial, event, step }) -> {
                event_id = Walker.walk_event!(event)
                Host.create_signal_fold!(initial, event_id, step)
            }

            ZipWith({ source, event, combine }) -> {
                source_id = Walker.walk_signal!(source)
                event_id = Walker.walk_event!(event)
                Host.create_signal_zip_with!(source_id, event_id, combine)
            }
        }
    }

    ## Walk an event graph, creating host nodes
    walk_event! : EventNode => NodeId
    walk_event! = |event| {
        match event {
            Source ->
                Host.create_event_source!({})

            Map({ source, transform }) -> {
                source_id = Walker.walk_event!(source)
                Host.create_event_map!(source_id, transform)
            }

            Filter({ source, predicate }) -> {
                source_id = Walker.walk_event!(source)
                Host.create_event_filter!(source_id, predicate)
            }

            Merge({ left, right }) -> {
                left_id = Walker.walk_event!(left)
                right_id = Walker.walk_event!(right)
                Host.create_event_merge!(left_id, right_id)
            }

            Changes({ signal }) -> {
                signal_id = Walker.walk_signal!(signal)
                Host.create_signal_changes!(signal_id)
            }

            Gate({ source, condition }) -> {
                source_id = Walker.walk_event!(source)
                cond_id = Walker.walk_signal!(condition)
                Host.create_event_gate!(source_id, cond_id)
            }

            WithLatest({ event, signal }) -> {
                event_id = Walker.walk_event!(event)
                signal_id = Walker.walk_signal!(signal)
                Host.create_event_with_latest!(event_id, signal_id)
            }
        }
    }
}
```

---

## Zig Host Implementation (Pseudocode)

```zig
const std = @import("std");

// ============ Type Definitions ============

const NodeId = u64;
const ElemId = u64;

// NodeValue matches Roc's NodeValue type
const NodeValue = extern struct {
    tag: u8,
    payload: extern union {
        i64_val: i64,
        str_val: RocStr,
        bool_val: bool,
        f64_val: f64,
        unit: void,
        list_val: RocList,
        record_val: RocList,
    },
};

// Opaque handle to a boxed Roc closure
const RocBox = *anyopaque;

// ============ Graph Node Types ============

const NodeKind = union(enum) {
    // Events
    event_source: void,
    event_map: struct { source: NodeId, transform: RocBox },
    event_filter: struct { source: NodeId, predicate: RocBox },
    event_merge: struct { left: NodeId, right: NodeId },
    event_gate: struct { source: NodeId, condition: NodeId },
    event_with_latest: struct { event: NodeId, signal: NodeId },
    signal_changes: NodeId,

    // Signals
    signal_const: void,
    signal_map: struct { source: NodeId, transform: RocBox },
    signal_map2: struct { src1: NodeId, src2: NodeId, transform: RocBox },
    signal_hold: struct { event: NodeId },
    signal_fold: struct { event: NodeId, step: RocBox },
    signal_zip_with: struct { source: NodeId, event: NodeId, combine: RocBox },
};

const Node = struct {
    id: NodeId,
    kind: NodeKind,
    value: NodeValue,           // Current value
    dirty: bool,                // Needs recalculation
    dependents: []NodeId,       // Nodes that depend on this one
};

// ============ Graph State ============

const Graph = struct {
    nodes: std.ArrayList(Node),
    next_node_id: NodeId,
    next_elem_id: ElemId,
    dom_bindings: std.ArrayList(DomBinding),
    allocator: std.mem.Allocator,
    roc_ops: *RocOps,

    fn add_node(self: *Graph, kind: NodeKind, initial: NodeValue) NodeId {
        const id = self.next_node_id;
        self.next_node_id += 1;
        self.nodes.append(.{
            .id = id,
            .kind = kind,
            .value = initial,
            .dirty = false,
            .dependents = &.{},
        }) catch unreachable;
        return id;
    }

    fn add_dependency(self: *Graph, source: NodeId, dependent: NodeId) void {
        // source.dependents.append(dependent)
    }
};

// ============ Roc Entrypoints ============

extern fn roc_main_for_host(ops: *RocOps, ret: *anyopaque, args: *anyopaque) void;
extern fn roc_evaluate_transform(ops: *RocOps, ret: *NodeValue, args: *anyopaque) void;
extern fn roc_evaluate_step(ops: *RocOps, ret: *NodeValue, args: *anyopaque) void;
extern fn roc_evaluate_predicate(ops: *RocOps, ret: *bool, args: *anyopaque) void;

// ============ Event Propagation ============

fn fire_event(graph: *Graph, source_id: NodeId, payload: NodeValue) void {
    // 1. Set source node value
    graph.nodes.items[source_id].value = payload;
    graph.nodes.items[source_id].dirty = true;

    // 2. Collect dirty nodes in topological order
    var dirty_nodes = collect_dirty_nodes(graph, source_id);

    // 3. Evaluate each dirty node
    for (dirty_nodes) |node_id| {
        evaluate_node(graph, node_id);
    }

    // 4. Update DOM bindings
    update_dom_bindings(graph);
}

fn evaluate_node(graph: *Graph, node_id: NodeId) void {
    const node = &graph.nodes.items[node_id];

    switch (node.kind) {
        .event_source => {}, // Value already set by fire_event

        .event_map => |data| {
            const input = graph.nodes.items[data.source].value;
            node.value = call_roc_transform(graph, data.transform, input);
        },

        .event_filter => |data| {
            const input = graph.nodes.items[data.source].value;
            const passes = call_roc_predicate(graph, data.predicate, input);
            if (passes) {
                node.value = input;
            }
            // If predicate fails, event doesn't propagate
        },

        .signal_fold => |data| {
            const event_val = graph.nodes.items[data.event].value;
            const old_acc = node.value;
            node.value = call_roc_step(graph, data.step, old_acc, event_val);
        },

        .signal_zip_with => |data| {
            const signal_val = graph.nodes.items[data.source].value;
            const event_val = graph.nodes.items[data.event].value;
            node.value = call_roc_step(graph, data.combine, signal_val, event_val);
        },

        // ... other node types
    }

    node.dirty = false;
}

fn call_roc_transform(graph: *Graph, closure: RocBox, input: NodeValue) NodeValue {
    const Args = extern struct { closure: RocBox, input: NodeValue };
    var args = Args{ .closure = closure, .input = input };
    var result: NodeValue = undefined;
    roc_evaluate_transform(graph.roc_ops, &result, @ptrCast(&args));
    return result;
}

fn call_roc_step(graph: *Graph, closure: RocBox, acc: NodeValue, event: NodeValue) NodeValue {
    const Args = extern struct { closure: RocBox, acc: NodeValue, event: NodeValue };
    var args = Args{ .closure = closure, .acc = acc, .event = event };
    var result: NodeValue = undefined;
    roc_evaluate_step(graph.roc_ops, &result, @ptrCast(&args));
    return result;
}

fn call_roc_predicate(graph: *Graph, closure: RocBox, input: NodeValue) bool {
    const Args = extern struct { closure: RocBox, input: NodeValue };
    var args = Args{ .closure = closure, .input = input };
    var result: bool = undefined;
    roc_evaluate_predicate(graph.roc_ops, &result, @ptrCast(&args));
    return result;
}

// ============ Main Entry Point ============

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();

    var graph = Graph.init(allocator);

    // Initialize RocOps with hosted function pointers
    var roc_ops = RocOps{
        .env = @ptrCast(&graph),
        .hosted_fns = hosted_function_ptrs,
        // ... other callbacks
    };
    graph.roc_ops = &roc_ops;

    // Call Roc's main to build the UI
    roc_main_for_host(&roc_ops, null, null);

    // Event loop
    while (true) {
        // Handle UI events, fire into graph
        // fire_event(&graph, click_node_id, unit_value);
    }
}
```

---

## Complete Example Application

```roc
app [main] { pf: platform "./platform/main.roc" }

import pf.Signal exposing [Signal]
import pf.Event exposing [Event, EventSender]
import pf.Elem exposing [Elem, div, button, label]

## Counter component - reusable, encapsulated
Counter := { count : I64 }.{
    init : I64 -> Counter
    init = |n| { count: n }

    render : Signal(Counter) -> Elem
    render = |state| {
        ## Create event channels
        { sender: inc_send, receiver: inc_recv } = Event.channel()
        { sender: dec_send, receiver: dec_recv } = Event.channel()

        ## Merge and fold events into count
        all_deltas = Event.merge(
            inc_recv.map(|_| 1),
            dec_recv.map(|_| -1),
        )

        count = state.map(|c| c.count)
            .zip_with(all_deltas, |c, delta| c + delta)

        count_str = count.map(I64.to_str)

        div([
            button({ on_click: dec_send, label: Signal.const("-") }),
            label(count_str),
            button({ on_click: inc_send, label: Signal.const("+") }),
        ])
    }
}

## App with two counters using Elem.translate
App := { left : Counter, right : Counter }.{
    init : {} -> App
    init = |{}| {
        left: Counter.init(0),
        right: Counter.init(0),
    }

    render : Signal(App) -> Elem
    render = |state| {
        ## Wire up left counter to state.left
        left_counter = Elem.translate(
            Counter.render,
            |app| app.left,
            |app, counter| { ..app, left: counter },
        )

        ## Wire up right counter to state.right
        right_counter = Elem.translate(
            Counter.render,
            |app| app.right,
            |app, counter| { ..app, right: counter },
        )

        ## Derive total from both
        total = state.map(|app|
            app.left.count + app.right.count
        )

        div([
            label(Signal.const("Left:")),
            left_counter(state),
            label(Signal.const("Right:")),
            right_counter(state),
            label(total.map(|t| "Total: ${I64.to_str(t)}")),
        ])
    }
}

main : {} -> Elem
main = |{}| {
    ## Create app state signal
    initial_state = App.init({})
    state_signal = Signal.const(initial_state)

    App.render(state_signal)
}
```

---

## Data Flow Summary

### Startup Sequence

```
1. Host calls roc_main_for_host()
       │
       ▼
2. Platform calls user's pure main({})
       │
       ▼
3. User code builds Elem tree containing:
   - Signal(a) phantom wrappers around SignalNode (type-erased)
   - Event(a) phantom wrappers around EventNode (type-erased)
   - EventSender(a) for UI element connections
   - Boxed closures for transforms, steps, predicates
       │
       ▼
4. Platform's Walker.walk_elem!() traverses tree:
   - Pattern matches on SignalNode/EventNode variants
   - Creates host graph nodes via Host.create_*!()
   - Passes boxed closures to host for storage
   - Creates DOM elements and bindings
       │
       ▼
5. Host stores everything:
   - Graph nodes with dependencies
   - Boxed Roc closures (opaque pointers)
   - DOM bindings
```

### Event Fire Sequence

```
1. User clicks button
       │
       ▼
2. Host looks up event node from DOM binding
       │
       ▼
3. Host fires event: fire_event(node_id, payload)
       │
       ▼
4. Host propagates through graph (topological order):
   - For each node, calls roc_evaluate_*() with stored closure
   - Roc unboxes and invokes closure
   - Result stored in host's node.value
       │
       ▼
5. Host updates DOM bindings with new values
```

---

## Key Design Decisions

| Decision | Rationale |
|----------|-----------|
| **User API is pure** | Clean code, easy to test, no hidden state |
| **Phantom types for Signal/Event** | Compile-time type safety with runtime type erasure |
| **Structural unification** | Nominal types unify with their inner types - no explicit wrapping |
| **Separate Node types (SignalNode/EventNode)** | Non-parametric internal representation enables storage |
| **Closures boxed at construction** | Type erasure happens early, before reaching host |
| **Host stores boxed closures** | Avoids impossible mutable registry in Roc |
| **Encode/decode with where clauses** | Roc's NEW compiler pattern for polymorphism |
| **Type var aliases (`A : a`)** | Enables access to type variable methods (`A.encode(...)`) |
| **Event.channel() for UI connection** | Clean separation of sender (UI) and receiver (logic) |
| **Crash on encode/decode failure** | Errors are programming bugs, not recoverable |

---

## Future Extensions

### Additional NodeValue Types

```roc
NodeValue := [
    # Current
    NvI64(I64), NvStr(Str), NvBool(Bool), NvUnit, NvF64(F64),
    NvList(List(NodeValue)), NvRecord(List({ key : Str, value : NodeValue })),

    # Future
    NvDec(Dec),                              # Fixed-point decimal
    NvTuple2(NodeValue, NodeValue),
    NvTuple3(NodeValue, NodeValue, NodeValue),
]
```

### Timing Operations (Host-Implemented)

```roc
Event.debounce : Event(a), U64 -> Event(a)   # milliseconds
Event.throttle : Event(a), U64 -> Event(a)   # milliseconds
```

These require timer infrastructure in the host.

### HTTP Resources

```roc
Resource(a) : Signal([Idle, Loading, Success(a), Failure(Str)])

fetch : { trigger : Event(*), url : Signal(Str), decoder : ... } -> Resource(a)
```

---

## Trade-offs

| Pro | Con |
|-----|-----|
| Pure user code | Encode/decode overhead |
| No mutable Roc state | Complex `where` clauses |
| Host manages all runtime state | Boxed closure indirection |
| Extensible via encode/decode | Initial learning curve |
| Type-safe at compile time | Runtime type mismatches crash |
| Works with Roc's NEW type system | Verbose for simple cases |
| Clean channel pattern | Two types (Event/EventSender) for events |

---

## Syntax Notes for New Zig Compiler

The new Zig-based Roc compiler uses different syntax:

| Feature | Old Syntax | New Syntax |
|---------|------------|------------|
| Pattern matching | `when x is` | `match x {` |
| Pattern arm | `Pattern -> body` | `Pattern => body` |
| Unreachable | `crash("msg")` | `...` |
| Type application | `List U8` | `List(U8)` |

Example:
```roc
to_i64 = |nv| {
    match nv {
        NvI64(n) => n
        _ => ...
    }
}
```

---

## Implementation File Reference

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
