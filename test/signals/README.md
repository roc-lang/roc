# Signals Test Platform

This directory contains a small Roc platform that demonstrates a signal-based UI architecture with controlled child components.

The host is intentionally a simulated DOM and test runner. It is not a UI framework, but it exercises the same machinery a real UI framework would need: event sources, state signals, derived signals, component composition, and targeted text updates without rerendering the whole tree.

## Running

From the repository root:

```sh
zig build run-test-signals
```

The default spec is `test_counter.txt`, which simulates clicks and checks text content in the host DOM.

The build step compiles the demo with the LLVM backend and debug info so host/app failures can be inspected with native debugging tools.

To only rebuild the host archive along with the other test platforms:

```sh
zig build build-test-hosts -Dplatform=signals
```

## App API

Apps import:

```roc
import pf.Elem
import pf.Reactive
```

The public reactive API is nested under `Reactive`:

- `Reactive.Signal(a)` is a continuous value with a current value.
- `Reactive.Event(a)` is a discrete stream of occurrences.
- `Reactive.EventSender(a)` is the write handle used by UI elements.
- `Reactive.Codec(a)` explicitly describes how values cross the Roc/host graph boundary.

The current demo uses explicit codecs because graph nodes store values as `NodeValue`. App authors define codecs for their state types:

```roc
Counter := { count : I64 }.{
    codec : Reactive.Codec(Counter)
    codec = Reactive.Codec.make(Counter.encode, Counter.decode)

    encode : Counter -> NodeValue
    decode : NodeValue -> Try(Counter, [TypeMismatch])
}
```

Primitive codecs are provided as `Reactive.Codec.unit`, `Reactive.Codec.i64`, and `Reactive.Codec.str`.

## Components

A component is:

```roc
Elem.Component(a) := {
    elem : Elem,
    changes : Reactive.Event(a),
}
```

`elem` is the static element tree to walk once at startup. `changes` emits the next complete component state when local events occur.

The demo counter is reusable because it only knows about `Counter` state:

```roc
Counter.render! : Reactive.Signal(Counter) => Elem.Component(Counter)
```

It creates local click channels, samples the latest counter state when a button is clicked, and emits the next `Counter` value.

## Lens-Style Translate

`Elem.translate` adapts a controlled child component to parent-owned state:

```roc
Elem.translate(
    App.codec,
    Counter.codec,
    Counter.render!,
    |model| model.left,
    |model, counter| App.make(counter, model.right),
)
```

The getter projects `Signal(App)` into `Signal(Counter)`. The setter lifts child changes back into `Event(App)`. The parent merges translated child changes and passes them to `Elem.run_component!`, which updates the root state signal.

The result is the same counter component rendered twice against different fields of `App`.

## Host Model

The host stores:

- graph nodes for events and signals,
- simulated DOM elements,
- click bindings from buttons to event nodes,
- text bindings from labels/buttons to signal nodes.

`Elem.run_component!` creates one mutable root state signal. When a bound event fires, the host propagates through the graph, updates the root state from component changes, recomputes dependent signals, and updates text bindings.

The implemented graph node kinds include:

- event source, map, filter, merge, and with-latest,
- signal const, state, map, hold, fold, and zip-with.

## Extending to a Real UI Framework

A real renderer would replace the simulated DOM host effects with platform-specific effects:

- create/append/remove/move real UI nodes,
- bind real input events to `EventSender`s,
- schedule propagation on the UI thread,
- add lifecycle cleanup for graph nodes and event bindings,
- add typed element APIs beyond buttons, labels, and text,
- add dynamic structure such as keyed lists and conditional children.

The app-facing composition model can stay the same: components receive `Signal(state)`, emit `Event(next_state)`, and use `Elem.translate` to connect child state to parent state through explicit getter/setter functions.
