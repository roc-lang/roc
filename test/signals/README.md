# Signals Test Platform

This directory contains a small Roc platform that demonstrates how Roc can build a realistic signal-based UI framework.

The host is intentionally a simulated DOM and test runner. It does not draw pixels, but it exercises the same Roc machinery a real renderer needs: event channels, boxed callbacks, typed signal/event APIs in Roc, type-erased graph values in the host, scoped dynamic children, lifecycle events, keyed list reuse, and targeted text updates.

## Running

From the repository root:

```sh
zig build run-test-signals
```

The build step compiles `app.roc`, links it with the Zig host, and runs `test_counter.txt`. The spec simulates button clicks and checks the simulated DOM.

The host can also be rebuilt directly:

```sh
zig build build-test-hosts -Dplatform=signals
```

The generated glue lives in `platform/roc_platform_abi.zig`. Regenerate it after changing hosted effects or exposed platform types:

```sh
./zig-out/bin/roc glue src/glue/src/ZigGlue.roc test/signals/platform test/signals/platform/main.roc
```

## NodeValue Boundary Benchmarks

The host has a research benchmark mode for measuring the `NodeValue` boundary
without changing the production API:

```sh
zig build run-signals-bench
```

The built demo binary can also be run directly:

```sh
./zig-out/bin/signals-demo --bench --bench-iterations 1000
./zig-out/bin/signals-demo --bench --bench-iterations 5000 --bench-samples 3
./zig-out/bin/signals-demo --bench --bench-prototype A
```

Benchmark output is CSV. Each row records the prototype, case name, iteration
sample number, iteration count, wall time, operation count, Roc
allocations/deallocations, retained allocation delta, `NodeValue`
incref/decref/equality counts, synthetic boundary encode/decode counts,
callback count, event count, evaluated graph nodes, signal
writes/changes/suppressed writes, text updates, active graph nodes, and
scheduler counters for affected collection, readiness scans, node evaluation,
dynamic rendering, keyed rendering, text binding updates, and changed-signal
lookups.

The prototype rows are:

| Prototype | API impact | ABI impact | Ownership model | Callback shape | Equality strategy | `Elem.dynamic` / `Elem.each` |
| --- | --- | --- | --- | --- | --- | --- |
| A. Baseline `NodeValue` | Current typed Roc API | Current 32-byte tagged union | Host stores owned `NodeValue` copies | Boxed callbacks receive `NodeValue` | Recursive `NodeValue` equality | Supported today |
| B. Scalar fast paths | Typed Roc API can stay, but scalar nodes need explicit host paths | Adds typed primitive node/callback paths | Scalars are copied directly; generic values stay `NodeValue` | Primitive callbacks can use scalar arguments | Direct scalar equality; recursive fallback for lists | Dynamic/list paths still need generic `NodeValue` |
| C. Host value handles | Roc API may need handles or borrowed views at dynamic boundaries | Replaces hot values with host-owned handles | Host owns value storage and passes handles/views | Callbacks receive handles or borrowed views | Handle/version equality where valid | Needs explicit view support for dynamic and keyed list items |
| D. Generated boundary | Roc API can stay typed, with generated per-type glue | Adds per-type encode/decode/equality/callback glue | Ownership follows generated type-specific code | Callbacks receive generated app-model shapes | Generated per-type equality | Needs generated glue for dynamic/list item types |

Synthetic rows compare the four prototypes against identical microbenchmarks
and signal-graph shapes. Current-app scenario rows use the existing compiled
demo and report the production scalar/`NodeValue` hybrid rather than a pure
baseline. The generated boundary prototype includes separate fresh and reused
`BoundaryPayload` rows so allocation-dominated and retained-payload costs stay
visible. The benchmark mode first runs `test_counter.txt` and a focused
unchanged-value suppression check before printing measurements.

See [nodevalue_boundary_research.md](nodevalue_boundary_research.md) for the latest matrix and interpretation. The current implementation includes real scalar host paths for `I64`, `Bool`, and `Str`, generated boundary payload experiments, and generated Zig refcount helpers such as `decrefNodeValue`.

## Demo Coverage

`app.roc` is the acceptance demo. It covers:

- controlled parent/child composition with `Elem.translate`,
- independent local signal cells built with `Signal.fold`,
- `Signal.map2` over two independent signals,
- a diamond graph that must update topologically without stale inputs,
- event merge preserving both same-transaction occurrences,
- conditional dynamic structure with mount and unmount lifecycle events,
- keyed list reordering that preserves child-local state,
- keyed removal that tears down scoped nodes and sends unmount events,
- targeted text updates observed through `expect_updates` in the spec.

## Roc API

Apps import the platform modules through `pf`:

```roc
import pf.Elem
import pf.Reactive
import pf.NodeValue exposing [NodeValue]
```

The public reactive API is nested under `Reactive`:

- `Reactive.Signal(a)` is a continuous value with a current value.
- `Reactive.Event(a)` is a discrete stream of occurrences.
- `Reactive.EventSender(a)` is the write-only handle used by buttons and lifecycle hooks.
- `Reactive.Unit` is the nominal payload for click and lifecycle events.

Values cross the host boundary as `NodeValue`. The public API asks for Roc's normal static-dispatch `encode` and `decode` methods, so app-specific state defines methods on the nominal type:

```roc
Counter := { count : I64 }.{
    init : I64 -> Counter
    init = |count| { count: count }

    encode : Counter, NodeValue -> Try(NodeValue, [])
    encode = |counter, fmt| counter.count.encode(fmt)

    decode : NodeValue, NodeValue -> (Try(Counter, [TypeMismatch]), NodeValue)
    decode = |nv, fmt| {
        (result, rest) = I64.decode(nv, fmt)
        counter_result =
            match result {
                Ok(count) => Ok(Counter.init(count))
                Err(err) => Err(err)
            }

        (counter_result, rest)
    }
}
```

The generated Zig ABI also provides recursive retain/release helpers for host-facing Roc layouts. Hosts should prefer those helpers over hand-written layout cleanup; for example, nested `NvList(List(NodeValue))` values are released by generated `decrefNodeValue`.

## Components

A component is a static element tree plus an event stream of state changes:

```roc
Elem.Component(a) := {
    elem : Elem,
    changes : Reactive.Event(a),
}
```

Controlled components receive a parent-owned `Signal(state)` and emit `Event(next_state)`. `Elem.translate` adapts a child component to one field of a parent state value:

```roc
Elem.translate(
    Counter.render!,
    |model| model.left,
    |model, counter| App.make(counter, model.right),
)
```

The getter derives a child signal from the parent signal. The setter lifts child changes back into parent changes. The demo renders the same counter component twice against different fields of `App`.

## Local Signals

The platform also supports fine-grained local state. A component can create its own event channel and fold it into a local signal:

```roc
{ sender, receiver } = Reactive.Event.unit_channel!()
deltas : Reactive.Event(I64)
deltas = Reactive.Event.map(receiver, |_| 1)
count = Reactive.Signal.fold(0, deltas, |current, delta| current + delta)
```

Independent local signals can be combined with `Signal.map2`:

```roc
total =
    Reactive.Signal.map2(
        left.count,
        right.count,
        |left_value, right_value| left_value + right_value,
    )
```

## Dynamic Structure

`Elem.when` mounts one of two branches from a `Signal(Bool)`.

`Elem.each` mounts a keyed list from a `Signal(List(a))`. The key function decides identity, so reordering keeps child scopes and their local signal state, while removing a key unmounts that scope.

`Elem.lifecycle` sends unit events when a scope mounts or unmounts:

```roc
Elem.lifecycle({
    on_mount: mount_send,
    on_unmount: unmount_send,
})
```

These APIs are simulated in the host, but they mirror the responsibilities a real UI renderer would need: create nodes, move keyed children, deactivate removed scopes, drop bindings, and dispatch lifecycle events.

## Host Model

Pure Roc builds immutable descriptions of elements, signals, events, and boxed callbacks. The Zig host owns mutable state:

- graph nodes for events and signals,
- current signal values and transaction-local event occurrences,
- simulated DOM elements and text/click bindings,
- mount scopes for dynamic children and keyed lists,
- boxed Roc callbacks for maps, folds, renders, keys, and lifecycle wiring.

When an event fires, the host:

1. records the source occurrence,
2. computes the affected subgraph,
3. evaluates affected nodes only after affected inputs are ready,
4. records signals whose value actually changed,
5. updates only text bindings attached to changed signals,
6. drains lifecycle events queued during the transaction.

The current node kinds include event source, map, filter, merge, with-latest, signal const, state, map, map2, hold, fold, zip-with, dynamic, and keyed each.

## Extending To A Real Renderer

A real UI framework would replace the simulated DOM effects in `platform/host.zig` with renderer-specific work:

- create, append, remove, and move real UI nodes,
- translate native input callbacks into `EventSender` sends,
- schedule propagation on the UI thread,
- render more element types and attributes,
- attach native resources to mount scopes,
- release renderer resources when scopes unmount.

The app-facing model can stay the same: Roc code describes the signal graph and element tree, while the host owns mutation, scheduling, and renderer resources.
