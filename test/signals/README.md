# Signals Test Platform

This directory contains a small Roc platform for researching a signal-based UI
framework. The host is a simulated retained-mode UI and test runner: it does
not draw pixels, but it exercises the same machinery a real renderer needs.

The maintained coverage is the representative app suite in
[`apps/`](apps/README.md):

- ops dashboard,
- checkout wizard,
- kanban board.

## Running

From the repository root:

```sh
zig build run-test-signals
```

That step checks, builds, and runs all three representative apps against their
semantic specs.

The host can also be rebuilt directly:

```sh
zig build build-test-hosts -Dplatform=signals
```

Regenerate glue after changing hosted effects or exposed platform types:

```sh
./zig-out/bin/roc glue src/glue/src/ZigGlue.roc test/signals/platform test/signals/platform/main.roc
```

## Spec Language

Specs use semantic locators rather than positional DOM indices:

```txt
expect_visible role:heading name:"Checkout wizard"
fill label:"Email" "team@example.com"
expect_value label:"Email" "team@example.com"
check label:"Accept terms"
expect_checked label:"Accept terms" true
click role:button name:"Place order"
```

Supported locators:

- `role:<role> name:"<accessible name>"`
- `label:"<label>"`
- `text:"<exact text>"`
- `test_id:"<id>"`

Supported commands:

- `click <locator>`
- `fill <locator> "<text>"`
- `check <locator>` and `uncheck <locator>`
- `expect_visible <locator>`
- `expect_text <locator> "<text>"`
- `expect_value <locator> "<text>"`
- `expect_checked <locator> true|false`
- `expect_disabled <locator> true|false`
- `expect_updates <locator> <count>`

## Benchmarks

The host has a research benchmark mode for measuring boundary and scheduler
trade-offs without adding benchmark payloads to app Roc code:

```sh
zig build run-signals-bench
./zig-out/bin/signals-ops-dashboard --bench --bench-iterations 5000 --bench-samples 3
```

Benchmark output is CSV. Rows cover synthetic scalar boundaries, list/record
boundaries, equality, callback shape, deep map chains, and wide fanout. The
current prototypes are:

| Prototype | What it measures |
| --- | --- |
| A. Baseline `NodeValue` | Generic 32-byte tagged union values across the boundary |
| B. Scalar fast paths | Explicit primitive paths for hot `I64`, `Bool`, and `Str` graph nodes |
| C. Host value handles | Host-owned handles/views for values with explicit lifetime questions |
| D. Generated boundary | Type-specific ABI/callback/equality shapes generated from Roc layout data |

See [nodevalue_boundary_research.md](nodevalue_boundary_research.md) for the
current matrix and interpretation.

## Roc API Shape

Apps import:

```roc
import pf.Elem
import pf.Reactive
import pf.NodeValue exposing [NodeValue]
```

The app-facing API keeps mutation in the host and graph descriptions in Roc:

- `Reactive.Signal(a)` is a continuous value with a current value.
- `Reactive.Event(a)` is a discrete stream of occurrences.
- `Reactive.EventSender(a)` is the write-only handle used by controls.
- Values crossing generic dynamic/list boundaries use `NodeValue` through Roc
  static-dispatch `encode` and `decode`.

Stateful signal constructors are eager and effectful:

```roc
{ sender, receiver } = Reactive.Event.unit_channel!()
deltas = Reactive.Event.map_unit_i64_const(receiver, 1)
count = Reactive.Signal.fold_i64!(0, deltas, |current, delta| current + delta)
```

The eager `!` constructors create host node identity immediately. This avoids
late first-walk behavior where a dynamic branch could construct a fresh stateful
node from its initial value after earlier events had already occurred.

## Dynamic Structure

`Elem.dynamic_keyed` mounts one branch from a signal with a stable branch key.
`Elem.each` mounts a keyed list from a `Signal(List(a))`. Keys define identity:
reordering keeps child scopes and local signal state, while removing a key
unmounts that scope.

The host owns:

- graph nodes for events and signals,
- current signal values and transaction-local event occurrences,
- simulated DOM elements and form/text/click bindings,
- mount scopes for dynamic children and keyed lists,
- boxed Roc callbacks for maps, folds, renders, keys, and lifecycle wiring.

When an event fires, the host records the occurrence, computes the affected
subgraph, evaluates ready nodes, records changed signals, updates only affected
bindings, and drains lifecycle events queued during the transaction.
