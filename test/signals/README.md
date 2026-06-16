# Signals Test Platform

This directory contains a small Roc platform for researching a signal-based UI
framework. The host is a simulated UI and spec runner: it does not draw pixels,
but it exercises the same event, render, and command-application path a real
renderer would need.

The current design keeps graph and render semantics on the Roc side of the
boundary:

- apps expose pure `main : {} -> Elem`,
- the platform provides `roc_ui_init`, `roc_ui_dispatch`, and `roc_ui_drop`,
- the Roc runtime keeps signal state and event ids in a boxed `Runtime`,
- the host sends boxed events and applies command batches to the simulated DOM.

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

Regenerate glue after changing exposed platform types or provided entrypoints:

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

## Roc API Shape

Apps import:

```roc
import pf.Elem
import pf.Reactive
import pf.NodeValue exposing [NodeValue]
```

`Reactive` builds pure graph descriptions:

- `Reactive.Signal(a)` is a continuous value with a current value.
- `Reactive.Event(a)` is a discrete stream of occurrences.
- `Reactive.EventSender(a)` is the write-only handle used by controls.
- Stateful signals use explicit string keys so state can be retained across
  rebuilt graph descriptions.

`NodeValue` is still used inside the Roc graph representation for erased
dynamic/list payloads. It is no longer a host-owned graph boundary value.

## Host Boundary

The host calls `roc_ui_init` once, stores the returned boxed runtime, and applies
the initial command list. For each spec event, it boxes a `HostEvent`, calls
`roc_ui_dispatch`, stores the returned boxed runtime, and applies the returned
commands.

The host does not create graph nodes, store callbacks, or evaluate signals.
Those responsibilities live in `UiRuntime.roc`.
