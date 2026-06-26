# Signals Browser Runtime Spikes

This directory contains the browser runtime (`runtime.mjs`) and the JS↔WASM
contract guards. The architecture is in `../DESIGN.md` (one engine, two thin
hosts; the wasm host is the browser boundary). Engine semantics and work budgets
are proven by the native spec runner, not re-tested here; the only JS-side
guards worth keeping are the codec/boundary contract ones below.

## Controlled Input / IME

`controlled_input_policy.mjs` is the deterministic executor policy for
`SetValue` patches on text inputs:

- equal values are no-ops;
- differing values are deferred while the input is focused;
- differing values are deferred while composition is active;
- the latest deferred value is applied after blur, unless a later user input
  already matched that value.

Run the guard with:

```sh
node --test test/signals/browser/controlled_input_policy.test.mjs
```

Open `controlled_input_spike.html` in a browser for the real focus and IME
exercise. The page is intentionally standalone so it can be used before the
wasm host grows the full runtime.

## `memory.grow` View Invalidation

`wasm_memory_views.mjs` is the deterministic guard for JS typed-array views over
Wasm linear memory. The chosen browser-runtime rule is:

- JS may cache `Uint8Array`, `Int32Array`, and `DataView` objects only between
  host calls;
- after every call into a host export that may allocate, JS must refresh those
  views before reading command buffers, strings, or event payload memory;
- the initial runtime does not require a host-bumped memory generation export.

Run the guard with:

```sh
node --test test/signals/browser/wasm_memory_views.test.mjs
```

## Render Command Buffer

`../src/render_commands.zig` owns the shared render op ids, command-count
rollup, metrics accumulator, and fixed-width command-buffer record shape. The
native host consumes the shared counters; the wasm host serializes browser DOM
patches through the same fixed-width record shape. Browser string payloads are
stored in the wasm host string buffer and referenced by command records as
offset/length pairs.

`runtime.mjs` is the browser-side command executor. It instantiates a Signals
wasm app, calls `roc_ui_mount`, applies command-buffer records to the DOM, and
routes click/input/check events back through `roc_ui_event`. It records the
records drained by the most recent host call in `lastCommands` so guards can
inspect the command-buffer codec.

The browser app shell enables telemetry by default. Disable it with
`?telemetry=0`, or persist that setting with:

```js
localStorage.setItem("signals:telemetry", "0")
```

Telemetry is emitted as one JSON object per console line with the `[signals]`
prefix. It records host calls, command batches, event binding/clearing, DOM
events, marshalled payloads, task lifecycle events, and a capture-phase pointer
probe. `window.signalsRuntime` is also exposed for inspecting `lastCommands`
after a manual interaction.

`dom_double.mjs` is a dependency-free DOM stand-in implementing exactly the
surface `runtime.mjs` touches, so the executor can be driven under `node --test`
without jsdom.

`runtime_contract.test.mjs` keeps the JS-side surface narrow: op-code to DOM-op
mapping, event payload marshalling, listener cleanup, and command-buffer reads
after memory growth. It deliberately does not re-assert app semantics or work
budgets; the native spec runner owns those.

Run the guard with:

```sh
node --test test/signals/browser/runtime_contract.test.mjs
```

## Build and Serve

Build a wasm artifact next to the page, then serve the browser asset directory:

```sh
test/signals/serve.py
```

Open:

```text
http://localhost:8000/
```

The helper builds `test/signals/src/wasm_host.zig` with `ReleaseSmall`, builds
the maintained six-app suite with `--target=wasm32 --opt=size`, writes each
artifact under `test/signals/browser`, and serves an app index from
`test/signals/browser/index.html`.

Build one maintained example, build one arbitrary app path, or use the dev app
optimization mode with:

```sh
test/signals/serve.py --example ops_dashboard --port 9001
test/signals/serve.py test/signals/apps/ops_dashboard.roc --port 9001
test/signals/serve.py --app-opt dev
```

Use `--no-server` when you only want the build steps. With no app argument, the
helper builds the maintained six-app suite; pass `--example <stem>` or one app
path for targeted QA.

`../src/signal_graph.zig` owns the active graph node shape, dependent-edge
mutation, reachable-dependent traversal, and rank sorting. Both hosts reach it
through the shared engine.

`../src/scope_tree.zig` owns scope branch identity, root/component/when/row
scope interning, active row lookup, and ancestry queries. Row payload refcounts
and disposal are driven by the shared engine through each host's `Ctx`.

`../src/identity_table.zig` owns node/DOM identity interning, including the
browser-relevant rule that DOM ids are one-based because id `0` is the mount
root.

`../src/keyed_rows.zig` owns the host-agnostic keyed-row match plan. The shared
engine executes that plan through the host value/thunk/scope adapter exposed by
each host's `Ctx`.

`../src/host_value_registry.zig` owns the shared `roc_host_value_*` handle table:
one-based handles, vacant-slot reuse, clone/get/take, and debug type tags.

Run the guard with:

```sh
zig test test/signals/src/render_commands.zig
zig test test/signals/src/signal_graph.zig
zig test test/signals/src/scope_tree.zig
zig test test/signals/src/identity_table.zig
zig test test/signals/src/keyed_rows.zig
zig test test/signals/src/host_value_registry.zig
```
