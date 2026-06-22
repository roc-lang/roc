# Signals Browser Runtime Spikes

This directory contains browser-runtime spikes that answer the open questions
in `../BROWSER_RUNTIME_DESIGN.md`.

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
routes click/input/check events back through `roc_ui_event`.

The first manual browser app is `../apps/counter.roc`. Build the local ignored
wasm next to the page, then serve the browser asset directory:

```sh
test/signals/serve.py
```

Open:

```text
http://localhost:8000/counter.html
```

The helper builds `test/signals/src/wasm_host.zig` with `ReleaseSmall`, builds
the app with `--target=wasm32 --opt=size`, writes
`test/signals/browser/counter.wasm`, and serves only `test/signals/browser`.

Build a different app or use the dev backend with:

```sh
test/signals/serve.py test/signals/apps/ops_dashboard.roc --port 9001
test/signals/serve.py --app-opt dev
```

Use `--no-server` when you only want the build steps.

`../src/signal_graph.zig` owns the active graph node shape, dependent-edge
mutation, reachable-dependent traversal, and rank sorting. The native host
drives it today; the wasm host instantiates it at build time so the shared graph
primitive stays wasm-safe while the rest of the engine is extracted.

`../src/scope_tree.zig` owns scope branch identity, root/component/when/row
scope interning, active row lookup, and ancestry queries. Native still owns row
payload refcounts and disposal until keyed-row matching is extracted.

`../src/identity_table.zig` owns node/DOM identity interning, including the
browser-relevant rule that DOM ids are one-based because id `0` is the mount
root.

`../src/keyed_rows.zig` owns the host-agnostic keyed-row match plan. Native
currently executes that plan through its HostValue/thunk/scope adapter; the wasm
host should grow the same adapter surface rather than a separate row matcher.

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
