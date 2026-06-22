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
native host consumes the shared counters; the wasm host exposes an empty command
buffer surface that the future browser render sink will fill.

`../src/signal_graph.zig` owns the active graph node shape, dependent-edge
mutation, reachable-dependent traversal, and rank sorting. The native host
drives it today; the wasm host instantiates it at build time so the shared graph
primitive stays wasm-safe while the rest of the engine is extracted.

Run the guard with:

```sh
zig test test/signals/src/render_commands.zig
zig test test/signals/src/signal_graph.zig
```
