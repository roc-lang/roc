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
