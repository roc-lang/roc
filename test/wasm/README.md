# WebAssembly runtime regressions

These fixtures exercise the compiler's WebAssembly output against the test host
and Bytebox, including stack bounds, aggregate lifetimes, and host ABI behavior.
Their build registrations are in `build.zig`.

Run one registered cart by its unique output filename, without building or
running the other carts:

```sh
zig build run-test-wasm-cart -Dplatform=wasm \
  -Dwasm-cart=issue_12922_lambda_set_frame_static_lib_app.wasm
```

The frame regression also has `boxy`, `v1`, and `boxy_v1` variants, for example
`-Dwasm-cart=issue_12922_lambda_set_frame_boxy_v1_static_lib_app.wasm`.
They preserve the same source, expected result, and stack limit across strategies.

Frame-growth regressions also need structural backend tests: one chosen
recursion depth can pass while storage still grows with mutually exclusive
branches.
