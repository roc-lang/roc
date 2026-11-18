# Wasm development builds with `dbg`

Reproduction for https://github.com/roc-lang/roc/issues/13102.
The build must succeed and produce a module importing `env.roc_dbg`.
Calling `roc_main(41)` must return `42` and send `"hi"` to that import.

From the repository root:

```sh
zig build roc -j2
mkdir -p test/repros/issue_13102_wasm_dbg/platform/targets/wasm32
printf '\0asm\1\0\0\0\0\011\007linking\002' \
  > test/repros/issue_13102_wasm_dbg/platform/targets/wasm32/host.wasm
zig-out/bin/roc build --opt=dev --specialize=yes --target=wasm32 \
  --no-cache --output=zig-out/issue_13102.wasm \
  test/repros/issue_13102_wasm_dbg/main.roc
```

`--specialize=yes` explicitly selects the affected object-composition path;
the current default uses Boxy and a different linker path. The host object
is the issue's empty relocatable Wasm object, generated rather than checked in.

The focused regression tests are in `src/backend/wasm/WasmModule.zig`:

```sh
zig build run-test-zig-module-backend -j2 \
  -Dtest-filter='backend tests' -Dtest-filter='issue 13102' \
  -- --test-filter 'issue 13102'
```

The compile-time filter includes the backend aggregator so Zig discovers the
nested Wasm tests; the runtime filter executes only these regressions.

They require object merging to preserve the indirect table's `NO_STRIP`
requirement, whether the destination already imports the table or not.
Function-address relocations implicitly depend on that table even when no
reachable `call_indirect` explicitly references its symbol.
The controls also check that unflagged inputs do not force retention and that
the requirements survive object serialization.
