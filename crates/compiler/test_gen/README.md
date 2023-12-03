# Running our CodeGen tests

Our code generation tests are all in this crate. Feature flags are used to run the tests with a specific backend. For convenience, some aliases are added in `.cargo/config`:

```toml
[alias]
test-gen-llvm = "test -p test_gen"
test-gen-dev = "test -p test_gen --no-default-features --features gen-dev"
test-gen-wasm = "test -p test_gen --no-default-features --features gen-wasm"
```

So we can run:

```sh
cargo test-gen-llvm
```

To run the gen tests with the LLVM backend. To filter tests, append a filter like so:

```sh
> cargo test-gen-wasm wasm_str::small
    Finished test [unoptimized + debuginfo] target(s) in 0.13s
     Running src/tests.rs (target/debug/deps/test_gen-b4ad63a9dd50f050)

running 2 tests
test wasm_str::small_str_literal ... ok
test wasm_str::small_str_zeroed_literal ... ok
```
