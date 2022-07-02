set -eux

# Build the compiler for WebAssembly target
# We *could* write a build.rs to do this but we'd have nested cargo processes with different targets.
# If we ever decide to do that, one of the cargo processes will need to use the `--target-dir` option.

# We need to clear RUSTFLAGS for this command, as CI sets normally some flags that are specific to CPU targets.
# Tests target wasm32-wasi instead of wasm32-unknown-unknown, so that we can debug with println! and dbg!
RUSTFLAGS="" cargo build --target wasm32-wasi -p roc_repl_wasm --no-default-features --features wasmer --release

# Build & run the test code on *native* target, not WebAssembly
cargo test -p repl_test --features wasm -- --test-threads=1
