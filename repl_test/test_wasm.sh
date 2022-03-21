set -eux

# Build the compiler for WebAssembly target
# We *could* write a build.rs to do this but we'd have nested cargo processes with different targets.
# That can be solved using two separate target direcories with --target-dir but there isn't a huge win.
# We need to clear RUSTFLAGS for this command, as CI sets normally some flags that are specific to CPU targets.
RUSTFLAGS="" cargo build --target wasm32-unknown-unknown -p roc_repl_wasm --no-default-features --features wasmer --release

# Build & run the test code on *native* target, not WebAssembly
cargo test -p repl_test --features wasm -- --test-threads=1
