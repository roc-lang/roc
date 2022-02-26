set -eux

# Build a .wasm binary for the compiler, then build & run a native binary for the test code
# We *could* write a build.rs to do this but we'd have nested cargo processes with different targets.
# That can be solved using two separate target direcories with --target-dir but there isn't a huge win.
# Instead we are calling this script from the Earthfile in CI

cargo build --target wasm32-unknown-unknown -p roc_repl_wasm --features wasmer --release
cargo test -p repl_test --features wasm -- --test-threads=1
