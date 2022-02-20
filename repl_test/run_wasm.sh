# At the moment we are using this script instead of `cargo test`
# Cargo doesn't really have a good way to build two targets (host and wasm).
# We can try to make the build nicer later

cargo build --target wasm32-unknown-unknown -p roc_repl_wasm --features wasmer --release
cargo test -p repl_test --features wasm
