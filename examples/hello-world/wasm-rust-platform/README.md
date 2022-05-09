
First change `cli/src/build.rs` line 259(`std::fs::copy...`) to your Desktop path
```
cargo run build --target wasm32 ./examples/hello-world/wasm-rust-platform/app/helloWeb.roc
cd yourDesktopPath
llc-13 -O3 -filetype=obj roc_app_wasm.bc -o roc_app_wasm.o
ar rcs libroc_app.a roc_app_wasm.o
```
Copy `libroc_app.a` to examples/hello-world/wasm-rust-platform
```
cd rocFolder/examples/hello-world/wasm-rust-platform
RUSTFLAGS="-C embed-bitcode" cargo build --target wasm32-unknown-unknown
wasm-bindgen ./target/wasm32-unknown-unknown/debug/combined.wasm  --out-dir wasm_out --target web
```

issue:
rocFolder/examples/hello-world/wasm-rust-platform/wasm_out/combined_bg.wasm still contains references to env:
```
  (import "env" "malloc" (func $malloc (type $t4)))
  (import "env" "realloc" (func $realloc (type $t7)))
  (import "env" "free" (func $free (type $t3)))
```