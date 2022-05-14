
```
cargo run build --target wasm32 --no-link ./examples/hello-world/wasm-rust-platform/app/helloWeb.roc
```
```
sudo apt-get install ripgrep
```
From the root of the repo do "rg gitrepos" and update all paths for your machine.


```
cd examples/hello-world/wasm-rust-platform/combined/
wasm-pack build
```


issue:
/home/anton/gitrepos/2roc/roc/examples/hello-world/wasm-rust-platform/combined/pkg/combined_bg.wasm still contains a reference to env:
```
  (import "env" "roc_dealloc" (func $env.roc_dealloc (type $t0)))
```