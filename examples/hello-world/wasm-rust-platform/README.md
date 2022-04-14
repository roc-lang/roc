```
cd examples/hello-world/wasm-rust-platform
curl https://rustwasm.github.io/wasm-pack/installer/init.sh -sSf | sh 
RUSTFLAGS="-C embed-bitcode" wasm-pack build --target web
npm install -g http-server
http-server
```
