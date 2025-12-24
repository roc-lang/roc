app [main!] { pf: platform "./platform/main.roc" }

main! = || {
    _boxed = Box.box("test")
    "Hello from Roc WASM!"
}
