app [main!] { pf: platform "./platform/main.roc" }

main! = || {
    # Test Box.box and Box.unbox round-trip
    value = "Hello from Roc WASM!"
    boxed = Box.box(value)
    Box.unbox(boxed)
}
