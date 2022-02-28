app "hello_rust"
    packages { pf: "rust-platform" }
    imports []
    provides [ main ] to pf

greeting =
    hi = "Hello"
    name = "World"

    "\(hi), \(name)!"

main = greeting
