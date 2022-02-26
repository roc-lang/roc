app "hello_zig"
    packages { pf: "zig-platform" }
    imports []
    provides [ main ] to pf

greeting =
    hi = "Hello"
    name = "World"

    "\(hi), \(name)!"

main = greeting
