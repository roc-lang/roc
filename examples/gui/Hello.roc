app "hello-rust"
    packages { pf: "platform" }
    imports []
    provides [ main ] to pf

greeting =
    hi = "Hello"
    name = "World"

    "\(hi), \(name)!\n"

main = greeting
