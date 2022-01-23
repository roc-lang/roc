app "hello-rust"
    packages { pf: "platform" }
    imports []
    provides [ render ] to pf

greeting =
    hi = "Hello"
    name = "World!"

    "\(hi), \(name)!\n"

render = greeting
