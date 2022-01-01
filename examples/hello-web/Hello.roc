app "hello-web"
    packages { pf: "platform" }
    imports []
    provides [ main ] to pf

greeting =
    hi = "Hello"
    name = "World"

    "\(hi), \(name)!"

main = greeting
