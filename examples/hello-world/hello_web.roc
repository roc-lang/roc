app "hello_web"
    packages { pf: "web-platform" }
    imports []
    provides [ main ] to pf

greeting =
    hi = "Hello"
    name = "World"

    "\(hi), \(name)!"

main = greeting
