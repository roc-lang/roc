app "hello-world"
    packages { base: "platform" }
    imports []
    provides [ main ] to base


ConsList a : [ Cons a (ConsList a), Nil ]

greeting =
    hi = "Hello"
    name = "World"

    "\(hi), \(name)!!!!!!!!!!!!!"

main = greeting
