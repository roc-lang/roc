app "hello-rust"
    packages { pf: "platform" }
    imports [ Action.{ Action }, Elem.{ button, text, row, col } ]
    provides [ main ] to pf

greeting =
    hi = "Hello"
    name = "World"

    "\(hi), \(name)!\n"

main =
    btn = Button { onPress : \_ -> Action.none } (text "Hello, button!")

    greeting
