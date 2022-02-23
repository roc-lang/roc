app "hello-gui"
    packages { pf: "platform" }
    imports [ ] #[ pf.Action.{ Action }, pf.Elem.{ button, text, row, col } ]
    provides [ render ] to pf

render =
    # btn = button { onPress : \prev, _ -> Action.none } (text "Hello, button!")

    Button (Text "Hello, World!") { left: 300, top: 400, height: 300, width: 400 }
