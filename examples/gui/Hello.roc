app "hello-gui"
    packages { pf: "platform" }
    imports [ pf.Action.{ Action }, pf.Elem.{ button, text, row, col } ]
    provides [ render ] to pf

render =
    btn = Button { onPress : \_ -> Action.none } (text "Hello, button!")

    "Hello, World!"
