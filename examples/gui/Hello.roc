app "hello-gui"
    packages { pf: "platform" }
    imports [ ] #[ pf.Action.{ Action }, pf.Elem.{ button, text, row, col } ]
    provides [ render ] to pf

render =
    # btn = button { onPress : \prev, _ -> Action.none } (text "Hello, button!")

    div0 = \numerator, denominator -> (numerator / denominator) |> Result.withDefault 0

    rgba = \r, g, b, a -> { r: div0 r 255, g: div0 g 255, b: div0 b 255, a }

    styles = { bgColor: rgba 100 200 250 1, borderColor: rgba 10 20 30 1,  borderWidth : 10, textColor : rgba 220 220 250 1 }

    Button (Text "Hello, World!") styles
