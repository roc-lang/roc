app "hello-gui"
    packages { pf: "platform" }
    imports []# [ pf.Action.{ Action }, pf.Elem.{ button, text, row, col } ]
    provides [ render ] to pf

render =
    div0 = \numerator, denominator -> (numerator / denominator) |> Result.withDefault 0

    rgba = \r, g, b, a -> { r: div0 r 255, g: div0 g 255, b: div0 b 255, a }

    styles = { bgColor: rgba 100 50 50 1, borderColor: rgba 10 20 30 1, borderWidth: 10, textColor: rgba 220 220 250 1 }

    Col
        [
            Row
                [
                    Button (Text "Corner     ") styles,
                    Button (Text "Top Mid    ") { styles & bgColor: rgba 100 100 50 1 },
                    Button (Text "Top Right  ") { styles & bgColor: rgba 50 50 150 1 },
                ],
            Button (Text "Mid Left   ") { styles & bgColor: rgba 150 100 100 1 },
            Button (Text "Bottom Left") { styles & bgColor: rgba 150 50 50 1 },
        ]
