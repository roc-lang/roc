app "breakout"
    packages { pf: "platform" }
    imports []# [ pf.Action.{ Action }, pf.Elem.{ button, text, row, col } ]
    provides [ program ] to pf

program = { render }

render = \state ->
    div0 = \numerator, denominator -> (numerator / denominator) |> Result.withDefault 0

    rgba = \r, g, b, a -> { r: div0 r 255, g: div0 g 255, b: div0 b 255, a }

    styles = { bgColor: rgba 100 50 50 1, borderColor: rgba 10 20 30 1, borderWidth: 10, textColor: rgba 220 220 250 1 }

    #Text "Hello!"
    # Rect { r: 1, g: 2, b: 3, a: 4 }
    # Rect styles
    Text "Hello!"
