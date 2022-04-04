app "hello-gui"
    packages { pf: "platform" }
    imports []# [ pf.Action.{ Action }, pf.Elem.{ button, text, row, col } ]
    provides [ program ] to pf

program = { render }

render = \state ->
    div0 = \numerator, denominator -> (numerator / denominator) |> Result.withDefault 0

    rgba = \r, g, b, a -> { r: div0 r 255, g: div0 g 255, b: div0 b 255, a }

    styles = { bgColor: rgba 100 50 50 1, borderColor: rgba 10 20 30 1, borderWidth: 10, textColor: rgba 220 220 250 1 }

    height = if state.height == 1000 then "correct!" else if state.height == 0 then "zero" else "incorrect"
    width = if state.width == 1900 then "Correct!" else if state.width == 0 then "zero" else "Incorrect"

    Rect (Text "Success! ") styles
