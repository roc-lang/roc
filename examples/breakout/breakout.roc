app "breakout"
    packages { pf: "platform" }
    imports []# [ pf.Action.{ Action }, pf.Elem.{ button, text, row, col } ]
    provides [ program ] to pf

program = { render }

render = \state ->
    div0 = \numerator, denominator -> (numerator / denominator) |> Result.withDefault 0
    intDiv0 = \numerator, denominator -> (numerator // denominator) |> Result.withDefault 0

    rgba = \r, g, b, a -> { r: div0 r 255, g: div0 g 255, b: div0 b 255, a }

    styles = { bgColor: rgba 100 50 50 1, borderColor: rgba 10 20 30 1, borderWidth: 10, textColor: rgba 220 220 250 1 }

    numRows = 4
    numCols = 8

    blocks = List.map (List.range 0 (numRows * numCols)) \index ->
        { col: index, row: (index // 5 |> Result.withDefault 0) }

    blockWidth = 50
    blockHeight = 20

    List.map blocks \{ row, col } ->
        left = Num.toF32 (col * blockWidth)
        top = Num.toF32 (row * blockHeight)
        red =
            250 // col
                |> Result.withDefault 0
                |> Num.toF32

        green =
            250 // row
                |> Result.withDefault 0
                |> Num.toF32

        color = rgba red 120 green 1

        Rect { left, top, width: blockWidth, height: blockHeight, color }
