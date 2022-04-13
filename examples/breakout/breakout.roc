app "breakout"
    packages { pf: "platform" }
    imports []# [ pf.Action.{ Action }, pf.Elem.{ button, text, row, col } ]
    provides [ program ] { Model } to pf

Model : { height : F32, width : F32 }

program = { init, update, render }

init = \_ -> { width: 1900, height: 1000 }

update = \state, event ->
    when event is
        Resize size -> size
        KeyUp keyCode -> { width: 1900, height: 1000 }
        KeyDown keyCode -> { width: 1900, height: 1000 }

render = \state ->
    numRows = 4
    numCols = 8
    numBlocks = numRows * numCols

    blocks = List.map (List.range 0 numBlocks) \index ->
        col =
            Num.rem index numCols
                |> Result.withDefault 0
                |> Num.toF32

        row =
            index // numCols
                |> Result.withDefault 0
                |> Num.toF32

        red = (col / Num.toF32 numCols) |> Result.withDefault 0
        green = ((row / Num.toF32 numRows) |> Result.withDefault 0)
        blue = (Num.toF32 index / Num.toF32 numBlocks) |> Result.withDefault 0

        color = { r: red * 0.8, g: 0.2 + green * 0.6, b: 0.2 + blue * 0.8, a: 1 }

        { row, col, color }

    blockWidth = state.width / numCols |> Result.withDefault 0
    blockHeight = 80

    rects =
        List.map blocks \{ row, col, color } ->
            left = Num.toF32 col * blockWidth
            top = Num.toF32 (row * blockHeight)

            Rect { left, top, width: blockWidth, height: blockHeight, color }

    paddle =
        color = { r: 0.8, g: 0.8, b: 0.8, a: 1.0 }
        width = state.width * 0.25
        height = blockHeight
        left = (state.width * 0.5) - (width * 0.5)
        top = state.height - (height * 2)

        Rect { left, top, width, height, color }

    # List.append rects paddle
    [ Rect { left: 1, top: 2, width: state.width, height: state.height, color: { r: 0.8, g: 0.8, b: 0.8, a: 1.0 } } ]
