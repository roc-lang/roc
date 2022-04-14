app "breakout"
    packages { pf: "platform" }
    imports []
    provides [ program ] { Model } to pf

Model : { height : F32, width : F32, pos : F32 }

program = { init, update, render }

init : { height : F32, width : F32 } -> Model
init = \_ -> { width: 1900, height: 1000, pos: 100 }

# update : Model, Event -> Model
update = \model, event ->
    when event is
        Resize size -> { model & width: size.width, height: size.height }
        KeyUp _ -> model
        KeyDown keyCode -> { model & pos: model.pos + 50 }

# render : Model -> List Elem
render = \model ->
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
                |> Num.toF32

        red = col / Num.toF32 numCols
        green = row / Num.toF32 numRows
        blue = Num.toF32 index / Num.toF32 numBlocks

        color = { r: red * 0.8, g: 0.2 + green * 0.6, b: 0.2 + blue * 0.8, a: 1 }

        { row, col, color }

    blockWidth = model.width / numCols
    blockHeight = 80

    rects =
        List.map blocks \{ row, col, color } ->
            left = Num.toF32 col * blockWidth
            top = Num.toF32 (row * blockHeight)

            Rect { left, top, width: blockWidth, height: blockHeight, color }

    paddle =
        color = { r: 0.8, g: 0.8, b: 0.8, a: 1.0 }
        width = model.width * 0.25
        height = blockHeight
        left = (model.width * 0.5) - (width * 0.5) + model.pos
        top = model.height - (height * 2)

        Rect { left, top, width, height, color }

    List.append rects paddle
