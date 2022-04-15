app "breakout"
    packages { pf: "platform" }
    imports [ pf.Game.{ Bounds, Elem, Event } ]
    provides [ program ] { Model } to pf

Model : {
    # Screen height and width
    height : F32,
    width : F32,

    # Paddle X-coordinate
    paddleX : F32,

    # Ball coordinates
    ballX : F32,
    ballY : F32,
}

init : Bounds -> Model
init = \{ width, height } ->
    {
        # Screen height and width
        width,
        height,

        # Paddle X-coordinate
        paddleX: (width * 0.5),

        # Ball coordinates
        ballX: (width * 0.5),
        ballY : (height * 0.5),
    }

update : Model, Event -> Model
update = \model, event ->
    when event is
        Resize size -> { model & width: size.width, height: size.height }
        KeyUp _ -> model
        KeyDown Left -> { model & paddleX: model.paddleX - 50 }
        KeyDown Right -> { model & paddleX: model.paddleX + 50 }
        KeyDown _ -> model

render : Model -> List Elem
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

    ball =
        color = { r: 0.7, g: 0.3, b: 0.9, a: 1.0 }
        width = 50
        height = 50
        left = model.ballX
        top = model.ballY

        Rect { left, top, width, height, color }

    paddle =
        color = { r: 0.8, g: 0.8, b: 0.8, a: 1.0 }
        width = model.width * 0.25
        height = blockHeight
        left = (model.width * 0.5) - (width * 0.5) + model.paddleX
        top = model.height - (height * 2)

        Rect { left, top, width, height, color }

    List.concat rects [ paddle, ball ]

program = { init, update, render }
