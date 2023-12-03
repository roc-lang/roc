app "breakout"
    packages { pf: "platform/main.roc" }
    imports [pf.Game.{ Bounds, Elem, Event }]
    provides [program] { Model } to pf

paddleWidth = 0.2 # width of the paddle, as a % of screen width
paddleHeight = 50 # height of the paddle, in pixels
paddleSpeed = 65 # how many pixels the paddle moves per keypress
blockHeight = 80 # height of a block, in pixels
blockBorder = 0.025 # border of a block, as a % of its width
ballSize = 55
numRows = 4
numCols = 8
numBlocks = numRows * numCols

Model : {
    # Screen height and width
    height : F32,
    width : F32,
    # Paddle X-coordinate
    paddleX : F32,
    # Ball coordinates
    ballX : F32,
    ballY : F32,
    dBallX : F32,
    # delta x - how much it moves per tick
    dBallY : F32,
    # delta y - how much it moves per tick
}

init : Bounds -> Model
init = \{ width, height } -> {
    # Screen height and width
    width,
    height,
    # Paddle X-coordinate
    paddleX: (width * 0.5) - (paddleWidth * width * 0.5),
    # Ball coordinates
    ballX: width * 0.5,
    ballY: height * 0.4,
    # Delta - how much ball moves in each tick
    dBallX: 4,
    dBallY: 4,
}

update : Model, Event -> Model
update = \model, event ->
    when event is
        Resize size ->
            { model & width: size.width, height: size.height }

        KeyDown Left ->
            { model & paddleX: model.paddleX - paddleSpeed }

        KeyDown Right ->
            { model & paddleX: model.paddleX + paddleSpeed }

        Tick _ ->
            tick model

        _ ->
            model

tick : Model -> Model
tick = \model ->
    model
    |> moveBall

moveBall : Model -> Model
moveBall = \model ->
    ballX = model.ballX + model.dBallX
    ballY = model.ballY + model.dBallY

    paddleTop = model.height - blockHeight - (paddleHeight * 2)
    paddleLeft = model.paddleX
    paddleRight = paddleLeft + (model.width * paddleWidth)

    # If its y used to be less than the paddle, and now it's greater than or equal,
    # then this is the frame where the ball collided with it.
    crossingPaddle = model.ballY < paddleTop && ballY >= paddleTop

    # If it collided with the paddle, bounce off.
    directionChange =
        if crossingPaddle && (ballX >= paddleLeft && ballX <= paddleRight) then
            -1f32
        else
            1f32

    dBallX = model.dBallX * directionChange
    dBallY = model.dBallY * directionChange

    { model & ballX, ballY, dBallX, dBallY }

render : Model -> List Elem
render = \model ->

    blocks = List.map
        (List.range { start: At 0, end: Length numBlocks })
        \index ->
            col =
                Num.rem index numCols
                |> Num.toF32

            row =
                index
                // numCols
                |> Num.toF32

            red = col / Num.toF32 numCols
            green = row / Num.toF32 numRows
            blue = Num.toF32 index / Num.toF32 numBlocks

            color = { r: red * 0.8, g: 0.2 + green * 0.6, b: 0.2 + blue * 0.8, a: 1 }

            { row, col, color }

    blockWidth = model.width / numCols

    rects =
        List.joinMap
            blocks
            \{ row, col, color } ->
                left = Num.toF32 col * blockWidth
                top = Num.toF32 (row * blockHeight)
                border = blockBorder * blockWidth

                outer = Rect {
                    left,
                    top,
                    width: blockWidth,
                    height: blockHeight,
                    color: { r: color.r * 0.8, g: color.g * 0.8, b: color.b * 0.8, a: 1 },
                }

                inner = Rect {
                    left: left + border,
                    top: top + border,
                    width: blockWidth - (border * 2),
                    height: blockHeight - (border * 2),
                    color,
                }

                [outer, inner]

    ball =
        color = { r: 0.7, g: 0.3, b: 0.9, a: 1.0 }
        width = ballSize
        height = ballSize
        left = model.ballX
        top = model.ballY

        Rect { left, top, width, height, color }

    paddle =
        color = { r: 0.8, g: 0.8, b: 0.8, a: 1.0 }
        width = model.width * paddleWidth
        height = paddleHeight
        left = model.paddleX
        top = model.height - blockHeight - height

        Rect { left, top, width, height, color }

    List.concat rects [paddle, ball]

program = { init, update, render }
