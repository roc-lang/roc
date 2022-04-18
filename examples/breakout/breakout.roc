app "breakout"
    packages { pf: "platform" }
    imports [ pf.Game.{ Bounds, Elem, Event, Rgba } ]
    provides [ program ] { Model } to pf

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
    blocks : List Block,

    # Screen height and width
    height : F32,
    width : F32,

    # Paddle X-coordinate
    paddleX : F32,

    # Ball coordinates
    ballX : F32,
    ballY : F32,
    dBallX : F32, # delta x - how much it moves per tick
    dBallY : F32, # delta y - how much it moves per tick
}

Block : {
    left : F32,
    top : F32,
    color : Rgba,
    status : [ Active, Removed, Fading F32 ],
}

init : Bounds -> Model
init = \{ width, height } ->
    {
        blocks: initBlocks width,

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

initBlocks : F32 -> List Block
initBlocks = \width ->
    blockWidth = width / numCols

    List.map (List.range 0 numBlocks) \index ->
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

        left = Num.toF32 col * blockWidth
        top = Num.toF32 row * blockHeight

        { left, top, color, status: Active }

update : Model, Event -> Model
update = \model, event ->
    when event is
        Resize size -> { model & width: size.width, height: size.height }
        KeyDown Left -> { model & paddleX: model.paddleX - paddleSpeed }
        KeyDown Right -> { model & paddleX: model.paddleX + paddleSpeed }
        Tick _ -> tick model
        _ -> model

tick : Model -> Model
tick = \model ->
    model
        |> moveBall
        |> updateBlocks

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

updateBlocks : Model -> Model
updateBlocks = \model ->
    blockWidth = model.width / numCols
    blocks = List.map model.blocks \block ->
        when block.status is
            Removed -> block
            Active ->
                ballRect = { left: model.ballX, top: model.ballY, width: ballSize, height: ballSize }
                blockRect = { left: block.left, top: block.top, height: blockHeight, width: blockWidth }

                if isOverlapping blockRect ballRect then
                    { block & status: Removed }
                else
                    block
            Fading amount ->
                if amount <= 0 then
                    { block & status: Removed }
                else
                    { block & status: Fading (amount - 0.1) }

    { model & blocks }

isOverlapping = \rect1, rect2 ->
    (rect1.left + rect1.width >= rect2.left)
        && (rect2.left + rect2.width >= rect1.left)
        && (rect1.top + rect1.height >= rect2.top)
        && (rect2.top + rect2.height >= rect1.top)

render : Model -> List Elem
render = \model ->
    blockWidth = model.width / numCols

    rects =
        List.joinMap model.blocks \{ left, top, color, status } ->
            border = blockBorder * blockWidth
            alpha =
                when status is
                    Fading amount -> amount
                    Active -> 1
                    Removed -> 0

            # This outer rectangle gets drawn first, and will appear to be a border.
            outer = Rect {
                left,
                top,
                width: blockWidth,
                height: blockHeight,
                color: { r: color.r * 0.8, g: color.g * 0.8, b: color.b * 0.8, a: alpha },
            }

            # The inner retangle is smaller than the outer one, but gets drawn on top of it,
            # such that the outer one appears to be a border.
            inner = Rect {
                left: left + border,
                top: top + border,
                width: blockWidth - (border * 2),
                height: blockHeight - (border * 2),
                color: { color & a: alpha },
            }

            [ outer, inner ]

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

    List.concat rects [ paddle, ball ]

program = { init, update, render }
