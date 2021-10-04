interface Window
    exposes [ Window, createWindow ]
    imports [ fx.Effect, Task.{ Task } ]


Window : {}


createWindow : Task {} *
createWindow =
    Effect.createWindow
    |> Effect.map \_ -> Ok {}