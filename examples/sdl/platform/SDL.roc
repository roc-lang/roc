interface SDL
    exposes [
        init,
        Window,
        createWindow,
        Renderer,
        createRenderer,
        eventLoop
    ]
    imports [ fx.Effect, Task.{ Task } ]
    

init : Task {} *
init =
    Effect.init {}
    |> Effect.map (\_ -> {})
    |> Effect.after Task.succeed
    

Window : [ @Window Nat ]


createWindow : Str -> Task Window *
createWindow = \title ->
    Effect.createWindow title
    |> Effect.map (\ptr -> @Window ptr)
    |> Effect.after Task.succeed


Renderer : [ @Renderer Nat ]


createRenderer : Window -> Task Renderer *
createRenderer = \@Window window ->
    Effect.createRenderer window
    |> Effect.map (\ptr -> @Renderer ptr)
    |> Effect.after Task.succeed
    

eventLoop : Renderer -> Task {} *
eventLoop = \@Renderer renderer ->
    Effect.eventLoop renderer
    |> Effect.map \_ -> Ok {}