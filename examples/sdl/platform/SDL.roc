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

## This module is a wrapper around SDL
## https://github.com/MasterQ32/SDL.zig/blob/master/src/wrapper/sdl.zig    

init : Task {} *
init =
    Effect.init {}
    |> Effect.map (\_ -> {})
    |> Effect.after Task.succeed
    

Window : [ @Window Nat ]

WindowPosition : [ Default, Centered, Absolute Nat ]


WindowConfig :
    {
        title: Str,
        x ? WindowPosition,
        y ? WindowPosition,
        width: Nat,
        height: Nat
    }


createWindow : WindowConfig -> Task Window *
createWindow = \{ title, x ? Default, y ? Default, width, height } ->
    # newX =
    #     when x is
    #             Default -> 536805376
    #             Centered -> 805240832
    #             Absolute val -> val

    # newY =
    #     when x is
    #             Default -> 536805376
    #             Centered -> 805240832
    #             Absolute val -> val

     
    Effect.createWindow title width height
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