interface SDL
    exposes [
        Window,
        WindowConfig,
        Renderer
    ]
    imports [ fx.Effect, Task.{ Task } ]

## This module is a wrapper around SDL
## https://github.com/MasterQ32/SDL.zig/blob/master/src/wrapper/sdl.zig

Window : [ @Window Nat ]

Renderer : [ @Renderer Nat ]

WindowPosition : [ Default, Centered, Absolute Nat ]

WindowConfig :
    {
        title: Str,
        # x ? WindowPosition,
        # y ? WindowPosition,
        width: Nat,
        height: Nat
    }

