platform examples/sdl
    requires {}{
        gameContext : SDL.Context
    }
    exposes []
    packages {}
    imports [ SDL ]
    provides [
        contextForHost
    ]
    effects fx.Effect
        { 
            putLine : Str -> Effect {}
        }

contextForHost : SDL.Context
contextForHost = gameContext
