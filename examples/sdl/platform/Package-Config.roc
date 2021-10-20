platform examples/sdl
    requires {}{
        gameContext : SDL.Context
    }
    exposes []
    packages {}
    imports [ Task.{ Task }, SDL ]
    provides [
        windowPropertiesForHost,
        handleEventForHost
    ]
    effects fx.Effect
        { 
            putLine : Str -> Effect {}
        }

windowPropertiesForHost : SDL.WindowConfig
windowPropertiesForHost = gameContext.props

handleEventForHost : Task {} [] as Fx
handleEventForHost = gameContext.handler
