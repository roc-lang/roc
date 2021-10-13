platform examples/sdl
    requires {}{
        # handleEventForHost : Effect {}
        windowProperties : SDL.WindowConfig
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
windowPropertiesForHost = windowProperties

handleEventForHost : Task {} [] as Fx
handleEventForHost = Task.putLine "Got an event"
