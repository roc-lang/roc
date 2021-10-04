platform examples/sdl
    requires {}{ main : Effect {} }
    exposes []
    packages {}
    imports [ Task.{ Task } ]
    provides [ mainForHost ]
    effects fx.Effect
        { 
            putLine : Str -> Effect {},
            createWindow : Effect {}
        }

mainForHost : Task {} [] as Fx
mainForHost = main
