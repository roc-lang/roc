platform examples/sdl
    requires {}{ main : Effect {} }
    exposes []
    packages {}
    imports [ Task.{ Task } ]
    provides [ mainForHost ]
    effects fx.Effect
        { 
            putLine : Str -> Effect {},
            createRenderer : Nat -> Effect Nat,
            eventLoop : Nat -> Effect {},
            createWindow : Effect Nat,
            init : Effect {}
        }

mainForHost : Task {} [] as Fx
mainForHost = main
