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
            createWindow : Str -> Effect Nat,
            eventLoop : Nat -> Effect {},
            init : {} -> Effect {}
        }

mainForHost : Task {} [] as Fx
mainForHost = main
