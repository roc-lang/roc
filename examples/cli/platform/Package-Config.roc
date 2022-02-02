platform "examples/cli"
    requires {} { main : Task {} [] }
    exposes []
    packages {}
    imports [ Task.{ Task } ]
    provides [ mainForHost ]
    effects fx.Effect { putLine : Str -> Effect {}, getLine : Effect Str }

mainForHost : Task {} [] as Fx
mainForHost = main
