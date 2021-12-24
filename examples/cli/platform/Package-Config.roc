platform "examples/cli"
    requires {} { main : Task {} [] }# TODO FIXME
    exposes []
    packages {}
    imports [ Task.{ Task } ]
    provides [ mainForHost ]
    effects fx.Effect
    {
    putLine : Str -> Effect {},
    getLine : Effect Str
     }

mainForHost : Task {} [] as Fx
mainForHost = main
