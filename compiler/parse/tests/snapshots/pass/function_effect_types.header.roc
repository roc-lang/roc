platform "examples/cli"
    requires {}{ main : Task {} [] } # TODO FIXME
    exposes []
    packages {}
    imports [ Task.{ Task } ]
    provides [ mainForHost ]
    effects fx.Effect
        {
            getLine : Effect Str,
            putLine : Str -> Effect {},
            twoArguments : Int, Int -> Effect {}
        }
