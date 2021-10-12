platform examples/cli
    requires {}{ main : Task {} [] } # TODO FIXME
    exposes []
    packages {}
    imports [ Task.{ Task } ]
    provides [ setupForHost ]
    effects fx.Effect
        {
            putLine : Str -> Effect {},
            getLine : Effect Str
        }

setupForHost : Task {} [] as Fx
setupForHost = setup
