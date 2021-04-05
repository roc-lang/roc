platform rtfeldman/roc-cli
    requires { main : Task.Task {} * } # TODO FIXME
    exposes [] # TODO FIXME actually expose modules
    packages {}
    imports [ Task ] # TODO FIXME Task.{ Task }
    provides [ mainForHost ]
    effects fx.Effect
        {
            putChar : I64 -> Effect {},
            putLine : Str -> Effect {},
            getLine : Effect Str
        }


mainForHost : Task.Task {} * as Fx # TODO FIXME Task.Task {} []
mainForHost = main
