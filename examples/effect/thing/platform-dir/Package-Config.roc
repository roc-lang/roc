platform folkertdev/foo
    requires {model=>Model, msg=>Msg} {main : Effect {}}
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
