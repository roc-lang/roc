platform folkertdev/foo
    requires {model=>Model, msg=>Msg} {main : Effect {}}
    exposes []
    packages {}
    imports [ Task.{ Task } ]
    provides [ mainForHost ]
    effects fx.Effect
        {
            putLine : Str -> Effect {},
            putInt : I64 -> Effect {},
            getInt : Effect { value: I64, errorCode: [ InvalidCharacter, IOError ], isError: Bool }
        }

mainForHost : Task {} [InvalidCharacter, IOError] as Fx
mainForHost = main
