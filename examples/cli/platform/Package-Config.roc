platform examples/cli
    requires {}{ main : Task {} [] }
    exposes []
    packages {}
    imports [ Task.{ Task } ]
    provides [ mainForHost ]
    effects fx.Effect
        {
            putLine : Str -> Effect {},
            getLine : Effect Str,
            readAllUtf8 : Str -> Effect { bytes : List U8, errno : I32 },
            writeAllUtf8 : Str, Str -> Effect { errno: I32 }
        }

mainForHost : Task {} [] as Fx
mainForHost = main
