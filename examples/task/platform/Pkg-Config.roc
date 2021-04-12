platform folkertdev/foo
    requires { main : Task {} [] }
    exposes []
    packages {}
    imports [ Task ]
    provides [ mainForHost ]
    effects fx.Effect
        {
            # TODO change errno to I32
            readAllUtf8 : Str -> Effect { errno : I64, bytes : List U8 },
            putLine : Str -> Effect {}
        }

mainForHost : Task.Task {} [] as Fx
mainForHost = main
