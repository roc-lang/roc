platform rtfeldman/roc-cli
    requires { main : Task {} [] }
    exposes []
    packages {}
    imports [ Task.{ Task }, File ]
    provides [ mainForHost ]
    effects Effect
        {
            #readAllUtf8 : Str -> Effect { errno : I32, bytes : List U8 },
            putLine : Str -> Effect {},
            getLine : Effect Str
        }

mainForHost : Effect {} as Fx
mainForHost = main
