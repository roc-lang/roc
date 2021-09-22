platform examples/cli
    requires {}{ main : Task {} [] }
    exposes []
    packages {}
    imports [ Task.{ Task } ]
    provides [ mainForHost ]
    effects fx.Effect
        {
            readAllUtf8 : Str -> Effect { str: Str, errno: I32 },
            writeAllUtf8 : Str, Str -> Effect I32,
            putLine : Str -> Effect {},
            errLine : Str -> Effect {},
            httpGetUtf8 : Str -> Effect { status : U16, body : Str },
            getLine : Effect Str
        }


mainForHost : Task {} [] as Fx
mainForHost = main
