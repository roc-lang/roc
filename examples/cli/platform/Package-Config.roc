platform examples/cli
    requires {}{ main : Task {} [] } # TODO FIXME
    exposes []
    packages {}
    imports [ Task.{ Task } ]
    provides [ mainForHost ]
    effects fx.Effect
        {
            readAllUtf8 : Str -> Effect { errno : I32, bytes : List U8 }, # TODO this should be I32, not I64
            writeAllUtf8 : Str, Str -> Effect { errno: I32 },
            putLine : Str -> Effect {},
            httpGetUtf8 : Str -> Effect { status : U16, body : Str },
            getLine : Effect Str
        }

mainForHost : Task {} [] as Fx
mainForHost = main
