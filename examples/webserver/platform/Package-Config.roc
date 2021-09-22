platform examples/webserver
    requires {}{ runHandlers : List Handler }
    exposes []
    packages {}
    imports [ Task.{ Task }, Route, Response.{ Response } ]
    provides [ mainForHost ]
    effects fx.Effect
        {
            readAllUtf8 : Str -> Effect { str: Str, errno: I32 },
            writeAllUtf8 : Str, Str -> Effect I32,
            putLine : Str -> Effect {},
            errLine : Str -> Effect {},
            httpGetUtf8 : Str -> Effect { status : U16, body : Str }
        }

mainForHost : Str -> (Task Response [] as Fx)
mainForHost = \url -> runHandlers url
