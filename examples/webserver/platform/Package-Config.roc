platform examples/webserver
    requires {}{ handlers : List Handler }
    exposes []
    packages {}
    imports [ Task.{ Task }, Route.{ Handler } ]
    provides [ routeHandlers ]
    effects fx.Effect
        {
            readAllUtf8 : Str -> Effect { str: Str, errno: I32 },
            writeAllUtf8 : Str, Str -> Effect I32,
            putLine : Str -> Effect {},
            errLine : Str -> Effect {},
            httpGetUtf8 : Str -> Effect { status : U16, body : Str }
        }


routeHandlers : List Handler
outeHandlers = handlers
