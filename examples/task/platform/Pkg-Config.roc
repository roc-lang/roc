platform folkertdev/foo
    requires { main : Effect {} }
    exposes []
    packages {}
    imports [ Task, File ]
    provides [ mainForHost ]
    effects Effect
        {
            # TODO change sig to Effect { errno : I32, bytes : List U8 }
            readAllUtf8 : Str -> Effect { errno : I64, bytes : Str },
            putLine : Str -> Effect {}
        }

mainForHost : Task.Task {} (File.FileReadErr [BadUtf8]) as Fx
mainForHost = main
