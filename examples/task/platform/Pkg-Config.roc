platform folkertdev/foo
    requires { main : Task {} * }
    exposes []
    packages {}
    imports [ Task, File ]
    provides [ mainForHost ]
    effects fx.Effect
        {
            # TODO change errno to I32
            readAllUtf8 : Str -> Effect { errno : I64, bytes : List U8 },
            putLine : Str -> Effect {}
        }

mainForHost : Task.Task {} (File.FileReadErr [ BadUtf8 Str.Utf8ByteProblem Nat ]) as Fx
mainForHost = main
