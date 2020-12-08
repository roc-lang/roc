platform folkertdev/foo
    requires { main : Effect {} }
    exposes []
    packages {}
    imports [ File ]
    provides [ mainForHost ]
    effects Effect
        {
            # TODO change sig to Effect { errno : I32, bytes : List U8 }
            readAllUtf8 : Str -> Effect { errno : I64, bytes : Str }
        }

mainForHost : Effect {} as Fx
mainForHost = main
