platform folkertdev/foo
    requires { main : Effect {} }
    exposes []
    packages {}
    imports [ Task ]
    provides [ mainForHost ]
    effects Effect
        {
            putLine : Str -> Effect {}
        }

mainForHost : Task.Task {} [] as Fx
mainForHost = main
