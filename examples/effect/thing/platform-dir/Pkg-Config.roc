platform folkertdev/foo
    requires { main : Effect {} }
    exposes []
    packages {}
    imports [Effect]
    provides [ mainForHost ]
    effects Effect
        {
            putChar : I64 -> Effect {},
            putLine : Str -> Effect {},
            getLine : Effect Str
        }

mainForHost : Effect.Effect {} as Fx
mainForHost = main
