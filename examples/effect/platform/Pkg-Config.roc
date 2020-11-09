platform folkertdev/foo
    provides [ mainForHost ]
    requires { main : Effect {} }
    imports []
    effects Effect
        {
            putChar : Int -> Effect {},
            putLine : Str -> Effect {},
            getLine : Effect Str
        }

mainForHost : Effect {} as Fx
mainForHost = main
