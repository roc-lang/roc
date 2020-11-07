platform folkertdev/foo
    provides [ mainForHost ]
    requires { main : Effect {} }
    imports []
    effects Effect
        {
            putChar : Int -> Effect {},
            putLine : Str -> Effect {},
            getLine : Int -> Effect Str
        }
