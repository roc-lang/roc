platform folkertdev/foo
    requires { main : Effect {} }
    exposes []
    packages {}
    imports [Cmd]
    provides [ mainForHost ]
    effects Effect
        {
            putChar : I64 -> Effect {},
            putLine : Str -> Effect {},
            getLine : Effect Str
        }


mainForHost : {init : ({} -> I64) as Init }
mainForHost = 
    { init : \{} -> 42 }


