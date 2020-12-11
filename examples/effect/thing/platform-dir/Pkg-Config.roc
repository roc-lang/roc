platform folkertdev/foo
    requires { main : Effect {} }
    exposes []
    packages {}
    imports []
    provides [ mainForHost ]
    effects Effect
        {
            putLine : Str -> Effect {}
        }


# putChar : I64 -> Effect {},
# getLine : Effect Str


Effect a : [ Effect ({} -> a) ]
Task a err : Effect (Result a err)

mainForHost : Task {} F64 as Fx
mainForHost = main
