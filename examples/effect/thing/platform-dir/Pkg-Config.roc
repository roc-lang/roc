platform folkertdev/foo
    requires { main : Effect {} }
    exposes []
    packages {}
    imports [Task]
    provides [ mainForHost ]
    effects Effect
        {
            putLine : Str -> Effect {}
        }


# putChar : I64 -> Effect {},
# getLine : Effect Str

mainForHost : Task.Task {} F64 as Fx
mainForHost = main
