platform folkertdev/foo
    requires { main : Effect {} }
    exposes []
    packages {}
    imports [Task]
    provides [ mainForHost ]
    effects Effect
        {
            putChar : I64 -> Effect {},
            putLine : Str -> Effect {},
            getLine : Effect Str
        }


mainForHost : Task.Task {} [] as Fx
mainForHost = main
