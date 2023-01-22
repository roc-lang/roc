platform "examples/arena-allocator"
    requires {}{ main : Task {} [] }
    exposes []
    packages {}
    imports [ Task.{ Task } ]
    provides [ mainForHost ]
    effects fx.Effect
        {
            arenaStart : Effect U8,
            arenaEnd : Effect {},
            stdinRead : Effect Str,
            stdoutWrite : Str -> Effect {},
        }

mainForHost : Task {} [] as Fx
mainForHost =
    main
