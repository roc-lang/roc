platform "cli"
    requires {} { main : List Str -> Task {} [] * }
    exposes []
    packages {}
    imports [Task.{ Task }, InternalTask, Effect.{ Effect }]
    provides [mainForHost]

mainForHost : List Str -> Effect (Result {} []) as Fx
mainForHost = \args -> InternalTask.toEffect (main args)
