platform "cli"
    requires {} { main : Task {} [] * }
    exposes []
    packages {}
    imports [Task.{ Task }, InternalTask, Effect.{ Effect }]
    provides [mainForHost]

mainForHost : Effect (Result {} []) as Fx
mainForHost = InternalTask.toEffect main
