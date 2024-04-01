platform "echo-in-rust"
    requires {} { main : Task {} [] }
    exposes [Task]
    packages {}
    imports [InternalTask.{ Task }]
    provides [mainForHost]

mainForHost : Task {} []
mainForHost = main
