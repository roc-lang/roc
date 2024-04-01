platform "echo-in-rust"
    requires {} { main : Task {} [] }
    exposes [Task]
    packages {}
    imports [Task.{ Task }]
    provides [mainForHost]

mainForHost : Task.Op
mainForHost = Task.taskToOp main
