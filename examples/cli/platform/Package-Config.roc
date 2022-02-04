platform "examples/cli"
    requires {} { main : Task {} [] }# TODO FIXME
    exposes []
    packages {}
    imports [ Task.{ Task } ]
    provides [ mainForHost ]

mainForHost : Task {} [] as Fx
mainForHost = main
