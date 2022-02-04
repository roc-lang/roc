platform "examples/cli"
    requires {} { main : Str -> Task {} [] }# TODO FIXME
    exposes []
    packages {}
    imports [ Task.{ Task } ]
    provides [ mainForHost ]

mainForHost : Str -> Task {} [] as Fx
mainForHost = \file -> main file
