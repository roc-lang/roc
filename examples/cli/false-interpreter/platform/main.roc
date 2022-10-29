platform "false-interpreter"
    requires {} { main : Str -> Task {} [] }
    exposes []
    packages {}
    imports [Task.{ Task }]
    provides [mainForHost]

mainForHost : Str -> Task {} [] as Fx
mainForHost = \file -> main file
