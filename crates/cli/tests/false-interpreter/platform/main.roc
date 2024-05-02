platform "false-interpreter"
    requires {} { main : Str -> Task {} [] }
    exposes [Context, Variable]
    packages {}
    imports [Task.{ Task }]
    provides [mainForHost]

mainForHost : Str -> Task {} []
mainForHost = \file -> main file
