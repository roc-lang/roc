platform "false-interpreter"
    requires {} { main : Str -> Task {} [] }
    exposes []
    packages {}
    imports []
    provides [mainForHost]

mainForHost : Str -> Task {} []
mainForHost = \file -> main file
