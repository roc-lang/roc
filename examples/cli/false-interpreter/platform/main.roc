platform "false-interpreter"
    requires {} { main : Str -> Task {} [] }
    exposes []
    packages {}
    provides [mainForHost]

import Task exposing [Task]

mainForHost : Str -> Task {} []
mainForHost = \file -> main file
