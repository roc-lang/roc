platform "benchmarks"
    requires {} { main : Task {} [] }
    exposes []
    packages {}
    provides [mainForHost]

import Task exposing [Task]

mainForHost : Task {} []
mainForHost = main
