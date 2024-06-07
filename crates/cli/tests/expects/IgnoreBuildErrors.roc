interface IgnoreBuildErrors
    exposes []
    imports []

x = 1

expect x == 1

foo = \x -> x + 1

expect foo 2 == 3

