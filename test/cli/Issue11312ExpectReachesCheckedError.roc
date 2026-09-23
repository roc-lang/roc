# The first expect calls a function whose crash message does not type check.
# The expect still runs; the crash that checking put in its place fails it.

poly = || {
    crash YYYYY
    "x"
}

expect poly() == "x"
expect 1 + 1 == 2
