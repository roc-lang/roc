# The first expect calls a function whose crash message does not type check.
# The expect is blocked by that checked error; the independent expect runs.

poly = || {
    crash YYYYY
    "x"
}

expect poly() == "x"
expect 1 + 1 == 2
