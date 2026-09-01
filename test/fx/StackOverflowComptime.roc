## An intentional stack overflow during compile-time evaluation.
##
## `boom` is a top-level constant, so the compiler evaluates it during
## compile-time finalization. `helper` recurses without bound (`n` starts at 1
## and only grows, and the `+ 1` keeps the call out of tail position), so that
## evaluation overflows its stack. The overflow must surface as an ordinary
## compile-time diagnostic on this definition; it must never take the compiler
## process down.

StackOverflowComptime := {}.{
    helper : U64 -> U64
    helper = |n| {
        if n == 0 {
            0
        } else {
            helper(n + 1) + 1
        }
    }
}

boom : U64
boom = StackOverflowComptime.helper(1)

expect boom == 0
