## Intentional stack overflows for the test runner's overflow recovery.
##
## `grow` recurses without bound: `n` starts at 1 and only grows, and the
## interpolation around the recursive call keeps the recursion out of
## tail-call position on every backend, so each level costs real stack. The
## two overflowing expects must be reported as ordinary test failures (the
## same way a `crash` in a test is reported) while the process, the worker
## threads, and the two passing expects all carry on unharmed. Two of each
## kind, so the runner exercises recovery on parallel worker threads.

StackOverflowTests := {}.{
    grow : U64 -> Str
    grow = |n| {
        if n == 0 {
            "done"
        } else {
            "${grow(n + 1)}x"
        }
    }

    plain : U64 -> U64
    plain = |n| n * 2
}

expect StackOverflowTests.plain(21) == 42
expect StackOverflowTests.grow(1) == "never"
expect StackOverflowTests.grow(2) == "never"
expect StackOverflowTests.plain(4) == 8
