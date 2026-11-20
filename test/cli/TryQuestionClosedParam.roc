TryQuestionClosedParam := {}

# `?` on a parameter whose error row is closed composes it into the wider
# result.

use : Try(U64, [BadNum]) -> Try(U64, [BadNum, Empty])
use = |t| {
    x = t?
    if x == 0 Err(Empty) else Ok(x + 1)
}

expect use(Err(BadNum)) == Err(BadNum)
expect use(Ok(0)) == Err(Empty)
expect use(Ok(4)) == Ok(5)
