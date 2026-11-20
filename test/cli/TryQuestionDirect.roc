TryQuestionDirect := {}

# `?` on a call whose annotated error row is narrower than the enclosing
# function's.

parse : Str -> Try(U64, [BadNum])
parse = |s| if s == "" Err(BadNum) else Ok(7)

use : Str -> Try(U64, [BadNum, Empty])
use = |s| {
    x = parse(s)?
    if x == 0 Err(Empty) else Ok(x + 1)
}

expect use("") == Err(BadNum)
expect use("a") == Ok(8)
