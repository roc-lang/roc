TryQuestionForward := {}

# `?` on the result of a function whose body forwards its parameter, so its
# result row is closed.

fwd : Try(U64, [BadNum]) -> Try(U64, [BadNum])
fwd = |t| t

use : Try(U64, [BadNum]) -> Try(U64, [BadNum, Empty])
use = |t| {
    x = fwd(t)?
    if x == 0 Err(Empty) else Ok(x + 1)
}

show : Try(U64, [BadNum, Empty]) -> Str
show = |r| match r { Ok(n) => "Ok(${n.to_str()})", Err(BadNum) => "BadNum", Err(Empty) => "Empty" }

expect show(use(Err(BadNum))) == "BadNum"
expect show(use(Ok(0))) == "Empty"
expect show(use(Ok(2))) == "Ok(3)"
