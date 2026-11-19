TryQuestionBody := {}

# A function with a `?` includes its body's own closed result in its composed
# result, a forwarding call included.

use : Try(U64, [BadNum]), Try(U64, [Empty]) -> Try(U64, [BadNum, Empty])
use = |t, u| {
    _ = u?
    t
}

fwd : Try(U64, [BadNum]) -> Try(U64, [BadNum])
fwd = |t| t

use2 : Try(U64, [BadNum]), Try(U64, [Empty]) -> Try(U64, [BadNum, Empty])
use2 = |t, u| {
    _ = u?
    fwd(t)
}

show : Try(U64, [BadNum, Empty]) -> Str
show = |r| match r { Ok(n) => "Ok(${n.to_str()})", Err(BadNum) => "BadNum", Err(Empty) => "Empty" }

expect show(use(Err(BadNum), Ok(1))) == "BadNum"
expect show(use(Ok(5), Err(Empty))) == "Empty"
expect show(use(Ok(5), Ok(1))) == "Ok(5)"
expect show(use2(Err(BadNum), Ok(1))) == "BadNum"
expect show(use2(Ok(5), Err(Empty))) == "Empty"
