TryQuestionBoundedValue := {}

# `?` on an annotated local value and an annotated top-level value, whose rows
# their annotations bound, and in an inferred function.

closed_top : Try(U64, [BadNum])
closed_top = Err(BadNum)

use : Bool -> Try(U64, [BadNum, Empty])
use = |b| {
    local : Try(U64, [BadNum])
    local = if b Ok(0) else Err(BadNum)
    x = local?
    y = if b x else closed_top?
    if y == 0 Err(Empty) else Ok(y)
}

infer = |t| {
    x = t?
    if x == 0 Err(Zero) else Ok(x)
}

closed_arg : Try(U64, [BadNum])
closed_arg = Err(BadNum)

show : Try(U64, [BadNum, Empty]) -> Str
show = |r| match r { Ok(n) => "Ok(${n.to_str()})", Err(BadNum) => "BadNum", Err(Empty) => "Empty" }

show2 : Try(U64, [BadNum, Zero]) -> Str
show2 = |r| match r { Ok(n) => "Ok(${n.to_str()})", Err(BadNum) => "BadNum", Err(Zero) => "Zero" }

expect show(use(Bool.True)) == "Empty"
expect show(use(Bool.False)) == "BadNum"
expect show2(infer(closed_arg)) == "BadNum"
expect show2(infer(Ok(0))) == "Zero"
