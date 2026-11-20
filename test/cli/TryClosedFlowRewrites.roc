TryClosedFlowRewrites := {}

# A closed `Try` value does not flow into a wider error or ok row by itself.
# Each definition here is one way to write that flow: `Ok(t?)`, `map_err`,
# `map_ok`, a `match` that rebuilds the value, and a callee that rebuilds its
# result.

use : Try(U64, [BadNum]), Bool -> Try(U64, [BadNum, Empty])
use = |t, b| if b Err(Empty) else Ok(t?)

fwd : Try(U64, [BadNum]) -> Try(U64, [BadNum])
fwd = |t| t

use2 : Try(U64, [BadNum]), Bool -> Try(U64, [BadNum, Empty])
use2 = |t, b| if b Err(Empty) else Ok(fwd(t)?)

use3 : Try(U64, [BadNum]), Bool -> Try(U64, [BadNum, Empty])
use3 = |t, b| if b Err(Empty) else t.map_err(|BadNum| BadNum)

describe : a -> Try([Some(Str), None], [E1]) where [a.m : a -> Try([Some(Str)], [E1])]
describe = |x| x.m().map_ok(|Some(s)| Some(s))

Job := [Pending].{
    m : Job -> Try([Some(Str)], [E1])
    m = |_| Ok(Some("cv"))
}

run : a, Bool -> Try(U64, [BadNum, Empty]) where [a.parse : a -> Try(U64, [BadNum])]
run = |x, empty| if empty Err(Empty) else Ok(x.parse()?)

expect use(Err(BadNum), Bool.False) == Err(BadNum)
expect use2(Ok(3), Bool.False) == Ok(3)
expect use3(Err(BadNum), Bool.False) == Err(BadNum)
expect match describe(Job.Pending) { Ok(Some("cv")) => Bool.True, _ => Bool.False }

use_ok : Try([A, B], Str), Bool -> Try([A, B, C], Str)
use_ok = |t, b| if b Ok(C) else t.map_ok(|v| match v { A => A, B => B })

use_ok2 : Try([A, B], Str), Bool -> Try([A, B, C], Str)
use_ok2 = |t, b| if b Ok(C) else Ok(match t? { A => A, B => B })

build : Try(U64, [BadNum]) -> Try(U64, [BadNum])
build = |t| match t { Ok(n) => Ok(n), Err(e) => Err(e) }

use_build : Try(U64, [BadNum]), Bool -> Try(U64, [BadNum, Empty])
use_build = |t, b| if b Err(Empty) else build(t)

Num := [Num(U64)].{
    parse : Num -> Try(U64, [BadNum])
    parse = |Num(n)| if n == 0 Err(BadNum) else Ok(n)
}

expect use(Ok(1), Bool.True) == Err(Empty)
expect use2(Err(BadNum), Bool.False) == Err(BadNum)
expect use3(Ok(2), Bool.False) == Ok(2)
expect use_ok(Ok(A), Bool.False) == Ok(A)
expect use_ok(Ok(B), Bool.True) == Ok(C)
expect use_ok2(Err("x"), Bool.False) == Err("x")
expect use_ok2(Ok(B), Bool.False) == Ok(B)
expect use_build(Err(BadNum), Bool.False) == Err(BadNum)
expect use_build(Ok(5), Bool.False) == Ok(5)
expect use_build(Ok(5), Bool.True) == Err(Empty)
expect run(Num.Num(0), Bool.False) == Err(BadNum)
expect run(Num.Num(6), Bool.False) == Ok(6)
expect run(Num.Num(6), Bool.True) == Err(Empty)
