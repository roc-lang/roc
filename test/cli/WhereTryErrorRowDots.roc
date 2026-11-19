WhereTryErrorRowDots := {}

# A `where` method's `Try` error row lists exactly the tags an implementation
# may return, so a `match` on it needs no catch-all. Writing `..` in that row
# accepts an implementation that returns more tags, and the `match` then
# needs an `Err(_)` branch.

Narrow := [N].{
    parse : Narrow -> Try(U64, [BadNum])
    parse = |_| Err(BadNum)
}

Wide := [W(U64)].{
    parse : Wide -> Try(U64, [BadNum, TooBig(Str)])
    parse = |W(n)| if n > 9 Err(TooBig("${n.to_str()} is too big")) else if n == 0 Err(BadNum) else Ok(n)
}

describe_exact : a -> Str where [a.parse : a -> Try(U64, [BadNum])]
describe_exact = |x|
    match x.parse() {
        Ok(n) => "ok ${n.to_str()}"
        Err(BadNum) => "bad number"
    }

describe_open : a -> Str where [a.parse : a -> Try(U64, [BadNum, ..])]
describe_open = |x|
    match x.parse() {
        Ok(n) => "ok ${n.to_str()}"
        Err(BadNum) => "bad number"
        Err(_) => "some other error"
    }

# The open row is the enclosing function's, so it can be handed back.
forward_open : a -> Try(U64, [BadNum, ..rest]) where [a.parse : a -> Try(U64, [BadNum, ..rest])]
forward_open = |x| x.parse()

show : Try(U64, [BadNum, TooBig(Str)]) -> Str
show = |r| match r {
    Ok(n) => "ok ${n.to_str()}"
    Err(BadNum) => "bad number"
    Err(TooBig(msg)) => msg
}

expect describe_exact(Narrow.N) == "bad number"
expect describe_open(Narrow.N) == "bad number"
expect describe_open(Wide.W(0)) == "bad number"
expect describe_open(Wide.W(12)) == "some other error"
expect describe_open(Wide.W(3)) == "ok 3"
expect show(forward_open(Wide.W(12))) == "12 is too big"
expect show(forward_open(Wide.W(0))) == "bad number"
expect show(forward_open(Wide.W(4))) == "ok 4"
expect match forward_open(Narrow.N) { Err(BadNum) => Bool.True, Ok(_) => Bool.False }
