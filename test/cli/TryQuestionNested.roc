TryQuestionNested := {}

# `?` on a row with a nested payload row, and an inferred function whose error
# row shares its callback's tail.

inner : Str -> Try(U64, [Outer([In1, In2]), Plain])
inner = |s| if s == "1" Err(Outer(In1)) else if s == "p" Err(Plain) else Ok(3)

closed : Try(U64, [Outer([In1, In2]), Plain]) -> Try(U64, [Outer([In1, In2]), Plain, Extra(Str)])
closed = |t| {
    x = t?
    if x == 0 Err(Extra("zero")) else Ok(x)
}

# tail-sharing: an inferred function whose error row tail is the callback's
fetch = |send, url| {
    response = send(url)?
    if response == 0 Err(Bad(url)) else Ok(response)
}

sender : Str -> Try(U64, [Net, Timeout])
sender = |u| if u == "n" Err(Net) else if u == "t" Err(Timeout) else if u == "z" Ok(0) else Ok(9)

show : Try(U64, [Outer([In1, In2]), Plain, Extra(Str)]) -> Str
show = |r| match r { Ok(n) => "Ok(${n.to_str()})", Err(Outer(In1)) => "In1", Err(Outer(In2)) => "In2", Err(Plain) => "Plain", Err(Extra(s)) => "Extra(${s})" }

show2 : Try(U64, [Net, Timeout, Bad(Str)]) -> Str
show2 = |r| match r { Ok(n) => "Ok(${n.to_str()})", Err(Net) => "Net", Err(Timeout) => "Timeout", Err(Bad(u)) => "Bad(${u})" }

expect show(closed(inner("1"))) == "In1"
expect show(closed(inner("p"))) == "Plain"
expect show(closed(Ok(0))) == "Extra(zero)"
expect show(closed(inner("x"))) == "Ok(3)"
expect show2(fetch(sender, "n")) == "Net"
expect show2(fetch(sender, "z")) == "Bad(z)"
expect show2(fetch(sender, "q")) == "Ok(9)"
