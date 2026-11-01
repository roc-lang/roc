app [main!] { pf: platform "./platform/main.roc" }

import pf.FallibleHost

# Built without specialization, every function here is generic at run time:
# its type-variable arguments arrive by address, and a result of variable type
# is written to storage its caller provides. Each cycle is made only of tail
# calls, so it runs in constant stack space.

keep : U64, a -> a
keep = |n, x| if n == 0 x else keep(n - 1, x)

keep_list : U64, List(a) -> List(a)
keep_list = |n, xs| if n == 0 xs else keep_list(n - 1, xs)

# Each call's argument is built in the frame that the call replaces.
step : U64, a, (a -> a) -> a
step = |n, x, f| if n == 0 x else step(n - 1, f(x), f)

# A dynamic result, between callees that take different arguments.
ping : U64, a, Str -> a
ping = |n, x, _label| if n == 0 x else pong(n - 1, x)

pong : U64, a -> a
pong = |n, x| if n == 0 x else ping(n - 1, x, "a label long enough to live on the heap")

# Dynamic arguments and a closed result.
tick : U64, U64, a -> U64
tick = |n, total, x| if n == 0 total else tock(n - 1, total + 1, x)

tock : U64, U64, a -> U64
tock = |n, total, x| if n == 0 total else tick(n - 1, total + 2, x)

# Ordinary calls into a cycle from generic code, whose result storage is itself
# known only at run time.
both : U64, a -> List(a)
both = |n, x| [keep(n, x), keep(n + 1, x)]

# What the host returns makes every value here a run-time value, so none of
# these cycles is evaluated at compile time.
main! = || {
    input = FallibleHost.json_input!({})
    depth = Str.count_utf8_bytes(input) * 2_000
    kept = keep(depth, "${input} and a string long enough to live on the heap")
    listed = keep_list(depth, [kept, "b"])
    counted = step(depth, 0.U64, |c| c + 1)
    grown = step(3, kept, |s| Str.concat(s, "!"))
    bounced = ping(depth + 1, grown, "s")
    ticked = tick(depth, 0, listed)
    pair = both(depth, "c")
    "${bounced} ${Str.join_with(listed, ",")} ${counted.to_str()} ${ticked.to_str()} ${Str.join_with(pair, "")}"
}
