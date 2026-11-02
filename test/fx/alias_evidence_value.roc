app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout

# A value use of an alias reaches its target the way a call does, however the
# value travels.
via = |value| value.to_str()

stored : U64 -> Str
stored = via

apply = |f, x| f(x)

in_list = [stored, stored]
in_record = { f: stored }

main! = || {
    f = stored
    Stdout.line!(f(1))
    Stdout.line!(apply(stored, 2))
    Stdout.line!(Str.join_with(List.map([3.U64, 4], stored), ","))
    Stdout.line!(Str.join_with(List.map(in_list, |g| g(5)), ","))
    Stdout.line!((in_record.f)(6))
}
