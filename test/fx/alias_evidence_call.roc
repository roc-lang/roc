app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout

# An alias's use instantiates the alias's scheme: `stored` has no variables and
# no requirements, while `via` requires a dictionary only the alias's forwarded
# lookup records.
via = |value| value.to_str()

stored : U64 -> Str
stored = via

same = |a, b| a == b

at_str : Str, Str -> Bool
at_str = same

Shown := [S].{
    show : Shown, U64 -> Str
    show = |_self, value| stored(value)
}

main! = || {
    Stdout.line!(stored(7))
    Stdout.line!(Shown.S.show(8))
    Stdout.line!(Str.inspect(at_str("a", "a")))
}
