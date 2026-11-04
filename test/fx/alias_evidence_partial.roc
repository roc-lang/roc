app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout

# `half` quantifies one of `pair`'s two variables, so a use's substitution is
# not its target's. `ignores_first` keeps a variable with no requirement.
pair = |a, b| "${a.to_str()} ${b.to_str()}"

half : U64, b -> Str where [b.to_str : b -> Str]
half = pair

second_only = |_a, b| b.to_str()

ignores_first : a, U8 -> Str
ignores_first = second_only

apply = |f, x, y| f(x, y)

main! = || {
    Stdout.line!(half(1, 2.U8))
    Stdout.line!(half(3, 4.5.Dec))
    Stdout.line!(apply(half, 5, 6.I8))
    Stdout.line!(ignores_first("ignored", 7))
    Stdout.line!(ignores_first(1.5.Dec, 8))
}
