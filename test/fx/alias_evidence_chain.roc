app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout

# Each alias's lookup is read in that alias's scope, up to the procedure the
# chain reaches.
via = |value| value.to_str()

first : U64 -> Str
first = via

second = first

third : U64 -> Str
third = second

generic = via
generic_again = generic

at_u8 : U8 -> Str
at_u8 = generic_again

apply = |f, x| f(x)

main! = || {
    Stdout.line!(third(1))
    Stdout.line!(apply(third, 2))
    Stdout.line!(generic_again(3.U16))
    Stdout.line!(apply(generic_again, 4.I8))
    Stdout.line!(at_u8(5))
    Stdout.line!(apply(at_u8, 6))
}
