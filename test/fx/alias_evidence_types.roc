app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout

# Evidence of a generalized alias's lookup belongs to the alias's scope. Reading
# it from any one use would send every use to that use's target.
Tagged := [T(U64)].{
    to_str : Tagged -> Str
    to_str = |Tagged.T(n)| "tag ${n.to_str()}"
}

via = |value| "<${value.to_str()}>"

again = via

at_u8 : U8 -> Str
at_u8 = again

apply = |f, x| f(x)

main! = || {
    Stdout.line!(again(7.U64))
    Stdout.line!(again(Tagged.T(3)))
    Stdout.line!(again(-2.I8))
    Stdout.line!(at_u8(9))
    Stdout.line!(apply(again, 1.5.Dec))
    Stdout.line!(apply(again, Tagged.T(4)))
}
