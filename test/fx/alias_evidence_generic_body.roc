app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout

# The first use of `again` is at a variable of `wrap`, which pins nothing, and
# `wrap`'s instantiations reach it at different types.
via = |value| value.to_str()

again = via

wrap = |x| again(x)

as_value = |x| {
    f = again
    f(x)
}

main! = || {
    Stdout.line!(wrap(7.U64))
    Stdout.line!(wrap(8.U8))
    Stdout.line!(as_value(9.I16))
    Stdout.line!(as_value(4.U32))
}
