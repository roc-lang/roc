app [main!] { pf: platform "./platform/main.roc" }

import pf.FallibleHost

# Built without specialization, one generic body runs at several type arguments
# in turn, so its composition sites hit and miss. A composition reused across
# type arguments prints the wrong shape.

wrap = |value| { item: value, more: [value] }

show = |value| Str.inspect(wrap(value))

same = |value| wrap(value) == wrap(value)

deepen : a, U64 -> Str
deepen = |value, n| {
    if n == 0 {
        Str.inspect(value)
    } else {
        "${Str.inspect([value])}/${deepen([value], n - 1)}"
    }
}

Chain(a) := [End, Link(a, Chain(a))]

chain = |value, n| {
    if n == 0 {
        Chain.End
    } else {
        Chain.Link(value, chain(value, n - 1))
    }
}

main! = || {
    n = FallibleHost.json_input!({}).count_utf8_bytes()
    label = Str.inspect(n)
    equal = if (same(n) and same(label) and same(n)) "same" else "bad"
    "${show(n)} ${show(label)} ${show(n)} ${equal} ${deepen(n, 2)} ${Str.inspect(chain(n, 1))} ${Str.inspect(chain(label, 1))} ${Str.inspect(chain(n, 2))}"
}
