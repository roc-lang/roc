app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout

# An alias of a target over open rows, called with wider rows than its target
# names, directly and as a value.
name = |tag|
    match tag {
        One => "one"
        _ => "other"
    }

named = name

x_of = |r| r.x

x_again = x_of

wide = |flag| if flag One else Two

show = |g, v| g(v)

main! = || {
    Stdout.line!(named(wide(True)))
    Stdout.line!(named(wide(False)))
    Stdout.line!(x_again({ x: "row", y: 1.U8 }))
    Stdout.line!(x_again({ x: "wider", y: 1.U8, z: "s" }))
    Stdout.line!(show(named, wide(True)))
    Stdout.line!(show(x_again, { x: "value", w: 2.U16 }))
}
