app [main!] { pf: platform "./platform/main.roc" }

import pf.Host
import pf.Stdout

fresh : () -> Box(U64)
fresh = || Box.box(0)

# Non-generic plain function with a single textual fresh() call site.
make_token : () -> { token : Box(U64) }
make_token = || { token: fresh() }

# Non-generic plain function inlining Box.box directly.
make_inline : () -> { token : Box(U64) }
make_inline = || { token: Box.box(0) }

report! : Str, Box(U64), Box(U64) => {}
report! = |label, left, right| {
    if Host.same_box_u64!(left, right) == 1 {
        Stdout.line!("${label}: same allocation")
    } else {
        Stdout.line!("${label}: distinct allocations")
    }
}

main! = || {
    a = fresh()
    b = fresh()
    report!("direct fresh() twice", a, b)

    c = make_token()
    d = make_token()
    report!("plain fn calling fresh()", c.token, d.token)

    e = make_inline()
    f = make_inline()
    report!("plain fn inlining Box.box(0)", e.token, f.token)
}
