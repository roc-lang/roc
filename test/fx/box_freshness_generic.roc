app [main!] { pf: platform "./platform/main.roc" }

import pf.Host
import pf.Stdout

fresh : () -> Box(U64)
fresh = || Box.box(0)

# Same shape as the issue, but the generic method inlines Box.box(0).
SignalInline(a) := { token : Box(U64) }.{
    const : a -> SignalInline(a)
    const = |_| { token: Box.box(0) }

    map : SignalInline(a), (a -> b) -> SignalInline(b)
    map = |_, _| { token: Box.box(0) }
}

# Helper-call version, used to test sharing across distinct specializations.
Signal(a) := { token : Box(U64) }.{
    const : a -> Signal(a)
    const = |_| { token: fresh() }
}

report! : Str, Box(U64), Box(U64) => {}
report! = |label, left, right| {
    if Host.same_box_u64!(left, right) == 1 {
        Stdout.line!("${label}: same allocation")
    } else {
        Stdout.line!("${label}: distinct allocations")
    }
}

main! = || {
    base = SignalInline.const("base")
    first = base.map(|value| "${value}-first")
    second = first.map(|value| "${value}-second")
    report!("generic method inlining Box.box(0)", first.token, second.token)

    str_signal = Signal.const("hello")
    num_signal = Signal.const(42.U8)
    report!("fresh() across two specializations", str_signal.token, num_signal.token)
}
