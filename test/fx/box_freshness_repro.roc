app [main!] { pf: platform "./platform/main.roc" }

import pf.Host
import pf.Stdout

fresh : () -> Box(U64)
fresh = || Box.box(0)

Signal(a) := { token : Box(U64) }.{
    const : a -> Signal(a)
    const = |_| { token: fresh() }

    map : Signal(a), (a -> b) -> Signal(b)
    map = |_, _| { token: fresh() }
}

main! = || {
    base = Signal.const("base")
    first = base.map(|value| "${value}-first")
    second = first.map(|value| "${value}-second")

    if Host.same_box_u64!(first.token, second.token) == 1 {
        Stdout.line!("same allocation")
    } else {
        Stdout.line!("distinct allocations")
    }
}
