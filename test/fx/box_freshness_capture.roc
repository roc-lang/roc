app [main!] { pf: platform "./platform/main.roc" }

import pf.Host
import pf.Stdout

# Boxes can hide from type-level analysis inside closure captures behind an
# opaque nominal. Each `make()` call must still allocate a fresh box (issue
# #10171): the hoist keep-gate treats function-containing backings as
# conservatively box-bearing precisely because captures are invisible to types.
Thunk :: [Mk(() -> Box(U64))].{
    make : () -> Thunk
    make = || {
        b = Box.box(0)
        Mk(|| b)
    }

    get : Thunk -> Box(U64)
    get = |t| match t {
        Mk(f) => f()
    }
}

main! = || {
    t1 = Thunk.make()
    t2 = Thunk.make()

    if Host.same_box_u64!(Thunk.get(t1), Thunk.get(t2)) == 1 {
        Stdout.line!("same allocation")
    } else {
        Stdout.line!("distinct allocations")
    }
}
