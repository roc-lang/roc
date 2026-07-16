app [main!] { pf: platform "./platform/main.roc" }

import pf.Host
import pf.Stdout

# A top-level binding is one value: every reference shares the single
# (static, refcount-pinned) allocation. This is the deliberate exception to
# Box.box's distinct-allocation-per-evaluation guarantee (issue #10171).
boxed : Box(U64)
boxed = Box.box(0)

main! = || {
    if Host.same_box_u64!(boxed, boxed) == 1 {
        Stdout.line!("same allocation")
    } else {
        Stdout.line!("distinct allocations")
    }
}
