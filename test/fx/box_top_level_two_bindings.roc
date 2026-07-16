app [main!] { pf: platform "./platform/main.roc" }

import pf.Host
import pf.Stdout

# Two textually identical top-level box bindings are two values: the const
# store never dedupes by content, so each binding keeps its own allocation
# identity (issue #10171).
first : Box(U64)
first = Box.box(0)

second : Box(U64)
second = Box.box(0)

main! = || {
    if Host.same_box_u64!(first, second) == 1 {
        Stdout.line!("same allocation")
    } else {
        Stdout.line!("distinct allocations")
    }
}
