app [main!] { pf: platform "../../fx-open/platform/main.roc" }

import pf.Stdout
import Closed
import Stable

stable : U64
stable = Stable.total(2)
expect stable == 1

extra_stable : U64
extra_stable = Stable.total(3)
expect extra_stable == 3

initial : U64
initial = {
    dbg "CTFE publication observation"
    Closed.total(10)
}

expect initial == 45

extra_total : U64
extra_total = Closed.total(3)
expect extra_total >= 3

scaled : U64
scaled = Closed.checked_scale(1)
expect scaled == 10

extra_scaled : U64
extra_scaled = Closed.checked_scale(2)
expect extra_scaled == 20

main! = |args| {
    runtime = Closed.runtime_total(args.len().to_u64() + 10)
    Stdout.line!("${initial.to_str()} ${runtime.to_str()}")
    Ok({})
}
