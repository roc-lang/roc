app [main!] { pf: platform "../../fx-open/platform/main.roc" }

import pf.Stdout
import Closed

# Checking and native execution share this closed producer.
initial : U64
initial = Closed.total(10)

main! = |args| {
    runtime = Closed.total(args.len().to_u64() + 10)
    Stdout.line!("original ${initial.to_str()} ${runtime.to_str()}")
    Ok({})
}
