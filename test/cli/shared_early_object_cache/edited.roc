app [main!] { pf: platform "../../fx-open/platform/main.roc" }

import pf.Stdout
import Closed

initial : U64
initial = Closed.total(10)

# Same app path, different filling of the platform's main! requirement.
main! = |args| {
    runtime = Closed.total(args.len().to_u64() + 10)
    Stdout.line!("edited ${initial.to_str()} ${runtime.to_str()}")
    Ok({})
}
