app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout
import boxy_procedure_aliases/Aliases as Aliases

local_alias = Aliases.chained
apply = |fn, value| fn(value)

# Exact aliases need no evaluator wrapper; computed callables still do.
# The platform also forwards its provided entrypoint to this app's main!.
main! = || {
    Stdout.line!(local_alias("direct"))
    Stdout.line!(apply(local_alias, "passed"))
    Stdout.line!(local_alias(42.U64).to_str())
    Stdout.line!(Aliases.stored("computed"))
}
